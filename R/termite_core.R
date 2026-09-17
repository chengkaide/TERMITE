# =============================================================================
#  TERMITE core —— LA-ICP-MS 微量元素数据归算内核
#  ---------------------------------------------------------------------------
#  LASP / Mischel et al. 的 TERMITE 脚本（Version 1）的纯 base-R 重写。
#  · 零外部依赖（不需要 matrixStats / ggplot2 / reshape2 / miscTools）
#  · 函数式、可复用，不依赖全局变量与 setwd()
#  · 保留原始算法的数学定义，同时修正已定位的 bug（见 docs/FIXES.md）
#
#  归一算链（一段话）：
#     净信号 cps  →  内标归一(IS)  →  同位素丰度+原子量换算(摩尔比→质量比)
#                 →  × 内标浓度   →  ÷ RSF  →  检出限截断  →  浓度 µg/g
#
#  作者：重构自 Simon A. Mischel 等 (2017) TERMITE v1
# =============================================================================


# -----------------------------------------------------------------------------
# 0. 参数默认值
# -----------------------------------------------------------------------------

#' TERMITE 运行参数
#'
#' @param mode    "spot"（点分析）或 "line"（线扫描）。
#' @param dir     主目录；其下应有 Rawdata_* / ReferenceMaterial_* / TERMITEScriptFolder。
#' @param machine "Agilent"（逗号分隔）或 "Element2"（制表符分隔、含分辨率后缀）。
#' @param column_IS  内标同位素在**原始文件中的列号**（含时间列，从 1 计）。默认 5 = Ca43。
#' @param is_conc    样品内标元素含量 [µg/g]。
#' @param header_line / signal_line  同位素表头行号 / 数据起始行号（文件真实行号，从 1 计）。
#' @param first_blank,last_blank     制备空白（gas blank）区间，文件真实行号。
#' @param first_signal,last_signal   信号区间，文件真实行号（点分析：样品与参考物质共用）。
#' @param first_blank_ls,last_blank_ls,first_signal_ls,last_signal_ls
#'                                   线扫描样品专用的空白/信号区间。
#' @param laser_speed 激光扫描速率 [µm/s]。
#' @param outlier_pct 离群判据的百分数 m（%），阈值 = 中位数 ± m%×中位数。
#' @param legacy_lod       TRUE 时复刻原脚本有缺陷的 LoD 文件选取与 0 标准差替换。
#' @param legacy_time_axis TRUE 时复刻原脚本线扫描 x 轴的错位写法。
#' @param clip_negative    净信号是否截断负值为 0（原脚本：点分析裁剪、线扫描不裁剪）。
termite_defaults <- function(mode = c("spot", "line")) {
  mode <- match.arg(mode)
  spot <- identical(mode, "spot")
  list(
    mode            = mode,
    dir             = "your_main_directory",
    machine         = "Agilent",
    dir_sample      = if (spot) "Rawdata_spotscan"        else "Rawdata_linescan",
    dir_ref         = if (spot) "ReferenceMaterial_spotscan" else "ReferenceMaterial_linescan",
    dir_results     = "Results",
    file_isotopes   = "TERMITEScriptFolder/AtomGewIsoAbund_NIST.csv",
    file_standards  = "TERMITEScriptFolder/Standards_GeoReM.csv",

    n_iso           = 12L,
    header_line     = 3L,
    signal_line     = 4L,
    resolution      = "(LR)",

    column_IS       = 5L,
    is_conc         = 400003.81,
    background      = "median",          # "median" | "mean"

    first_blank     = 5L,
    last_blank      = 124L,
    # 注意：两个入口脚本对「参考物质积分窗口」的默认值并不相同——
    #   TERMITE_spotscan.r : 180 – 440
    #   TERMITE_linescan.r: 130 – 545
    first_signal    = if (spot) 180L else 130L,
    last_signal     = if (spot) 440L else 545L,

    first_blank_ls  = 5L,
    last_blank_ls   = 124L,
    first_signal_ls = 143L,
    last_signal_ls  = 6890L,
    laser_speed     = 5,                 # µm/s

    outlier_test    = TRUE,
    outlier_pct     = 30,

    ref_materials   = if (spot) c("NIST612", "MACS3") else "NIST612",

    clip_negative    = spot,             # 原脚本的默认行为
    legacy_lod       = FALSE,
    legacy_time_axis = FALSE,

    n_sweeps_sample = NA_integer_,       # NA = 读取文件全部数据行
    n_sweeps_ref    = NA_integer_
  )
}


# -----------------------------------------------------------------------------
# 1. 底层 I/O
# -----------------------------------------------------------------------------

.termite_sep <- function(machine) if (identical(machine, "Element2")) "\t" else ","

#' 把可能带杂质字符的文本向量转成数值
#'
#' 原脚本用 gsub("[^E0-9.0-9]", "NA", x)，字符类里重复了 0-9，
#' 且**会把正负号和小数点以外的所有字符（含减号）一起吃掉**，
#' 于是 "-1.5" 变成 "1.5"。这里保留 0-9 . e E + -。
termite_sanitize <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  out <- suppressWarnings(as.numeric(x))
  bad <- is.na(out) & !is.na(x) & nzchar(x) & x != "NA"
  if (any(bad)) {
    cleaned <- gsub("[^0-9eE.+-]", "", x[bad])
    out[bad] <- suppressWarnings(as.numeric(cleaned))
  }
  out
}

#' 读表头（同位素名单）
termite_read_header <- function(path, cfg) {
  sep  <- .termite_sep(cfg$machine)
  skip <- max(0L, as.integer(cfg$header_line) - 1L)
  df <- utils::read.table(path, header = FALSE, sep = sep, skip = skip, nrows = 1L,
                          colClasses = "character", fill = TRUE, quote = "",
                          comment.char = "", check.names = FALSE)
  if (ncol(df) < cfg$n_iso + 1L)
    stop(sprintf("表头行只有 %d 列，少于所需的 %d 列：%s", ncol(df), cfg$n_iso + 1L, path))
  h <- trimws(as.character(unlist(df[1, 2:(cfg$n_iso + 1L)])))
  if (!is.null(cfg$resolution) && nzchar(cfg$resolution))
    h <- trimws(gsub(cfg$resolution, "", h, fixed = TRUE))
  h
}

#' 读一个原始数据文件
#'
#' @param n_sweeps 文件总行数上限；NA 表示读取全部数据行。
#' @return list(time = 时间列, values = 纯数值矩阵 n×p)
termite_read_raw <- function(path, cfg, n_sweeps = NA_integer_) {
  sep        <- .termite_sep(cfg$machine)
  skip       <- max(0L, as.integer(cfg$signal_line) - 1L)
  take       <- if (is.null(n_sweeps) || is.na(n_sweeps)) -1L else as.integer(n_sweeps) - skip
  df <- utils::read.table(path, header = FALSE, sep = sep, skip = skip,
                          nrows = take, colClasses = "character", fill = TRUE,
                          quote = "", comment.char = "", blank.lines.skip = FALSE,
                          check.names = FALSE)
  if (ncol(df) < cfg$n_iso + 1L)
    stop(sprintf("数据区只有 %d 列，少于所需的 %d 列：%s", ncol(df), cfg$n_iso + 1L, path))
  raw <- as.matrix(df[, 2:(cfg$n_iso + 1L), drop = FALSE])
  m <- matrix(termite_sanitize(as.vector(raw)), nrow = nrow(raw), ncol = ncol(raw))
  list(time = termite_sanitize(df[[1]]), values = m)
}

#' 读同位素原子量/丰度表（AtomGewIsoAbund_NIST.csv，制表符分隔）
termite_read_isotopes <- function(path) {
  df <- utils::read.table(path, header = TRUE, sep = "\t", check.names = FALSE,
                          colClasses = "character", quote = "", comment.char = "")
  rn <- as.character(df[[1]])
  df[[1]] <- NULL
  num <- as.data.frame(lapply(df, termite_sanitize), check.names = FALSE)
  names(num) <- names(df)
  row.names(num) <- rn
  num
}

#' 读参考物质推荐值表（Standards_GeoReM.csv，逗号分隔，首列是物质名）
termite_read_standards <- function(path) {
  df <- utils::read.table(path, header = TRUE, sep = ",", row.names = 1,
                          check.names = FALSE, colClasses = "character",
                          quote = "", comment.char = "")
  num <- as.data.frame(lapply(df, termite_sanitize), check.names = FALSE)
  names(num) <- names(df)
  row.names(num) <- row.names(df)     # lapply 会丢行名，必须补回
  num
}


# -----------------------------------------------------------------------------
# 2. 小工具
# -----------------------------------------------------------------------------

#' 文件中「真实行号」→ 读取后矩阵内的行号
#'
#' 原脚本靠把 Line.of.Signal 先减 1 再做减法的隐式约定，
#' 这里显式写出来：read.table(skip = signal_line - 1) 之后
#' 第 1 行对应的是文件第 signal_line 行。
.to_matrix_row <- function(line_no, signal_line) as.integer(line_no) - (as.integer(signal_line) - 1L)

#' 列中位数（matrixStats::colMedians 的 base-R 等价实现）
.col_medians <- function(m, na.rm = TRUE) {
  m <- as.matrix(m)
  if (ncol(m) == 0L) return(numeric(0))
  apply(m, 2L, stats::median, na.rm = na.rm)
}

#' 列标准差（matrixStats::colSds 的 base-R 等价实现）
.col_sds <- function(m, na.rm = TRUE) {
  m <- as.matrix(m)
  if (ncol(m) == 0L) return(numeric(0))
  apply(m, 2L, stats::sd, na.rm = na.rm)
}

#' 背景（gas blank）统计量
.blank_stat <- function(m, i1, i2, method = "median") {
  sub <- m[i1:i2, , drop = FALSE]
  if (identical(method, "mean")) colMeans(sub, na.rm = TRUE) else .col_medians(sub, na.rm = TRUE)
}

#' 元素符号：从 "Ca43" 取 "Ca"
element_of <- function(isotopes) sub("^([[:alpha:]]*).*", "\\1", isotopes)


# -----------------------------------------------------------------------------
# 3. 核心：一个「文件块」的归算
# -----------------------------------------------------------------------------

#' 归算一组原始文件
#'
#' @param mats      list，每个元素是一个 n×p 的净强度矩阵
#' @param files     文件名向量（与 mats 同序）
#' @param ref_rm    每个文件的参考物质名（样品文件为 NA）
#' @param cfg, header, iso_tab, std_tab  见上
#' @param i1,i2,i3,i4  空白/信号区间（文件真实行号）
#' @return list，含逐文件的 LoD、内标归一矩阵、离群过滤均值等
.reduce_block <- function(mats, files, ref_rm, cfg, isotopes, iso_tab, std_tab, i1, i2, i3, i4) {
  n  <- length(mats)
  p  <- length(isotopes)
  p1 <- .to_matrix_row(i1, cfg$signal_line)
  p2 <- .to_matrix_row(i2, cfg$signal_line)
  s1 <- .to_matrix_row(i3, cfg$signal_line)
  s2 <- .to_matrix_row(i4, cfg$signal_line)
  is_idx <- as.integer(cfg$column_IS) - 1L          # 原始内标列号 → 矩阵列号
  m_pct  <- cfg$outlier_pct / 100
  blank   <- vector("list", n)    # 每个文件的空白统计
  net     <- vector("list", n)    # 扣除空白后的净信号（已按需裁剪负值）
  isn     <- vector("list", n)    # 内标归一后的矩阵
  lodm    <- matrix(NA_real_, n, p)   # 逐文件 LoD（同一文件内各行相同）
  means   <- matrix(NA_real_, n, p)   # 逐文件、逐元素的（离群过滤后）均值
  frac    <- matrix(NA_real_, n, p)   # 离群剔除比例
  rsd     <- matrix(NA_real_, n, p)   # 相对标准偏差（未过滤）

  for (k in seq_len(n)) {
    m <- mats[[k]]
    s2k <- s2
    if (s2k > nrow(m)) {
      warning(sprintf("信号结束行 %d 超出文件 %s 的实际行数 %d，已截断。",
                      i4, basename(files[k]), nrow(m) + cfg$signal_line - 1L),
              call. = FALSE)
      s2k <- nrow(m)
    }
    nrow_win <- s2k - s1 + 1L
    bl <- .blank_stat(m, p1, p2, cfg$background)
    blank[[k]] <- bl

    x  <- m[s1:s2k, , drop = FALSE] - matrix(bl, nrow = nrow_win, ncol = p, byrow = TRUE)
    if (isTRUE(cfg$clip_negative)) x[!is.na(x) & x < 0] <- 0
    net[[k]] <- x

    # ---- 检出限 LoD（µg/g）--------------------------------------------------
    # 原式：3σ(blank) / mean(净信号 cps) × 首个参考物质的推荐含量
    sd3 <- 3 * .col_sds(m[p1:p2, , drop = FALSE], na.rm = TRUE)
    if (isTRUE(cfg$legacy_lod)) sd3[!is.na(sd3) & sd3 == 0] <- 1   # 原脚本的补丁
    mu  <- colMeans(x, na.rm = TRUE)
    lod_ref_iso <- std_tab[cfg$ref_materials[1], isotopes]
    lodm[k, ] <- (sd3 / mu) * as.numeric(lod_ref_iso)

    # ---- 内标归一 ----------------------------------------------------------
    denom <- m[s1:s2k, is_idx] - bl[is_idx]
    isn[[k]] <- x / denom

    # ---- 离群检验：阈值 = 中位数 ± m% × 中位数 -----------------------------
    med <- apply(isn[[k]], 2L, stats::median, na.rm = TRUE)
    hi  <- matrix(med * (1 + m_pct), nrow = nrow_win, ncol = p, byrow = TRUE)
    lo  <- matrix(med * (1 - m_pct), nrow = nrow_win, ncol = p, byrow = TRUE)
    keep <- isn[[k]]
    if (isTRUE(cfg$outlier_test)) {
      drop_mask <- !is.na(keep) & (keep > hi | keep < lo)
      frac[k, ] <- colMeans(drop_mask)
      keep[drop_mask] <- NA
    } else {
      frac[k, ] <- 0
    }
    means[k, ] <- colMeans(keep, na.rm = TRUE)

    # ---- 相对标准偏差（用未过滤的内标归一值）-------------------------------
    sdv <- .col_sds(isn[[k]], na.rm = TRUE)
    muv <- colMeans(isn[[k]], na.rm = TRUE)
    rsd[k, ] <- sdv / muv
  }

  dimnames(means) <- dimnames(lodm) <- dimnames(frac) <- dimnames(rsd) <- list(basename(files), isotopes)
  names(blank) <- basename(files)
  list(lod = lodm, mean_is_norm = means, outlier_frac = frac, rsd = rsd,
       blank = blank, net = net, is_norm = isn, m_pct = m_pct)
}


#' LoD 汇总：哪些文件参与「参考物质 LoD」（返回 lod_per_file 的行号）
#'
#' 两个入口脚本在这里并不同构，很容易看错：
#'   · 点分析  TERMITE_spotscan.r —— 用的是 length(alleDatenCount)（**样品文件数**），
#'             代入 `(A-(A-B)+1):A` 后恰好等于「全部参考文件」，是正确的。
#'   · 线扫描  Correction_Script_line_scan_TERMITE.r —— 用的是 `alleDatenRefs` 的
#'             **list 长度**，也就是参考物质**种类数** B。此时 lod 列表里只有参考文件，
#'             `(A-(A-B)+1):A` = `(B+1):A`，会**漏掉前 B 个参考文件**，是 bug。
#' 所以 legacy_lod 开关只对线扫描产生影响。
.lod_selection <- function(mode, n_sample, n_ref_files, n_ref_materials, legacy = FALSE) {
  if (identical(mode, "spot")) {
    if (n_sample <= 0L) return(seq_len(n_ref_files))
    return(seq.int(n_sample + 1L, n_sample + n_ref_files))
  }
  from <- if (isTRUE(legacy)) n_ref_materials + 1L else 1L
  if (from > n_ref_files) return(seq_len(n_ref_files))
  seq.int(from, n_ref_files)
}


# -----------------------------------------------------------------------------
# 4. 主入口
# -----------------------------------------------------------------------------

#' 跑一遍完整的 TERMITE 归算
#'
#' @param cfg       termite_defaults() 产出的参数列表（可先修改）
#' @param progress  可选回调 function(i, n, msg)，用于 Shiny 进度条
#' @return 结构化的结果对象（见文件尾部的字段说明）
termite_run <- function(cfg, progress = NULL) {
  stopifnot(is.list(cfg))
  mode <- cfg$mode
  say  <- function(i, n, msg) if (!is.null(progress)) progress(i, n, msg)

  pj <- function(...) file.path(cfg$dir, ...)
  if (!dir.exists(cfg$dir)) stop("数据目录不存在：", cfg$dir)

  # ---- 4.1 读取参考数据库 -------------------------------------------------
  say(1, 8, "读取同位素与参考物质数据库")
  iso_tab <- termite_read_isotopes(pj(cfg$file_isotopes))
  std_tab <- termite_read_standards(pj(cfg$file_standards))

  # ---- 4.2 收集文件 -------------------------------------------------------
  exts <- "\\.(csv|asc|txt|CSV|ASC|TXT)$"
  sample_files <- character(0)
  if (dir.exists(pj(cfg$dir_sample)))
    sample_files <- list.files(pj(cfg$dir_sample), pattern = exts, full.names = TRUE)
  sample_files <- sort(sample_files)

  ref_files <- character(0); ref_rm <- character(0)
  ref_dir <- pj(cfg$dir_ref)
  if (dir.exists(ref_dir)) {
    for (rm in cfg$ref_materials) {
      f <- sort(list.files(ref_dir, pattern = rm, full.names = TRUE))
      f <- f[grepl(exts, f)]
      if (!length(f)) warning(sprintf("参考物质目录中没有匹配 '%s' 的文件。", rm), call. = FALSE)
      ref_files <- c(ref_files, f)
      ref_rm    <- c(ref_rm, rep(rm, length(f)))
    }
  }
  if (!length(ref_files)) stop("未找到任何参考物质文件，请检查目录与参考物质名称。")

  # ---- 4.3 读表头 ---------------------------------------------------------
  say(2, 8, "读取同位素表头")
  hdr_src <- if (length(sample_files)) sample_files[1] else ref_files[1]
  isotopes <- termite_read_header(hdr_src, cfg)
  if (length(isotopes) != cfg$n_iso)
    stop(sprintf("表头读出 %d 个同位素，与设定的 n_iso = %d 不一致。",
                 length(isotopes), cfg$n_iso))
  elements <- element_of(isotopes)
  is_idx   <- as.integer(cfg$column_IS) - 1L
  if (is_idx < 1L || is_idx > length(isotopes))
    stop("内标列号 column_IS 超出同位素范围。")

  # 参考数据库按表头取子集
  miss_i <- setdiff(isotopes, names(iso_tab))
  miss_s <- setdiff(isotopes, names(std_tab))
  if (length(miss_i)) stop("同位素数据库缺少：", paste(miss_i, collapse = ", "))
  if (length(miss_s)) stop("参考物质数据库缺少：", paste(miss_s, collapse = ", "))
  iso_sub <- iso_tab[, isotopes, drop = FALSE]
  std_sub <- std_tab[, isotopes, drop = FALSE]

  # ---- 4.4 读取原始数据 ---------------------------------------------------
  read_all <- function(files, tag, n_sweeps) {
    out <- vector("list", length(files))
    for (k in seq_along(files)) {
      say(2, 8, sprintf("读取%s %d/%d：%s",
                        if (tag == "sample") "样品" else "参考物质",
                        k, length(files), basename(files[k])))
      out[[k]] <- termite_read_raw(files[[k]], cfg, n_sweeps = n_sweeps)
    }
    out
  }
  sample_raw <- read_all(sample_files, "sample", cfg$n_sweeps_sample)
  ref_raw    <- read_all(ref_files,    "ref",    cfg$n_sweeps_ref)
  sample_mat <- lapply(sample_raw, `[[`, "values")
  ref_mat    <- lapply(ref_raw,    `[[`, "values")

  # 表头一致性检查
  chk <- function(files) {
    for (f in files) {
      h <- tryCatch(termite_read_header(f, cfg), error = function(e) NULL)
      if (!is.null(h) && !identical(h, isotopes))
        warning(sprintf("%s 的表头与主表头不同，已按主表头处理。", basename(f)), call. = FALSE)
    }
  }
  chk(setdiff(c(sample_files, ref_files), hdr_src))

  # ---- 4.5 参考物质的 RSF -------------------------------------------------
  say(4, 8, "归算参考物质并计算 RSF")
  ref_block <- .reduce_block(ref_mat, basename(ref_files), ref_rm, cfg, isotopes,
                             iso_sub, std_sub,
                             cfg$first_blank, cfg$last_blank,
                             cfg$first_signal, cfg$last_signal)

  # 摩尔比→质量比：Iso_j = (A_IS / A_j) · (W_j / W_IS)
  #   A = 同位素丰度（表中第 2 行），W = 原子量（第 1 行）
  iso_fac <- as.numeric(iso_sub[2, is_idx]) / as.numeric(iso_sub[2, ]) *
             as.numeric(iso_sub[1, ]) / as.numeric(iso_sub[1, is_idx])
  names(iso_fac) <- isotopes

  cert_ref <- as.matrix(std_sub[ref_rm, , drop = FALSE])
  is_iso   <- isotopes[is_idx]

  # 参考物质：内标浓度用推荐值
  ref_is_conc <- as.numeric(std_sub[ref_rm, is_iso])
  uncorr_ref  <- ref_block$mean_is_norm * matrix(iso_fac, nrow = length(ref_files),
                                                 ncol = length(isotopes), byrow = TRUE) * ref_is_conc

  rsf_file <- uncorr_ref / cert_ref
  rsf_file[!is.na(rsf_file) & rsf_file == 0] <- NA
  dimnames(rsf_file) <- list(basename(ref_files), isotopes)
  rsf <- colMeans(rsf_file, na.rm = TRUE)

  # ---- 4.6 LoD 汇总 ------------------------------------------------------
  mean_block <- if (identical(mode, "spot")) {
    .reduce_block(sample_mat, basename(sample_files), rep(NA_character_, length(sample_files)),
                  cfg, isotopes, iso_sub, std_sub,
                  cfg$first_blank, cfg$last_blank, cfg$first_signal, cfg$last_signal)
  } else NULL

  n_total   <- length(sample_files) + length(ref_files)
  lod_all   <- rbind(if (!is.null(mean_block)) mean_block$lod else NULL, ref_block$lod)
  lod_sel   <- .lod_selection(mode, length(sample_files), length(ref_files),
                              length(cfg$ref_materials), cfg$legacy_lod)
  lod_used  <- .col_medians(lod_all[lod_sel, , drop = FALSE], na.rm = TRUE)
  names(lod_used) <- isotopes

  res <- list(cfg = cfg, mode = mode, isotopes = isotopes, elements = elements,
              is_iso = is_iso, is_idx = is_idx, iso_factor = iso_fac,
              files = list(sample = basename(sample_files), ref = basename(ref_files),
                           ref_rm = ref_rm),
              ref = ref_block, rsf_per_file = rsf_file, rsf = rsf,
              lod_per_file = lod_all, lod_selection = lod_sel, lod = lod_used,
              legacy_lod = isTRUE(cfg$legacy_lod))

  # ---- 4.7 分模式出结果 ---------------------------------------------------
  if (identical(mode, "spot")) {
    say(6, 8, "归算样品并套用 RSF")
    uncorr <- mean_block$mean_is_norm *
              matrix(iso_fac, nrow = length(sample_files), ncol = length(isotopes), byrow = TRUE) *
              cfg$is_conc
    conc <- uncorr / matrix(rsf, nrow = length(sample_files), ncol = length(isotopes), byrow = TRUE)
    dimnames(conc) <- list(basename(sample_files), isotopes)

    # 逐元素相对标准偏差 → 标记不均匀样品
    rsd   <- mean_block$rsd
    rsd_m <- rep(colMeans(rsd, na.rm = TRUE), each = nrow(rsd))
    flag  <- matrix(rsd > rsd_m, nrow = nrow(rsd), ncol = ncol(rsd))

    conc_masked <- conc
    conc_masked[!is.na(conc_masked) & conc_masked <  matrix(lod_used, nrow(conc), ncol(conc), byrow = TRUE)] <- NA
    conc_masked[!is.na(conc_masked) & conc_masked <= 0] <- NA
    dimnames(conc_masked) <- dimnames(conc)
    res$sample <- list(mean_is_norm = mean_block$mean_is_norm, uncorr = uncorr,
                       conc = conc, conc_masked = conc_masked,
                       rsd = rsd, rsd_flag = flag,
                       outlier_frac = mean_block$outlier_frac,
                       blank = mean_block$blank, net = mean_block$net)
  } else {
    # ---------------- 线扫描：逐文件算剖面 ----------------
    say(6, 8, "计算线扫描浓度剖面")
    f1 <- .to_matrix_row(cfg$first_signal_ls, cfg$signal_line)
    f2 <- .to_matrix_row(cfg$last_signal_ls,  cfg$signal_line)
    b1 <- .to_matrix_row(cfg$first_blank_ls, cfg$signal_line)
    b2 <- .to_matrix_row(cfg$last_blank_ls,  cfg$signal_line)

    profiles <- vector("list", length(sample_files))
    names(profiles) <- basename(sample_files)
    for (k in seq_along(sample_files)) {
      m <- sample_mat[[k]]
      if (f2 > nrow(m)) {
        warning(sprintf("样品信号结束行 %d 超出 %s 的实际行数 %d，已截断。",
                        cfg$last_signal_ls, basename(sample_files[k]), nrow(m) + cfg$signal_line - 1L),
                call. = FALSE)
        f2 <- nrow(m)
      }
      bl  <- .blank_stat(m, b1, b2, cfg$background)
      bgc <- m[f1:f2, , drop = FALSE] -
             matrix(bl, nrow = f2 - f1 + 1L, ncol = length(isotopes), byrow = TRUE)
      if (isTRUE(cfg$clip_negative)) bgc[!is.na(bgc) & bgc < 0] <- 0

      ratio <- bgc / bgc[, is_idx]
      conc  <- ratio *
               matrix(iso_fac, nrow = nrow(bgc), ncol = length(isotopes), byrow = TRUE) *
               cfg$is_conc /
               matrix(rsf,     nrow = nrow(bgc), ncol = length(isotopes), byrow = TRUE)

      t_all <- sample_raw[[k]]$time
      if (isTRUE(cfg$legacy_time_axis)) {
        t_axis <- t_all[seq_len(f2 - f1 + 1L)]                       # 原脚本：从文件开头取
      } else {
        t_axis <- t_all[f1:f2]                                       # 修正：与数据行对齐
        t_axis <- t_axis - t_axis[1]                                 # 以激光开剥时刻为 0
      }
      dist <- t_axis * cfg$laser_speed / 1000                        # s · µm/s → mm

      masked <- conc
      masked[!is.na(masked) & masked < matrix(lod_used, nrow(conc), ncol(conc), byrow = TRUE)] <- NA
      masked[!is.na(masked) & masked < 0] <- NA     # 原脚本此处保留 0 值
      dimnames(masked) <- dimnames(conc) <- NULL
      colnames(conc) <- colnames(masked) <- isotopes

      profiles[[k]] <- list(file = basename(sample_files[k]), distance = dist,
                            conc = conc, conc_masked = masked, blank = bl)
    }
    res$line <- profiles
  }

  say(8, 8, "完成")
  class(res) <- c("termite_result", "list")
  res
}


# -----------------------------------------------------------------------------
# 5. 导出辅助
# -----------------------------------------------------------------------------

#' 结果 → 数据框（用于表格展示与 CSV 导出）
#'
#' @param by "element"（默认，列名用元素符号，与原脚本输出一致）
#'           或 "isotope"（列名保留质量数，便于区分同元素的不同同位素）。
termite_table <- function(res, by = c("element", "isotope")) {
  by <- match.arg(by)
  cn <- if (by == "element") res$elements else res$isotopes
  if (identical(res$mode, "spot")) {
    d <- as.data.frame(res$sample$conc_masked, check.names = FALSE)
    names(d) <- cn
    data.frame(ID = rownames(res$sample$conc_masked), d, check.names = FALSE, row.names = NULL)
  } else {
    f <- res$line[[1]]
    d <- as.data.frame(f$conc_masked, check.names = FALSE)
    names(d) <- cn
    data.frame(length_mm = f$distance, d, check.names = FALSE, row.names = NULL)
  }
}

#' RSF 表
termite_rsf_table <- function(res) {
  data.frame(isotope = res$isotopes, element = res$elements,
             RSF = as.numeric(res$rsf), row.names = NULL)
}

#' LoD 表
termite_lod_table <- function(res) {
  data.frame(isotope = res$isotopes, element = res$elements,
             LoD_ug_g = as.numeric(res$lod), row.names = NULL)
}

#' 参考物质回收率校核：用 RSF 反算的浓度 / 推荐值
termite_rm_check <- function(res, rm_name, isotopes = NULL) {
  rr <- which(res$files$ref_rm == rm_name)
  if (!length(rr)) return(NULL)
  if (is.null(isotopes)) isotopes <- res$isotopes
  rec <- sweep(res$rsf_per_file[rr, isotopes, drop = FALSE], 2, res$rsf[isotopes], "/")
  data.frame(isotope = isotopes,
             measured = as.numeric(colMeans(res$rsf_per_file[rr, isotopes, drop = FALSE], na.rm = TRUE)),
             recovery_pct = as.numeric(colMeans(rec, na.rm = TRUE)) * 100,
             row.names = NULL)
}

#' 取某个文件的背景（gas blank）统计量
#'
#' 点分析时样品与参考物质都存在 `res$sample$blank` / `res$ref$blank`；
#' 线扫描时样品不走 `_reduce_block`，背景存在 `res$line[[k]]$blank`。
termite_blank_of <- function(res, which = c("ref", "sample"), i = 1L) {
  which <- match.arg(which)
  b <- if (identical(which, "ref")) {
    res$ref$blank[[i]]
  } else if (identical(res$mode, "spot")) {
    res$sample$blank[[i]]
  } else {
    res$line[[i]]$blank
  }
  b
}
