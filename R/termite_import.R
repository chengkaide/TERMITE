# =============================================================================
#  TERMITE 导入层 —— 仪器格式自动识别 + 统一读取
#  ---------------------------------------------------------------------------
#  目前支持三类导出格式：
#
#  ① Qtegra (Thermo iCAP RQ / iCAP TQ)，本文件主要新增支持
#       第 1 行      SRM 610:08/06/2023 03:07:07 AM;      ← 样品名:日期时间;
#       第 2–13 行   仪器元数据（Software / Configuration / STD / RF / … ）
#       第 14 行     表头：Time,7Li,9Be,…,238U,
#       第 15 行     每通道元数据：,dwell time=0.01;xcal factor=53845.6,…  ← 跳过
#       第 16 行起   数值数据（**计数率 cps**，Qtegra 已按 dwell time 换算好）
#
#  ② Agilent / 四极杆（TERMITE 原始示例）
#       第 1–2 行    采集信息
#       第 3 行      表头：Time [Sec],Mg25,Al27,…
#       第 4 行起    数据，逗号分隔
#
#  ③ Element2 / SF-ICP-MS（TERMITE 原始示例）
#       结构同 ②，但分隔符是制表符，表头带分辨率后缀（如 Ca43(LR)）
#
#  识别方式**不依赖固定行号**：靠内容匹配定位表头和第一条数据行，
#  见 termite_probe()。这与用户既有的 Python 实现（upb/io/qtegra.py）一致。
# =============================================================================


# -----------------------------------------------------------------------------
# 1. 同位素命名
# -----------------------------------------------------------------------------

#' 同位素名规范化：统一成「元素在前」的写法（数据库用的是这种）
#'   25Mg → Mg25 ； 238U → U238 ； Mg25 原样返回
termite_normalize_isotope <- function(x) {
  x <- trimws(as.character(x))
  out <- sub("^([0-9]+)([A-Za-z]{1,2})$", "\\2\\1", x)
  out
}

#' 判断一套表头用的是哪种写法
#'   "mass-first"     25Mg     （Qtegra）
#'   "element-first"  Mg25     （TERMITE 示例）
#'   "unknown"
termite_isotope_style <- function(x) {
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (!length(x)) return("unknown")
  mf <- mean(grepl("^[0-9]+[A-Za-z]{1,2}$", x))
  ef <- mean(grepl("^[A-Za-z]{1,2}[0-9]+$", x))
  if (mf >= 0.9) return("mass-first")
  if (ef >= 0.9) return("element-first")
  "unknown"
}


# -----------------------------------------------------------------------------
# 2. 探测：不读全文件，只读头部若干行
# -----------------------------------------------------------------------------

#' 猜分隔符：在候选分隔符里选能把这行切成最多字段的那个
.termite_sniff_sep <- function(line) {
  cands <- c(",", "\t", ";", " ")
  counts <- vapply(cands, function(s) length(strsplit(line, s, fixed = TRUE)[[1]]), 1L)
  cands[which.max(counts)]
}

#' 去掉行尾的空字段（Qtegra 表头末尾多一个逗号）
.trim_trailing_empty <- function(v) {
  v <- as.character(v)
  while (length(v) && !nzchar(trimws(v[length(v)]))) v <- v[-length(v)]
  v
}

#' 拆分一行；返回 NULL 表示这行不像数据行
.split_line <- function(line, sep) {
  if (is.na(line) || !nzchar(trimws(line))) return(NULL)
  .trim_trailing_empty(strsplit(line, sep, fixed = TRUE)[[1]])
}

#' 从首行里抽样品名
#'
#' Qtegra 形如 `SRM 610:08/06/2023 03:07:07 AM;`。
#' 判据是「冒号后面跟日期」，避免把普通含冒号的路径当样品名。
.termite_sample_name_from_line1 <- function(line1) {
  if (is.null(line1) || is.na(line1)) return(NA_character_)
  s <- sub("^\\ufeff", "", line1)
  m <- regmatches(s, regexec("^\\s*(.+?)\\s*:\\s*[0-9]{1,4}[/.-][0-9]{1,2}[/.-][0-9]{1,4}",
                             s))[[1]]
  if (length(m) == 2L) return(trimws(m[2]))
  NA_character_
}

#' 探测一个原始数据文件的格式
#'
#' @param path 文件路径
#' @param head_lines 读多少行用于探测（默认 80，足够覆盖 Qtegra 的 13 行元数据）
#' @return 列表，含 format / sep / header_line / signal_line / n_iso /
#'         isotopes_raw / isotope_style / sample_name / dwell / unit_row / notes
termite_probe <- function(path, head_lines = 80L) {
  if (!file.exists(path)) stop("文件不存在：", path)
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  lines <- tryCatch(readLines(con, n = head_lines, warn = FALSE),
                    error = function(e) character(0))
  if (!length(lines)) stop("文件为空：", path)
  lines[1] <- sub("^\\ufeff", "", lines[1])

  notes <- character(0)

  # ---- 定位表头行：第 1 个字段以 time 开头，且至少有 3 个字段 ----
  header_line <- NA_integer_
  sep <- NA_character_
  hdr <- NULL
  for (i in seq_along(lines)) {
    for (s in c(",", "\t", ";")) {
      v <- .split_line(lines[i], s)
      if (is.null(v) || length(v) < 3L) next
      if (grepl("^time", tolower(trimws(v[1])))) {
        header_line <- i; sep <- s; hdr <- v
        break
      }
    }
    if (!is.na(header_line)) break
  }
  if (is.na(header_line))
    stop("在文件前 ", head_lines, " 行里找不到以 Time 开头的表头行：", basename(path))

  # 分隔符复核：用整行里出现次数最多的那个（表头已经给出很强的信号，
  # 但如果表头里同时有逗号和制表符，取字段更多的）
  if (length(hdr) < 3L) stop("表头解析失败：", basename(path))

  n_iso <- length(hdr) - 1L
  isotopes_raw <- trimws(hdr[-1])

  # ---- 定位第一条数据行：字段数一致且第 2 个字段是数字 ----
  signal_line <- NA_integer_
  probe_to <- min(length(lines), header_line + 10L)
  for (i in seq.int(header_line + 1L, probe_to)) {
    v <- .split_line(lines[i], sep)
    if (is.null(v) || length(v) < 2L) next
    if (abs(length(v) - (n_iso + 1L)) > 1L) next
    if (!is.na(suppressWarnings(as.numeric(v[2])))) { signal_line <- i; break }
  }
  if (is.na(signal_line))
    stop("在表头之后找不到数值数据行：", basename(path))

  # ---- 表头与数据之间的那几行（Qtegra 的 dwell/xcal 行在这里）----
  unit_rows <- if (signal_line > header_line + 1L)
    lines[seq.int(header_line + 1L, signal_line - 1L)] else character(0)

  # dwell time（Qtegra）：取所有出现过的值，去重
  dwell <- numeric(0)
  if (length(unit_rows)) {
    txt <- paste(unit_rows, collapse = " ")
    m <- regmatches(txt, gregexpr("dwell\\s*time\\s*=\\s*[0-9.eE+-]+", txt))[[1]]
    if (length(m)) {
      dwell <- unique(suppressWarnings(
        as.numeric(sub("^dwell\\s*time\\s*=\\s*", "", m))))
      dwell <- dwell[is.finite(dwell)]
    }
  }

  # ---- 样品名 ----
  sample_name <- .termite_sample_name_from_line1(lines[1])
  if (is.na(sample_name)) {
    # TERMITE 示例第 1 行是采集信息/路径，退回用文件名
    sample_name <- tools::file_path_sans_ext(basename(path))
    notes <- c(notes, "首行不是「样品名:日期」，样品名改用文件名")
  }

  # ---- 格式归类 ----
  fmt <- "unknown"
  if (length(unit_rows) && length(dwell)) fmt <- "Qtegra"
  else if (identical(sep, "\t"))               fmt <- "Element2"
  else if (any(grepl("Intensity Vs Time", lines[seq_len(min(5L, length(lines)))],
                     fixed = TRUE)))           fmt <- "Agilent"
  else                                          fmt <- "generic"

  style <- termite_isotope_style(isotopes_raw)
  if (identical(style, "unknown"))
    notes <- c(notes, "同位素表头写法无法识别（既不是 25Mg 也不是 Mg25）")

  db <- termite_normalize_isotope(isotopes_raw)

  list(path = path, file = basename(path),
       format = fmt, sep = sep,
       header_line = header_line, signal_line = signal_line,
       unit_rows = unit_rows,
       n_iso = n_iso,
       isotopes_raw = isotopes_raw,
       isotopes_db = db,
       isotope_style = style,
       sample_name = sample_name,
       dwell = dwell,
       first_line = lines[1],
       notes = notes)
}


# -----------------------------------------------------------------------------
# 3. 统一读取
# -----------------------------------------------------------------------------

#' 按探测结果读一个文件的数据区
#'
#' @return list(time, values(n×p 数值矩阵), probe)
termite_read_probed <- function(path, probe = NULL, n_sweeps = NA_integer_,
                                verbose = FALSE) {
  if (is.null(probe)) probe <- termite_probe(path)
  skip <- probe$signal_line - 1L
  take <- if (is.null(n_sweeps) || is.na(n_sweeps)) -1L else
    as.integer(n_sweeps) - skip
  df <- utils::read.table(path, header = FALSE, sep = probe$sep, skip = skip,
                          nrows = take, colClasses = "character", fill = TRUE,
                          quote = "", comment.char = "", blank.lines.skip = TRUE,
                          check.names = FALSE)
  need <- probe$n_iso + 1L
  if (ncol(df) < need)
    stop(sprintf("%s：数据区只有 %d 列，表头声明了 %d 列。",
                 probe$file, ncol(df), probe$n_iso))
  # 第 1 列是时间，同位素从第 2 列开始
  raw <- as.matrix(df[, seq.int(2L, need), drop = FALSE])
  m <- matrix(termite_sanitize(as.vector(raw)), nrow = nrow(raw), ncol = ncol(raw))
  if (verbose) message(sprintf("%s: %d 行 × %d 列", probe$file, nrow(m), ncol(m)))
  list(time = termite_sanitize(df[[1]]), values = m, probe = probe, n_lines = nrow(m) + skip)
}


# -----------------------------------------------------------------------------
# 4. 目录扫描与分类
# -----------------------------------------------------------------------------

.termite_ext_re <- "\\.(csv|asc|txt|CSV|ASC|TXT|TXT)$"

#' 扫描一个目录，逐个探测文件格式与样品名
#'
#' @param dir 目录
#' @param pattern 文件名正则（默认 csv/asc/txt）
#' @param recursive 是否递归
#' @param probe_all 是否逐个探测（TRUE 才能拿到样品名/格式；FALSE 只列文件名）
termite_scan_dir <- function(dir, pattern = .termite_ext_re, recursive = FALSE,
                             probe_all = TRUE) {
  if (!dir.exists(dir))
    return(data.frame(file = character(0), path = character(0),
                      sample_name = character(0), format = character(0),
                      n_iso = integer(0), header_line = integer(0),
                      signal_line = integer(0), sep = character(0),
                      stringsAsFactors = FALSE))
  fs <- sort(list.files(dir, pattern = pattern, full.names = TRUE,
                        recursive = recursive))
  if (!length(fs))
    return(data.frame(file = character(0), path = character(0),
                      sample_name = character(0), format = character(0),
                      n_iso = integer(0), header_line = integer(0),
                      signal_line = integer(0), sep = character(0),
                      stringsAsFactors = FALSE))
  rows <- lapply(fs, function(f) {
    if (!probe_all)
      return(list(file = basename(f), path = f, sample_name = tools::file_path_sans_ext(basename(f)),
                  format = NA_character_, n_iso = NA_integer_, header_line = NA_integer_,
                  signal_line = NA_integer_, sep = NA_character_, ok = TRUE, msg = ""))
    p <- tryCatch(termite_probe(f), error = function(e) e)
    if (inherits(p, "error"))
      return(list(file = basename(f), path = f, sample_name = NA_character_,
                  format = "ERROR", n_iso = NA_integer_, header_line = NA_integer_,
                  signal_line = NA_integer_, sep = NA_character_,
                  ok = FALSE, msg = conditionMessage(p)))
    list(file = basename(f), path = f, sample_name = p$sample_name,
         format = p$format, n_iso = p$n_iso, header_line = p$header_line,
         signal_line = p$signal_line, sep = p$sep,
         ok = TRUE, msg = paste(p$notes, collapse = "; "))
  })
  do.call(rbind, lapply(rows, function(r)
    data.frame(file = r$file, path = r$path, sample_name = r$sample_name,
               format = r$format, n_iso = r$n_iso, header_line = r$header_line,
               signal_line = r$signal_line, sep = r$sep,
               ok = r$ok, msg = r$msg, stringsAsFactors = FALSE)))
}

#' 套用别名表
#'
#' 表里是「仪器里的写法 → 参考表里的键」。
#' 既用于参考物质名称（SRM 610 → NIST610），也用于同位素（In115 → In116）。
#' 表里没有的名字原样返回。
termite_apply_alias <- function(x, alias_tab = NULL) {
  if (is.null(alias_tab)) return(x)
  i <- match(tolower(trimws(x)), tolower(alias_tab$alias))
  out <- x
  hit <- !is.na(i)
  out[hit] <- alias_tab$target[i[hit]]
  out
}

#' 读别名表（两列：alias,target；`#` 开头是注释）
termite_read_alias <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !file.exists(path)) return(NULL)
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                                comment.char = "#", strip.white = TRUE),
                error = function(e) NULL)
  if (is.null(d) || ncol(d) < 2L) return(NULL)
  names(d)[1:2] <- c("alias", "target")
  d <- d[!is.na(d$alias) & !is.na(d$target), c("alias", "target"), drop = FALSE]
  d$alias <- trimws(d$alias); d$target <- trimws(d$target)
  d[nzchar(d$alias) & nzchar(d$target), , drop = FALSE]
}


#' 定位随程序分发的资源文件（同位素表、推荐值表、别名表）
#'
#' 这些是**程序资源**，不属于用户的数据目录。数据目录可以指向任意位置
#' （例如 `G:/3.珊瑚钨锡矿床/.../20230806CKDA`），资源仍需能找到。
#' 解析顺序：
#'   1. 原样（绝对路径，或相对当前工作目录）
#'   2. 数据目录 cfg$dir 下
#'   3. 程序目录 cfg$app_dir 下
#'   4. 程序目录下的 your_main_directory/
termite_resource <- function(path, cfg) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) return(NA_character_)
  if (file.exists(path)) return(path)
  cands <- character(0)
  if (!is.null(cfg$dir) && nzchar(cfg$dir)) cands <- c(cands, file.path(cfg$dir, path))
  if (!is.null(cfg$app_dir) && nzchar(cfg$app_dir)) {
    cands <- c(cands, file.path(cfg$app_dir, path),
               file.path(cfg$app_dir, "your_main_directory", path))
  }
  hit <- cands[file.exists(cands)]
  if (length(hit)) return(hit[1])
  path    # 都不存在：原样返回，让下游报出清晰的错误
}


#' 逐文件找出「信号最强的同位素」，用于判断内标该选谁
#'
#' 一批样品可能来自不同基体（例如锡石以 Sn 为主、白钨矿以 W 为主），
#' 一个大小的内标含量套不住全部。这里把每个文件在信号窗内的净计数排序，
#' 让用户一眼看出该文件属于哪一类，再决定内标与含量。
#'
#' @param i1..i4 空白/信号区间（**文件真实行号**，与 cfg 里的口径一致）
#' @param top    每个文件列出前几个
#' @return data.frame(file, sample_name, rank, isotope, net_cps, snr)
termite_suggest_is <- function(plan, cfg, i1, i2, i3, i4, top = 3L) {
  rows <- lapply(seq_len(nrow(plan)), function(k) {
    base <- data.frame(file = plan$file[k], sample_name = plan$sample_name[k],
                       kind = plan$kind[k], stringsAsFactors = FALSE)
    res <- tryCatch({
      pr  <- termite_probe(plan$path[k])
      r   <- termite_read_probed(plan$path[k], pr)
      off <- pr$signal_line - 1L
      nr  <- nrow(r$values)
      b1 <- max(1L, i1 - off); b2 <- min(nr, i2 - off)
      s1 <- max(1L, i3 - off); s2 <- min(nr, i4 - off)
      if (s2 <= s1 || b2 <= b1) stop("积分窗口超出文件范围")
      bl  <- .col_medians(r$values[b1:b2, , drop = FALSE], na.rm = TRUE)
      net <- r$values[s1:s2, , drop = FALSE] -
             matrix(bl, nrow = s2 - s1 + 1L, ncol = ncol(r$values), byrow = TRUE)
      mu  <- colMeans(net, na.rm = TRUE)
      ord <- order(mu, decreasing = TRUE)
      jj  <- ord[seq_len(min(top, length(ord)))]
      data.frame(base,
                 rank = seq_along(jj),
                 isotope = pr$isotopes_raw[jj],
                 net_cps = as.numeric(mu[jj]),
                 snr = as.numeric(mu[jj]) / pmax(bl[jj], 1),
                 note = "",
                 stringsAsFactors = FALSE)
    }, error = function(e)
      data.frame(base, rank = NA_integer_, isotope = NA_character_,
                 net_cps = NA_real_, snr = NA_real_,
                 note = conditionMessage(e), stringsAsFactors = FALSE))
    res
  })
  do.call(rbind, rows)
}
