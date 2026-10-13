# =============================================================================
#  积分窗口 —— 读原始信号、自动定位剥蚀平台、评估窗口质量
#  ---------------------------------------------------------------------------
#  为什么需要「自动定窗 / 手动选窗」：
#  归算链上唯一由人工划定、不依赖任何标定的输入就是积分窗口。现行做法是
#  全批共用一个写死的信号窗（本批 = 文件行 100-158）。这在两类情况下出错：
#    ① 各测点剥蚀时长本来不同 —— 提前熄火的点，窗口右半截落到了熄火之后，
#       把背景水平平均进来，该点全部元素被系统性压低；
#    ② 信号平台本身随时间下降（剥蚀坑变深的分馏效应），窗口落在平台前段
#       还是后段，结果能差百分之十几。
#  所以窗口不该是常量，而应从每条曲线自己的形态里量出来。
#
#  为什么放在 R/ 而不是 tools/：
#  Shiny 由 app.R 装载 R/*.R，界面里的「微量元素处理 · 区间选择」页要用同一套
#  定窗逻辑。tools/window_lib.R 现在只是薄壳，把本文件 source 进来，保证
#  「界面上画的窗口」「CLI 脚本算的窗口」「报告里写的窗口」永远是同一份代码。
#
#  本文件只做三件事：读信号、从信号里量出剥蚀起止、评估任一给定窗口的质量。
#  它不碰归算，也不设全局状态。绘图在 R/termite_plots.R 的 termite_plot_window()。
# =============================================================================

# ---- 转义码守卫 -------------------------------------------------------------
# Bash 工具会注入 LC_ALL=C.UTF-8（Windows 无此 locale），R 会回退到 "C"/ASCII，
# 之后 readLines/source(encoding="UTF-8") 里的中文会转码失败、整行被吞掉。
window_utf8_guard <- function() {
  if (!l10n_info()[["UTF-8"]])
    for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
      if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break
  invisible(TRUE)
}

# ---- 行号口径 ---------------------------------------------------------------
# 文件真实行号（含表头、从 1 计）与数据矩阵行号之间的换算，与内核
# R/termite_core.R 的 .to_matrix_row() 同式。两处必须一致，否则画出来的窗口
# 不是归算实际用的窗口。
window_row_of  <- function(line_no, signal_line) as.integer(line_no) - (as.integer(signal_line) - 1L)
window_line_of <- function(row_no,  signal_line) as.integer(row_no)  + (as.integer(signal_line) - 1L)

# 行内字段数不齐时右侧补 NA —— Qtegra 导出偶尔少尾列，read.table 会直接报错
.pad_fields <- function(v, n) {
  if (length(v) < n) v <- c(v, rep(NA_character_, n - length(v)))
  v[seq_len(n)]
}

# 滑窗「连续 k 个真」的起点 / 终点，用前缀和向量化（不写 for）
.first_run <- function(v, k) {
  n <- length(v)
  if (n < k || k < 1L) return(NA_integer_)
  cs <- cumsum(c(0L, as.integer(v)))
  w  <- cs[(k + 1L):(n + 1L)] - cs[1L:(n - k + 1L)]   # w[i] = sum(v[i:(i+k-1)])
  hit <- which(w == k)
  if (length(hit)) hit[1L] else NA_integer_
}
.last_run <- function(v, k) {
  n <- length(v)
  if (n < k || k < 1L) return(NA_integer_)
  cs <- cumsum(c(0L, as.integer(v)))
  w  <- cs[(k + 1L):(n + 1L)] - cs[1L:(n - k + 1L)]
  hit <- which(w == k)
  if (length(hit)) hit[length(hit)] + k - 1L else NA_integer_
}

# =============================================================================
#  读信号
# =============================================================================

#' 读一个原始文件的表头（返回同位素名，**保持文件里的原始写法**）
#'
#' 必须保持原始写法：window_read_point() 是拿它去 match() 表头的，
#' 若在这里规范化成「元素+质量数」，反而会找不到列。
window_read_header <- function(path, header_line = 14L, sep = ",") {
  ln <- sub("\r$", "", readLines(path, encoding = "UTF-8", n = max(header_line, 1L), warn = FALSE))
  if (length(ln) < header_line) stop("文件行数不足（", length(ln), " < ", header_line, "）：", basename(path))
  trimws(strsplit(ln[header_line], sep, fixed = TRUE)[[1L]])[-1L]
}

#' 读单个测点
#'
#' @param path        单个原始文件
#' @param chs         要读的通道名（**文件表头里的原始写法**，如 "29Si"）
#' @param header_line 表头所在文件行号（Qtegra = 14）
#' @param signal_line 数据起始文件行号（Qtegra = 16；第 15 行是 dwell/xcal 参数行）
#' @param sep         字段分隔符，默认逗号；Qtegra 导出另有制表符/分号的情况
#' @return list(file, name, t, y = n×length(chs) 矩阵（cps）, n)
window_read_point <- function(path, chs = "29Si", header_line = 14L, signal_line = 16L, sep = ",") {
  ln <- sub("\r$", "", readLines(path, encoding = "UTF-8", warn = FALSE))
  if (length(ln) < signal_line) stop("文件行数不足（", length(ln), " < ", signal_line, "）：", basename(path))
  name <- trimws(sub(":.*$", "", ln[1L]))                       # 首行 "样品名:日期;"
  hdr  <- trimws(strsplit(ln[header_line], sep, fixed = TRUE)[[1L]])
  jch  <- match(chs, hdr)
  if (anyNA(jch)) stop("表头找不到通道 ", paste(chs[is.na(jch)], collapse = ", "), "：", basename(path))
  body <- ln[signal_line:length(ln)]
  body <- body[nzchar(trimws(body))]
  if (!length(body)) stop("没有数据行：", basename(path))
  want <- c(1L, jch)
  mm <- t(vapply(strsplit(body, sep, fixed = TRUE),
                 function(v) suppressWarnings(as.numeric(.pad_fields(v, length(hdr))[want])),
                 numeric(length(want))))
  colnames(mm) <- c("Time", chs)
  list(file = basename(path), name = name, t = mm[, 1L],
       y = mm[, -1L, drop = FALSE], n = nrow(mm))
}

#' 按目录批量读（按文件名里的序号排序，避免字典序把 10 排到 2 前面）
window_scan_dir <- function(dir, chs = "29Si", header_line = 14L, signal_line = 16L,
                            pattern = "[.]csv$", sep = ",") {
  fs <- list.files(dir, pattern = pattern)
  if (!length(fs)) stop("目录里没有匹配 ", pattern, " 的文件：", dir)
  ord <- suppressWarnings(as.integer(sub("^.*_(\\d+)\\.[A-Za-z]+$", "\\1", fs)))
  fs  <- fs[order(ord, fs)]
  lapply(file.path(dir, fs), window_read_point,
         chs = chs, header_line = header_line, signal_line = signal_line, sep = sep)
}

# =============================================================================
#  背景水平
# =============================================================================

#' 曲线自身的背景水平：前段（默认前 20% 行）的稳健中位数
#'
#' 这里有一个极易搞混的地方，就是**它不等于「配置的背景窗中位数」**：
#'   · 本函数取的是剥蚀开始**之前**的前段水平，用途是判断剥蚀有没有开始
#'     （阈值 hi_k·b），以及作为信背比的分母；
#'   · 归算真正扣掉的是 cfg$first_blank..last_blank 那段的中位数。
#' 两者在正常数据上接近，但语义不同，报数时不要混用。
#' 前段中位数非正（整段基线为 0）时退回「最小正值」，否则阈值判定会全部失效。
.window_bg_level <- function(y, bg_frac = 0.20) {
  n <- length(y)
  pre <- seq_len(max(3L, floor(n * bg_frac)))
  b <- stats::median(y[pre], na.rm = TRUE)
  if (!is.finite(b) || b <= 0) {
    pos <- y[y > 0]
    b <- if (length(pos)) min(pos, na.rm = TRUE) else NA_real_
  }
  b
}

# =============================================================================
#  窗口质量指标
# =============================================================================

#' 评估任意一组窗口的质量
#'
#' 自动定窗与界面上的手动拖动必须共用这一套指标：否则「手动窗口好不好」只能靠
#' 肉眼判断，而且两边报出来的信背比、RSD 会对不上，界面就失去了参考价值。
#' 所以 window_auto_detect() 最后也是从这里取数，不另算一遍。
#'
#' @param y       单通道净信号（cps），长度 = 数据行数
#' @param bg1,bg2 背景窗起止（**数据行号**，含两端）
#' @param s1,s2   信号窗起止（**数据行号**，含两端）
#' @param sw      平滑窗宽（强制成 >=3 的奇数）
#' @param sm      已算好的平滑信号；给了就复用，省一次 runmed
#' @param bg_frac 前段水平所占比例（只在 b 未给时用来算信背比的分母）
#' @param b       背景水平（信背比的分母）。默认 NULL → 内部按 bg_frac 现算。
#'                信背比的分母是**前段水平**而不是背景窗中位数——这是原脚本的
#'                口径，改了会让报告里的信背比整列变数，所以必须显式传、不能默认换。
#' @return list(bg_med, bg_win_med, sig_med, snr, rsd, rsd_dt, n_bg, n_sig)
window_stats <- function(y, bg1, bg2, s1, s2, sw = 5L, sm = NULL,
                         bg_frac = 0.20, b = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  seg <- function(v, a, b) {                       # 夹到 [1, length(v)]，越界返回空
    if (length(a) != 1L || length(b) != 1L) return(numeric(0))
    if (!is.finite(a) || !is.finite(b)) return(numeric(0))
    a <- max(1L, as.integer(a)); b <- min(length(v), as.integer(b))
    if (b < a) numeric(0) else v[a:b]
  }
  if (is.null(sm)) {
    k <- max(3L, as.integer(sw)); if (k %% 2L == 0L) k <- k + 1L
    sm <- if (n >= k) suppressWarnings(stats::runmed(y, k, endrule = "median")) else y
  }
  bgv <- seg(y, bg1, bg2)
  sgv <- seg(y, s1, s2)
  if (is.null(b)) b <- .window_bg_level(y, bg_frac)
  # 背景窗内的中位数另算一列：它才是归算真正扣掉的量，与 b（前段水平）是两个东西
  bwm <- if (length(bgv)) stats::median(bgv, na.rm = TRUE) else NA_real_
  mp  <- if (length(sgv)) stats::median(sgv, na.rm = TRUE) else NA_real_
  snr <- if (is.finite(mp) && is.finite(b) && b > 0) mp / b else NA_real_
  # 平滑信号算 RSD 与去趋势 RSD：波动主要是高频噪声
  sms <- seg(sm, s1, s2)
  rsd <- if (is.finite(mp) && mp != 0) stats::sd(sms, na.rm = TRUE) / mp else NA_real_
  # 去趋势 RSD：平台本身单调下降是分馏造成的系统项，扣掉趋势后剩下的才是真噪声
  ii <- seq_along(sms)
  dt <- tryCatch(stats::resid(stats::lm(sms ~ ii)), error = function(e) rep(NA_real_, length(ii)))
  rsd_dt <- if (is.finite(mp) && mp != 0) stats::sd(dt, na.rm = TRUE) / mp else NA_real_
  list(bg_med = b, bg_win_med = bwm, sig_med = mp, snr = snr,
       rsd = rsd, rsd_dt = rsd_dt, n_bg = length(bgv), n_sig = length(sgv))
}

# =============================================================================
#  自动定窗
# =============================================================================
#  思路（全部只依赖信号自身形态，不需要任何标定参数）：
#    1) 背景水平 b：取前段（默认前 20% 行）的稳健中位数；
#    2) 剥蚀开始 on：平滑信号首次「连续 run 点」高于 hi_k·b；
#    3) 剥蚀结束 off：平滑信号最后一次「连续 run 点」高于 lo_k·b；
#    4) 信号窗 = 平台中段，左留 lead 行跳过上升沿稳定期，右留 tail 行避开熄火；
#    5) 背景窗 = 剥蚀开始前、剔除预触发尖峰后的一段。
#
#  用「连续 k 点」而不是单点越阈，是为了让激光开启瞬间的孤立尖峰（本批在
#  上升沿前约 1 行处确实存在）无法触发 on，否则平台会被判早、窗口整体前移。
#
#  @return 单行 data.frame（数据行口径 + 文件行口径一并给出）
window_auto_detect <- function(t, y, signal_line = 16L,
                               sw = 5L, hi_k = 5, lo_k = 3, run = 3L,
                               lead = 6L, tail = 8L, bg_frac = 0.20,
                               bg_span = 0.50, label = NULL) {
  n <- length(y)
  if (n < 12L) stop("数据行太少（", n, "），无法定窗")
  y <- as.numeric(y)

  # --- 1) 背景水平（与 window_stats 共用同一个实现，避免两处口径漂）---
  b <- .window_bg_level(y, bg_frac)

  # --- 2) 平滑（中位平滑抗离群；k 取奇数）---
  k <- max(3L, as.integer(sw))
  if (k %% 2L == 0L) k <- k + 1L
  sm <- if (n >= k) suppressWarnings(stats::runmed(y, k, endrule = "median")) else y

  # --- 3) 剥蚀起止 ---
  on  <- .first_run(sm > hi_k * b, as.integer(run))
  off <- .last_run(sm > lo_k * b,  as.integer(run))

  na_out <- function(reason) data.frame(
    样品 = if (is.null(label)) NA_character_ else label, 数据行 = n,
    剥蚀起_行 = NA_integer_, 剥蚀止_行 = NA_integer_,
    信号窗起_行 = NA_integer_, 信号窗止_行 = NA_integer_,
    背景窗起_行 = NA_integer_, 背景窗止_行 = NA_integer_,
    剥蚀起_s = NA_real_, 剥蚀止_s = NA_real_,
    信号窗起_s = NA_real_, 信号窗止_s = NA_real_,
    背景中位 = round(b, 1), 平台中位 = NA_real_, 信背比 = NA_real_,
    窗内RSD = NA_real_, 去趋势RSD = NA_real_, 平台宽 = NA_integer_,
    判定 = reason, stringsAsFactors = FALSE)

  if (is.na(on) || is.na(off) || off <= on) return(na_out("未检出剥蚀段"))
  if (!is.finite(b) || b <= 0)              return(na_out("背景水平非正"))

  # --- 4) 背景窗：剥蚀开始前，剔掉紧贴上升沿的预触发尖峰 ---
  # （先算背景窗再算信号窗，只是为了让 window_stats() 一次拿齐四个边界；
  #   两者互不依赖，顺序变了不影响数值。）
  bg_end <- as.integer(on - 1L)
  while (bg_end > 1L && isTRUE(sm[bg_end] > 2 * b)) bg_end <- bg_end - 1L
  bg_len   <- max(5L, as.integer(round((off - on) * bg_span)))
  bg_start <- max(1L, bg_end - bg_len + 1L)
  if (bg_end <= bg_start) { bg_start <- 1L; bg_end <- max(1L, on - 1L) }

  # --- 5) 信号窗：平台中段 ---
  s1 <- as.integer(on + lead)
  s2 <- as.integer(off - tail)
  if (s2 <= s1) {                       # 平台太短，收紧余量再试一次
    s1 <- as.integer(on + max(1L, round((off - on) * 0.15)))
    s2 <- as.integer(off - max(1L, round((off - on) * 0.15)))
  }
  if (s2 <= s1) return(na_out("平台过短"))

  st  <- window_stats(y, bg_start, bg_end, s1, s2, sw = k, sm = sm,
                      bg_frac = bg_frac, b = b)
  mp  <- st$sig_med; snr <- st$snr; rsd <- st$rsd; rsd_d <- st$rsd_dt

  # --- 判定标签 -------------------------------------------------------------
  # 只把「窗口划错了」这一类放进判定。平台波动（去趋势 RSD）是信号与样品自身的
  # 特性，窗口划得再准也改不掉它：这批锆石整组的 RSD 都在 10~20%
  # （中位仅 3.4%），那是样品本身剥蚀不稳，不是窗口问题。混进判定会让
  # 「需人工确认」失去指向性。RSD 仍作为两列指标输出，供判读信号质量。
  verdict <- "正常"
  if (is.na(snr) || snr < 3) verdict <- "信背比不足"

  data.frame(
    样品 = if (is.null(label)) NA_character_ else label, 数据行 = n,
    剥蚀起_行 = on, 剥蚀止_行 = off,
    信号窗起_行 = s1, 信号窗止_行 = s2,
    背景窗起_行 = bg_start, 背景窗止_行 = bg_end,
    剥蚀起_s = round(t[on], 2), 剥蚀止_s = round(t[off], 2),
    信号窗起_s = round(t[s1], 2), 信号窗止_s = round(t[s2], 2),
    背景中位 = round(b, 1), 平台中位 = round(mp, 1), 信背比 = round(snr, 2),
    窗内RSD = round(rsd, 4), 去趋势RSD = round(rsd_d, 4), 平台宽 = off - on + 1L,
    判定 = verdict, stringsAsFactors = FALSE)
}

#' 批处理：给每个测点定窗，并把行号换算成文件行号
#' @param pts window_scan_dir() 的产物
#' @param ch  pts[[i]]$y 里用来定窗的通道名（默认第一个）
window_auto_batch <- function(pts, ch = NULL, signal_line = 16L, ...) {
  sig_line <- as.integer(signal_line)
  ch <- if (is.null(ch)) colnames(pts[[1L]]$y)[1L] else ch
  out <- do.call(rbind, lapply(seq_along(pts), function(i) {
    p <- pts[[i]]
    d <- window_auto_detect(p$t, p$y[, ch], signal_line = sig_line, label = p$name, ...)
    cbind(序号 = i, 文件 = p$file, d, stringsAsFactors = FALSE)
  }))
  # 行号口径：内核配置用文件真实行号，这里一并给出，免得下游再换算出错
  out$信号窗起_文件行 <- window_line_of(out$信号窗起_行, sig_line)
  out$信号窗止_文件行 <- window_line_of(out$信号窗止_行, sig_line)
  out$背景窗起_文件行 <- window_line_of(out$背景窗起_行, sig_line)
  out$背景窗止_文件行 <- window_line_of(out$背景窗止_行, sig_line)
  # 列序：先身份，再结论，再明细
  out[, c("序号", "文件", "样品", "数据行",
          "信号窗起_文件行", "信号窗止_文件行", "信号窗起_行", "信号窗止_行",
          "背景窗起_文件行", "背景窗止_文件行", "背景窗起_行", "背景窗止_行",
          "剥蚀起_s", "剥蚀止_s", "信号窗起_s", "信号窗止_s",
          "背景中位", "平台中位", "信背比", "窗内RSD", "去趋势RSD", "平台宽", "判定")]
}

# =============================================================================
#  折成「全批统一窗口」
# =============================================================================
#  内核对所有文件只接受同一组窗口，所以必须把逐点窗口折成一对数字。
#  q 越大越保守：左边界取高分位（大部分点已进入剥蚀）、右边界取低分位
#  （大部分点尚未熄火）。

#' 折一档统一窗口
#' @param s1,s2 逐点信号窗起止（口径由调用方保证一致）
#' @return data.frame(口径, 信号窗起, 信号窗止) —— 两行：严格 / 稳健
window_fold <- function(s1, s2, q = c(0.90, 0.10)) {
  data.frame(
    口径     = c("严格（覆盖全部测点）", "稳健（覆盖 90% 测点）"),
    信号窗起 = c(max(s1, na.rm = TRUE),
                 stats::quantile(s1, q[1L], na.rm = TRUE, names = FALSE)),
    信号窗止 = c(min(s2, na.rm = TRUE),
                 stats::quantile(s2, q[2L], na.rm = TRUE, names = FALSE)),
    stringsAsFactors = FALSE)
}

#' 统一窗口候选（含一档占位「现行（配置值）」，由调用方回填）
window_union <- function(res, q = c(0.90, 0.10)) {
  f <- window_fold(res$信号窗起_行, res$信号窗止_行, q)
  data.frame(
    口径 = c(f$口径, "现行（配置值）"),
    信号窗起_行 = c(f$信号窗起, NA_integer_),
    信号窗止_行 = c(f$信号窗止, NA_integer_),
    stringsAsFactors = FALSE)
}
