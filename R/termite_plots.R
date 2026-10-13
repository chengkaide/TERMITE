# =============================================================================
#  TERMITE 绘图 —— 纯 base graphics，不依赖 ggplot2
# =============================================================================

.termite_theme <- list(
  bg      = "#ffffff",       # 画布底色
  line    = "#2b6cb0",       # 主曲线 / 数据点
  out     = "#c53030",       # 离群 / 可疑点
  mean    = "#2f855a",       # 均值线 / 合并 RSF
  muted   = "#718096",       # 辅助线、说明文字
  grid    = "#e2e8f0",       # 网格
  fg      = "#1a202c",       # 坐标轴与标签
  blank   = "#3182ce18",     # 空白区底色（最后两位是 alpha）
  signal  = "#e53e3e14"      # 信号区底色
)

#' 面板太多时截断，并给出说明
#'
#' 58 个同位素铺满一页会小到看不清，某些设备上还会直接报
#' "figure margins too large"。统一在这里限制面板数。
.limit_panels <- function(idx, max_panels) {
  if (length(idx) <= max_panels) return(list(idx = idx, note = ""))
  list(idx = idx[seq_len(max_panels)],
       note = sprintf("　（共 %d 个，仅显示前 %d 个）", length(idx), max_panels))
}

# ---- 下面四个helper是四个多面板图共用的样板，原先在每处重复一遍 ----

#' 开启画布（返回原来的 par，交给调用方 on.exit(par(op)) 恢复）
#'
#' @param mfrow 多面板布局 c(行, 列)；NULL 表示单面板
.begin_canvas <- function(mar, mfrow = NULL, mgp = NULL, cex.main = 0.95) {
  op <- par(no.readonly = TRUE)
  par(mar = mar, bg = .termite_theme$bg, fg = .termite_theme$fg,
      col.axis = .termite_theme$fg, col.lab = .termite_theme$fg, cex.main = cex.main)
  if (!is.null(mfrow)) par(mfrow = as.integer(mfrow), mgp = mgp)
  op
}

#' 多面板布局：每行列数上限 ncol，面板数不足时收缩成一行，不留空位
.panel_layout <- function(p, ncol) {
  nc <- max(1L, min(ncol, p))
  c(ceiling(p / nc), nc)
}

#' 收尾：整体说明写在整块图的下边缘（与单个面板标题区分开）
.end_panels <- function(main, cex = 0.9) {
  mtext(main, outer = TRUE, line = -1.4, cex = cex, col = .termite_theme$muted)
}

#' 面板 y 轴范围
#'
#' 几类退化情况要兜住，否则 plot() 会报 "cannot set infinite ylim"：
#'   · 整列 NA        → 给一个占位区间
#'   · 取值全部相同   → 按 expand 比例上下撑开
#'   · 含非正值       → 剔掉（对数轴下必须剔；原始信号图也一律剔）
.panel_ylim <- function(y, drop_nonpos = TRUE, expand = c(0.5, 2)) {
  if (drop_nonpos) y[y <= 0] <- NA
  fin <- y[is.finite(y)]
  if (!length(fin)) return(c(1, 10))
  rng <- range(fin)
  if (diff(rng) > 0) rng else rng * expand
}

#' 空白区 / 信号区的底色块（x 轴口径是文件真实行号）
.panel_bands <- function(blank, signal, ylim) {
  if (!is.null(blank))  rect(blank[1],  ylim[1], blank[2],  ylim[2],
                             col = .termite_theme$blank, border = NA)
  if (!is.null(signal)) rect(signal[1], ylim[1], signal[2], ylim[2],
                             col = .termite_theme$signal, border = NA)
}

#' 原始信号多面板图：每个同位素一格，标出空白区与信号区
#'
#' @param rows   要画的**矩阵行号**；NULL 表示全部。
#' @param offset 矩阵行号 → 文件真实行号的偏移（= signal_line - 1），
#'               用于让横轴、空白区、信号区三者口径一致。
termite_plot_raw <- function(values, isotopes, blank = NULL, signal = NULL,
                             main = "", log_y = TRUE, rows = NULL,
                             offset = 0L, xlab = "文件行号",
                             which_iso = NULL, max_panels = 16L) {
  n_all <- nrow(values)
  if (is.null(rows)) rows <- seq_len(n_all)
  rows <- rows[rows >= 1L & rows <= n_all]
  if (!length(rows)) rows <- seq_len(min(50L, n_all))
  if (!is.null(which_iso)) {
    jj <- match(which_iso, isotopes); jj <- jj[!is.na(jj)]
    if (length(jj)) { values <- values[, jj, drop = FALSE]; isotopes <- isotopes[jj] }
  }
  lim <- .limit_panels(seq_len(ncol(values)), max_panels)
  values <- values[, lim$idx, drop = FALSE]; isotopes <- isotopes[lim$idx]
  if (nzchar(lim$note)) main <- paste0(main, lim$note)
  v <- values[rows, , drop = FALSE]
  p <- ncol(v)
  op <- .begin_canvas(c(3.0, 3.6, 2.0, 0.7), .panel_layout(p, 4L), c(2.1, 0.6, 0))
  on.exit(par(op))
  x <- rows + offset
  for (j in seq_len(p)) {
    ylim <- .panel_ylim(v[, j], expand = c(0.5, 2))
    plot(NA, xlim = range(x), ylim = ylim, log = if (log_y) "y" else "",
         xlab = xlab, ylab = "cps", main = isotopes[j], las = 1,
         xaxs = "i", yaxs = "i", bty = "l")
    grid(col = .termite_theme$grid, lty = 1)
    .panel_bands(blank, signal, ylim)
    lines(x, v[, j], col = .termite_theme$line, lwd = 1)
    box(bty = "l")
  }
  .end_panels(main, cex = 0.95)
}

#' RSF 逐文件折线 + 均值线（对应原脚本 RSFused_*.pdf）
termite_plot_rsf <- function(res, main = "RSF（灵敏度因子）",
                             isotopes = NULL, max_panels = 16L) {
  d <- res$rsf_per_file
  if (!is.null(isotopes)) {
    jj <- match(isotopes, res$isotopes); jj <- jj[!is.na(jj)]
    if (length(jj)) d <- d[, jj, drop = FALSE]
  }
  lim <- .limit_panels(seq_len(ncol(d)), max_panels)
  d <- d[, lim$idx, drop = FALSE]
  if (nzchar(lim$note)) main <- paste0(main, lim$note)
  # 面板可能已被筛选/截断，标题与标注一律用 d 自己的列名，
  # 否则列号 j 会串到 res$isotopes 的全集上去
  iso_j <- colnames(d)
  brk <- cumsum(table(res$files$ref_rm))
  p <- ncol(d)
  op <- .begin_canvas(c(3.4, 3.6, 2.0, 0.7), .panel_layout(p, 4L), c(2.1, 0.6, 0))
  on.exit(par(op))
  for (j in seq_len(p)) {
    y <- d[, j]
    if (all(is.na(y))) { plot.new(); title(main = iso_j[j]); next }
    plot(seq_along(y), y, type = "b", pch = 16, cex = 0.7, col = .termite_theme$line,
         ylim = .panel_ylim(y, drop_nonpos = FALSE, expand = c(0.9, 1.1)),
         xlab = "", ylab = paste("RSF", element_of(iso_j[j])), main = iso_j[j],
         las = 1, xaxt = "n")
    axis(1, at = seq_along(y), labels = FALSE, tick = FALSE)
    abline(v = brk[-length(brk)] + 0.5, lty = 2, col = .termite_theme$muted)
    abline(h = res$rsf[iso_j[j]], lty = 3, lwd = 2, col = .termite_theme$mean)
    box(bty = "l")
  }
  .end_panels(main, cex = 0.95)
}

#' 检出限（对应原脚本 LoD_ReferenceMaterial_*.pdf）
termite_plot_lod <- function(res, main = "检出限 LoD") {
  y <- as.numeric(res$lod)
  op <- .begin_canvas(c(4.2, 4.4, 2.4, 1)); on.exit(par(op))
  plot(seq_along(y), y, log = "y", pch = 16, col = .termite_theme$line,
       xaxt = "n", xlab = "同位素", ylab = expression("LoD  [" * mu * "g/g]"),
       main = main, las = 1)
  axis(1, at = seq_along(y), labels = res$isotopes, las = 2, cex.axis = 0.8)
  grid(col = .termite_theme$grid, lty = 1)
  # 网格会盖在点上，重画一次让点回到上层
  points(seq_along(y), y, pch = 16, col = .termite_theme$line)
  box(bty = "l")
}

#' 点分析：逐元素浓度散点 + 相对标准偏差标红（对应原脚本 Results_*.pdf）
termite_plot_spot <- function(res, elements = NULL, max_panels = 16L) {
  el <- res$elements
  # 内标元素的浓度是输入值（不做测量），画出来没有意义，默认跳过
  keep <- if (is.null(elements)) which(el != el[res$is_idx]) else match(elements, el)
  keep <- keep[!is.na(keep)]
  lim  <- .limit_panels(keep, max_panels)
  keep <- lim$idx
  d    <- res$sample$conc_masked[, keep, drop = FALSE]
  p    <- ncol(d)
  op <- .begin_canvas(c(3.6, 4.0, 2.2, 0.7), .panel_layout(p, 4L), c(2.2, 0.6, 0))
  on.exit(par(op))
  for (k in seq_len(p)) {
    j <- keep[k]
    y <- d[, k]
    if (all(is.na(y))) { plot.new(); title(main = el[j]); next }
    plot(seq_along(y), y, type = "b", pch = 16, cex = 0.8, col = .termite_theme$line,
         xlab = "点位序号", ylab = expression("[" * mu * "g/g]"), main = el[j], las = 1)
    flag <- res$sample$rsd_flag[, j]
    flag[is.na(y)] <- FALSE
    if (any(flag)) points(which(flag), y[flag], col = .termite_theme$out, pch = 16, cex = 0.9)
    abline(h = res$lod[j], lty = 3, col = .termite_theme$muted)
    box(bty = "l")
  }
  .end_panels(paste0("红点 = 相对标准偏差高于均值（可疑不均匀点）", lim$note))
}

#' 线扫描：距离-浓度剖面
termite_plot_profile <- function(res, elements = NULL, idx = 1L, max_panels = 12L) {
  prof <- res$line[[idx]]
  el   <- res$elements
  keep <- if (is.null(elements)) which(el != el[res$is_idx]) else match(elements, el)
  keep <- keep[!is.na(keep)]
  lim  <- .limit_panels(keep, max_panels); keep <- lim$idx
  d <- prof$conc_masked[, keep, drop = FALSE]
  p <- ncol(d)
  op <- .begin_canvas(c(3.8, 4.2, 2.0, 0.7), .panel_layout(p, 3L), c(2.4, 0.6, 0))
  on.exit(par(op))
  for (k in seq_len(p)) {
    j <- keep[k]
    y <- d[, k]
    if (all(is.na(y))) { plot.new(); title(main = el[j]); next }
    plot(prof$distance, y, type = "l", col = .termite_theme$line, lwd = 1,
         xlab = "距离 [mm]", ylab = expression("[" * mu * "g/g]"), main = el[j], las = 1)
    grid(col = .termite_theme$grid, lty = 1)
    # 网格会盖在线上，重画一次让剖面回到上层
    lines(prof$distance, y, col = .termite_theme$line, lwd = 1)
    abline(h = res$lod[j], lty = 3, col = .termite_theme$muted)
    box(bty = "l")
  }
  .end_panels(sprintf("样品：%s　虚线 = LoD%s", prof$file, lim$note))
}

#' 背景（gas blank）柱状对比
termite_plot_blank <- function(res, files = 1L, which = c("ref", "sample")) {
  which <- match.arg(which)
  b <- termite_blank_of(res, which, files)
  if (is.null(b)) { plot.new(); title(main = "无背景数据"); return(invisible(NULL)) }
  op <- .begin_canvas(c(4.4, 4.4, 2.2, 0.8)); on.exit(par(op))
  barplot(b, names.arg = res$isotopes, las = 2, col = .termite_theme$line,
          border = NA, ylab = "blank cps",
          main = paste0("背景（gas blank）· ",
                        if (identical(which, "ref")) res$files$ref[files] else
                          if (identical(res$mode, "spot")) res$files$sample[files] else
                            res$line[[files]]$file),
          cex.names = 0.8)
  box(bty = "l")
}


# =============================================================================
#  积分窗口预览（界面「微量元素处理 · 区间选择」页用）
# =============================================================================

#' 单点信号曲线 + 窗口叠加预览
#'
#' 横轴用**文件真实行号**，不是矩阵行号也不是行内序号：这样界面上的滑块、拖框
#' 给出的行号可以**原样**写进归算参数（cfg$first_signal / last_signal），
#' 中间不需要任何换算——行号换算是这类窗口工具最容易出错的地方，一处错位整张
#' 图就与归算对不上，而且不会报错。上轴另标时间，供判读剥蚀时长。
#'
#' @param t,x   时间（s）与单通道净信号（cps），等长
#' @param x0    第一个数据点对应的文件行号（= cfg$signal_line）
#' @param bg,sig 当前选定的背景窗 / 信号窗（**文件行号**，长度 2）
#' @param auto  算法建议的信号窗（虚线参照），NULL 不画
#' @param cur   现行配置的信号窗（点线参照），NULL 不画
#' @return 隐式返回横轴坐标（文件行号），供调用方核对
termite_plot_window <- function(t, y, x0 = 1L, bg = NULL, sig = NULL,
                                auto = NULL, cur = NULL, main = "",
                                log_y = TRUE, ylab = "cps") {
  n <- length(y)
  if (!n) stop("没有数据点")
  x <- seq_len(n) + as.integer(x0) - 1L
  ylim <- .panel_ylim(y)
  # 上边距留 4.4 行，从下到上依次排：刻度数字 / 「时间 (s)」 / 图标题，三层不打架；
  # 左边距 4.8 + mgp[1]=3.0：y 轴刻度是「2e+06」这种宽标签，las=1 横排后需要更多空间。
  op <- .begin_canvas(c(3.8, 4.8, 4.4, 1.2), NULL, c(3.0, 0.8, 0))
  on.exit(par(op))

  band <- function(rng, fill, border) {
    if (is.null(rng) || length(rng) != 2L || !all(is.finite(rng))) return(invisible(NULL))
    if (rng[1] == rng[2]) return(invisible(NULL))
    rect(min(rng), ylim[1], max(rng), ylim[2], col = fill, border = border, lwd = 1)
  }

  plot(NA, xlim = range(x), ylim = ylim, log = if (log_y) "y" else "",
       xlab = "文件行号", ylab = ylab, main = "", las = 1,
       xaxs = "i", yaxs = "i", bty = "l")
  grid(col = .termite_theme$grid, lty = 1)
  band(bg,  "#3182ce2e", "#3182ce")          # 背景窗：蓝
  band(sig, "#38a1692e", "#38a169")          # 信号窗：绿
  # 参照线画在曲线之下、底纹之上，避免盖住曲线
  if (!is.null(auto) && length(auto) == 2L && all(is.finite(auto)))
    abline(v = auto, col = "#805ad5", lty = 2, lwd = 1.3)
  if (!is.null(cur) && length(cur) == 2L && all(is.finite(cur)))
    abline(v = cur, col = "#c53030", lty = 3, lwd = 1.3)

  yy <- as.numeric(y)
  if (log_y) yy[!is.finite(yy) | yy <= 0] <- NA   # 对数轴下非正值必须剔，否则 lines() 报警
  lines(x, yy, col = .termite_theme$line, lwd = 1.1)
  box(bty = "l")

  # 上轴：时间。用 approx 把秒映射回行号，只在整点处标，避免与曲线打架
  tt <- as.numeric(t)
  ok <- is.finite(tt) & is.finite(x)
  if (sum(ok) >= 2L) {
    at <- pretty(range(tt[ok]), 6)
    at <- at[at >= min(tt[ok]) & at <= max(tt[ok])]
    if (length(at)) {
      rw <- stats::approx(tt[ok], x[ok], xout = at, rule = 2)$y
      axis(3, at = rw, labels = formatC(at, format = "g", digits = 3),
           cex.axis = 0.72, tcl = -0.22, line = 0.55, col.axis = .termite_theme$muted)
      mtext("时间 (s)", side = 3, line = 2.0, cex = 0.72, col = .termite_theme$muted)
    }
  }
  if (nzchar(main)) mtext(main, side = 3, line = 3.2, cex = 1.0, font = 2)
  # 图例压在背景窗底纹上，给个半透明白底才读得清（bty="o" + 淡边框）
  legend("topleft", bty = "o", bg = "#ffffffdc", box.col = "#cbd5e0",
         cex = 0.78, inset = 0.012,
         legend = c("背景窗", "信号窗", "算法建议", "现行配置"),
         fill = c("#3182ce4d", "#38a1694d", NA, NA),
         border = c("#3182ce", "#38a169", NA, NA),
         lty = c(NA, NA, 2, 3), lwd = c(NA, NA, 1.3, 1.3),
         col = c(NA, NA, "#805ad5", "#c53030"))
  invisible(x)
}
