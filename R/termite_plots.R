# =============================================================================
#  TERMITE 绘图 —— 纯 base graphics，不依赖 ggplot2
# =============================================================================

.termite_theme <- list(
  bg      = "#ffffff",
  panel   = "#f7fafc",
  line    = "#2b6cb0",
  line2   = "#c05621",
  blank   = "#3182ce18",
  signal  = "#e53e3e14",
  mean    = "#2f855a",
  out     = "#c53030",
  grid    = "#e2e8f0",
  fg      = "#1a202c",
  muted   = "#718096"
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
  p <- ncol(v); nc <- min(4L, p); nr <- ceiling(p / nc)
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(nr, nc), mar = c(3.0, 3.6, 2.0, 0.7), mgp = c(2.1, 0.6, 0),
      bg = .termite_theme$bg, fg = .termite_theme$fg, col.axis = .termite_theme$fg,
      col.lab = .termite_theme$fg, cex.main = 0.95)
  x <- rows + offset
  for (j in seq_len(p)) {
    y <- v[, j]
    y[y <= 0] <- NA
    fin <- y[is.finite(y)]
    ylim <- if (!length(fin)) c(1, 10) else range(fin)
    if (diff(ylim) == 0) ylim <- ylim * c(0.5, 2)
    plot(NA, xlim = range(x), ylim = ylim, log = if (log_y) "y" else "",
         xlab = xlab, ylab = "cps", main = isotopes[j], las = 1,
         xaxs = "i", yaxs = "i", bty = "l")
    grid(col = .termite_theme$grid, lty = 1)
    if (!is.null(blank))  rect(blank[1],  ylim[1], blank[2],  ylim[2],
                               col = .termite_theme$blank, border = NA)
    if (!is.null(signal)) rect(signal[1], ylim[1], signal[2], ylim[2],
                               col = .termite_theme$signal, border = NA)
    lines(x, v[, j], col = .termite_theme$line, lwd = 1)
    box(bty = "l")
  }
  mtext(main, outer = TRUE, line = -1.4, cex = 0.95, col = .termite_theme$muted)
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
  p <- ncol(d); nc <- min(4L, p); nr <- ceiling(p / nc)
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(nr, nc), mar = c(3.4, 3.6, 2.0, 0.7), mgp = c(2.1, 0.6, 0),
      bg = .termite_theme$bg, fg = .termite_theme$fg, col.axis = .termite_theme$fg,
      col.lab = .termite_theme$fg, cex.main = 0.95)
  brk <- cumsum(table(res$files$ref_rm))
  for (j in seq_len(p)) {
    y <- d[, j]
    if (all(is.na(y))) { plot.new(); title(main = res$isotopes[j]); next }
    r <- range(y, na.rm = TRUE); if (diff(r) == 0) r <- r * c(0.9, 1.1)
    plot(seq_along(y), y, type = "b", pch = 16, cex = 0.7, col = .termite_theme$line,
         ylim = r, xlab = "", ylab = paste("RSF", res$elements[j]), main = res$isotopes[j],
         las = 1, xaxt = "n")
    axis(1, at = seq_along(y), labels = FALSE, tick = FALSE)
    abline(v = brk[-length(brk)] + 0.5, lty = 2, col = .termite_theme$muted)
    abline(h = res$rsf[j], lty = 3, lwd = 2, col = .termite_theme$mean)
    box(bty = "l")
  }
  mtext(main, outer = TRUE, line = -1.4, cex = 0.95, col = .termite_theme$muted)
}

#' 检出限（对应原脚本 LoD_ReferenceMaterial_*.pdf）
termite_plot_lod <- function(res, main = "检出限 LoD") {
  y <- as.numeric(res$lod)
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(4.2, 4.4, 2.4, 1), bg = .termite_theme$bg, fg = .termite_theme$fg,
      col.axis = .termite_theme$fg, col.lab = .termite_theme$fg)
  plot(seq_along(y), y, log = "y", pch = 16, col = .termite_theme$line,
       xaxt = "n", xlab = "同位素", ylab = expression("LoD  [" * mu * "g/g]"),
       main = main, las = 1)
  axis(1, at = seq_along(y), labels = res$isotopes, las = 2, cex.axis = 0.8)
  grid(col = .termite_theme$grid, lty = 1)
  points(seq_along(y), y, pch = 16, col = .termite_theme$line)
  box(bty = "l")
}

#' 点分析：逐元素浓度散点 + 相对标准偏差标红（对应原脚本 Results_*.pdf）
termite_plot_spot <- function(res, elements = NULL, max_panels = 16L) {
  el  <- res$elements
  keep <- if (is.null(elements)) which(el != res$elements[res$is_idx]) else
    match(elements, el)
  keep <- keep[!is.na(keep)]
  lim <- .limit_panels(keep, max_panels)
  keep <- lim$idx
  d <- res$sample$conc_masked[, keep, drop = FALSE]
  if (nzchar(lim$note)) elements <- NULL
  sd_ <- res$sample$rsd[, keep, drop = FALSE]
  p <- ncol(d); nc <- min(4L, p); nr <- ceiling(p / nc)
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(nr, nc), mar = c(3.6, 4.0, 2.2, 0.7), mgp = c(2.2, 0.6, 0),
      bg = .termite_theme$bg, fg = .termite_theme$fg, col.axis = .termite_theme$fg,
      col.lab = .termite_theme$fg, cex.main = 0.95)
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
  mtext(paste0("红点 = 相对标准偏差高于均值（可疑不均匀点）", lim$note),
        outer = TRUE, line = -1.4, cex = 0.9, col = .termite_theme$muted)
}

#' 线扫描：距离-浓度剖面
termite_plot_profile <- function(res, elements = NULL, idx = 1L, max_panels = 12L) {
  prof <- res$line[[idx]]
  el   <- res$elements
  keep <- if (is.null(elements)) which(el != el[res$is_idx]) else match(elements, el)
  keep <- keep[!is.na(keep)]
  lim <- .limit_panels(keep, max_panels); keep <- lim$idx
  d <- prof$conc_masked[, keep, drop = FALSE]
  p <- ncol(d); nc <- min(3L, p); nr <- ceiling(p / nc)
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(nr, nc), mar = c(3.8, 4.2, 2.0, 0.7), mgp = c(2.4, 0.6, 0),
      bg = .termite_theme$bg, fg = .termite_theme$fg, col.axis = .termite_theme$fg,
      col.lab = .termite_theme$fg, cex.main = 0.95)
  for (k in seq_len(p)) {
    j <- keep[k]
    y <- d[, k]
    if (all(is.na(y))) { plot.new(); title(main = el[j]); next }
    plot(prof$distance, y, type = "l", col = .termite_theme$line, lwd = 1,
         xlab = "距离 [mm]", ylab = expression("[" * mu * "g/g]"), main = el[j], las = 1)
    grid(col = .termite_theme$grid, lty = 1)
    lines(prof$distance, y, col = .termite_theme$line, lwd = 1)
    abline(h = res$lod[j], lty = 3, col = .termite_theme$muted)
    box(bty = "l")
  }
  mtext(sprintf("样品：%s　虚线 = LoD%s", prof$file, lim$note),
        outer = TRUE, line = -1.4, cex = 0.9, col = .termite_theme$muted)
}

#' 背景（gas blank）柱状对比
termite_plot_blank <- function(res, files = 1L, which = c("ref", "sample")) {
  which <- match.arg(which)
  b <- termite_blank_of(res, which, files)
  if (is.null(b)) { plot.new(); title(main = "无背景数据"); return(invisible(NULL)) }
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(4.4, 4.4, 2.2, 0.8), bg = .termite_theme$bg, fg = .termite_theme$fg,
      col.axis = .termite_theme$fg, col.lab = .termite_theme$fg)
  barplot(b, names.arg = res$isotopes, las = 2, col = .termite_theme$line,
          border = NA, ylab = "blank cps",
          main = paste0("背景（gas blank）· ",
                        if (identical(which, "ref")) res$files$ref[files] else
                          if (identical(res$mode, "spot")) res$files$sample[files] else
                            res$line[[files]]$file),
          cex.names = 0.8)
  box(bty = "l")
}
