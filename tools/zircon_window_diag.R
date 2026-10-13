#!/usr/bin/env Rscript
# =============================================================================
#  积分窗口「逐点位」诊断 —— 把每个测点的背景窗 / 信号窗画回原始信号曲线
#  ---------------------------------------------------------------------------
#  为什么需要这张图：
#  归算链的共同起点是「选哪一段当背景、哪一段当信号」。这是唯一一处
#  不依赖任何校准参数、纯由人工划定的输入。一旦某个测点的信号窗压到剥蚀结束
#  之后，或背景窗落在剥蚀尾巴上，该测点全部元素会整体偏移，
#  而下游的结果表里看不出任何异常（数值仍然“正常”）。
#
#  行号口径与内核严格一致（见 R/termite_core.R 的 .to_matrix_row）：
#      数据矩阵第 r 行  ==  文件第 r + signal_line − 1 行
#  本脚本用同一式子反算，保证画出来的窗口就是归算实际用的窗口。
#
#  用法：
#    export TERMITE_DIR=<批次目录>
#    export OUT=<输出目录>
#    "C:/Program Files/R/R-4.2.2/bin/Rscript.exe" tools/zircon_window_diag.R
#
#  可选环境变量（默认值对应当前锆石批次）：
#    HEADER_LINE=14  SIGNAL_LINE=16
#    BLANK1=25  BLANK2=60      背景窗（文件行号）
#    SIG1=100   SIG2=158       信号窗（文件行号）
#    CH=29Si                   画哪条通道
#    PER_PAGE=4                每页面板数（逐点页）
# =============================================================================

options(encoding = "UTF-8")
# Bash 工具会注入 LC_ALL=C.UTF-8（Windows 无此 locale），R 回退到 "C"/ASCII 后
# readLines(encoding="UTF-8") 的中文会转码失败。显式设回 UTF-8 族。
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

DIR <- Sys.getenv("TERMITE_DIR", "")
if (!nzchar(DIR)) stop("请先用环境变量 TERMITE_DIR 指定批次目录。")
OUT <- Sys.getenv("OUT", file.path(DIR, "图表"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

HL  <- as.integer(Sys.getenv("HEADER_LINE", "14"))
SL  <- as.integer(Sys.getenv("SIGNAL_LINE", "16"))
B1  <- as.integer(Sys.getenv("BLANK1", "25"))
B2  <- as.integer(Sys.getenv("BLANK2", "60"))
S1  <- as.integer(Sys.getenv("SIG1",   "100"))
S2  <- as.integer(Sys.getenv("SIG2",   "158"))
CH  <- Sys.getenv("CH", "29Si")
PER <- as.integer(Sys.getenv("PER_PAGE", "4"))
SAMPLE <- as.integer(Sys.getenv("SAMPLE", "0"))   # >0 时另出一张「前 N 个测点」的大图，便于逐点核对
PICK   <- Sys.getenv("PICK", "")                  # 逗号分隔的序号，另出一张「指定测点」的大图

# 文件行号 → 数据矩阵行号（与内核 .to_matrix_row 同式，1-based）
.row_of <- function(line_no) as.integer(line_no) - (SL - 1L)
P1 <- .row_of(B1); P2 <- .row_of(B2)      # 背景窗（矩阵行）
Q1 <- .row_of(S1); Q2 <- .row_of(S2)      # 信号窗（矩阵行）

# ---- 读单个测点 ------------------------------------------------------------
# 只取 Time 与目标通道两列；行内字段数不齐时右侧补 NA，避免 read.table 报列数错误
.pad <- function(v, n) {
  if (length(v) < n) v <- c(v, rep(NA_character_, n - length(v)))
  v[seq_len(n)]
}

.read_point <- function(path) {
  ln <- sub("\r$", "", readLines(path, encoding = "UTF-8", warn = FALSE))
  if (length(ln) < SL) stop("文件行数不足：", basename(path))
  name <- trimws(sub(":.*$", "", ln[1L]))
  hdr  <- trimws(strsplit(ln[HL], ",", fixed = TRUE)[[1L]])
  jch  <- match(CH, hdr)
  if (is.na(jch)) stop("表头里找不到通道 ", CH, "：", basename(path))
  body <- ln[SL:length(ln)]
  body <- body[nzchar(trimws(body))]
  want <- c(1L, jch)
  mm <- t(vapply(strsplit(body, ",", fixed = TRUE),
                 function(v) suppressWarnings(as.numeric(.pad(v, length(hdr))[want])),
                 numeric(length(want))))
  list(file = basename(path), name = name, t = mm[, 1L], sig = mm[, 2L], n = nrow(mm))
}

# ---- 收集（按文件名里的序号排序，而不是字典序）-----------------------------
fs  <- list.files(DIR, pattern = "[.]csv$")
if (!length(fs)) stop("目录里没有 csv 文件：", DIR)
ord <- suppressWarnings(as.integer(sub("^.*_(\\d+)[.]csv$", "\\1", fs)))
fs  <- fs[order(ord, fs)]
pts <- lapply(file.path(DIR, fs), .read_point)
cat("读入", length(pts), "个测点；通道 =", CH,
    "；背景窗", B1, "-", B2, "行 → 矩阵行", P1, "-", P2,
    "；信号窗", S1, "-", S2, "行 → 矩阵行", Q1, "-", Q2, "\n")

# ---- 统计表：每个测点的窗口内信号水平 --------------------------------------
.stat <- function(p) {
  n  <- p$n
  bi <- seq(max(1L, P1), min(n, P2))
  si <- seq(max(1L, Q1), min(n, Q2))
  mb <- stats::median(p$sig[bi], na.rm = TRUE)
  ms <- stats::median(p$sig[si], na.rm = TRUE)
  # 剥蚀段 = 信号高于「背景中位 × 3」的点，最外侧越阈点之外即视为熄火。
  # 若信号窗右边界伸到最外侧越阈点之后，说明窗口把熄火后的低信号也平均了进去，
  # 会系统性压低该测点的全部元素 —— 这正是这张图要提前发现的那类错误。
  over <- which(p$sig > 3 * mb)
  tail_over <- if (length(over)) Q2 - max(over) else NA_integer_
  data.frame(序号 = NA_integer_, 文件 = p$file, 样品 = p$name,
             数据行 = n, 背景窗中位 = round(mb, 1), 信号窗中位 = round(ms, 1),
             信背比 = round(ms / mb, 1), 窗尾超出点 = tail_over,
             信号窗截断 = (Q2 > n), check.names = FALSE)
}
st <- do.call(rbind, lapply(pts, .stat))
st$序号 <- seq_len(nrow(st))
utils::write.csv(st, file.path(OUT, "积分窗口_统计.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
cat("\n=== 窗口内信号水平（前 8 行 / 共", nrow(st), "行）===\n")
print(utils::head(st, 8), row.names = FALSE)
bad <- st[!is.na(st$信背比) & st$信背比 < 3, ]
if (nrow(bad)) {
  cat("\n!! 信背比 < 3 的测点（窗口可能选歪）：\n")
  print(bad[, c("序号", "文件", "样品", "背景窗中位", "信号窗中位", "信背比")],
        row.names = FALSE)
} else {
  cat("\n全部测点信背比 >= 3（信号窗确实落在剥蚀段内）。\n")
}
tbad <- st[!is.na(st$窗尾超出点) & st$窗尾超出点 > 3, ]
if (nrow(tbad)) {
  cat("\n!! 信号窗右边界超出了剥蚀结束点（右边这段低信号会被平均进去）：\n")
  print(tbad[, c("序号", "文件", "样品", "信号窗中位", "窗尾超出点")], row.names = FALSE)
} else {
  cat("全部测点的信号窗右边界都落在剥蚀结束点之内。\n")
}
if (any(st$信号窗截断))
  cat("\n!! 下列测点的信号窗超过了文件长度，归算时会被截断：",
      paste(st$文件[st$信号窗截断], collapse = ", "), "\n")

# ---- 绘图 ------------------------------------------------------------------
CL_BG <- "#4C78A84D"   # 背景窗：半透明蓝（alpha 30%，缩到总览小图里也还看得见）
CL_SG <- "#E457564D"   # 信号窗：半透明红
CL_LN <- "#1f4e79"     # 信号曲线
CL_BD <- "#c0392b"     # 窗口边界

allv <- unlist(lapply(pts, function(p) p$sig[p$sig > 0]), use.names = FALSE)
YLIM <- 10^range(log10(allv))          # 全体测点共用对数纵轴 → 面板之间可横向比较

.panel <- function(p, idx, lab, tiny = FALSE) {
  y <- p$sig; y[y <= 0] <- NA          # 对数轴下 0 无法表达，置 NA 而非压到轴底
  op <- par(mar = if (tiny) c(1.0, 1.0, 1.1, 0.35) else c(2.9, 3.1, 2.0, 0.7),
            mgp = c(1.7, 0.5, 0), tcl = -0.25,
            cex.axis = if (tiny) 0.4 else 0.75)
  on.exit(par(op))
  plot(p$t, y, type = "n", log = "y", ylim = YLIM, xlab = "", ylab = "",
       axes = tiny, xaxs = "r")
  if (!tiny) {
    axis(1, cex.axis = 0.75)
    axis(2, las = 1, cex.axis = 0.75)
    box(col = "#9a9a9a")
    mtext("时间 (s)", side = 1, line = 1.9, cex = 0.72)
    mtext(paste0(CH, " 信号 (cps)"), side = 2, line = 2.1, cex = 0.72)
  }
  u <- par("usr"); n <- p$n
  rect(p$t[max(1L, P1)], u[3], p$t[min(n, P2)], u[4], col = CL_BG, border = NA)
  rect(p$t[max(1L, Q1)], u[3], p$t[min(n, Q2)], u[4], col = CL_SG, border = NA)
  lines(p$t, y, col = CL_LN, lwd = if (tiny) 0.7 else 1.5)
  if (!tiny) {
    # 窗口内的中位线：如果某个窗口里出现“中位偏离平台”的情况，说明窗口没包住平台
    mb <- stats::median(p$sig[seq(max(1L, P1), min(n, P2))], na.rm = TRUE)
    ms <- stats::median(p$sig[seq(max(1L, Q1), min(n, Q2))], na.rm = TRUE)
    if (is.finite(mb)) abline(h = mb, col = "#2b6cb0", lty = 3, lwd = 1)
    if (is.finite(ms)) abline(h = ms, col = CL_BD,    lty = 3, lwd = 1)
    for (x in c(p$t[max(1L, Q1)], p$t[min(n, Q2)]))
      abline(v = x, col = CL_BD, lty = 2, lwd = 0.9)
  }
  mtext(lab, side = 3, line = if (tiny) 0.15 else 0.6,
        cex = if (tiny) 0.55 else 1.0, font = 2, col = "#1a1a1a")
}

.open <- function(file, w, h, onefile = TRUE) {
  if (isTRUE(capabilities("cairo")))
    cairo_pdf(file, width = w, height = h, onefile = onefile, bg = "white",
              family = "Microsoft YaHei")
  else
    pdf(file, width = w, height = h, onefile = onefile, bg = "white")
}

# ---- 主交付：多页 PDF（第 1 页总览 + 逐点放大）-----------------------------
PDF <- file.path(OUT, "积分窗口_逐点位.pdf")
.open(PDF, 11.69, 8.27)

# 第 1 页：54 个测点一屏看全，只判“窗口位置对不对”
nc0 <- 9L; nr0 <- ceiling(length(pts) / nc0)
par(mfrow = c(nr0, nc0), oma = c(0.6, 0.6, 2.4, 0.4))
for (i in seq_along(pts)) .panel(pts[[i]], i, sprintf("%d", i), tiny = TRUE)
mtext(sprintf("积分窗口总览：%d 个测点  蓝 = 背景窗（文件行 %d-%d）  红 = 信号窗（文件行 %d-%d）  曲线 = %s",
              length(pts), B1, B2, S1, S2, CH),
      side = 3, line = 0.5, outer = TRUE, cex = 1.08, font = 2)
mtext("坐标轴全部为对数、且共用同一纵轴范围，因此面板之间可直接比较信号水平",
      side = 3, line = -0.35, outer = TRUE, cex = 0.72, col = "#666666")

# 后续页：逐点放大，看清窗口边界落在信号的哪个位置
nr <- if (PER <= 2L) 1L else 2L
nc <- ceiling(PER / nr); ppp <- nr * nc
for (k in seq(1L, length(pts), by = ppp)) {
  ii <- k:min(k + ppp - 1L, length(pts))
  par(mfrow = c(nr, nc), oma = c(1.0, 1.0, 2.2, 0.6))
  for (i in ii)
    .panel(pts[[i]], i, sprintf("#%d  %s", i, pts[[i]]$name))
  if (length(ii) < ppp) for (z in seq_len(ppp - length(ii))) plot.new()
  mtext(sprintf("测点 %d – %d（共 %d）", ii[1], ii[length(ii)], length(pts)),
        side = 3, line = 0.4, outer = TRUE, cex = 1.05, font = 2)
}
grDevices::dev.off()
cat("\n写出", PDF, "\n")

# ---- 附：单张总览 PNG（便于直接预览）--------------------------------------
PNG <- file.path(OUT, "积分窗口_总览.png")
grDevices::png(PNG, width = 2600, height = 1800, res = 150, bg = "white")
par(mfrow = c(6, 9), oma = c(0.6, 0.6, 3.6, 0.4))
for (i in seq_along(pts)) .panel(pts[[i]], i, sprintf("%d", i), tiny = TRUE)
mtext(sprintf("积分窗口总览：%d 个测点  蓝 = 背景窗（文件行 %d-%d）  红 = 信号窗（文件行 %d-%d）",
              length(pts), B1, B2, S1, S2),
      side = 3, line = 1.6, outer = TRUE, cex = 1.35, font = 2)
mtext(sprintf("曲线 = %s（对数轴，全体共用同一纵向范围）", CH),
      side = 3, line = 0.5, outer = TRUE, cex = 0.95, col = "#555555")
grDevices::dev.off()
cat("写出", PNG, "\n")

# ---- 附：前 N 个测点的大图（逐点核对窗口边界用；SAMPLE=0 时不输出）---------
if (SAMPLE > 0L) {
  sn <- min(SAMPLE, length(pts))
  BIG <- file.path(OUT, sprintf("积分窗口_放大_前%d测点.png", sn))
  grDevices::png(BIG, width = 2400, height = 1650, res = 150, bg = "white")
  par(mfrow = c(2, 2), oma = c(1.0, 1.0, 3.0, 0.6))
  for (i in seq_len(sn))
    .panel(pts[[i]], i, sprintf("#%d  %s", i, pts[[i]]$name))
  mtext(sprintf("窗口边界核对（前 %d 个测点）  蓝 = 背景窗 %d-%d 行   红 = 信号窗 %d-%d 行",
                sn, B1, B2, S1, S2),
        side = 3, line = 0.9, outer = TRUE, cex = 1.3, font = 2)
  grDevices::dev.off()
  cat("写出", BIG, "\n")
}

# ---- 附：指定序号的测点大图（复核个别可疑点用）------------------------------
if (nzchar(PICK)) {
  pi <- as.integer(strsplit(PICK, "[, ]+")[[1]])
  pi <- pi[!is.na(pi) & pi >= 1L & pi <= length(pts)]
  if (length(pi)) {
    K  <- length(pi); nc2 <- min(3L, K); nr2 <- ceiling(K / nc2)
    PIN <- file.path(OUT, sprintf("积分窗口_指定测点_%s.png", paste(pi, collapse = "-")))
    grDevices::png(PIN, width = 2300, height = 720 * nr2, res = 150, bg = "white")
    par(mfrow = c(nr2, nc2), oma = c(1.0, 1.0, 3.0, 0.6))
    for (z in pi) .panel(pts[[z]], z, sprintf("#%d  %s", z, pts[[z]]$name))
    mtext(sprintf("指定测点复核  蓝 = 背景窗 %d-%d 行   红 = 信号窗 %d-%d 行",
                  B1, B2, S1, S2),
          side = 3, line = 0.9, outer = TRUE, cex = 1.3, font = 2)
    grDevices::dev.off()
    cat("写出", PIN, "\n")
  }
}

cat("\n完成。输出目录：", OUT, "\n", sep = "")
