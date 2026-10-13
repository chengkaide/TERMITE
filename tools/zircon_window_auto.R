#!/usr/bin/env Rscript
# =============================================================================
#  积分窗口「自动优化」批处理
#  ---------------------------------------------------------------------------
#  读一个批次目录里的全部测点，从各自的信号形态量出剥蚀起止，给出
#    · 每点建议的信号窗 / 背景窗（数据行 + 文件行两种口径）
#    · 质量标记（信背比、平台起伏、平台是否过短）
#    · 折成一对「全批统一窗口」（内核只接受同一组窗口）
#    · 与现行写死窗口的逐点对比图
#
#  用法：
#    export TERMITE_DIR=<批次目录>
#    export OUT=<输出目录>
#    "C:/Program Files/R/R-4.2.2/bin/Rscript.exe" tools/zircon_window_auto.R
#
#  可选环境变量（默认值对应当前锆石批次）：
#    HEADER_LINE=14  SIGNAL_LINE=16          Qtegra 行号口径
#    CH=29Si                                 用来定窗的通道（选信号最强的）
#    SW=5  HI_K=5  LO_K=3  RUN=3             平滑窗 / 上升阈值倍数 / 下降阈值倍数 / 连续点数
#    LEAD=6  TAIL=8                          信号窗左右各留的余量（数据行）
#    CUR_BG1=25 CUR_BG2=60 CUR_SIG1=100 CUR_SIG2=158   现行窗口（文件行），只用于对比
#    PICK=1,2,3,4                            单独出大图的测点序号（默认前 4 个）
# =============================================================================

options(encoding = "UTF-8")
# 必须在 source() 之前修好 LC_CTYPE：Bash 工具注入的 LC_ALL=C.UTF-8 在 Windows 上
# 不存在，R 回退到 "C"/ASCII，此时 source(encoding="UTF-8") 里的中文无法转码，
# 整个 window_lib.R 会被吞掉、函数一个都不定义（报错却是"找不到函数"，极易误判）。
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1L])) else getwd()
}
source(file.path(.script_dir(), "window_lib.R"), encoding = "UTF-8")

DIR <- Sys.getenv("TERMITE_DIR", "")
if (!nzchar(DIR)) stop("请先用环境变量 TERMITE_DIR 指定批次目录。")
OUT <- Sys.getenv("OUT", file.path(DIR, "图表"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

HL <- as.integer(Sys.getenv("HEADER_LINE", "14"))
SL <- as.integer(Sys.getenv("SIGNAL_LINE", "16"))
CH <- Sys.getenv("CH", "29Si")
SW   <- as.integer(Sys.getenv("SW", "5"))
HI_K <- as.numeric(Sys.getenv("HI_K", "5"))
LO_K <- as.numeric(Sys.getenv("LO_K", "3"))
RUN  <- as.integer(Sys.getenv("RUN", "3"))
LEAD <- as.integer(Sys.getenv("LEAD", "6"))
TAIL <- as.integer(Sys.getenv("TAIL", "8"))

# 现行窗口（文件行）——只作为对比基准，不参与算法
CB1 <- as.integer(Sys.getenv("CUR_BG1", "25"))
CB2 <- as.integer(Sys.getenv("CUR_BG2", "60"))
CS1 <- as.integer(Sys.getenv("CUR_SIG1", "100"))
CS2 <- as.integer(Sys.getenv("CUR_SIG2", "158"))

# ---- 读点并定窗 -------------------------------------------------------------
pts <- window_scan_dir(DIR, chs = CH, header_line = HL, signal_line = SL)
cat("读入", length(pts), "个测点；定窗通道 =", CH, "\n")

res <- window_auto_batch(pts, ch = CH, signal_line = SL,
                         sw = SW, hi_k = HI_K, lo_k = LO_K, run = RUN,
                         lead = LEAD, tail = TAIL)

utils::write.csv(res, file.path(OUT, "自动窗口建议.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

# ---- 折出统一窗口 -----------------------------------------------------------
uni <- window_union(res)
cur <- c(信号窗起_行 = window_row_of(CS1, SL), 信号窗止_行 = window_row_of(CS2, SL))
uni$信号窗起_行[3L] <- cur[["信号窗起_行"]]
uni$信号窗止_行[3L] <- cur[["信号窗止_行"]]
uni$信号窗起_文件行 <- window_line_of(uni$信号窗起_行, SL)
uni$信号窗止_文件行 <- window_line_of(uni$信号窗止_行, SL)
utils::write.csv(uni, file.path(OUT, "自动窗口_统一窗口建议.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cat("\n=== 每点建议窗口（前 8 行 / 共", nrow(res), "行）===\n")
print(utils::head(res[, c("序号", "样品", "剥蚀起_s", "剥蚀止_s",
                          "信号窗起_s", "信号窗止_s", "信背比", "去趋势RSD", "判定")], 8L),
      row.names = FALSE)

cat("\n=== 统一窗口候选（数据行 / 文件行）===\n")
print(uni, row.names = FALSE)

bad <- res[res$判定 != "正常", , drop = FALSE]
if (nrow(bad)) {
  cat("\n!! 需要人工确认的测点（", nrow(bad), " 个）：\n", sep = "")
  print(bad[, c("序号", "样品", "剥蚀起_s", "剥蚀止_s", "平台宽", "信背比", "判定")], row.names = FALSE)
}
# 熄火明显偏早的点：用 MAD 做稳健离群判定，但 MAD 在全批几乎同值时会被压到近零，
# 会把「只差一个采样点」的点也标出来，因此再加一道 3% 相对容差兜底。
off_med <- stats::median(res$剥蚀止_s, na.rm = TRUE)
off_mad <- stats::mad(res$剥蚀止_s, na.rm = TRUE)
off_tol <- max(3 * off_mad, 0.03 * off_med)
early <- res[!is.na(res$剥蚀止_s) & res$剥蚀止_s < off_med - off_tol, , drop = FALSE]
if (nrow(early)) {
  cat("\n!! 熄火明显早于全批中位（", round(off_med, 1), " s ± 3MAD）的测点：\n", sep = "")
  print(early[, c("序号", "样品", "剥蚀止_s", "信号窗止_s")], row.names = FALSE)
}

# ---- 绘图 -------------------------------------------------------------------
CL_CUR_S <- "#E45756"      # 现行信号窗
CL_AUT_S <- "#2E9E5B"      # 自动信号窗
CL_CUR_B <- "#4C78A8"      # 现行背景窗
CL_AUT_B <- "#1B7A9E"      # 自动背景窗
CL_LN    <- "#1f4e79"

allv <- unlist(lapply(pts, function(p) p$y[p$y > 0]), use.names = FALSE)
YLIM <- 10^range(log10(allv))

.open <- function(file, w, h) {
  if (isTRUE(capabilities("cairo")))
    cairo_pdf(file, width = w, height = h, bg = "white", family = "Microsoft YaHei")
  else pdf(file, width = w, height = h, bg = "white")
}

# 单个面板：实线框 = 自动窗口，虚线框 = 现行窗口
.panel <- function(p, r, tiny = FALSE) {
  y <- p$y[, CH]; y[!is.finite(y) | y <= 0] <- NA
  op <- par(mar = if (tiny) c(1.0, 1.0, 1.1, 0.35) else c(3.0, 3.2, 2.2, 0.8),
            mgp = c(1.8, 0.5, 0), tcl = -0.25, cex.axis = if (tiny) 0.42 else 0.78)
  on.exit(par(op))
  plot(p$t, y, type = "n", log = "y", ylim = YLIM, xlab = "", ylab = "", axes = tiny)
  if (!tiny) {
    axis(1); axis(2, las = 1); box(col = "#9a9a9a")
    mtext("时间 (s)", side = 1, line = 2.0, cex = 0.74)
    mtext(paste0(CH, " 信号 (cps)"), side = 2, line = 2.2, cex = 0.74)
  }
  u <- par("usr"); n <- p$n
  rect(p$t[max(1L, CB1 - SL + 1L)], u[3], p$t[min(n, CB2 - SL + 1L)], u[4],
       col = paste0(CL_CUR_B, "33"), border = NA)                       # 现行背景
  rect(p$t[max(1L, CS1 - SL + 1L)], u[3], p$t[min(n, CS2 - SL + 1L)], u[4],
       col = paste0(CL_CUR_S, "33"), border = NA)                       # 现行信号
  lines(p$t, y, col = CL_LN, lwd = if (tiny) 0.7 else 1.5)
  if (!tiny && !is.na(r$信号窗起_行)) {
    abline(v = p$t[c(r$信号窗起_行, r$信号窗止_行)], col = CL_AUT_S, lwd = 1.6)   # 自动信号窗边界
    abline(v = p$t[c(r$背景窗起_行, r$背景窗止_行)], col = CL_AUT_B, lwd = 1.2, lty = 5)
    abline(v = p$t[c(CS1, CS2) - SL + 1L], col = CL_CUR_S, lwd = 1.0, lty = 2)   # 现行信号窗边界
  }
  mtext(sprintf("#%d  %s", r$序号, r$样品), side = 3, line = if (tiny) 0.15 else 0.7,
        cex = if (tiny) 0.5 else 0.95, font = 2, col = "#1a1a1a")
}

# --- 图 1：逐点对比（多页 PDF，第 1 页总览 + 后续放大）---
PDF <- file.path(OUT, "自动窗口_逐点对比.pdf")
.open(PDF, 11.69, 8.27)
nc0 <- 9L; nr0 <- ceiling(length(pts) / nc0)
par(mfrow = c(nr0, nc0), oma = c(0.6, 0.6, 2.6, 0.4))
for (i in seq_along(pts)) .panel(pts[[i]], res[i, ], tiny = TRUE)
mtext("积分窗口：实线 = 自动识别边界   虚线 = 现行配置边界",
      side = 3, line = 0.6, outer = TRUE, cex = 1.05, font = 2)
for (k in seq(1L, length(pts), by = 4L)) {
  ii <- k:min(k + 3L, length(pts))
  par(mfrow = c(2, 2), oma = c(1.0, 1.0, 2.4, 0.6))
  for (i in ii) .panel(pts[[i]], res[i, ])
  if (length(ii) < 4L) for (z in seq_len(4L - length(ii))) plot.new()
  mtext(sprintf("测点 %d – %d（共 %d）", ii[1], ii[length(ii)], length(pts)),
        side = 3, line = 0.5, outer = TRUE, cex = 1.0, font = 2)
}
grDevices::dev.off()
cat("\n写出", PDF, "\n")

# --- 图 2：剥蚀跨度分布（这张图才是「为什么要自适应」的直接证据）---
PICK <- as.integer(strsplit(Sys.getenv("PICK", "1,2,3,4"), "[, ]+")[[1L]])
PICK <- PICK[!is.na(PICK) & PICK >= 1L & PICK <= length(pts)]
if (length(PICK)) {
  PNG <- file.path(OUT, sprintf("自动窗口_对比_测点%s.png", paste(PICK, collapse = "-")))
  nr <- if (length(PICK) <= 2L) 1L else 2L
  nc <- ceiling(length(PICK) / nr)
  grDevices::png(PNG, width = 700 * nc, height = 620 * nr, res = 150, bg = "white")
  par(mfrow = c(nr, nc), oma = c(1.0, 1.0, 3.2, 0.6))
  for (z in PICK) .panel(pts[[z]], res[z, ])
  mtext("自动窗口 vs 现行窗口：绿实线 = 自动识别，红虚线 = 现行配置",
        side = 3, line = 1.3, outer = TRUE, cex = 1.15, font = 2)
  mtext(sprintf("通道 %s   共 %d 个测点", CH, length(pts)),
        side = 3, line = 0.3, outer = TRUE, cex = 0.9, col = "#555555")
  grDevices::dev.off()
  cat("写出", PNG, "\n")
}

# --- 图 3：全体测点剥蚀跨度 vs 现行/建议窗口 ---
# 注意：纵轴是测点序号、横轴是秒。窗口边界必须用 pts$t 换算成时间再画，
# 直接拿数据行号 abline() 会画到 x 轴范围之外（或错位），看不出任何东西。
PNG2 <- file.path(OUT, "剥蚀窗口分布.png")
grDevices::png(PNG2, width = 2600, height = 1600, res = 150, bg = "white")
par(mar = c(4.6, 5.2, 3.6, 13.5), mgp = c(2.6, 0.7, 0))
k  <- nrow(res)
tt <- pts[[1L]]$t
t_of  <- function(row) tt[pmin(pmax(as.integer(row), 1L), length(tt))]
t_cur <- t_of(c(CS1, CS2) - SL + 1L)
t_str <- t_of(c(uni$信号窗起_行[1L], uni$信号窗止_行[1L]))
t_rob <- t_of(c(uni$信号窗起_行[2L], uni$信号窗止_行[2L]))

xr <- range(c(res$剥蚀起_s, res$剥蚀止_s), na.rm = TRUE)
plot(NA, xlim = xr * c(0.97, 1.03), ylim = c(k + 0.5, 0.5),   # 序号 1 在顶部
     xlab = "时间 (s)", ylab = "测点序号",
     main = sprintf("各测点剥蚀平台跨度与建议信号窗（共 %d 点）", k),
     cex.main = 1.35, cex.lab = 1.05)
gx <- seq(0, 300, 10); gx <- gx[gx <= xr[2L] * 1.03]
abline(v = gx, col = "#ececec", lty = 3)
segments(res$剥蚀起_s, res$序号, res$剥蚀止_s, res$序号, col = "#B9C6D4", lwd = 4.4, lend = "butt")
segments(res$信号窗起_s, res$序号, res$信号窗止_s, res$序号, col = "#2E9E5B", lwd = 2.6, lend = "butt")
abline(v = t_cur, col = CL_CUR_S, lty = 2, lwd = 1.8)
abline(v = t_str, col = "#1f7a3f", lty = 1, lwd = 1.8)
abline(v = t_rob, col = "#F2A93B", lty = 3, lwd = 1.8)
axis(2, at = seq(1L, k, by = 5L), las = 1, cex.axis = 0.9)
if (nrow(early))
  text(res$剥蚀止_s[early$序号], early$序号, labels = "!", col = "#c0392b", pos = 4, font = 2, cex = 1.3)
legend(x = par("usr")[2L], y = par("usr")[4L], xpd = NA, inset = c(0.01, 0.01), bty = "n",
       cex = 0.92, seg.len = 2.4, lwd = c(4.4, 2.6, 1.8, 1.8, 1.8, NA),
       lty = c(1, 1, 2, 1, 3, NA), pch = c(NA, NA, NA, NA, NA, 33),
       col = c("#B9C6D4", "#2E9E5B", CL_CUR_S, "#1f7a3f", "#F2A93B", "#c0392b"),
       legend = c("剥蚀平台实测跨度", "自动建议信号窗",
                  sprintf("现行窗口 %.1f–%.1f s", t_cur[1L], t_cur[2L]),
                  sprintf("严格统一窗 %.1f–%.1f s", t_str[1L], t_str[2L]),
                  sprintf("稳健统一窗 %.1f–%.1f s", t_rob[1L], t_rob[2L]),
                  "熄火偏早"))
grDevices::dev.off()
cat("写出", PNG2, "\n")

cat("\n完成。输出目录：", OUT, "\n", sep = "")
