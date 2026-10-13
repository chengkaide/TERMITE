#!/usr/bin/env Rscript
# =============================================================================
#  锆石微量元素「不同处理方式」对比图组
#  ---------------------------------------------------------------------------
#  读入 tools/zircon_methods.R / zircon_compare.R 的产出，生成 6 张独立图：
#
#    F1 主量氧化物闭合         —— 一句话判死活：锆石必须 ≈ 100 wt%
#    F2 Si : (Zr+Hf) 摩尔比    —— 比总量闭合更灵敏，理想 = 1.000
#    F3 逐元素偏差（M3 ÷ 实验室）—— 哪个元素被抬高、抬高多少
#    F4 四口径 × 全元素 热图     —— 一图看全 4×38 的偏差指纹
#    F5 稀土配分（球粒陨石标准化）—— 形状一致、整体平移
#    F6 各口径偏差分布箱线       —— 偏置（中位）与离散（四分位）
#
#  用法：OUT=<结果目录> Rscript tools/zircon_compare_plots.R
#  输出：<结果目录>/图表/F1_*.png ... F6_*.png
# =============================================================================

options(encoding = "UTF-8")
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

OUT <- Sys.getenv("OUT", "")
if (!nzchar(OUT)) stop("请先用环境变量 OUT 指定结果目录。")
PLOT <- file.path(OUT, "图表")
dir.create(PLOT, showWarnings = FALSE, recursive = TRUE)

rd <- function(f) utils::read.csv(file.path(OUT, f), check.names = FALSE,
                                  fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
rdm <- function(f) { d <- rd(f); m <- as.matrix(d[, -1, drop = FALSE]); rownames(m) <- d[[1]]; m }

# --- 数据 -------------------------------------------------------------------
cl   <- rd("主量氧化物闭合_五口径.csv")     # 口径 SiO2 ZrO2 HfO2 UTh YREE 其他 总和
sr   <- rd("SiZr摩尔比_五口径.csv")          # 口径 Si比ZrHf Si比Zr
summ <- rd("方法对比_汇总统计.csv")          # 元素 中位_* 比值_* ...

Ml <- list("M1 Zr内标"          = rdm("方法_M1_Zr内标.csv"),
           "M2 Si内标"          = rdm("方法_M2_Si内标.csv"),
           "M3 归一法_fixedSi1" = rdm("方法_M3_归一法_fixedSi1.csv"),
           "M4 归一法_anhydrous" = rdm("方法_M4_归一法_anhydrous.csv"))
# 样品名前缀由环境变量给出，见 tools/zircon_compare.R 的同一说明
ZPRE <- Sys.getenv("ZIRCON_PREFIX", "")
zs <- if (nzchar(ZPRE)) grep(ZPRE, rownames(Ml[[1]]), value = TRUE) else rownames(Ml[[1]])
Ml <- lapply(Ml, function(m) m[zs, , drop = FALSE])

lab_long <- rd("实验室AYCF参考值.csv")
els <- colnames(Ml[[1]])
LAB <- matrix(NA_real_, length(zs), length(els), dimnames = list(zs, els))
ll <- lab_long[lab_long$sample %in% zs, ]
for (i in seq_len(nrow(ll))) if (ll$element[i] %in% els)
  LAB[ll$sample[i], ll$element[i]] <- ll$value_ug_g[i]

med <- function(x) stats::median(x, na.rm = TRUE)

CL  <- c("M1 Zr内标" = "#2c7fb8", "M2 Si内标" = "#e6550d",
         "M3 归一法_fixedSi1" = "#31a354", "M4 归一法_anhydrous" = "#756bb1",
         "LAB AYCF" = "#111111")
SHORT <- c("M1 Zr内标" = "Zr内标", "M2 Si内标" = "Si内标",
           "M3 归一法_fixedSi1" = "归一法\nfixed Si:1",
           "M4 归一法_anhydrous" = "归一法\nanhydrous", "LAB AYCF" = "实验室\nAYCF")
FLAT  <- c("M1 Zr内标" = "Zr内标", "M2 Si内标" = "Si内标",
           "M3 归一法_fixedSi1" = "归一法 fixed Si:1",
           "M4 归一法_anhydrous" = "归一法 anhydrous", "LAB AYCF" = "实验室 AYCF")
MEAN4 <- c("M1 Zr内标", "M2 Si内标", "M3 归一法_fixedSi1", "M4 归一法_anhydrous")
lc5 <- cl$口径; lsr <- sr$口径

# 已知干扰通道 / 近检出限通道（图上要标注，别让读者误读）
IFER  <- c("Nb", "Ta", "Tb", "Pb", "Ca", "W")            # 多原子离子干扰
DILIM <- c("Ni", "Mo", "In", "Sn", "Ag")                 # 实验室值接近检出限

pngdev <- function(f, w, h, res = 200)
  grDevices::png(file.path(PLOT, f), width = w, height = h, res = res,
                 bg = "white", family = "sans")

# =============================================================================
# F1  主量氧化物闭合
# =============================================================================
pngdev("F1_主量氧化物闭合.png", 1700, 1150)
op <- par(mar = c(5, 5.2, 4, 5.6), mgp = c(3.2, 0.8, 0), las = 1, cex.axis = 0.95)
seg <- c("SiO2", "ZrO2", "HfO2", "UTh", "YREE", "其他")
SCOL <- c("#4a90d9", "#d95f02", "#7570b3", "#66a61e", "#e7298a", "#bdbdbd")
mt <- t(as.matrix(cl[, seg]))
bp <- barplot(mt, col = SCOL, border = "white", ylim = c(0, 152), las = 1,
              names.arg = SHORT[lc5], cex.names = 0.9,
              ylab = "氧化物加和 (wt%)",
              main = "F1  主量氧化物闭合：锆石必须 ≈ 100 wt%",
              cex.main = 1.25)
abline(h = 100, lty = 2, lwd = 2.4, col = "#c0392b")
# 标注放在右侧轴外：柱体上方全是数值标签，放图内必然压字
mtext("100 wt%", side = 4, at = 100, las = 1, line = 1.0, cex = 0.95,
      col = "#c0392b", font = 2)
tots <- cl$总和
text(bp, tots + 4, sprintf("%.1f", tots), cex = 1.1, font = 2)
for (i in seq_along(tots)) {
  d <- tots[i] - 100
  if (abs(d) > 0.5)
    text(bp[i], tots[i] + 11, sprintf("%+.1f", d), cex = 1.0, font = 2,
         col = if (d > 0) "#c0392b" else "#1a7f37")
}
legend("topright", legend = c("SiO2", "ZrO2", "HfO2", "U+Th", "Y+REE", "其他"),
       fill = SCOL, border = "white", bty = "n", cex = 0.85, ncol = 2)
par(op); grDevices::dev.off()

# =============================================================================
# F2  Si : (Zr+Hf)
# =============================================================================
pngdev("F2_SiZr摩尔比.png", 1500, 980)
op <- par(mar = c(6.4, 10, 5.4, 2), mgp = c(3, 0.8, 0), las = 1)
v <- sr$Si比ZrHf
nm <- SHORT[lsr]
ok <- abs(v - 1) <= 0.03
bc <- ifelse(ok, "#31a354", "#c0392b")
# 横向条形自下而上绘制，倒序以便从 Zr内标 读到 实验室
bp <- barplot(rev(v), horiz = TRUE, col = rev(bc), border = "white", xlim = c(0, 1.45),
              names.arg = rev(nm), cex.names = 0.98, las = 1,
              xlab = "Si / (Zr+Hf) 摩尔比",
              main = "F2  Si : (Zr+Hf) 摩尔比（理想锆石 = 1.000）\n绿色 = 偏离 <3%",
              cex.main = 1.22)
abline(v = 1, lty = 2, lwd = 2.4, col = "#c0392b")
arrows(1, bp, rev(v), bp, length = 0.09, lwd = 2.2, col = rev(bc), angle = 20)
text(pmax(rev(v), 1) + 0.05, bp, sprintf("%.3f", rev(v)), cex = 1.05, font = 2, adj = 0)
mtext(sprintf("仅「%s %.3f」与「%s %.3f」通过化学计量检验",
              FLAT[ok][1], v[ok][1], FLAT[ok][2], v[ok][2]),
      side = 1, line = 4.4, cex = 0.9, adj = 0, col = "#1a7f37")
par(op); grDevices::dev.off()

# =============================================================================
# F3  逐元素偏差： M3 归一法 ÷ 实验室
# =============================================================================
pngdev("F3_逐元素偏差.png", 1450, 2750)
op <- par(mar = c(4.5, 4.5, 4, 2.2), mgp = c(3, 0.8, 0), las = 1)
r3 <- summ[["比值_M3 归一法_fixedSi1"]]
names(r3) <- summ$元素
keep <- names(r3)[is.finite(r3) & r3 > 0]
r3 <- r3[keep]
o <- order(r3)
r3 <- r3[o]; kk <- names(r3)
lg <- log2(r3)
colr <- ifelse(kk %in% IFER, "#8e44ad",
               ifelse(kk %in% DILIM, "#95a5a6",
                      ifelse(lg >= 0, "#c0392b", "#1a7f37")))
bp <- barplot(lg, horiz = TRUE, col = colr, border = NA, las = 1,
              names.arg = kk, cex.names = 1.0, xlim = c(-0.55, 2.30),
              xlab = "log2( 归一法 fixed Si:1  ÷  实验室 AYCF )",
              main = "F3  逐元素偏差（0 = 与实验室一致）", cex.main = 1.25)
abline(v = 0, lwd = 1.8)
abline(v = log2(c(0.8, 1.25)), lty = 3, col = "#999999")
# 注意：horiz=TRUE 时 barplot 返回的是「y 位置」，所以标签必须写成 text(x=值, y=bp)
text(lg, bp, sprintf("%.2f", r3), pos = ifelse(lg >= 0, 4, 2), cex = 0.82, font = 2)
legend("bottomright", legend = c("偏高", "偏低", "多原子离子干扰通道", "实验室值近检出限"),
       fill = c("#c0392b", "#1a7f37", "#8e44ad", "#95a5a6"), border = NA,
       bty = "n", cex = 0.9, inset = c(0.02, 0.02))
par(op); grDevices::dev.off()

# =============================================================================
# F4  四口径 × 全元素 热图
# =============================================================================
pngdev("F4_口径元素热图.png", 2900, 1150)
els4 <- names(r3)                       # 与 F3 同序（按 M3÷实验室 从小到大）
Z <- vapply(MEAN4, function(g) {
  mg <- summ[[paste0("中位_", g)]]; names(mg) <- summ$元素
  ml <- summ[["中位_LAB AYCF"]];     names(ml) <- summ$元素
  log2(mg[els4] / ml[els4])
}, numeric(length(els4)))               # 元素 × 口径
Z <- Z[, rev(seq_along(MEAN4)), drop = FALSE]   # 翻转列序，使 M1 落在最上一行
rowlabs <- rev(SHORT[MEAN4])
ramp <- grDevices::colorRampPalette(c("#1a9850", "#91cf60", "#d9ef8b",
                                      "#ffffbf", "#fee08b", "#fc8d59", "#d73027"))
LO <- -0.5; HI <- 2.5; NC <- 60
# 颜色：超界截断；非有限值（M1/M2 的 Ni、Mo 中位数为 0 → 比值 0 → log2 = -Inf）
# 单独用灰底，不要拿绿色冒充「很低」。
mkc <- function(v) {
  v <- pmin(pmax(v, LO), HI)
  ramp(NC)[pmax(1, pmin(NC, round((v - LO) / (HI - LO) * (NC - 1)) + 1))]
}
cellcol <- mkc(as.vector(Z))
cellcol[!is.finite(as.vector(Z))] <- "#d0d0d0"
flab <- function(v) ifelse(is.finite(v), sprintf("%+.2f", v),
                           ifelse(is.na(v), "NA", "n.d."))

op <- par(mar = c(8.5, 10.5, 5.2, 2.0), mgp = c(3, 0.8, 0))
nE <- length(els4); nM <- ncol(Z)        # nE = 元素数（x 轴），nM = 口径数（y 轴）
plot(NA, xlim = c(0.5, nE + 0.5), ylim = c(0.5, nM + 0.5),
     axes = FALSE, xlab = "", ylab = "")
rect(rep(seq_len(nE), times = nM) - 0.5,
     rep(seq_len(nM), each = nE) - 0.5,
     rep(seq_len(nE), times = nM) + 0.5,
     rep(seq_len(nM), each = nE) + 0.5,
     col = cellcol, border = "white", lwd = 1.2)
for (i in seq_len(nM)) {
  v <- Z[, i]
  text(seq_len(nE), i, flab(v), cex = 0.66,
       col = ifelse(!is.finite(v), "#555555",
                    ifelse(v <= 0.15 | v >= 2.15, "white", "#2c2c2c")))
}
axis(1, seq_len(nE), els4, las = 2, cex.axis = 1.0, tick = FALSE)
axis(2, seq_len(nM), rowlabs, las = 1, cex.axis = 1.1, tick = FALSE)
box(col = "#666666")
mtext(sprintf("F4  四口径 × %d 元素：log2( 口径 ÷ 实验室 ) 偏差指纹", nE),
      side = 3, line = 3.0, cex = 1.25, font = 2)
mtext("灰底 n.d. = 该口径下中位数为 0（元素近检出限），比值无意义",
      side = 3, line = 1.6, cex = 0.85, col = "#666666")
# 底部色标
par(xpd = NA)
sx <- seq(1, length(els4), length.out = 41)
rect(sx[-length(sx)], -1.95, sx[-1], -1.35, col = mkc(seq(LO, HI, length.out = 41)[-41]),
     border = NA)
rect(min(sx), -1.95, max(sx), -1.35, border = "#666666")
text(sx[c(1, 11, 21, 31, 41)], -2.25, sprintf("%+.1f", seq(LO, HI, length.out = 41)[c(1, 11, 21, 31, 41)]),
     cex = 0.9)
text(min(sx) - 0.6, -1.65, "log2 偏差\n（绿=偏低，红=偏高）", cex = 0.9, adj = 1)
par(xpd = FALSE)
par(op); grDevices::dev.off()

# =============================================================================
# F5  稀土配分（球粒陨石标准化）
# =============================================================================
CHOND <- c(La = 0.237, Ce = 0.613, Pr = 0.0928, Nd = 0.457, Sm = 0.148, Eu = 0.0563,
           Gd = 0.199, Tb = 0.0361, Dy = 0.246, Ho = 0.0546, Er = 0.160, Tm = 0.0247,
           Yb = 0.161, Lu = 0.0246)   # McDonough & Sun (1995)
pngdev("F5_稀土配分.png", 1650, 1050)
op <- par(mar = c(4.5, 5.2, 4.4, 1.5), mgp = c(3.2, 0.8, 0))
yall <- cbind(
  vapply(names(Ml), function(g) vapply(names(CHOND), function(e) med(Ml[[g]][, e]) / CHOND[[e]], numeric(1)), numeric(14)),
  LAB = vapply(names(CHOND), function(e) med(LAB[, e]) / CHOND[[e]], numeric(1)))
matplot(seq_along(CHOND), yall, type = "b", pch = 19, lty = 1, lwd = 2.4,
        col = CL[colnames(yall)], xaxt = "n", log = "y",
        xlim = c(0.6, 14.6), ylim = c(0.3, 2.2e4),
        xlab = "", ylab = "样品 / 球粒陨石（McDonough & Sun 1995）",
        main = "F5  稀土配分：形状一致、整体平移（30 点中位数）", cex.main = 1.25)
axis(1, seq_along(CHOND), names(CHOND), cex.axis = 0.95)
# 五条曲线在图上几乎重合，逐条标注会互相压住——把「整体倍率」放进图例里
# 倍率 = 各稀土元素相对实验室倍数的中位数（反映整条线的平移量）
mult <- vapply(colnames(yall), function(g) med(yall[, g] / yall[, "LAB"]), numeric(1))
lgd  <- sprintf("%-22s ×%.2f", c(paste0(names(Ml)), "LAB AYCF"), mult)
legend("topleft", legend = lgd, col = CL[colnames(yall)],
       lty = 1, pch = 19, bty = "n", cex = 0.95, lwd = 2.2)
mtext("曲线形状重合 ⇒ 差异只是整体乘一个常数；图例中的 ×倍率即该常数",
      side = 1, line = 2.9, cex = 0.86, adj = 0, col = "#555555")
par(op); grDevices::dev.off()

# =============================================================================
# F6  各口径偏差分布箱线
# =============================================================================
pngdev("F6_偏差分布箱线.png", 1450, 1000)
op <- par(mar = c(5, 5, 4, 1.5), mgp = c(3.2, 0.8, 0), las = 1)
L <- vapply(MEAN4, function(g) {
  mg <- summ[[paste0("中位_", g)]]; names(mg) <- summ$元素
  ml <- summ[["中位_LAB AYCF"]];     names(ml) <- summ$元素
  e <- names(r3)
  log2(mg[e] / ml[e])
}, numeric(length(r3)))
boxplot(L, col = CL[MEAN4], border = "#333333", outline = FALSE, las = 1,
        names = SHORT[MEAN4], ylim = c(-1.5, 2.6), cex.axis = 0.95,
        ylab = "log2( 口径 ÷ 实验室 AYCF )",
        main = "F6  四个口径的偏差分布（箱 = 四分位，线 = 中位）", cex.main = 1.25)
abline(h = 0, lty = 2, lwd = 2, col = "#c0392b")
abline(h = log2(c(0.8, 1.25)), lty = 3, col = "#999999")
for (i in seq_along(MEAN4)) {
  points(jitter(rep(i, nrow(L)), amount = 0.12), L[, i], pch = 16,
         cex = 0.55, col = adjustcolor(CL[MEAN4][i], 0.45))
  text(i, max(L[, i]) + 0.15, sprintf("中位 %+.2f", median(L[, i])), cex = 0.88, font = 2)
}
# 注释挪到左上空白区；灰虚线 ±25% 参考带
text(0.55, 2.45, "红虚线 0 = 与实验室一致    灰虚线 = ±25%", adj = 0,
     cex = 0.88, col = "#c0392b")
par(op); grDevices::dev.off()

cat("完成，共 6 张图 →", PLOT, "\n")
cat(paste(list.files(PLOT), collapse = "\n"), "\n")
