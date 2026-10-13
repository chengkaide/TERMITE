#!/usr/bin/env Rscript
# =============================================================================
#  锆石微量元素「多口径」对比：Zr 内标 / Si 内标 / 归一法 × 实验室 AYCF
#  ---------------------------------------------------------------------------
#  读入 tools/zircon_methods.R 产出的 4 张口径表 + 实验室 AYCF 参考值，
#  在**全部 38 个实测元素**上对齐比较（不只 REE）：
#
#    · 逐样品 × 逐元素长表（5 个口径并排）
#    · 逐元素汇总统计（各口径中位数、相对实验室的比值、样品间 RSD）
#    · 参考物质覆盖率诊断（NIST610/612 在当前通道下能看到多少质量）
#    · 四联图：主量闭合 / 逐元素比值 / Si:Zr 摩尔比 / REE 配分
#
#  用法：Rscript tools/zircon_compare.R
# =============================================================================

options(encoding = "UTF-8")
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.args <- commandArgs(trailingOnly = FALSE)
scr <- sub("^--file=", "", .args[grepl("^--file=", .args)])
ROOT <- if (length(scr)) dirname(dirname(normalizePath(scr[1], mustWork = FALSE))) else getwd()
for (f in list.files(file.path(ROOT, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  source(f, encoding = "UTF-8")

# 输出目录用环境变量传入（脚本要进公开仓库，不写死任何本机路径）
OUT <- Sys.getenv("OUT", "")
if (!nzchar(OUT))
  stop("请先用环境变量 OUT 指定结果目录（即 tools/zircon_methods.R 的输出目录）。")

read_el <- function(f) {
  d <- utils::read.csv(file.path(OUT, f), check.names = FALSE,
                       fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  m <- as.matrix(d[, -1, drop = FALSE])
  rownames(m) <- d[[1]]
  m
}
M <- list("M1 Zr内标"        = read_el("方法_M1_Zr内标.csv"),
          "M2 Si内标"        = read_el("方法_M2_Si内标.csv"),
          "M3 归一法_fixedSi1" = read_el("方法_M3_归一法_fixedSi1.csv"),
          "M4 归一法_anhydrous" = read_el("方法_M4_归一法_anhydrous.csv"))

# 只留 30 个锆石测点（内标法结果里也含参考/监控样品，这里排除）
# 锆石测点的样品名前缀由环境变量给出（不把真实样品号写进仓库）。
# 不指定时取全部行——此时若 CSV 里混有参考物质行，需自行确认。
ZPRE <- Sys.getenv("ZIRCON_PREFIX", "")
zs <- if (nzchar(ZPRE)) grep(ZPRE, rownames(M[[1]]), value = TRUE) else rownames(M[[1]])
# 自然排序：按样品名末尾的数字排，否则 _10 会排到 _2 前面
num <- suppressWarnings(as.integer(sub("^.*?(\\d+)\\D*$", "\\1", zs)))
zs <- if (all(is.na(num))) sort(zs) else zs[order(num, zs)]
M  <- lapply(M, function(m) m[zs, , drop = FALSE])
cat("锆石测点 ", length(zs), " 个，元素 ", ncol(M[[1]]), " 个\n", sep = "")

# --- 实验室 AYCF 参考值 ------------------------------------------------------
lab_long <- utils::read.csv(file.path(OUT, "实验室AYCF参考值.csv"),
                            fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
lab_long <- lab_long[lab_long$sample %in% zs, ]
els <- colnames(M[[1]])
LAB <- matrix(NA_real_, length(zs), length(els), dimnames = list(zs, els))
for (i in seq_len(nrow(lab_long)))
  if (lab_long$element[i] %in% els)
    LAB[lab_long$sample[i], lab_long$element[i]] <- lab_long$value_ug_g[i]
M[["LAB AYCF"]] <- LAB

# --- 1. 长表：样品 × 元素 × 口径 --------------------------------------------
long <- do.call(rbind, lapply(names(M), function(g) {
  m <- M[[g]]
  data.frame(样品 = rep(rownames(m), times = ncol(m)),
             元素 = rep(colnames(m), each  = nrow(m)),
             口径 = g, 浓度_ug_g = as.numeric(m), stringsAsFactors = FALSE)
}))
wide <- reshape(long, idvar = c("样品", "元素"), timevar = "口径", direction = "wide")
names(wide) <- sub("^浓度_ug_g[.]", "", names(wide))
ref <- "LAB AYCF"
for (g in setdiff(names(M), ref))
  wide[[paste0("比值_", sub(" ", "", g), "_比实验室")]] <- wide[[g]] / wide[[ref]]
utils::write.csv(wide, file.path(OUT, "方法对比_全元素长表.csv"),
                 row.names = FALSE, na = "NA")

# --- 2. 逐元素汇总 ----------------------------------------------------------
med <- function(x) stats::median(x, na.rm = TRUE)
rsd <- function(x) { x <- x[is.finite(x) & x > 0]; if (length(x) < 3) NA_real_ else stats::sd(x) / mean(x) * 100 }
summ <- data.frame(元素 = els)
for (g in names(M)) {
  v <- vapply(els, function(e) med(M[[g]][, e]), numeric(1)); names(v) <- els
  summ[[paste0("中位_", g)]] <- v
}
for (g in setdiff(names(M), ref))
  summ[[paste0("比值_", g)]] <- summ[[paste0("中位_", g)]] / summ[[paste0("中位_", ref)]]
summ[["实验室_RSD%"]] <- vapply(els, function(e) rsd(LAB[, e]), numeric(1))
summ[["Zr内标_RSD%"]] <- vapply(els, function(e) rsd(M[["M1 Zr内标"]][, e]), numeric(1))
# 比值口径之间的一致性：M3 与 M4 的相对差
summ[["M3比M4"]] <- summ[["中位_M3 归一法_fixedSi1"]] / summ[["中位_M4 归一法_anhydrous"]]
utils::write.csv(summ, file.path(OUT, "方法对比_汇总统计.csv"), row.names = FALSE, na = "NA")

cat("\n=== 逐元素：各口径中位数 ÷ 实验室 AYCF（只列 >1 µg/g 的元素）===\n")
big <- els[vapply(els, function(e) med(LAB[, e]) > 1, logical(1))]
tab <- summ[match(big, summ$元素),
            c("元素", "中位_LAB AYCF", "比值_M1 Zr内标", "比值_M2 Si内标",
              "比值_M3 归一法_fixedSi1", "比值_M4 归一法_anhydrous")]
tab[, -1] <- lapply(tab[, -1], function(x) round(x, 3))
print(tab, row.names = FALSE)

# --- 3. 主量闭合 ------------------------------------------------------------
M_O <- 15.999
oxo <- function(x, Mw, r) x * (Mw + r * M_O) / Mw / 1e4
REE <- c("La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu")
closure <- function(m) {
  g <- function(e) if (e %in% colnames(m)) as.numeric(m[, e]) else rep(0, nrow(m))
  sio2 <- oxo(g("Si"), 28.0855, 2); zro2 <- oxo(g("Zr"), 91.224, 2)
  hfo2 <- oxo(g("Hf"), 178.49, 2)
  uo2  <- oxo(g("U"), 238.02891, 2); tho2 <- oxo(g("Th"), 232.0377, 2)
  y2o3 <- oxo(g("Y"), 88.90584, 1.5)
  reo  <- vapply(REE, function(e) oxo(g(e), .termite_M_default[[e]], 1.5), numeric(nrow(m)))
  others <- oxo(g("Ti"), 47.867, 2) + oxo(g("Ca"), 40.078, 1) + oxo(g("K"), 39.0983, 0.5) +
            oxo(g("Fe"), 55.845, 1) + oxo(g("Nb"), 92.90637, 2.5) + oxo(g("Ta"), 180.94788, 2.5) +
            oxo(g("W"), 183.84, 3) + oxo(g("Pb"), 207.2, 1)
  c(SiO2 = mean(sio2), ZrO2 = mean(zro2), HfO2 = mean(hfo2),
    UTh = mean(uo2 + tho2), YREE = mean(y2o3 + rowSums(reo)), 其他 = mean(others))
}
cl_num <- t(vapply(M, closure, numeric(6)))       # 口径 × 组分（数值）
cl     <- data.frame(口径 = rownames(cl_num), round(cl_num, 3),
                     总和 = round(rowSums(cl_num), 2), row.names = NULL)
utils::write.csv(cl, file.path(OUT, "主量氧化物闭合_五口径.csv"), row.names = FALSE)
cat("\n=== 主量氧化物闭合（30 点平均，wt%）===\n"); print(cl, row.names = FALSE)

# Si/Zr 摩尔比：理想 (Zr,Hf)SiO4 应为 1.00
mol <- t(vapply(M, function(m) {
  g <- function(e) mean(as.numeric(m[, e]), na.rm = TRUE)
  c(Si = g("Si") / 28.0855, Zr = g("Zr") / 91.224, Hf = g("Hf") / 178.49)
}, numeric(3)))
sr <- data.frame(口径 = rownames(mol), Si比ZrHf = mol[, "Si"] / (mol[, "Zr"] + mol[, "Hf"]),
                 Si比Zr = mol[, "Si"] / mol[, "Zr"])
utils::write.csv(sr, file.path(OUT, "SiZr摩尔比_五口径.csv"), row.names = FALSE)
cat("\n=== Si : (Zr+Hf) 摩尔比（理想锆石 = 1.000）===\n")
print(data.frame(口径 = sr$口径, Si比ZrHf = round(sr$Si比ZrHf, 4)), row.names = FALSE)

# --- 4. 参考物质覆盖率（回答「SRM 玻璃能不能归一到 100%」）------------------
cfg <- termite_defaults("spot"); cfg$app_dir <- ROOT
std_tab <- termite_read_standards(termite_resource(cfg$file_standards, cfg))
cov <- lapply(c("NIST610", "NIST612", "NIST614"), function(rm) {
  cv <- termite_ref_coverage(std_tab, rm, els)
  if (is.null(cv)) return(NULL)
  data.frame(参考物质 = rm, 满质量_wt = round(cv$full_wt_pct, 2),
             当前通道可见_wt = round(cv$measured_wt_pct, 2),
             覆盖率 = round(cv$coverage, 4),
             缺失主要组分 = paste(sprintf("%s %.2f wt%%", names(cv$missing_wt_pct),
                                          cv$missing_wt_pct), collapse = "；"))
})
cov <- do.call(rbind, cov)
utils::write.csv(cov, file.path(OUT, "参考物质覆盖率诊断.csv"), row.names = FALSE)
cat("\n=== 参考物质质量覆盖率（当前 38 元素通道）===\n"); print(cov, row.names = FALSE)

# --- 5. 图 ------------------------------------------------------------------
CHOND <- c(La = 0.237, Ce = 0.613, Pr = 0.0928, Nd = 0.457, Sm = 0.148, Eu = 0.0563,
           Gd = 0.199, Tb = 0.0361, Dy = 0.246, Ho = 0.0546, Er = 0.160, Tm = 0.0247,
           Yb = 0.161, Lu = 0.0246)   # McDonough & Sun (1995)

COL <- c("M1 Zr内标" = "#2c7fb8", "M2 Si内标" = "#e6550d",
         "M3 归一法_fixedSi1" = "#31a354", "M4 归一法_anhydrous" = "#756bb1",
         "LAB AYCF" = "#111111")
png(file.path(OUT, "三法对比图.png"), width = 2000, height = 1500, res = 190)
op <- par(mfrow = c(2, 2), mar = c(4.5, 5, 3, 1.5), mgp = c(3, 0.8, 0), las = 1,
          family = "sans", cex.axis = 0.85, cex.lab = 0.95)

## (A) 主量氧化物闭合：堆叠柱 + 100 wt% 参考线
barplot(t(cl_num[, c("SiO2", "ZrO2", "HfO2", "UTh", "YREE", "其他")]), horiz = FALSE,
        col = c("#4a90d9", "#d95f02", "#7570b3", "#66a61e", "#e7298a", "#bdbdbd"),
        border = "white", ylim = c(0, 148), las = 1,
        names.arg = c("Zr内标", "Si内标", "归一法\nfixedSi1", "归一法\nanhydrous", "实验室\nAYCF"),
        ylab = "氧化物加和 (wt%)", main = "(A) 主量氧化物闭合：锆石应 ≈ 100 wt%")
abline(h = 100, lty = 2, lwd = 2, col = "#c0392b")
text(1:5, rowSums(cl_num) + 4,
     labels = sprintf("%.1f", rowSums(cl_num)), cex = 0.8, font = 2)
legend("topright", legend = c("SiO2", "ZrO2", "HfO2", "U+Th", "Y+REE", "其他"),
       fill = c("#4a90d9", "#d95f02", "#7570b3", "#66a61e", "#e7298a", "#bdbdbd"),
       border = "white", bty = "n", cex = 0.72)

## (B) 逐元素：各口径中位数 ÷ 实验室（log2）
big2 <- els[vapply(els, function(e) med(LAB[, e]) > 1, logical(1))]
ord  <- order(summ[["比值_M1 Zr内标"]][match(big2, summ$元素)], decreasing = TRUE)
big3 <- big2[ord]
# 4 行（口径）× N 列（元素）；横向条形便于放下 20+ 个元素符号
ratio <- vapply(big3, function(e)
  vapply(setdiff(names(M), ref),
         function(g) log2(med(M[[g]][, e]) / med(LAB[, e])), numeric(1)), numeric(4))
barplot(ratio, beside = TRUE, horiz = TRUE, las = 1, cex.names = 0.62,
        col = COL[rownames(ratio)], border = NA, xlim = c(-1.35, 1.35),
        xlab = "log2(口径 ÷ 实验室)", main = "(B) 逐元素偏差（0 = 与实验室一致）")
abline(v = 0, lwd = 1.5)
abline(v = c(-0.32, 0.32), lty = 3, col = "#888888")   # ±25%
legend("topright", legend = sub("M\\d ", "", rownames(ratio)), fill = COL[rownames(ratio)],
       border = NA, bty = "n", cex = 0.7)

## (C) Si : (Zr+Hf) 摩尔比
bp <- barplot(sr$Si比ZrHf, col = c(COL[rownames(sr)[1:4]]), border = "white",
              names.arg = c("Zr内标", "Si内标", "归一法\nfixedSi1", "归一法\nanhydrous", "实验室\nAYCF")[seq_len(nrow(sr))],
              ylim = c(0, 1.5), ylab = "Si / (Zr+Hf) 摩尔比",
              main = "(C) Si : (Zr+Hf) 摩尔比（理想锆石 = 1.00）")
abline(h = 1, lty = 2, lwd = 2, col = "#c0392b")
text(bp, sr$Si比ZrHf + 0.05, sprintf("%.2f", sr$Si比ZrHf), cex = 0.85, font = 2)

## (D) REE 配分（球粒陨石标准化，中位数）
y <- vapply(names(M), function(g) {
  vapply(names(CHOND), function(e) med(M[[g]][, e]) / CHOND[[e]], numeric(1))
}, numeric(length(CHOND)))
matplot(seq_along(CHOND), y, type = "b", pch = 19, lty = 1, lwd = 2,
        col = COL[colnames(y)], xaxt = "n", log = "y", ylim = c(1, 1e5),
        xlab = "", ylab = "样品 / 球粒陨石（McDonough & Sun 1995）",
        main = "(D) 稀土配分（30 点中位数）")
axis(1, seq_along(CHOND), names(CHOND), cex.axis = 0.75)
legend("topleft", legend = names(M), col = COL[names(M)], lty = 1, pch = 19,
       bty = "n", cex = 0.72)
par(op); dev.off()
cat("\n图：", file.path(OUT, "三法对比图.png"), "\n")

# --- 6. 结论速览 ------------------------------------------------------------
cat("\n================ 结论速览 ================\n")
tt <- rowSums(cl_num)
cat(sprintf("① 主量闭合（30 点平均）：Zr内标 %.1f wt%%（>100 不可能）、Si内标 %.1f wt%%（太低）、\n",
            tt[1], tt[2]))
cat(sprintf("   归一法 fixedSi1 %.1f、归一法 anhydrous %.1f、实验室 AYCF %.1f wt%%\n",
            tt[3], tt[4], tt[5]))
cat(sprintf("② Si : (Zr+Hf)（理想 1.00）：Zr内标 %.2f、Si内标 %.2f、fixedSi1 %.2f、anhydrous %.2f、实验室 %.2f\n",
            sr$Si比ZrHf[1], sr$Si比ZrHf[2], sr$Si比ZrHf[3], sr$Si比ZrHf[4], sr$Si比ZrHf[5]))
cat(sprintf("③ NIST612 覆盖率 %.1f%%（缺 %s）→ 玻璃标样不能归一到 100%%\n",
            cov$覆盖率[cov$参考物质 == "NIST612"] * 100,
            cov$缺失主要组分[cov$参考物质 == "NIST612"]))
cat(sprintf("   而锆石样品 SiO2+ZrO2+HfO2 = %.1f wt%%，主量全在通道内 → 样品可以归一\n",
            sum(cl_num["M3 归一法_fixedSi1", c("SiO2", "ZrO2", "HfO2")])))
