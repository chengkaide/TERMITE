# =============================================================================
#  无内标「矿物化学式归一化」校准 · 数值自检
#  ---------------------------------------------------------------------------
#  思路：构造「已知成分、精确电荷平衡」的矿物，反推出它会产生什么净计数率
#  （cps = 真值 / 灵敏度 lambda），喂给归算函数，验证能还原已知成分。
#  这是纯代数还原：给定正确的 lambda，归算应当精确还原（相对偏差 < 1e-8）。
#
#  用法：  Rscript tests/test-formula.R
#  退出码：0 = 全部通过，1 = 有断言失败
# =============================================================================

root <- Sys.getenv("TERMITE_ROOT", unset = ".")
if (!file.exists(file.path(root, "R", "termite_core.R"))) root <- getwd()
if (!file.exists(file.path(root, "R", "termite_core.R")))
  stop("找不到 R/termite_core.R，请在仓库根目录运行，或设置 TERMITE_ROOT。")
setwd(root)
options(encoding = "UTF-8")
# 本机 R 的 native 编码若是 "C"（ASCII），source(encoding="UTF-8") 会因中文注释无法
# 转码而把整段代码吞掉、函数不定义。切到 UTF-8 原生 locale（Windows 10+ 支持），
# 不行再用 GBK 兜底；已是 MBCS/UTF-8 时这是无操作。
if (!l10n_info()[["MBCS"]] && !l10n_info()[["UTF-8"]]) {
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break
}
for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(f, encoding = "UTF-8")

cat("\n===================================================================\n")
cat("  TERMITE formula · 无内标矿物化学式归一化校准自检\n")
cat("===================================================================\n")

fails <- 0L
check_close <- function(label, got, want, tol = 1e-8) {
  got  <- as.numeric(got)
  want <- as.numeric(want)
  rel  <- max(abs(got - want) / pmax(abs(want), 1e-12))
  ok   <- is.finite(rel) && rel <= tol
  if (!ok) fails <<- fails + 1L
  cat(sprintf("  %-46s %s  (max_rel=%.2e)\n", label,
              if (ok) "OK" else "FAIL", rel))
}

# 价态与原子量（与 termite_formula.R 内部一致）
M <- c(Ca = 40.078, Sr = 87.62, Mn = 54.93804, W = 183.84, Si = 28.0855,
       P = 30.973761998, Na = 22.98976928, Al = 26.9815385, K = 39.0983)
val <- .termite_valence_default

# ---------------------------------------------------------------------------
# 1. 白钨矿 CaWO4（无水，电荷归一化）
#    Ca 位 = 1（Ca 0.98 + Sr 0.02，均 +2），W 位 = 1（+6），O = 4
# ---------------------------------------------------------------------------
cat("\n[1] 白钨矿（无水矿物电荷归一化）\n")
n_O <- 4
apfu_true <- c(Ca = 0.98, Sr = 0.02, W = 1.0)
M_total <- sum(apfu_true * M[c("Ca", "Sr", "W")]) + n_O * .M_O
conc_true <- apfu_true * M[c("Ca", "Sr", "W")] / M_total * 1e6
lam <- c(Ca = 0.085, Sr = 0.090, W = 0.15)          # 任意合理灵敏度
cps <- conc_true / lam
r <- termite_formula_anhydrous(cps, lam, M[c("Ca", "Sr", "W")],
                               val[c("Ca", "Sr", "W")], n_O)
check_close("Ca 还原", r$conc_ug_g["Ca"], conc_true["Ca"])
check_close("Sr 还原", r$conc_ug_g["Sr"], conc_true["Sr"])
check_close("W  还原", r$conc_ug_g["W"],  conc_true["W"])
check_close("apfu 还原 Ca", r$apfu["Ca"], apfu_true["Ca"])
check_close("apfu 还原 W",  r$apfu["W"],  apfu_true["W"])
check_close("电荷平衡 = 2·n_O", sum(r$apfu * val[c("Ca", "Sr", "W")]), 2 * n_O)

# ---------------------------------------------------------------------------
# 2. 氟磷灰石 Ca5(PO4)3F（结构式补 P、F）
#    Ca 位 = 5（Ca 4.9 + Sr 0.05 + Mn 0.05），P 位 = 3（P 2.9 + Si 0.1），F=1，O=12
# ---------------------------------------------------------------------------
cat("\n[2] 氟磷灰石（结构式补 P/F）\n")
apfu_true <- c(Ca = 4.9, Sr = 0.05, Mn = 0.05, Si = 0.1)
Fn_P <- 2.9; Fn_F <- 1; n_O <- 12
M_total <- sum(apfu_true * M[c("Ca", "Sr", "Mn", "Si")]) + Fn_P * .M_P + Fn_F * .M_F + n_O * .M_O
conc_true <- apfu_true * M[c("Ca", "Sr", "Mn", "Si")] / M_total * 1e6
conc_P    <- Fn_P * .M_P / M_total * 1e6
lam <- c(Ca = 0.085, Sr = 0.090, Mn = 0.088, Si = 0.075)
cps <- conc_true / lam
r <- termite_formula_apatite(cps, lam, M[c("Ca", "Sr", "Mn", "Si")],
                             val[c("Ca", "Sr", "Mn", "Si")])
check_close("Ca 还原", r$conc_ug_g["Ca"], conc_true["Ca"])
check_close("Sr 还原", r$conc_ug_g["Sr"], conc_true["Sr"])
check_close("Mn 还原", r$conc_ug_g["Mn"], conc_true["Mn"])
check_close("Si 还原", r$conc_ug_g["Si"], conc_true["Si"])
check_close("P  理论补", r$conc_ug_g["P"], conc_P)

# ---------------------------------------------------------------------------
# 3. 白云母 KAl2(AlSi3)O10(OH)2（结构式补 K、OH，AYCF2）
#    X 位 = 1（K 0.9 + Na 0.1），Y 位 Al 2.0，Z 位 Si 3.0 + Al 1.0（总 Al 3.0），O=10，OH=2
# ---------------------------------------------------------------------------
cat("\n[3] 白云母（结构式补 K/OH）\n")
apfu_true <- c(Na = 0.1, Al = 3.0, Si = 3.0)
Fn_K <- 0.9; Fn_OH <- 2; n_O <- 10
M_total <- sum(apfu_true * M[c("Na", "Al", "Si")]) + Fn_K * .M_K + Fn_OH * (.M_O + .M_H) + n_O * .M_O
conc_true <- apfu_true * M[c("Na", "Al", "Si")] / M_total * 1e6
conc_K    <- Fn_K * .M_K / M_total * 1e6
lam <- c(Na = 0.12, Al = 0.06, Si = 0.05)
cps <- conc_true / lam
r <- termite_formula_mica(cps, lam, M[c("Na", "Al", "Si")],
                          val[c("Na", "Al", "Si")])
check_close("Na 还原", r$conc_ug_g["Na"], conc_true["Na"])
check_close("Al 还原", r$conc_ug_g["Al"], conc_true["Al"])
check_close("Si 还原", r$conc_ug_g["Si"], conc_true["Si"])
check_close("K  理论补", r$conc_ug_g["K"],  conc_K)

# ---------------------------------------------------------------------------
# 3b. mineral_formulas.csv 能被正确解析（6 种矿物、含中文备注、BOM）
# ---------------------------------------------------------------------------
cat("\n[3b] mineral_formulas.csv 解析\n")
cfg0 <- list(dir = "", app_dir = root)
defs_csv <- termite_mineral_defs(cfg0, "TERMITEScriptFolder/mineral_formulas.csv")
check_close("CSV 矿物数 = 6", nrow(defs_csv), 6)
check_close("CSV 含 scheelite", sum(defs_csv$name == "scheelite"), 1)
check_close("scheelite mode = anhydrous",
            sum(defs_csv$name == "scheelite" & defs_csv$mode == "anhydrous"), 1)
check_close("fluorapatite excluded = P",
            sum(defs_csv$name == "fluorapatite" & defs_csv$excluded == "P"), 1)

# ---------------------------------------------------------------------------
# 4. 真实白钨矿数据端到端冒烟（若目录存在）
# ---------------------------------------------------------------------------
qt_dir <- "G:/3.珊瑚钨锡矿床/锡石-白钨矿微量元素/20230806CKDA"
if (dir.exists(qt_dir)) {
  cat("\n[4] 真实白钨矿数据（端到端）\n")
  cfg <- termite_defaults("spot")
  cfg$dir <- qt_dir; cfg$layout <- "flat"; cfg$auto_detect <- TRUE
  cfg$dir_sample <- ""; cfg$ref_names <- c("SRM 610", "SRM 612")
  cfg$qc_names <- c("BIR-1G", "BCR-2G", "BHVO-2G")
  cfg$first_blank <- 25; cfg$last_blank <- 60
  cfg$first_signal <- 70; cfg$last_signal <- 118
  r <- termite_run_formula(cfg, "scheelite")
  W  <- r$samples[, "W"]
  Ca <- r$samples[, "Ca"]
  cat(sprintf("    样品数 %d；W 中位数 %.0f（理论 638500）；Ca 中位数 %.0f（理论 139200）\n",
              nrow(r$samples), stats::median(W, na.rm = TRUE),
              stats::median(Ca, na.rm = TRUE)))
  rel_w <- abs(stats::median(W, na.rm = TRUE) - 638500) / 638500
  check_close("W 接近化学计量（+/-5%）", stats::median(W, na.rm = TRUE), 638500, tol = 0.05)
} else {
  cat("\n[4] 跳过真实数据用例：找不到 ", qt_dir, "\n", sep = "")
}

cat("\n===================================================================\n")
if (fails == 0L) cat("  结果：全部通过\n") else cat(sprintf("  结果：%d 项失败\n", fails))
cat("===================================================================\n\n")
if (fails > 0L) quit(status = 1L)
