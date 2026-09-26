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
       P = 30.973761998, Na = 22.98976928, Al = 26.9815385, K = 39.0983,
       Be = 9.0121831, B = 10.81, Cr = 51.9961, Sc = 44.955908,
       Mg = 24.305, Fe = 55.845, Zr = 91.224, Hf = 178.49,
       Ti = 47.867, V = 50.9415, Zn = 65.38)
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
# 3c. mineral_formulas.csv 能被正确解析（11 种矿物、含中文备注、BOM）
# ---------------------------------------------------------------------------
cat("\n[3c] mineral_formulas.csv 解析\n")
cfg0 <- list(dir = "", app_dir = root)
defs_csv <- termite_mineral_defs(cfg0, "TERMITEScriptFolder/mineral_formulas.csv")
check_close("CSV 矿物数 = 11", nrow(defs_csv), 11)
check_close("CSV 含 scheelite", sum(defs_csv$name == "scheelite"), 1)
check_close("scheelite mode = anhydrous",
            sum(defs_csv$name == "scheelite" & defs_csv$mode == "anhydrous"), 1)
check_close("fluorapatite excluded = P",
            sum(defs_csv$name == "fluorapatite" & defs_csv$excluded == "P"), 1)
check_close("beryl fixed = Be:3",
            sum(defs_csv$name == "beryl" & defs_csv$fixed == "Be:3"), 1)
check_close("tourmaline fixed = B:3;Si:6",
            sum(defs_csv$name == "tourmaline" & defs_csv$fixed == "B:3;Si:6"), 1)
check_close("zircon fixed = Si:1",
            sum(defs_csv$name == "zircon" & defs_csv$fixed == "Si:1"), 1)
check_close("zircon 用 fixed 模式",
            sum(defs_csv$name == "zircon" & defs_csv$mode == "fixed"), 1)
check_close("zircon_full 用 anhydrous 模式",
            sum(defs_csv$name == "zircon_full" & defs_csv$mode == "anhydrous"), 1)
check_close("magnetite 用 stoich 模式、n_cat = 3",
            sum(defs_csv$name == "magnetite" & defs_csv$mode == "stoich" &
                defs_csv$n_cat == 3), 1)
check_close("chromite 用 stoich 模式、n_cat = 3",
            sum(defs_csv$name == "chromite" & defs_csv$mode == "stoich" &
                defs_csv$n_cat == 3), 1)

# ---------------------------------------------------------------------------
# 4. 绿柱石 Be3Al2Si6O18（fixed 模式，固定扣除 Be）
#    Al 位 = 2（Al 1.8 + Cr 0.1 + Sc 0.1，均 +3），Si = 6，Be 固定 3，O = 18
#    可测阳离子电荷目标 = 2·18 − 3·2 = 30
# ---------------------------------------------------------------------------
cat("\n[4] 绿柱石（固定扣除 Be）\n")
apfu_true <- c(Al = 1.8, Cr = 0.1, Sc = 0.1, Si = 6.0)
Fn_Be <- 3; n_O <- 18
M_total <- sum(apfu_true * M[c("Al", "Cr", "Sc", "Si")]) + Fn_Be * M["Be"] + n_O * .M_O
conc_true <- apfu_true * M[c("Al", "Cr", "Sc", "Si")] / M_total * 1e6
conc_Be   <- Fn_Be * M["Be"] / M_total * 1e6
lam <- c(Al = 0.06, Cr = 0.10, Sc = 0.09, Si = 0.05)
cps <- conc_true / lam
r <- termite_formula_fixed(cps, lam, M[c("Al", "Cr", "Sc", "Si")],
                           val[c("Al", "Cr", "Sc", "Si")],
                           n_O = n_O, fixed = c(Be = 3))
check_close("Al 还原", r$conc_ug_g["Al"], conc_true["Al"])
check_close("Cr 还原", r$conc_ug_g["Cr"], conc_true["Cr"])
check_close("Sc 还原", r$conc_ug_g["Sc"], conc_true["Sc"])
check_close("Si 还原", r$conc_ug_g["Si"], conc_true["Si"])
check_close("Be 理论补", r$conc_ug_g["Be"], conc_Be)
check_close("apfu Be = 3", r$apfu["Be"], 3)
check_close("电荷目标 = 30", r$charge_target, 30)

# ---------------------------------------------------------------------------
# 5. 电气石 XY3Z6(T6O18)(BO3)3(OH)4（fixed 模式，固定扣除 B、Si + OH 修正）
#    X 位 Na 0.8 + Ca 0.1，Y 位 Mg 2.6 + Fe 0.4，Z 位 Al 6.0；B=3、Si=6、O=31、OH=4
#    可测阳离子电荷目标 = 2·31 − 4 − (3×3 + 6×4) = 25
# ---------------------------------------------------------------------------
cat("\n[5] 电气石（固定扣除 B/Si + OH 修正）\n")
apfu_true <- c(Na = 0.8, Ca = 0.1, Mg = 2.6, Fe = 0.4, Al = 6.0)
Fn_B <- 3; Fn_Si <- 6; n_O <- 31; n_OH <- 4
M_total <- sum(apfu_true * M[c("Na", "Ca", "Mg", "Fe", "Al")]) +
           Fn_B * M["B"] + Fn_Si * M["Si"] + n_O * .M_O + n_OH * .M_H
conc_true <- apfu_true * M[c("Na", "Ca", "Mg", "Fe", "Al")] / M_total * 1e6
conc_B  <- Fn_B * M["B"] / M_total * 1e6
conc_Si <- Fn_Si * M["Si"] / M_total * 1e6
lam <- c(Na = 0.12, Ca = 0.085, Mg = 0.07, Fe = 0.08, Al = 0.06)
cps <- conc_true / lam
r <- termite_formula_fixed(cps, lam, M[c("Na", "Ca", "Mg", "Fe", "Al")],
                           val[c("Na", "Ca", "Mg", "Fe", "Al")],
                           n_O = n_O, fixed = c(B = 3, Si = 6), n_OH = n_OH)
check_close("Na 还原", r$conc_ug_g["Na"], conc_true["Na"])
check_close("Ca 还原", r$conc_ug_g["Ca"], conc_true["Ca"])
check_close("Mg 还原", r$conc_ug_g["Mg"], conc_true["Mg"])
check_close("Fe 还原", r$conc_ug_g["Fe"], conc_true["Fe"])
check_close("Al 还原", r$conc_ug_g["Al"], conc_true["Al"])
check_close("B  理论补", r$conc_ug_g["B"], conc_B)
check_close("Si 理论补", r$conc_ug_g["Si"], conc_Si)
check_close("apfu B = 3", r$apfu["B"], 3)
check_close("apfu Si = 6", r$apfu["Si"], 6)
check_close("电荷目标 = 25", r$charge_target, 25)

# ---------------------------------------------------------------------------
# 5b. 锆石 ZrSiO4（fixed 模式，固定扣除 Si=1）
#     Si 在 ICP-MS 上灵敏度差、且常有 29Si 的多原子干扰，按结构式固定为 1 apfu。
#     剩下可测阳离子的电荷目标 = 2·4 − 4·1 = 4，即「Zr 位 = 1 apfu」约束。
#     Zr 位 = 1（Zr 0.98 + Hf 0.02，均 +4），O = 4
# ---------------------------------------------------------------------------
cat("\n[5b] 锆石（固定扣除 Si=1）\n")
apfu_true <- c(Zr = 0.98, Hf = 0.02)
Fn_Si <- 1; n_O <- 4
M_total <- sum(apfu_true * M[c("Zr", "Hf")]) + Fn_Si * M["Si"] + n_O * .M_O
conc_true <- apfu_true * M[c("Zr", "Hf")] / M_total * 1e6
conc_Si   <- Fn_Si * M["Si"] / M_total * 1e6
lam <- c(Zr = 0.11, Hf = 0.10)
cps <- conc_true / lam
r <- termite_formula_fixed(cps, lam, M[c("Zr", "Hf")], val[c("Zr", "Hf")],
                           n_O = n_O, fixed = c(Si = 1))
check_close("Zr 还原", r$conc_ug_g["Zr"], conc_true["Zr"])
check_close("Hf 还原", r$conc_ug_g["Hf"], conc_true["Hf"])
check_close("Si 理论补", r$conc_ug_g["Si"], conc_Si)
check_close("apfu Si = 1", r$apfu["Si"], 1)
check_close("电荷目标 = 4", r$charge_target, 4)
# 交叉检验：纯 ZrSiO4（Zr 位 = 1 apfu）⟹ Zr 含量应等于化学计量值 49.77 wt%
cps_pure <- c(Zr = 49.77e4 / 0.11)
r_pure <- termite_formula_fixed(cps_pure, c(Zr = 0.11), M["Zr"], val["Zr"],
                                n_O = 4, fixed = c(Si = 1))
check_close("纯 ZrSiO4 的 Zr 含量 = 49.77%", r_pure$conc_ug_g[["Zr"]] / 1e4,
            49.77, tol = 1e-3)
check_close("纯 ZrSiO4 的 Si 含量 = 15.32%", r_pure$conc_ug_g[["Si"]] / 1e4,
            15.32, tol = 1e-3)

# ---------------------------------------------------------------------------
# 5c. 磁铁矿 Fe3O4（stoich 模式，阳离子位点总数 = 3）
#     尖晶石 AB2O4：四面体 1 + 八面体 2，阳离子总数恒 = 3，O = 4。
#     Ti/Mg/Al 替代 Fe 时总数不变；Fe²⁺/Fe³⁺ 比不定，故不用电荷归一化。
#     设 Fe 2.90 + Ti 0.05 + Mg 0.03 + Al 0.02 = 3.00
# ---------------------------------------------------------------------------
cat("\n[5c] 磁铁矿（阳离子位点 = 3）\n")
apfu_true <- c(Fe = 2.90, Ti = 0.05, Mg = 0.03, Al = 0.02)
n_O <- 4; n_cat <- 3
M_total <- sum(apfu_true * M[c("Fe", "Ti", "Mg", "Al")]) + n_O * .M_O
conc_true <- apfu_true * M[c("Fe", "Ti", "Mg", "Al")] / M_total * 1e6
lam <- c(Fe = 0.09, Ti = 0.10, Mg = 0.07, Al = 0.06)
cps <- conc_true / lam
r <- termite_formula_stoich(cps, lam, M[c("Fe", "Ti", "Mg", "Al")],
                            val[c("Fe", "Ti", "Mg", "Al")],
                            n_cat = n_cat, n_O = n_O)
check_close("Fe 还原", r$conc_ug_g["Fe"], conc_true["Fe"])
check_close("Ti 还原", r$conc_ug_g["Ti"], conc_true["Ti"])
check_close("Mg 还原", r$conc_ug_g["Mg"], conc_true["Mg"])
check_close("Al 还原", r$conc_ug_g["Al"], conc_true["Al"])
check_close("位点目标 = 3", r$site_target, 3)
check_close("apfu 总和 = 3", sum(r$apfu), 3)
# 纯 Fe3O4 端元的 Fe 含量应为 72.36 wt%：把替代元素去掉再算一遍
cps_pure <- c(Fe = 72.36e4 / 0.09)
r_pure <- termite_formula_stoich(cps_pure, c(Fe = 0.09), M["Fe"], val["Fe"],
                                 n_cat = 3, n_O = 4)
check_close("纯 Fe3O4 的 Fe 含量 = 72.36%", r_pure$conc_ug_g[["Fe"]] / 1e4,
            72.36, tol = 1e-3)

# ---------------------------------------------------------------------------
# 5d. 铬铁矿 FeCr2O4（stoich 模式，同为尖晶石，阳离子位点 = 3）
#     Fe 0.95 + Cr 2.00 + Mg 0.05 = 3.00，O = 4
# ---------------------------------------------------------------------------
cat("\n[5d] 铬铁矿（阳离子位点 = 3）\n")
apfu_true <- c(Fe = 0.95, Cr = 2.00, Mg = 0.05)
M_total <- sum(apfu_true * M[c("Fe", "Cr", "Mg")]) + n_O * .M_O
conc_true <- apfu_true * M[c("Fe", "Cr", "Mg")] / M_total * 1e6
lam <- c(Fe = 0.09, Cr = 0.10, Mg = 0.07)
cps <- conc_true / lam
r <- termite_formula_stoich(cps, lam, M[c("Fe", "Cr", "Mg")],
                            val[c("Fe", "Cr", "Mg")],
                            n_cat = 3, n_O = 4)
check_close("Fe 还原", r$conc_ug_g["Fe"], conc_true["Fe"])
check_close("Cr 还原", r$conc_ug_g["Cr"], conc_true["Cr"])
check_close("Mg 还原", r$conc_ug_g["Mg"], conc_true["Mg"])
check_close("apfu 总和 = 3", sum(r$apfu), 3)

# ---------------------------------------------------------------------------
# 6. 真实白钨矿数据端到端冒烟（若目录存在）
# ---------------------------------------------------------------------------
qt_dir <- "G:/3.珊瑚钨锡矿床/锡石-白钨矿微量元素/20230806CKDA"
if (dir.exists(qt_dir)) {
  cat("\n[6] 真实白钨矿数据（端到端）\n")
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
  cat("\n[6] 跳过真实数据用例：找不到 ", qt_dir, "\n", sep = "")
}

# ---------------------------------------------------------------------------
# 7. 参考物质质量覆盖率诊断（SRM 玻璃在微量元素通道下看不到 100%）
# ---------------------------------------------------------------------------
cat("\n[7] 参考物质覆盖率诊断\n")

# 构造一个 NIST610 风格的假标样：主量 Si/Na/Ca/Al + 一个微量元素 Zr
std_fake <- data.frame(row.names = c("FAKE610"),
                       Si28 = 325800, Na23 = 99400, Ca43 = 82200,
                       Al27 = 10300, Zr91 = 400, check.names = FALSE)
# 只测 Si、Ca、Zr（不测 Na、Al）—— 正是锆石微量元素方法的典型通道配置
cov <- termite_ref_coverage(std_fake, "FAKE610", c("Si", "Ca", "Zr"))

check_close("覆盖率对象非空", length(cov) > 0, TRUE)
check_close("全元素折算氧化物 ≈ 96.6 wt%", cov$full_wt_pct, 96.59, tol = 1e-3)
check_close("实测通道折算氧化物 ≈ 81.2 wt%", cov$measured_wt_pct, 81.25, tol = 1e-3)
check_close("覆盖率 ≈ 0.841（远不到 100%）", cov$coverage, 0.8411, tol = 1e-3)
check_close("缺口主因是 Na ≈ 9.94 wt%", cov$missing_wt_pct[["Na"]], 9.94, tol = 1e-3)
check_close("其次是 Al ≈ 1.03 wt%", cov$missing_wt_pct[["Al"]], 1.03, tol = 1e-3)

# 通道测全时覆盖率应回到 1
cov_all <- termite_ref_coverage(std_fake, "FAKE610",
                                c("Si", "Na", "Ca", "Al", "Zr"))
check_close("测全元素时覆盖率 = 1", cov_all$coverage, 1.0, tol = 1e-8)

# 找不到的参考物质返回 NULL（不该让调用方崩）
check_close("未知参考物质返回 NULL",
            is.null(termite_ref_coverage(std_fake, "NOPE", c("Si"))), TRUE)

cat(sprintf("    覆盖率 %.1f%%（实测 %.2f / 全部 %.2f wt%%）；缺口：%s\n",
            cov$coverage * 100, cov$measured_wt_pct, cov$full_wt_pct,
            paste(sprintf("%s %.2f%%", names(cov$missing_wt_pct),
                          cov$missing_wt_pct), collapse = "、")))

cat("\n===================================================================\n")
if (fails == 0L) cat("  结果：全部通过\n") else cat(sprintf("  结果：%d 项失败\n", fails))
cat("===================================================================\n\n")
if (fails > 0L) quit(status = 1L)
