# =============================================================================
#  TERMITE formula —— 无内标「矿物化学式归一化」校准（AYCF 家族）
#  ---------------------------------------------------------------------------
#  背景：传统 TERMITE（termite_run）用「内标元素」归一化剥蚀产率，需要先用
#  EPMA 测出内标含量 C_IS。文献提出另一种校准策略：把测得的各元素按「矿物
#  结构式」归一化，用矿物化学式（a.p.f.u. / 电荷平衡）替代内标，省去 EPMA。
#
#    · Liu et al. (2008) Chem. Geol. 257:34-43 —— AYCF，无水矿物归一化到 100 wt%
#    · Zhang et al. (2022) JAAS 37:1793 —— 氟磷灰石，结构式补 P 与 F
#    · Zhang et al. (2023) JAAS 38:1387 —— 云母，结构式补 K 与 OH(F)
#
#  核心思想（一段话）：
#     cps → x灵敏度 lambda → 未归一化浓度 C' → 转摩尔 → 按矿物结构式归一化（电荷 /
#     位点 apfu）求「化学式因子」→ 测不准的元素（P/F/K/OH）用结构式约束理论补
#     → 按总分子量换算成 µg/g。全程不需要内标元素。
#
#  本模块是**独立的校准策略**，不修改 termite_run() 的数值路径；
#  默认仍走内标法，本模块由 termite_run_formula() 单独调用。
#
#  零外部依赖（纯 base R）。
# =============================================================================


# -----------------------------------------------------------------------------
# 0. 原子量常数（IUPAC）与默认价态表
# -----------------------------------------------------------------------------

.M_O  <- 15.999
.M_F  <- 18.9984
.M_H  <- 1.00784
.M_P  <- 30.973761998
.M_K  <- 39.0983

#' 元素标准原子量（IUPAC），与 .termite_valence_default 的元素集一一对应。
#'
#' 用途：`fixed` 模式里被固定扣除的阳离子（如绿柱石的 Be、电气石的 B/Si）
#' 通常不在样品通道里，传入的 M/valence 查不到它们；用这张表取原子量与价态。
.termite_M_default <- c(
  "Li" = 6.94,       "Be" = 9.0121831,  "B" = 10.81,       "Na" = 22.98976928,
  "Mg" = 24.305,     "Al" = 26.9815385, "Si" = 28.0855,    "P" = 30.973761998,
  "K" = 39.0983,     "Ca" = 40.078,     "Sc" = 44.955908,  "Ti" = 47.867,
  "V" = 50.9415,     "Cr" = 51.9961,    "Mn" = 54.938044,  "Fe" = 55.845,
  "Co" = 58.933194,  "Ni" = 58.6934,    "Cu" = 63.546,     "Zn" = 65.38,
  "Ga" = 69.723,     "Ge" = 72.630,     "As" = 74.921595,  "Se" = 78.971,
  "Rb" = 85.4678,    "Sr" = 87.62,      "Y" = 88.90584,    "Zr" = 91.224,
  "Nb" = 92.90637,   "Mo" = 95.95,      "In" = 114.818,    "Sn" = 118.710,
  "Sb" = 121.760,    "Cs" = 132.90545196, "Ba" = 137.327,
  "La" = 138.90547,  "Ce" = 140.116,    "Pr" = 140.90766,  "Nd" = 144.242,
  "Sm" = 150.36,     "Eu" = 151.964,    "Gd" = 157.25,     "Tb" = 158.92535,
  "Dy" = 162.500,    "Ho" = 164.93033,  "Er" = 167.259,    "Tm" = 168.93422,
  "Yb" = 173.045,    "Lu" = 174.9668,   "Hf" = 178.49,     "Ta" = 180.94788,
  "W" = 183.84,      "Re" = 186.207,    "Pb" = 207.2,      "Th" = 232.0377,
  "U" = 238.02891
)

#' 默认价态表：元素 → 价态（决定氧化物形式）
#'
#' 这些是地学常用氧化物价态（Fe→FeO、Mn→MnO 用二价；W→WO3、Mo→MoO3 用六价）。
#' 个别矿物可覆盖（见 mineral_formulas.csv 的 valence 列），
#' 价态直接决定「电荷归一化」里的电荷贡献，也等价于「元素→氧化物」换算
#' （l_i = 1 + (z_i/2)·M_O/M_i）。
.termite_valence_default <- c(
  "Li" = 1, "Be" = 2, "B" = 3, "Na" = 1, "Mg" = 2, "Al" = 3, "Si" = 4, "P" = 5,
  "K" = 1, "Ca" = 2, "Sc" = 3, "Ti" = 4, "V" = 3, "Cr" = 3, "Mn" = 2, "Fe" = 2,
  "Co" = 2, "Ni" = 2, "Cu" = 2, "Zn" = 2, "Ga" = 3, "Ge" = 4, "As" = 5, "Se" = 4,
  "Rb" = 1, "Sr" = 2, "Y" = 3, "Zr" = 4, "Nb" = 5, "Mo" = 6, "In" = 3, "Sn" = 4,
  "Sb" = 5, "Cs" = 1, "Ba" = 2,
  "La" = 3, "Ce" = 3, "Pr" = 3, "Nd" = 3, "Sm" = 3, "Eu" = 3, "Gd" = 3,
  "Tb" = 3, "Dy" = 3, "Ho" = 3, "Er" = 3, "Tm" = 3, "Yb" = 3, "Lu" = 3,
  "Hf" = 4, "Ta" = 5, "W" = 6, "Re" = 7, "Pb" = 2, "Th" = 4, "U" = 4
)

#' 矿物结构式参数（内置，mineral_formulas.csv 可覆盖/扩展）
#'
#' 每行一个矿物：mode 决定归算走哪条路径，n_O/n_F/n_OH 决定阴离子框架，
#' n_cat 是「阳离子位点总数」（stoich 模式用，如尖晶石 AB2O4 → 3），
#' excluded 是 ICP-MS 测不准、需要按结构式理论补的元素。
.termite_mineral_builtin <- function() {
  data.frame(
    name     = c("scheelite", "cassiterite", "zircon", "zircon_full",
                 "fluorapatite", "muscovite", "biotite", "beryl",
                 "tourmaline", "magnetite", "chromite"),
    formula  = c("CaWO4", "SnO2", "ZrSiO4", "ZrSiO4", "Ca5(PO4)3F",
                 "KAl2(AlSi3)O10(OH,F)2", "K(Mg,Fe)3(AlSi3)O10(OH,F)2",
                 "Be3Al2Si6O18", "Na(Mg,Fe)3Al6Si6O18(BO3)3(OH)4",
                 "Fe3O4", "FeCr2O4"),
    mode     = c("anhydrous", "anhydrous", "fixed", "anhydrous", "apatite",
                 "mica", "mica", "fixed", "fixed", "stoich", "stoich"),
    n_O      = c(4, 2, 4, 4, 12, 10, 10, 18, 31, 4, 4),
    n_F      = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
    n_OH     = c(0, 0, 0, 0, 0, 2, 2, 0, 4, 0, 0),
    n_cat    = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3),
    fixed    = c("", "", "Si:1", "", "", "", "", "Be:3", "B:3;Si:6", "", ""),
    excluded = c("", "", "Si", "", "P", "K", "K", "", "", "", ""),
    note     = c("白钨矿", "锡石", "锆石（Si 按结构式固定，推荐）",
                 "锆石（全元素电荷归一化，需可靠 Si 通道）", "氟磷灰石",
                 "白云母", "黑云母", "绿柱石", "电气石", "磁铁矿", "铬铁矿"),
    stringsAsFactors = FALSE
  )
}

#' 读取矿物结构式参数：优先 mineral_formulas.csv，否则用内置表
termite_mineral_defs <- function(cfg = NULL, path = "TERMITEScriptFolder/mineral_formulas.csv") {
  p <- if (is.null(cfg)) NA_character_ else termite_resource(path, cfg)
  if (!is.na(p) && file.exists(p)) {
    d <- utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE,
                         comment.char = "#", strip.white = TRUE,
                         fileEncoding = "UTF-8")
    return(d)
  }
  .termite_mineral_builtin()
}

#' 取一个矿物的参数（不存在则报错，列出可用矿物）
termite_mineral_param <- function(mineral, defs = NULL) {
  if (is.null(defs)) defs <- .termite_mineral_builtin()
  i <- match(tolower(mineral), tolower(defs$name))
  if (is.na(i))
    stop("未知矿物 '", mineral, "'。可用：", paste(defs$name, collapse = ", "))
  defs[i, ]
}


# -----------------------------------------------------------------------------
# 1. 灵敏度 lambda：多参考物质回归（lambda_i = sum C_i^rm / sum cps_i^rm）
# -----------------------------------------------------------------------------

#' 计算各元素的灵敏度 lambda（µg/g per cps）
#'
#' @param cps_list  list，每个元素是一个参考物质文件的净计数率矩阵（列=同位素）
#'                  更常用的调用方式见 termite_run_formula()，这里接受
#'                  list(每文件一个命名向量 cps) 与 conc(每文件一个命名向量 C)
#' @param conc_list list，与 cps_list 同构，参考物质推荐值（元素浓度 µg/g）
#' @return 命名向量 lambda（同位素名）
termite_sensitivity <- function(cps_list, conc_list) {
  iso <- names(cps_list[[1]])
  lam <- numeric(length(iso)); names(lam) <- iso
  for (j in iso) {
    cs <- vapply(cps_list, function(v) as.numeric(v[[j]]), 1.0)
    cc <- vapply(conc_list, function(v) as.numeric(v[[j]]), 1.0)
    ok <- is.finite(cs) & is.finite(cc) & cs > 0
    lam[j] <- if (any(ok)) sum(cc[ok]) / sum(cs[ok]) else NA_real_
  }
  lam
}


# -----------------------------------------------------------------------------
# 2. 归算核心：三种矿物模式
# -----------------------------------------------------------------------------

#' 参考物质「质量覆盖率」诊断
#'
#' 回答一个问题：**当前这套通道，能看到该参考物质总质量的百分之多少？**
#'
#' 为什么需要它：硅酸盐玻璃标样（NIST 610/612）的主量元素是 Si、Na、Ca、Al，
#' 但锆石/白钨矿这类「微量元素方法」通常**只接稀土与高场强元素，不接 Na、Al**。
#' 实测下来 NIST610 在这类通道下只能看到约 83 wt% 的质量，
#' 缺的正是 Na（9.94 wt%）与 Al（1.03 wt%），折算 Na₂O + Al₂O₃ ≈ 15.4 wt%。
#' 于是**标样不能当作「可归一到 100%」的物质来用**：
#' 若强行把测到的元素归一化到 100%，所有元素会被系统性抬高 1/0.83 ≈ 1.2 倍。
#'
#' 本模块的 lambda 是**逐元素独立回归**的（λ_j = Σ C_j / Σ cps_j），
#' 不做任何总量求和，所以覆盖率低**不影响**归算结果；
#' 这个诊断只是提醒使用者：别拿标样做「总量归一 / 100% 检验」。
#'
#' 口径：元素 → 氧化物折算 `ox = C · (1 + (z/2)·M_O/M_i)`，把没测到的氧补回来；
#' 挥发性/阴离子元素（H、C、N、O、F、Cl、S、Br、I）不计入求和。
#'
#' @param std_tab 参考物质推荐值表（termite_read_standards 的返回值）
#' @param rm      行名，如 "NIST610"
#' @param elements 当前实测通道覆盖的元素名
#' @return list(rm, full_wt_pct, measured_wt_pct, coverage, missing_wt_pct)；
#'          找不到该参考物质时返回 NULL
termite_ref_coverage <- function(std_tab, rm, elements,
                                 M = NULL, valence = NULL) {
  if (is.null(rm) || !rm %in% rownames(std_tab)) return(NULL)
  M <- if (is.null(M)) .termite_M_default else M
  valence <- if (is.null(valence)) .termite_valence_default else valence

  v <- suppressWarnings(as.numeric(std_tab[rm, ]))
  names(v) <- element_of(names(std_tab))
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) return(NULL)
  e_all <- unique(names(v))
  by_el <- vapply(e_all, function(e) v[names(v) == e][1], 1.0)

  # 玻璃里的挥发性/阴离子组分不计入「氧化物总量」
  skip <- c("H", "C", "N", "O", "F", "Cl", "S", "Br", "I")
  ox <- function(e) {
    if (e %in% skip) return(0)
    aw <- unname(M[e]); z <- unname(valence[e])
    if (length(aw) != 1 || length(z) != 1 || is.na(aw) || is.na(z)) return(0)
    by_el[[e]] * (1 + (z / 2) * .M_O / aw)
  }
  o_all  <- vapply(e_all, ox, 0)
  full   <- sum(o_all)
  meas   <- sum(o_all[e_all %in% elements])
  miss   <- sort(by_el[!(e_all %in% elements)] / 1e4, decreasing = TRUE)
  miss   <- miss[miss >= 0.01]

  list(rm = rm,
       full_wt_pct     = full / 1e4,
       measured_wt_pct = meas / 1e4,
       coverage        = if (full > 0) meas / full else NA_real_,
       missing_wt_pct  = if (length(miss)) head(miss, 6L) else numeric(0))
}


#' 通用「固定阳离子扣除」归一化（fixed 模式）
#'
#' 无水矿物电荷归一化的推广：结构式里有几个「配位数固定、但 ICP-MS 测不准」的
#' 阳离子时，把它们固定成化学式给出的 a.p.f.u.，从阴离子电荷里扣除，其余可测
#' 阳离子再按电荷归一化。等价地，也就是从「氧化物归一化到 100 wt%」的总和里，
#' 先按结构式扣掉那些测不准的组分，再归一化剩下测得到的。
#'
#'   · 绿柱石 Be3Al2Si6O18：fixed = c(Be = 3)，O = 18 → 可测阳离子电荷目标 30
#'   · 电气石 XY3Z6(T6O18)(BO3)3(OH,F)4：fixed = c(B = 3, Si = 6)，O = 31，
#'     n_OH = 4（V3W 位全按 OH 计，与 mica 的「F≈OH」近似一致）
#'     → 可测阳离子电荷目标 = 2·31 − 4 − (3×3 + 6×4) = 25
#'
#' @param fixed  命名向量，元素 → 固定 a.p.f.u.（如 c(Be = 3)）；空则退化为无水矿物
#' @param n_OH   OH 数（每个 OH 比 O2- 少 1 个负电荷，阴离子电荷 = 2·n_O − n_OH）
#' @param n_F    F 数（原子量近似按 OH 计，只影响总质量）
termite_formula_fixed <- function(cps, lambda, M, valence, n_O,
                                  fixed = numeric(0), n_OH = 0, n_F = 0) {
  keep <- setdiff(names(cps), names(fixed))          # 固定阳离子不参与电荷平衡
  Cprime <- cps[keep] * lambda[keep]
  n  <- Cprime / M[keep]
  z  <- valence[keep]

  # 固定阳离子的价态与原子量：优先用传入表，缺省回退内置默认表（Be/B/Si 常不在通道里）
  zf <- .termite_valence_default[names(fixed)]
  Mf <- .termite_M_default[names(fixed)]
  hv <- names(fixed)[names(fixed) %in% names(valence)]
  if (length(hv)) zf[hv] <- valence[hv]
  hm <- names(fixed)[names(fixed) %in% names(M)]
  if (length(hm)) Mf[hm] <- M[hm]

  anion_charge <- 2 * n_O - n_OH                       # 阴离子总负电荷（OH 少 1）
  fixed_charge <- sum(zf * fixed, na.rm = TRUE)        # 固定阳离子贡献的正电荷
  charge_target <- anion_charge - fixed_charge         # 可测阳离子需平衡的电荷
  Ff <- charge_target / sum(n * z, na.rm = TRUE)       # 化学式因子
  apfu <- n * Ff

  M_cat   <- sum(apfu * M[keep], na.rm = TRUE)
  M_fixed <- sum(Mf * fixed, na.rm = TRUE)
  M_total <- M_cat + M_fixed + n_O * .M_O + n_OH * .M_H + n_F * .M_F

  conc        <- apfu * M[keep] / M_total * 1e6
  conc_fixed  <- Mf * fixed / M_total * 1e6            # 固定阳离子的理论含量（供展示/校核）

  list(conc_ug_g = c(conc, conc_fixed),
       apfu = c(apfu, fixed),
       formula_factor = Ff, charge_target = charge_target,
       anion_charge = anion_charge, fixed_charge = fixed_charge,
       M_total = M_total, fixed = fixed)
}

#' 通用「阳离子位点总数固定」归一化（stoich 模式）
#'
#' 有些矿物的**阳离子位点总数**由结构决定、且不随类质同象替代变化，
#' 而各元素的价态/氧化态反而不确定——此时用「位点计数」比「电荷平衡」更稳：
#'
#'   · 尖晶石族 AB2O4（磁铁矿 Fe3O4、铬铁矿 FeCr2O4、尖晶石 MgAl2O4）：
#'     阳离子总数恒 = 3（四面体 1 + 八面体 2），O = 4。
#'     Ti/Mg/Al/Cr/Mn/Zn/V/Ni/Ga 替代 Fe 时总数不变；
#'     而 Fe²⁺/Fe³⁺ 比会在磁铁矿—钛铁晶石固溶体里变，用电荷归一化就要先假设价态，
#'     用位点计数则完全绕开价态问题。
#'
#' 数学上：F_f = n_cat / Σ n_i（未归一化摩尔数之和），与电荷无关。
#'
#' @param n_cat  阳离子位点总数（a.p.f.u.），如尖晶石 = 3
#' @param fixed  可选，被固定扣除的元素（这些位点从 n_cat 里先扣掉）
termite_formula_stoich <- function(cps, lambda, M, valence, n_cat,
                                   n_O, fixed = numeric(0), n_OH = 0, n_F = 0) {
  keep <- setdiff(names(cps), names(fixed))
  Cprime <- cps[keep] * lambda[keep]
  n  <- Cprime / M[keep]

  Mf <- .termite_M_default[names(fixed)]
  hm <- names(fixed)[names(fixed) %in% names(M)]
  if (length(hm)) Mf[hm] <- M[hm]

  site_target <- n_cat - sum(fixed, na.rm = TRUE)   # 留给可测元素的位点数
  Ff <- site_target / sum(n, na.rm = TRUE)          # 化学式因子
  apfu <- n * Ff

  M_cat   <- sum(apfu * M[keep], na.rm = TRUE)
  M_fixed <- sum(Mf * fixed, na.rm = TRUE)
  M_total <- M_cat + M_fixed + n_O * .M_O + n_OH * .M_H + n_F * .M_F

  conc       <- apfu * M[keep] / M_total * 1e6
  conc_fixed <- Mf * fixed / M_total * 1e6

  list(conc_ug_g = c(conc, conc_fixed),
       apfu = c(apfu, fixed),
       formula_factor = Ff, site_target = site_target, n_cat = n_cat,
       M_total = M_total, fixed = fixed)
}

#' 无水矿物：电荷归一化（等价于「元素氧化物归一化到 100 wt%」的 AYCF）
#'
#' 是 fixed 模式在「无固定阳离子、无 OH」时的特例。
#' 白钨矿 CaWO4：n_O=4 → 电荷目标 8；锡石 SnO2：n_O=2 → 4；锆石 ZrSiO4：n_O=4 → 8。
termite_formula_anhydrous <- function(cps, lambda, M, valence, n_O)
  termite_formula_fixed(cps, lambda, M, valence, n_O = n_O)

#' 解析 CSV 里的 fixed 列："Be:3" → c(Be=3)；"B:3;Si:6" → c(B=3, Si=6)；空 → numeric(0)
.parse_fixed <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s) || !nzchar(trimws(s)))
    return(numeric(0))
  parts <- strsplit(trimws(s), ";", fixed = TRUE)[[1L]]
  out <- numeric(0)
  for (p in parts) {
    p <- trimws(p)
    if (!nzchar(p)) next
    kv <- strsplit(p, ":", fixed = TRUE)[[1L]]
    out[trimws(kv[1L])] <- as.numeric(trimws(kv[2L]))
  }
  out
}

#' 氟磷灰石 Ca5(PO4)3F：结构式补 P 与 F
#'
#' P 因高电离能、F 因不电离，ICP-MS 测不准。用结构式约束理论补：
#'   · Ca 位（除 P 位外的阳离子）apfu 和 = 5
#'   · P 位（P + Si + As）apfu 和 = 3  →  P = 3 - Si - As
#'   · F = 1，O = 12
termite_formula_apatite <- function(cps, lambda, M, valence) {
  keep <- setdiff(names(cps), "P")                   # P 测不准，剔除；F 不测
  Cprime <- cps[keep] * lambda[keep]
  n  <- Cprime / M[keep]
  p_site <- intersect(c("Si", "As"), keep)           # P 位里测得到的元素
  ca_site <- setdiff(keep, p_site)                   # Ca 位（其余阳离子）
  Ff <- 5 / sum(n[ca_site], na.rm = TRUE)            # Ca 位 apfu 和 = 5
  apfu <- n * Ff
  Fn_Si <- if ("Si" %in% names(apfu)) apfu[["Si"]] else 0
  Fn_As <- if ("As" %in% names(apfu)) apfu[["As"]] else 0
  Fn_P  <- 3 - Fn_Si - Fn_As                         # 结构式约束
  Fn_F  <- 1
  n_O   <- 12
  M_cat  <- sum(apfu * M[keep], na.rm = TRUE)
  M_total <- M_cat + Fn_P * .M_P + Fn_F * .M_F + n_O * .M_O
  conc    <- apfu * M[keep] / M_total * 1e6
  conc_P  <- Fn_P * .M_P / M_total * 1e6
  list(conc_ug_g = c(conc, P = conc_P),
       apfu = c(apfu, P = Fn_P, F = Fn_F),
       formula_factor = Ff, Fn_P = Fn_P, Fn_F = Fn_F, M_total = M_total)
}

#' 云母 X Y2-3 Z4 O10 (OH,F)2：结构式补 K 与 OH（AYCF2，Zhang 2023）
#'
#' K 在层间位，LA 测不准（层间位分馏 + 低 K 参考物质），用 X 位约束理论补：
#'   · Y+Z 位（非层间阳离子）电荷归一化到 21（总 22，X 位整体按 +1 计）
#'   · X 位（Na + K + Rb + Cs + Ca + Ba + Sr）apfu 和 = 1 → K = 1 - 其余
#'   · 挥发性组分全按 OH 计：OH = 2，O = 10（F 与 OH 原子量相近，可近似）
termite_formula_mica <- function(cps, lambda, M, valence) {
  keep  <- setdiff(names(cps), c("K", "F", "Cl"))    # K 测不准；F/Cl 不测
  Xsite <- intersect(c("Na", "Rb", "Cs", "Ca", "Ba", "Sr"), keep)
  YZsite <- setdiff(keep, Xsite)
  Cprime <- cps[keep] * lambda[keep]
  n  <- Cprime / M[keep]
  z  <- valence[keep]
  Ff <- 21 / sum(n[YZsite] * z[YZsite], na.rm = TRUE)   # Y+Z 位电荷归一化
  apfu <- n * Ff
  Fn_X  <- sum(apfu[Xsite], na.rm = TRUE)               # 测得的层间阳离子 apfu
  Fn_K  <- 1 - Fn_X                                     # X 位 apfu 和 = 1
  Fn_OH <- 2
  n_O   <- 10
  M_cat  <- sum(apfu * M[keep], na.rm = TRUE)
  M_total <- M_cat + Fn_K * .M_K + Fn_OH * (.M_O + .M_H) + n_O * .M_O
  conc   <- apfu * M[keep] / M_total * 1e6
  conc_K <- Fn_K * .M_K / M_total * 1e6
  list(conc_ug_g = c(conc, K = conc_K),
       apfu = c(apfu, K = Fn_K, OH = Fn_OH),
       formula_factor = Ff, Fn_K = Fn_K, Fn_OH = Fn_OH, M_total = M_total)
}


# -----------------------------------------------------------------------------
# 3. 顶层入口：跑一遍无内标校准
# -----------------------------------------------------------------------------

#' 无内标矿物化学式归一化校准
#'
#' @param cfg       termite_defaults() 的参数列表（数据目录、积分窗口等）
#' @param mineral   矿物名（见 termite_mineral_defs()，如 scheelite / fluorapatite / muscovite）
#' @param valence   可选，元素→价态覆盖（命名向量）；缺省用内置默认价态表
#' @return list(isotopes, elements, lambda, samples=data.frame(每样品浓度),
#'              apfu, formula_factor, mineral)
termite_run_formula <- function(cfg, mineral, valence = NULL) {
  stopifnot(is.list(cfg))
  mode <- cfg$mode
  if (!identical(mode, "spot"))
    stop("无内标校准目前只支持点分析（mode = 'spot'）。")

  # ---- 读数据库与文件清单（复用核心层）----
  f_iso <- termite_resource(cfg$file_isotopes, cfg)
  f_std <- termite_resource(cfg$file_standards, cfg)
  if (!file.exists(f_iso)) stop("找不到同位素原子量/丰度表：", f_iso)
  if (!file.exists(f_std)) stop("找不到参考物质推荐值表：", f_std)
  iso_tab <- termite_read_isotopes(f_iso)
  std_tab <- termite_read_standards(f_std)

  plan <- termite_plan(cfg)
  if (!nrow(plan)) stop("没有找到任何原始数据文件。")
  sample_files <- plan$path[plan$kind == "sample"]
  ref_files    <- plan$path[plan$kind == "ref"]
  ref_rm       <- plan$material[plan$kind == "ref"]
  if (!length(sample_files)) stop("没有识别出任何样品文件。")
  if (!length(ref_files))    stop("没有识别出任何定标参考物质。")

  # ---- 表头 / 元素 / 原子量 ----
  hdr_src <- if (length(sample_files)) sample_files[1] else ref_files[1]
  isotopes <- termite_read_header(hdr_src, cfg)
  if (!is.null(cfg$resolution) && nzchar(cfg$resolution))
    isotopes <- trimws(gsub(cfg$resolution, "", isotopes, fixed = TRUE))
  isotopes_db <- termite_normalize_isotope(isotopes)
  elements <- element_of(isotopes)

  # 同位素 → 参考表列名（复用别名 + 同元素借用）
  iso_alias <- termite_read_alias(termite_resource(cfg$file_iso_alias, cfg))
  db_key <- termite_apply_alias(isotopes_db, iso_alias)
  for (k in which(!db_key %in% names(iso_tab))) {
    same <- names(iso_tab)[element_of(names(iso_tab)) == elements[k]]
    if (length(same)) db_key[k] <- same[1]
  }
  if (any(!db_key %in% names(iso_tab)))
    stop("同位素原子量/丰度表中缺少：",
         paste(unique(isotopes[!db_key %in% names(iso_tab)]), collapse = ", "))

  # 元素原子量（Atomic_weight 行按元素取，同一元素各同位素相同）
  Aw <- as.numeric(iso_tab["Atomic_weight", db_key])
  names(Aw) <- isotopes
  # 同位素丰度（用于给多通道元素挑主同位素）
  Ab <- as.numeric(iso_tab["isotope_abundance", db_key])
  names(Ab) <- isotopes

  # 每个元素挑「丰度最高的同位素」作为代表通道
  keep_iso <- logical(length(isotopes)); names(keep_iso) <- isotopes
  for (el in unique(elements)) {
    ii <- which(elements == el)
    keep_iso[ii[which.max(Ab[ii])]] <- TRUE
  }
  main_iso <- isotopes[keep_iso]
  main_el  <- elements[keep_iso]
  M_el <- Aw[main_iso]; names(M_el) <- main_el

  # 价态
  val <- .termite_valence_default
  if (!is.null(valence)) val[names(valence)] <- valence
  val_el <- val[main_el]; names(val_el) <- main_el

  # ---- 灵敏度 lambda：多参考物质回归 ----
  .net_cps <- function(path) {
    r  <- termite_read_raw(path, cfg, n_sweeps = cfg$n_sweeps_ref)
    m  <- r$values
    # auto_detect 模式下数据起始行来自探测结果，不能用 cfg$signal_line（未回填）
    sig_line <- if (isTRUE(cfg$auto_detect)) r$fmt$signal_line else cfg$signal_line
    s1 <- .to_matrix_row(cfg$first_signal, sig_line)
    s2 <- .to_matrix_row(cfg$last_signal,  sig_line)
    b1 <- .to_matrix_row(cfg$first_blank,  sig_line)
    b2 <- .to_matrix_row(cfg$last_blank,   sig_line)
    s2 <- min(s2, nrow(m)); s1 <- min(s1, s2)
    b2 <- min(b2, nrow(m)); b1 <- min(b1, b2)
    bl <- .blank_stat(m, b1, b2, cfg$background)
    net <- m[s1:s2, , drop = FALSE] -
           matrix(bl, nrow = s2 - s1 + 1L, ncol = ncol(m), byrow = TRUE)
    if (isTRUE(cfg$clip_negative)) net[!is.na(net) & net < 0] <- 0
    mu <- colMeans(net, na.rm = TRUE)
    names(mu) <- isotopes
    mu
  }
  ref_cps <- lapply(ref_files, .net_cps)
  ref_conc <- lapply(ref_rm, function(rm)
    as.numeric(std_tab[rm, db_key]))
  names(ref_conc) <- NULL
  for (k in seq_along(ref_conc)) names(ref_conc[[k]]) <- isotopes
  lambda <- termite_sensitivity(ref_cps, ref_conc)

  # ---- 参考物质质量覆盖率诊断 ----
  # 只作提示用：本模块的 lambda 逐元素回归，不依赖「总量归一到 100%」；
  # 覆盖率低说明标样有主量元素没测到（典型是玻璃里的 Na、Al），
  # 此时不能拿标样做总量检验，但归算结果本身不受影响。
  ref_coverage <- lapply(unique(ref_rm), function(rm)
    termite_ref_coverage(std_tab, rm, unique(main_el)))
  names(ref_coverage) <- unique(ref_rm)

  # ---- 矿物参数 + 归算 ----
  # 优先读 mineral_formulas.csv（用户可覆盖/扩展），否则用内置表
  mp <- termite_mineral_param(mineral, termite_mineral_defs(cfg))
  reducer <- switch(mp$mode,
                    anhydrous = termite_formula_anhydrous,
                    apatite   = termite_formula_apatite,
                    mica      = termite_formula_mica,
                    fixed     = termite_formula_fixed,
                    stoich    = termite_formula_stoich,
                    stop("不支持的矿物模式：", mp$mode))

  sample_cps <- lapply(sample_files, .net_cps)
  sid <- if (identical(cfg$layout, "flat"))
    plan$sample_name[plan$kind == "sample"] else basename(sample_files)

  # 灵敏度按「元素」重命名，与 cps_k / M_el / val_el 对齐（还原函数按名字取 lambda）
  lambda_el <- lambda[main_iso]
  names(lambda_el) <- main_el

  fx <- .parse_fixed(mp$fixed)
  out <- lapply(seq_along(sample_files), function(k) {
    cps_k <- sample_cps[[k]][main_iso]; names(cps_k) <- main_el
    # anhydrous 只需 n_O；fixed 还需固定阳离子与 OH/F；stoich 还需阳离子位点总数；
    # 磷灰石/云母用结构式常数
    nc <- if (is.null(mp$n_cat) || length(mp$n_cat) == 0L || is.na(mp$n_cat))
      0 else as.numeric(mp$n_cat)
    if (identical(mp$mode, "anhydrous"))
      reducer(cps_k, lambda_el, M_el, val_el, n_O = mp$n_O)
    else if (identical(mp$mode, "fixed"))
      reducer(cps_k, lambda_el, M_el, val_el,
              n_O = mp$n_O, fixed = fx,
              n_OH = as.numeric(mp$n_OH), n_F = as.numeric(mp$n_F))
    else if (identical(mp$mode, "stoich"))
      reducer(cps_k, lambda_el, M_el, val_el,
              n_cat = nc, n_O = mp$n_O, fixed = fx,
              n_OH = as.numeric(mp$n_OH), n_F = as.numeric(mp$n_F))
    else
      reducer(cps_k, lambda_el, M_el, val_el)
  })
  names(out) <- sid

  conc_mat <- do.call(rbind, lapply(out, function(r) r$conc_ug_g))
  rownames(conc_mat) <- sid

  list(mineral = mp, mode = "formula", isotopes = main_iso, elements = main_el,
       lambda = lambda[main_iso], samples = conc_mat,
       ref_coverage = ref_coverage,
       apfu = lapply(out, `[[`, "apfu"),
       formula_factor = vapply(out, `[[`, numeric(1), "formula_factor"),
       M_total = vapply(out, `[[`, numeric(1), "M_total"))
}

#' 无内标结果 → 数据框（供展示/导出）
termite_formula_table <- function(res) {
  d <- as.data.frame(res$samples, check.names = FALSE)
  data.frame(ID = rownames(res$samples), d, check.names = FALSE, row.names = NULL)
}
