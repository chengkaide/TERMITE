#!/usr/bin/env Rscript
# =============================================================================
#  QC 玻璃参考物质参考值查证 + 本批实测对照（BIR-1G / BCR-2G / BHVO-2G）
#  ---------------------------------------------------------------------------
#  ① 从 PANGAEA doi:10.1594/PANGAEA.919305 的原始 textfile 里**程序化**提取
#     GeoReM 推荐值（preferred / recommend values [GEOREM]），不手抄，
#     避免转录错误；列序由文件自己的表头行决定，不写死数字。
#  ② 与 USGS 2022 Information Sheet 的证书值交叉核对（嵌入，带出处）。
#  ③ 用查证后的参考值复核本批 TERMITE 链在三玻璃上的表现：
#     以 29Si 为内标（玻璃基体的 Si 证书值可靠、29Si 无显著干扰），
#     把内标含量换成各玻璃自己的化学计量 Si，逐元素算回收率。
#  ④ 量化三玻璃的「可见主量闭合」缺口 —— 本批没有 Na / Mg / Al / P / Cr / Mn
#     通道，玄武岩玻璃因此**天然**加和不到 100 wt%，与样品无关。
#
#  环境变量：
#    TERMITE_DIR  数据批次目录
#    OUT          输出目录（默认 <批次>/TERMITE_results）
#    PANGAEA_TXT  PANGAEA 原始 textfile（缺省则跳过 GeoReM，只用 USGS）
#    SIG1 SIG2    积分窗口（配置行号，默认 100 / 158）
#
#  用法：TERMITE_DIR=<批次目录> OUT=<输出目录> Rscript tools/qc_ref_check.R
# =============================================================================

options(encoding = "UTF-8")

# --- 本机 locale 守卫（Bash 注入 LC_ALL=C.UTF-8 会让 source() 吞掉中文） ----
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.args <- commandArgs(trailingOnly = FALSE)
scr <- sub("^--file=", "", .args[grepl("^--file=", .args)])
ROOT <- if (length(scr)) dirname(dirname(normalizePath(scr[1], mustWork = FALSE))) else getwd()
for (f in list.files(file.path(ROOT, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  source(f, encoding = "UTF-8")

DIR <- Sys.getenv("TERMITE_DIR", "")
if (!nzchar(DIR))
  stop("请先用环境变量 TERMITE_DIR 指定数据批次目录。")
OUT <- Sys.getenv("OUT", file.path(DIR, "TERMITE_results"))
PAN <- Sys.getenv("PANGAEA_TXT", "")
SIG1 <- as.integer(Sys.getenv("SIG1", "100"))
SIG2 <- as.integer(Sys.getenv("SIG2", "158"))

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

GLASS <- c("BIR-1G", "BCR-2G", "BHVO-2G")   # 本批三个 QC 玻璃
IS_SI <- 153200                             # 仅为记账常数（内标法里会自行抵消）

# 氧化物 / 元素摩尔质量
M_O <- 15.999
M <- list(Si = 28.0855, O = M_O, Ti = 47.867, Fe = 55.845, K = 39.0983, Ca = 40.078)


# =============================================================================
# 1. 参考值：PANGAEA 原始文件里的 GeoReM 推荐值
# =============================================================================
# PANGAEA 导出格式：`/* ... */` 描述段，随后一行表头（制表符分隔），再是数据行。
# 数据行前三列固定为 样品名 / n / Ref data；此后主量块 11 个氧化物（每个
# 占 2 列：值 + 标准差）；再一列 Analyt method；随后 39 个微量元素（同样每个
# 占 2 列）。列序由表头行自身给出，下面的索引偏置只是把「表头顺序」换算成
# 「1 基列号」，避免每换一次数据集就重新数一遍。
EL_TRACE <- c("Li", "B", "Sc", "V", "Cr", "Co", "Ni", "Cu", "Zn", "Ga", "Ge",
              "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Cs", "Ba", "La", "Ce", "Pr",
              "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu",
              "Hf", "Ta", "W", "Pb", "Th", "U")
OX_MAJ <- c("SiO2", "TiO2", "Al2O3", "Fe2O3", "MnO", "MgO", "CaO", "Na2O",
            "K2O", "P2O5", "Total")

#' 读 PANGAEA textfile，返回 list(物质名 = list(kind, major, trace))
parse_pangaea <- function(path) {
  ln  <- readLines(path, encoding = "UTF-8", warn = FALSE)
  i0  <- which(trimws(ln) == "*/")[1]
  if (is.na(i0)) stop("PANGAEA 文件里找不到 */ 结束行：", path)
  # 表头行紧跟在 */ 之后（可能中间有空行）
  i1  <- i0 + which(nzchar(trimws(ln[(i0 + 1L):length(ln)])))[1]
  sp  <- strsplit(ln[(i1 + 1L):length(ln)], "\t")
  sp  <- sp[vapply(sp, function(v) nzchar(trimws(v[1])), TRUE)]

  # 取第 i 列，空 / # 打头的（PANGAEA 用 #0.000 表示 n.d.）一律作 NA
  at <- function(v, i) {
    if (i > length(v)) return(NA_real_)
    x <- trimws(v[[i]])
    if (!nzchar(x) || substr(x, 1L, 1L) == "#") NA_real_
    else suppressWarnings(as.numeric(x))
  }
  out <- list()
  for (v in sp) {
    kind <- trimws(v[3])                                  # Ref data 列
    if (!grepl("GEOREM", kind, ignore.case = TRUE)) next
    key  <- trimws(v[1])
    maj  <- vapply(seq_along(OX_MAJ),
                   function(j) at(v, 5L + 2L * (j - 1L)), numeric(1))
    tr   <- vapply(seq_along(EL_TRACE),
                   function(j) at(v, 27L + 2L * (j - 1L)), numeric(1))
    names(maj) <- OX_MAJ; names(tr) <- EL_TRACE
    out[[key]] <- list(kind = kind, major = maj, trace = tr)
  }
  out
}

georem <- if (nzchar(PAN) && file.exists(PAN)) parse_pangaea(PAN) else list()
cat(sprintf("GeoReM（PANGAEA 解析）拿到 %d 个物质：%s\n",
            length(georem), paste(names(georem), collapse = ", ")))


# =============================================================================
# 2. 参考值：USGS 2022 Information Sheet（交叉核对用）
# =============================================================================
# 出处：USGS 信息表（BHVO-2g / BIR-1g 引 Jochum et al. 2005 GGR；
#       BCR-2g 引 Jochum et al. 2016 GGR 的 GeoReM 推荐值）。
# 主量单位 g/100 g，微量单位 mg/kg。BCR-2g 的主量取自检索片段（PDF 未直读），
# 只用于「可见主量闭合」量级判断，不参与逐元素回收率。
USGS <- list(
  "BHVO-2G" = list(
    major = c(SiO2 = 49.3, TiO2 = 2.78, Al2O3 = 13.6, FeOT = 11.3, MnO = 0.17,
              MgO = 7.13, CaO = 11.4, Na2O = 2.40, K2O = 0.51, P2O5 = 0.29),
    trace = c(Ba = 131.2, Ce = 37.67, Co = 43.0, Cs = 0.102, Cu = 120,
              Dy = 5.281, Er = 2.566, Eu = 2.077, Ga = 20.3, Gd = 6.125,
              Hf = 4.22, Ho = 0.983, K = 4270, La = 15.25, Lu = 0.2766,
              Mo = 3.78, Nb = 18.2, Nd = 24.55, Ni = 112, Pb = 1.76,
              Pr = 5.28, Rb = 9.183, Sm = 6.119, Sr = 396.0, Ta = 1.08,
              Tm = 0.328, Tb = 0.941, Th = 1.14, U = 0.403, Y = 29.2,
              Yb = 1.995, Zr = 172)),
  "BIR-1G" = list(
    major = c(SiO2 = 47.3, TiO2 = 0.98, Al2O3 = 15.6, FeOT = 10.5, MnO = 0.18,
              MgO = 9.37, CaO = 13.4, Na2O = 1.89, K2O = 0.03, P2O5 = 0.028),
    trace = c(Ba = 6.496, Ce = 1.865, Co = 52.5, Cu = 121, Dy = 2.531,
              Er = 1.686, Eu = 0.514, Ga = 14.0, Gd = 1.856, Hf = 0.620,
              Ho = 0.595, K = 183.2, La = 0.607, Lu = 0.241, Nb = 0.535,
              Nd = 2.348, Ni = 179, Pb = 3.13, Pr = 0.376, Rb = 0.192,
              Sm = 1.079, Sr = 107.5, Ta = 0.038, Tm = 0.249, Tb = 0.414,
              Th = 0.030, U = 0.028, Y = 16.0, Yb = 1.621, Zr = 17.8)),
  "BCR-2G" = list(
    major = c(SiO2 = 54.1, TiO2 = 2.26, Al2O3 = 13.5, Fe2O3T = 13.8, MnO = 0.20,
              MgO = 3.59, CaO = 7.12, Na2O = 3.16, K2O = 1.79, P2O5 = 0.35),
    trace = c(Ba = 683.0, Ce = 53.30, Co = 35.5, Cs = 1.18, Dy = 6.437,
              Er = 3.697, Eu = 1.965, Ga = 21.2, Gd = 6.712, Hf = 4.71,
              Ho = 1.28, K = 14900, La = 24.73, Lu = 0.503, Mo = 238,
              Nb = 12.3, Nd = 28.87, Ni = 10.9, Pb = 10.9, Pr = 6.77,
              Rb = 47.02, Sm = 6.588, Sr = 342.0, Ta = 0.744, Tb = 1.10,
              Th = 5.58, U = 1.63, Y = 36.1, Yb = 3.391, Zr = 188)))


# =============================================================================
# 3. 跑一遍 TERMITE，拿三玻璃的实测浓度
# =============================================================================
# 用 29Si 内标、ref = SRM 612、QC 清单留空 —— 于是 12 个 QC 文件连同 30 个
# 锆石一起被当成样品归算，我们只挑出三玻璃的行来核对。
# ★ 内标法里 C_IS 只是个记账常数：RSF 由 612 推出时已经吸收掉它，
#   所以样品结果与 C_IS 取值无关（下面会打印 Si 列来验证这一点）。
base_cfg <- function() {
  cfg <- termite_defaults("spot")
  cfg$dir         <- DIR
  cfg$app_dir     <- ROOT
  cfg$layout      <- "flat"
  cfg$auto_detect <- TRUE
  cfg$ref_names   <- c("SRM 612")
  cfg$qc_names    <- character(0)
  cfg$first_blank <- 25L;  cfg$last_blank  <- 60L
  cfg$first_signal <- SIG1; cfg$last_signal <- SIG2
  cfg
}

probe <- termite_probe(termite_plan(base_cfg())$path[1])
j_si  <- match("29Si", probe$isotopes_raw)
if (is.na(j_si)) stop("表头里找不到 29Si")

cfg <- base_cfg()
cfg$column_IS <- j_si + 1L          # column_IS 是文件列号（含 Time 列）
cfg$is_conc   <- IS_SI
cat(sprintf("归算中：内标 29Si = 第 %d 列，C_IS = %g（记账常数）…\n",
            cfg$column_IS, IS_SI))
res <- termite_run(cfg)

# --- 每个元素取「丰度最高的同位素」当代表通道（与公式法同一规则） ----------
# 丰度表是按「元素在前、质量数在后」命名的（Si29），表头是反过来的（29Si）；
# res$db_key 就是 core 已经做好的「表头 → 丰度表列名」映射（含别名与借位），
# 直接拿它查表，别自己再拼一遍，免得漏掉别名那几条。
iso_tab <- termite_read_isotopes(termite_resource(cfg$file_isotopes, cfg))
Ab  <- suppressWarnings(as.numeric(iso_tab["isotope_abundance", ]))
names(Ab) <- names(iso_tab)
abv <- Ab[res$db_key]
names(abv) <- res$isotopes
# 借位也没接上的（该元素在丰度表里一个同位素都没有）→ 退化成第一个通道
for (b in which(is.na(abv))) {
  cand <- grep(paste0("^", res$elements[b], "[0-9]+$"), names(Ab), value = TRUE)
  if (length(cand)) abv[b] <- max(Ab[cand], na.rm = TRUE)
}
grp <- split(seq_along(res$isotopes), factor(res$elements, levels = unique(res$elements)))
pick <- vapply(grp, function(ii) {
  ii[if (any(!is.na(abv[ii]))) which.max(replace(abv[ii], is.na(abv[ii]), -Inf)) else 1L]
}, 1L)
pick <- sort(pick)
main_iso <- res$isotopes[pick]; main_el <- res$elements[pick]

conc <- res$sample$conc[, main_iso, drop = FALSE]
dimnames(conc) <- list(res$files$sample_name, main_el)

# 自检：内标法把内标通道钳在 C_IS 上，三玻璃的 Si 应恰好 = IS_SI
cat(sprintf("自检：Si 列取值 %s（应恒为 %g）\n",
            paste(unique(format(conc[, "Si"], digits = 7)), collapse = " / "), IS_SI))


# =============================================================================
# 4. 参考值对照表（GeoReM vs USGS）
# =============================================================================
els <- unique(c(EL_TRACE, "K", "Na", "Mg", "Al", "P", "Cr", "Mn"))
# 两套参考值覆盖的元素不一样（USGS 不列 Sc/V/Ge/W，GeoReM 不列 K），
# 取不到就回 NA，不能直接 [[ ]] —— 那会越界报错。
pick1 <- function(v, e) if (!is.null(v) && e %in% names(v)) unname(v[[e]]) else NA_real_
ref_rows <- list()
for (g in GLASS) {
  gv <- if (!is.null(georem[[g]])) georem[[g]]$trace else NULL
  uv <- USGS[[g]]$trace
  for (e in els) {
    a <- pick1(gv, e)
    b <- pick1(uv, e)
    ref_rows[[length(ref_rows) + 1L]] <- data.frame(
      玻璃 = g, 元素 = e, GeoReM = a, USGS = b,
      `相对差pct` = if (!is.na(a) && !is.na(b) && b != 0) (a - b) / b * 100 else NA_real_,
      `本批有通道` = ifelse(e %in% main_el, "有", "无"), check.names = FALSE)
  }
}
ref_tab <- do.call(rbind, ref_rows)
utils::write.csv(ref_tab, file.path(OUT, "QC参考值_GeoReM对USGS.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cat("\n=== 两套参考值相对差 > 5% 的条目（查证重点）===\n")
d <- ref_tab[!is.na(ref_tab[["相对差pct"]]) & abs(ref_tab[["相对差pct"]]) > 5, ]
if (nrow(d)) print(d[, c("玻璃", "元素", "GeoReM", "USGS", "相对差pct")],
                   row.names = FALSE) else cat("（无）\n")


# =============================================================================
# 5. 三玻璃回收率（Si 锚定内标法）
# =============================================================================
#' 玻璃参考 SiO2 (wt%) → Si (µg/g)
si_ug_g <- function(sio2) sio2 * (M$Si / (M$Si + 2 * M_O)) * 1e4

#' 参考表：元素 → 参考浓度（优先 GeoReM，缺则退到 USGS，两边都缺就跳过）
#' ★ 别写成 out[names(v)] <- v[!is.na(v)] —— 有 NA 时左右长度不等，
#'   R 会把值按位置错位填进去，整串参考值跟着串行（BIR-1G/BHVO-2G 的 B、W 是 n.d.）。
ref_of <- function(g) {
  gv <- if (!is.null(georem[[g]])) georem[[g]]$trace else NULL
  uv <- USGS[[g]]$trace
  out <- c()
  for (e in union(names(uv), names(gv))) {
    a <- pick1(gv, e); b <- pick1(uv, e)
    v <- if (!is.na(a)) a else b
    if (!is.na(v)) out[[e]] <- v
  }
  out
}

rec_long <- list(); rec_sum <- list(); clo <- list()
for (g in GLASS) {
  idx <- which(res$files$sample_name == g)
  if (!length(idx)) { warning("本批没有 ", g); next }
  sio2 <- USGS[[g]]$major[["SiO2"]]
  f    <- si_ug_g(sio2) / IS_SI          # 把钳在 153200 的 Si 换回该玻璃自己的 Si
  ref  <- ref_of(g)

  for (k in idx) {
    file <- res$files$sample[[k]]
    for (e in names(ref)) {
      if (!e %in% main_el || is.na(ref[[e]])) next
      meas <- conc[k, e] * f
      # 空白扣除后 ≤0 的通道（BIR-1G 的 Ta/Th/U/Mo 只有 0.03~0.07 µg/g，
      # 远低于本方法检出限）不要报成"回收率 0%"，那是个假象，直接略过。
      if (is.na(meas) || meas <= 0) next
      rec_long[[length(rec_long) + 1L]] <- data.frame(
        玻璃 = g, 文件 = file, 元素 = e, 实测 = meas, 参考 = ref[[e]],
        回收率pct = meas / ref[[e]] * 100, check.names = FALSE)
    }
  }
  # 该玻璃 4 个测点的均值 / RSD / 回收率
  sub <- do.call(rbind, rec_long[vapply(rec_long, function(z) z$玻璃[1] == g, TRUE)])
  for (e in unique(sub$元素)) {
    v <- sub$实测[sub$元素 == e]
    rec_sum[[length(rec_sum) + 1L]] <- data.frame(
      玻璃 = g, 元素 = e, n = length(v), 参考 = sub$参考[sub$元素 == e][1],
      实测均值 = mean(v), RSDpct = sd(v) / mean(v) * 100, 回收率pct = mean(v) / sub$参考[sub$元素 == e][1] * 100,
      check.names = FALSE)
  }
  # 可见主量闭合：本批只有 Si / Ti / Fe / Ca / K 这 5 个主量相关通道
  uj <- USGS[[g]]$major
  fe2o3t <- if ("Fe2O3T" %in% names(uj)) uj[["Fe2O3T"]] else uj[["FeOT"]] * 1.1113
  vis_ref <- uj[["SiO2"]] + uj[["TiO2"]] + fe2o3t + uj[["CaO"]] + uj[["K2O"]]
  vis_meas <- mean(conc[idx, "Si"] * f * (M$Si + 2 * M_O) / M$Si / 1e4 +
                   conc[idx, "Ti"] * f * (M$Ti + 2 * M_O) / M$Ti / 1e4 +
                   conc[idx, "Fe"] * f * (2 * M$Fe + 3 * M_O) / (2 * M$Fe) / 1e4 +
                   conc[idx, "Ca"] * f * (M$Ca + M_O) / M$Ca / 1e4 +
                   conc[idx, "K"]  * f * (2 * M$K + M_O) / (2 * M$K) / 1e4)
  miss <- uj[["Al2O3"]] + uj[["MgO"]] + uj[["Na2O"]] + uj[["MnO"]] + uj[["P2O5"]]
  clo[[length(clo) + 1L]] <- data.frame(
    玻璃 = g, 可见主量参考 = vis_ref, 可见主量实测 = vis_meas,
    无通道主量合计 = miss, 可见占全岩 = vis_ref / (vis_ref + miss) * 100,
    全岩合计 = vis_ref + miss, check.names = FALSE)
}

rec_l <- do.call(rbind, rec_long)
rec_s <- do.call(rbind, rec_sum)
cl_t  <- do.call(rbind, clo)
ox <- function(x, m, r) x * (m + r * M_O) / m / 1e4   # 浓度 µg/g → 氧化物 wt%

# ---- 5b. 多锚点对照：换一个主量元素当内标，看偏移是不是 ²⁹Si 专属 ----------
# 内标法只会把「锚元素」那一列钳死，其余元素按相对灵敏度还原。所以用同一份
# 原始跑动、只换锚元素做后验换算，就能回答「是 Si 偏高，还是别的都偏低」。
# 若换 Ti 锚定后 Si 回到 ~125%、其余元素回到 ~100%，那问题就出在 ²⁹Si 通道。
ANCH <- list(
  Si = function(u) u[["SiO2"]] * (M$Si / (M$Si + 2 * M_O)) * 1e4,
  Ti = function(u) u[["TiO2"]] * (M$Ti / (M$Ti + 2 * M_O)) * 1e4,
  Ca = function(u) u[["CaO"]]  * (M$Ca / (M$Ca + 1 * M_O)) * 1e4)
KEY <- c("Si", "Ti", "Ca", "K", "Sr", "Y", "Zr", "Nb", "Ba", "La", "Ce", "Nd",
         "Sm", "Eu", "Gd", "Dy", "Er", "Yb", "Lu", "Hf", "Pb", "Th", "U")
anchor_tab <- list()
for (g in GLASS) {
  idx <- which(res$files$sample_name == g)
  uj  <- USGS[[g]]$major
  ref <- ref_of(g)
  for (a in names(ANCH)) {
    ta <- unname(ANCH[[a]](uj))
    if (is.na(ta) || !a %in% main_el) next
    sc <- ta / mean(conc[idx, a], na.rm = TRUE)      # 按锚元素换算的缩放
    for (e in KEY) {
      if (!e %in% main_el || !e %in% names(ref)) next
      mv <- mean(conc[idx, e], na.rm = TRUE) * sc
      if (is.na(mv) || mv <= 0) next
      anchor_tab[[length(anchor_tab) + 1L]] <- data.frame(
        玻璃 = g, 锚元素 = a, 元素 = e, 回收率pct = mv / ref[[e]] * 100,
        check.names = FALSE)
    }
  }
}
anc <- do.call(rbind, anchor_tab)
utils::write.csv(anc, file.path(OUT, "QC回收率_多锚点对照.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

utils::write.csv(rec_l, file.path(OUT, "QC回收率_三玻璃_逐测点.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
utils::write.csv(rec_s, file.path(OUT, "QC回收率_三玻璃_汇总.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
utils::write.csv(cl_t, file.path(OUT, "QC可见主量闭合.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cat("\n=== 三玻璃逐元素回收率（Si 锚定，%）===\n")
wide <- reshape(rec_s[is.na(rec_s$RSDpct) | rec_s$RSDpct < 1e9, c("元素", "玻璃", "回收率pct")],
                idvar = "元素", timevar = "玻璃", direction = "wide")
names(wide) <- sub("^回收率pct[.]", "", names(wide))
ord <- c("Si", "Ti", "K", "Ca", "Fe", "Rb", "Sr", "Y", "Zr", "Nb", "Ba",
         "La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er",
         "Tm", "Yb", "Lu", "Hf", "Ta", "Pb", "Th", "U", "Cs", "Mo", "W", "Ga", "Ge")
wide <- wide[match(intersect(ord, wide$元素), wide$元素), ]
print(data.frame(元素 = wide$元素, round(wide[, -1], 1)), row.names = FALSE)

cat("\n=== 可见主量闭合（三玻璃无 Na/Mg/Al 通道）===\n")
print(cl_t, row.names = FALSE, digits = 4)

cat("\n=== 多锚点对照：回收率（%）—— 锚元素本身必然是 100 ===\n")
w2 <- reshape(anc, idvar = c("玻璃", "元素"), timevar = "锚元素", direction = "wide")
names(w2) <- sub("^回收率pct[.]", "", names(w2))
w2 <- w2[order(w2$玻璃, match(w2$元素, KEY)), ]
print(w2, row.names = FALSE, digits = 3)

# ---- 6. 计数率诊断：612 的 ²⁹Si 是不是落在探测器非线性区 ------------------
# ²⁹Si 在 NIST612 里是全场最强的通道（SiO2 72 wt%）。若探测器在这一档出现
# 死时间/堆积，612 的 cps_Si 会偏低 → (cps_el/cps_Si)^612 偏大 → 由 612 传递
# 出去的非 Si 元素整体偏低。这里直接把净计数率量出来，看数量级。
#' 净计数率（信号窗均值 − 背景窗均值），按同位素名取列
net_cps <- function(path) {
  raw <- termite_read_raw(path, cfg, n_sweeps = cfg$n_sweeps_sample)
  v <- raw$values
  i1 <- .to_matrix_row(cfg$first_blank,  cfg$signal_line)
  i2 <- .to_matrix_row(cfg$last_blank,   cfg$signal_line)
  i3 <- .to_matrix_row(cfg$first_signal, cfg$signal_line)
  i4 <- .to_matrix_row(cfg$last_signal,  cfg$signal_line)
  sig <- colMeans(v[i3:i4, , drop = FALSE], na.rm = TRUE)
  blk <- colMeans(v[i1:i2, , drop = FALSE], na.rm = TRUE)
  names(sig) <- names(blk) <- probe$isotopes_raw
  sig - blk
}
DIAG <- c("29Si", "49Ti", "43Ca", "39K", "89Y", "140Ce")
path_of <- function(base) res$plan$path[match(base, basename(res$plan$path))]
diag_tab <- list()
sel <- c("SRM 612" = list(res$files$ref[1]),
         lapply(setNames(GLASS, GLASS), function(g)
           res$files$sample[which(res$files$sample_name == g)[1]]))
for (g in names(sel)) {
  p <- path_of(sel[[g]])
  if (length(p) != 1L || is.na(p)) next
  nc <- net_cps(p)
  kp <- intersect(DIAG, names(nc))
  diag_tab[[length(diag_tab) + 1L]] <- data.frame(
    物质 = g, 文件 = basename(p),
    as.data.frame(as.list(round(nc[kp] / 1000, 1)), check.names = FALSE),
    check.names = FALSE)
}
dgn <- do.call(rbind, diag_tab)
cat("\n=== 净计数率诊断（k cps，信号窗均值 − 背景窗均值）===\n")
print(dgn, row.names = FALSE)
utils::write.csv(dgn, file.path(OUT, "QC计数率诊断.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

# ---- 7. 生成「可直接贴进 Standards_GeoReM.csv」的三行 ------------------------
# 该表按「元素含量」填，同一元素的各同位素列取值相同（core 里也是这么用的）。
# 我们不直接改那张表（改了会让 tests/test-parity.R 的逐位基准失效），
# 只把现成的三行写出来，由你决定是否并入。
MM <- list(Si = M$Si, Ti = M$Ti, Ca = M$Ca, K = M$K, Al = 26.9815385,
           Mg = 24.305, Na = 22.98976928, P = 30.973761998,
           Mn = 54.938044, Fe = M$Fe)
#' 元素浓度（µg/g）= 氧化物 wt% × (元素摩尔质量 / 氧化物摩尔质量) × 1e4
#' 铁用全铁：BIR-1G/BHVO-2G 给的是 FeO_T，BCR-2G 给的是 Fe2O3_T
ref_all <- function(g) {
  out <- ref_of(g)
  uj  <- USGS[[g]]$major
  # 元素 ← 证书里的氧化物；铁用全铁（BIR-1G/BHVO-2G 给 FeO_T，BCR-2G 给 Fe2O3_T）
  spec <- list(c("Si", "SiO2", MM$Si + 2 * M_O), c("Ti", "TiO2", MM$Ti + 2 * M_O),
               c("Ca", "CaO", MM$Ca + M_O),      c("K",  "K2O", 2 * MM$K + M_O),
               c("Al", "Al2O3", 2 * MM$Al + 3 * M_O),
               c("Mg", "MgO", MM$Mg + M_O),      c("Na", "Na2O", 2 * MM$Na + M_O),
               c("P",  "P2O5", 2 * MM$P + 5 * M_O),
               c("Mn", "MnO", MM$Mn + M_O),
               c("Fe", "Fe2O3T", 2 * MM$Fe + 3 * M_O),
               c("Fe", "FeOT", 2 * MM$Fe + 2 * M_O))
  for (s in spec) {
    el <- s[[1]]; ox <- s[[2]]; mr <- as.numeric(s[[3]])
    if (ox %in% names(uj) && !el %in% names(out))
      out[[el]] <- uj[[ox]] * (MM[[el]] / mr) * 1e4
  }
  out
}
hdr  <- names(termite_read_standards(termite_resource(cfg$file_standards, cfg)))
cols <- setdiff(hdr, "Element")
elc  <- sub("[0-9]+$", "", cols)
drop_in <- data.frame(Element = GLASS, check.names = FALSE)
for (k in seq_along(cols)) {
  drop_in[[cols[k]]] <- vapply(GLASS, function(g) {
    a <- ref_all(g); i <- match(elc[k], names(a))
    if (is.na(i)) "NA" else format(a[[i]], digits = 10, scientific = FALSE, trim = TRUE)
  }, "")
}
utils::write.csv(drop_in, file.path(OUT, "QC参考值_可直接并入Standards_GeoReM.csv"),
                 row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
cat("\n已写出可直接并入参考表的三行：QC参考值_可直接并入Standards_GeoReM.csv\n")

cat("\n完成。输出目录：", OUT, "\n", sep = "")
