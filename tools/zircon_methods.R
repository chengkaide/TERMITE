#!/usr/bin/env Rscript
# =============================================================================
#  锆石微量元素「多口径」归算 —— Zr 内标 / Si 内标 / 化学式归一化
#  ---------------------------------------------------------------------------
#  同一批数据、同一套积分窗口，只换校准口径：
#
#    M1  Zr 内标   91Zr，样品内标含量取锆石化学计量值 497700 µg/g
#                  （等价于假定 ZrO2 ≈ 67.2 wt%）
#    M2  Si 内标   29Si，样品内标含量取锆石化学计量值 153200 µg/g
#                  （等价于假定 SiO2 ≈ 32.8 wt%）
#    M3  归一法-A  化学式 fixed Si:1 —— Si 固定 1 apfu，其余阳离子按电荷平衡归一
#                  （强行用锆石结构式把 Zr : Si 钉在 1 : 1）
#    M4  归一法-B  化学式 anhydrous —— 全电荷归一（2·n_O = 8）；
#                  锆石阳离子以 4+ 为主，故等价于「氧化物归一到 100 wt%」，
#                  也就是实验室 ICPMSDataCal「Contents (AYCF)」的做法
#
#  M1/M2 是内标法：必须假定某个内标元素的绝对含量，假定错了全体元素整体平移。
#  M3/M4 是无内标归一化：用矿物化学式替代内标，不依赖任何单元素的绝对含量。
#  两者在「Si 与 Zr 的相对灵敏度」上分道扬镳 —— 这正是要比较的地方。
#
#  四条口径的元素轴统一到「每个元素的主同位素」（丰度最高者，与公式法内部一致）。
#
#  用法：Rscript tools/zircon_methods.R
# =============================================================================

options(encoding = "UTF-8")

# --- 本机 locale 守卫 ------------------------------------------------------
# Bash 工具会注入 LC_ALL=C.UTF-8（Windows 无此 locale），R 回退到 "C"/ASCII，
# 于是 source() 里中文转码失败会把整个文件吞掉。这里显式设回 UTF-8 族。
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.args <- commandArgs(trailingOnly = FALSE)
scr <- sub("^--file=", "", .args[grepl("^--file=", .args)])
ROOT <- if (length(scr)) dirname(dirname(normalizePath(scr[1], mustWork = FALSE))) else getwd()
for (f in list.files(file.path(ROOT, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  source(f, encoding = "UTF-8")

# --- 批次与积分窗口 ---------------------------------------------------------
# 数据目录用环境变量传入（脚本要进公开仓库，不写死任何本机路径）：
#   TERMITE_DIR=<批次目录>  OUT=<输出目录>  SIG1=<信号起始配置行>  SIG2=<信号结束配置行>
# 行号口径：Qtegra 文件 header_line = 14、signal_line = 16，
# 于是「配置行号 R」= 数据第 R-15 行。默认 100–158 即数据行 85–143，
# 落在剥蚀段（数据行约 62–150）中段，避开了表面预剥蚀与尾部洗脱。
DIR <- Sys.getenv("TERMITE_DIR", "")
if (!nzchar(DIR))
  stop("请先用环境变量 TERMITE_DIR 指定数据批次目录（例如 TERMITE_DIR=/path/to/batch）。")
OUT <- Sys.getenv("OUT", file.path(DIR, "TERMITE_results"))
SIG1 <- as.integer(Sys.getenv("SIG1", "100"))
SIG2 <- as.integer(Sys.getenv("SIG2", "158"))

# 锆石化学计量 ZrSiO4：Si 15.32 wt%、Zr 49.77 wt%
IS_SI <- 153200
IS_ZR <- 497700

base_cfg <- function(mode = "spot", layout = "flat") {
  cfg <- termite_defaults(mode)
  cfg$dir         <- DIR
  cfg$app_dir     <- ROOT
  cfg$layout      <- layout
  cfg$auto_detect <- TRUE            # flat 布局必须靠内容探测
  cfg$ref_names   <- c("SRM 612")
  cfg$qc_names    <- c("BIR-1G", "BCR-2G", "BHVO-2G", "Tanz")
  cfg$first_blank <- 25L;  cfg$last_blank  <- 60L
  cfg$first_signal <- SIG1; cfg$last_signal <- SIG2
  cfg
}

# 内标列号由同位素名现查表头，不写死数字
probe <- termite_probe(termite_plan(base_cfg())$path[1])
cat("文件通道（", length(probe$isotopes_raw), " 个）：",
    paste(probe$isotopes_raw, collapse = ", "), "\n", sep = "")

#' 内标法：返回 list(res, conc)（conc 未做 LoD 掩膜，便于全元素对齐）
run_is <- function(is_iso, is_conc, tag) {
  cfg <- base_cfg()
  j <- match(is_iso, probe$isotopes_raw)
  if (is.na(j)) stop("表头里找不到同位素 ", is_iso)
  cfg$column_IS <- j + 1L            # column_IS 是文件列号（含 Time 列）
  cfg$is_conc   <- is_conc
  cat(sprintf("\n== %s：内标 %s = 文件第 %d 列，C = %g µg/g ==\n",
              tag, is_iso, cfg$column_IS, is_conc))
  res <- termite_run(cfg)
  cat(sprintf("   SRM612(n=%d) 内标通道回收率 %.3f–%.3f %%；全部通道 %.1f–%.1f %%\n",
              sum(res$files$ref_rm == "NIST612"),
              min(res$recovery_ref[, is_iso], na.rm = TRUE),
              max(res$recovery_ref[, is_iso], na.rm = TRUE),
              min(res$recovery_ref, na.rm = TRUE), max(res$recovery_ref, na.rm = TRUE)))
  list(res = res, conc = res$sample$conc)
}

#' 公式法归一化
run_formula <- function(mineral, tag) {
  cat(sprintf("\n== %s：公式法 %s ==\n", tag, mineral))
  res <- termite_run_formula(base_cfg(), mineral)
  list(res = res, conc = res$samples)
}

m1 <- run_is("91Zr", IS_ZR, "M1_Zr内标")
m2 <- run_is("29Si", IS_SI, "M2_Si内标")
m3 <- run_formula("zircon",      "M3_归一法_fixedSi1")
m4 <- run_formula("zircon_full", "M4_归一法_anhydrous")

# --- 统一元素轴：公式法已按「每元素主同位素」挑选，内标法照此取列 --------------
main_iso <- m3$res$isotopes            # 如 29Si / 91Zr / 115In / 208Pb ...
main_el  <- m3$res$elements
cat("\n元素轴（", length(main_el), " 个）：", paste(main_el, collapse = " "), "\n", sep = "")

#' 内标法结果是「同位素列」→ 按主同位素选列并改成元素名；
#' 公式法结果本来就是「元素列」，只需统一行名/列名。
to_el <- function(conc, sid) {
  if (all(main_el %in% colnames(conc))) {        # 公式法
    m <- conc[, main_el, drop = FALSE]
  } else {                                       # 内标法
    if (!all(main_iso %in% colnames(conc)))
      stop("缺列：", paste(setdiff(main_iso, colnames(conc)), collapse = ", "))
    m <- conc[, main_iso, drop = FALSE]
  }
  dimnames(m) <- list(sid, main_el)
  m
}
C1 <- to_el(m1$conc, m1$res$files$sample_name)
C2 <- to_el(m2$conc, m2$res$files$sample_name)
C3 <- to_el(m3$conc, rownames(m3$conc))
C4 <- to_el(m4$conc, rownames(m4$conc))

# 样品顺序对齐（公式法用 plan 里的样品名，与内标法一致；这里显式校验）
stopifnot(identical(rownames(C1), rownames(C3)), identical(rownames(C1), rownames(C4)))

# --- 导出：每个口径一张「样品 × 元素」CSV ----------------------------------
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
write_mat <- function(m, file) {
  d <- data.frame(ID = rownames(m), as.data.frame(m, check.names = FALSE),
                  check.names = FALSE, row.names = NULL)
  utils::write.csv(d, file.path(OUT, file), row.names = FALSE, na = "NA")
  cat(sprintf("   写出 %-36s %d 样品 × %d 元素\n", file, nrow(d), ncol(d) - 1L))
}
write_mat(C1, "方法_M1_Zr内标.csv")
write_mat(C2, "方法_M2_Si内标.csv")
write_mat(C3, "方法_M3_归一法_fixedSi1.csv")
write_mat(C4, "方法_M4_归一法_anhydrous.csv")

# SRM612 逐通道回收率留档
if (!is.null(m1$res$recovery_ref))
  utils::write.csv(data.frame(文件 = rownames(m1$res$recovery_ref),
                              m1$res$recovery_ref, check.names = FALSE),
                   file.path(OUT, "回收率_SRM612_Zr内标.csv"), row.names = FALSE, na = "NA")

# --- 主量闭合诊断：SiO2 / ZrO2 / HfO2 / 其余，应加和到 ~100 wt% --------------
# 锆石近似 (Zr,Hf)SiO4。加和显著偏离 100 wt% 就说明「Si 与 Zr 的相对灵敏度」
# 没被这套校准链正确还原 —— 这正是本对比要暴露的核心问题。
M_O <- 15.999
# 元素 µg/g → 氧化物 wt%：氧化物化学式中每个阳离子配 r 个氧
#   2 → SiO2/ZrO2/HfO2/TiO2/UO2/ThO2 …   1 → CaO/FeO/PbO …
#   1.5 → R2O3（REE、Y）                  0.5 → K2O   2.5 → Nb2O5/Ta2O5/P2O5
oxo <- function(x, M, r) x * (M + r * M_O) / M / 1e4
REE <- c("La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu")

close_tab <- function(conc, tag) {
  el  <- colnames(conc)
  get <- function(e) if (e %in% el) as.numeric(conc[, e]) else rep(0, nrow(conc))
  sio2 <- oxo(get("Si"), 28.0855, 2);  zro2 <- oxo(get("Zr"), 91.224, 2)
  hfo2 <- oxo(get("Hf"), 178.49, 2);   tio2 <- oxo(get("Ti"), 47.867, 2)
  uo2  <- oxo(get("U"), 238.02891, 2); tho2 <- oxo(get("Th"), 232.0377, 2)
  y2o3 <- oxo(get("Y"), 88.90584, 1.5)
  reo  <- vapply(REE, function(e) oxo(get(e), .termite_M_default[[e]], 1.5),
                 numeric(nrow(conc)))
  xo   <- oxo(get("Ca"), 40.078, 1) + oxo(get("K"), 39.0983, 0.5) +
          oxo(get("Fe"), 55.845, 1) + oxo(get("Nb"), 92.90637, 2.5) +
          oxo(get("Ta"), 180.94788, 2.5) + oxo(get("W"), 183.84, 3) +
          oxo(get("Pb"), 207.2, 1)
  data.frame(口径 = tag, SiO2 = mean(sio2), ZrO2 = mean(zro2), HfO2 = mean(hfo2),
             ZrSiHf = mean(sio2 + zro2 + hfo2),
             UTh = mean(uo2 + tho2), YREE = mean(y2o3 + rowSums(reo)),
             其他 = mean(tio2 + xo),
             总和 = mean(sio2 + zro2 + hfo2 + uo2 + tho2 + y2o3 + rowSums(reo) + tio2 + xo))
}
cl <- rbind(close_tab(C1, "M1_Zr内标"), close_tab(C2, "M2_Si内标"),
            close_tab(C3, "M3_归一法_fixedSi1"), close_tab(C4, "M4_归一法_anhydrous"))
cat("\n=== 主量氧化物闭合（30 点平均，wt%）===\n")
print(cbind(口径 = cl$口径, round(cl[, -1], 3)), row.names = FALSE)
utils::write.csv(cl, file.path(OUT, "主量闭合诊断.csv"), row.names = FALSE)

cat("\n完成。输出目录：", OUT, "\n", sep = "")
