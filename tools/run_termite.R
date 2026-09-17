#!/usr/bin/env Rscript
# =============================================================================
#  TERMITE 命令行批处理入口
#  ---------------------------------------------------------------------------
#  不启动 Shiny 也能跑：
#
#    Rscript tools/run_termite.R \
#      --dir "G:/3.珊瑚钨锡矿床/锡石-白钨矿微量元素/20230806CKDA" \
#      --layout flat --auto \
#      --ref "SRM 610,SRM 612" --qc "BIR-1G,BCR-2G,BHVO-2G" \
#      --is 182W --is-conc 638500 \
#      --blank 25,60 --signal 70,118 \
#      --out "G:/.../20230806CKDA_TERMITE结果"
#
#  参数一律写成 --key value 或 --key=value；带逗号的值用引号包住。
# =============================================================================

.args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(a) {
  out <- list()
  i <- 1L
  while (i <= length(a)) {
    if (!grepl("^--", a[i])) { i <- i + 1L; next }
    kv <- sub("^--", "", a[i])
    if (grepl("=", kv, fixed = TRUE)) {
      k <- sub("=.*$", "", kv); v <- sub("^[^=]*=", "", kv)
      i <- i + 1L
    } else {
      k <- kv
      v <- if (i < length(a) && !grepl("^--", a[i + 1L])) a[i + 1L] else "TRUE"
      i <- i + if (v == "TRUE" && (i == length(a) || grepl("^--", a[i + 1L]))) 1L else 2L
    }
    out[[k]] <- v
  }
  out
}

split_csv <- function(x) if (is.null(x) || !nzchar(x)) character(0) else
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])

split2 <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  v <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
  if (length(v) != 2L || any(is.na(v))) return(default)
  as.integer(v)
}

flag <- function(x) !is.null(x) && (identical(x, "TRUE") || identical(x, "true") || identical(x, "1"))

a <- parse_args(.args)
if (is.null(a$dir)) {
  cat("用法：Rscript tools/run_termite.R --dir <数据目录> [选项]\n",
      "  常用：--layout flat|dir  --auto  --ref a,b  --qc a,b\n",
      "        --is <同位素>  --is-conc <µg/g>  --blank a,b  --signal a,b\n",
      "        --mode spot|line  --filter <正则>  --sample-filter <正则>\n",
      "        --out <输出目录>  --no-outlier  --no-clip  --legacy-lod\n", sep = "")
  quit(status = 2L)
}

# 定位仓库根目录（本脚本在 <root>/tools/ 下）
.root <- local({
  a2 <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a2[grep("^--file=", a2)])
  if (length(f)) dirname(dirname(normalizePath(f[1]))) else getwd()
})
for (f in list.files(file.path(.root, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
  source(f, encoding = "UTF-8")

cfg <- termite_defaults(if (!is.null(a$mode) && a$mode == "line") "line" else "spot")
cfg$dir      <- a$dir
cfg$app_dir  <- .root
cfg$layout   <- if (is.null(a$layout)) "flat" else a$layout
cfg$auto_detect <- flag(a$auto) || identical(cfg$layout, "flat")
if (!is.null(a$ref))    cfg$ref_names    <- split_csv(a$ref)
if (!is.null(a$qc))     cfg$qc_names     <- split_csv(a$qc)
if (identical(cfg$layout, "dir") && !is.null(a$ref)) cfg$ref_materials <- split_csv(a$ref)
if (!is.null(a$filter)) cfg$file_pattern <- a$filter
if (!is.null(a$`sample-filter`)) cfg$sample_filter <- a$`sample-filter`
if (!is.null(a$`is-conc`)) cfg$is_conc <- as.numeric(a$`is-conc`)
if (!is.null(a$col))       cfg$column_IS <- as.integer(a$col)
if (!is.null(a$blank))  { v <- split2(a$blank,  c(5L, 124L)); cfg$first_blank  <- v[1]; cfg$last_blank  <- v[2] }
if (!is.null(a$signal)) { v <- split2(a$signal, c(180L, 440L)); cfg$first_signal <- v[1]; cfg$last_signal <- v[2] }
if (flag(a$`no-outlier`)) cfg$outlier_test <- FALSE
if (flag(a$`no-clip`))    cfg$clip_negative <- FALSE
if (flag(a$`legacy-lod`)) cfg$legacy_lod <- TRUE

# --is 给的是同位素名，需要先探测出列号
if (!is.null(a$`is`) && nzchar(a$`is`)) {
  pl <- termite_plan(cfg)
  if (nrow(pl)) {
    pr <- termite_probe(pl$path[1])
    j  <- match(a$`is`, pr$isotopes_raw)
    if (is.na(j)) {
      cat("[错误] 同位素 ", a$`is`, " 不在文件表头里。\n", sep = "")
      cat("  可用：", paste(pr$isotopes_raw, collapse = ", "), "\n", sep = "")
      quit(status = 2L)
    }
    cfg$column_IS <- j + 1L
    cat(sprintf("内标 %s = 第 %d 列\n", a$`is`, cfg$column_IS))
  }
}

cat("开始归算：", cfg$dir, "\n")
warn <- character(0)
t0 <- Sys.time()
res <- withCallingHandlers(
  termite_run(cfg, progress = function(i, n, msg) if (interactive()) cat(" ", msg, "\n")),
  warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") })
cat(sprintf("完成，用时 %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (length(warn)) {
  cat("\n警告：\n"); cat(paste0("  - ", unique(warn), collapse = "\n"), "\n")
}

cat("\n规模：同位素 ", length(res$isotopes),
    " / 样品 ", length(res$files$sample),
    " / 定标参考 ", length(res$files$ref),
    " / 质量监控 ", length(res$files$qc),
    "  内标 ", res$is_iso, "\n", sep = "")

out <- if (!is.null(a$out)) a$out else file.path(cfg$dir, "TERMITE_results")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

tag <- basename(sub("/+$", "", cfg$dir))
utils::write.table(termite_table(res), file.path(out, sprintf("Results_%s.csv", tag)),
                   sep = "\t", row.names = FALSE, na = "NA")
utils::write.table(termite_lod_table(res), file.path(out, sprintf("LoD_%s.csv", tag)),
                   sep = "\t", row.names = FALSE, na = "NA")
utils::write.table(termite_rsf_table(res), file.path(out, sprintf("RSFused_%s.csv", tag)),
                   sep = "\t", row.names = FALSE, na = "NA")
rec <- termite_recovery(res, "all")
if (!is.null(rec))
  utils::write.table(rec, file.path(out, sprintf("Recovery_%s.csv", tag)),
                     sep = "\t", row.names = FALSE, na = "NA")
plan <- termite_plan(cfg)
sug  <- tryCatch(termite_suggest_is(plan, cfg, cfg$first_blank, cfg$last_blank,
                                    cfg$first_signal, cfg$last_signal, top = 3L),
                 error = function(e) NULL)
if (!is.null(sug))
  utils::write.table(sug[sug$rank == 1, ], file.path(out, sprintf("MainIsotope_%s.csv", tag)),
                     sep = "\t", row.names = FALSE, na = "NA")

pdf(file.path(out, sprintf("Plots_%s.pdf", tag)), width = 12, height = 8.5)
try(termite_plot_spot(res), silent = TRUE)
try(termite_plot_rsf(res), silent = TRUE)
try(termite_plot_lod(res), silent = TRUE)
try(termite_plot_blank(res, 1, "ref"), silent = TRUE)
dev.off()

writeLines(c(
  sprintf("数据目录    : %s", cfg$dir),
  sprintf("布局        : %s", cfg$layout),
  sprintf("自动识别    : %s", cfg$auto_detect),
  sprintf("测量模式    : %s", cfg$mode),
  sprintf("同位素      : %d 个 (%s ... %s)", length(res$isotopes),
          res$isotopes[1], res$isotopes[length(res$isotopes)]),
  sprintf("内标        : %s (文件第 %d 列)，样品内标含量 %g µg/g",
          res$is_iso, cfg$column_IS, cfg$is_conc),
  sprintf("定标参考物质: %s", paste(unique(res$files$ref_rm), collapse = ", ")),
  sprintf("质量监控    : %s", if (length(res$files$qc)) paste(unique(res$files$qc_rm), collapse = ", ") else "无"),
  sprintf("空白窗口    : %d - %d", cfg$first_blank, cfg$last_blank),
  sprintf("信号窗口    : %d - %d", cfg$first_signal, cfg$last_signal),
  sprintf("背景算法    : %s", cfg$background),
  sprintf("离群检验    : %s (m = %g%%)", cfg$outlier_test, cfg$outlier_pct),
  sprintf("负值截断    : %s", cfg$clip_negative),
  sprintf("LoD 文件选取: %s", if (cfg$legacy_lod) "复刻原脚本" else "全部参考文件"),
  "",
  if (length(warn)) c("警告：", paste0("  - ", unique(warn))) else "无警告"
), file.path(out, "运行参数.txt"))

cat("\n输出目录：", out, "\n", sep = "")
cat("  ", paste(list.files(out), collapse = "\n   "), "\n", sep = "")
