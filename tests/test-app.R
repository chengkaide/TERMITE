# =============================================================================
#  Shiny 应用集成测试（无浏览器）
#  ---------------------------------------------------------------------------
#  用 shiny::testServer() 把 server 端整个跑一遍，检查：
#    · 所有输入名与界面一致、无拼写错误
#    · 反应链（参数 → 归算 → 各输出）能完整走通
#    · 点分析与线扫描两条路径都可用
#
#  用法：  Rscript tests/test-app.R
# =============================================================================

root <- Sys.getenv("TERMITE_ROOT", unset = ".")
if (!file.exists(file.path(root, "R", "termite_core.R"))) root <- getwd()
setwd(root)
suppressPackageStartupMessages(library(shiny))
for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(f, encoding = "UTF-8")

base_inputs <- list(
  dir = "your_main_directory", mode = "spot", machine = "Agilent",
  dir_sample = "Rawdata_spotscan", dir_ref = "ReferenceMaterial_spotscan",
  ref_mats = c("NIST612", "MACS3"),
  file_isotopes = "TERMITEScriptFolder/AtomGewIsoAbund_NIST.csv",
  file_standards = "TERMITEScriptFolder/Standards_GeoReM.csv",
  n_iso = 12, header_line = 3, signal_line = 4, column_IS = 5,
  is_conc = 400003.81, resolution = "(LR)", background = "median",
  first_blank = 5, last_blank = 124, first_signal = 180, last_signal = 440,
  first_blank_ls = 5, last_blank_ls = 124,
  first_signal_ls = 143, last_signal_ls = 6890, laser_speed = 5,
  outlier_test = TRUE, outlier_pct = 30,
  clip_negative = TRUE, legacy_lod = FALSE, legacy_time_axis = FALSE,
  n_sweeps_sample = NA, n_sweeps_ref = NA,
  tabs = "总览", raw_which = "ref", raw_log = TRUE, raw_rows = c(5, 300),
  raw_file = "NIST612_spotscan_example_001.csv",
  verify = 0
)

fails <- 0L
check <- function(label, expr) {
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    cat("    FAIL:", conditionMessage(e), "\n"); FALSE
  })
  if (!ok) fails <<- fails + 1L
  cat(sprintf("  %-42s %s\n", label, if (ok) "OK" else "FAIL"))
}

run_case <- function(label, over) {
  cat("\n[", label, "]\n")
  inp <- utils::modifyList(base_inputs, over)
  testServer(termite_server, {
    do.call(session$setInputs, inp)
    session$setInputs(run = 2)
    check("status_bar 有内容", { o <- output$status_bar; stopifnot(!is.null(o)) })
    check("result 无错误", {
      r <- result()
      stopifnot(is.null(r$error), !is.null(r$isotopes))
    })
    check("总览 KPI", { o <- output$overview_kpi; stopifnot(!is.null(o)) })
    check("文件清单", { o <- output$file_list; stopifnot(nchar(o) > 10) })
    check("表头表", { o <- output$header_tbl; stopifnot(nrow(o) > 0) })
    check("原始信号图", { o <- output$raw_plot; stopifnot(!is.null(o)) })
    check("背景柱状图", { o <- output$blank_plot; stopifnot(!is.null(o)) })
    check("RSF 图", { o <- output$rsf_plot; stopifnot(!is.null(o)) })
    check("RSF 表", { o <- output$rsf_tbl; stopifnot(nrow(o) > 0) })
    check("回收率表", { o <- output$rm_check_tbl; stopifnot(nrow(o) > 0) })
    check("LoD 图", { o <- output$lod_plot; stopifnot(!is.null(o)) })
    check("LoD 表", { o <- output$lod_tbl; stopifnot(nrow(o) > 0) })
    check("结果图", { o <- output$result_plot; stopifnot(!is.null(o)) })
    check("结果表", { o <- output$result_tbl; stopifnot(nrow(o) > 0) })
    check("结果摘要", { o <- output$result_summary; stopifnot(!is.null(o)) })
    check("原理页渲染", {
      o <- output$principles
      stopifnot(nchar(paste(as.character(o), collapse = "")) > 1000)
    })
    check("下载：结果表", {
      r <- result(); d <- termite_table(r); stopifnot(ncol(d) == 13)
    })
  })
}

cat("\n===================================================================\n")
cat("  TERMITE Shiny · 服务端集成测试\n")
cat("===================================================================\n")

run_case("点分析 · 默认参数", list())

run_case("点分析 · 关闭离群检验 + 均值背景", list(
  outlier_test = FALSE, background = "mean", clip_negative = FALSE))

run_case("点分析 · 看样品文件的原始信号", list(
  raw_which = "sample", raw_file = "HLK2_001.csv", raw_log = FALSE,
  raw_rows = c(100, 300)))

run_case("线扫描 · 默认参数", list(
  mode = "line", dir_sample = "Rawdata_linescan", dir_ref = "ReferenceMaterial_linescan",
  ref_mats = "NIST612", first_signal = 130, last_signal = 545,
  raw_file = "NIST612_linescan_example_001.csv"))

run_case("线扫描 · 复刻模式", list(
  mode = "line", dir_sample = "Rawdata_linescan", dir_ref = "ReferenceMaterial_linescan",
  ref_mats = "NIST612", first_signal = 130, last_signal = 545,
  legacy_lod = TRUE, legacy_time_axis = TRUE, clip_negative = FALSE,
  raw_which = "sample", raw_file = "HLK2_linescan_example_001.csv",
  raw_rows = c(100, 600)))

cat("\n===================================================================\n")
if (fails == 0L) cat("  结果：全部通过\n") else cat(sprintf("  结果：%d 项失败\n", fails))
cat("===================================================================\n\n")
if (fails > 0L) quit(status = 1L)
