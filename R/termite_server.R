# =============================================================================
#  TERMITE Shiny —— 服务端
# =============================================================================

.termite_app_root <- function() {
  if (file.exists(file.path("R", "termite_core.R"))) return(getwd())
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f[1])))
  getwd()
}

#' 行号输入 → 整数向量；空值返回 NA
.as_line <- function(id, input) {
  v <- suppressWarnings(as.numeric(input[[id]]))
  if (!length(v) || is.na(v)) NA_integer_ else as.integer(v)
}
termite_server <- function(input, output, session) {

  root <- .termite_app_root()

  # ---------------------------------------------------------------- 参数组装
  is_spot <- reactive(identical(input$mode, "spot"))

  # 模式切换时，自动把目录与默认积分窗口换成对应模式的默认值
  observeEvent(input$mode, {
    d <- termite_defaults(input$mode)
    updateTextInput(session, "dir_sample", value = d$dir_sample)
    updateTextInput(session, "dir_ref",    value = d$dir_ref)
    updateNumericInput(session, "first_signal",    value = d$first_signal)
    updateNumericInput(session, "last_signal",     value = d$last_signal)
    updateCheckboxInput(session, "clip_negative",  value = d$clip_negative)
    updateSelectizeInput(session, "ref_mats", selected = d$ref_materials)
  }, ignoreInit = TRUE)

  # 参考物质下拉候选：从推荐值表里读
  observe({
    std <- file.path(input$dir, input$file_standards)
    if (!file.exists(std)) return()
    nm <- tryCatch(row.names(termite_read_standards(std)), error = function(e) NULL)
    if (!is.null(nm))
      updateSelectizeInput(session, "ref_mats", choices = nm, server = FALSE)
  })

  observe({
    if (!length(input$ref_mats))
      updateSelectizeInput(session, "ref_mats",
                           selected = termite_defaults(input$mode)$ref_materials)
  })

  cfg <- reactive({
    d <- termite_defaults(input$mode)
    d$dir            <- input$dir
    d$dir_sample     <- input$dir_sample
    d$dir_ref        <- input$dir_ref
    d$dir_results    <- "Results"
    d$file_isotopes  <- input$file_isotopes
    d$file_standards <- input$file_standards
    d$machine        <- input$machine
    d$resolution     <- input$resolution
    d$n_iso          <- as.integer(input$n_iso)
    d$header_line    <- as.integer(input$header_line)
    d$signal_line    <- as.integer(input$signal_line)
    d$column_IS      <- as.integer(input$column_IS)
    d$is_conc        <- as.numeric(input$is_conc)
    d$background     <- input$background
    d$first_blank    <- as.integer(input$first_blank)
    d$last_blank     <- as.integer(input$last_blank)
    d$first_signal   <- as.integer(input$first_signal)
    d$last_signal    <- as.integer(input$last_signal)
    d$first_blank_ls   <- as.integer(input$first_blank_ls)
    d$last_blank_ls    <- as.integer(input$last_blank_ls)
    d$first_signal_ls  <- as.integer(input$first_signal_ls)
    d$last_signal_ls   <- as.integer(input$last_signal_ls)
    d$laser_speed    <- as.numeric(input$laser_speed)
    d$outlier_test   <- isTRUE(input$outlier_test)
    d$outlier_pct    <- as.numeric(input$outlier_pct)
    d$ref_materials  <- as.character(input$ref_mats)
    d$clip_negative  <- isTRUE(input$clip_negative)
    d$legacy_lod     <- isTRUE(input$legacy_lod)
    d$legacy_time_axis <- isTRUE(input$legacy_time_axis)
    d$n_sweeps_sample <- suppressWarnings(as.integer(input$n_sweeps_sample))
    d$n_sweeps_ref    <- suppressWarnings(as.integer(input$n_sweeps_ref))
    d
  })

  # ---------------------------------------------------------------- 运行
  run_flag <- reactiveVal(NULL)
  observeEvent(input$run, run_flag(Sys.time()), ignoreNULL = TRUE)
  # 首屏自动跑一次默认参数
  observe({ if (is.null(isolate(run_flag()))) run_flag(Sys.time()) })

  result <- eventReactive(run_flag(), {
    c0 <- cfg()
    warn <- character(0)
    out <- withCallingHandlers(
      tryCatch(
        withProgress(message = "归算中…", value = 0, {
          termite_run(c0, progress = function(i, n, msg)
            incProgress(1 / max(1, n), detail = msg))
        }),
        error = function(e) structure(list(error = conditionMessage(e)), class = "termite_error")
      ),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    attr(out, "warnings") <- warn
    out
  })

  ok <- reactive(!inherits(result(), "termite_error") && !is.null(result()$isotopes))

  # ---------------------------------------------------------------- 状态条
  output$status_bar <- renderUI({
    if (is.null(run_flag())) return(div(class = "ok-box", "正在准备…"))
    r <- result()
    if (inherits(r, "termite_error"))
      return(div(class = "warn-box", paste("运行出错：", r$error)))
    div(class = "ok-box",
        sprintf("完成 · 样品 %d 个 / 参考 %d 个 · %d 个同位素",
                length(r$files$sample), length(r$files$ref), length(r$isotopes)))
  })

  output$overview_kpi <- renderUI({
    req(ok()); r <- result()
    k <- function(lab, val) div(class = "kpi", HTML(sprintf("%s <b>%s</b>", lab, val)))
    tagList(
      k("模式", if (identical(r$mode, "spot")) "点分析" else "线扫描"),
      k("同位素", length(r$isotopes)),
      k("内标", r$is_iso),
      k("样品文件", length(r$files$sample)),
      k("参考文件", length(r$files$ref)),
      k("背景算法", if (identical(r$cfg$background, "mean")) "平均值" else "中位数"),
      k("离群检验", if (isTRUE(r$cfg$outlier_test))
        sprintf("开 (m=%g%%)", r$cfg$outlier_pct) else "关"),
      k("负值截断", if (isTRUE(r$cfg$clip_negative)) "开" else "关"),
      k("LoD 文件选取", if (isTRUE(r$cfg$legacy_lod)) "复刻原脚本" else "全部参考文件"),
      k("x 轴", if (identical(r$mode, "line"))
        (if (isTRUE(r$cfg$legacy_time_axis)) "复刻原脚本" else "按数据行对齐") else "—")
    )
  })

  output$run_warnings <- renderUI({
    w <- attr(result(), "warnings")
    if (!length(w)) return(div(class = "ok-box", "无警告。"))
    div(class = "warn-box", paste(unique(w), collapse = "\n"))
  })

  output$file_list <- renderText({
    req(ok()); r <- result()
    paste0("样品目录：", r$cfg$dir_sample, "\n  ",
           paste(r$files$sample, collapse = "\n  "),
           "\n\n参考物质目录：", r$cfg$dir_ref, "\n  ",
           paste(sprintf("%s  [%s]", r$files$ref, r$files$ref_rm), collapse = "\n  "))
  })

  output$header_tbl <- renderTable({
    req(ok()); r <- result()
    iso <- termite_read_isotopes(file.path(r$cfg$dir, r$cfg$file_isotopes))
    data.frame(序号 = seq_along(r$isotopes),
               同位素 = r$isotopes,
               元素 = r$elements,
               原子量 = signif(as.numeric(iso[1, r$isotopes]), 8),
               同位素丰度 = signif(as.numeric(iso[2, r$isotopes]), 8),
               换算因子 = signif(as.numeric(r$iso_factor), 6),
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  # ---------------------------------------------------------------- 原始信号
  output$raw_file_ui <- renderUI({
    req(ok()); r <- result()
    f <- if (identical(input$raw_which, "ref")) r$files$ref else r$files$sample
    selectInput("raw_file", "文件", f)
  })

  .raw_mat <- reactive({
    req(ok(), input$raw_file); r <- result()
    files <- if (identical(input$raw_which, "ref")) r$cfg$dir_ref else r$cfg$dir_sample
    p <- file.path(r$cfg$dir, files, input$raw_file)
    if (!file.exists(p)) return(NULL)
    termite_read_raw(p, r$cfg, n_sweeps = NA)$values
  })

  observe({
    m <- .raw_mat(); req(m)
    r <- result()
    n <- nrow(m); lo <- r$cfg$signal_line
    updateSliderInput(session, "raw_rows", min = lo, max = n + lo - 1L,
                      value = c(lo, min(n + lo - 1L, lo + 800L)), step = 1)
  })

  output$raw_plot <- renderPlot({
    m <- .raw_mat(); req(m); r <- result()
    off <- r$cfg$signal_line - 1L
    rr <- input$raw_rows
    if (is.null(rr)) rr <- c(r$cfg$signal_line, r$cfg$signal_line + min(nrow(m), 800L) - 1L)
    # 滑块给的是文件真实行号 → 换算成矩阵行号
    i1 <- max(1L, rr[1] - off)
    i2 <- min(nrow(m), rr[2] - off)
    rows <- if (i2 < i1) i1 else seq.int(i1, i2)
    bl <- c(r$cfg$first_blank, r$cfg$last_blank)
    sg <- if (!identical(input$raw_which, "ref") && identical(r$mode, "line"))
      c(r$cfg$first_signal_ls, r$cfg$last_signal_ls)
    else c(r$cfg$first_signal, r$cfg$last_signal)
    termite_plot_raw(m, r$isotopes, blank = bl, signal = sg, rows = rows,
                     offset = off, log_y = isTRUE(input$raw_log),
                     main = sprintf("%s　（灰=空白区，红=信号区）", input$raw_file))
  }, res = 96)

  output$blank_plot <- renderPlot({
    req(ok()); r <- result()
    if (identical(input$raw_which, "ref")) {
      i <- match(input$raw_file, r$files$ref); if (is.na(i)) i <- 1L
      termite_plot_blank(r, i, "ref")
    } else {
      fs <- if (identical(r$mode, "spot")) r$files$sample else vapply(r$line, `[[`, "", "file")
      i <- match(input$raw_file, fs); if (is.na(i)) i <- 1L
      termite_plot_blank(r, i, "sample")
    }
  }, res = 96)

  # ---------------------------------------------------------------- RSF
  output$rsf_plot <- renderPlot({ req(ok()); termite_plot_rsf(result()) }, res = 96)

  output$rm_check_ui <- renderUI({
    req(ok()); r <- result()
    tagList(
      p(style = "font-size:12.5px;color:#718096",
        "回收率 = 该参考物质实测 RSF ÷ 全局 RSF 均值。理想情况下应接近 100%；",
        "同一种参考物质内部各文件之间的一致性反映样品室内均匀性与信号稳定性。"),
      tableOutput("rm_check_tbl")
    )
  })

  output$rm_check_tbl <- renderTable({
    req(ok()); r <- result()
    parts <- lapply(unique(r$files$ref_rm), function(rm) {
      d <- termite_rm_check(r, rm)
      if (is.null(d)) return(NULL)
      data.frame(参考物质 = rm, 同位素 = d$isotope,
                 实测RSF均值 = signif(d$measured, 6),
                 回收率_pct = round(d$recovery_pct, 2), check.names = FALSE)
    })
    do.call(rbind, parts)
  }, striped = TRUE, bordered = TRUE, na = "")

  output$rsf_tbl <- renderTable({
    req(ok()); r <- result()
    d <- termite_rsf_table(r); d$RSF <- signif(d$RSF, 6)
    names(d) <- c("同位素", "元素", "RSF（全局均值）"); d
  }, striped = TRUE, bordered = TRUE)

  output$dl_rsf <- downloadHandler(
    filename = function() sprintf("RSFused_%s.csv", Sys.Date()),
    content  = function(f) write.csv(termite_rsf_table(result()), f, row.names = FALSE)
  )

  # ---------------------------------------------------------------- LoD
  output$lod_plot <- renderPlot({ req(ok()); termite_plot_lod(result()) }, res = 96)

  output$lod_tbl <- renderTable({
    req(ok()); r <- result()
    d <- termite_lod_table(r); d$LoD_ug_g <- signif(d$LoD_ug_g, 6)
    names(d) <- c("同位素", "元素", "LoD [µg/g]")
    d
  }, striped = TRUE, bordered = TRUE)

  output$dl_lod <- downloadHandler(
    filename = function() sprintf("LoD_%s.csv", Sys.Date()),
    content  = function(f) write.csv(termite_lod_table(result()), f, row.names = FALSE)
  )

  # ---------------------------------------------------------------- 结果
  output$result_summary <- renderUI({
    req(ok()); r <- result()
    d <- termite_table(r)
    if (identical(r$mode, "spot")) {
      div(class = "ok-box",
          sprintf("点分析：%d 个样品 × %d 个同位素。低于 LoD 或 ≤0 的单元格已置为 NA。",
                  nrow(d), length(r$isotopes)),
          br(), "导出列名使用元素符号，与原脚本 write.table 的输出一致。")
    } else {
      div(class = "ok-box",
          sprintf("线扫描：%d 个数据点 × %d 个同位素，剖面长 %.4f mm。",
                  nrow(d), length(r$isotopes), max(r$line[[1]]$distance)),
          br(), "x 轴为激光开剥起点的距离。")
    }
  })

  output$result_plot <- renderPlot({
    req(ok()); r <- result()
    if (identical(r$mode, "spot")) termite_plot_spot(r) else termite_plot_profile(r)
  }, res = 96)

  output$result_tbl_note <- renderUI({
    req(ok()); r <- result(); d <- termite_table(r)
    if (nrow(d) > 200)
      div(style = "font-size:12.5px;color:#718096",
          sprintf("表中共 %d 行，页面只预览前 200 行；完整数据请用下方按钮下载。", nrow(d)))
  })

  output$result_tbl <- renderTable({
    req(ok()); r <- result()
    d <- termite_table(r)
    if (nrow(d) > 200) d <- d[seq_len(200), , drop = FALSE]
    num <- vapply(d, is.numeric, logical(1))
    d[num] <- lapply(d[num], function(x) ifelse(is.na(x), NA, signif(x, 6)))
    d
  }, striped = TRUE, bordered = TRUE, digits = 6, na = "")

  output$dl_result <- downloadHandler(
    filename = function() sprintf("TERMITE_results_%s_%s.csv",
                                  result()$mode, format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(f)
      utils::write.table(termite_table(result()), f, sep = "\t", row.names = FALSE, na = "NA")
  )

  output$dl_masked_flag <- downloadHandler(
    filename = function() sprintf("TERMITE_outlier_frac_%s_%s.csv",
                                  result()$mode, format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(f) {
      r <- result()
      m <- if (identical(r$mode, "spot")) r$sample$outlier_frac else r$ref$outlier_frac
      utils::write.table(round(m, 4), f, sep = "\t", col.names = NA)
    }
  )

  # ---------------------------------------------------------------- 原理
  output$principles <- renderUI({
    p <- file.path(root, "docs", "PRINCIPLES.md")
    if (!file.exists(p))
      return(div(class = "warn-box", paste("找不到", p)))
    HTML(md_to_html(paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
  })

  output$verify_doc <- renderUI({
    HTML(md_to_html(paste(
      "自检会把本内核在**复刻模式**下重跑一遍，并与 `tests/reference/` 里的基准文件逐列比较。",
      "",
      "- 基准文件由**原始脚本**在自带示例数据上产出。",
      "- 点分析：原脚本可直接运行。",
      "- 线扫描：原脚本有 3 处致命/类型错误，先做最小修补后才跑通；修补后输出与仓库自带的",
      "  `Results_*_linescan.csv` 逐位一致。",
      "- 判定标准：NA 模式必须完全一致，且最大相对偏差 ≤ 1e-9（RSF/LoD 因为基准文件带舍入，放宽到 1e-12 的舍入后比较）。",
      sep = "\n")))
  })

  verify_res <- eventReactive(input$verify, {
    withProgress(message = "正在重跑一致性检查…", value = 0.3, {
      incProgress(0.3)
      v <- termite_verify(data_dir = file.path(root, "your_main_directory"),
                          ref_dir = file.path(root, "tests", "reference"))
      incProgress(0.4)
      v
    })
  })

  output$verify_out <- renderText({
    if (is.null(input$verify) || input$verify == 0)
      return("点击上面的按钮开始检查（约需 1 分钟）。")
    termite_verify_text(verify_res())
  })
}
