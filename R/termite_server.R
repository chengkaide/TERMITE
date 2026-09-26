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

  # 参考物质下拉候选：推荐值表的行名 + 别名表里的仪器写法
  observe({
    c0 <- list(dir = input$dir, app_dir = root)
    std <- termite_resource(input$file_standards, c0)
    nm  <- if (file.exists(std)) tryCatch(row.names(termite_read_standards(std)),
                                          error = function(e) NULL) else NULL
    al  <- termite_read_alias(termite_resource(input$file_alias, c0))
    if (!is.null(al)) nm <- unique(c(nm, al$alias))
    if (!is.null(nm)) updateSelectizeInput(session, "ref_mats", choices = nm, server = FALSE)
    if (!is.null(nm)) updateSelectizeInput(session, "qc_mats",  choices = nm, server = FALSE)
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
    d$layout         <- input$layout
    d$auto_detect    <- isTRUE(input$auto_detect)
    d$file_pattern   <- if (nzchar(input$file_pattern)) input$file_pattern else "\\.(csv|asc|txt)$"
    d$sample_filter  <- input$sample_filter
    d$file_alias     <- input$file_alias
    d$file_iso_alias <- input$file_iso_alias
    d$ref_names      <- as.character(input$ref_mats)
    d$qc_names       <- as.character(input$qc_mats)
    d$app_dir        <- root
    d$dir_results    <- "Results"
    d$file_isotopes  <- input$file_isotopes
    d$file_standards <- input$file_standards
    d$machine        <- input$machine
    d$resolution     <- input$resolution
    d$n_iso          <- as.integer(input$n_iso)
    d$header_line    <- as.integer(input$header_line)
    d$signal_line    <- as.integer(input$signal_line)
    d$column_IS      <- as.integer(input$column_IS)
    if (length(input$is_iso_sel) && nzchar(input$is_iso_sel)) {   # 选了同位素名就换算成列号
      iso_now <- scan_rv()$iso
      j <- match(input$is_iso_sel, iso_now)
      if (!is.na(j)) d$column_IS <- j + 1L
    }
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
    iso <- termite_read_isotopes(termite_resource(r$cfg$file_isotopes, r$cfg))
    key <- r$db_key                 # 查参考表用的列名（可能与文件里的写法不同）
    data.frame(序号 = seq_along(r$isotopes),
               同位素 = r$isotopes,
               元素 = r$elements,
               参考表列名 = ifelse(key == r$isotopes_db, "", key),
               原子量 = signif(as.numeric(iso[1, key]), 8),
               同位素丰度 = signif(as.numeric(iso[2, key]), 8),
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
                     which_iso = plot_iso(),
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
  output$rsf_plot <- renderPlot({
    req(ok()); termite_plot_rsf(result(), isotopes = plot_iso())
  }, res = 96)

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
    el <- if (is.null(plot_iso())) NULL else unique(element_of(plot_iso()))
    if (identical(r$mode, "spot")) termite_plot_spot(r, elements = el)
    else termite_plot_profile(r, elements = el)
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

  # 绘图要显示哪些同位素（58 个全画会糊成一团）
  observe({
    s <- scan_rv()
    rr <- if (!is.null(isolate(run_flag()))) isolate(result()) else NULL
    ch <- if (!is.null(rr) && is.null(rr$error) && !is.null(rr$isotopes)) rr$isotopes else
          if (!is.null(s) && length(s$iso)) s$iso else NULL
    if (!is.null(ch)) updateSelectizeInput(session, "plot_iso", choices = ch)
  })

  plot_iso <- reactive({
    v <- input$plot_iso
    if (is.null(v) || !length(v)) NULL else as.character(v)
  })

  # ---------------------------------------------------------------- 导入数据
  scan_rv <- reactiveVal(NULL)

  observeEvent(input$scan, {
    c0 <- cfg()
    out <- tryCatch({
      pl <- termite_plan(c0, probe_all = TRUE)
      pr <- NULL; n_data <- NA_integer_
      if (nrow(pl)) {
        pr <- tryCatch(termite_probe(pl$path[1]), error = function(e) NULL)
        n_data <- tryCatch(nrow(termite_read_probed(pl$path[1], pr)$values),
                           error = function(e) NA_integer_)
      }
      list(plan = pl, probe = pr, n_data = n_data, error = NULL,
           iso = if (!is.null(pr)) pr$isotopes_raw else character(0))
    }, error = function(e)
      list(plan = NULL, probe = NULL, n_data = NA_integer_,
           error = conditionMessage(e), iso = character(0)))
    scan_rv(out)
    if (is.null(out$error) && nrow(out$plan)) {
      nm <- unique(out$plan$sample_name)
      isolate({
        updateSelectizeInput(session, "ref_mats", choices = unique(c(input$ref_mats, nm)),
                             selected = input$ref_mats)
        updateSelectizeInput(session, "qc_mats", choices = unique(c(input$qc_mats, nm)),
                             selected = input$qc_mats)
      })
      updateSelectizeInput(session, "is_iso_sel", choices = out$iso,
                           selected = if (length(out$iso)) out$iso[1] else NULL)
    }
  })

  observeEvent(input$apply_probe, {
    s <- scan_rv(); req(s, s$probe)
    updateNumericInput(session, "header_line", value = s$probe$header_line)
    updateNumericInput(session, "signal_line", value = s$probe$signal_line)
    updateNumericInput(session, "n_iso",       value = s$probe$n_iso)
    if (!is.na(s$n_data)) {                       # 给一组可用的起始积分窗口
      lo <- s$probe$signal_line; hi <- lo + s$n_data - 1L
      updateNumericInput(session, "first_blank",  value = lo)
      updateNumericInput(session, "last_blank",   value = lo + max(1L, round(s$n_data * 0.15)) - 1L)
      updateNumericInput(session, "first_signal", value = lo + round(s$n_data * 0.35))
      updateNumericInput(session, "last_signal",  value = lo + max(1L, round(s$n_data * 0.92)))
    }
    updateActionButton(session, "apply_probe", label = "已填回左侧参数 ✓")
  })

  output$import_summary <- renderUI({
    s <- scan_rv()
    if (is.null(s)) return(div(class = "kpi", "还没有扫描。点左侧「扫描目录并预览」。"))
    if (!is.null(s$error))
      return(div(class = "warn-box", paste("扫描失败：", s$error)))
    pl <- s$plan
    if (!nrow(pl)) return(div(class = "warn-box", "没有找到任何数据文件，请检查目录与文件名过滤。"))
    k <- function(l, v) div(class = "kpi", HTML(sprintf("%s <b>%s</b>", l, v)))
    pr <- s$probe
    tagList(
      k("文件数", nrow(pl)),
      k("样品", sum(pl$kind == "sample")),
      k("定标参考", sum(pl$kind == "ref")),
      k("质量监控", sum(pl$kind == "qc")),
      if (!is.null(pr)) k("识别格式", pr$format),
      if (!is.null(pr)) k("表头行", pr$header_line),
      if (!is.null(pr)) k("数据起始行", pr$signal_line),
      if (!is.null(pr)) k("同位素个数", pr$n_iso),
      if (!is.null(pr)) k("分隔符", if (pr$sep == "\t") "制表符" else pr$sep),
      if (!is.null(pr)) k("同位素写法", if (pr$isotope_style == "mass-first") "25Mg" else "Mg25"),
      if (!is.na(s$n_data)) k("每文件数据行", s$n_data),
      div(style = "margin-top:6px"),
      if (sum(pl$kind == "sample") == 0)
        div(class = "warn-box",
            "没有识别出任何样品。flat 布局下，参考物质/监控名单**之外**的文件才算样品；",
            "请确认名单填对了，或检查样品名过滤是否把样品全滤掉了。")
    )
  })

  output$import_tbl <- renderTable({
    s <- scan_rv(); req(s, s$plan)
    if (!nrow(s$plan)) return(NULL)
    d <- s$plan
    d$kind <- c(sample = "样品", ref = "定标参考", qc = "质量监控")[d$kind]
    d[, c("file", "sample_name", "kind", "material", "format", "n_iso")]
  }, striped = TRUE, bordered = TRUE, na = "")

  output$dl_plan <- downloadHandler(
    filename = function() sprintf("TERMITE_import_plan_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(f) {
      s <- scan_rv(); if (is.null(s) || is.null(s$plan)) { writeLines("", f); return() }
      utils::write.csv(s$plan, f, row.names = FALSE)
    }
  )

  suggest_rv <- reactiveVal(NULL)
  observeEvent(input$suggest_is, {
    c0 <- cfg()
    out <- tryCatch({
      pl <- termite_plan(c0, probe_all = TRUE)
      termite_suggest_is(pl, c0, c0$first_blank, c0$last_blank,
                         c0$first_signal, c0$last_signal, top = 3L)
    }, error = function(e) structure(list(error = conditionMessage(e)), class = "termite_error"))
    suggest_rv(out)
  })

  output$suggest_note <- renderUI({
    s <- suggest_rv()
    if (is.null(s)) return(NULL)
    if (inherits(s, "termite_error"))
      return(div(class = "warn-box", paste("计算失败：", s$error)))
    top1 <- s[s$rank == 1, ]
    div(class = "ok-box",
        "每个文件信号最强的同位素：",
        paste(sprintf("%s = %s", names(table(top1$isotope)),
                      as.integer(table(top1$isotope))), collapse = "；"),
        br(), "若出现多种主量元素，说明这批样品基体不同，",
        "单一内标含量与单一 RSF 未必同时适用 —— 可以先用「样品名过滤」分批处理。")
  })

  output$suggest_tbl <- renderTable({
    s <- suggest_rv(); req(s)
    if (inherits(s, "termite_error")) return(NULL)
    d <- s[s$rank == 1, c("sample_name", "kind", "isotope", "net_cps", "snr")]
    d$net_cps <- signif(d$net_cps, 4); d$snr <- signif(d$snr, 3)
    names(d) <- c("样品名", "类别", "主量同位素", "净计数 cps", "信号/背景")
    d
  }, striped = TRUE, bordered = TRUE, digits = 4)

  # ---------------------------------------------------------------- 公式法校准（无内标）
  # 矿物下拉候选：来自 mineral_formulas.csv（可扩展），缺省回退内置 11 种矿物
  observe({
    defs <- termite_mineral_defs(list(dir = input$dir, app_dir = root))
    updateSelectizeInput(session, "formula_mineral",
                         choices = setNames(defs$name,
                                            sprintf("%s · %s", defs$name, defs$note)))
  })

  formula_flag <- reactiveVal(NULL)
  observeEvent(input$run_formula, formula_flag(Sys.time()), ignoreNULL = TRUE)

  formula_res <- eventReactive(formula_flag(), {
    c0 <- cfg()
    if (!identical(c0$mode, "spot"))
      return(structure(list(error = "无内标「矿物化学式归一化」校准目前只支持点分析（spot）。"),
                       class = "termite_error"))
    if (is.null(input$formula_mineral) || !nzchar(input$formula_mineral))
      return(structure(list(error = "请先在左侧选择矿物。"), class = "termite_error"))
    warn <- character(0)
    out <- withCallingHandlers(
      tryCatch(
        withProgress(message = "公式法归算中…", value = 0, {
          termite_run_formula(c0, mineral = input$formula_mineral)
        }),
        error = function(e) structure(list(error = conditionMessage(e)), class = "termite_error")
      ),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    attr(out, "warnings") <- warn
    out
  })

  formula_ok <- reactive(
    !inherits(formula_res(), "termite_error") && !is.null(formula_res()$elements))

  # 理论补的元素名（excluded + fixed）
  .formula_theory <- function(mp) {
    th <- character(0)
    if (nzchar(trimws(mp$excluded))) th <- c(th, strsplit(trimws(mp$excluded), ",")[[1]])
    fx <- .parse_fixed(mp$fixed)
    if (length(fx)) th <- c(th, names(fx))
    unique(trimws(th))
  }

  # 参考物质质量覆盖率提示：标样若没测全主量元素（玻璃常见的 Na、Al），
  # 就不能拿它做「总量归一到 100%」的检验；本方法的 lambda 逐元素回归，不受影响。
  .formula_coverage_text <- function(cov) {
    if (is.null(cov) || !length(cov)) return("")
    cov <- cov[vapply(cov, function(x) is.list(x) && is.finite(x$coverage), TRUE)]
    if (!length(cov)) return("")
    parts <- vapply(cov, function(x) {
      miss <- ""
      if (length(x$missing_wt_pct))
        miss <- sprintf("（未测：%s）", paste(
          sprintf("%s %.2f%%", names(x$missing_wt_pct), x$missing_wt_pct),
          collapse = "、"))
      sprintf("%s 覆盖率 %.0f%%（实测 %.1f / 全部 %.1f wt%%）%s",
              x$rm, x$coverage * 100, x$measured_wt_pct, x$full_wt_pct, miss)
    }, "")
    pct <- max(vapply(cov, function(x) x$coverage, 0))
    head_txt <- if (pct < 0.95)
      "标样未测全元素，不能归一到 100%（本方法不依赖总量归一，结果不受影响）："
    else
      "标样元素覆盖完整："
    paste0(head_txt, paste(parts, collapse = "；"))
  }

  output$formula_status_bar <- renderUI({
    if (is.null(formula_flag())) return(NULL)
    r <- formula_res()
    if (inherits(r, "termite_error"))
      return(div(class = "warn-box", paste("出错：", r$error)))
    div(class = "ok-box",
        sprintf("完成 · %d 样品 × %d 元素", nrow(r$samples), length(r$elements)))
  })

  output$formula_summary <- renderUI({
    if (is.null(formula_flag()))
      return(div(class = "kpi", "还没有运行。左侧选矿物后点「运行公式法校准」。"))
    r <- formula_res()
    if (inherits(r, "termite_error"))
      return(div(class = "warn-box", paste("运行出错：", r$error)))
    mp <- r$mineral
    th <- .formula_theory(mp)
    cov <- .formula_coverage_text(r$ref_coverage)
    div(class = "ok-box",
        sprintf("矿物：%s（%s，%s）· 模式 %s", mp$name, mp$note, mp$formula, mp$mode),
        br(),
        sprintf("样品 %d 个 × 元素 %d 个；按结构式理论补：%s。",
                nrow(r$samples), length(r$elements),
                if (length(th)) paste(th, collapse = ", ") else "无（全部实测）"),
        if (nzchar(cov)) list(br(), span(class = "muted", cov)))
  })

  output$formula_factor_tbl <- renderTable({
    req(formula_ok()); r <- formula_res()
    data.frame(样品 = names(r$formula_factor),
               归一化因子 = signif(r$formula_factor, 6),
               总分子量_g_mol = signif(r$M_total, 6),
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  output$formula_tbl_note <- renderUI({
    req(formula_ok()); r <- formula_res()
    d <- termite_formula_table(r)
    if (nrow(d) > 200)
      div(style = "font-size:12.5px;color:#718096",
          sprintf("共 %d 行，页面预览前 200 行；完整数据请用下方按钮下载。", nrow(d)))
    else
      div(style = "font-size:12.5px;color:#718096",
          "带 * 的元素为按结构式理论补出的值（非实测）。")
  })

  output$formula_tbl <- renderTable({
    req(formula_ok()); r <- formula_res()
    d <- termite_formula_table(r)
    th <- .formula_theory(r$mineral)
    th <- intersect(th, names(d))
    if (length(th)) names(d)[match(th, names(d))] <- paste0(th, " *")
    if (nrow(d) > 200) d <- d[seq_len(200), , drop = FALSE]
    num <- vapply(d, is.numeric, logical(1))
    d[num] <- lapply(d[num], function(x) ifelse(is.na(x), NA, signif(x, 6)))
    d
  }, striped = TRUE, bordered = TRUE, digits = 6, na = "")

  output$dl_formula <- downloadHandler(
    filename = function() sprintf("TERMITE_formula_%s_%s.csv",
                                  formula_res()$mineral$name,
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(f)
      utils::write.table(termite_formula_table(formula_res()), f,
                         sep = "\t", row.names = FALSE, na = "NA")
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
