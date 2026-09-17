# =============================================================================
#  TERMITE Shiny —— 界面
# =============================================================================

.termite_css <- "
body, .container-fluid { font-family: -apple-system, 'Segoe UI', 'Microsoft YaHei', Roboto, sans-serif; }
.navbar-brand { font-weight: 700; letter-spacing: .5px; }
details.acc { border: 1px solid #dfe3e8; border-radius: 6px; margin-bottom: 8px; background:#fbfcfd; }
details.acc > summary { cursor: pointer; padding: 7px 10px; font-weight: 600; font-size: 13px;
                        color:#2d3748; outline: none; list-style: none; }
details.acc > summary::-webkit-details-marker { display:none; }
details.acc > summary:before { content:'▸ '; color:#718096; }
details.acc[open] > summary:before { content:'▾ '; }
details.acc > div { padding: 2px 10px 10px 10px; }
.sidebar { font-size: 12.5px; }
.sidebar .form-group { margin-bottom: 6px; }
.sidebar label { font-weight: 500; margin-bottom: 2px; color:#4a5568; }
.btn-run { width:100%; font-weight:600; }
.md-h { margin: 18px 0 8px; }
.md-p { line-height: 1.75; margin: 6px 0; }
.md-ul, .md-ol { line-height: 1.75; }
.md-code { background:#f1f5f9; padding:10px 12px; border-radius:6px; font-size:12.5px;
           border-left:3px solid #94a3b8; overflow-x:auto; white-space:pre; }
.md-table { border-collapse: collapse; margin: 10px 0; font-size: 13px; }
.md-table th, .md-table td { border:1px solid #dfe3e8; padding:5px 9px; }
.md-table th { background:#f1f5f9; }
.md-quote { border-left:3px solid #cbd5e0; margin:8px 0; padding:4px 12px; color:#4a5568; }
.md-hr { border:none; border-top:1px solid #e2e8f0; margin:16px 0; }
.kpi { display:inline-block; background:#f1f5f9; border-radius:6px; padding:6px 12px;
       margin:0 6px 6px 0; font-size:12.5px; }
.kpi b { font-size:15px; color:#2b6cb0; }
.warn-box { background:#fffaf0; border-left:3px solid #dd6b20; padding:8px 12px;
            border-radius:4px; font-size:12.5px; margin:8px 0; white-space:pre-wrap; }
.ok-box { background:#f0fff4; border-left:3px solid #38a169; padding:8px 12px;
          border-radius:4px; font-size:12.5px; margin:8px 0; }
.code-out { background:#f8fafc; border:1px solid #e2e8f0; border-radius:6px;
            padding:10px 12px; font-family: Consolas, Menlo, monospace;
            font-size:12px; white-space:pre; overflow-x:auto; }
"

#' 侧栏折叠块（用原生 <details>，不引入额外依赖）
acc <- function(title, ..., open = FALSE) {
  args <- c(list(class = "acc"),
            if (isTRUE(open)) list(open = NA),
            list(tags$summary(title)),
            list(tags$div(...)))
  do.call(tags$details, args)
}

#' 侧栏数字输入（紧凑）
numin <- function(id, label, value, step = 1, min = NA, max = NA) {
  numericInput(id, label, value = value, step = step, min = min, max = max)
}

termite_ui <- function() {
  fluidPage(
    tags$head(tags$style(HTML(.termite_css))),
    titlePanel(
      div(style = "display:flex;align-items:baseline;gap:14px",
          tags$span("TERMITE · 交互式数据归算"),
          tags$span(style = "font-size:12.5px;color:#718096;font-weight:400",
                    "LA-ICP-MS 微量元素 · 点分析 / 线扫描 · 纯 base R 内核"))
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3, class = "sidebar",

        actionButton("run", "▶  运行归一算", class = "btn-run btn-primary"),
        div(style = "height:8px"),
        uiOutput("status_bar"),

        acc("① 数据与仪器", open = TRUE,
          textInput("dir", "数据主目录", "your_main_directory"),
          div(style = "font-size:11.5px;color:#718096;margin:-4px 0 6px",
              "主目录下应含 Rawdata_* / ReferenceMaterial_* / TERMITEScriptFolder"),
          radioButtons("mode", "测量模式", c("点分析 spot" = "spot", "线扫描 line" = "line"),
                       inline = TRUE),
          radioButtons("machine", "质谱", c("Agilent / 四极杆" = "Agilent",
                                            "Element2 / SF-ICP-MS" = "Element2"),
                       inline = TRUE),
          textInput("dir_sample", "样品目录（相对主目录）", "Rawdata_spotscan"),
          textInput("dir_ref", "参考物质目录（相对主目录）", "ReferenceMaterial_spotscan"),
          selectizeInput("ref_mats", "参考物质（可多个，按名称匹配文件名）",
                         choices = NULL, multiple = TRUE,
                         options = list(create = TRUE, placeholder = "例如 NIST612")),
          textInput("file_isotopes", "同位素原子量/丰度表", "TERMITEScriptFolder/AtomGewIsoAbund_NIST.csv"),
          textInput("file_standards", "参考物质推荐值表", "TERMITEScriptFolder/Standards_GeoReM.csv")
        ),

        acc("② 行列与内标", open = TRUE,
          numin("n_iso", "同位素个数", 12),
          numin("header_line", "同位素表头所在行", 3),
          numin("signal_line", "数据起始行", 4),
          numin("column_IS", "内标所在列（含时间列，从1计）", 5),
          numin("is_conc", "样品内标含量 [µg/g]", 400003.81, step = 0.01),
          textInput("resolution", "分辨率后缀（仅 Element2，如 (LR)）", "(LR)"),
          selectInput("background", "背景算法", c("中位数 median" = "median",
                                                  "平均值 mean" = "mean"))
        ),

        acc("③ 积分窗口（文件真实行号）",
          tags$div(style = "font-weight:600;color:#2d3748;margin-top:4px", "点分析 / 参考物质"),
          numin("first_blank", "空白起点", 5), numin("last_blank", "空白终点", 124),
          numin("first_signal", "信号起点", 180), numin("last_signal", "信号终点", 440),
          tags$div(style = "font-weight:600;color:#2d3748;margin-top:8px", "线扫描样品"),
          numin("first_blank_ls", "空白起点", 5), numin("last_blank_ls", "空白终点", 124),
          numin("first_signal_ls", "信号起点", 143), numin("last_signal_ls", "信号终点", 6890),
          numin("laser_speed", "激光扫描速率 [µm/s]", 5, step = 0.1)
        ),

        acc("④ 离群检验",
          checkboxInput("outlier_test", "启用离群剔除（中位数 ± m% 窗口）", TRUE),
          numin("outlier_pct", "阈值 m [%]", 30, step = 1)
        ),

        acc("⑤ 兼容与原脚本修正",
          checkboxInput("clip_negative", "净信号截断负值为 0", TRUE),
          div(style = "font-size:11.5px;color:#718096;margin:-4px 0 6px",
              "原脚本：点分析裁剪、线扫描不裁剪。裁剪会在低含量处引入正偏倚。"),
          checkboxInput("legacy_lod", "复刻原脚本的 LoD 文件选取（线扫描会漏掉首个参考文件）", FALSE),
          checkboxInput("legacy_time_axis", "复刻原脚本的线扫描 x 轴（起点偏 0.78 µm）", FALSE)
        ),

        acc("⑥ 高级",
          numericInput("n_sweeps_sample", "样品文件总行数（留空=全部）", NA),
          numericInput("n_sweeps_ref", "参考文件总行数（留空=全部）", NA),
          div(style = "font-size:11.5px;color:#718096",
              "仅当文件尾部有非数据内容时才需要填写。")
        )
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          id = "tabs", type = "pills",

          tabPanel("总览", br(),
                   uiOutput("overview_kpi"),
                   h4("本次运行的提示信息"), uiOutput("run_warnings"),
                   h4("文件清单"), verbatimTextOutput("file_list"),
                   h4("同位素表头"), tableOutput("header_tbl")),

          tabPanel("原始信号", br(),
                   fluidRow(
                     column(4, selectInput("raw_which", "看哪一组", c("参考物质" = "ref", "样品" = "sample"))),
                     column(4, uiOutput("raw_file_ui")),
                     column(4, checkboxInput("raw_log", "纵轴对数", TRUE))
                   ),
                   sliderInput("raw_rows", "显示的行范围（文件中真实行号）",
                               min = 1, max = 100, value = c(1, 100), step = 1),
                   plotOutput("raw_plot", height = "620px"),
                   h4("背景（gas blank）"), plotOutput("blank_plot", height = "300px")),

          tabPanel("校准 RSF", br(),
                   h4("RSF 逐文件 / 逐同位素"),
                   plotOutput("rsf_plot", height = "560px"),
                   h4("各参考物质回收率校核（用 RSF 反算 / 推荐值）"),
                   uiOutput("rm_check_ui"),
                   h4("汇总 RSF"), tableOutput("rsf_tbl"),
                   downloadButton("dl_rsf", "下载 RSF CSV")),

          tabPanel("检出限 LoD", br(),
                   plotOutput("lod_plot", height = "420px"),
                   h4("LoD 数值"), tableOutput("lod_tbl"),
                   downloadButton("dl_lod", "下载 LoD CSV")),

          tabPanel("结果", br(),
                   uiOutput("result_summary"),
                   plotOutput("result_plot", height = "660px"),
                   h4("结果表"),
                   uiOutput("result_tbl_note"),
                   tableOutput("result_tbl"),
                   downloadButton("dl_result", "下载结果 CSV"),
                   div(style = "height:10px"),
                   downloadButton("dl_masked_flag", "下载离群标记 CSV")),

          tabPanel("原理", br(),
                   uiOutput("principles")),

          tabPanel("验证", br(),
                   div(class = "ok-box",
                       "内核已通过与原脚本的逐位一致性测试：点分析与线扫描的浓度表、RSF 表、",
                       "LoD 表在「复刻模式」下与原始脚本输出的最大相对偏差均为 0。"),
                   p("下面可以随时在本机重跑这项检查（需要 tests/reference/ 里的基准文件）。"),
                   actionButton("verify", "重新运行一致性检查", class = "btn-primary"),
                   br(), br(),
                   verbatimTextOutput("verify_out"),
                   h4("测试方式"),
                   uiOutput("verify_doc"))
        )
      )
    )
  )
}
