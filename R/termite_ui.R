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
  navbarPage(
    title = "TERMITE · 交互式数据归算",
    id = "nav",
    header = tags$head(tags$style(HTML(.termite_css))),

    tabPanel("内标法归算",
      div(style = "font-size:12.5px;color:#718096;margin:2px 0 6px",
          "LA-ICP-MS 微量元素 · 点分析 / 线扫描 · 纯 base R 内核"),
      sidebarLayout(
        sidebarPanel(
          width = 3, class = "sidebar",

        actionButton("run", "▶  运行归一算", class = "btn-run btn-primary"),
        div(style = "height:8px"),
        uiOutput("status_bar"),

        acc("① 导入数据", open = TRUE,
          textInput("dir", "数据主目录", "your_main_directory"),
          div(style = "font-size:11.5px;color:#718096;margin:-4px 0 6px",
              "可以指向任意目录；参考表/别名表会自动在程序目录里找。"),
          radioButtons("layout", "目录组织方式",
                       c("样品与参考物质分目录" = "dir",
                         "同一目录、按文件里的样品名分类" = "flat"), selected = "dir"),
          checkboxInput("auto_detect", "自动识别格式（表头行/数据行/分隔符/同位素数）", FALSE),
          actionButton("scan", "扫描目录并预览", class = "btn-sm"),
          div(style = "height:6px"),
          textInput("file_pattern", "文件名过滤（正则）", "\\.(csv|asc|txt)$"),
          textInput("sample_filter", "样品名过滤（正则，留空=全部）", ""),
          selectizeInput("is_iso_sel", "内标同位素（扫描后自动列出）",
                         choices = NULL, multiple = FALSE,
                         options = list(placeholder = "先扫描目录，或直接填列号")),
          div(style = "font-size:11.5px;color:#718096",
              "选定后会自动把下面的「内标列号」填成对应的列号。")
        ),

        acc("② 数据与仪器",
          div(style = "font-size:11.5px;color:#718096;margin:0 0 6px",
              "分目录时：主目录下应有样品目录与参考物质目录"),
          radioButtons("mode", "测量模式", c("点分析 spot" = "spot", "线扫描 line" = "line"),
                       inline = TRUE),
          radioButtons("machine", "质谱（仅在关闭自动识别时使用）",
                       c("Agilent / 四极杆" = "Agilent", "Element2 / SF-ICP-MS" = "Element2"),
                       inline = TRUE),
          textInput("dir_sample", "样品目录（相对主目录；flat 布局留空）", "Rawdata_spotscan"),
          textInput("dir_ref", "参考物质目录（相对主目录）", "ReferenceMaterial_spotscan"),
          selectizeInput("ref_mats", "参与定标的参考物质", choices = NULL, multiple = TRUE,
                         options = list(create = TRUE, placeholder = "例如 NIST612 / SRM 610")),
          selectizeInput("qc_mats", "质量监控样品（只报回收率，不参与 RSF）",
                         choices = NULL, multiple = TRUE,
                         options = list(create = TRUE, placeholder = "例如 BIR-1G")),
          textInput("file_isotopes", "同位素原子量/丰度表", "TERMITEScriptFolder/AtomGewIsoAbund_NIST.csv"),
          textInput("file_standards", "参考物质推荐值表", "TERMITEScriptFolder/Standards_GeoReM.csv"),
          textInput("file_alias", "参考物质别名表", "TERMITEScriptFolder/ReferenceMaterial_aliases.csv"),
          textInput("file_iso_alias", "同位素别名表", "TERMITEScriptFolder/Isotope_aliases.csv")
        ),

        acc("③ 行列与内标", open = TRUE,
          numin("n_iso", "同位素个数", 12),
          numin("header_line", "同位素表头所在行", 3),
          numin("signal_line", "数据起始行", 4),
          numin("column_IS", "内标所在列（含时间列，从1计）", 5),
          numin("is_conc", "样品内标含量 [µg/g]", 400003.81, step = 0.01),
          textInput("resolution", "分辨率后缀（仅 Element2，如 (LR)）", "(LR)"),
          selectInput("background", "背景算法", c("中位数 median" = "median",
                                                  "平均值 mean" = "mean"))
        ),

        acc("④ 绘图与积分窗口",
          selectizeInput("plot_iso", "图中显示的同位素（可多选；留空 = 自动取前若干个）",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "先扫描目录或运行一次")),
          tags$div(style = "font-weight:600;color:#2d3748;margin-top:4px", "点分析 / 参考物质"),
          numin("first_blank", "空白起点", 5), numin("last_blank", "空白终点", 124),
          numin("first_signal", "信号起点", 180), numin("last_signal", "信号终点", 440),
          tags$div(style = "font-weight:600;color:#2d3748;margin-top:8px", "线扫描样品"),
          numin("first_blank_ls", "空白起点", 5), numin("last_blank_ls", "空白终点", 124),
          numin("first_signal_ls", "信号起点", 143), numin("last_signal_ls", "信号终点", 6890),
          numin("laser_speed", "激光扫描速率 [µm/s]", 5, step = 0.1)
        ),

        acc("⑤ 离群检验",
          checkboxInput("outlier_test", "启用离群剔除（中位数 ± m% 窗口）", TRUE),
          numin("outlier_pct", "阈值 m [%]", 30, step = 1)
        ),

        acc("⑥ 兼容与原脚本修正",
          checkboxInput("clip_negative", "净信号截断负值为 0", TRUE),
          div(style = "font-size:11.5px;color:#718096;margin:-4px 0 6px",
              "原脚本：点分析裁剪、线扫描不裁剪。裁剪会在低含量处引入正偏倚。"),
          checkboxInput("legacy_lod", "复刻原脚本的 LoD 文件选取（线扫描会漏掉首个参考文件）", FALSE),
          checkboxInput("legacy_time_axis", "复刻原脚本的线扫描 x 轴（起点偏 0.78 µm）", FALSE)
        ),

        acc("⑦ 高级",
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

          tabPanel("导入数据", br(),
                   div(class = "ok-box",
                       "点左侧「扫描目录并预览」：程序会逐个探测文件的格式、表头行、",
                       "数据起始行、分隔符、同位素个数，并从文件里读出样品名，",
                       "再按你填的参考物质 / 质量监控名单把文件分类。"),
                   uiOutput("import_summary"),
                   h4("文件清单与分类"), tableOutput("import_tbl"),
                   div(style = "height:6px"),
                   actionButton("apply_probe", "把探测到的参数填回左侧", class = "btn-sm"),
                   downloadButton("dl_plan", "下载分类清单 CSV"),
                   div(style = "height:16px"),
                   h4("内标怎么选：逐文件的信号最强同位素"),
                   p(style = "font-size:12.5px;color:#718096",
                     "不同基体的样品需要不同的内标（例如锡石用 Sn、白钨矿用 W）。",
                     "这里列出每个文件在信号窗内净计数最高的几个同位素，",
                     "据此判断这批样品属于哪一类。"),
                   actionButton("suggest_is", "计算主量同位素", class = "btn-sm"),
                   div(style = "height:6px"),
                   uiOutput("suggest_note"),
                   tableOutput("suggest_tbl")),

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
    ),

    tabPanel("无内标公式法",
      div(style = "font-size:12.5px;color:#718096;margin:2px 0 6px",
          "无内标「矿物化学式归一化」校准（AYCF 家族）——省去 EPMA 测内标含量"),
      sidebarLayout(
        sidebarPanel(
          width = 3, class = "sidebar",
          div(class = "ok-box",
              "数据目录、参考物质名单、积分窗口沿用「内标法归算」页的设置；",
              "如需修改，请先回上一页调整。"),
          acc("矿物", open = TRUE,
            div(style = "font-size:11.5px;color:#718096;margin:0 0 6px",
                "用矿物结构式（电荷平衡 / 固定配位阳离子）替代内标元素。"),
            selectizeInput("formula_mineral", "选择矿物",
                           choices = NULL,
                           options = list(placeholder = "请选择矿物")),
            div(style = "font-size:11.5px;color:#718096;margin:-4px 0 6px",
                "无水矿物（白钨矿/锡石/锆石）· 氟磷灰石 · 云母 · 绿柱石 · 电气石。"),
            actionButton("run_formula", "▶  运行公式法校准", class = "btn-run btn-success"),
            div(style = "height:6px"),
            uiOutput("formula_status_bar")
          )
        ),
        mainPanel(
          width = 9,
          div(class = "ok-box",
              "无内标「矿物化学式归一化」校准（AYCF 家族）：用矿物结构式替代内标，",
              "省去 EPMA。左侧选矿物后点「运行公式法校准」。",
              "测不准的元素（磷灰石的 P/F、云母的 K/OH、绿柱石的 Be、电气石的 B/Si）",
              "按结构式理论补，结果表里用 * 标出。"),
          uiOutput("formula_summary"),
          h4("归一化因子（化学式因子）"),
          p(style = "font-size:12.5px;color:#718096",
            "把未归一化的摩尔数缩放到满足矿物结构式电荷平衡的因子；",
            "越接近化学计量，该因子越稳定。"),
          tableOutput("formula_factor_tbl"),
          h4("结果表（元素浓度 µg/g）"),
          uiOutput("formula_tbl_note"),
          tableOutput("formula_tbl"),
          downloadButton("dl_formula", "下载公式法结果 CSV")
        )
      )
    )
  )
}
