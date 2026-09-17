# =============================================================================
#  TERMITE · 交互式 LA-ICP-MS 微量元素数据归算
#
#  启动：
#     shiny::runApp(".")                        # 在仓库根目录
#     shiny::runApp("path/to/TERMITE-master")   # 或指定路径
#
#  也可以直接：  Rscript -e 'shiny::runApp(".")'
#
#  目录结构：
#     R/termite_core.R    归算内核（纯 base R，无外部依赖）
#     R/termite_plots.R   绘图
#     R/termite_verify.R  与原脚本的一致性自检
#     R/termite_md.R      极简 Markdown 渲染
#     R/termite_ui.R      界面
#     R/termite_server.R  服务端
#     docs/               原理与问题清单
#     tests/              一致性测试与基准输出
#     your_main_directory/ 自带示例数据
# =============================================================================

# ---- 依赖 -------------------------------------------------------------------
if (!requireNamespace("shiny", quietly = TRUE))
  stop("需要 shiny 包：install.packages('shiny')")
library(shiny)

# ---- 定位应用目录（不依赖 setwd）---------------------------------------------
.termite_app_dir <- function() {
  if (file.exists(file.path("R", "termite_core.R"))) return(getwd())
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) {
    d <- dirname(normalizePath(f[1], mustWork = FALSE))
    if (file.exists(file.path(d, "R", "termite_core.R"))) return(d)
  }
  if (file.exists("app.R")) return(getwd())
  stop("找不到 R/termite_core.R；请把工作目录切到仓库根目录后再启动。")
}

.app_dir <- .termite_app_dir()
for (.f in list.files(file.path(.app_dir, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
  source(.f, encoding = "UTF-8")

shinyApp(ui = termite_ui(), server = termite_server)
