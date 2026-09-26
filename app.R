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

options(encoding = "UTF-8")   # 本机 R 的 native 编码可能是 GBK，源码里有中文注释

# ---- 定位仓库根目录并装载 R/ 下全部源码 ---------------------------------------
# 入口可能在任意工作目录启动，也可能以 Rscript --file=... 方式启动，
# 所以按「当前目录 → 脚本所在目录」依次试探含 R/termite_core.R 的那层。
# 这一段必须是入口内联的：config 源还没装载，暂时用不到 R/ 里的函数。
.app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  scr  <- sub("^--file=", "", args[grepl("^--file=", args)])
  cand <- unique(c(getwd(), if (length(scr)) dirname(normalizePath(scr[1], mustWork = FALSE))))
  hit  <- cand[vapply(cand, function(d) file.exists(file.path(d, "R", "termite_core.R")), TRUE)]
  if (!length(hit))
    stop("找不到 R/termite_core.R；请把工作目录切到仓库根目录后再启动。")
  hit[1]
})
for (.f in list.files(file.path(.app_dir, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  source(.f, encoding = "UTF-8")

shinyApp(ui = termite_ui(), server = termite_server)
