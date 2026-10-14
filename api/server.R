#!/usr/bin/env Rscript
# =============================================================================
#  TERMITE API · 服务启动入口
#  ---------------------------------------------------------------------------
#  启动：
#     Rscript api/server.R                                  # 127.0.0.1:8787
#     Rscript api/server.R --port 8788
#     Rscript api/server.R --host 0.0.0.0 --token <密钥>    # 跨机访问（须设 token）
#
#  环境变量（命令行参数优先）：
#     TERMITE_API_WORKDIR  任务状态与结果 CSV 的落点。默认系统临时目录；
#                          **C 盘紧张时务必指到数据盘**，例如：
#                            set TERMITE_API_WORKDIR=D:/termite_jobs
#     TERMITE_API_HOST / TERMITE_API_PORT / TERMITE_API_TOKEN
#
#  接口自描述：GET /capabilities —— 智能体可据此自动装配调用参数，无需读源码。
# =============================================================================

options(encoding = "UTF-8")

# 中文用户名下 LC_CTYPE=C 会让 .libPaths() 解析失败，requireNamespace("httpuv")
# 会误报「没有这个包」。必须在任何库操作之前修好 locale。
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English United States.utf8", "English_United States.utf8",
               "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.argv <- commandArgs(trailingOnly = TRUE)
.opt <- function(key, default = NULL) {
  i <- which(.argv == paste0("--", key))
  if (length(i) && length(.argv) > i[1]) return(.argv[i[1] + 1L])
  j <- which(grepl(paste0("^--", key, "="), .argv))
  if (length(j)) return(sub(paste0("^--", key, "="), "", .argv[j[1]]))
  default
}

# 本脚本在 <仓库根>/api/ 下，往上一级即含 R/termite_core.R 的那层
.root <- local({
  a <- commandArgs(trailingOnly = FALSE)
  s <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(s)) dirname(dirname(normalizePath(s[1], mustWork = FALSE))) else getwd()
})
if (!file.exists(file.path(.root, "R", "termite_core.R")))
  stop("定位不到仓库根（缺 R/termite_core.R）：", .root)
assign(".TERMITE_ROOT", .root, envir = globalenv())

for (f in list.files(file.path(.root, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  source(f, encoding = "UTF-8")
source(file.path(.root, "api", "termite_api.R"), encoding = "UTF-8")

.host  <- as.character(.opt("host",  Sys.getenv("TERMITE_API_HOST",  "127.0.0.1")))[1]
.port  <- suppressWarnings(as.integer(.opt("port", Sys.getenv("TERMITE_API_PORT", "8787"))[1]))
if (is.na(.port)) .port <- 8787L
.token <- as.character(.opt("token", Sys.getenv("TERMITE_API_TOKEN", "")))[1]

termite_api_start(.host, .port, if (nzchar(.token)) .token else NULL)
# httpuv::service() 的 timeout 单位是**毫秒**，写成 TRUE 会被当成 1 ms 立刻返回，
# 服务起来就退出（这个坑踩过一次）。用有限值循环既保持常驻，又能被 Ctrl-C 打断。
repeat httpuv::service(1000)
