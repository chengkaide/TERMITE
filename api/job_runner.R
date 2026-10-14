#!/usr/bin/env Rscript
# =============================================================================
#  TERMITE API · 单任务执行进程
#  ---------------------------------------------------------------------------
#  由 api/termite_api.R 的 .api_job_submit() 以 Rscript 方式拉起，一次只跑一个任务。
#  参数：<状态文件路径>
#
#  为什么单独起进程：httpuv 是单线程事件循环，长任务放在服务主进程里会把整个
#  服务卡死（连 /health 都无响应）。独立进程还顺带隔离风险——任务崩了，服务照常。
#
#  用法（一般不手工调用）：
#     Rscript api/job_runner.R <workdir>/jobs/<job_id>.json
# =============================================================================

options(encoding = "UTF-8")

# 中文用户名下 LC_CTYPE=C 会让 .libPaths() 解析失败（显示成 <U+51EF>），
# requireNamespace() 于是找不到包。必须在任何库操作之前把 locale 修好。
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English United States.utf8", "English_United States.utf8",
               "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.argv <- commandArgs(trailingOnly = TRUE)
if (length(.argv) < 1L) {
  cat("用法：Rscript api/job_runner.R <状态文件路径>\n")
  quit(status = 2L)
}
.jobfile <- .argv[1L]
if (!file.exists(.jobfile)) {
  cat("状态文件不存在：", .jobfile, "\n", sep = "")
  quit(status = 2L)
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

st  <- jsonlite::fromJSON(.jobfile, simplifyVector = FALSE)
.id <- st$job_id
.t0 <- Sys.time()

# 进度回调：节流到 0.4 s。内核的 progress 只在 8 个关键节点被调用，每次都写文件
# 没必要；但也不能不写——智能体靠它区分「还在跑」和「已经卡死」。
# 顺带在这里查一次取消标志，让长任务有机会主动退出。
.last <- 0
# 状态文件写不进去绝不能把计算判死：进度可以丢，结果不能丢。
# 2026-10-14 实测过：一次状态写入失败直接把子进程 halt 掉，任务永远停在 queued。
.patch <- function(patch)
  tryCatch({ .api_job_patch(.id, patch); TRUE },
           error = function(e) {
             cat("状态写入失败：", conditionMessage(e), "\n")
             FALSE
           })

prog <- function(i, n, msg) {
  now <- as.numeric(Sys.time())
  if (now - .last < 0.4 && i < n) return(invisible(NULL))
  .last <<- now
  .patch(list(progress = list(i = as.integer(i), n = as.integer(n),
                              msg = as.character(msg)[1L])))
  if (.api_job_cancelled(.id)) stop("任务已被取消")
}

.patch(list(status = "running", started = .api_now(),
            pid = as.integer(Sys.getpid()),
            progress = list(i = 0L, n = NA_integer_, msg = "开始执行")))

res <- tryCatch(
  switch(st$action,
         plan    = .api_do_plan(st$params, prog),
         run     = .api_do_run(st$params, prog),
         formula = .api_do_formula(st$params, prog),
         window  = .api_do_window(st$params, prog),
         stop("未知 action：", st$action)),
  error = function(e) e)

.elapsed <- round(as.numeric(difftime(Sys.time(), .t0, units = "secs")), 2)

if (inherits(res, "error")) {
  .patch(list(status = "error",
              error = as.character(conditionMessage(res))[1L],
              finished = .api_now(), elapsed_s = .elapsed,
              progress = list(i = 1L, n = 1L, msg = "失败")))
  quit(status = 1L)
}

# 结果表落盘失败不该把整个任务判死：summary 仍然有效，只是拿不到表。
cat_ <- tryCatch(.api_save_tables(.id, .api_or(res$tables, list())),
                 error = function(e) list())
.patch(list(status = "done",
             summary = .api_or(res$summary, list()),
             tables = cat_,
             finished = .api_now(), elapsed_s = .elapsed,
             progress = list(i = 1L, n = 1L, msg = "完成")))
