# =============================================================================
#  TERMITE · HTTP API —— 智能体 / 外部程序调用入口
#  ---------------------------------------------------------------------------
#  设计要点
#   1) 只依赖 httpuv + jsonlite。二者本就在 shiny 的依赖链上，无需新装任何包。
#   2) 本文件位于 api/ 而非 R/，所以 R/ 的「仅 base R + shiny」约定不被破坏：
#      不启动 API 时，Shiny 应用完全感知不到 jsonlite 的存在。
#   3) 耗时任务走「子进程 + 状态文件」队列。httpuv 是单线程事件循环，任务若在
#      主进程里跑会把整个服务卡死；改为 Rscript 起独立子进程执行，主进程只读写
#      JSON，因此天然支持并发提交与随时查询进度。
#   4) 响应一律 UTF-8 JSON；表格按 dataframe="rows" 输出，NA / NaN → null。
#   5) 默认只监听 127.0.0.1；传 token 可开启请求头鉴权。
#
#  端点速览（更详细的见 api/README.md，或运行时 GET /capabilities）
#    GET    /health                健康检查（含工作目录、队列长度）
#    GET    /capabilities          自描述：端点、参数白名单、枚举值（供智能体自动配置）
#    GET    /minerals              无内标公式法的矿物参数字典
#    POST   /jobs                  提交任务 {action, params} → job_id
#    GET    /jobs                  任务列表
#    GET    /jobs/{id}             任务状态与进度
#    GET    /jobs/{id}/result      任务结果（?format=json|csv&table=...&limit=...）
#    GET    /jobs/{id}/log         执行日志尾部（?limit=），任务卡住时用它看原因
#    DELETE /jobs/{id}             取消 / 删除任务
#    POST   /plan                  预检（等价于 /jobs + action=plan + wait=true）
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("TERMITE API 需要 jsonlite 包（通常随 shiny 的依赖链已安装）。")

.API_VERSION <- "1.0.0"


# -----------------------------------------------------------------------------
# 0. 小工具
# -----------------------------------------------------------------------------

#' 取第一个非空值（R 里 list 的 NULL 元素取不出，写 cfg$k 会直接 NULL，
#' 用这个函数避免到处写 if (is.null(x)) 兜底）
.api_or <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' 仓库根目录：server.R 会先写全局 .TERMITE_ROOT；单独 source 时退回内核的探测
.api_root <- function() {
  r <- get0(".TERMITE_ROOT", envir = globalenv(), ifnotfound = NULL)
  if (is.null(r) || !nzchar(r)) r <- .termite_find_root()
  r
}

.api_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

#' JSON 文本 → 响应对象。httpuv 的 body 用 raw 最稳：
#' 直接给 character 时，多字节中文在不同版本的字符编码处理上并不一致。
.api_json <- function(x, status = 200L) {
  txt <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                                       na = "null", digits = NA, force = TRUE,
                                       dataframe = "rows"))
  list(status  = status,
       headers = list("Content-Type" = "application/json; charset=utf-8",
                      "Access-Control-Allow-Origin" = "*",
                      "Cache-Control" = "no-store"),
       body    = charToRaw(enc2utf8(txt)))
}

.api_ok  <- function(...) .api_json(c(list(ok = TRUE),  list(...)))
.api_err <- function(msg, status = 400L, ...)
  .api_json(c(list(ok = FALSE, error = as.character(msg)[1]), list(...)), status)

#' 表格 → CSV 响应。带 BOM，Excel 直接双击打开不乱码（地质同行几乎都用 Excel）。
.api_csv <- function(df, filename = "termite.csv", bom = TRUE) {
  tmp <- tempfile(fileext = ".csv", tmpdir = .api_job_dir())
  on.exit(unlink(tmp), add = TRUE)
  utils::write.table(df, tmp, sep = ",", row.names = FALSE, na = "",
                     qmethod = "double", fileEncoding = "UTF-8")
  b <- readBin(tmp, "raw", n = file.info(tmp)$size)
  if (bom) b <- c(as.raw(c(0xEF, 0xBB, 0xBF)), b)
  list(status  = 200L,
       headers = list("Content-Type" = "text/csv; charset=utf-8",
                      "Content-Disposition" = sprintf("attachment; filename=\"%s\"", filename),
                      "Access-Control-Allow-Origin" = "*"),
       body    = b)
}

#' 解析查询串
#'
#' 不用 httpuv 的 parseQueryString：**它没有导出**（裸名调用直接
#' `could not find function`，用 httpuv::: 又要碰内部 API）。自己解析反而更可控。
#' 另外 httpuv 的 req$QUERY_STRING 实测带前导 "?"（与 Rook 规范不同），一并剥掉。
.api_query <- function(qs) {
  qs <- as.character(.api_or(qs, ""))[1]
  qs <- sub("^[?]", "", qs)
  if (!nzchar(qs)) return(list())
  out <- list()
  for (piece in strsplit(qs, "&", fixed = TRUE)[[1]]) {
    if (!nzchar(piece)) next
    k <- sub("=.*$", "", piece)
    v <- if (grepl("=", piece, fixed = TRUE)) sub("^[^=]*=", "", piece) else ""
    out[[utils::URLdecode(k)]] <- utils::URLdecode(gsub("+", " ", v, fixed = TRUE))
  }
  out
}

#' 读请求体 JSON。simplifyVector = FALSE 保持 R 的 list 语义，
#' 否则单元素数组会被自动降维成标量，参数校验时就分不清 "a" 与 ["a"]。
.api_body <- function(req) {
  raw <- tryCatch(req$rook.input$read(), error = function(e) raw(0))
  if (!length(raw)) return(list())
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!nzchar(trimws(txt))) return(list())
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

#' 抛「调用方错误」（HTTP 400）而不是「服务端错误」（500）
#'
#' 参数写错和代码崩溃必须分开报：前者调用方自己改参数就能好，
#' 后者才需要来找我。用条件类在路由出口统一映射成状态码。
.api_stop <- function(...) {
  stop(structure(class = c("api_bad_request", "error", "condition"),
                 list(message = paste0(...), call = NULL)))
}

.api_need <- function(v, name) {
  if (is.null(v) || !nzchar(as.character(v)[1]))
    .api_stop(sprintf("缺少必填参数 %s", name))
  as.character(v)[1]
}


# -----------------------------------------------------------------------------
# 1. 配置白名单
# -----------------------------------------------------------------------------
#  智能体可能传错字段名或类型。这里用**显式白名单 + 类型强制**，未列出的键
#  会被原样回报到 unknown 而不是静默塞进 cfg —— 静默吞掉拼错的参数，会让调用方
#  以为设置生效了，是最难查的一类问题。

.api_cfg_spec <- list(
  mode = "enum:spot,line", dir = "str", app_dir = "str", machine = "str",
  dir_sample = "str", dir_ref = "str", dir_results = "str",
  file_isotopes = "str", file_standards = "str",
  file_alias = "str", file_iso_alias = "str",
  layout = "enum:dir,flat",
  auto_detect = "bool", file_pattern = "str", sample_filter = "str",
  resolution = "str",
  ref_names = "chr", qc_names = "chr", ref_materials = "chr",
  n_iso = "int", header_line = "int", signal_line = "int",
  column_IS = "int", is_conc = "num",
  background = "enum:median,mean",
  first_blank = "int", last_blank = "int",
  first_signal = "int", last_signal = "int",
  first_blank_ls = "int", last_blank_ls = "int",
  first_signal_ls = "int", last_signal_ls = "int", laser_speed = "num",
  outlier_test = "bool", outlier_pct = "num",
  clip_negative = "bool", legacy_lod = "bool", legacy_time_axis = "bool",
  n_sweeps_sample = "int", n_sweeps_ref = "int"
)

.api_coerce <- function(key, ty, v) {
  ty <- .api_or(ty, "str")
  if (grepl("^enum:", ty)) {
    opts <- strsplit(sub("^enum:", "", ty), ",", fixed = TRUE)[[1]]
    v1 <- as.character(v)[1]
    if (!v1 %in% opts)
      .api_stop(sprintf("参数 %s 取值 '%s' 非法，可选：%s", key, v1, paste(opts, collapse = " / ")))
    return(v1)
  }
  if (identical(ty, "chr"))  return(as.character(unlist(v, use.names = FALSE)))
  if (identical(ty, "bool")) {
    if (is.logical(v)) return(isTRUE(v))
    return(toupper(trimws(as.character(v)[1])) %in% c("TRUE", "T", "1", "YES", "Y"))
  }
  if (identical(ty, "int")) {
    x <- suppressWarnings(as.integer(v[1]))
    if (is.na(x)) .api_stop(sprintf("参数 %s 需要整数，收到：%s", key, paste(v, collapse = " ")))
    return(x)
  }
  if (identical(ty, "num")) {
    x <- suppressWarnings(as.numeric(v[1]))
    if (is.na(x)) .api_stop(sprintf("参数 %s 需要数值，收到：%s", key, paste(v, collapse = " ")))
    return(x)
  }
  as.character(v)[1]
}

#' 参数 list → cfg。以 termite_defaults(mode) 为底，只覆盖显式给出的键。
.api_build_cfg <- function(params, base_mode = "spot") {
  params <- .api_or(params, list())
  mode <- if (!is.null(params$mode)) as.character(params$mode)[1] else base_mode
  cfg  <- termite_defaults(mode)
  unknown <- setdiff(names(params), names(cfg))
  applied <- list()
  for (k in intersect(names(params), names(cfg))) {
    if (is.null(params[[k]])) next
    cfg[[k]] <- .api_coerce(k, .api_cfg_spec[[k]], params[[k]])
    applied[[k]] <- cfg[[k]]
  }
  cfg$app_dir <- .api_root()
  list(cfg = cfg, unknown = unknown, applied = applied)
}


# -----------------------------------------------------------------------------
# 2. 任务队列（子进程 + 状态文件）
# -----------------------------------------------------------------------------
#  为什么不用 later / promises：R 是单线程，回调仍在主事件循环里跑，长任务照样
#  卡住服务。起独立进程是唯一不阻塞的做法，而且顺带换来「崩了不影响服务」。
#
#  工作目录：结果 CSV 可能几十 MB，默认放系统临时目录，可用环境变量
#  TERMITE_API_WORKDIR 指到数据盘（C 盘紧张时必设）。

.api_job_dir <- function() {
  d <- Sys.getenv("TERMITE_API_WORKDIR", "")
  if (!nzchar(d)) d <- file.path(tempdir(), "termite_api_jobs")
  if (!dir.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

.api_job_files_dir <- function() {
  d <- file.path(.api_job_dir(), "jobs")
  if (!dir.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

.api_job_path <- function(id) file.path(.api_job_files_dir(), paste0(id, ".json"))

#' 终态：到了这几个状态任务就不会再变。只有终态才值得为它写侧车 ——
#' 中间进度丢了无所谓（下一个进度点会补上），终态丢了却是致命的。
.api_job_terminal <- function(s)
  isTRUE(as.character(s)[1] %in% c("done", "error", "cancelled", "timeout"))

.api_job_final_path <- function(id)
  file.path(.api_job_files_dir(), "final", paste0(id, ".json"))

#' 读一个状态 JSON：先整读成文本再解析，并且重试。直接 fromJSON(path) 会在
#' rename 的瞬间拿到空文件或半写内容；解析失败返回 NULL 会让调用方以为任务不存在。
.api_job_read_file <- function(p) {
  if (!file.exists(p)) return(NULL)
  txt <- NULL
  for (k in seq_len(6L)) {
    txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) NULL)
    if (!is.null(txt) && length(txt)) break
    Sys.sleep(0.02)
  }
  if (is.null(txt) || !length(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

.api_job_read <- function(id) {
  p   <- .api_job_path(id)
  fin <- .api_job_final_path(id)
  st  <- .api_job_read_file(p)
  # 侧车（成因见 .api_job_write）：仅当它**比主文件新**时才生效。
  # 少了这道守卫，一条过期的 running 侧车会把主文件里的终态整个盖掉，
  # API 于是永远报「在跑」，而计算其实早就结束了 —— 2026-10-14 实测踩中：
  # 侧车写于 09:24:08 的 running，主文件 09:24:16 已经是 done + 2 张表。
  if (file.exists(fin) &&
      (!file.exists(p) || isTRUE(file.mtime(fin) > file.mtime(p)))) {
    f <- .api_job_read_file(fin)
    if (!is.null(f)) {
      if (is.null(st)) return(f)
      for (k in names(f)) st[[k]] <- f[[k]]
    }
  }
  st
}

#' 写一个状态 JSON，原地覆写（非原子，只做兜底用）。
#' 单次失败不致命：重试若干次，失败返回 FALSE，由调用方决定怎么办。
.api_job_write_file <- function(p, txt) {
  d <- dirname(p)
  if (!dir.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  for (k in seq_len(8L)) {
    if (isTRUE(suppressWarnings(tryCatch({ writeLines(txt, p, useBytes = TRUE); TRUE },
                                          error = function(e) FALSE))))
      return(TRUE)
    Sys.sleep(if (k <= 4L) 0.02 else 0.1)
  }
  FALSE
}

#' 原子写状态：先写临时文件再 rename 覆盖，避免「读到只写了一半的 JSON」。
#'
#' ★ 这个函数踩过大坑，改动前请读完：
#'   1) 目标文件被别的进程打开时（索引 / 杀软扫过），rename 会报「拒绝访问」，
#'      所以必须带重试。
#'   2) 绝不能把「写失败」变成**抛错**。旧写法在重试耗尽后让 writeLines 直接抛错，
#'      结果整个执行子进程当场 halt、任务永远停在 queued —— 而计算其实早跑完了。
#'      2026-10-14 实测废掉两个任务，日志里只有
#'      `cannot open file '.../<job>.json': Permission denied`。
#'   3) 主文件可能被**长期**占用（实测数分钟，怀疑与杀软/索引在高负载下挂住有关）。
#'      所以最终兜底不是无限重试，而是把状态写到**另一个文件名**的侧车
#'      （jobs/final/<id>.json）—— Windows 的锁是按文件名生效的，换个名字就绕开了。
#'      .api_job_read() 会把它合并进来，于是「任务已完成」这个事实永远读得到。
#'
#' 三条路依次降级：rename 覆盖（原子）→ 原地覆写 → 侧车。全过程不抛错；
#' 连侧车都写不成才 warning，且只返回 FALSE，任调用方处置。
.api_job_write <- function(st) {
  p   <- .api_job_path(st$job_id)
  txt <- as.character(jsonlite::toJSON(st, auto_unbox = TRUE, null = "null",
                                       na = "null", digits = NA, force = TRUE))
  # 临时名必须唯一：崩溃遗留的旧临时文件可能正被占用，复用同名会连带失败
  tmp <- tempfile(pattern = paste0(basename(p), "."), tmpdir = .api_job_files_dir())
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  if (isTRUE(suppressWarnings(tryCatch({ writeLines(txt, tmp, useBytes = TRUE); TRUE },
                                        error = function(e) FALSE)))) {
    for (d in c(rep(0.02, 6L), rep(0.12, 6L)))
      if (isTRUE(suppressWarnings(file.rename(tmp, p)))) return(invisible(p))
  }

  # 退路一：原地覆写。本机长期靠它工作，平时是通的。
  if (.api_job_write_file(p, txt)) return(invisible(p))

  # 退路二：侧车，且**只对终态**启用。主文件被长期占用时，至少让收尾状态
  # 能被读到；中间进度不写侧车，免得留下过期状态去盖主文件。
  if (.api_job_terminal(st$status) &&
      .api_job_write_file(.api_job_final_path(st$job_id), txt)) {
    warning("任务状态主文件被占用，已写入终态侧车：", st$job_id)
    return(invisible(FALSE))
  }
  warning("任务状态写入失败：", st$job_id)
  invisible(FALSE)
}

.api_job_new <- function(id, action, params) {
  list(job_id = id, action = action, status = "queued",
       created = .api_now(), started = NA_character_, finished = NA_character_,
       elapsed_s = NA_real_, pid = NA_integer_,
       progress = list(i = 0L, n = NA_integer_, msg = "已提交，正在启动执行进程"),
       params = params, error = NA_character_, warnings = list(),
       summary = list(), tables = list())
}

#' 读-改-写状态。子进程用它在执行途中上报进度；
#' 顺带回传「是否已被取消」，让长任务有机会主动退出。
.api_job_patch <- function(id, patch) {
  st <- .api_job_read(id)
  if (is.null(st)) return(invisible(FALSE))
  # 已取消的任务：后续进度回调不许把状态改回 running/done，
  # 否则「取消」按钮会在任务收尾时被自己抹掉。
  if (identical(st$status, "cancelled")) patch$status <- NULL
  for (k in names(patch)) st[[k]] <- patch[[k]]
  .api_job_write(st)
  invisible(TRUE)
}

.api_job_cancelled <- function(id) {
  st <- .api_job_read(id)
  !is.null(st) && identical(st$status, "cancelled")
}

#' 提交任务：写状态 → 起 Rscript 子进程 → 立刻返回
.api_job_submit <- function(action, params) {
  if (!action %in% c("plan", "run", "formula", "window"))
    .api_stop("action 必须是 plan / run / formula / window 之一，收到：", action)
  id <- paste0(action, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-",
               paste(sample(c(letters, 0:9), 6L, TRUE), collapse = ""))
  .api_job_write(.api_job_new(id, action, params))

  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  runner  <- file.path(.api_root(), "api", "job_runner.R")
  if (!file.exists(runner)) stop("找不到任务执行器：", runner)
  logf <- file.path(.api_job_files_dir(), paste0(id, ".log"))
  system2(rscript, args = shQuote(c(runner, .api_job_path(id))),
          wait = FALSE, stdout = logf, stderr = logf)
  .api_job_read(id)
}

.api_job_list <- function() {
  fs <- list.files(.api_job_files_dir(), pattern = "[.]json$", full.names = TRUE)
  if (!length(fs)) return(list())
  sts <- lapply(fs, function(f) tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                                         error = function(e) NULL))
  sts <- sts[!vapply(sts, is.null, logical(1))]
  sts[order(vapply(sts, function(s) .api_or(s$created, ""), ""), decreasing = TRUE)]
}

#' 一个任务的「简要视图」：列表端点不该把 summary/tables 全吐出来
.api_job_brief <- function(st) {
  out <- list(job_id = st$job_id, action = st$action, status = st$status,
              created = st$created, finished = st$finished,
              elapsed_s = st$elapsed_s, progress = st$progress,
              error = st$error,
              n_tables = length(st$tables))
  # 未结束的任务补一个「已等待多少秒」。子进程冷启动要 15–25 s，智能体靠这个
  # 数字区分「还在起」和「真的卡住了」——否则只能干等或暴力超时。
  if (!isTRUE(st$status %in% c("done", "error", "cancelled", "timeout"))) {
    t0 <- suppressWarnings(as.POSIXct(.api_or(st$created, ""),
                                      format = "%Y-%m-%dT%H:%M:%S%z"))
    if (length(t0) == 1L && !is.na(t0))
      out$waiting_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1L)
  }
  out
}

#' 等待任务结束（供 wait=true 与 /plan 用）。超时返回最后状态，不抛错。
.api_job_wait <- function(id, timeout_s = 120) {
  t0 <- Sys.time()
  repeat {
    st <- .api_job_read(id)
    if (is.null(st)) return(NULL)
    if (st$status %in% c("done", "error", "cancelled", "timeout")) return(st)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_s)
      return(.api_job_read(id))
    Sys.sleep(0.15)
  }
}


# -----------------------------------------------------------------------------
# 3. 序列化
# -----------------------------------------------------------------------------

#' 表格 → list of rows，超过 limit 行就截断（线扫描动辄上万行，别把响应撑爆）
.api_rows <- function(df, limit = NULL) {
  if (is.null(df)) return(list())
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(limit) && length(limit) == 1L && is.finite(limit) &&
      limit > 0 && nrow(df) > limit)
    df <- df[seq_len(as.integer(limit)), , drop = FALSE]
  df
}

#' 把配置里真正影响数值的参数回抄一遍。
#' 智能体经常需要「这条结果的窗口/内标是什么」才能判断可比性，缺了它结果没法解释。
.api_cfg_echo <- function(cfg) {
  keys <- c("mode", "dir", "layout", "auto_detect", "machine",
            "ref_names", "qc_names", "ref_materials",
            "column_IS", "is_conc", "background",
            "first_blank", "last_blank", "first_signal", "last_signal",
            "first_blank_ls", "last_blank_ls", "first_signal_ls", "last_signal_ls",
            "laser_speed", "outlier_test", "outlier_pct",
            "clip_negative", "legacy_lod", "legacy_time_axis",
            "sample_filter", "n_iso", "header_line", "signal_line")
  out <- list()
  for (k in keys) {
    v <- cfg[[k]]
    if (is.null(v)) next
    if (length(v) == 1L && is.atomic(v) && !is.na(v) && !nzchar(as.character(v)[1]) &&
        is.character(v)) next          # 空字符串不回报
    out[[k]] <- if (is.factor(v)) as.character(v) else v
  }
  out
}

.api_warn_capture <- function(expr, warn_env) {
  withCallingHandlers(expr, warning = function(w) {
    warn_env$w <- c(warn_env$w, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}


#' 提交前的快速参数校验
#'
#' 只构造一次 cfg（不读任何数据文件，毫秒级），把「参数名拼错 / 取值非法」
#' 拦在**提交阶段**。对智能体来说，静默忽略一个写错的参数比直接报错危险得多 ——
#' 它会让调用方以为设置生效了，最后拿着一份用默认参数跑出来的结果做判断。
.api_precheck <- function(action, params) {
  params <- .api_or(params, list())
  extra <- switch(action,
                  plan    = c("suggest_is", "top"),
                  formula = "mineral",
                  window  = c("chs", "header_line", "signal_line", "sep", "pattern", "ch"),
                  character(0))
  if (action %in% c("plan", "run", "formula")) {
    chk <- .api_build_cfg(params[setdiff(names(params), extra)])
    if (length(chk$unknown))
      .api_stop(sprintf("不认识这些参数：%s（可用参数见 GET /capabilities 的 config_keys）",
                        paste(chk$unknown, collapse = ", ")))
  }
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# 3b. 结果表落盘 / 回读
# -----------------------------------------------------------------------------
#  表数据**不写进状态 JSON**：线扫描结果上万行，塞进状态文件会让每次进度回报
#  都重写几 MB。状态里只留「表目录」（名字、行列数、列名、文件名），数据走 CSV。

.api_table_path <- function(st, name)
  file.path(.api_job_files_dir(), sprintf("%s.%s.csv", st$job_id, name))

.api_save_tables <- function(id, tabs) {
  cat_ <- list()
  for (nm in names(tabs)) {
    df <- tabs[[nm]]
    if (is.null(df)) next
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(df)) next
    p <- file.path(.api_job_files_dir(), sprintf("%s.%s.csv", id, nm))
    utils::write.table(df, p, sep = ",", row.names = FALSE, na = "",
                       qmethod = "double", fileEncoding = "UTF-8")
    cat_[[nm]] <- list(name = nm, rows = nrow(df), cols = ncol(df),
                       columns = names(df), file = basename(p))
  }
  cat_
}

.api_load_table <- function(st, name, limit = NULL) {
  p <- .api_table_path(st, name)
  if (!file.exists(p)) return(NULL)
  df <- utils::read.csv(p, check.names = FALSE, fileEncoding = "UTF-8",
                        stringsAsFactors = FALSE)
  .api_rows(df, limit)
}


# -----------------------------------------------------------------------------
# 4. 四个任务的执行体（在**子进程**里被 job_runner.R 调用）
# -----------------------------------------------------------------------------

#' 预检：文件清单 + 表头口径 + 内标建议。不跑归算，只回答「这批数据长什么样」
.api_do_plan <- function(params, prog) {
  params <- .api_or(params, list())
  # 这两个是 plan 的指令参数，不是 cfg 键，先摘出来再建 cfg
  do_suggest <- !identical(params$suggest_is, FALSE)
  top        <- as.integer(.api_or(params$top, 3L))
  params$suggest_is <- NULL; params$top <- NULL

  dir <- .api_need(params$dir, "dir")
  bc  <- .api_build_cfg(params)
  cfg <- bc$cfg; cfg$dir <- dir
  if (!dir.exists(dir)) stop("数据目录不存在：", dir)

  prog(1, 4, "枚举数据文件")
  plan <- termite_plan(cfg)
  if (!nrow(plan)) stop("没有找到任何数据文件；请检查 dir / layout / file_pattern")

  prog(2, 4, "探测表头与数据起始行")
  first_path <- if (any(plan$kind == "sample")) plan$path[plan$kind == "sample"][1] else plan$path[1]
  pr <- tryCatch(termite_probe(first_path), error = function(e) NULL)

  sug <- NULL
  if (do_suggest) {
    prog(3, 4, "评估各文件信号最强的同位素（用于选内标）")
    sug <- tryCatch(termite_suggest_is(plan, cfg, cfg$first_blank, cfg$last_blank,
                                       cfg$first_signal, cfg$last_signal, top = top),
                    error = function(e) NULL)
  }
  prog(4, 4, "完成")

  list(
    summary = list(
      kind = "plan", dir = dir,
      n_files = nrow(plan),
      n_sample = sum(plan$kind == "sample"),
      n_ref    = sum(plan$kind == "ref"),
      n_qc     = sum(plan$kind == "qc"),
      ref_materials = unique(plan$material[plan$kind == "ref"]),
      qc_materials  = unique(plan$material[plan$kind == "qc"]),
      header = if (is.null(pr)) list() else list(
        sampled_file = pr$file, format = pr$format, sep = pr$sep,
        header_line = pr$header_line, signal_line = pr$signal_line,
        n_iso = pr$n_iso, isotopes = pr$isotopes_raw,
        isotope_style = pr$isotope_style,
        sample_name = pr$sample_name, dwell = pr$dwell, notes = pr$notes),
      line_convention = sprintf(
        "矩阵第 r 行 = 文件第 r + (signal_line - 1) 行%s；接口一律用文件真实行号",
        if (is.null(pr)) "" else sprintf("（本批 signal_line = %d）", pr$signal_line)),
      config = .api_cfg_echo(cfg),
      unknown_params = bc$unknown,
      warnings = list()),
    tables = list(
      files = plan[, c("file", "kind", "material", "sample_name",
                       "format", "n_iso", "ok", "msg"), drop = FALSE],
      main_isotope = sug))
}

#' 内标法归算
.api_do_run <- function(params, prog) {
  bc  <- .api_build_cfg(params, "spot")
  cfg <- bc$cfg
  cfg$dir <- .api_need(params$dir, "dir")
  if (!dir.exists(cfg$dir)) stop("数据目录不存在：", cfg$dir)

  w <- new.env(); w$w <- character(0)
  t0 <- Sys.time()
  res <- .api_warn_capture(termite_run(cfg, progress = prog), w)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  tabs <- list(results  = termite_table(res),
               lod      = termite_lod_table(res),
               rsf      = termite_rsf_table(res),
               recovery = termite_recovery(res, "all"))
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]

  list(
    summary = list(
      kind = "run", mode = res$mode, elapsed_s = round(elapsed, 2),
      n_isotopes = length(res$isotopes),
      isotopes = res$isotopes, elements = res$elements,
      is_isotope = res$is_iso, is_column = cfg$column_IS, is_conc = cfg$is_conc,
      n_sample = length(res$files$sample),
      n_ref = length(res$files$ref), n_qc = length(res$files$qc),
      ref_materials = unique(res$files$ref_rm),
      qc_materials  = unique(res$files$qc_rm),
      config = .api_cfg_echo(cfg),
      unknown_params = bc$unknown,
      warnings = unique(c(w$w, .api_or(res$warnings, character(0))))),
    tables = tabs)
}

#' 无内标公式法（矿物化学式归一化）
.api_do_formula <- function(params, prog) {
  params <- .api_or(params, list())
  mineral <- .api_need(params$mineral,
                       "mineral（可用矿物见 GET /minerals）")
  bc  <- .api_build_cfg(params, "spot")
  cfg <- bc$cfg
  cfg$dir <- .api_need(params$dir, "dir")
  if (!dir.exists(cfg$dir)) stop("数据目录不存在：", cfg$dir)

  w <- new.env(); w$w <- character(0)
  t0 <- Sys.time()
  res <- .api_warn_capture(termite_run_formula(cfg, mineral), w)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  cov <- res$ref_coverage
  cov_df <- if (length(cov)) do.call(rbind, lapply(names(cov), function(nm) {
    x <- cov[[nm]]
    if (is.null(x)) return(NULL)
    miss <- as.numeric(x$missing_wt_pct)
    # missing_wt_pct 在内核里是**向量**（各缺失元素的质量百分比，最多 6 个、
    # 降序、<0.01 wt% 已剔除）。直接塞进 data.frame 会与标量列 recycle 出
    # 多行假数据 —— 实测 NIST610 因此被撑成 5 行重复。压成一格字符串，
    # 「一行一材料」的语义才成立，信息也不丢。
    data.frame(material = nm,
               full_wt_pct     = as.numeric(x$full_wt_pct)[1],
               measured_wt_pct = as.numeric(x$measured_wt_pct)[1],
               coverage        = as.numeric(x$coverage)[1],
               n_missing       = length(miss),
               missing_wt_pct  = if (length(miss)) paste(sprintf("%.4f", miss),
                                                         collapse = "; ") else "",
               stringsAsFactors = FALSE)
  })) else NULL

  mp <- res$mineral
  list(
    summary = list(
      kind = "formula", elapsed_s = round(elapsed, 2),
      mineral = as.character(mp$name), mineral_mode = as.character(mp$mode),
      n_sample = nrow(res$samples),
      is_isotope = NA_character_,       # 公式法不用内标，此字段恒为空
      elements = res$elements, isotopes = res$isotopes,
      formula_factor = as.numeric(res$formula_factor),
      M_total = as.numeric(res$M_total),
      config = .api_cfg_echo(cfg),
      unknown_params = bc$unknown,
      warnings = unique(c(w$w,
        if (!is.null(cov_df) && any(cov_df$coverage < 0.95))
          sprintf("参考物质 %s 的质量覆盖率 %.1f%% < 95%%，不建议拿它做总量检验",
                  paste(cov_df$material[cov_df$coverage < 0.95], collapse = ", "),
                  100 * min(cov_df$coverage)) else character(0)))),
    tables = list(results = termite_formula_table(res), coverage = cov_df))
}

#' 区间窗口扫描 + 自动定窗（给「微量元素处理 · 区间选择」页做无界面调用）
.api_do_window <- function(params, prog) {
  params <- .api_or(params, list())
  dir <- .api_need(params$dir, "dir")
  chs <- as.character(.api_or(params$chs, "29Si"))[1]
  hl  <- as.integer(.api_or(params$header_line, 14L))
  sl  <- as.integer(.api_or(params$signal_line, 16L))
  sep <- as.character(.api_or(params$sep, ","))[1]
  pat <- as.character(.api_or(params$pattern, "[.]csv$"))[1]
  ch  <- if (is.null(params$ch)) NULL else as.character(params$ch)[1]
  if (!dir.exists(dir)) stop("目录不存在：", dir)

  prog(1, 3, "读取逐点信号曲线")
  pts <- window_scan_dir(dir, chs = chs, header_line = hl, signal_line = sl,
                         pattern = pat, sep = sep)
  prog(2, 3, sprintf("自动定窗（%d 个测点）", length(pts)))
  auto <- window_auto_batch(pts, ch = ch, signal_line = sl)
  fold <- window_fold(auto$信号窗起_文件行, auto$信号窗止_文件行)
  prog(3, 3, "完成")

  ch_used <- if (is.null(ch)) colnames(pts[[1L]]$y)[1L] else ch
  list(
    summary = list(
      kind = "window", dir = dir, n_points = length(pts), channel = ch_used,
      header_line = hl, signal_line = sl,
      line_convention = sprintf("矩阵第 r 行 = 文件第 r + %d 行；窗口起止一律为文件真实行号",
                                sl - 1L),
      n_normal = sum(auto$判定 == "正常"),
      suggested = list(
        strict = list(first_signal = as.integer(fold$信号窗起[1L]),
                      last_signal  = as.integer(fold$信号窗止[1L])),
        robust = list(first_signal = as.integer(round(fold$信号窗起[2L])),
                      last_signal  = as.integer(round(fold$信号窗止[2L]))),
        note = paste0("以上数值已是文件行口径，可直接填进 run 任务的 ",
                      "first_signal / last_signal。改窗口会改变数值，",
                      "请在确认后重算。")),
      config = list(chs = chs, sep = sep, pattern = pat, ch = ch_used),
      warnings = list()),
    tables = list(windows = auto, fold = fold))
}


# -----------------------------------------------------------------------------
# 5. 端点实现
# -----------------------------------------------------------------------------

.api_health <- function() {
  js   <- .api_job_list()
  t0   <- .api_or(get0(".TERMITE_API_T0", envir = globalenv(), ifnotfound = NULL), Sys.time())
  .api_ok(service = "TERMITE API", version = .API_VERSION,
          r_version = R.version.string,
          root = .api_root(), workdir = .api_job_dir(),
          n_jobs = length(js),
          n_running = sum(vapply(js, function(s) identical(s$status, "running"), TRUE)),
          uptime_s = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
          time = .api_now())
}

#' 自描述。智能体接进来第一件事应该是读这个：它给出了全部端点、参数白名单、
#' 枚举取值与矿物清单，不必去猜或读源码。
.api_capabilities <- function() {
  cfg0 <- termite_defaults("spot"); cfg0$app_dir <- .api_root()
  defs <- tryCatch(termite_mineral_defs(cfg0), error = function(e) NULL)
  .api_ok(
    service = "TERMITE API", version = .API_VERSION,
    description = "LA-ICP-MS 微量元素数据归算：内标法 / 无内标公式法 / 区间窗口定窗 / 预检",
    workflow_hint = paste(
      "1) GET /capabilities 了解参数；2) POST /jobs {action:'plan', params:{dir,...}} 预检；",
      "3) POST /jobs 提交 run/formula/window；4) 轮询 GET /jobs/{id}；",
      "5) GET /jobs/{id}/result 取结果（json，或 ?format=csv&table=results 下载 CSV）"),
    endpoints = list(
      list(method = "GET",    path = "/health",           blocking = FALSE, desc = "健康检查"),
      list(method = "GET",    path = "/capabilities",     blocking = FALSE, desc = "本自描述文档"),
      list(method = "GET",    path = "/minerals",         blocking = FALSE, desc = "公式法矿物参数字典"),
      list(method = "POST",   path = "/plan",             blocking = TRUE,  desc = "预检（同步，等价 action=plan + wait）"),
      list(method = "POST",   path = "/jobs",             blocking = FALSE, desc = "提交任务 {action, params, wait?}"),
      list(method = "GET",    path = "/jobs",             blocking = FALSE, desc = "任务列表"),
      list(method = "GET",    path = "/jobs/{id}",        blocking = FALSE, desc = "任务状态与进度"),
      list(method = "GET",    path = "/jobs/{id}/result", blocking = FALSE, desc = "结果 ?format=json|csv&table=&limit="),
      list(method = "GET",    path = "/jobs/{id}/log",    blocking = FALSE, desc = "执行日志尾部（?limit=），诊断卡住的任务"),
      list(method = "DELETE", path = "/jobs/{id}",        blocking = FALSE, desc = "取消 / 删除任务")),
    actions = list(
      plan = list(required = list("dir"),
                  optional = c(names(.api_cfg_spec), "suggest_is", "top"),
                  desc = "不出结果，只报文件清单、表头口径与内标建议",
                  tables = "files, main_isotope"),
      run = list(required = list("dir"),
                 optional = names(.api_cfg_spec),
                 desc = "内标法归算",
                 tables = "results, lod, rsf, recovery"),
      formula = list(required = list("dir", "mineral"),
                     optional = c(names(.api_cfg_spec), "mineral"),
                     desc = "无内标公式法（矿物化学式归一化），只支持 mode=spot",
                     tables = "results, coverage"),
      window = list(required = list("dir"),
                    optional = list("chs", "header_line", "signal_line", "sep", "pattern", "ch"),
                    desc = "逐点信号曲线扫描 + 自动定窗；只给建议，不改任何数值",
                    tables = "windows, fold")),
    config_keys = .api_cfg_spec,
    enums = list(layout = c("dir", "flat"), mode = c("spot", "line"),
                 background = c("median", "mean"),
                 machine = c("Agilent", "Element2", "Qtegra", "generic")),
    minerals = if (is.null(defs)) character(0) else as.character(defs$name),
    job_status = c("queued", "running", "done", "error", "cancelled", "timeout"),
    notes = list(
      "积分窗口一律用**文件真实行号**：矩阵第 r 行 = 文件第 r + (signal_line - 1) 行",
      "flat 布局必须给 ref_names（进 RSF 的参考物质）；qc_names 只报回收率、绝不进 RSF",
      "auto_detect=true 时表头行/数据行/分隔符按内容自动识别，无需手工填 header_line/signal_line",
      "run/formula 的耗时随样品数增长，请用 /jobs 异步提交后轮询，不要设 wait=true 干等",
      "结果 CSV 带 UTF-8 BOM，Excel 双击直接打开不乱码"))
}

.api_minerals <- function() {
  cfg0 <- termite_defaults("spot"); cfg0$app_dir <- .api_root()
  defs <- termite_mineral_defs(cfg0)
  .api_ok(n = nrow(defs), minerals = defs)
}

.api_job_detail <- function(st) {
  if (is.null(st)) return(.api_err("任务不存在", 404L))
  live <- isTRUE(st$status %in% c("queued", "running"))
  br   <- .api_job_brief(st)
  .api_ok(job = br,
          summary = .api_or(st$summary, list()),
          table_catalog = unname(lapply(.api_or(st$tables, list()),
                                 function(t) t[c("name", "rows", "cols", "columns")])),
          next_steps = if (identical(st$status, "done"))
            list(result = sprintf("GET /jobs/%s/result", st$job_id))
          else if (live)
            list(log    = sprintf("GET /jobs/%s/log", st$job_id),
                 cancel = sprintf("DELETE /jobs/%s", st$job_id))
          else list(),
          # 子进程冷启动 15–25 s，超过 120 s 还没有结果就该去看日志了
          note = if (live && isTRUE(.api_or(br$waiting_s, 0) > 120))
            sprintf("已等待 %.0f s 仍未出结果：用 GET /jobs/%s/log 看执行进程的输出",
                    .api_or(br$waiting_s, 0), st$job_id) else NULL)
}

#' 执行进程的日志尾部。这是排查「任务卡住」的唯一现场：
#' 子进程若在启动阶段就崩了（写状态失败、找不到 R 包……），状态文件会永远停在
#' queued，只有日志里留着原因。
.api_job_log <- function(id, limit = 200L) {
  p <- file.path(.api_job_files_dir(), paste0(id, ".log"))
  if (!file.exists(p)) return(.api_err(paste0("该任务没有日志：", id), 404L))
  ln <- tryCatch(readLines(p, warn = FALSE), error = function(e) character(0))
  n  <- length(ln)
  if (!is.null(limit) && length(limit) == 1L && is.finite(limit) &&
      limit > 0 && n > limit)
    ln <- ln[seq.int(n - as.integer(limit) + 1L, n)]
  .api_ok(job_id = id, n_lines_total = n, n_lines_returned = length(ln),
          lines = unname(ln))
}

.api_jobs_post <- function(req) {
  body   <- .api_body(req)
  action <- as.character(.api_or(body$action, ""))[1]
  params <- .api_or(body$params, list())
  if (!nzchar(action)) .api_stop("缺少 action（plan / run / formula / window）")
  # 先做同步校验：参数写错要立刻报错，别让用户等子进程起来才发现
  if (action %in% c("plan", "run", "formula", "window")) .api_need(params$dir, "params.dir")
  if (identical(action, "formula")) .api_need(params$mineral, "params.mineral")
  .api_precheck(action, params)

  st <- .api_job_submit(action, params)
  if (isTRUE(body$wait)) {
    to <- as.numeric(.api_or(body$timeout_s, 120))
    st <- .api_job_wait(st$job_id, to)
    return(.api_job_result_payload(st, as.integer(.api_or(body$limit, 200L))))
  }
  .api_ok(job = .api_job_brief(st),
          next_steps = list(poll   = sprintf("GET /jobs/%s", st$job_id),
                      result = sprintf("GET /jobs/%s/result", st$job_id)))
}

#' 组装「状态 + 结果表」的响应（结果表按 limit 截断）
.api_job_result_payload <- function(st, limit = 200L) {
  if (is.null(st)) return(.api_err("任务不存在", 404L))
  if (identical(st$status, "error"))
    return(.api_err(st$error, 409L, job = .api_job_brief(st)))
  if (!identical(st$status, "done"))
    return(.api_json(list(ok = FALSE,
                          error = sprintf("任务尚未完成（当前状态 %s）", st$status),
                          job = .api_job_brief(st)), 202L))
  tabs <- lapply(names(.api_or(st$tables, list())),
                 function(nm) .api_load_table(st, nm, limit))
  names(tabs) <- names(st$tables)
  .api_ok(job = .api_job_brief(st), summary = .api_or(st$summary, list()),
          tables = tabs,
          table_catalog = unname(lapply(st$tables,
                                 function(t) t[c("name", "rows", "cols", "columns")])),
          limit = limit)
}

.api_job_result <- function(id, q) {
  st <- .api_job_read(id)
  if (is.null(st)) return(.api_err(paste0("任务不存在：", id), 404L))
  if (identical(st$status, "error"))
    return(.api_err(st$error, 409L, job = .api_job_brief(st)))
  if (!identical(st$status, "done"))
    return(.api_json(list(ok = FALSE,
                          error = sprintf("任务尚未完成（当前状态 %s）", st$status),
                          job = .api_job_brief(st)), 202L))

  fmt  <- tolower(as.character(.api_or(q$format, "json"))[1])
  name <- if (!is.null(q$table)) as.character(q$table)[1] else NULL
  if (identical(fmt, "csv")) {
    if (is.null(name)) name <- names(.api_or(st$tables, list()))[1]
    df <- .api_load_table(st, name, NULL)
    if (is.null(df)) return(.api_err(paste0("没有这张结果表：", name), 404L,
                                     tables = names(st$tables)))
    return(.api_csv(df, sprintf("%s_%s.csv", st$job_id, name)))
  }
  if (!is.null(name)) {
    df <- .api_load_table(st, name, as.integer(.api_or(q$limit, 200L)))
    if (is.null(df)) return(.api_err(paste0("没有这张结果表：", name), 404L,
                                     tables = names(st$tables)))
    return(.api_ok(job = .api_job_brief(st), table = name, rows = df))
  }
  .api_job_result_payload(st, as.integer(.api_or(q$limit, 200L)))
}

.api_job_delete <- function(id) {
  st <- .api_job_read(id)
  if (is.null(st)) return(.api_err(paste0("任务不存在：", id), 404L))
  if (st$status %in% c("queued", "running")) {
    st$status <- "cancelled"; st$finished <- .api_now()
    .api_job_write(st)
    return(.api_ok(job = .api_job_brief(.api_job_read(id)),
                   note = "已标记取消；执行中的任务会在下一个进度点自行退出"))
  }
  for (nm in names(.api_or(st$tables, list()))) {
    f <- .api_table_path(st, nm)
    if (file.exists(f)) unlink(f)
  }
  for (f in c(.api_job_path(id), .api_job_final_path(id),
              file.path(.api_job_files_dir(), paste0(id, ".log"))))
  .api_ok(deleted = id)
}


# -----------------------------------------------------------------------------
# 6. 路由
# -----------------------------------------------------------------------------

.api_id_ok <- function(id) grepl("^[A-Za-z0-9._-]+$", id)

.api_route <- function(req) {
  m <- as.character(.api_or(req$REQUEST_METHOD, "GET"))[1]
  p <- as.character(.api_or(req$PATH_INFO, "/"))[1]
  if (!nzchar(p)) p <- "/"
  p <- sub("/+$", "", p); if (!nzchar(p)) p <- "/"
  q <- .api_query(req$QUERY_STRING)

  tok <- get0(".TERMITE_API_TOKEN", envir = globalenv(), ifnotfound = NULL)
  if (!is.null(tok) && nzchar(tok) &&
      !identical(as.character(.api_or(req$HTTP_X_AUTH_TOKEN, ""))[1], as.character(tok)))
    return(.api_err("鉴权失败：请在请求头携带 X-Auth-Token", 401L))

  if (identical(m, "OPTIONS"))
    return(list(status = 204L,
                headers = list("Access-Control-Allow-Origin" = "*",
                               "Access-Control-Allow-Methods" = "GET,POST,DELETE,OPTIONS",
                               "Access-Control-Allow-Headers" = "Content-Type,X-Auth-Token"),
                body = raw(0)))

  # 任务 id 白名单校验：挡住 ../ 之类的路径穿越
  hit <- regmatches(p, regexec("^/jobs/([^/]+)", p))[[1]]
  if (length(hit) == 2L && !.api_id_ok(hit[2]))
    return(.api_err("非法任务 id", 400L))

  if (identical(m, "GET")) {
    if (identical(p, "/") || identical(p, "/index.html"))
      return(.api_ok(service = "TERMITE API", version = .API_VERSION,
                     hint = "端点自描述见 GET /capabilities"))
    if (identical(p, "/health"))       return(.api_health())
    if (identical(p, "/capabilities")) return(.api_capabilities())
    if (identical(p, "/minerals"))     return(.api_minerals())
    if (identical(p, "/jobs")) {
      js <- .api_job_list()
      return(.api_ok(n = length(js), jobs = lapply(js, .api_job_brief)))
    }
    r <- regmatches(p, regexec("^/jobs/([^/]+)/log$", p))[[1]]
    if (length(r) == 2L) return(.api_job_log(r[2], as.integer(.api_or(q$limit, 200L))))
    r <- regmatches(p, regexec("^/jobs/([^/]+)/result$", p))[[1]]
    if (length(r) == 2L) return(.api_job_result(r[2], q))
    r <- regmatches(p, regexec("^/jobs/([^/]+)$", p))[[1]]
    if (length(r) == 2L) return(.api_job_detail(.api_job_read(r[2])))
  }
  if (identical(m, "POST")) {
    if (identical(p, "/jobs")) return(.api_jobs_post(req))
    if (identical(p, "/plan")) {
      body <- .api_body(req)
      params <- if (!is.null(body$params)) body$params else body
      params$dir <- .api_need(params$dir, "dir")
      .api_precheck("plan", params)
      st <- .api_job_submit("plan", params)
      st <- .api_job_wait(st$job_id, as.numeric(.api_or(body$timeout_s, 90)))
      return(.api_job_result_payload(st, as.integer(.api_or(body$limit, 500L))))
    }
  }
  if (identical(m, "DELETE")) {
    r <- regmatches(p, regexec("^/jobs/([^/]+)$", p))[[1]]
    if (length(r) == 2L) return(.api_job_delete(r[2]))
  }
  .api_err(sprintf("未知端点：%s %s", m, p), 404L)
}


# -----------------------------------------------------------------------------
# 7. 启动 / 停止
# -----------------------------------------------------------------------------

#' 启动 HTTP 服务（阻塞式；Rscript 直接跑到底即可）
#' @param host "127.0.0.1" 只允许本机；要跨机访问再改成 "0.0.0.0"，并务必设 token
#' @param port 端口
#' @param token 非空则要求请求头 X-Auth-Token
termite_api_start <- function(host = "127.0.0.1", port = 8787L, token = NULL) {
  if (!requireNamespace("httpuv", quietly = TRUE))
    stop("TERMITE API 需要 httpuv 包（通常随 shiny 的依赖链已安装）。")
  if (!is.null(token) && nzchar(token))
    assign(".TERMITE_API_TOKEN", as.character(token)[1], envir = globalenv())
  assign(".TERMITE_API_T0", Sys.time(), envir = globalenv())
  .api_job_dir()                                   # 提前建好目录

  srv <- httpuv::startServer(host, as.integer(port), list(
    call = function(req) tryCatch(
      .api_route(req),
      error = function(e) {
        if (inherits(e, "api_bad_request")) .api_err(conditionMessage(e), 400L)
        else .api_err(conditionMessage(e), 500L)
      })))
  assign(".TERMITE_API_SERVER", srv, envir = globalenv())
  cat(sprintf(paste0("TERMITE API v%s 已启动\n",
                     "  地址       http://%s:%d/\n",
                     "  工作目录   %s\n",
                     "  鉴权       %s\n",
                     "  端点自描述 GET /capabilities\n"),
              .API_VERSION, host, as.integer(port), .api_job_dir(),
              if (is.null(token) || !nzchar(token)) "未开启" else "X-Auth-Token"))
  srv
}

termite_api_stop <- function() {
  srv <- get0(".TERMITE_API_SERVER", envir = globalenv(), ifnotfound = NULL)
  if (!is.null(srv)) httpuv::stopServer(srv)
  invisible(TRUE)
}

termite_api_serve <- function(host = "127.0.0.1", port = 8787L, token = NULL) {
  termite_api_start(host, port, token)
  repeat httpuv::service(1000)   # 常驻服务直到进程结束
}
