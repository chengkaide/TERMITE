#!/usr/bin/env Rscript
# =============================================================================
#  生成「积分窗口手动复核」单文件 HTML 页面
#  ---------------------------------------------------------------------------
#  与 tools/zircon_window_auto.R 的关系：那个脚本给出**算法初选**，
#  这个页面给人**手动微调**的界面。两者共用 tools/window_lib.R 的定窗逻辑，
#  所以页面上默认落位的就是算法建议值，不是另一套口径。
#
#  页面特点：
#    · 单文件、零外部依赖、双击即开（数据与绘图代码都内嵌，离线可用）
#    · 每个测点一条曲线，拖动彩色竖线即可改背景窗 / 信号窗
#    · 顶部一键「采用算法建议」或「重置为现行配置」
#    · 导出 CSV 带「是否已偏离算法」列，便于回到批处理流程时区分人工改动
#
#  用法：
#    export TERMITE_DIR=<批次目录>
#    export OUT=<输出目录>
#    "C:/Program Files/R/R-4.2.2/bin/Rscript.exe" tools/make_window_picker.R
# =============================================================================

options(encoding = "UTF-8")
if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

.script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1L])) else getwd()
}
SD <- .script_dir()
source(file.path(SD, "window_lib.R"), encoding = "UTF-8")

DIR <- Sys.getenv("TERMITE_DIR", "")
if (!nzchar(DIR)) stop("请先用环境变量 TERMITE_DIR 指定批次目录。")
OUT <- Sys.getenv("OUT", file.path(DIR, "图表"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

HL <- as.integer(Sys.getenv("HEADER_LINE", "14"))
SL <- as.integer(Sys.getenv("SIGNAL_LINE", "16"))
CH <- Sys.getenv("CH", "29Si")
CB1 <- as.integer(Sys.getenv("CUR_BG1", "25"))
CB2 <- as.integer(Sys.getenv("CUR_BG2", "60"))
CS1 <- as.integer(Sys.getenv("CUR_SIG1", "100"))
CS2 <- as.integer(Sys.getenv("CUR_SIG2", "158"))

TPL <- file.path(SD, "window_picker_template.html")
if (!file.exists(TPL)) stop("找不到模板：", TPL)

# ---- 读点 + 算法定窗 --------------------------------------------------------
pts <- window_scan_dir(DIR, chs = CH, header_line = HL, signal_line = SL)
win <- window_auto_batch(pts, ch = CH, signal_line = SL)
cat("读入", length(pts), "个测点；定窗通道 =", CH, "\n")

# ---- 组装 JSON --------------------------------------------------------------
# 手写 JSON 而不引 jsonlite：本仓库一律零外部依赖，而这里的数据结构极简单
# （对象 + 数字数组），手写反而更可控、更好读。
.jstr <- function(s) {                     # 转义进 JSON 字符串
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  gsub("[\r\n\t]", " ", s)
}
.jnum <- function(x, d = 2L) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.finite(x)) return("null")
  formatC(as.numeric(x), format = "f", digits = d)
}
.jint <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("null")
  as.character(as.integer(x))
}

js_point <- function(i, p, r) {
  yy <- p$y[, 1L]; yy[!is.finite(yy) | yy < 0] <- 0
  tt <- p$t; tt[!is.finite(tt)] <- 0
  # 建议窗口：算法给不出结果时退回现行配置，保证页面永远有可用的初值
  a <- if (is.na(r$信号窗起_行)) c(CB1 - SL + 1L, CB2 - SL + 1L, CS1 - SL + 1L, CS2 - SL + 1L)
       else c(r$背景窗起_行, r$背景窗止_行, r$信号窗起_行, r$信号窗止_行)
  sprintf(paste0(
    '{"i":%d,"file":"%s","name":"%s","n":%d,',
    '"on":%s,"off":%s,"on_s":%s,"off_s":%s,"snr":%s,"verdict":"%s",',
    '"t":[%s],"y":[%s],',
    '"cur":{"bg1":%d,"bg2":%d,"s1":%d,"s2":%d},',
    '"auto":{"bg1":%d,"bg2":%d,"s1":%d,"s2":%d},',
    '"init":{"bg1":%d,"bg2":%d,"s1":%d,"s2":%d}}'),
    i, .jstr(p$file), .jstr(r$样品), p$n,
    .jint(r$剥蚀起_行), .jint(r$剥蚀止_行), .jnum(r$剥蚀起_s, 2L), .jnum(r$剥蚀止_s, 2L),
    .jnum(r$信背比, 2L), .jstr(r$判定),
    paste(formatC(tt, format = "f", digits = 3), collapse = ","),
    paste(formatC(round(yy), format = "d"), collapse = ","),
    CB1 - SL + 1L, CB2 - SL + 1L, CS1 - SL + 1L, CS2 - SL + 1L,
    as.integer(a[1L]), as.integer(a[2L]), as.integer(a[3L]), as.integer(a[4L]),
    as.integer(a[1L]), as.integer(a[2L]), as.integer(a[3L]), as.integer(a[4L]))
}

json <- paste0('{"channel":"', .jstr(CH), '","signalLine":', SL,
               ',"headerLine":', HL, ',"points":[',
               paste(vapply(seq_along(pts), function(i) js_point(i, pts[[i]], win[i, ]),
                            character(1L)), collapse = ",\n"),
               ']}')

# ---- 注入模板并写出 ---------------------------------------------------------
html <- readLines(TPL, encoding = "UTF-8", warn = FALSE)
k <- grep("__DATA__", html, fixed = TRUE)
if (!length(k)) stop("模板里找不到 __DATA__ 占位符：", TPL)
# 只替换占位符本身，必须保留该行原有的 `const D = ` 与结尾分号：
# 整行替换会把赋值语句一起删掉，结果是页面能打开但数据变量根本不存在（静默失效）。
html[k[1L]] <- sub("__DATA__", json, html[k[1L]], fixed = TRUE)

HTM <- file.path(OUT, "积分窗口_手动复核.html")
con <- file(HTM, open = "wb")
writeLines(enc2utf8(html), con, useBytes = TRUE)
close(con)

cat("写出", HTM, "（", round(file.size(HTM) / 1024), " KB）\n", sep = "")
cat("说明：双击打开即可拖动窗口边界；导出的 CSV 可回灌到批处理流程。\n")
