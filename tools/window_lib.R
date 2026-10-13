# =============================================================================
#  积分窗口工具库 —— CLI 脚本的入口（**实现在 R/termite_window.R**）
#  ---------------------------------------------------------------------------
#  本文件现在只是薄壳。定窗逻辑只有一份，放在 R/termite_window.R —— 那里是
#  app.R 装载的位置，Shiny 界面里的「微量元素处理 · 区间选择」页要用同一套代码。
#  若在这里再抄一份，就会出现「界面画的窗口」与「CLI 算的窗口」两套口径，
#  这类偏差不会报错，只会让结果对不上，非常难查。所以这里只负责把它 source 进来。
#
#  被谁引用：
#    tools/make_window_picker.R      生成手动复核的单文件页面
#    tools/zircon_window_auto.R      批处理自动定窗
#    report/TERMITE_quality_report.Rmd / TERMITE_report.Rmd
#  这些调用方一律先修好 LC_CTYPE 再 source 本文件；这里再兜一次，是为了防止
#  有人漏写守卫时踩到「中文被吞、函数一个都没定义」那个坑（症状是"找不到函数"）。
# =============================================================================

if (!l10n_info()[["UTF-8"]])
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break

# ---- 定位真正的实现 ---------------------------------------------------------
# 依次尝试：source() 时留下的 ofile → Rscript 的 --file= → 常见工作目录。
.window_lib_find <- function() {
  cand <- character(0)

  # ① source() 会在求值环境里留下 ofile；从最内层往外找第一个非空的
  fr <- sys.frames()
  for (i in rev(seq_along(fr))) {
    of <- fr[[i]]$ofile
    if (!is.null(of) && is.character(of) && length(of) == 1L && nzchar(of)) {
      cand <- c(cand, file.path(dirname(of), "..", "R", "termite_window.R"))
      break
    }
  }
  # ② Rscript tools/xxx.R 时的 --file=
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) && nzchar(f[1L]))
    cand <- c(cand, file.path(dirname(f[1L]), "..", "R", "termite_window.R"))
  # ③ 常见工作目录：仓库根，或 report/（knitr 的 cwd 是 Rmd 所在目录）
  cand <- c(cand, file.path(getwd(), "R", "termite_window.R"),
                  file.path(getwd(), "..", "R", "termite_window.R"))

  hit <- cand[file.exists(cand)]
  if (!length(hit))
    stop("找不到 R/termite_window.R；请从仓库根目录运行，或确认它没被移动。\n",
         "已尝试：\n  ", paste(unique(cand), collapse = "\n  "))
  normalizePath(hit[1L], winslash = "/", mustWork = TRUE)
}

source(.window_lib_find(), encoding = "UTF-8")
