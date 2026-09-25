# =============================================================================
#  数值一致性测试：本内核 vs 原始 TERMITE 脚本
#
#  用法：  Rscript tests/test-parity.R
#          TERMITE_ROOT=/path/to/repo Rscript tests/test-parity.R
#  退出码：0 = 全部通过，1 = 有列不一致
# =============================================================================

root <- Sys.getenv("TERMITE_ROOT", unset = ".")
if (!file.exists(file.path(root, "R", "termite_core.R"))) root <- getwd()
if (!file.exists(file.path(root, "R", "termite_core.R")))
  stop("找不到 R/termite_core.R，请在仓库根目录运行，或设置 TERMITE_ROOT。")
setwd(root)
options(encoding = "UTF-8")     # 本机 R 的 native 编码可能是 GBK，必须显式指定
# native 编码若是 "C"（ASCII），source(encoding="UTF-8") 会因中文注释无法转码而吞掉代码；
# 切到 UTF-8 原生 locale（Windows 10+ 支持），不行再用 GBK 兜底（已是 MBCS/UTF-8 时无操作）。
if (!l10n_info()[["MBCS"]] && !l10n_info()[["UTF-8"]]) {
  for (lc in c("English_United States.utf8", "Chinese (Simplified)_China.936"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) break
}
for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(f, encoding = "UTF-8")

cat("\n===================================================================\n")
cat("  TERMITE 内核 · 与原始脚本的数值一致性测试\n")
cat("===================================================================\n")

v <- termite_verify(data_dir = "your_main_directory", ref_dir = "tests/reference")
cat(termite_verify_text(v))

cat("\n===================================================================\n")
if (isTRUE(v$passed)) cat("  结果：全部通过\n") else cat("  结果：存在不一致\n")
cat("===================================================================\n\n")

if (!isTRUE(v$passed)) quit(status = 1L)
