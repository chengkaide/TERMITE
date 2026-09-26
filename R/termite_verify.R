# =============================================================================
#  一致性自检：本内核 vs 原始 TERMITE 脚本的输出
#  ---------------------------------------------------------------------------
#  基准文件 tests/reference/ 由原始脚本在自带示例数据上生成：
#    · spotscan —— 原脚本可直接运行
#    · linescan —— 原脚本有 3 处致命/类型错误，需先做最小修补（见 docs/FIXES.md）
# =============================================================================

.vfy_num <- function(x) {
  x <- trimws(as.character(x))
  suppressWarnings(as.numeric(x))
}

.vfy_read <- function(path) {
  read.table(path, header = TRUE, sep = "\t", check.names = FALSE,
             colClasses = "character", comment.char = "", quote = "\"")
}

#' 逐列比较两个数据框，返回明细
.vfy_compare <- function(mine, ref, case, table_label, tol = 1e-9) {
  res <- do.call(rbind, lapply(names(ref), function(cn) {
    a <- .vfy_num(mine[[cn]]); b <- .vfy_num(ref[[cn]])
    if (length(a) != length(b))
      return(data.frame(case = case, table = table_label, column = cn,
                        n = NA_integer_, max_rel = NA_real_,
                        na_pattern = NA, pass = FALSE, note = "长度不一致"))
    ok <- !is.na(a) & !is.na(b)
    rel <- if (!any(ok)) 0 else {
      d <- pmax(abs(b[ok]), .Machine$double.eps)
      max(abs(a[ok] - b[ok]) / d)
    }
    same_na <- identical(is.na(a), is.na(b))
    data.frame(case = case, table = table_label, column = cn,
               n = sum(ok), max_rel = rel, na_pattern = same_na,
               pass = same_na && rel <= tol, note = "")
  }))
  res
}

#' 跑一致性自检
#'
#' @param data_dir 示例数据目录（含 Rawdata_* / ReferenceMaterial_* / TERMITEScriptFolder）
#' @param ref_dir  基准输出目录（tests/reference）
#' @return list(summary = 字符串, detail = 数据框, passed = 逻辑)
termite_verify <- function(data_dir = "your_main_directory",
                           ref_dir  = "tests/reference") {
  if (!dir.exists(ref_dir))
    return(list(summary = sprintf("找不到基准目录 %s，跳过自检。", ref_dir),
                detail = NULL, passed = NA))

  read_ref <- function(f) .vfy_read(file.path(ref_dir, f))
  need <- c("Results_your_sample_name_spotscan.csv",
            "RSFused_your_sample_name_spotscan.csv",
            "LoD_ReferenceMaterial_your_sample_name_spotscan.csv",
            "Results_your_sample_name_linescan.csv",
            "RSFused_your_sample_name_linescan.csv",
            "LoD_ReferenceMaterial_your_sample_name_linescan.csv")
  miss <- need[!file.exists(file.path(ref_dir, need))]
  if (length(miss))
    return(list(summary = paste0("缺少基准文件：", paste(miss, collapse = ", ")),
                detail = NULL, passed = NA))

  detail <- list()

  # ---------------- 点分析 ----------------
  cfg <- termite_defaults("spot"); cfg$dir <- data_dir; cfg$legacy_lod <- TRUE
  rs  <- termite_run(cfg)

  d  <- termite_table(rs); rf <- read_ref(need[1]); names(rf)[1] <- "ID"
  detail[[1]] <- .vfy_compare(d, rf, "点分析", "Results_*_spotscan.csv")
  detail[[2]] <- .vfy_compare(
    data.frame(x = round(as.numeric(rs$rsf), 2)),
    data.frame(x = .vfy_num(read_ref(need[2])$x)), "点分析", "RSFused_*_spotscan.csv", tol = 1e-12)
  detail[[3]] <- .vfy_compare(
    data.frame(LoD = round(as.numeric(rs$lod), 5)),
    data.frame(LoD = .vfy_num(read_ref(need[3])[[1]])), "点分析", "LoD_*_spotscan.csv", tol = 1e-12)

  # ---------------- 线扫描 ----------------
  cfg <- termite_defaults("line"); cfg$dir <- data_dir
  cfg$legacy_lod <- TRUE; cfg$legacy_time_axis <- TRUE; cfg$clip_negative <- FALSE
  rl <- termite_run(cfg)

  d  <- termite_table(rl); rf <- read_ref(need[4]); names(rf)[1] <- "length_mm"
  detail[[4]] <- .vfy_compare(d, rf, "线扫描", "Results_*_linescan.csv")
  detail[[5]] <- .vfy_compare(
    data.frame(x = round(as.numeric(rl$rsf), 2)),
    data.frame(x = .vfy_num(read_ref(need[5])$x)), "线扫描", "RSFused_*_linescan.csv", tol = 1e-12)
  detail[[6]] <- .vfy_compare(
    data.frame(LoD = round(as.numeric(rl$lod), 5)),
    data.frame(LoD = .vfy_num(read_ref(need[6])[[1]])), "线扫描", "LoD_*_linescan.csv", tol = 1e-12)

  det <- do.call(rbind, detail)
  nfail <- sum(!det$pass)

  # 修正模式带来的差异
  cfg2 <- termite_defaults("line"); cfg2$dir <- data_dir
  cfg2$legacy_lod <- TRUE; cfg2$legacy_time_axis <- TRUE; cfg2$clip_negative <- FALSE
  cfg3 <- cfg2; cfg3$legacy_lod <- FALSE; cfg3$legacy_time_axis <- FALSE
  ta <- termite_table(termite_run(cfg2)); tb <- termite_table(termite_run(cfg3))
  # 修正模式带来的差异：逐列比较 NA 状态，xor 出的差异数即受影响的单元格数
  cols <- setdiff(names(ta), "length_mm")
  chg  <- sum(vapply(cols, function(cn)
    sum(xor(is.na(.vfy_num(ta[[cn]])), is.na(.vfy_num(tb[[cn]])))), 1L))
  dx <- abs(tb$length_mm[1] - ta$length_mm[1]) * 1000

  s <- paste0(
    if (nfail == 0L) "自检通过：内核在复刻模式下与原始脚本输出逐位一致。\n"
    else sprintf("自检未通过：%d / %d 列不一致。\n", nfail, nrow(det)),
    sprintf("  比对列数 %d，最大相对偏差 %.3e\n", nrow(det), max(det$max_rel, na.rm = TRUE)),
    sprintf("  线扫描 x 轴修正：起点偏移 %.2f µm\n", dx),
    sprintf("  LoD 文件选取修正：线扫描剖面 %d 个单元格的 NA 状态改变\n", chg))
  list(summary = s, detail = det, passed = (nfail == 0L))
}


#' 生成自检结果的可读表格文本
termite_verify_text <- function(v) {
  if (is.null(v$detail)) return(v$summary)
  det <- v$detail
  # 按 case 分组（levels 显式给出，保证输出顺序与输入一致）
  groups <- split(seq_len(nrow(det)), factor(det$case, levels = unique(det$case)))
  blocks <- vapply(groups, function(ii) {
    rows <- sprintf("  %-34s %-4s n=%-6s rel=%.2e  %s%s",
                    det$table[ii], det$column[ii],
                    ifelse(is.na(det$n[ii]), "-", det$n[ii]),
                    det$max_rel[ii],
                    ifelse(det$pass[ii], "OK", "FAIL"),
                    ifelse(det$na_pattern[ii], "", "  [NA 模式不同]"))
    paste0("\n[", det$case[ii[1]], "]\n", paste(rows, collapse = "\n"), "\n")
  }, character(1))
  paste0(v$summary, paste(blocks, collapse = ""))
}
