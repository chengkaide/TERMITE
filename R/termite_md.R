# =============================================================================
#  极简 Markdown → HTML 渲染器
#  ---------------------------------------------------------------------------
#  只为在 Shiny 里显示 docs/*.md 而写，不引入 markdown / commonmark 依赖。
#  支持：# ~ #### 标题、--- 分隔线、- / 1. 列表、> 引用、``` 代码块、
#        | 表格 |、**粗体**、*斜体*、`行内代码`、[链接](url)。
# =============================================================================

.md_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

.md_inline <- function(x) {
  x <- .md_escape(x)
  x <- gsub("`([^`]+)`", "<code>\\1</code>", x)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x)
  x <- gsub("(?<![*[:alnum:]])\\*([^*]+)\\*(?![*[:alnum:]])", "<em>\\1</em>", x, perl = TRUE)
  x <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "<a href=\"\\2\" target=\"_blank\">\\1</a>", x)
  x
}

#' 把 Markdown 文本转成 HTML 片段
md_to_html <- function(txt) {
  lines <- strsplit(gsub("\r\n", "\n", txt, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  out <- character(0)
  i <- 1L; n <- length(lines)
  in_ul <- FALSE; in_ol <- FALSE; in_code <- FALSE
  close_lists <- function() {
    if (in_ul) { out <<- c(out, "</ul>"); in_ul <<- FALSE }
    if (in_ol) { out <<- c(out, "</ol>"); in_ol <<- FALSE }
  }
  while (i <= n) {
    ln <- lines[i]
    # 代码块
    if (grepl("^\\s*```", ln)) {
      if (!in_code) { close_lists(); out <- c(out, "<pre class='md-code'>"); in_code <- TRUE }
      else          { out <- c(out, "</pre>"); in_code <- FALSE }
      i <- i + 1L; next
    }
    if (in_code) { out <- c(out, .md_escape(ln)); i <- i + 1L; next }

    if (!nzchar(trimws(ln))) { close_lists(); i <- i + 1L; next }

    # 水平线
    if (grepl("^\\s*(-{3,}|\\*{3,})\\s*$", ln)) {
      close_lists(); out <- c(out, "<hr class='md-hr'>"); i <- i + 1L; next
    }
    # 标题
    m <- regmatches(ln, regexec("^(#{1,6})\\s+(.*)$", ln))[[1]]
    if (length(m) == 3) {
      close_lists()
      lv <- nchar(m[2])
      out <- c(out, sprintf("<h%d class='md-h'>%s</h%d>", lv, .md_inline(m[3]), lv))
      i <- i + 1L; next
    }
    # 表格
    if (grepl("^\\s*\\|", ln) && i < n && grepl("^\\s*\\|[\\s:|-]+\\|\\s*$", lines[i + 1L])) {
      close_lists()
      hdr <- trimws(strsplit(trimws(ln), "|", fixed = TRUE)[[1]])
      hdr <- hdr[nzchar(hdr)]
      out <- c(out, "<table class='md-table'><thead><tr>",
               paste0("<th>", .md_inline(hdr), "</th>"), "</tr></thead><tbody>")
      i <- i + 2L
      while (i <= n && grepl("^\\s*\\|", lines[i])) {
        cells <- trimws(strsplit(trimws(lines[i]), "|", fixed = TRUE)[[1]])
        cells <- cells[nzchar(cells)]
        out <- c(out, "<tr>", paste0("<td>", .md_inline(cells), "</td>"), "</tr>")
        i <- i + 1L
      }
      out <- c(out, "</tbody></table>")
      next
    }
    # 引用
    if (grepl("^\\s*>\\s?", ln)) {
      close_lists()
      out <- c(out, sprintf("<blockquote class='md-quote'>%s</blockquote>",
                            .md_inline(sub("^\\s*>\\s?", "", ln))))
      i <- i + 1L; next
    }
    # 无序列表
    if (grepl("^\\s*[-*+]\\s+", ln)) {
      if (!in_ul) { close_lists(); out <- c(out, "<ul class='md-ul'>"); in_ul <- TRUE }
      out <- c(out, sprintf("<li>%s</li>", .md_inline(sub("^\\s*[-*+]\\s+", "", ln))))
      i <- i + 1L; next
    }
    # 有序列表
    if (grepl("^\\s*[0-9]+\\.\\s+", ln)) {
      if (!in_ol) { close_lists(); out <- c(out, "<ol class='md-ol'>"); in_ol <- TRUE }
      out <- c(out, sprintf("<li>%s</li>", .md_inline(sub("^\\s*[0-9]+\\.\\s+", "", ln))))
      i <- i + 1L; next
    }
    close_lists()
    out <- c(out, sprintf("<p class='md-p'>%s</p>", .md_inline(ln)))
    i <- i + 1L
  }
  if (in_code) out <- c(out, "</pre>")
  close_lists()
  paste(out, collapse = "\n")
}
