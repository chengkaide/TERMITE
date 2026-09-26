#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""把 docs/*.md 转成与落地页同风格的自包含 HTML。

为什么需要这一步：docs/ 下有 .nojekyll，GitHub Pages 会把 .md 原样投递
（Content-Type: text/markdown），浏览器只会显示源码原文。这里离线预渲染成
静态 HTML，保持「单击就能看、不依赖 CDN 与本地服务器」的离线自包含特性。

用法（在仓库根目录执行）：
    <venv>/Scripts/python.exe tools/md2html.py

新增/改动 .md 之后重跑本脚本即可。生成的 .html 不要手改。
"""

import re
import pathlib
import html as html_mod

try:
    import markdown
except ImportError:
    raise SystemExit("需要 Python-Markdown：pip install markdown")

ROOT = pathlib.Path(__file__).resolve().parent.parent
DOCS = ROOT / "docs"

# 顶部导航 + 文档编号顺序（file, 标签, 一句话描述）
PAGES = [
    ("index.html",          "概览",       "项目主页与快速开始"),
    ("HANDBOOK.html",       "入门手册",   "LA-ICP-MS 微量元素数据处理入门"),
    ("PRINCIPLES.html",     "归算原理",   "七步链路每一步在算什么、哪里容易错"),
    ("IMPORT.html",         "数据导入",   "支持的仪器格式、别名表、内标选择"),
    ("FORMULA_CAL.html",    "无内标校准", "矿物化学式归一化（AYCF 家族）"),
    ("FIXES.html",          "问题清单",   "原始脚本的缺陷与逐条修复说明"),
    ("ORIGINAL_README.html", "原始说明",  "上游 TERMITE 仓库的 README"),
]

# 由 md 生成的页面（md 名 -> html 名）
GEN = {
    "PRINCIPLES.md":      "PRINCIPLES.html",
    "IMPORT.md":          "IMPORT.html",
    "FORMULA_CAL.md":     "FORMULA_CAL.html",
    "FIXES.md":           "FIXES.html",
    "ORIGINAL_README.md": "ORIGINAL_README.html",
}

CSS = """
  :root{
    --ink:#26251F; --ink2:#5F5E5A; --ink3:#8A8880;
    --line:#D8D6CC; --bg:#FFFFFF; --panel:#F8F7F4; --panel2:#EFEEE8;
    --pur:#4C43AE; --purbg:#EEEDFE; --purline:#AFA9EC;
    --tea:#0E6650; --teabg:#E3F5EF; --tealine:#9FE1CB;
    --amb:#7D4A0A; --ambbg:#FAEEDA; --ambline:#FAC775;
  }
  *{box-sizing:border-box}
  html{-webkit-text-size-adjust:100%;scroll-behavior:smooth}
  body{margin:0;background:var(--bg);color:var(--ink);
       font-family:"Microsoft YaHei","PingFang SC",system-ui,sans-serif;
       font-size:15.5px;line-height:1.8;font-variant-numeric:tabular-nums}
  a{color:var(--pur);text-decoration:none}
  a:hover{text-decoration:underline}
  .wrap{max-width:1040px;margin:0 auto;padding:0 30px 90px}

  /* ---------- sticky nav ---------- */
  .nav{position:sticky;top:0;z-index:100;background:rgba(255,255,255,.92);
       backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);
       border-bottom:1px solid var(--line)}
  .nav .inner{max-width:1040px;margin:0 auto;padding:0 30px;display:flex;
       align-items:center;gap:16px;height:56px}
  .nav .brand{font-weight:700;font-size:15.5px;color:var(--ink);white-space:nowrap;
       letter-spacing:-.2px}
  .nav .brand i{color:var(--pur);font-style:normal}
  .nav .links{margin-left:auto;display:flex;gap:2px;overflow-x:auto;white-space:nowrap}
  .nav .links a{color:var(--ink2);font-size:13.5px;padding:6px 11px;border-radius:7px}
  .nav .links a:hover{background:var(--panel);text-decoration:none;color:var(--ink)}
  .nav .links a.on{background:var(--purbg);color:var(--pur);font-weight:600}
  @media(max-width:720px){.nav .links{display:none}}

  /* ---------- 文档头 ---------- */
  .dochead{margin:38px 0 30px;padding-bottom:22px;border-bottom:1px solid var(--line)}
  .dochead .kicker{font-size:12.5px;letter-spacing:.14em;color:var(--pur);
       font-weight:700;margin:0 0 12px}
  .dochead h1{font-size:32px;font-weight:700;margin:0 0 12px;line-height:1.3;
       letter-spacing:-.4px}
  .dochead .sub{color:var(--ink2);font-size:16px;margin:0 0 16px}
  .dochead .meta{font-size:12.5px;color:var(--ink3)}
  .dochead .meta code{font-family:Consolas,Menlo,monospace;background:var(--panel);
       padding:1px 6px;border-radius:4px}

  /* ---------- 布局：侧边目录 + 正文 ---------- */
  .layout{display:grid;grid-template-columns:228px 1fr;gap:40px;align-items:start}
  @media(max-width:900px){.layout{grid-template-columns:1fr;gap:0}}
  .toc{position:sticky;top:76px;font-size:13.5px;line-height:1.65;
       max-height:calc(100vh - 110px);overflow-y:auto}
  @media(max-width:900px){.toc{position:static;max-height:none;margin-bottom:26px;
       padding:16px 18px;background:var(--panel);border-radius:12px}}
  .toc .t{font-size:11.5px;letter-spacing:.12em;color:var(--ink3);font-weight:700;
       margin-bottom:10px}
  .toc ul{list-style:none;margin:0;padding:0}
  .toc li{margin:0 0 5px}
  .toc a{color:var(--ink2);display:block;padding:1px 0}
  .toc a:hover{color:var(--pur);text-decoration:none}
  .toc ul ul{margin:3px 0 3px 12px;border-left:1px solid var(--line);padding-left:10px}
  .toc ul ul a{font-size:12.5px;color:var(--ink3)}
  .toc ul ul a:hover{color:var(--pur)}

  /* ---------- 正文 ---------- */
  .mbody{min-width:0}
  .mbody h2{font-size:23px;font-weight:700;margin:44px 0 16px;padding-top:20px;
       border-top:2px solid var(--line);letter-spacing:-.2px;position:relative;
       scroll-margin-top:70px}
  .mbody h2:first-child{border-top:0;padding-top:0;margin-top:0}
  .mbody h2::before{content:"";position:absolute;top:-2px;left:0;width:56px;height:2px;
       background:var(--pur)}
  .mbody h3{font-size:17.5px;font-weight:600;margin:30px 0 10px;color:var(--ink);
       scroll-margin-top:70px}
  .mbody h4{font-size:15.5px;font-weight:600;margin:22px 0 8px;color:var(--ink)}
  .mbody p{margin:0 0 14px;color:var(--ink2)}
  .mbody strong{color:var(--ink);font-weight:700}
  .mbody ul,.mbody ol{margin:0 0 14px;padding-left:24px;color:var(--ink2)}
  .mbody li{margin:0 0 6px}
  .mbody hr{border:0;border-top:1px dashed var(--line);margin:34px 0}
  .mbody blockquote{margin:0 0 16px;padding:12px 20px;background:var(--panel);
       border-left:3px solid var(--purline);border-radius:0 10px 10px 0;color:var(--ink2)}
  .mbody blockquote p{margin:0;color:var(--ink2)}

  /* 行内代码 */
  .mbody code{font-family:Consolas,Menlo,monospace;font-size:13px;
       background:var(--panel);padding:1.5px 6px;border-radius:4px;color:#2A2470}
  .mbody pre code,.mbody .cite-block code{background:none;color:inherit;padding:0}
  /* 代码块 */
  .mbody pre{background:#2A2A33;color:#EAEAF2;border-radius:11px;padding:16px 20px;
       overflow-x:auto;margin:0 0 16px;line-height:1.7}
  .mbody pre code{font-family:Consolas,Menlo,monospace;font-size:13.2px}

  /* 表格（宽表可横向滚动） */
  .tw{overflow-x:auto;margin:0 0 16px;border:1px solid var(--line);border-radius:10px}
  .mbody table{border-collapse:collapse;width:100%;font-size:14px;background:#fff}
  .mbody th,.mbody td{border-bottom:1px solid var(--line);padding:9px 14px;
       text-align:left;vertical-align:top}
  .mbody th{background:var(--panel2);font-weight:700;color:var(--ink);white-space:nowrap}
  .mbody tbody tr:last-child td{border-bottom:0}
  .mbody td code{font-size:12.5px}

  /* 图（md 里内联的 SVG） */
  .mbody figure{margin:26px 0 30px}
  .mbody figure svg{display:block;width:100%;height:auto;border:1px solid var(--line);
       border-radius:12px;background:#fff;box-shadow:0 2px 8px rgba(38,37,31,.05)}
  .mbody figcaption{color:var(--ink3);font-size:12.5px;margin-top:10px;line-height:1.75}

  /* ---------- footer ---------- */
  footer{margin-top:44px;padding-top:20px;border-top:1px solid var(--line);
       color:var(--ink3);font-size:13.5px;line-height:1.8}
  footer a{color:var(--ink2)}
"""


def build_nav(active_html: str) -> str:
    links = []
    for href, label, _desc in PAGES:
        cls = ' class="on"' if href == active_html else ""
        links.append('<a href="%s"%s>%s</a>' % (href, cls, label))
    return "\n    ".join(links)


def build_toc(tokens, max_level: int = 3) -> str:
    """把 markdown 的 toc_tokens 递归转成嵌套 <ul>，跳过 level 1（文档标题已在页头）。"""
    def walk(items, depth):
        if not items:
            return ""
        parts = ["<ul>"]
        for it in items:
            lvl = int(it.get("level", 1))
            if lvl < 2 or lvl > max_level:
                parts.extend(walk(it.get("children", []), depth))
                continue
            name = html_mod.unescape(re.sub(r"<[^>]+>", "", it.get("name", "")))
            parts.append('<li><a href="#%s">%s</a>' % (it.get("id", ""), name.strip()))
            parts.append(walk(it.get("children", []), depth + 1))
            parts.append("</li>")
        parts.append("</ul>")
        return "".join(parts)

    return walk(tokens, 0)


def convert(md_name: str, html_name: str) -> dict:
    md_path = DOCS / md_name
    src = md_path.read_text(encoding="utf-8")

    mdr = markdown.Markdown(extensions=["tables", "fenced_code", "sane_lists",
                                        "toc", "attr_list"])
    body = mdr.convert(src)

    # 抽出文档标题（第一个 h1），避免和页头重复
    title = md_name
    m = re.search(r"<h1[^>]*>(.*?)</h1>", body, re.S)
    if m:
        title = re.sub(r"<[^>]+>", "", m.group(1)).strip()
        title = html_mod.unescape(title)
        body = body[:m.start()] + body[m.end():]

    # 内部 md 链接 -> html
    def _fix_link(mo):
        target = mo.group(1)
        if re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*://", target) or target.startswith("#"):
            return mo.group(0)
        return 'href="%s.html"' % target[:-3] if target.endswith(".md") else mo.group(0)

    body = re.sub(r'href="([^"]+)"', _fix_link, body)

    # 链接显示文本与 href 同步：如 <a href="PRINCIPLES.html">PRINCIPLES.md</a>
    # 的文字部分若写着旧 .md 名，一并替换，避免「文字说 md、点开是 html」的错位。
    def _fix_label(mo):
        href, inner = mo.group(1), mo.group(2)
        old_md = href[:-5] + ".md"          # X.html -> X.md
        if old_md in inner:
            inner = inner.replace(old_md, href)
        return '<a href="%s">%s</a>' % (href, inner)

    body = re.sub(r'<a href="([^"]+\.html)">([^<]*)</a>', _fix_label, body)

    # 宽表格包一层做横向滚动
    body = body.replace("<table>", '<div class="tw"><table>')
    body = body.replace("</table>", "</table></div>")

    toc_html = build_toc(getattr(mdr, "toc_tokens", []) or [])

    desc = next((d for f, _l, d in PAGES if f == html_name), "")
    page = """<!DOCTYPE html>
<html lang="zh-CN">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{title} · TERMITE 文档</title>
<style>{css}</style>
</head>
<body>
<nav class="nav"><div class="inner">
  <div class="brand">TERMITE <i>·</i> 文档</div>
  <div class="links">
    {nav}
  </div>
</div></nav>

<div class="wrap">
  <div class="dochead">
    <div class="kicker">TERMITE 文档</div>
    <h1>{title}</h1>
    <p class="sub">{desc}</p>
    <div class="meta">由 <code>docs/{md_name}</code> 自动生成 ·
      <a href="https://github.com/chengkaide/TERMITE/blob/master/docs/{md_name}">在 GitHub 上查看源文件</a></div>
  </div>

  <div class="layout">
    <aside class="toc">
      <div class="t">目录</div>
      {toc}
    </aside>
    <article class="mbody">
{body}
    </article>
  </div>

  <footer>
    TERMITE 交互式 LA-ICP-MS 微量元素数据归算 ·
    <a href="index.html">返回项目主页</a> ·
    <a href="https://github.com/chengkaide/TERMITE">GitHub 仓库</a>
  </footer>
</div>
</body>
</html>
"""
    out = page.format(title=html_mod.escape(title), css=CSS, nav=build_nav(html_name),
                      desc=html_mod.escape(desc), md_name=md_name,
                      toc=toc_html or '<div class="t" style="border:0">（无章节标题）</div>',
                      body=body)
    out_path = DOCS / html_name
    out_path.write_text(out, encoding="utf-8", newline="\n")

    # 质量提示：找出可能失效的链接
    suspicious = []
    for href in re.findall(r'href="([^"]+)"', body):
        if re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*://", href) or href.startswith(("#", "mailto:")):
            continue
        target = href.split("#")[0]
        if not target:
            continue
        if not (DOCS / target).exists():
            suspicious.append(href)

    return {"md": md_name, "html": html_name, "title": title,
            "toc_items": toc_html.count("<li>"),
            "tables": body.count("<table>"), "code_blocks": body.count("<pre>"),
            "size": out_path.stat().st_size, "suspicious": suspicious}


def main():
    print("docs 目录：", DOCS)
    for md_name, html_name in GEN.items():
        info = convert(md_name, html_name)
        print("  %-22s -> %-22s %6.1f KB  目录%2d项 表格%2d 代码块%2d  [%s]"
              % (info["md"], info["html"], info["size"] / 1024, info["toc_items"],
                 info["tables"], info["code_blocks"], info["title"]))
        if info["suspicious"]:
            print("     !! 可能失效的链接：", ", ".join(sorted(set(info["suspicious"]))))
    print("\n完成。下一步：更新 docs/index.html 里指向 .md 的链接为 .html。")


if __name__ == "__main__":
    main()
