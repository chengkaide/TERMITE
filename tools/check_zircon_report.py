#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
校验「锆石四口径对比_图解.html」——单文件自包含文档的结构、良构与排版。

分四层（越靠后越硬）：
  1 结构：标签配对、无外链、无残留占位符、图号连续
  2 良构：每张内联 SVG 单独喂 XML 解析器（能解析就一定能画出来）
  3 排版：文字包围盒估算——出框 / 贴底 / 两两压盖
  4 数字：正文与表里的手写数字，用数据源（CSV）现算比对

判据只写一份；失败全部收集起来最后一起报，并 exit 1。
用法：  OUT=<结果目录> python tools/check_zircon_report.py
"""
import csv
import io
import math
import os
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

OUT = os.environ.get("OUT", "")
if not OUT:
    sys.exit("请先用环境变量 OUT 指定结果目录。")
OUTP = pathlib.Path(OUT)
DOC = OUTP / "锆石四口径对比_图解.html"
if not DOC.exists():
    sys.exit(f"找不到文档：{DOC}")

H = DOC.read_text(encoding="utf-8")
BAD = []


def bad(label, detail=""):
    BAD.append(f"{label}　｜　{detail}" if detail else label)


def rd(name):
    with io.open(OUTP / name, encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


# ---------------------------------------------------------------- 1 结构层
for tag in ("figure", "figcaption", "table", "thead", "tbody"):
    a, b = H.count(f"<{tag}>"), H.count(f"</{tag}>")
    if a != b:
        bad(f"<{tag}> 不配对", f"{a} 开 / {b} 闭")

if H.count("<svg") != H.count("</svg>"):
    bad("<svg> 不配对", f"{H.count('<svg')} 开 / {H.count('</svg>')} 闭")

if re.search(r'<img[^>]+src="http', H):
    bad("存在外链图片（单文件自包含被破坏）")

if re.search(r"@@[A-Z0-9_]+@@", H):
    bad("有未替换的占位符", str(re.findall(r"@@[A-Z0-9_]+@@", H)))

# 内联 SVG 里禁止出现 HTML breakout 标签
BREAK = ("sup|sub|span|var|i|b|p|div|code|em|strong|small|u|s|br|hr|li|ul|ol|"
         "table|tt|pre|h1|h2|h3|h4|h5|h6|center|font|nobr|ruby|listing|menu|"
         "body|head|meta|embed|img|big|blockquote|dd|dt|dl|a")
for k, m in enumerate(re.finditer(r"<svg\b.*?</svg>", H, re.S), 1):
    hits = sorted(set(re.findall(r"<(" + BREAK + r")(?=[\s>/])", m.group(0))))
    if hits:
        bad(f"图 {k} 的 SVG 里混入 breakout 标签（整张图会塌）", ", ".join(hits))

# 图号连续
nums = [int(x) for x in re.findall(r"<b>图 (\d+)</b>", H)]
if nums != list(range(1, len(nums) + 1)):
    bad("图号不连续", str(nums))
if len(nums) != H.count("<figure>"):
    bad("figure 数与带编号图注数不一致", f"figure {H.count('<figure>')} / 编号 {len(nums)}")

# 正文里引用的「图 N」必须存在
for n in set(int(x) for x in re.findall(r"（图 (\d+)）", H)):
    if n not in nums:
        bad("正文引用了不存在的图号", f"图 {n}")

# ---------------------------------------------------------------- 2 良构层
SVGS = re.findall(r"<svg\b.*?</svg>", H, re.S)
BOXES = []          # (图号, [文字包围盒])
for k, s in enumerate(SVGS, 1):
    try:
        ET.fromstring(s)
    except ET.ParseError as e:
        bad(f"图 {k} 的 SVG 不是良构 XML", str(e))
        continue

    # ------------------------------------------------------------ 3 排版层
    m = re.search(r'viewBox="([^"]+)"', s)
    _, _, W, Ht = [float(x) for x in m.group(1).split()]
    boxes = []
    for t in re.finditer(r"<text ([^>]*)>(.*?)</text>", s, re.S):
        at, body = t.group(1), t.group(2)
        g = lambda key: (re.search(r'\b%s="([^"]*)"' % key, at) or [None, None])[1]
        body = re.sub(r"<[^>]+>", "", body)          # 去掉 tspan，只留字面文字
        body = (body.replace("&amp;", "&").replace("&lt;", "<")
                    .replace("&gt;", ">").replace("&quot;", '"'))
        x, y = float(g("x")), float(g("y"))
        fs = float(g("font-size") or 12)
        anch = g("text-anchor")
        w = sum(fs if ord(c) > 0x2E80 else fs * 0.55 for c in body)
        x0 = x - w / 2 if anch == "middle" else (x - w if anch == "end" else x)
        if x0 + w > W - 4 or x0 < -2:
            bad(f"图 {k} 文字出框", f"右缘 {x0 + w:.0f} > {W:.0f}　「{body[:22]}」")
        if y > Ht - 8:
            bad(f"图 {k} 文字贴底（画布可能不够高）", f"y={y} 高 {Ht}　「{body[:22]}」")
        boxes.append((x0, y - 0.82 * fs, x0 + w, y + 0.25 * fs, body[:16]))
    # 两两压盖：重叠 >4px 且超过小框面积 30%
    for i in range(len(boxes)):
        for j in range(i + 1, len(boxes)):
            a, b = boxes[i], boxes[j]
            ox = min(a[2], b[2]) - max(a[0], b[0])
            oy = min(a[3], b[3]) - max(a[1], b[1])
            if ox > 4 and oy > 4:
                sa = (a[2] - a[0]) * (a[3] - a[1])
                sb = (b[2] - b[0]) * (b[3] - b[1])
                if ox * oy > 0.30 * min(sa, sb):
                    bad(f"图 {k} 文字压盖", f"「{a[4]}」×「{b[4]}」重叠 {ox:.0f}×{oy:.0f}px")
    BOXES.append((k, len(boxes)))

# ---------------------------------------------------------------- 4 数字层
cl = {r["口径"]: r for r in rd("主量氧化物闭合_五口径.csv")}
sr = {r["口径"]: r for r in rd("SiZr摩尔比_五口径.csv")}

PAIRS = [
    ("M1　Zr 内标", "M1 Zr内标"), ("M2　Si 内标", "M2 Si内标"),
    ("M3　归一法 fixed Si:1", "M3 归一法_fixedSi1"),
    ("M4　归一法 anhydrous", "M4 归一法_anhydrous"),
    ("实验室 AYCF（ICPMSDataCal）", "LAB AYCF"),
]


def close(a, b, tol):
    return abs(a - b) <= tol


# 表体逐格核对
tbody = re.search(r"<tbody>(.*?)</tbody>", H, re.S).group(1)
rows = re.findall(r"<tr>(.*?)</tr>", tbody, re.S)
if len(rows) != len(PAIRS):
    bad("表格行数与口径数不符", f"{len(rows)} vs {len(PAIRS)}")
for tr, (label, key) in zip(rows, PAIRS):
    tds = [re.sub(r"<[^>]+>", "", td) for td in re.findall(r"<td[^>]*>(.*?)</td>", tr, re.S)]
    if key not in cl:
        bad("数据源里没有这个口径", key)
        continue
    exp = [float(cl[key]["SiO2"]), float(cl[key]["ZrO2"]), float(cl[key]["总和"]),
           float(sr[key]["Si比ZrHf"])]
    got = [float(tds[1]), float(tds[2]), float(tds[3]), float(tds[4])]
    for nm, e, g in zip(("SiO2", "ZrO2", "总和", "Si:(Zr+Hf)"), exp, got):
        if not close(e, g, 0.05):
            bad("表格数字与数据源不符", f"{label} 的 {nm}：表中 {g}，实算 {e:.4f}")

# 正文里手写的总量数字（111.5 / 89.2）也要核
for s_txt, key in (("111.5", "M1 Zr内标"), ("89.2", "M2 Si内标")):
    if not close(float(cl[key]["总和"]), float(s_txt), 0.05):
        bad("正文手写数字与数据源不符", f"{s_txt} vs {cl[key]['总和']}")

# 图注里引用的 wt% 与摩尔比
for s_txt, key, col in (("111.5", "M1 Zr内标", "总和"), ("89.2", "M2 Si内标", "总和"),
                        ("100.0", "M3 归一法_fixedSi1", "总和")):
    if s_txt not in H:
        bad("图注里不见了应有的数字", f"{key} {col} = {s_txt}")

for s_txt, key in (("1.016", "M3 归一法_fixedSi1"), ("0.980", "LAB AYCF")):
    if not close(float(sr[key]["Si比ZrHf"]), float(s_txt), 0.0005):
        bad("图注里的摩尔比与数据源不符", f"{s_txt} vs {sr[key]['Si比ZrHf']}")

# 口径数：M3 是唯一同时通过两条检验的
okc = [k for k in cl if k != "LAB AYCF"
       and close(float(cl[k]["总和"]), 100.0, 0.5)
       and close(float(sr[k]["Si比ZrHf"]), 1.0, 0.03)]
if okc != ["M3 归一法_fixedSi1"]:
    bad("「唯一通过两条检验的是 M3」这个结论与数据源不符", str(okc))

# ---------------------------------------------------------------- 汇总
print(f"文档：{DOC.name}　{len(H) / 1024:.1f} KB　图 {len(nums)} 张（内联 SVG {len(SVGS)} 张）")
for k, n in BOXES:
    print(f"  图 {k}：检查文字 {n} 条")
if BAD:
    print(f"\n不通过 {len(BAD)} 项：")
    for b in BAD:
        print("  FAIL", b)
    sys.exit(1)
print("\n全部通过。")
