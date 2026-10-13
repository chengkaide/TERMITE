#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
生成「锆石微量元素四口径对比」单文件自包含 HTML。

设计要点
--------
1. 单文件自包含：6 张 R 出的 PNG 全部 base64 内嵌，2 张流程图用内联 SVG 手绘，
   双击即可离线打开，不需要图片目录、不联网。
2. 图多字少：正文只保留「一句话结论 + 一张数字表 + 四条要点」，其余交给图。
3. 两张手绘 SVG 的坐标一律手算并写在注释里 —— 凭感觉放文字必然溢出或压字。
   内联 SVG 里禁止出现 sub/sup/b/span 等 HTML breakout 标签（会让整张图塌掉），
   上下标用 Unicode 上标字符或 <tspan dy=...>。

用法
----
    OUT=<结果目录> python tools/zircon_report_html.py
    # 可选 FIGDIR 覆盖图片目录（默认 <OUT>/图表）
产出
----
    <OUT>/锆石四口径对比_图解.html
"""
import base64
import os
import pathlib
import sys

OUT = os.environ.get("OUT", "")
if not OUT:
    sys.exit("请先用环境变量 OUT 指定结果目录（tools/zircon_report_html.py 的输入与输出目录）。")
OUTP = pathlib.Path(OUT)
FIGDIR = pathlib.Path(os.environ.get("FIGDIR", str(OUTP / "图表")))
DST = OUTP / "锆石四口径对比_图解.html"

FONT = "Microsoft YaHei,PingFang SC,system-ui,sans-serif"
MONO = "Consolas,Cascadia Mono,monospace"

# 主题色（与 HTML 的 :root 保持一致；SVG 里的颜色直接写死，导出成图片也不会丢）
INK, INK2, INK3 = "#2C2C2A", "#5F5E5A", "#888780"
LINE, PANEL = "#D3D1C7", "#F7F6F3"
TEA, RED, VIOLET = "#0F6E56", "#A32D2D", "#534AB7"
C_M1, C_M2, C_M3, C_M4 = "#2c7fb8", "#e6550d", "#31a354", "#756bb1"


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def b64(p):
    return base64.b64encode(pathlib.Path(p).read_bytes()).decode("ascii")


# =============================================================================
# 图 1 —— 流程与四口径对照
# -----------------------------------------------------------------------------
# viewBox 900 x 860，各元素坐标全部显式写死：
#   共同前端三个 chip： x=250..650，y = 26/78/130，高 36，chip 间距 16（箭头占 16px）
#   分叉虚线： y=200，x 40..860；分叉标签 chip 盖在线上
#   四条支路： y = 224 / 364 / 504 / 644，高 126，间距 14 → 末行止于 770
#   结论条：   y=790..842
#   左右留白 40，行内分栏：左侧色条 40..46，文字 62 起；公式栏 62..540；
#                          分隔线 x=560；结果栏 580 起，右对齐边界 844
# =============================================================================
VB1 = "0 0 900 860"
ROWS = [
    dict(y=224, c=C_M1, name="M1　Zr 内标法",
         anchor="锚 ⁹¹Zr ｜ C_IS = 497 700 µg/g",
         f=["C_j = λ_j × cps_j × ( cps_IS(612) ÷ cps_IS(样品) )",
            "λ_j 来自 SRM 612，逐元素独立：λ_j = C_j(证书) ÷ cps_j(612)",
            "同一测点内 K 对所有元素相同，但 38 个 λ_j 互不相干"],
         r1="氧化物总和 111.5 wt%", r1c=RED,
         r2="Si : (Zr+Hf) = 1.235",
         r3="无化学式约束 → 比例无从校正"),
    dict(y=364, c=C_M2, name="M2　Si 内标法",
         anchor="锚 ²⁹Si ｜ C_IS = 153 200 µg/g",
         f=["C_j = λ_j × cps_j × ( cps_IS(612) ÷ cps_IS(样品) )",
            "公式与 M1 逐字相同，唯一区别是锚元素换成 ²⁹Si",
            "换锚 → 全部元素的绝对值整体平移，比例关系不变"],
         r1="氧化物总和 89.2 wt%", r1c=TEA,
         r2="Si : (Zr+Hf) = 1.231",
         r3="同样无化学式约束"),
    dict(y=504, c=C_M3, name="M3　归一法 fixed Si:1",
         anchor="化学式约束：Si = 1 apfu ｜ 电荷配平",
         f=["① C′_j = λ_j × cps_j　→　n_j = C′_j ÷ M_j（各元素摩尔数）",
            "② F_f = 电荷目标 ÷ Σ( n_j × z_j )　→　C_j = n_j × F_f × M_j",
            "电荷目标 = 2×4 − 4×1(Si) = 4　Zr/U/Th/REE 分摊剩余电荷"],
         r1="氧化物总和 100.0 wt%", r1c=TEA,
         r2="Si : (Zr+Hf) = 1.016",
         r3="同时锁死总量与 Si/Zr 分配"),
    dict(y=644, c=C_M4, name="M4　归一法 anhydrous",
         anchor="化学式约束：仅总量电荷配平",
         f=["① C′_j = λ_j × cps_j　→　n_j = C′_j ÷ M_j（各元素摩尔数）",
            "② F_f = 电荷目标 ÷ Σ( n_j × z_j )　→　C_j = n_j × F_f × M_j",
            "电荷目标 = 2×n_O = 8　不固定 Si，Si 与 Zr 可自由置换"],
         r1="氧化物总和 100.0 wt%", r1c=TEA,
         r2="Si : (Zr+Hf) = 1.249",
         r3="总量闭合，但不约束 Si/Zr 分配"),
]


def chip(y, text, fill="#FFFFFF"):
    return (f'<rect x="250" y="{y}" width="400" height="36" rx="8" '
            f'fill="{fill}" stroke="{LINE}"/>'
            f'<text x="450" y="{y + 23}" font-size="12.5" fill="{INK}" '
            f'text-anchor="middle">{esc(text)}</text>')


def down_arrow(y0, y1):
    """从 y0 到 y1 的竖直箭头（y1 - y0 约 16px）。"""
    return (f'<line x1="450" y1="{y0}" x2="450" y2="{y1 - 6}" stroke="{INK3}" stroke-width="1.6"/>'
            f'<path d="M450 {y1} L445.5 {y1 - 8} L454.5 {y1 - 8} Z" fill="{INK3}"/>')


def row_svg(r):
    y, c = r["y"], r["c"]
    s = []
    s.append(f'<rect x="40" y="{y}" width="820" height="126" rx="10" fill="#FFFFFF" stroke="{LINE}"/>')
    s.append(f'<rect x="40" y="{y}" width="6" height="126" rx="3" fill="{c}"/>')
    s.append(f'<text x="62" y="{y + 28}" font-size="13.5" font-weight="600" fill="{INK}">{esc(r["name"])}</text>')
    s.append(f'<text x="844" y="{y + 28}" font-size="11" fill="{INK2}" text-anchor="end">{esc(r["anchor"])}</text>')
    s.append(f'<line x1="62" y1="{y + 40}" x2="844" y2="{y + 40}" stroke="#EFEEE9" stroke-width="1"/>')
    s.append(f'<line x1="560" y1="{y + 52}" x2="560" y2="{y + 114}" stroke="#EFEEE9" stroke-width="1"/>')
    for i, line in enumerate(r["f"]):
        s.append(f'<text x="62" y="{y + 62 + i * 21}" font-size="12" font-family="{MONO}" '
                 f'fill="{INK if i == 0 else INK2}">{esc(line)}</text>')
    s.append(f'<text x="580" y="{y + 62}" font-size="13" font-weight="600" fill="{r["r1c"]}">{esc(r["r1"])}</text>')
    s.append(f'<text x="580" y="{y + 84}" font-size="11.5" fill="{INK2}">{esc(r["r2"])}</text>')
    s.append(f'<text x="580" y="{y + 106}" font-size="11" fill="{INK3}">{esc(r["r3"])}</text>')
    return "\n".join(s)


SVG1 = f"""<svg viewBox="{VB1}" font-family="{FONT}" role="img"
     aria-label="四条归算支路的流程与结果对照">
  <rect width="900" height="860" rx="12" fill="{PANEL}"/>
  {chip(26, "① 读入 54 个 Qtegra CSV（每文件一个测点）")}
  {down_arrow(62, 78)}
  {chip(78, "② 背景窗扣除 + 信号窗积分 → 净计数率 cps")}
  {down_arrow(114, 130)}
  {chip(130, "③ 净计数率矩阵：42 个同位素通道 × 54 个文件")}
  <text x="450" y="176" font-size="11.5" fill="{INK3}" text-anchor="middle">四条支路到这一步完全相同 —— 差别全部发生在下一步：如何把 cps 换成 µg/g</text>
  <line x1="40" y1="200" x2="860" y2="200" stroke="{LINE}" stroke-width="1.2" stroke-dasharray="5 4"/>
  <rect x="366" y="188" width="168" height="22" rx="11" fill="{PANEL}" stroke="{LINE}"/>
  <text x="450" y="203" font-size="11.5" fill="{INK2}" text-anchor="middle">由此分叉（四条支路）</text>
  {row_svg(ROWS[0])}
  {row_svg(ROWS[1])}
  {row_svg(ROWS[2])}
  {row_svg(ROWS[3])}
  <rect x="40" y="790" width="820" height="52" rx="10" fill="#E1F5EE" stroke="#9FE1CB"/>
  <text x="62" y="814" font-size="12.5" font-weight="600" fill="{TEA}">结论：四条支路里只有 M3 同时通过「总量闭合」与「Si:(Zr+Hf) ≈ 1」两条检验。</text>
  <text x="62" y="833" font-size="11.5" fill="{TEA}">M4 靠总量约束也能闭合到 100%，但它不约束 Si 与 Zr 的分配比例 —— 这是把它挑出来的关键。</text>
</svg>"""

# =============================================================================
# 图 2 —— 约束强度示意：为什么只有归一法能闭合
# -----------------------------------------------------------------------------
# viewBox 900 x 470
#   两个面板： x=30..440 与 x=460..870，y=40..400
#   柱区：基线 y=330；柱宽 26、间距 14，9 根 → 30..440 面板内起点 x=66（右缘 412）
#                                              460..870 面板内起点 x=496（右缘 842）
#   设计高度（96~130）乘 S=1.65 后，柱顶最高到 y=115；面板标题基线在 y=70/90，
#   留 25px 不冲突。画序必须是「引线 → 柱 → 红线」，柱体才会自然盖住内侧那段引线。
# =============================================================================
TRUE_H = [96, 88, 102, 84, 108, 90, 78, 100, 94]
MEAS_L = [112, 70, 128, 66, 88, 118, 56, 130, 74]      # 内标法：偏差无规律
MEAS_R = [round(h * 1.20) for h in TRUE_H]             # 归一法：统一 ×1.20
BASE, S, BW, BGAP, N = 330, 1.65, 26, 14, 9
TRUE_H = [round(h * S) for h in TRUE_H]
MEAS_L = [round(h * S) for h in MEAS_L]
MEAS_R = [round(h * S) for h in MEAS_R]


def panel(x0, x1, title, sub, meas, accent, legend_x):
    s = [f'<rect x="{x0}" y="40" width="{x1 - x0}" height="360" rx="10" fill="#FFFFFF" stroke="{LINE}"/>',
         f'<text x="{x0 + 16}" y="70" font-size="13" font-weight="600" fill="{INK}">{esc(title)}</text>',
         f'<text x="{x0 + 16}" y="90" font-size="11" fill="{INK3}">{esc(sub)}</text>']
    bx = x0 + 36
    for i in range(N):
        h_t, h_m = TRUE_H[i], meas[i]
        x = bx + i * (BW + BGAP)
        y_m, y_t = BASE - h_m, BASE - h_t
        # ① 先画引线（柱体会盖住落在柱内的那一段）
        s.append(f'<line x1="{x + BW / 2}" y1="{min(y_m, y_t)}" x2="{x + BW / 2}" y2="{max(y_m, y_t)}" '
                 f'stroke="{RED}" stroke-width="1" stroke-dasharray="2 2"/>')
        # ② 再画柱
        s.append(f'<rect x="{x}" y="{y_m}" width="{BW}" height="{h_m}" fill="{accent}" opacity="0.85" rx="2"/>')
        # ③ 最后画「应有的比例」红线，压在柱顶之上
        s.append(f'<line x1="{x}" y1="{y_t}" x2="{x + BW}" y2="{y_t}" stroke="{RED}" stroke-width="2.4"/>')
    s.append(f'<line x1="{bx - 8}" y1="{BASE}" x2="{bx + N * (BW + BGAP) - BGAP + 8}" y2="{BASE}" '
             f'stroke="{INK3}" stroke-width="1.4"/>')
    # 图例
    s.append(f'<rect x="{legend_x}" y="362" width="14" height="10" rx="2" fill="{accent}" opacity="0.85"/>')
    s.append(f'<text x="{legend_x + 20}" y="371" font-size="11" fill="{INK2}">本口径算出的比例</text>')
    s.append(f'<line x1="{legend_x + 148}" y1="367" x2="{legend_x + 172}" y2="367" stroke="{RED}" stroke-width="2.4"/>')
    s.append(f'<text x="{legend_x + 178}" y="371" font-size="11" fill="{INK2}">应有的比例</text>')
    return "\n  ".join(s)


SVG2 = f"""<svg viewBox="0 0 900 470" font-family="{FONT}" role="img"
     aria-label="内标法与环境法的约束强度对比示意">
  <rect width="900" height="470" rx="12" fill="{PANEL}"/>
  {panel(30, 440, "内标法：38 个乘数互相独立", "每个元素各自用自己的 λ 反算", MEAS_L, C_M1, 66)}
  {panel(460, 870, "归一法：1 个共享乘数 F_f", "所有元素乘同一个 F_f，比例不被破坏", MEAS_R, C_M3, 496)}
  <text x="450" y="428" font-size="12" fill="{INK2}" text-anchor="middle">内标法：偏差没有规律 —— 有些元素偏高、有些偏低，元素之间的比例没有任何化学约束兜底。</text>
  <text x="450" y="448" font-size="12" fill="{INK2}" text-anchor="middle">归一法：所有元素被同一个乘数抬起，形状不变 —— 闭合与否只取决于化学式约束选得对不对。</text>
</svg>"""

# =============================================================================
# 表：五口径关键数字（数值全部来自 主量氧化物闭合_五口径.csv / SiZr摩尔比_五口径.csv）
# =============================================================================
TAB = """
<table>
<thead><tr><th>口径</th><th>SiO₂</th><th>ZrO₂</th><th>氧化物总和</th>
<th>Si : (Zr+Hf)</th><th>与实验室的中位比</th><th>判定</th></tr></thead>
<tbody>
<tr><td>M1　Zr 内标</td><td class="n">41.06</td><td class="n">67.23</td><td class="n">111.52</td>
<td class="n">1.235</td><td class="n">1.18</td><td>总和不可能 &gt;100 wt%，弃</td></tr>
<tr><td>M2　Si 内标</td><td class="n">32.77</td><td class="n">53.86</td><td class="n">89.23</td>
<td class="n">1.231</td><td class="n">0.97</td><td>总和过低，弃</td></tr>
<tr><td><b>M3　归一法 fixed Si:1</b></td><td class="n">32.38</td><td class="n">64.44</td><td class="n">99.99</td>
<td class="n"><b>1.016</b></td><td class="n">1.30</td><td><b>两条检验都通过，采用</b></td></tr>
<tr><td>M4　归一法 anhydrous</td><td class="n">37.03</td><td class="n">59.98</td><td class="n">99.99</td>
<td class="n">1.249</td><td class="n">1.20</td><td>总量通过，化学计量不通过</td></tr>
<tr><td>实验室 AYCF（ICPMSDataCal）</td><td class="n">31.68</td><td class="n">65.58</td><td class="n">99.96</td>
<td class="n">0.980</td><td class="n">—</td><td>参照（独立软件链）</td></tr>
</tbody>
</table>
<p class="meta" style="margin-top:-6px">单位：wt%（氧化物）／无量纲（摩尔比、中位比）。「与实验室的中位比」= 37 个通道上
本口径中位数 ÷ 实验室中位数的中位值。</p>
"""

# =============================================================================
# 六张实测图：图号 → (文件名, alt 文本, 图注)
# =============================================================================
FIGS = [
    ("F1_主量氧化物闭合.png",
     "主量氧化物闭合堆叠柱状图",
     "主量氧化物闭合法：把 SiO₂、ZrO₂、HfO₂、U+Th、Y+REE 的氧化物加总。"
     "锆石近端元成分是 (Zr,Hf)SiO₄，加总必须 ≈ 100 wt%。"
     "Zr 内标 111.5 wt%（物理上不可能）、Si 内标 89.2 wt%（缺 10.8），"
     "两支归一法与实验室都落在 100.0。"),
    ("F2_SiZr摩尔比.png",
     "Si 与 Zr+Hf 的摩尔比条形图",
     "比总量闭合更灵敏的判据。理想锆石 Si : (Zr+Hf) = 1.000。"
     "归一法 fixed Si:1 得 1.016、实验室 0.980，其余三个口径都在 1.23～1.25 —— "
     "它们把 Si 相对 Zr 抬高了约 23%。"),
    ("F3_逐元素偏差.png",
     "37 个元素相对实验室的偏差横条图",
     "M3 归一法 ÷ 实验室，按 log₂ 排序。绝对值横条上的数字是实际倍数。"
     "多数元素落在 1.0～1.5 倍；紫色是已知多原子离子干扰通道（Nb、Ta、Tb、Pb、Ca、W），"
     "灰色是实验室值已接近检出限（Mo、Ni、In、Sn、Ag），这两类不要当准确度看。"),
    ("F4_口径元素热图.png",
     "四个口径乘以 37 个元素的偏差热图",
     "四个口径的偏差指纹一览。同一列上下四格如果颜色相近，说明该元素的相对关系比较稳；"
     "最右侧 Ni、Mo 两列的灰底 n.d. 表示该口径下中位数为 0（元素近检出限），比值本身没有意义。"),
    ("F5_稀土配分.png",
     "稀土元素球粒陨石标准化配分曲线",
     "五条曲线形状完全重合，只差一个整体乘数（已标在图例里）。"
     "这说明四种口径都没有改变稀土的相对配分 —— 差别只是整体的绝对水平。"),
    ("F6_偏差分布箱线.png",
     "四个口径偏置分布的箱线图",
     "箱是四分位、线是中位。Si 内标中位 −0.05（几乎贴着实验室），"
     "但它同时是 F1 里总和只有 89.2 的那个口径 —— "
     "「中位数接近实验室」并不等于「数值可用」，必须和闭合检验一起看。"),
]

fig_html = []
for i, (fn, alt, cap) in enumerate(FIGS, start=3):
    p = FIGDIR / fn
    if not p.exists():
        sys.exit(f"缺少图片：{p}（先跑 tools/zircon_compare_plots.R）")
    fig_html.append(
        f'<figure>\n'
        f'  <img src="data:image/png;base64,{b64(p)}" alt="{esc(alt)}">\n'
        f'  <figcaption><b>图 {i}</b>　{esc(cap)}</figcaption>\n'
        f'</figure>')

HTML = f"""<!DOCTYPE html>
<html lang="zh-CN">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>锆石微量元素：四种处理方式的对比</title>
<style>
  :root{{
    --ink:#2C2C2A; --ink2:#5F5E5A; --ink3:#888780;
    --line:#D3D1C7; --bg:#FFFFFF; --panel:#F7F6F3; --panel2:#EFEEE9;
    --pur:#534AB7; --purbg:#EEEDFE; --purline:#AFA9EC;
    --tea:#0F6E56; --teabg:#E1F5EE; --tealine:#9FE1CB;
    --amb:#854F0B; --ambbg:#FAEEDA; --ambline:#FAC775;
    --red:#A32D2D; --redbg:#FDF6F6;
  }}
  *{{box-sizing:border-box}}
  body{{margin:0;background:var(--bg);color:var(--ink);
       font-family:"Microsoft YaHei","PingFang SC",system-ui,sans-serif;
       font-size:15px;line-height:1.78}}
  .wrap{{max-width:940px;margin:0 auto;padding:56px 32px 110px}}
  h1{{font-size:28px;font-weight:600;margin:0 0 8px}}
  .sub{{color:var(--ink2);font-size:15px;margin:0 0 8px}}
  .meta{{color:var(--ink3);font-size:13px;margin:0 0 34px}}
  h2{{font-size:21px;font-weight:600;margin:58px 0 14px;padding-top:24px;
     border-top:1px solid var(--line)}}
  h2:first-of-type{{border-top:none;padding-top:0}}
  h3{{font-size:16.5px;font-weight:600;margin:32px 0 10px}}
  code{{font-family:Consolas,monospace;font-size:13.5px;
       background:var(--panel);padding:1px 5px;border-radius:4px;color:#26215C}}
  table{{border-collapse:collapse;width:100%;margin:0 0 18px;font-size:13.5px}}
  th,td{{border:1px solid var(--line);padding:8px 11px;text-align:left;vertical-align:top}}
  th{{background:var(--panel);font-weight:600;white-space:nowrap}}
  td.n{{white-space:nowrap;font-family:Consolas,monospace;font-size:13px}}
  .lead{{background:var(--panel);border-left:3px solid var(--pur);
        border-radius:0 10px 10px 0;padding:16px 20px;margin:0 0 30px}}
  .note{{border-left:3px solid var(--ambline);background:#FFFBF3;
        border-radius:0 10px 10px 0;padding:14px 18px;margin:0 0 18px;font-size:14px}}
  .warn{{border-left:3px solid #E24B4A;background:var(--redbg);
        border-radius:0 10px 10px 0;padding:14px 18px;margin:0 0 18px;font-size:14px}}
  .ok{{border-left:3px solid var(--tealine);background:var(--teabg);
      border-radius:0 10px 10px 0;padding:14px 18px;margin:0 0 18px;font-size:14px}}
  figure{{margin:28px 0 32px}}
  figure svg,figure img{{display:block;width:100%;height:auto;border-radius:12px}}
  figcaption{{color:var(--ink2);font-size:13px;margin-top:10px;line-height:1.65}}
  ul{{padding-left:22px}} li{{margin:6px 0}}
</style>
</head>
<body>
<div class="wrap">

<h1>锆石微量元素：四种处理方式的对比</h1>
<p class="sub">同一批 54 个原始文件（30 个锆石测点 + SRM 612 + 三块 QC 玻璃），四条归算链跑同一个数据。</p>
<p class="meta">数据批次 batch-A　·　图 1–2 为流程示意，图 3–8 为实测结果</p>

<div class="lead">
<b>一句话结论：</b>四种方式里只有 <b>M3「归一法 fixed Si:1」</b>能用 ——
它是唯一同时让氧化物总和回到 100 wt%、又把 Si:(Zr+Hf) 摩尔比压在 1.000 附近的口径。
另外三个口径分别错在「总和不可能」「总和过低」和「总量对了但 Si 与 Zr 的比例没被约束」。
</div>

<h2>一、处理流程：四条支路从哪里分叉</h2>
<p>四条支路的前半段是同一段代码，分叉点只有一个：<b>怎么把净计数率（cps）换成 µg/g</b>。
内标法选一个已知含量的锚元素去缩放；归一法用一个化学式约束去缩放。</p>

<figure>
{SVG1}
<figcaption><b>图 1</b>　流程与四口径对照。四行的公式、约束、结果并排放，差别一眼可见：
前两行只动一个锚元素，后两行用化学式因子 F_f 同时缩放所有元素。右栏的「氧化物总和」和
「Si:(Zr+Hf)」是判定用的两个数字。</figcaption>
</figure>

<p>两类的根本区别是<b>约束数量</b>：内标法有 38 个互相独立的乘数（λ_j），
归一法只有 1 个乘数（F_f）作用在全部元素上。</p>

<figure>
{SVG2}
<figcaption><b>图 2</b>　约束强度示意（示意图，不是实测数据）。左侧内标法：每个元素各自
缩放，偏差方向无规律，元素的相对比例没有化学约束兜底。右侧归一法：所有元素乘同一个 F_f，
相对比例被完整保留 —— 所以一旦 F_f 的约束选错（如 M4 不固定 Si），比例错得再好也看不出来。</figcaption>
</figure>

<h2>二、用哪两个数字判定</h2>
<div class="ok">
<b>① 主量氧化物总和</b>：锆石近端元是 (Zr,Hf)SiO₄，SiO₂+ZrO₂+HfO₂+U+Th+Y+REE 的氧化物加总<b>必须 ≈ 100 wt%</b>。<br>
<b>② Si : (Zr+Hf) 摩尔比</b>：理想锆石恒等于 <b>1.000</b>。这条比总量闭合灵敏得多 ——
M4 的总量也闭合到 100%，但它不约束 Si 与 Zr 怎么分，所以这条一下子就被挑出来。
</div>

{TAB}

<h2>三、实测结果</h2>

{fig_html[0]}

{fig_html[1]}

{fig_html[2]}

{fig_html[3]}

{fig_html[4]}

{fig_html[5]}

<h2>四、四条要点</h2>
<div class="ok">
<b>1. 只有 M3 站得住。</b>内标法的两个口径给出的总量一个 111.5、一个 89.2 wt%，
物理上都无法成立；M4 虽然也闭合到 100%，却把 Si:(Zr+Hf) 推到 1.249，
说明它只是把总量摊平，没有约束住 Si 与 Zr 的分配。
</div>
<div class="ok">
<b>2. 换锚元素只改整体水平，不改比例。</b>M1 与 M2 的公式逐字相同，只换了锚：
Zr 内标的总量 111.5、Si 内标 89.2，差出一个整体倍率；两者的 Si:(Zr+Hf) 却分别是
1.235 与 1.231，几乎一样。所以「换个内标元素」解决不了比例失真的问题。
</div>
<div class="note">
<b>3. 归一法保证的是「内部自洽」，不是「绝对准确」。</b>M3 的绝对值中位比实验室高约 30%
（图 8），这个系统项来自 λ —— λ 是用 SRM 612 玻璃标样定的，而样品是锆石基体，
属于跨基体定标。要修只能引入锆石基体标样，不是靠换算法能解决的。
</div>
<div class="warn">
<b>4. 「中位数接近实验室」不能当成可用的证据。</b>Si 内标（M2）的逐元素偏差中位数是 −0.05，
几乎贴着实验室的 0，但它的主量总和只有 89.2 wt%。偏差中位数只反映「整体平移了多少」，
掩盖了元素间的比例失真 —— 必须和闭合检验、摩尔比检验一起看。
</div>

<h2>五、结论</h2>
<p>报告锆石微量元素时采用 <b>M3（<code>termite_run_formula(cfg, "zircon")</code>，fixed Si:1）</b>。
相对配分（图 7）在四个口径下完全一致，可以放心用；绝对含量受 SRM 612 跨基体定标限制，
系统偏高约 30%，需要在方法学说明里写清楚，或加锆石基体标样重新定 λ。</p>

<p class="meta">本例所有数值由 <code>tools/zircon_methods.R</code>、<code>tools/zircon_compare.R</code>、
<code>tools/zircon_compare_plots.R</code> 现算，未经手工修改。</p>

</div>
</body>
</html>
"""

DST.write_text(HTML, encoding="utf-8", newline="\n")
print(f"已写出：{DST}")
print(f"体积：{DST.stat().st_size / 1024:.1f} KB")
print(f"内嵌图片：{len(FIGS)} 张　手绘 SVG：2 张")
