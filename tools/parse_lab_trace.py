# =============================================================================
#  解析中国地质大学(武汉) ICPMSDataCal「Contents (AYCF)」报告 → 长表 CSV
#  ---------------------------------------------------------------------------
#  源表：<实验室报告>.xlsx 的 TraceEle 工作表
#    r1: ICPMSDataCal 12.2
#    r2: Contents (AYCF) + 元素名（SiO2 / K / Ca / ... / ZrO2 / ... / Total / Th / U）
#    r3: 质量数（29 / 39 / ...）
#    r4: 单位（wt% / ppm）
#    r5 起：数据。col0 = Rep|std，col1 = 文件名，col2 = 样品名，col4 起是数值
#
#  ★ 坑：同一文件名在表里出现 3 次，只有第 1 次是有效结果，后两次是失败的
#    重复运行（示例 batch-A_8：SiO2 = 33.66 / 1.56 / 0.032 wt%）。
#    所以按「文件名 + 样品名」去重，只取首次出现。
#
#  输出：<OUT>/实验室AYCF参考值.csv（长表）
#    file, sample, element, value_ug_g, unit_raw, col_idx
#  SiO2(wt%) / ZrO2(wt%) 一律换算成元素 µg/g，便于和 TERMITE 结果直接比。
#
#  用法（脚本要进公开仓库，不写死任何本机路径）：
#    LAB_XLSX=<实验室报告.xlsx>  OUT=<结果目录>  python tools/parse_lab_trace.py
# =============================================================================
import csv
import os
import sys
import openpyxl

XLSX = os.environ.get("LAB_XLSX", "")
OUT_DIR = os.environ.get("OUT", "")
if not XLSX or not OUT_DIR:
    raise SystemExit("请先用环境变量 LAB_XLSX 与 OUT 指定实验室报告与结果目录。")
OUT = os.path.join(OUT_DIR, "实验室AYCF参考值.csv")

# 列索引(0基) → (元素, 源单位)。索引 3 是空列。
# In：报告同时给 113In 与 115In 两列，取 115In（丰度 95.7%，与 TERMITE 的主同位素一致）
# Pb：索引 40 是 204Pb，索引 41 列名 r2 写 Pb、r3 写 Total → 取 Total 作元素 Pb
COLS = {
    4:  ("Si", "SiO2_wt%"),
    5:  ("K", "ppm"), 6: ("Ca", "ppm"), 7: ("Ti", "ppm"), 8: ("V", "ppm"),
    9:  ("Fe", "ppm"), 10: ("Ni", "ppm"), 11: ("Cu", "ppm"), 12: ("Zn", "ppm"),
    13: ("Sr", "ppm"), 14: ("Y", "ppm"),
    15: ("Zr", "ZrO2_wt%"),
    16: ("Nb", "ppm"), 17: ("Mo", "ppm"), 18: ("Ag", "ppm"), 20: ("In", "ppm"),
    21: ("Sn", "ppm"),
    22: ("La", "ppm"), 23: ("Ce", "ppm"), 24: ("Pr", "ppm"), 25: ("Nd", "ppm"),
    26: ("Sm", "ppm"), 27: ("Eu", "ppm"), 28: ("Gd", "ppm"), 29: ("Tb", "ppm"),
    30: ("Dy", "ppm"), 31: ("Ho", "ppm"), 32: ("Er", "ppm"), 33: ("Tm", "ppm"),
    34: ("Yb", "ppm"), 35: ("Lu", "ppm"), 36: ("Hf", "ppm"), 37: ("Ta", "ppm"),
    38: ("W", "ppm"), 39: ("Hg", "ppm"), 41: ("Pb", "ppm"),
    42: ("Th", "ppm"), 43: ("U", "ppm"),
}

# 原子量（与 R/termite_formula.R 的 .termite_M_default 保持一致）
M_SI, M_ZR, M_O = 28.0855, 91.224, 15.999
F_SI = M_SI / (M_SI + 2 * M_O)      # SiO2 wt% → Si µg/g 的倍数（再乘 1e4）
F_ZR = M_ZR / (M_ZR + 2 * M_O)

wb = openpyxl.load_workbook(XLSX, read_only=True, data_only=True)
ws = wb["TraceEle"]

rows = []
seen = set()
n_dup = 0
for r in ws.iter_rows(min_row=5, values_only=True):
    if r is None or len(r) < 5:
        continue
    fname = r[1]
    sname = r[2]
    if not fname or not sname:
        continue
    key = str(fname).strip()
    if key in seen:
        n_dup += 1
        continue                      # 只保留首次出现
    seen.add(key)
    for ci, (el, unit) in COLS.items():
        if ci >= len(r):
            continue
        v = r[ci]
        if v is None or v == "":
            continue
        try:
            v = float(v)
        except (TypeError, ValueError):
            continue
        if unit == "SiO2_wt%":
            v = v * F_SI * 1e4         # wt% → µg/g (元素)
        elif unit == "ZrO2_wt%":
            v = v * F_ZR * 1e4
        rows.append([key, str(sname).strip(), el, f"{v:.8g}", unit])
wb.close()

with open(OUT, "w", newline="", encoding="utf-8-sig") as fh:
    w = csv.writer(fh)
    w.writerow(["file", "sample", "element", "value_ug_g", "unit_raw"])
    w.writerows(rows)

samples = sorted({r[1] for r in rows})
print(f"[ok] 文件 {len(seen)} 个（跳过重复行 {n_dup} 个），样品名 {len(samples)} 个，记录 {len(rows)} 条")
print(f"[ok] 写出 {OUT}")
print("     样品名示例：", ", ".join(samples[:8]))
