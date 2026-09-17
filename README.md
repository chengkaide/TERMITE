# TERMITE · 交互式 LA-ICP-MS 微量元素数据归算

把 [TERMITE](https://github.com/)（Simon A. Mischel 等，Version 1）的 R 脚本重写为一个
**纯 base R 内核 + Shiny 交互界面**的工具：解析原始脚本、修掉其中跑不通和算错的地方、
把归算原理写清楚，并保证结果与原始脚本**逐位一致**。

> 原始 TERMITE 是一套用于 LA-ICP-MS 微量元素定量的 R 脚本。它本身可用，但线扫描分支
> 存在若干致命错误，且全部算法细节散落在 1150 行命令式代码的注释里。
> 本项目保留全部算法定义，重写为可复用、可验证、可交互的形式。

---

## 快速开始

```r
# 1. 只需要 shiny，其余全部是 base R
install.packages("shiny")

# 2. 在仓库根目录启动
shiny::runApp(".")
```

浏览器会自动打开。**首次进入会用自带的示例数据自动跑一遍点分析**，直接就能看到结果。

自带示例数据在 `your_main_directory/`，来自原仓库，包含：

| 目录 | 内容 |
|---|---|
| `Rawdata_spotscan/` | 40 个样品的点分析原始文件 |
| `Rawdata_linescan/` | 1 条线扫描剖面 |
| `ReferenceMaterial_spotscan/` | NIST612 × 6、MACS3 × 3 |
| `ReferenceMaterial_linescan/` | NIST612 × 6 |
| `TERMITEScriptFolder/` | 同位素原子量/丰度表、参考物质推荐值表、原始脚本 |

---

## 界面功能

| 页面 | 内容 |
|---|---|
| **总览** | 本次运行的全部参数快照、警告信息、文件清单、同位素表 |
| **原始信号** | 逐同位素的 cps 时序图（可对数轴），标出空白区与信号区；背景柱状图 |
| **校准 RSF** | 逐文件 RSF 折线 + 均值线、各参考物质的回收率校核、RSF 表与导出 |
| **检出限 LoD** | LoD 图、LoD 表与导出 |
| **结果** | 点分析散点图（RSD 异常点标红）/ 线扫描浓度剖面；结果表与 CSV 导出 |
| **原理** | 从 cps 到 µg/g 的完整推导，含每一步的假设与陷阱 |
| **验证** | 一键重跑与原脚本的逐位一致性自检 |

侧栏参数分 6 组折叠：数据与仪器、行列与内标、积分窗口、离群检验、兼容与修正、高级。

---

## 数值一致性：这是本项目最重要的一步

`tests/test-parity.R` 会把内核在**复刻模式**下重跑，并与 `tests/reference/` 里的
基准输出逐列比对（基准输出由**原始脚本**在示例数据上生成）。

```
$ Rscript tests/test-parity.R

  浓度表 Results_*_spotscan.csv
  列               n  最大相对偏差  判定
  Mg              40    0.000e+00   OK
  Al              39    0.000e+00   OK
  ...
  RSF 表 RSFused_*_spotscan.csv      12    0.000e+00   OK
  LoD 表 LoD_*_spotscan.csv          12    0.000e+00   OK

  浓度剖面 Results_*_linescan.csv
  length_mm     6748    0.000e+00   OK
  Mg            6748    0.000e+00   OK
  ...
  结果：全部通过
```

**判定标准**：NA 模式必须完全一致，最大相对偏差 ≤ 1e-9。
点分析 40×12 单元格、线扫描 6748×12 单元格，全部为 `0.000e+00`。

> 线扫描的基准是先把原始脚本的 3 处致命/类型错误做**最小修补**后跑出来的；
> 修补后它的输出与仓库自带的 `Results_your_sample_name_linescan.csv` 也逐位一致，
> 说明修补是行为保持的。

另有 `tests/test-app.R` 用 `shiny::testServer()` 把服务端整条反应链跑一遍
（5 组参数、每组 17 项检查），不需要浏览器。

---

## 修了什么

完整清单见 **[docs/FIXES.md](docs/FIXES.md)**，每条都带复现证据。摘要：

| # | 问题 | 影响 |
|---|---|---|
| F1 | 线扫描脚本在第 190 行 `detach("package:matrixStats")`，后面还要用 `colMedians` | **线扫描必然崩溃** |
| F2 | 把 `data.frame` 传给只接受矩阵的 `colMedians` | 修掉 F1 后立刻撞上，**仍然崩溃** |
| F3 | 加载了从未使用的 `miscTools`，未安装时脚本死在第 1 行 | 换台机器就跑不起来 |
| F4 | 文本清洗正则 `[^E0-9.0-9]` 会吃掉负号 | `-1.5` → `1.5`，符号静默改变 |
| F5 | 表头清洗后半段写死 `"(LR)"` 而不用 `resolution` 参数 | 换分辨率后表头对不上 |
| F6 | 线扫描的 LoD 文件选取漏掉第一个参考文件 | 剖面中 **242 个单元格**的 NA 状态被判错 |
| F7 | LoD 公式在 σ=0 时用常数 1 兜底 | 量纲失效 |
| F8 | 线扫描 x 轴与实际数据行错位 | 整条曲线平移 0.78 µm |
| F9 | 两个入口脚本的参考物质积分窗口默认值不同（180–440 vs 130–545） | 用错会偏 0.5% |
| F10 | 复用的 `X` 矩阵不清零 | 当前参数下无影响，属脆弱写法 |
| F11 | 负值处理两种模式不一致且无文档 | 点分析截断（正偏倚）、线扫描不截断 |
| F12 | 死代码：未使用的 `r <- 3`、两个完全相同的 `if/else` 分支等 | 阅读障碍 |

**修正 vs 复刻**：几个会改变数值的修正做成了开关，默认走修正模式，
勾选后可按原始脚本行为运行以便复现旧结果：

- `legacy_lod` —— LoD 文件选取（F6/F7）
- `legacy_time_axis` —— 线扫描 x 轴（F8）
- `clip_negative` —— 负值截断（F11）

---

## 算法原理

完整推导见 **[docs/PRINCIPLES.md](docs/PRINCIPLES.md)**（也可以在应用内的「原理」页阅读）。
一句话版本：

```
净信号 cps → 内标归一 → 同位素丰度/原子量换算（摩尔比→质量比）
           → × 内标含量 → ÷ RSF → LoD 截断 → 浓度 µg/g
```

其中最关键的一步是 **RSF（相对灵敏度因子）**：

```
RSF_j = C_j^未校正(ref) / C_j^推荐值(ref)
C_j   = C_j^未校正(样品) / mean(RSF_j)
```

它把「剥蚀量 × 传输效率 × 电离效率」全部打包成一个由参考物质测出来的经验因子。
**这既是 TERMITE 简洁的原因，也是它不可跨基体转移的原因。**

文档里还详细说明了几个容易踩的坑：

- `column_IS` 是**原始文件列号（含时间列）**，不是同位素序号；
- 内标元素自己输出的浓度**恒等于你输入的 `C_IS`**，它不是测出来的；
- 离群窗口是**围绕中位数的固定百分比**，中位数为 0 时会把所有非零点剔掉 → 输出 NA
  （示例数据里 La、Ce、Y 大面积 NA 就是这个原因）；
- LoD 借用的是**样品自身的灵敏度**，把它当绝对检出限解读要谨慎。

---

## 目录结构

```
app.R                         Shiny 入口
R/
  termite_core.R              归算内核（纯 base R，零外部依赖）
  termite_plots.R             全部绘图（base graphics，不用 ggplot2）
  termite_verify.R            与原脚本的一致性自检
  termite_md.R                极简 Markdown 渲染（不依赖 markdown 包）
  termite_ui.R / _server.R    界面与服务端
docs/
  PRINCIPLES.md               算法原理与推导
  FIXES.md                    原脚本问题清单与修改说明
  ORIGINAL_README.md          原仓库 README（存档）
tests/
  test-parity.R               与原脚本的逐位一致性测试
  test-app.R                  Shiny 服务端集成测试
  reference/                  原始脚本跑出的基准输出
your_main_directory/          自带示例数据 + 原始脚本
```

---

## 环境要求

- **R ≥ 3.6**（推荐 4.x）
- **shiny**（唯一需要的包）

原脚本需要的 `matrixStats` / `ggplot2` / `reshape2` / `miscTools` 全部不再需要。
`colMedians` / `colSds` 用 `apply` 重写，绘图改用 base graphics。

---

## 用于自己的数据

1. 把数据按原脚本的结构摆好：
   ```
   你的目录/
     Rawdata_spotscan/          样品原始文件
     ReferenceMaterial_spotscan/  参考物质原始文件（文件名需含参考物质名，如 NIST612）
     TERMITEScriptFolder/       AtomGewIsoAbund_NIST.csv、Standards_GeoReM.csv
   ```
2. 在界面「数据与仪器」里改**数据主目录**，必要时改子目录名与参考物质名称。
3. 逐项确认**积分窗口**（文件真实行号）——这一项最容易填错，且会影响全部输出。
   建议先在「原始信号」页把窗口调对，再运行归算。

---

## 来源与许可

原始 TERMITE 由 **Simon A. Mischel、Regina Mertz-Kraus、Klaus Peter Jochum、Denis Scholz**
开发（Speleothem Research Group, University of Mainz），Version 1。
参考物质推荐值来自 [GeoReM](http://georem.mpch-mainz.gwdg.de/)，同位素参数来自 NIST。

本仓库是在原脚本基础上的重写与扩展。上游未附明确的许可声明，
因此本仓库沿用上游条款；如需再分发或用于商业用途，请先联系原作者。

如果这个工具对你的工作有帮助，请引用原文：

> Mischel, S. A., Mertz-Kraus, R., Jochum, K. P., & Scholz, D.
> TERMITE: An R script for fast reduction of LA-ICPMS data and its application
> to trace element measurements.
