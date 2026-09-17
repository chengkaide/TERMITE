# 原始 TERMITE 脚本的问题清单与修改说明

本文记录在把原始脚本（`your_main_directory/` 中的 4 个 `.r` 文件，共 1150 行）重构为
`R/termite_core.R` 的过程中，实际**复现并验证过**的问题，以及每一处的处理方式。

所有"影响"一栏的结论都来自实测，不是代码阅读的推测。
复现方法：`Rscript tests/test-parity.R`。

---

## 一、致命问题（脚本直接跑不起来）

### F1 · 线扫描脚本中途卸载了 `matrixStats`，导致必然崩溃

**位置**：`TERMITEScriptFolder/Correction_Script_line_scan_TERMITE.r:190`

```r
write.table(round(colMedians(w.LoD,na.rm=TRUE), digits=5), ... )
detach("package:matrixStats", unload=TRUE)      # ← 这里
...
background <- matrix(colMedians(sample.linescan[...]), ...)   # ← 148 行之后还要用
```

**现象**（实测）：

```
Error in colMedians(sample.linescan[first.blankValue.linescan:last.blankValue.linescan, :
  could not find function "colMedians"
Execution halted
```

**影响**：线扫描模式**从头到尾无法产出 `Results_*_linescan.csv`**。
仓库 `Results/` 里那份线扫描结果，必然来自更早的脚本版本。

**处理**：延后到脚本末尾再 detach（重构版根本不需要 detach，因为不加载该包）。

---

### F2 · 线扫描把 `data.frame` 传给了只接受矩阵的 `colMedians`

**位置**：同上，第 440 行；根源在第 398–399 行

```r
sample.linescan <- as.data.frame(EWerte[[1]])          # ← 变成了 data.frame
...
background <- matrix(colMedians(sample.linescan[first.blankValue.linescan:last.blankValue.linescan,]), ...)
```

`matrixStats::colMedians()` 要求 `matrix` 或 `vector`；`data.frame[i, ]` 取出来仍是 `data.frame`。

**现象**（实测，把 F1 修掉之后立刻撞上）：

```
Error ... Argument 'x' must be a matrix or a vector.
Calls: source ... eval -> eval -> matrix -> colMedians
```

**为什么点分析脚本没这个问题**：点分析里 `LISTE[[i]]` 是被 `sapply()` 转过的 **matrix**，
所以 `colMedians(LISTE[[i]][rows, ])` 合法。两个脚本在这一点上并不对称。

**处理**：重构版内部一律用 matrix；读取函数返回矩阵。

修掉 F1、F2，并把几处 `data.frame` 与 `matrix` 的隐式运算显式化之后，
线扫描脚本的输出与仓库自带的 `Results_your_sample_name_linescan.csv` **逐位一致**
（6748 行 × 13 列，最大相对偏差 0）。

---

### F3 · 加载了从未使用的 `miscTools`，未安装该包时脚本死在第 1 行

**位置**：`TERMITE_spotscan.r:22`、`TERMITE_linescan.r:23`

```r
if (!require("miscTools")) install.packages("miscTools", dependencies = TRUE)
```

**实测**：本机 R 4.2.2 未装 `miscTools`，于是

```
Error in contrib.url(repos, "source") :
  trying to use CRAN without setting a mirror
```

**并且**：对全部 4 个 `.r` 文件做过检索，`miscTools` 只出现在这一行 `require` 里，
**没有任何地方调用它的函数**。四个实际用到的函数 `colSds` / `colMedians` / `colMeans`
分别来自 `matrixStats` 和 `base`。

**处理**：删除该依赖。重构版只需要 `base`，连 `matrixStats` / `ggplot2` / `reshape2` 都不再需要。

---

## 二、数值/逻辑缺陷

### F4 · 文本清洗的正则会把负号吃掉

**位置**：`Correction_Script_spot_scan_TERMITE.r:133`、`Correction_Script_line_scan_TERMITE.r:127`

```r
as.numeric(gsub("[^E0-9.0-9]", "NA", x))
```

字符类 `[^E0-9.0-9]` 里 `0-9` 写了两次（无害），但**集合里没有 `+` 和 `-`**，
于是负号被替换掉：

| 输入 | 结果 |
|---|---|
| `-1.5` | `"1.5"` → `1.5`（**符号丢失**） |
| `1,234` | `"1NA234"` → `NA` |
| `1.2E+05` | `"1.2E05"` → `120000` |

示例数据全为正值，所以这个 bug 不会显现；但背景扣除后的中间量是会出现负号的，
一旦有人把"已扣背景"的数据回灌进来，符号就被静默改掉。

**处理**：保留 `0-9 e E . + -` 五个类别，其余字符才剔除。

---

### F5 · 分辨率后缀写死成 `"(LR)"`，`resolution` 变量在关键位置被忽略

**位置**：`Correction_Script_line_scan_TERMITE.r:384`

```r
Kopf <- gsub("(LR)","",Kopf,fixed=TRUE)     # 写死
```

而脚本第 90 行读取表头时用的是变量：

```r
Kopf <- gsub(resolution,"",Kopf,fixed=TRUE)  # 用变量
```

同一个文件里两种写法。用户把 `resolution` 改成 `"(MR)"` 之后，
前半段正常、后半段失效，样品表头会残留 `(MR)`，与参考物质表头对不上。

**处理**：统一使用 `resolution` 参数。同时增加表头一致性检查，不一致时给出警告。

---

### F13 · 参考表里 In-115 被误写成 In-116

**位置**：`TERMITEScriptFolder/AtomGewIsoAbund_NIST.csv` 与 `Standards_GeoReM.csv`
的表头、以及 `In113,In116` 两列

**证据**：

- 铟天然只有 **In-113（4.29%）** 与 **In-115（95.71%）** 两个稳定同位素，
  不存在天然 In-116；
- 表中 `In116` 列的丰度恰为 **0.9571**，原子量 114.818 —— 与 In-115 完全一致；
- 而 `In113` 列的丰度 0.0429 也与 In-113 对得上。

**影响**：凡是测了 m/z = 115 的数据，查表直接失败（`同位素数据库缺少 In115`），
整批数据跑不动。

**处理**：不动上游数据文件，改用 `TERMITEScriptFolder/Isotope_aliases.csv`
把 `In115` 指向 `In116` 列，映射理由写在表内注释里。查表时先走别名表，
仍查不到才退回「借用同元素的其他同位素」并给出警告。

---

## 三、设计缺陷（结果可解释性）

### F6 · 报出的"参考物质 LoD"在线扫描时漏掉了第一个参考文件

**位置**：`Correction_Script_line_scan_TERMITE.r:173-177`

```r
w.LoD <- matrix(NA, nrow=(length(Kopf.rows)-length(alleDatenRefs)), ncol=measured.isotopes)
for (i in (length(Kopf.rows)-(length(Kopf.rows)-length(alleDatenRefs))+1):length(Kopf.rows)) {
  w.LoD[i - (length(Kopf.rows)-(length(Kopf.rows)-length(alleDatenRefs))), ] <- LoD[[i]][1, ]
}
```

设 `A = length(Kopf.rows)`（线扫描下就是参考文件数 6），
`B = length(alleDatenRefs)`。注意 `alleDatenRefs` 是一个 **list**，
它的长度是**参考物质的种类数**（示例里只有 NIST612 一种，所以 `B = 1`），
而**不是文件数**。

代入后：`w.LoD` 只有 `A − B = 5` 行，循环从 `B+1 = 2` 开始，**丢掉了第 1 个参考文件**。

对比点分析脚本 `Correction_Script_spot_scan_TERMITE.r:198-201`：那里用的是
`length(alleDatenCount)`，也就是**样品文件数**，代入后恰好等于"取全部参考文件"，是对的。
两个脚本在同一个位置上写法不同，其中一个必然是错的。

**影响**（实测，示例数据）：

- 线扫描剖面中 **242 个单元格**的 NA 状态因为这项修正而改变。
- 也就是 242 个数据点，原本被错误地判为"低于检出限"或"未被判为低于检出限"。

**处理**：默认取全部参考文件；`legacy_lod = TRUE` 可复刻原行为用于逐位比对。

---

### F7 · LoD 公式在 `σ = 0` 时的兜底会摧毁量纲

**位置**：两个 `.r` 文件都有

```r
ifelse(3*colSds(blank)==0, 1, 3*colSds(blank))
```

当空白段计数率完全恒定（`σ = 0`）时，分子被替换成 `1`，于是

```
LoD = 1 / mean(I_j) × C_j^cert
```

这与"3σ"已经没有任何关系，纯粹是一个无量纲的兜底常数。
真实数据里空白段完全恒定几乎不可能，但**合成了空白扣除后的数据、或做了内部平均之后**就会遇到。

**处理**：默认保留 `0`（LoD = 0，即不产生截断）；`legacy_lod = TRUE` 时复刻原行为。

---

### F8 · 线扫描 x 轴与实际数据行错位

**位置**：`Correction_Script_line_scan_TERMITE.r:368-374`

```r
sample.time <- as.numeric(as.character(
  read.table(Daten.linescan, sep=",", skip=Line.of.Signal)[1:(last.sampleValue.linescan-first.sampleValue.linescan+1), 1]))
line.length <- sample.time*laser.speed/1000
```

`sample.time` 取的是**文件最前面** `6748` 行的时间，
而浓度数组对应的却是第 `140 : 6887` 行（`first.sampleValue.linescan = 143` 减去偏移）。
两者相差 139 个 sweep。

由于时间列是等间隔的，这个错位退化成一个**常数平移**：

| | 修正前 | 修正后 |
|---|---|---|
| 起点 | 0.00078 mm | 0.00000 mm |
| 终点 | 4.92688 mm | 4.92610 mm |
| 偏移 | | **+0.78 µm** |

示例数据跑 4.93 mm，0.78 µm 只占 0.016%，肉眼看不出来；
但如果时间列不是等间隔（例如中间改了 dwell time），错位就不再是常数平移，误差会明显放大。

**处理**：按数据行取时间，并以激光开剥第一行归零。`legacy_time_axis = TRUE` 复刻原行为。

---

### F9 · 两个入口脚本对"参考物质积分窗口"的默认值不一致

| 参数 | `TERMITE_spotscan.r` | `TERMITE_linescan.r` |
|---|---|---|
| `first.sampleValue` | 180 | **130** |
| `last.sampleValue` | 440 | **545** |

这不是 bug，但它非常容易被忽略：**拿点分析的窗口去跑线扫描，RSF 会偏约 0.5%**。
本项目在调试阶段就真的因此对不上，排查后才定位到。

**处理**：`termite_defaults("line")` 内置正确的默认值；Shiny 界面切换模式时自动套用。

---

### F10 · `X` 矩阵在文件之间复用时没有清零

**位置**：两个脚本都有

```r
X <- matrix(0, nrow=last.sampleValue, ncol=measured.isotopes)
for (i in seq(along=LISTE)) {
  X[first.sampleValue:last.sampleValue, ] <- ...
  Y[[i]] <- as.data.frame(X[first.sampleValue:last.sampleValue, ])
}
```

每轮都完整覆盖窗口，所以**当前默认参数下没有实际影响**；
但只要 `first.sampleValue` / `last.sampleValue` 在循环内被改动（例如某些文件行数不足），
残留的旧数据就会静默混入。属于脆弱写法。

**处理**：重构版每轮重新构造，且有行数越界检查与显式警告。

---

### F11 · 负值处理在两种模式间不一致

- 点分析：`Y[[i]] <- ifelse(X[...] < 0, 0, X[...])` —— **截断为 0**
- 线扫描：`Y[[i]] <- as.data.frame(X[...])` —— **不截断**

同一份代码库对同一个物理量（背景扣除后的净计数率）采用两种处理，而截断会引入正偏倚。
两种做法都能自圆其说（点分析要取均值所以截断；线扫描逐点出结果所以保留），
但**没有在任何文档中说明**。

**处理**：做成显式开关 `clip_negative`，默认值保持与原脚本一致（点分析开、线扫描关），
并在界面上提示"截断会在低含量处引入正偏倚"。

---

### F12 · 若干死代码与误导性注释

| 位置 | 内容 |
|---|---|
| `Correction_Script_spot_scan_TERMITE.r:118` | `r <- 3`，注释"number of referenceglass-meassurements"，全文件未使用 |
| `Correction_Script_spot_scan_TERMITE.r:315-323` | `if(machine=="Element2"){...} else {...}` 两个分支**完全相同** |
| `Correction_Script_line_scan_TERMITE.r:296` | `#if(machine=="Element2") {` 注释掉的分支 |
| `TERMITE_linescan.r:97` | `sampleValues <- (last.sampleValue-first.sampleValue)+1   # ???` |
| 线扫描脚本 | `detach("package:matrixStats")` 之后还继续用 `colMedians`（见 F1） |

**处理**：重构版删除死代码；等价分支合并。

---

## 四、修改后的对照

| 项目 | 原脚本 | 重构版 |
|---|---|---|
| 外部依赖 | `miscTools`(未用) + `matrixStats` + `ggplot2` + `reshape2` | **无**（只用 base R） |
| 线扫描可用性 | 崩溃 | 正常 |
| 点分析输出 | 正常 | 逐位一致 |
| 线扫描输出 | 崩溃（修补后逐位一致） | 逐位一致（复刻模式） |
| 正负号 | 静默丢弃 | 保留 |
| 分辨率后缀 | 后半段写死 `(LR)` | 统一使用参数 |
| `σ=0` 的 LoD | 替换为常数 1 | 保留 0（可切换） |
| 线扫描 LoD 文件集 | 漏掉首个参考文件 | 全部参考文件（可切换） |
| 线扫描 x 轴 | 错位 0.78 µm | 按数据行对齐（可切换） |
| 参数文档 | 分散在 4 个文件的注释里 | 集中在 `docs/PRINCIPLES.md` |
| 一致性验证 | 无 | `tests/test-parity.R`，逐列比对 |

---

## 五、一个诚实的提醒

这些修改**没有改变任何一条物理/化学假设**。TERMITE 的定量能力仍然受限于它本来的设计：

- 依赖内标元素在样品中均匀分布且含量已知；
- RSF 由参考物质给出，**跨基体不可转移**；
- LoD 借用样品自身的灵敏度，不是严格的检出限定义。

换句话说：**修好脚本不等于结果就可信**。定量结果的可靠性取决于样品与参考物质的匹配程度，
这一点请自行评估，不要因为"测试全绿"就放松对数据的判断。
