# TERMITE HTTP API

把 TERMITE 的归算能力开放成 HTTP 接口，供**外部程序 / 智能体**调用。
界面照常可用，两者互不影响。

设计上只依赖 `httpuv` 与 `jsonlite` —— 它们本来就在 `shiny` 的依赖链上，
所以**不需要新装任何包**。API 代码放在 `api/` 而不是 `R/`，因此
`R/` 的「只用 base R + shiny」约定没有被破坏。

---

## 启动

```bat
Rscript api/server.R                          :: 127.0.0.1:8787
Rscript api/server.R --port 8788
Rscript api/server.R --host 0.0.0.0 --token <密钥>   :: 跨机访问，务必设 token
```

> **C 盘紧张时务必设工作目录**：任务状态与结果 CSV 默认落在系统临时目录，
> 线扫描结果可能几十 MB。
> ```bat
> set TERMITE_API_WORKDIR=D:/termite_jobs
> ```

环境变量（命令行参数优先）：`TERMITE_API_WORKDIR`、`TERMITE_API_HOST`、
`TERMITE_API_PORT`、`TERMITE_API_TOKEN`。

启动后先访问 `GET /capabilities`，它把全部端点、参数白名单、枚举取值和矿物清单
一次性给出 —— **智能体读这一个响应就能自己装配调用参数，不必读源码**。

---

## 端点

| 方法 | 路径 | 说明 |
|---|---|---|
| GET | `/health` | 健康检查（版本、工作目录、队列长度） |
| GET | `/capabilities` | 自描述：端点、参数白名单、枚举、矿物清单、注意事项 |
| GET | `/minerals` | 无内标公式法的矿物参数字典 |
| POST | `/plan` | 预检（同步）：文件清单 + 表头口径 + 内标建议 |
| POST | `/jobs` | **提交任务** `{action, params}` → 立即返回 `job_id` |
| GET | `/jobs` | 任务列表 |
| GET | `/jobs/{id}` | 任务状态与进度 |
| GET | `/jobs/{id}/result` | 结果（`?format=json\|csv&table=&limit=`） |
| GET | `/jobs/{id}/log` | 执行日志尾部（`?limit=`）—— 任务卡住时看这里 |
| DELETE | `/jobs/{id}` | 取消运行中的任务 / 删除已结束的任务 |

### 四种 action

| action | 必填 | 说明 | 结果表 |
|---|---|---|---|
| `plan` | `dir` | 只探测不计算：文件清单、表头行/数据行口径、逐文件内标建议 | `files`、`main_isotope` |
| `run` | `dir` | 内标法归算 | `results`、`lod`、`rsf`、`recovery` |
| `formula` | `dir`、`mineral` | 无内标公式法（矿物化学式归一化），只支持 `mode=spot` | `results`、`coverage` |
| `window` | `dir` | 逐点信号曲线扫描 + 自动定窗；**只给建议，不改任何数值** | `windows`、`fold` |

`params` 里可用的配置键见 `GET /capabilities` 的 `config_keys`（共 41 个），
取值枚举见 `enums`。**写错参数名会直接报 400**，不会被静默忽略。

---

## 为什么是异步

`httpuv` 是单线程事件循环。归算一批 40 个样品要几十秒，若在主进程里跑，
整个服务会卡住 —— 连 `/health` 都不响应。所以每个任务都由
`Rscript api/job_runner.R` 起**独立子进程**执行，主进程只读写 JSON 状态文件。
这样同时得到三件事：服务不被阻塞、多个任务可并行、任务崩了不影响服务。

代价是子进程冷启动约 15–25 秒（R 要装载 9 个源文件）。所以提交后
`status` 会先经过 `queued → running → done`，`progress` 字段可用于显示进度。

---

## 调用示例

### curl：跑一批数据

```bash
# 1) 预检：看清目录里有什么、表头在哪一行
curl -s -X POST http://127.0.0.1:8787/plan \
  -H "Content-Type: application/json" \
  -d '{"params":{"dir":"<数据目录>","layout":"flat","auto_detect":true,"dir_sample":""}}'

# 2) 提交归算
curl -s -X POST http://127.0.0.1:8787/jobs \
  -H "Content-Type: application/json" \
  -d '{"action":"run","params":{
        "dir":"<数据目录>","layout":"flat","auto_detect":true,"dir_sample":"",
        "ref_names":["SRM 610","SRM 612"],"qc_names":["BIR-1G","BCR-2G","BHVO-2G"],
        "column_IS":55,"is_conc":638500,
        "first_blank":25,"last_blank":60,"first_signal":70,"last_signal":118}}'
# → {"ok":true,"job":{"job_id":"run-20260101-120000-a1b2c3","status":"queued"}, ...}

# 3) 轮询
curl -s http://127.0.0.1:8787/jobs/run-20260101-120000-a1b2c3

# 4) 取结果
curl -s "http://127.0.0.1:8787/jobs/run-20260101-120000-a1b2c3/result?limit=50"
curl -s -o 结果.csv "http://127.0.0.1:8787/jobs/run-20260101-120000-a1b2c3/result?format=csv&table=results"
```

### Python

```python
import json, time, urllib.request

BASE = "http://127.0.0.1:8787"

def call(method, path, payload=None):
    req = urllib.request.Request(
        BASE + path,
        data=json.dumps(payload).encode() if payload else None,
        headers={"Content-Type": "application/json"},
        method=method)
    return json.load(urllib.request.urlopen(req))

call("POST", "/jobs", {"action": "run", "params": {
    "dir": "<数据目录>", "layout": "flat", "auto_detect": True, "dir_sample": "",
    "ref_names": ["SRM 610", "SRM 612"], "qc_names": ["BIR-1G", "BCR-2G", "BHVO-2G"],
    "column_IS": 55, "is_conc": 638500,
    "first_blank": 25, "last_blank": 60, "first_signal": 70, "last_signal": 118}})

# ... 轮询直到 status == "done" ...
```

### R

```r
job <- .api_job_submit("run", list(dir = "<数据目录>", layout = "flat"))
repeat {
  st <- .api_job_read(job$job_id)
  if (st$status %in% c("done", "error")) break
  Sys.sleep(1)
}
st$summary
```

---

## 语义约定

- **积分窗口一律用文件真实行号**：矩阵第 `r` 行 = 文件第 `r + (signal_line - 1)` 行。
  `/plan` 的 `summary$line_convention` 会把本批的换算关系直接写出来。
- **`window` 只出建议**：`summary$suggested.strict/robust` 里的
  `first_signal` / `last_signal` 已是文件行口径，可直接填回 `run` 任务。
  **改窗口会改变数值**，请确认后再重算 —— 接口不会替你自动重算。
- **`recovery` 表里 `kind=qc` 的行不参与 RSF**：QC 样品只报回收率，
  它偏离 100% 本身是判断基体差异的依据，不是错误。
- **结果是 UTF-8 CSV（带 BOM）**：Excel 双击直接打开不乱码。
- `run` / `formula` 的 `summary$config` 会回抄本次真正生效的参数 ——
  判断两份结果能不能直接比，看它比看请求更可靠。

## 任务卡住时怎么看

`GET /jobs/{id}` 的 `waiting_s` 是「已等待秒数」。子进程冷启动要 15–25 秒，
因此超过 120 秒还停在 `queued` / `running` 基本可以判定执行进程出事了，
这时 `GET /jobs/{id}/log` 会给出该进程的标准输出 —— 崩在启动阶段的原因都在那里。

最典型的一种是**状态文件写不进去**：Windows 下目标文件可能被间歇性占用，
此时 `open(w)` 与 rename 都会报「拒绝访问」。接口对此做了三层降级
（rename 覆盖 → 原地覆写 → 写入侧车 `jobs/final/<id>.json`，读取时自动合并），
并且**任何写入失败都不会中断计算**——计算早就跑完了却被报成「一直在跑」，
是智能体最难排查的一种故障。

## 状态码

| 码 | 含义 |
|---|---|
| 200 | 成功 |
| 202 | 任务还没跑完（`/result` 在 `queued`/`running` 时返回） |
| 400 | 参数写错（端点、action、必填项、取值枚举、参数名） |
| 401 | 开启了 `--token` 但请求头 `X-Auth-Token` 不匹配 |
| 404 | 任务或端点不存在 |
| 409 | 任务以 `error` 结束，`error` 字段是原因 |
| 500 | 服务端内部错误（参数问题请报 400，这里出现 500 说明是代码问题） |
