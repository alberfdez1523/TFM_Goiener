const DATA = {
  summary: "dashboard_eda_summary.csv",
  monthly: "dashboard_eda_monthly.csv",
  hourlyProfile: "dashboard_eda_hourly.csv",
  tariff: "dashboard_eda_tariff.csv",
  clusters: "cluster_profiles.csv",
  clusterBusiness: "cluster_business_mapping.csv",
  clusterSeparators: "cluster_top_separators.csv",
  clusterLeaderboard: "cluster_leaderboard.csv",
  forecastMaster: "forecast_master_leaderboard.csv",
  forecastDailyLeader: "forecast_leaderboard_daily.csv",
  forecastHourlyLeader: "forecast_leaderboard_hourly.csv",
  forecastClusterLeader: "forecast_leaderboard_cluster.csv",
  forecastDaily: "forecast_daily_predictions.csv",
  forecastDailyIntervals: "forecast_daily_intervals.csv",
  forecastHourly: "forecast_hourly_predictions.csv",
  forecastHourlyBusiness: "forecast_hourly_business_impact.csv",
  forecastCluster: "forecast_cluster_predictions.csv",
  forecastSlices: "forecast_error_slices.csv",
  forecastImportance: "forecast_daily_xgb_importance.csv",
  climate: "feature_climate_sensitivity_summary.csv",
  benchmark: "benchmark_results.csv",
  benchmarkDisk: "benchmark_disk_size.csv",
  benchmarkEnv: "benchmark_environment.csv",
  timings: "pipeline_timings.csv"
};

const PALETTE = ["#10b981", "#8b5cf6", "#fbbf24", "#f87171", "#38bdf8", "#ec4899", "#2dd4bf", "#a78bfa"];
const MODEL_COLORS = {
  xgb: "#10b981",
  lgbm: "#fbbf24",
  stack: "#8b5cf6",
  ensemble: "#38bdf8",
  ensemble_top3: "#38bdf8",
  rf: "#ec4899",
  arimax: "#f87171",
  ets: "#a78bfa",
  snaive7: "#2dd4bf",
  snaive24: "#a78bfa",
  snaive168: "#ec4899"
};

let state = {};

main().catch((error) => {
  console.error(error);
  document.querySelector("main")?.insertAdjacentHTML(
    "afterbegin",
    `<div class="empty">No se pudo inicializar el dashboard: ${escapeHtml(error.message)}</div>`
  );
});

async function main() {
  initTabs();
  state = await loadAll();
  await waitForPlotly();
  initControls();
  bindControls();
  renderAll();
}

async function loadAll() {
  const entries = await Promise.all(
    Object.entries(DATA).map(async ([key, file]) => {
      try {
        const response = await fetch(`/data/${file}`);
        if (!response.ok) throw new Error(response.statusText);
        return [key, parseCsv(await response.text())];
      } catch (error) {
        console.warn(`No se pudo cargar ${file}`, error);
        return [key, []];
      }
    })
  );
  return Object.fromEntries(entries);
}

function initTabs() {
  document.querySelector("[data-toggle-nav]")?.addEventListener("click", (event) => {
    const button = event.currentTarget;
    const nav = document.querySelector("#main-navbar");
    const open = !nav?.classList.contains("show");
    nav?.classList.toggle("show", open);
    button.setAttribute("aria-expanded", String(open));
  });

  document.querySelectorAll("[data-tab-link]").forEach((link) => {
    link.addEventListener("click", (event) => {
      event.preventDefault();
      const target = link.dataset.tabLink;
      document.querySelectorAll("[data-tab-link]").forEach((item) => item.classList.remove("active"));
      document.querySelectorAll(`[data-tab-link="${target}"]`).forEach((item) => item.classList.add("active"));
      document.querySelectorAll("[data-tab-panel]").forEach((panel) => {
        panel.classList.toggle("active", panel.dataset.tabPanel === target);
      });
      document.querySelector("#main-navbar")?.classList.remove("show");
      window.scrollTo({
        top: 0,
        behavior: window.matchMedia("(prefers-reduced-motion: reduce)").matches ? "auto" : "smooth"
      });
      setTimeout(resizePlots, 40);
    });
  });

  document.querySelectorAll("[data-subtab-link]").forEach((link) => {
    link.addEventListener("click", () => {
      const target = link.dataset.subtabLink;
      const nav = link.closest(".nav");
      const root = nav?.parentElement || document;
      nav?.querySelectorAll("[data-subtab-link]").forEach((item) => item.classList.remove("active"));
      link.classList.add("active");
      Array.from(root.children)
        .filter((child) => child.matches?.("[data-subtab-panel]"))
        .forEach((panel) => {
          panel.classList.toggle("active", panel.dataset.subtabPanel === target);
        });
      setTimeout(resizePlots, 40);
    });
  });
}

function initControls() {
  setDateInputs("daily", state.forecastDaily, "date", 214);
  setDateInputs("hourly", state.forecastHourly, "datetime", 7);

  const clusters = clusterRows();
  fillSelect("#cluster-picker", clusters.map((row) => row.cluster_label));

  const fcClusters = [...new Set(state.forecastCluster.map((row) => row.cluster).filter(Boolean))]
    .sort((a, b) => num(a) - num(b));
  fillSelect("#cluster-fc-picker", fcClusters);
}

function bindControls() {
  ["#daily-date-start", "#daily-date-end", "#daily-show-band"].forEach((selector) => {
    document.querySelector(selector)?.addEventListener("change", renderForecastDaily);
  });
  document.querySelectorAll("#daily-models input").forEach((input) => {
    input.addEventListener("change", renderForecastDaily);
  });

  ["#hourly-date-start", "#hourly-date-end", "#hourly-show-band"].forEach((selector) => {
    document.querySelector(selector)?.addEventListener("change", renderForecastHourly);
  });
  document.querySelectorAll("#hourly-models input").forEach((input) => {
    input.addEventListener("change", renderForecastHourly);
  });

  document.querySelector("#cluster-picker")?.addEventListener("change", renderClusterDetail);
  document.querySelector("#cluster-fc-picker")?.addEventListener("change", renderForecastCluster);
  window.addEventListener("resize", debounce(resizePlots, 120));
}

function renderAll() {
  renderHome();
  renderEda();
  renderClusters();
  renderForecastDaily();
  renderForecastHourly();
  renderForecastCluster();
  renderForecastMaster();
  renderBenchmarks();
  renderConclusions();
}

function renderHome() {
  const clusters = clusterRows();
  const summary = state.summary[0] || {};
  const climate = climateMap();
  const bestDaily = bestBy(state.forecastDailyLeader, "WAPE");
  const users = sum(clusters, "n") || num(summary.usuarios);
  const clusterCount = clusters.length;
  const betaHdd = climate.beta_hdd_mediana;
  const wape = bestDaily?.WAPE;

  setText("#hero-users", formatInt(users));
  setText("#hero-clusters", formatInt(clusterCount));
  setText("#hero-beta-hdd", formatFixed(betaHdd, 3));
  setText("#hero-wape", formatPercent(wape, 2));

  plotDailyForecast("#home-forecast-plot", state.forecastDaily, {
    models: ["xgb", "ensemble"],
    showBand: true,
    titleY: "kWh / día"
  });

  horizontalBar("#home-leaderboard-plot", state.forecastDailyLeader.slice(0, 8), {
    x: "WAPE",
    y: "model",
    text: (row) => `${formatFixed(row.WAPE, 2)}%`,
    marker: "#10b981",
    xTitle: "WAPE (%)",
    sort: "asc"
  });

  plot("#home-cluster-donut", [{
    type: "pie",
    labels: clusters.map((row) => row.cluster_label),
    values: clusters.map((row) => num(row.n)),
    hole: 0.55,
    marker: { colors: PALETTE },
    textinfo: "percent",
    textposition: "inside",
    insidetextorientation: "radial",
    sort: false,
    hovertemplate: "%{label}<br>%{value:,.0f} usuarios<extra></extra>"
  }], {
    showlegend: true,
    legend: { orientation: "v", x: 1.02, y: 0.5, xanchor: "left", yanchor: "middle" },
    margin: { t: 12, r: 150, b: 12, l: 10 }
  });

  scatterClusters("#home-climate-scatter", clusters, null);
}

function renderEda() {
  const summary = state.summary[0] || {};
  setText("#eda-kpi-records", formatCompact(summary.dias_usuario_completos));
  setText("#eda-kpi-users", formatInt(summary.usuarios));
  setText("#eda-kpi-median", `${formatFixed(summary.mediana_kWh, 1)} kWh`);
  setText("#eda-kpi-p99", `${formatFixed(summary.p99_kWh, 1)} kWh`);

  plot("#eda-monthly-plot", [{
    type: "scatter",
    mode: "lines",
    x: state.monthly.map((row) => row.month),
    y: state.monthly.map((row) => num(row.mean_daily_kWh)),
    name: "Consumo medio",
    line: { color: "#10b981", width: 2.5 },
    fill: "tozeroy",
    fillcolor: "rgba(16, 185, 129, 0.10)",
    hovertemplate: "%{x}<br>%{y:.2f} kWh<extra></extra>"
  }], {
    yaxis: { title: "kWh / día" },
    xaxis: { title: "" }
  });

  plot("#eda-hourly-plot", [{
    type: "scatter",
    mode: "lines+markers",
    x: state.hourlyProfile.map((row) => row.hour),
    y: state.hourlyProfile.map((row) => num(row.mean_kWh)),
    name: "Perfil horario",
    line: { color: "#38bdf8", width: 2.4 },
    marker: { color: "#38bdf8", size: 5 },
    hovertemplate: "Hora %{x}<br>%{y:.4f} kWh<extra></extra>"
  }], {
    yaxis: { title: "kWh" },
    xaxis: { title: "Hora" }
  });

  horizontalBar("#eda-tariff-plot", state.tariff, {
    x: "n",
    y: "tarifa",
    text: (row) => formatInt(row.n),
    marker: "#8b5cf6",
    xTitle: "Contratos",
    sort: "asc"
  });

  edaSummaryPanel("#eda-summary-dt", summary);
}

function renderClusters() {
  const clusters = clusterRows();

  horizontalBar("#cluster-overview-size-plot", clusters, {
    x: "pct",
    y: "cluster_label",
    text: (row) => `${formatFixed(row.pct, 1)}%`,
    marker: "#10b981",
    xTitle: "% usuarios",
    sort: "asc"
  });

  const shapeKeys = [
    ["morning_kWh_share", "Mañana", "#38bdf8"],
    ["afternoon_kWh_share", "Tarde", "#fbbf24"],
    ["evening_kWh_share", "Noche", "#8b5cf6"],
    ["night_kWh_share", "Madrugada", "#10b981"]
  ];
  plot("#cluster-overview-shape-plot", shapeKeys.map(([key, name, color]) => ({
    type: "bar",
    name,
    x: clusters.map((row) => row.cluster_label),
    y: clusters.map((row) => num(row[key])),
    marker: { color },
    hovertemplate: `${name}<br>%{y:.3f}<extra></extra>`
  })), {
    barmode: "stack",
    yaxis: { title: "Share horario" },
    xaxis: { title: "" }
  });

  scatterClusters("#cluster-overview-climate-plot", clusters, null);
  renderClusterDetail();
  table("#cluster-table", state.clusters, [
    ["Cluster", "cluster_label"],
    ["n", "n", formatInt],
    ["pct", "pct", (v) => `${formatFixed(v, 2)}%`],
    ["Media diaria", "mean_daily_kWh", (v) => formatFixed(v, 2)],
    ["Mediana diaria", "median_daily_kWh", (v) => formatFixed(v, 2)],
    ["Ratio noche/día", "ratio_night_day", (v) => formatFixed(v, 3)],
    ["Peak share", "peak_share", (v) => formatFixed(v, 3)],
    ["Beta HDD", "beta_hdd", (v) => formatFixed(v, 3)],
    ["Beta CDD", "beta_cdd", (v) => formatFixed(v, 3)]
  ]);
  highlightClusterTable(document.querySelector("#cluster-picker")?.value);
}

function renderClusterDetail() {
  const selected = document.querySelector("#cluster-picker")?.value || clusterRows()[0]?.cluster_label;
  const row = state.clusters.find((item) => item.cluster_label === selected) || {};

  plot("#cluster-hourly-plot", [{
    type: "bar",
    x: ["Mañana", "Tarde", "Noche", "Madrugada"],
    y: ["morning_kWh_share", "afternoon_kWh_share", "evening_kWh_share", "night_kWh_share"].map((key) => num(row[key])),
    marker: { color: ["#38bdf8", "#fbbf24", "#8b5cf6", "#10b981"] },
    hovertemplate: "%{x}<br>%{y:.3f}<extra></extra>"
  }], {
    yaxis: { title: "Share" },
    xaxis: { title: "" },
    showlegend: false
  });

  metricList("#cluster-indicators", [
    ["Usuarios", formatInt(row.n)],
    ["Peso", `${formatFixed(row.pct, 2)}%`],
    ["Media diaria", `${formatFixed(row.mean_daily_kWh, 2)} kWh`],
    ["Mediana diaria", `${formatFixed(row.median_daily_kWh, 2)} kWh`],
    ["Punta / valle", formatFixed(row.peak_to_valley_ratio, 2)],
    ["Beta HDD", formatFixed(row.beta_hdd, 3)],
    ["Beta CDD", formatFixed(row.beta_cdd, 3)]
  ]);

  scatterClusters("#cluster-climate-plot", clusterRows(), selected);

  horizontalBar("#cluster-separators-plot", state.clusterSeparators.filter((item) => item.cluster_label === selected), {
    x: "std_diff",
    y: "feature",
    text: (item) => formatFixed(item.std_diff, 2),
    marker: (item) => num(item.std_diff) >= 0 ? "#10b981" : "#f87171",
    xTitle: "z-score",
    sort: "abs"
  });

  renderBusinessCard(selected);
  highlightClusterTable(selected);
}

function renderBusinessCard(clusterLabel) {
  const row = state.clusterBusiness.find((item) => item.cluster_label === clusterLabel);
  const host = document.querySelector("#cluster-business-card");
  if (!host) return;
  if (!row) return empty(host);

  host.innerHTML = `
    <article class="card">
      <div class="card-header">
        <i class="fa-solid fa-briefcase" aria-hidden="true"></i>
        ${escapeHtml(row.cluster_label)} ${escapeHtml(row.finalidad || "")}
        <span class="chip ms-2">Prioridad ${escapeHtml(row.prioridad || "n/d")}</span>
      </div>
      <div class="card-body goi-prose">
        <h4>${escapeHtml(row.perfil_cluster || "")}</h4>
        <p>${escapeHtml(row.accion_recomendada || "")}</p>
        <div class="goi-evidence">
          <div class="goi-evidence-label">Evidencia clave</div>
          <div>${escapeHtml(row.evidencia_clave || "")}</div>
        </div>
        <div class="metric-list">
          ${metricRow("Mediana diaria", `${formatFixed(row.median_daily_kWh, 2)} kWh`)}
          ${metricRow("Peak share", formatFixed(row.peak_share, 3))}
          ${metricRow("Valley share", formatFixed(row.valley_share, 3))}
          ${metricRow("β_HDD", formatFixed(row.beta_hdd, 3))}
          ${metricRow("β_CDD", formatFixed(row.beta_cdd, 3))}
          ${metricRow("Core GoiEner", `${formatFixed(row.pct_goiener_core, 1)}%`)}
        </div>
      </div>
    </article>
  `;
}

function renderForecastDaily() {
  const rows = dateFilteredRows(state.forecastDaily, "date", "#daily-date-start", "#daily-date-end");
  const models = checkedValues("#daily-models");
  const showBand = document.querySelector("#daily-show-band")?.checked ?? true;

  plotDailyForecast("#daily-pred-plot", rows, { models, showBand, titleY: "kWh / día" });
  table("#daily-leader-dt", state.forecastDailyLeader, [
    ["Modelo", "model"],
    ["n", "n", formatInt],
    ["MAE", "MAE", (v) => formatFixed(v, 3)],
    ["RMSE", "RMSE", (v) => formatFixed(v, 3)],
    ["MAPE", "MAPE", (v) => formatFixed(v, 3)],
    ["sMAPE", "sMAPE", (v) => formatFixed(v, 3)],
    ["WAPE", "WAPE", (v) => formatFixed(v, 3)],
    ["EUR_dev", "EUR_dev", (v) => formatFixed(v, 0)],
    ["MASE", "MASE", (v) => formatFixed(v, 3)]
  ]);

  horizontalBar("#daily-importance-plot", state.forecastImportance.slice(0, 20), {
    x: "Gain",
    y: "Feature",
    text: (row) => formatFixed(row.Gain, 4),
    marker: "#10b981",
    xTitle: "Gain",
    sort: "asc"
  });

  plot("#daily-slices-plot", [{
    type: "bar",
    x: state.forecastSlices.map((row) => row.slice),
    y: state.forecastSlices.map((row) => num(row.WAPE)),
    marker: {
      color: state.forecastSlices.map((row) => num(row.WAPE)),
      colorscale: [[0, "#10b981"], [1, "#f87171"]]
    },
    text: state.forecastSlices.map((row) => `${formatFixed(row.WAPE, 2)}%`),
    textposition: "outside",
    customdata: state.forecastSlices.map((row) => num(row.MAE)),
    hovertemplate: "%{x}<br>WAPE = %{y:.2f}%<br>MAE = %{customdata:.0f}<extra></extra>"
  }], {
    yaxis: { title: "WAPE (%)" },
    xaxis: { title: "" }
  });
}

function renderForecastHourly() {
  const rows = dateFilteredRows(state.forecastHourly, "datetime", "#hourly-date-start", "#hourly-date-end");
  const models = checkedValues("#hourly-models");
  const showBand = document.querySelector("#hourly-show-band")?.checked ?? true;

  const traces = [];
  if (showBand && rows.some((row) => row.xgb_q05 && row.xgb_q95)) {
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.datetime),
      y: rows.map((row) => num(row.xgb_q95)),
      line: { color: "rgba(16,185,129,0)" },
      showlegend: false,
      hoverinfo: "skip"
    });
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.datetime),
      y: rows.map((row) => num(row.xgb_q05)),
      fill: "tonexty",
      fillcolor: "rgba(16,185,129,0.15)",
      line: { color: "rgba(16,185,129,0)" },
      name: "XGB q05-q95",
      hoverinfo: "skip"
    });
  }
  traces.push({
    type: "scatter",
    mode: "lines",
    x: rows.map((row) => row.datetime),
    y: rows.map((row) => num(row.actual)),
    name: "Real",
    line: { color: "#fafafa", width: 2.2 },
    hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>Real</extra>"
  });
  models.forEach((model) => {
    if (!rows.some((row) => row[model])) return;
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.datetime),
      y: rows.map((row) => num(row[model])),
      name: model,
      line: { color: MODEL_COLORS[model] || "#a1a1aa", width: 1.8 },
      hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>" + model + "</extra>"
    });
  });

  plot("#hourly-plot", traces, {
    yaxis: { title: "kWh / hora" },
    xaxis: { title: "" },
    hovermode: "x unified"
  });

  table("#hourly-leader-dt", state.forecastHourlyLeader, [
    ["Modelo", "model"],
    ["n", "n", formatInt],
    ["MAE", "MAE", (v) => formatFixed(v, 3)],
    ["RMSE", "RMSE", (v) => formatFixed(v, 3)],
    ["MAPE", "MAPE", (v) => formatFixed(v, 3)],
    ["sMAPE", "sMAPE", (v) => formatFixed(v, 3)],
    ["WAPE", "WAPE", (v) => formatFixed(v, 3)],
    ["MASE", "MASE", (v) => formatFixed(v, 3)]
  ]);

  horizontalBar("#hourly-business-plot", state.forecastHourlyBusiness, {
    x: "EUR_dev_total",
    y: "model",
    text: (row) => `${formatFixed(row.EUR_dev_total, 0)} €`,
    marker: "#f87171",
    xTitle: "Desviación económica total (€)",
    sort: "asc"
  });
}

function renderForecastCluster() {
  const selected = document.querySelector("#cluster-fc-picker")?.value || state.forecastCluster[0]?.cluster;
  const rows = state.forecastCluster.filter((row) => String(row.cluster) === String(selected));

  plot("#cluster-fc-plot", [
    {
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.date),
      y: rows.map((row) => num(row.actual)),
      name: "Real",
      line: { color: "#fafafa", width: 2.2 },
      hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>Real</extra>"
    },
    {
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.date),
      y: rows.map((row) => num(row.pred)),
      name: "Predicción",
      line: { color: "#10b981", width: 2.5 },
      hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>Predicción</extra>"
    }
  ], {
    yaxis: { title: "kWh / día" },
    xaxis: { title: "" },
    hovermode: "x unified"
  });

  const leader = state.forecastClusterLeader.find((row) => String(row.cluster) === String(selected)) || {};
  metricList("#cluster-fc-dt", [
    ["Cluster", selected],
    ["Modelo", leader.model || "-"],
    ["MAE", formatFixed(leader.MAE, 3)],
    ["RMSE", formatFixed(leader.RMSE, 3)],
    ["WAPE", `${formatFixed(leader.WAPE, 3)}%`],
    ["MASE", formatFixed(leader.MASE, 3)]
  ]);
}

function renderForecastMaster() {
  table("#master-leader-dt", state.forecastMaster, [
    ["Modelo", "model"],
    ["Target", "target"],
    ["Cluster", "cluster"],
    ["n", "n", formatInt],
    ["MAE", "MAE", (v) => formatFixed(v, 3)],
    ["RMSE", "RMSE", (v) => formatFixed(v, 3)],
    ["WAPE", "WAPE", (v) => formatFixed(v, 3)],
    ["MASE", "MASE", (v) => formatFixed(v, 3)]
  ]);

  scatterLeaderboard("#master-wape-plot", "WAPE", "WAPE (%)");
  scatterLeaderboard("#master-mae-plot", "MAE", "MAE (kWh)");
}

function benchmarkSeconds(row) {
  if (!row) return NaN;
  return Number.isFinite(num(row.warm_mediana_s)) ? num(row.warm_mediana_s) : num(row.mediana_s);
}

function renderBenchmarks() {
  const experiments = [...new Set(state.benchmark.map((row) => row.experimento))];
  const experimentOrder = new Map(experiments.map((experiment, index) => [experiment, index]));
  const benchmarkRows = [...state.benchmark]
    .filter((row) => Number.isFinite(benchmarkSeconds(row)))
    .sort((a, b) => experimentOrder.get(a.experimento) - experimentOrder.get(b.experimento) || benchmarkSeconds(b) - benchmarkSeconds(a));

  plot("#benchmark-time-plot", [{
    type: "bar",
    orientation: "h",
    y: benchmarkRows.map((row) => `${row.experimento} · ${row.metodo}`),
    x: benchmarkRows.map((row) => benchmarkSeconds(row)),
    text: benchmarkRows.map((row) => `${formatFixedEs(benchmarkSeconds(row), benchmarkSeconds(row) < 1 ? 3 : 2)} s`),
    textposition: "outside",
    cliponaxis: false,
    marker: {
      color: benchmarkRows.map((row) => PALETTE[experimentOrder.get(row.experimento) % PALETTE.length])
    },
    customdata: benchmarkRows.map((row) => [row.experimento, row.metodo, row.speedup, row.mem_alloc_mb]),
    hovertemplate: "%{customdata[0]}<br>%{customdata[1]}<br>Mediana templada = %{x:.3f}s<br>Speedup = %{customdata[2]}x<br>Memoria = %{customdata[3]:.1f} MB<extra></extra>"
  }], {
    xaxis: { title: "Mediana templada (s)", title_standoff: 16 },
    yaxis: { title: "", automargin: true, autorange: "reversed", tickfont: { size: 10 } },
    margin: { l: 290, r: 112, t: 12, b: 62 },
    showlegend: false
  });

  horizontalBar("#benchmark-disk-plot", state.benchmarkDisk, {
    x: "GB",
    y: "formato",
    text: (row) => `${formatFixedEs(row.GB, 2)} GB`,
    marker: "#8b5cf6",
    xTitle: "GB",
    sort: "asc"
  });

  renderBenchmarkReading();

  table("#benchmark-env-dt", state.benchmarkEnv, [
    ["Item", "item"],
    ["Value", "value"]
  ]);
}

function renderBenchmarkReading() {
  const host = document.querySelector("#benchmark-reading");
  if (!host) return;

  const csvDisk = state.benchmarkDisk.find((row) => String(row.formato || "").toLowerCase().includes("csv"));
  const parquetHourly = state.benchmarkDisk.find((row) => String(row.formato || "").toLowerCase().includes("parquet horario"));
  const duckRead = state.benchmark.find((row) =>
    row.experimento === "1. Lectura completa" && String(row.metodo || "").toLowerCase().includes("duckdb")
  );
  const arrowRead = state.benchmark.find((row) =>
    row.experimento === "1. Lectura completa" && String(row.metodo || "").toLowerCase().includes("arrow")
  );
  const preagg = state.benchmark.find((row) =>
    row.experimento === "5. Consulta compleja" && String(row.metodo || "").toLowerCase().includes("pre-agregadas")
  );
  const rawQuery = state.benchmark.find((row) =>
    row.experimento === "5. Consulta compleja" && String(row.metodo || "").toLowerCase().includes("horario bruto")
  );
  const memoryLimit = state.benchmarkEnv.find((row) => row.item === "duckdb_memory_limit")?.value || "10GB";

  const diskRatio = Number.isFinite(num(parquetHourly?.ratio_vs_csv)) ? num(parquetHourly.ratio_vs_csv) : num(csvDisk?.MB) / num(parquetHourly?.MB);
  const duckVsArrow = Number.isFinite(num(duckRead?.speedup)) ? num(duckRead.speedup) : benchmarkSeconds(arrowRead) / benchmarkSeconds(duckRead);
  const preaggRatio = Number.isFinite(num(preagg?.speedup)) ? num(preagg.speedup) : benchmarkSeconds(rawQuery) / benchmarkSeconds(preagg);

  host.innerHTML = `
    <div class="benchmark-reading-grid">
      ${benchmarkReadingItem("Compresión", `${formatFixedEs(diskRatio, 1)}x`, "CSV crudo frente a Parquet horario ZSTD-9", "box-archive")}
      ${benchmarkReadingItem("Lectura amplia", `${formatFixedEs(duckVsArrow, 1)}x`, "DuckDB reduce el coste frente a Arrow en lectura completa", "gauge-high")}
      ${benchmarkReadingItem("Pre-agregación", `${formatFixedEs(preaggRatio, 1)}x`, "Las tablas compactas evitan recorrer el horario bruto", "table-cells")}
      ${benchmarkReadingItem("Límite local", memoryLimit, "Suficiente para reproducir el TFM sin Spark", "memory")}
    </div>
    <div class="benchmark-reading-note">
      <i class="fa-solid fa-circle-check" aria-hidden="true"></i>
      <span>Conclusión operativa: Parquet queda como almacenamiento persistente, DuckDB como motor de consulta y las tablas pre-agregadas como capa interactiva del dashboard.</span>
    </div>
  `;
}

function benchmarkReadingItem(label, value, detail, icon) {
  return `
    <div class="benchmark-reading-item">
      <i class="fa-solid fa-${icon}" aria-hidden="true"></i>
      <span>${escapeHtml(label)}</span>
      <strong>${escapeHtml(value)}</strong>
      <small>${escapeHtml(detail)}</small>
    </div>
  `;
}

function renderConclusions() {
  const host = document.querySelector("#conclusions-ui");
  if (!host) return;
  const bestDaily = bestBy(state.forecastDailyLeader, "WAPE");
  const clusterCount = clusterRows().length;
  const users = sum(clusterRows(), "n");
  const medianKwh = state.summary[0]?.mediana_kWh;
  const csvDisk = bestBy(state.benchmarkDisk.filter((row) => String(row.formato || "").toLowerCase().includes("csv")), "MB");
  const parquetDisk = bestBy(state.benchmarkDisk.filter((row) => String(row.formato || "").toLowerCase().includes("parquet")), "MB");
  const intervalRows = state.forecastDailyIntervals.filter((row) =>
    Number.isFinite(num(row.actual)) && Number.isFinite(num(row.conformal_lo)) && Number.isFinite(num(row.conformal_hi))
  );
  const intervalCoverage = intervalRows.length
    ? 100 * intervalRows.filter((row) => num(row.actual) >= num(row.conformal_lo) && num(row.actual) <= num(row.conformal_hi)).length / intervalRows.length
    : NaN;

  const cards = [
    {
      icon: "magnifying-glass-chart",
      tag: "Cartera",
      title: "El consumo no es homogéneo",
      value: `${formatFixedEs(medianKwh, 2)} kWh`,
      detail: "mediana diaria",
      lead: "La memoria confirma asimetría, estacionalidad y crecimiento de cartera; por eso el pipeline filtra, agrega y valida en el tiempo antes de modelar.",
      points: ["segmentar antes de promediar", "top 5 territorial con clima defendible", "test efectivo 2023-07 → 2024-01-24"]
    },
    {
      icon: "layer-group",
      tag: "Clusters",
      title: "Segmentos útiles, no etiquetas personales",
      value: `${formatInt(users)}`,
      detail: `${clusterCount} grupos comparables`,
      lead: "PCA + K-Means convierte forma horaria, estacionalidad y sensibilidad climática en segmentos accionables y auditables.",
      points: ["silhouette + Jaccard + balance", "acción operativa por cluster", "lectura agregada, nunca individual"]
    },
    {
      icon: "chart-line",
      tag: "Forecast",
      title: "Forecast útil para cartera agregada",
      value: formatPercentEs(bestDaily?.WAPE, 2),
      detail: "WAPE diario",
      lead: "En diario domina XGBoost; en horario domina el stack XGBoost-LightGBM. Para compras agregadas, el enfoque top-down es el más defendible.",
      points: ["lags + calendario + HDD/CDD", `conformal ${formatPercentEs(intervalCoverage, 1)}`, "errores en domingo, lunes y mitad de semana"]
    },
    {
      icon: "database",
      tag: "Arquitectura",
      title: "Parquet + DuckDB hacen reproducible el TFM",
      value: `${formatFixedEs(csvDisk?.GB, 2)} → ${formatFixedEs(parquetDisk?.GB, 2)} GB`,
      detail: "CSV crudo vs Parquet horario",
      lead: "La decisión técnica no es estética: permite ejecutar el trabajo en local, medir tiempos y no depender de nube o clusters externos.",
      points: ["pre-agregación como palanca principal", "logs y timings por fase", "orden de magnitud defendible, no segundos absolutos"]
    },
    {
      icon: "shield-halved",
      tag: "Ética",
      title: "La frontera de uso queda declarada",
      value: "Agregado",
      detail: "no individual",
      lead: "La seudonimización no convierte el dato en anónimo fuerte; por eso las salidas son de cartera, cluster o benchmark, no de hogar concreto.",
      points: ["sin representatividad nacional", "sin causalidad fuerte", "sin decisiones intrusivas por usuario"]
    },
    {
      icon: "route",
      tag: "Siguiente fase",
      title: "Mejora donde hay incertidumbre real",
      value: "Monitorizar",
      detail: "deriva y cobertura",
      lead: "La memoria propone reforzar validación estacional, estabilidad de clusters y deriva del intervalo conformal antes de ampliar el alcance.",
      points: ["clima horario y eventos externos", "reentrenos con ventanas múltiples", "métricas de cobertura en producción"]
    }
  ];

  host.innerHTML = `
    <section class="conclusion-hero">
      <div>
        <span class="method-kicker">Síntesis de memoria</span>
        <h2>Evidencia → modelo → intervalo → decisión</h2>
        <p>El TFM queda defendible porque cada afirmación del dashboard puede volver a una tabla, un script y un log reproducible.</p>
      </div>
      <div class="decision-chain" aria-label="Cadena de decisión">
        ${["Datos", "Clusters", "Forecast", "Uso"].map((item, index) => `
          <span><strong>${index + 1}</strong>${item}</span>
        `).join("")}
      </div>
    </section>
    <div class="conclusion-grid">
      ${cards.map(conclusionCard).join("")}
    </div>
    <section class="limits-strip">
      <i class="fa-solid fa-triangle-exclamation" aria-hidden="true"></i>
      <div>
        <strong>Límite explícito de la memoria:</strong>
        resultados válidos para cartera GoiEner 2.0 con clima disponible, no para extrapolar a toda España ni actuar sobre hogares individuales.
      </div>
    </section>
  `;
}

function plotDailyForecast(selector, rows, options) {
  const traces = [];
  if (options.showBand && rows.some((row) => row.conformal_lo && row.conformal_hi)) {
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.date),
      y: rows.map((row) => num(row.conformal_hi)),
      line: { color: "rgba(255,255,255,0)" },
      showlegend: false,
      hoverinfo: "skip"
    });
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.date),
      y: rows.map((row) => num(row.conformal_lo)),
      fill: "tonexty",
      fillcolor: "rgba(255,255,255,0.10)",
      line: { color: "rgba(255,255,255,0)" },
      name: "Conformal 90%",
      hoverinfo: "skip"
    });
  }
  traces.push({
    type: "scatter",
    mode: "lines",
    x: rows.map((row) => row.date),
    y: rows.map((row) => num(row.actual)),
    name: "Real",
    line: { color: "#fafafa", width: 2.5 },
    hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>Real</extra>"
  });
  options.models.forEach((model) => {
    if (!rows.some((row) => row[model])) return;
    traces.push({
      type: "scatter",
      mode: "lines",
      x: rows.map((row) => row.date),
      y: rows.map((row) => num(row[model])),
      name: model === "xgb" ? "XGBoost" : model,
      line: {
        color: MODEL_COLORS[model] || "#a1a1aa",
        width: 2,
        dash: ["ensemble", "stack"].includes(model) ? "dot" : "solid"
      },
      hovertemplate: "%{x}<br>%{y:.0f} kWh<extra>" + escapeHtml(model) + "</extra>"
    });
  });
  plot(selector, traces, {
    yaxis: { title: options.titleY },
    xaxis: { title: "" },
    hovermode: "x unified"
  });
}

function scatterClusters(selector, rows, selected) {
  const selectedRows = selected ? rows.filter((row) => row.cluster_label === selected) : [];
  const baseRows = selected ? rows.filter((row) => row.cluster_label !== selected) : rows;
  const traceFor = (traceRows, options) => ({
    type: "scatter",
    mode: "markers+text",
    x: traceRows.map((row) => num(row.beta_hdd)),
    y: traceRows.map((row) => num(row.beta_cdd)),
    text: traceRows.map((row) => row.cluster_label),
    textposition: "top center",
    marker: {
      color: options.color,
      size: traceRows.map((row) => Math.max(options.minSize, Math.sqrt(num(row.n) || 1) / options.scale + options.extraSize)),
      opacity: options.opacity,
      line: { color: options.lineColor, width: options.lineWidth }
    },
    customdata: traceRows.map((row) => [row.n, row.mean_daily_kWh]),
    hovertemplate: "%{text}<br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<br>n=%{customdata[0]:,.0f}<br>media=%{customdata[1]:.2f} kWh<extra></extra>"
  });

  const traces = [
    traceFor(baseRows, {
      color: "#10b981",
      minSize: 12,
      scale: 2.5,
      extraSize: 0,
      opacity: 0.62,
      lineColor: "rgba(250,250,250,0.65)",
      lineWidth: 1
    })
  ];
  if (selectedRows.length) {
    traces.push(traceFor(selectedRows, {
      color: "#f97316",
      minSize: 20,
      scale: 2.5,
      extraSize: 7,
      opacity: 0.98,
      lineColor: "#ffedd5",
      lineWidth: 3
    }));
  }

  plot(selector, traces, {
    xaxis: { title: "β_HDD" },
    yaxis: { title: "β_CDD" },
    showlegend: false
  });
}

function scatterLeaderboard(selector, key, yTitle) {
  plot(selector, [{
    type: "scatter",
    mode: "markers",
    x: state.forecastMaster.map((row) => row.target),
    y: state.forecastMaster.map((row) => num(row[key])),
    text: state.forecastMaster.map((row) => `${row.model}${row.cluster && row.cluster !== "NA" ? ` / c${row.cluster}` : ""}`),
    marker: {
      color: state.forecastMaster.map((row) => MODEL_COLORS[row.model] || "#10b981"),
      size: 12,
      line: { color: "#fafafa", width: 1 }
    },
    hovertemplate: "%{text}<br>%{x}<br>%{y:.3f}<extra></extra>"
  }], {
    yaxis: { title: yTitle },
    xaxis: { title: "" }
  });
}

function horizontalBar(selector, rows, config) {
  const sorted = [...rows].filter((row) => Number.isFinite(num(row[config.x])));
  if (config.sort === "asc") sorted.sort((a, b) => num(a[config.x]) - num(b[config.x]));
  if (config.sort === "abs") sorted.sort((a, b) => Math.abs(num(a[config.x])) - Math.abs(num(b[config.x])));
  const plotted = sorted.slice(0, config.limit || 20);
  plot(selector, [{
    type: "bar",
    orientation: "h",
    x: plotted.map((row) => num(row[config.x])),
    y: plotted.map((row) => row[config.y]),
    text: plotted.map((row) => config.text ? config.text(row) : formatFixed(row[config.x], 2)),
    textposition: "outside",
    cliponaxis: false,
    marker: {
      color: typeof config.marker === "function" ? plotted.map(config.marker) : config.marker
    },
    hovertemplate: "%{y}<br>%{x:.4f}<extra></extra>"
  }], {
    xaxis: { title: config.xTitle || "" },
    yaxis: { title: "", automargin: true, autorange: "reversed" },
    margin: { l: 120, r: 116, t: 14, b: 48 },
    showlegend: false
  });
}

function plot(selector, traces, layout = {}) {
  const host = document.querySelector(selector);
  if (!host) return;
  if (!traces.length || traces.every((trace) => !trace.x?.length && !trace.labels?.length)) return empty(host);
  if (!window.Plotly) return empty(host, "Plotly no disponible");

  const height = Math.max(host.getBoundingClientRect().height || 0, minHeight(host));
  const base = {
    autosize: true,
    height,
    paper_bgcolor: "rgba(0,0,0,0)",
    plot_bgcolor: "rgba(0,0,0,0)",
    font: { color: "#fafafa", family: "JetBrains Mono, monospace", size: 11 },
    colorway: PALETTE,
    margin: { t: 14, r: 24, b: 44, l: 58 },
    xaxis: {
      gridcolor: "rgba(255,255,255,0.06)",
      zerolinecolor: "rgba(255,255,255,0.10)",
      tickfont: { color: "#a1a1aa" }
    },
    yaxis: {
      gridcolor: "rgba(255,255,255,0.06)",
      zerolinecolor: "rgba(255,255,255,0.10)",
      tickfont: { color: "#a1a1aa" }
    },
    legend: {
      bgcolor: "rgba(0,0,0,0)",
      bordercolor: "rgba(255,255,255,0)",
      font: { color: "#a1a1aa" },
      orientation: "h"
    },
    hoverlabel: { bgcolor: "#18181b", font: { color: "#fafafa" } }
  };

  const merged = {
    ...base,
    ...layout,
    margin: { ...base.margin, ...(layout.margin || {}) },
    xaxis: { ...base.xaxis, ...(layout.xaxis || {}) },
    yaxis: { ...base.yaxis, ...(layout.yaxis || {}) },
    legend: { ...base.legend, ...(layout.legend || {}) }
  };

  window.Plotly.react(host, traces, merged, {
    responsive: true,
    displaylogo: false,
    modeBarButtonsToRemove: ["zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"]
  });
}

function table(selector, rows, columns, options = {}) {
  const host = document.querySelector(selector);
  if (!host) return;
  const data = rows.slice(0, options.limit || rows.length);
  if (!data.length) return empty(host);

  const head = columns.map(([label]) => `<th>${escapeHtml(label)}</th>`).join("");
  const body = data.map((row) => {
    const cells = columns.map(([, key, formatter]) => {
      const value = formatter ? formatter(row[key]) : row[key];
      return `<td>${escapeHtml(value ?? "")}</td>`;
    }).join("");
    return `<tr>${cells}</tr>`;
  }).join("");

  host.innerHTML = `<table class="dataTable goi-datatable"><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table>`;
}

function metricList(selector, rows) {
  const host = document.querySelector(selector);
  if (!host) return;
  host.innerHTML = rows.map(([label, value]) => metricRow(label, value)).join("");
}

function edaSummaryPanel(selector, summary) {
  const host = document.querySelector(selector);
  if (!host) return;
  if (!summary || !Object.keys(summary).length) return empty(host);

  const items = [
    {
      label: "Cobertura",
      value: formatInt(summary.dias_usuario_completos),
      detail: "días-usuario completos",
      icon: "calendar-check"
    },
    {
      label: "Usuarios",
      value: formatInt(summary.usuarios),
      detail: "identificadores válidos",
      icon: "users"
    },
    {
      label: "Ventana",
      value: `${summary.fecha_min || "-"} → ${summary.fecha_max || "-"}`,
      detail: "periodo observado",
      icon: "timeline"
    },
    {
      label: "Consumo típico",
      value: `${formatFixed(summary.mediana_kWh, 2)} kWh`,
      detail: `media ${formatFixed(summary.media_kWh, 2)} kWh`,
      icon: "bolt"
    },
    {
      label: "Cola alta",
      value: `${formatFixed(summary.p99_kWh, 2)} kWh`,
      detail: `P90 ${formatFixed(summary.p90_kWh, 2)} kWh`,
      icon: "arrow-trend-up"
    },
    {
      label: "Total",
      value: formatInt(summary.kWh_total),
      detail: "kWh agregados",
      icon: "gauge-high"
    }
  ];

  host.innerHTML = `
    <div class="eda-summary-grid">
      ${items.map((item) => `
        <div class="eda-summary-item">
          <i class="fa-solid fa-${item.icon}" aria-hidden="true"></i>
          <span>${escapeHtml(item.label)}</span>
          <strong>${escapeHtml(item.value)}</strong>
          <small>${escapeHtml(item.detail)}</small>
        </div>
      `).join("")}
    </div>
  `;
}

function metricRow(label, value) {
  return `<div class="metric-row"><span>${escapeHtml(label)}</span><strong>${escapeHtml(value ?? "-")}</strong></div>`;
}

function conclusionCard(card) {
  return `
    <article class="conclusion-card">
      <div class="conclusion-card-top">
        <span><i class="fa-solid fa-${card.icon}" aria-hidden="true"></i> ${escapeHtml(card.tag)}</span>
        <strong>${escapeHtml(card.value)}</strong>
      </div>
      <h3>${escapeHtml(card.title)}</h3>
      <p>${escapeHtml(card.lead)}</p>
      <div class="conclusion-card-detail">${escapeHtml(card.detail)}</div>
      <div class="conclusion-points">
        ${card.points.map((point) => `<span>${escapeHtml(point)}</span>`).join("")}
      </div>
    </article>
  `;
}

function highlightClusterTable(selected) {
  document.querySelectorAll("#cluster-table tbody tr").forEach((row) => {
    const cluster = row.cells[0]?.textContent?.trim();
    row.classList.toggle("is-selected", Boolean(selected && cluster === selected));
  });
}

function setDateInputs(prefix, rows, key, defaultSpanDays) {
  if (!rows.length) return;
  const dates = rows.map((row) => String(row[key] || "").slice(0, 10)).filter(Boolean).sort();
  const min = dates[0];
  const max = dates[dates.length - 1];
  const start = document.querySelector(`#${prefix}-date-start`);
  const end = document.querySelector(`#${prefix}-date-end`);
  if (!start || !end) return;
  start.min = min;
  start.max = max;
  end.min = min;
  end.max = max;
  start.value = min;
  const defaultEnd = addDays(min, defaultSpanDays);
  end.value = defaultEnd > max ? max : defaultEnd;
}

function fillSelect(selector, values) {
  const select = document.querySelector(selector);
  if (!select) return;
  select.innerHTML = values.map((value) => `<option value="${escapeHtml(value)}">${escapeHtml(value)}</option>`).join("");
}

function dateFilteredRows(rows, key, startSelector, endSelector) {
  const start = document.querySelector(startSelector)?.value;
  const end = document.querySelector(endSelector)?.value;
  return rows.filter((row) => {
    const date = String(row[key] || "").slice(0, 10);
    return (!start || date >= start) && (!end || date <= end);
  });
}

function checkedValues(selector) {
  return Array.from(document.querySelectorAll(`${selector} input:checked`)).map((input) => input.value);
}

function clusterRows() {
  return state.clusters.filter((row) => row.cluster_label);
}

function climateMap() {
  return Object.fromEntries(state.climate.map((row) => [row.variable, num(row.value)]));
}

function bestBy(rows, key) {
  return rows.filter((row) => Number.isFinite(num(row[key]))).sort((a, b) => num(a[key]) - num(b[key]))[0] || null;
}

function sum(rows, key) {
  return rows.reduce((acc, row) => acc + (num(row[key]) || 0), 0);
}

function setText(selector, value) {
  const node = document.querySelector(selector);
  if (node) node.textContent = value ?? "-";
}

function empty(host, message = "Sin datos disponibles") {
  host.innerHTML = `<div class="empty">${escapeHtml(message)}</div>`;
}

function minHeight(host) {
  const match = Array.from(host.classList).map((name) => name.match(/^plot-h(\d+)$/)).find(Boolean);
  return match ? Number(match[1]) : 320;
}

function resizePlots() {
  if (!window.Plotly) return;
  document.querySelectorAll(".js-plotly-plot").forEach((node) => {
    window.Plotly.Plots.resize(node);
  });
}

function debounce(fn, wait) {
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), wait);
  };
}

async function waitForPlotly() {
  const started = Date.now();
  while (!window.Plotly && Date.now() - started < 8000) {
    await new Promise((resolve) => setTimeout(resolve, 80));
  }
}

function parseCsv(text) {
  const rows = [];
  let row = [];
  let cell = "";
  let quoted = false;

  for (let index = 0; index < text.length; index += 1) {
    const char = text[index];
    const next = text[index + 1];

    if (char === '"' && quoted && next === '"') {
      cell += '"';
      index += 1;
      continue;
    }

    if (char === '"') {
      quoted = !quoted;
      continue;
    }

    if (char === "," && !quoted) {
      row.push(cell);
      cell = "";
      continue;
    }

    if ((char === "\n" || char === "\r") && !quoted) {
      if (char === "\r" && next === "\n") index += 1;
      row.push(cell);
      if (row.some((value) => value !== "")) rows.push(row);
      row = [];
      cell = "";
      continue;
    }

    cell += char;
  }

  if (cell.length || row.length) {
    row.push(cell);
    rows.push(row);
  }

  const headers = (rows.shift() || []).map((header) => header.replace(/^\uFEFF/, ""));
  return rows.map((values) =>
    Object.fromEntries(headers.map((header, index) => [header, values[index] ?? ""]))
  );
}

function num(value) {
  if (value === null || value === undefined || value === "" || value === "NA") return NaN;
  const parsed = Number(String(value).replace(",", "."));
  return Number.isFinite(parsed) ? parsed : NaN;
}

function formatInt(value) {
  const parsed = num(value);
  if (!Number.isFinite(parsed)) return "-";
  return String(Math.round(parsed)).replace(/\B(?=(\d{3})+(?!\d))/g, " ");
}

function formatFixed(value, digits = 1) {
  const parsed = num(value);
  if (!Number.isFinite(parsed)) return "-";
  if (digits === 0) return formatInt(parsed);
  return parsed.toFixed(digits);
}

function formatPercent(value, digits = 2) {
  const parsed = num(value);
  return Number.isFinite(parsed) ? `${formatFixed(parsed, digits)}%` : "-";
}

function formatFixedEs(value, digits = 1) {
  return formatFixed(value, digits).replace(".", ",");
}

function formatPercentEs(value, digits = 2) {
  return formatPercent(value, digits).replace(".", ",");
}

function formatCompact(value) {
  const parsed = num(value);
  if (!Number.isFinite(parsed)) return "-";
  if (Math.abs(parsed) >= 1_000_000) return `${(parsed / 1_000_000).toFixed(1)} M`;
  if (Math.abs(parsed) >= 1_000) return `${(parsed / 1_000).toFixed(1)} k`;
  return parsed.toFixed(1);
}

function addDays(dateText, days) {
  const date = new Date(`${dateText}T00:00:00`);
  date.setDate(date.getDate() + days);
  return date.toISOString().slice(0, 10);
}

function escapeHtml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}
