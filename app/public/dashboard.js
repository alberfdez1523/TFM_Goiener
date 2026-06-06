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

  setText("#kpi-users", formatInt(users));
  setText("#kpi-clusters", formatInt(clusterCount));
  setText("#kpi-beta-hdd", formatFixed(betaHdd, 3));
  setText("#kpi-wape", formatPercent(wape, 2));

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
    textinfo: "label+percent",
    hovertemplate: "%{label}<br>%{value:,.0f} usuarios<extra></extra>"
  }], {
    showlegend: false,
    margin: { t: 12, r: 10, b: 12, l: 10 }
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

  table("#eda-summary-dt", state.summary, [
    ["Días completos", "dias_usuario_completos", formatInt],
    ["Usuarios", "usuarios", formatInt],
    ["Fecha min", "fecha_min"],
    ["Fecha max", "fecha_max"],
    ["Media kWh", "media_kWh", (v) => formatFixed(v, 2)],
    ["Mediana kWh", "mediana_kWh", (v) => formatFixed(v, 2)],
    ["P90", "p90_kWh", (v) => formatFixed(v, 2)],
    ["P99", "p99_kWh", (v) => formatFixed(v, 2)],
    ["kWh total", "kWh_total", formatInt]
  ]);
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

  const leader = state.forecastClusterLeader.filter((row) => String(row.cluster) === String(selected));
  table("#cluster-fc-dt", leader.length ? leader : state.forecastClusterLeader, [
    ["Cluster", "cluster"],
    ["Modelo", "model"],
    ["MAE", "MAE", (v) => formatFixed(v, 3)],
    ["RMSE", "RMSE", (v) => formatFixed(v, 3)],
    ["WAPE", "WAPE", (v) => formatFixed(v, 3)],
    ["MASE", "MASE", (v) => formatFixed(v, 3)]
  ], { limit: 12 });
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

function renderBenchmarks() {
  const experiments = [...new Set(state.benchmark.map((row) => row.experimento))];
  plot("#benchmark-time-plot", experiments.map((experiment, index) => {
    const rows = state.benchmark.filter((row) => row.experimento === experiment);
    return {
      type: "bar",
      orientation: "h",
      name: experiment,
      y: rows.map((row) => row.metodo),
      x: rows.map((row) => num(row.mediana_s)),
      marker: { color: PALETTE[index % PALETTE.length] },
      hovertemplate: `${escapeHtml(experiment)}<br>%{y}<br>%{x:.3f}s<extra></extra>`
    };
  }), {
    xaxis: { title: "Mediana (s)" },
    yaxis: { title: "" },
    barmode: "group"
  });

  horizontalBar("#benchmark-disk-plot", state.benchmarkDisk, {
    x: "MB",
    y: "formato",
    text: (row) => `${formatFixed(row.MB, 0)} MB`,
    marker: "#8b5cf6",
    xTitle: "MB",
    sort: "asc"
  });

  table("#benchmark-dt", state.benchmark, [
    ["Experimento", "experimento"],
    ["Método", "metodo"],
    ["n", "n", formatInt],
    ["Mediana s", "mediana_s", (v) => formatFixed(v, 3)],
    ["Media s", "media_s", (v) => formatFixed(v, 3)],
    ["P95 s", "p95_s", (v) => formatFixed(v, 3)]
  ]);

  table("#benchmark-env-dt", state.benchmarkEnv, [
    ["Item", "item"],
    ["Value", "value"]
  ]);
}

function renderConclusions() {
  const host = document.querySelector("#conclusions-ui");
  if (!host) return;
  const bestDaily = bestBy(state.forecastDailyLeader, "WAPE");
  const clusterCount = clusterRows().length;
  const users = sum(clusterRows(), "n");
  const medianKwh = state.summary[0]?.mediana_kWh;

  const cards = [
    {
      icon: "magnifying-glass-chart",
      title: "EDA — qué dice la cartera",
      lead: `La cartera presenta una distribución muy asimétrica de consumo: la mediana diaria ronda ${formatFixed(medianKwh, 2)} kWh mientras la cola derecha concentra un grupo reducido de hogares de consumo alto.`,
      bullets: [
        "Heterogeneidad entre usuarios mayor que la variabilidad diaria de un mismo hogar: tiene más sentido segmentar antes que ajustar un único modelo agregado.",
        "Estacionalidad clara con valles nocturnos, pico vespertino y mayor consumo en meses fríos.",
        "Crecimiento sostenido de la cartera 2014-2024: obliga a validación temporal estricta."
      ],
      footer: "Implicación: filtros de calidad por usuario, features de forma horaria y validación rolling."
    },
    {
      icon: "shield-halved",
      title: "Calidad, privacidad y alcance",
      lead: "Los datos están seudonimizados y se trabajan siempre como agregados o segmentos: la app no muestra hogares individuales.",
      bullets: [
        "Tasa de nulos y negativos muy baja, con trazabilidad de días incompletos.",
        "Spikes y negativos quedan auditados antes de features y clustering.",
        "Privacidad por diseño: salidas agregadas a cluster y sin atributos identificativos sensibles."
      ],
      footer: "Implicación ética: el TFM informa decisiones de cartera, no segmentaciones individuales."
    },
    {
      icon: "cloud-sun-rain",
      title: "Clima — qué aporta AEMET",
      lead: "La capa climática diaria se construye desde AEMET por provincia, se imputa cuando hay huecos y se cruza con calendario.",
      bullets: [
        "HDD/CDD resumen presión térmica de manera defendible.",
        "La sensibilidad climática se estima por usuario y luego se promedia por cluster.",
        "El calendario diferenciado evita confundir señales estatales con festivos autonómicos."
      ],
      footer: "Implicación operativa: si el feed AEMET falla, el WAPE del forecast se degrada de forma medible."
    },
    {
      icon: "layer-group",
      title: "Clusters — segmentación operativa",
      lead: `La solución K-Means sobre PCA segmenta ${formatInt(users)} hogares en ${clusterCount} grupos comparables, más un segmento descriptivo no_habitual.`,
      bullets: [
        "Los segmentos resisten remuestreo y no son artefactos del split concreto.",
        "Las variables más discriminantes son forma horaria, estacionalidad y sensibilidad climática.",
        "Cada cluster tiene una acción operativa asociada en cluster_business_mapping."
      ],
      footer: "Implicación: la segmentación sostiene recomendaciones de tarifa, eficiencia y forecasting por cluster."
    },
    {
      icon: "chart-line",
      title: "Forecast — utilidad y límites",
      lead: `El mejor modelo diario alcanza WAPE ${formatPercent(bestDaily?.WAPE, 2)} en test con XGBoost sobre target log-transformado, calendario extendido, lags y HDD/CDD.`,
      bullets: [
        "El modelo horario se apoya en diff_lag24/168 y stacking ridge sobre validación.",
        "Los intervalos conformal split quedan más alineados con cobertura nominal del 90%.",
        "Errores mayores se concentran en lunes, festivos y transiciones de régimen."
      ],
      footer: "Implicación: usar conformal para reporting financiero, quantile para pricing interno."
    },
    {
      icon: "database",
      title: "Arquitectura — reproducibilidad",
      lead: "El stack Parquet + DuckDB + tablas pre-agregadas hace viable reproducir el TFM en local sin desplegar infraestructura distribuida.",
      bullets: [
        "Compresión ZSTD-9 reduce el tamaño frente al CSV equivalente.",
        "El orquestador encadena extracción, calidad, clima, features, clustering, forecasting y benchmark.",
        "El re-render reproducible queda trazado con logs y timings por fase."
      ],
      footer: "Implicación: el TFM se valida 1:1 desde cero."
    }
  ];

  host.innerHTML = cards.map(conclusionCard).join("");
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
  plot(selector, [{
    type: "scatter",
    mode: "markers+text",
    x: rows.map((row) => num(row.beta_hdd)),
    y: rows.map((row) => num(row.beta_cdd)),
    text: rows.map((row) => row.cluster_label),
    textposition: "top center",
    marker: {
      color: rows.map((row) => row.cluster_label === selected ? "#fbbf24" : "#10b981"),
      size: rows.map((row) => Math.max(12, Math.sqrt(num(row.n) || 1) / 2.5)),
      opacity: 0.82,
      line: { color: "#fafafa", width: rows.map((row) => row.cluster_label === selected ? 2 : 1) }
    },
    customdata: rows.map((row) => [row.n, row.mean_daily_kWh]),
    hovertemplate: "%{text}<br>β_HDD=%{x:.3f}<br>β_CDD=%{y:.3f}<br>n=%{customdata[0]:,.0f}<br>media=%{customdata[1]:.2f} kWh<extra></extra>"
  }], {
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
    marker: {
      color: typeof config.marker === "function" ? plotted.map(config.marker) : config.marker
    },
    hovertemplate: "%{y}<br>%{x:.4f}<extra></extra>"
  }], {
    xaxis: { title: config.xTitle || "" },
    yaxis: { title: "", automargin: true, autorange: "reversed" },
    margin: { l: 120, r: 34, t: 14, b: 48 },
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

function metricRow(label, value) {
  return `<div class="metric-row"><span>${escapeHtml(label)}</span><strong>${escapeHtml(value ?? "-")}</strong></div>`;
}

function conclusionCard(card) {
  return `
    <article class="card">
      <div class="card-header"><i class="fa-solid fa-${card.icon}" aria-hidden="true"></i> ${escapeHtml(card.title)}</div>
      <div class="card-body goi-prose">
        <p>${escapeHtml(card.lead)}</p>
        <ul>${card.bullets.map((bullet) => `<li>${escapeHtml(bullet)}</li>`).join("")}</ul>
        <p class="mt-2 mb-0 text-secondary"><em>${escapeHtml(card.footer)}</em></p>
      </div>
    </article>
  `;
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
