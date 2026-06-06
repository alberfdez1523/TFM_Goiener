import { copyFile, mkdir, stat } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const appDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoDir = path.resolve(appDir, "..");
const sourceDir = path.join(repoDir, "outputs", "tables");
const targetDir = path.join(appDir, "public", "data");

const files = [
  "dashboard_eda_summary.csv",
  "dashboard_eda_monthly.csv",
  "dashboard_eda_hourly.csv",
  "dashboard_eda_tariff.csv",
  "cluster_profiles.csv",
  "cluster_socioeconomic.csv",
  "cluster_business_mapping.csv",
  "cluster_top_separators.csv",
  "cluster_leaderboard.csv",
  "forecast_master_leaderboard.csv",
  "forecast_leaderboard_daily.csv",
  "forecast_leaderboard_hourly.csv",
  "forecast_leaderboard_cluster.csv",
  "forecast_daily_predictions.csv",
  "forecast_daily_intervals.csv",
  "forecast_hourly_predictions.csv",
  "forecast_hourly_business_impact.csv",
  "forecast_cluster_predictions.csv",
  "forecast_error_slices.csv",
  "forecast_daily_xgb_importance.csv",
  "feature_climate_sensitivity_summary.csv",
  "benchmark_results.csv",
  "benchmark_environment.csv",
  "benchmark_disk_size.csv",
  "pipeline_timings.csv"
];

await mkdir(targetDir, { recursive: true });

let copied = 0;
for (const file of files) {
  const source = path.join(sourceDir, file);
  const target = path.join(targetDir, file);

  try {
    await stat(source);
    await copyFile(source, target);
    copied += 1;
    console.log(`copied ${file}`);
  } catch {
    console.warn(`missing ${file}`);
  }
}

console.log(`dashboard data sync complete: ${copied}/${files.length} files`);
