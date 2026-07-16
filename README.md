# NASCAR Analytics app

This Shiny app reads the latest generated NASCAR files from `app/data` first and
falls back to the project-level `data` directory. Run the numbered pipeline
steps before launching the app when forecasts or historical data need a refresh.

## Launch locally

From RStudio, open `app/app.R` and choose **Run App**. From a terminal at the
project root, run:

```powershell
& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" app/tools/run_local_app.R
```

The local app listens at <http://127.0.0.1:4488>.

## Current model contracts

- The 2026 forecast is fit once on 2018-2025 data.
- The 2025 backtest is fit once on 2018-2024 data.
- Finish, probability, and points pages expose the recency baseline, both
  general XGBoost models, Drafting, Road Course, Speedway, and Short / Steep
  specialists, routed consensus, and a live ensemble of the selected models.
  Qualifying keeps the three qualifying specialists and has no Short / Steep
  choice because that specialist is a race-outcome model.
- Historical prediction contracts are bundled as compact, column-pruned RDS
  files. Full CSV/modeling exports and feature-importance tables stay in the
  project-level `data` directory and are not part of the published app.
- Finish, probability, points, routed-specialist, and all-model consensus pages
  place the F1-style season betting ROI, selected-race consensus bets, predicted
  winner, and full order at the top. ROI windows combine the leakage-safe 2025
  fixed backtest with 2026 out-of-sample predictions.
- Winner ROI and average edge use archived full-field DraftKings winner boards
  for all 56 completed 2025–2026 races. The original eventual-winner archive is
  retained as a payout-validation source and fallback.
  Verified quotes take priority and missing podium prices use the documented
  implied-fair fallback below.
- Stage 15 publishes all eight current-race component predictions as well as the
  production routed-consensus contract used by downstream overlays and fantasy.
- The Model Consensus page has four inclusion controls—Finish, Probability,
  Points, and Routed Specialists—and inherits the actual model selections from
  those four tabs instead of asking the user to select them again.
- The Routed Specialists page automatically maps the selected race to one of
  four NASCAR routes: Drafting, Road Course, Conventional Speedway, or Short /
  Steep Oval. All 12 route/outcome checkboxes remain visible; changing the race
  checks its Position, Probability, and Points specialists by default, after
  which any of the 12 can be turned on or off. Each specialist is evaluated
  only on matching races.
- Verified top-three quotes are used when available. Missing podium prices are
  represented by fair Plackett-Luce top-three odds derived from each race's
  complete no-vig winner board and are labeled as implied rather than quoted.
- Qualifying pages report pole-pick and NASCAR front-row (top-two) hit rates;
  qualifying ROI is withheld until broader pole-market odds are archived.
- The Chatter Overlay page compares base-consensus and adjusted season ROI.
- Chatter remains neutral until its timestamps and sources pass verification.
- DraftKings output is provisional while starting positions are model-predicted.
- DraftKings NASCAR Classic uses six equal driver slots with no captain multiplier.
- Track pages compare historical driver and owner performance; marker size shows starts and color shows win rate.
- Driver and owner profiles compare average finish by track family; marker size shows starts and color shows top-three rate.

The original copied Formula One shell is retained under `app/_old/f1_copy_initial`.
The superseded summary-only NASCAR dashboard is retained under
`app/_old/summary_dashboard_20260716`.
