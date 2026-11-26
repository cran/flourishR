## ----setup--------------------------------------------------------------------
library(flourishR)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
set.seed(42)
n <- 240L
raw <- data.frame(ID = seq_len(n), COUNTRY = rep(c(3, 9, 23), each = 80),
                  STRATA = rep(rep(1:4, each = 20), 3),
                  PSU = rep(rep(1:20, each = 4), 3))
for (j in 1:10) raw[[paste0("TRAITS", j, "_Y1")]] <- sample(1:7, n, TRUE)
person <- rnorm(n)
for (w in 1:3) {
  active <- if (w == 1) rep(TRUE, n) else runif(n) < .85
  x <- pmin(10, pmax(0, 5 + person + rnorm(n)))
  y <- pmin(10, pmax(0, 4 + .4*x + person + rnorm(n)))
  raw[[paste0("WAVE_Y", w)]] <- ifelse(active, w, NA)
  raw[[paste0("LONELY_Y", w)]] <- ifelse(active, x, NA)
  raw[[paste0("LIFE_SAT_Y", w)]] <- ifelse(active, y, NA)
  raw[[paste0("ANNUAL_WEIGHT_C", w)]] <- ifelse(active, 1, NA)
}
raw$ANNUAL_WEIGHT_L2 <- raw$ANNUAL_WEIGHT_C2
raw$ANNUAL_WEIGHT_L3 <- ifelse(!is.na(raw$WAVE_Y2) & !is.na(raw$WAVE_Y3), 1, NA)
gfs <- score_tipi(gfs_prepare(raw))

## -----------------------------------------------------------------------------
gfs_dictionary("LIFE_SAT")
gfs_countries(c(3, 9, 23))
gfs_weights(1:3)
missing <- check_missingness(gfs[!is.na(gfs$WAVE), ], c("LIFE_SAT", "LONELY"), by = c("COUNTRY", "wave"))
head(missing)
plot_missingness(missing)
retention <- check_retention(gfs, group = "COUNTRY")
plot_retention(retention, group = "COUNTRY", cumulative = TRUE)

## -----------------------------------------------------------------------------
if (requireNamespace("survey", quietly = TRUE)) {
  design <- gfs_design(gfs, "Japan", waves = 3)
  survey::svymean(~LIFE_SAT, design, na.rm = TRUE)
  means <- describe_countries(gfs, "LIFE_SAT", wave = 3)
  print(plot_cross_national(means, "COUNTRY", "LIFE_SAT", estimates = TRUE))
  trajectories <- do.call(rbind, lapply(1:3, function(w) describe_countries(gfs, "LIFE_SAT", wave = w)))
  print(plot_longitudinal(trajectories, "ID", "wave", "LIFE_SAT", country_var = "COUNTRY", estimates = TRUE))
  results <- loop_countries(gfs, "COUNTRY", formula = LIFE_SAT ~ LONELY, wave = 3, weighted = TRUE)
  print(country_moderation(results, "LONELY"))
  if (requireNamespace("metafor", quietly = TRUE)) {
    pooled <- meta_countries(results, "LONELY")
    print(plot_country_effects(results, "LONELY", pooled = pooled))
  }
}

## -----------------------------------------------------------------------------
panel <- create_lags(gfs, c("LIFE_SAT", "LONELY"))
changes <- change_scores(gfs, "LIFE_SAT")
head(changes)
panel <- within_between(panel, "LONELY")
if (requireNamespace("survey", quietly = TRUE)) {
  prospective <- loop_longitudinal(panel, LIFE_SAT ~ LONELY_lag1 + LIFE_SAT_lag1)
  print(prospective)
}
if (requireNamespace("lme4", quietly = TRUE)) {
  mixed <- loop_longitudinal(panel, LIFE_SAT ~ LONELY_within + LONELY_between + factor(wave), model = "mixed")
  print(table(mixed$status))
}

## -----------------------------------------------------------------------------
context <- data.frame(COUNTRY = c("Brazil", "Japan", "Sweden"), context_index = c(2, 4, 3))
if (exists("results", inherits = FALSE) && requireNamespace("metafor", quietly = TRUE)) {
  linked <- join_country_data(results, context)
  meta_countries(linked, "LONELY", moderators = ~context_index)
}

