#Produced by Farah Mugrabi
#Set up-------------------------------------------------------------------------
rm(list=ls())
cat("\014")
path<- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
dpath<- paste0(path, "/1.Data/")
library(ggplot2)
library(fixest)
library(modelsummary)
library(pandoc)
library(lmtest)
library(plm)
library(dplyr)
library(kableExtra)
library(tibble)
library("paletteer")
library(tidyr)
library(ecb)
library(purrr)
library(fredr)
library(quantmod)
base_cols <- paletteer::paletteer_d("MetBrewer::Cross")
fig_path <- file.path(path, "2.Outcome", "Figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

#1. Data and vars###############################################################
dataph<- readxl::read_xlsx(paste0(dpath, "pharma_exports_usa.xlsx"))#Dataset requested to Gordon and Conor MFD

#1.1 Control vars----------
# Real exchange rate / competitiveness indicator from ECB
# Quarterly, CPI-deflated HCI, broad group + euro area countries
country_currency <- tibble(
  country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia",
              "Denmark", "Estonia", "Finland", "France", "Germany",
              "Greece", "Hungary", "Ireland", "Italy", "Latvia",
              "Lithuania", "Netherlands", "Poland", "Romania",
              "Slovenia", "Spain", "Sweden"),
  curr = c("ATS", "BEF", "BGN", "HRK", "CZK",
           "DKK", "EEK", "FIM", "FRF", "DEM",
           "GRD", "HUF", "IEP", "ITL", "LVL",
           "LTL", "NLG", "PLN", "RON",
           "SIT", "ESP", "SEK")
)

get_reer_ecb <- function(country_i, curr_i) {
  
  groups_try <- c("H03", "E03", "H02", "E02", "H42", "E42", "H41", "E41")
  freq_try <- c("M", "Q", "A")
  
  keys_try <- expand.grid(freq = freq_try, group = groups_try) %>%
    mutate(key = paste0("EXR.", freq, ".", group, ".", curr_i, ".NRC0.A")) %>%
    pull(key)
  
  x <- NULL
  key_ok <- NA_character_
  
  for (key_i in keys_try) {
    x_try <- tryCatch(
      ecb::get_data(key_i, filter = list(startPeriod = "2001-01")),
      error = function(e) NULL
    )
    
    if (!is.null(x_try) && nrow(x_try) > 0) {
      x <- x_try
      key_ok <- key_i
      break
    }
  }
  
  if (is.null(x)) {
    warning(paste0("No ECB REER series found for ", country_i, " / ", curr_i))
    return(tibble(country = country_i, curr = curr_i,
                  date = as.Date(character()), reer = numeric(),
                  ecb_key = character()))
  }
  
  names(x) <- tolower(names(x))
  
  date_col <- names(x)[grepl("time|date|period", names(x))][1]
  value_col <- names(x)[grepl("obs_value|value", names(x))][1]
  
  out <- x %>%
    mutate(
      country = country_i,
      curr = curr_i,
      ecb_key = key_ok,
      date_raw = .data[[date_col]],
      reer = as.numeric(.data[[value_col]])
    )
  
  if (grepl("^EXR\\.M\\.", key_ok)) {
    out <- out %>%
      mutate(date = as.Date(zoo::as.yearqtr(zoo::as.yearmon(date_raw, "%Y-%m")))) %>%
      group_by(country, curr, ecb_key, date) %>%
      summarise(reer = mean(reer, na.rm = TRUE), .groups = "drop")
  } else if (grepl("^EXR\\.Q\\.", key_ok)) {
    out <- out %>%
      mutate(date = as.Date(zoo::as.yearqtr(date_raw, format = "%Y-Q%q"))) %>%
      select(country, curr, ecb_key, date, reer)
  } else {
    out <- out %>%
      mutate(date = as.Date(paste0(date_raw, "-01-01"))) %>%
      select(country, curr, ecb_key, date, reer)
  }
  
  out
}

reer_ecb <- purrr::map2_dfr(
  country_currency$country,
  country_currency$curr,
  get_reer_ecb
)

reer_ecb %>% count(country, curr, ecb_key)
missing_reer <- setdiff(unique(dataph$country), unique(reer_ecb$country))
missing_reer

dataph <- dataph %>%
  left_join(reer_ecb %>% select(country, date, reer),
            by = c("country", "date")) %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(
    ln_reer = log(reer),
    reer_gr = ln_reer - dplyr:: lag(ln_reer, 1),
    reer_yoy = ln_reer - dplyr:: lag(ln_reer, 4),
    reer_vol_2y = zoo::rollapply(reer_gr, 8, sd,
                                 fill = NA, align = "right", na.rm = TRUE)
  ) %>%
  ungroup()
#1.2 Shocks ------------------
# global GDP shock
# getSymbols("NYGDPMKTPCDWLD", src = "FRED")
# 
# global_gdp <- data.frame(
#   date = as.Date(index(NYGDPMKTPCDWLD)),
#   global_gdp = as.numeric(NYGDPMKTPCDWLD[, 1])
# ) %>%
#   arrange(date) %>%
#   mutate(
#     ln_global_gdp = log(global_gdp),
#     ln_gdp_yoy = ln_global_gdp - dplyr::lag(ln_global_gdp, 1)
#   )
# 
# global_gdp_q <- global_gdp %>%
#   mutate(year = as.integer(format(date, "%Y"))) %>%
#   select(year, global_gdp, ln_global_gdp, ln_gdp_yoy) %>%
#   tidyr::crossing(q = 1:4) %>%
#   mutate(date = as.Date(as.yearqtr(paste0(year, " Q", q), format = "%Y Q%q"))) %>%
#   select(date, global_gdp, ln_global_gdp, ln_gdp_yoy)
# 
# dataph <- dataph %>%
#   left_join(global_gdp_q, by = "date")

getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", auto.assign = TRUE)

sp500_q <- data.frame(
  date_daily = as.Date(index(GSPC)),
  sp500 = as.numeric(GSPC[, 1])
) %>%
  filter(!is.na(sp500)) %>%
  arrange(date_daily) %>%
  mutate(
    ln_global_gdp = log(sp500),
    ln_gdp_yoy = ln_global_gdp - dplyr::lag(ln_global_gdp),
    date = as.Date(as.yearqtr(date_daily))
  )

dataph <- dataph %>%
  left_join(sp500_q, by = "date")

#1.3 Remove outliers----------
dataph <- dataph %>%
  filter(date >= "2001-01-01") %>%
  filter(exports_q_sum > 0) %>%
  mutate(ln_exports_q_sum = log(exports_q_sum))

country_outliers <- dataph %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(exports_yoy = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 4)) %>%
  ungroup() %>%
  mutate(exports_yoy = ifelse(is.finite(exports_yoy), exports_yoy, NA_real_)) %>%
  group_by(country) %>%
  summarise(
    mean_y = mean(exports_yoy, na.rm = TRUE),
    sd_y = sd(exports_yoy, na.rm = TRUE),
    max_abs_y = max(abs(exports_yoy), na.rm = TRUE),
    n = sum(!is.na(exports_yoy)),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_y, sd_y, max_abs_y), ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
  mutate(
    q_mean = quantile(abs(mean_y), 0.95, na.rm = TRUE),
    q_sd = quantile(sd_y, 0.95, na.rm = TRUE),
    q_max = quantile(max_abs_y, 0.95, na.rm = TRUE),
    outlier = abs(mean_y) > q_mean | sd_y > q_sd | max_abs_y > q_max | n < 20
  )

country_outliers %>% 
  filter(outlier)

# non_euro_countries <- c(
#   "Bulgaria",
#   "Czechia",
#   "Denmark",
#   "Hungary",
#   "Poland",
#   "Romania",
#   "Sweden")
dataph <- dataph %>%
  filter(!country %in% country_outliers$country[country_outliers$outlier]) 
  # filter(!country %in% non_euro_countries)

dataph$country %>% unique()

#2. Descriptive#############################
p <- dataph %>%
  ggplot(aes(x = date, y = ln_exports_q_sum, color = counterparty)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ country, scales = "free_y") +
  labs(x = "", y = "Log exports", color = "Counterparty",
       title = "Exports over time by country") +
  scale_color_manual(values = base_cols) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 8))

p

fig_path <- file.path(path, "2.Outcome", "Figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

ggsave("exports_by_country_panel.png",
       plot = p, path = fig_path,
       width = 10, height = 8, dpi = 300)

#3. YOY variation Estimation###################################################
#Note: not used
shock_vars <- c("ln_global_gdp_yoy", "ln_gdp_us_yoy", "ln_vix", "ln_vix_yoy")
run_export_feols <- function(data, shocks) {
  
  df <- data %>%
    arrange(country, counterparty, date) %>%
    group_by(country, counterparty) %>%
    mutate(
      exports_yoy = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 4),
      ln_global_gdp_yoy = ln_global_gdp - dplyr::lag(ln_global_gdp, 4),
      ln_gdp_us_yoy = ln_gdp_us - dplyr::lag(ln_gdp_us, 4),
      ln_vix_yoy = ln_vix - dplyr::lag(ln_vix, 4),
      trend = row_number(),
      year = factor(format(date, "%Y")),
      q = factor(quarters(date))
    ) %>%
    ungroup()
  

  df <- df %>%
    arrange(country, counterparty, date) %>%
    group_by(country, counterparty) %>%
    mutate(
      across(
        all_of(shock_vars),
        ~ dplyr::lag(.x, 1),
        .names = "{.col}_l1"
      )
    ) %>%
    ungroup()
  
  mods <- lapply(shocks, function(shock) {
    # shock_l1 <- paste0(shock, "_l1")
    
    feols(
      as.formula(paste0("exports_yoy ~ ", shock, " +reer_yoy | country[trend] ")),
      data = df,
      vcov = ~ country,
      panel.id = ~ country + date
    )
  })
  
  names(mods) <- shocks
  mods
}

mods <- run_export_feols(dataph, shock_vars)
etable(mods)

#TEST----------

tests <- lapply(mods, function(m) {
  list(
    ttest_country = coeftable(m, vcov = ~ country),
    ttest_twoway = coeftable(m, vcov = ~ country + date),
    ttest_DK = coeftable(m, vcov = "DK"),
    # ttest_NW = coeftable(m, vcov = "NW"),
    fit = fitstat(m, c("n", "r2", "wr2")),
    ljung_box_lag4 = Box.test(resid(m), lag = 4, type = "Ljung-Box"),
    ljung_box_lag8 = Box.test(resid(m), lag = 8, type = "Ljung-Box"),
    resid_acf = acf(resid(m), plot = FALSE)$acf[2:9]
  )
})

tests$ln_gdp_us_yoy
tests$ln_vix
tests$ln_vix_yoy

etable(
  mods,
  vcov = list(
    "each",
    "Country" = ~ country,
    "Country + Date" = ~ country + date,
    "DK" = "DK"
    # "NW" = "NW"
  )
)

#Export results------------------------
stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.1 ~ "*",
    TRUE ~ ""
  )
}

fmt_coef <- function(m, vc) {
  ct <- coeftable(m, vcov = vc)
  paste0(sprintf("%.3f", ct[1, 1]), stars(ct[1, 4]))
}

fmt_se <- function(m, vc) {
  ct <- coeftable(m, vcov = vc)
  paste0("(", sprintf("%.3f", ct[1, 2]), ")")
}

tab_latex <- bind_rows(lapply(names(mods), function(x) {
  m <- mods[[x]]
  
  tibble(
    Shock = x,
    `Country coef.` = fmt_coef(m, ~ country),
    `Country se` = fmt_se(m, ~ country),
    `Country-Date coef.` = fmt_coef(m, ~ country + date),
    `Country-Date se` = fmt_se(m, ~ country + date),
    `DK coef.` = fmt_coef(m, "DK"),
    `DK se` = fmt_se(m, "DK"),
    # `NW coef.` = fmt_coef(m, "NW"),
    # `NW se` = fmt_se(m, "NW"),
    N = nobs(m),
    R2 = sprintf("%.3f", r2(m, "r2")),
    `Within R2` = sprintf("%.3f", r2(m, "wr2"))
  )
}))

tab_latex %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l", rep("c", 11)),
    caption = "Export growth and external shocks"
  ) %>%
  add_header_above(c(
    " " = 1,
    "Cluster country" = 2,
    "Cluster country-date" = 2,
    "Driscoll-Kraay" = 2,
    # "Newey-West" = 2,
    "Fit" = 3
  )) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 8
  ) %>%
  save_kable(paste0(path,"/2.Outcome/exports_shocks_kable.tex"))

#Individual slopes -------------
df_export <- dataph %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(
    exports_yoy = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 4),
    ln_gdp_us_yoy = ln_gdp_us - dplyr::lag(ln_gdp_us, 4),
    ln_gdp_yoy = log(gdp) - dplyr::lag(log(gdp), 4),
    ln_vix_yoy = ln_vix - dplyr::lag(ln_vix, 4),
    exports_yoy_l1 = dplyr::lag(exports_yoy, 1),
    trend = row_number(),
    year = factor(format(date, "%Y")),
    q = factor(quarters(date))
  ) %>%
  ungroup() %>% 
  mutate(
    across(c(exports_yoy, exports_yoy_l1, ln_gdp_us_yoy, ln_gdp_yoy, ln_vix, ln_vix_yoy),
           ~ as.numeric(scale(.x)),
           .names = "{.col}_std")
  )

shock_labels <- c(
  ln_gdp_us_yoy = "US GDP growth",
  ln_gdp_yoy  = "S&P Return",
  ln_vix = "VIX",
  ln_vix_yoy = "VIX growth"

)

run_slope_model <- function(shock) {
  # shock_l1 <- paste0(shock, "_l1")
  feols(
    as.formula(paste0("exports_yoy ~ i(country, ", shock, "+reer_yoy) | country[trend]")),
    #as.formula(paste0("exports_yoy ~ exports_yoy_l1 + i(country, ", shock, ") | country[trend] + year + q")),
    data = df_export,
    vcov = ~ country,
    panel.id = ~ country + date
  )
}

mods_slopes <- lapply(names(shock_labels), run_slope_model)
names(mods_slopes) <- names(shock_labels)

slopes_tab <- bind_rows(lapply(names(mods_slopes), function(shock) {
  
  ct <- as.data.frame(coeftable(mods_slopes[[shock]])) %>%
    rownames_to_column("term")
  
  ct %>%
    filter(grepl("country::", term)) %>%
    mutate(
      shock = shock,
      shock_lab = shock_labels[shock],
      country = sub("country::", "", term),
      country = sub(paste0(":", shock, ".*"), "", country),
      ci_low = Estimate - 1.645 * `Std. Error`,
      ci_high = Estimate +1.645  * `Std. Error`,
      line_type = ifelse(`Pr(>|t|)` < 0.10, "Significant", "Not significant / truncated")
    ) %>%
    select(shock, shock_lab, country, estimate = Estimate, se = `Std. Error`,
           pval = `Pr(>|t|)`, ci_low, ci_high, line_type)
}))

slopes_tab
fig_path <- file.path(path, "2.Outcome", "Figures")
for (shock_i in names(shock_labels)) {
  
  p <- slopes_tab %>%
    filter(shock == shock_i) %>%
    ggplot(aes(x = reorder(country, estimate), y = estimate, linetype = line_type)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_linerange(aes(ymin = ci_low, ymax = ci_high),
                   linewidth = 0.7, color = base_cols[8]) +
    geom_point(size = 2, color = base_cols[1]) +
    coord_flip() +
    labs(x = "", y = "Country-specific coefficient", linetype = "",
         title = paste0(shock_labels[shock_i], " shock - country-specific slopes")) +
    scale_linetype_manual(values = c(
      "Significant" = "solid",
      "Not significant / truncated" = "11")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey90"),
          axis.text.x = element_text(angle = 25, hjust = 1),
          plot.caption = element_text(hjust = 0, size = 8))
  
  print(p)
  
  ggsave(filename = paste0("country_specific_slopes_", shock_i, ".pdf"),
         plot = p, path = file.path(path, "2.Outcome", "Figures"),
         width = 10, height = 8)
}

#Ireland dummy ---------------------------
df_export <- dataph %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(
    exports_yoy = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 4),
    ln_gdp_us_yoy = ln_gdp_us - dplyr::lag(ln_gdp_us, 4),
    ln_gdp_yoy = log(gdp) - dplyr::lag(log(gdp), 4),
    ln_vix_yoy = ln_vix - dplyr::lag(ln_vix, 4),
    ireland = as.integer(country == "Ireland"),
    trend = row_number(),
    year = factor(format(date, "%Y")),
    q = factor(quarters(date))
  ) %>%
  ungroup()

df_export <- df_export %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(exports_yoy_l1 = dplyr::lag(exports_yoy, 1)) %>%
  ungroup()

shock_labels <- c(
  ln_gdp_us_yoy = "US GDP growth",
  ln_gdp_yoy = "S&P return",
  ln_vix = "VIX",
  ln_vix_yoy = "VIX growth"
)

run_ireland_model <- function(shock) {
  
  df <- df_export %>%
    mutate(ireland_shock = ireland * .data[[shock]])
  
  # shock_l1 <- paste0(shock, "_l1")

  feols(
    as.formula(paste0("exports_yoy ~  ", shock, " + ireland_shock +reer_yoy| country[trend] ")),
    # as.formula(paste0("exports_yoy ~ exports_yoy_l1 + ", shock, " + ireland_shock | country[trend] + year + q")),
    data = df,
    vcov = ~ country,
    panel.id = ~ country + date
  )
}

mods_ireland <- lapply(names(shock_labels), run_ireland_model)
names(mods_ireland) <- names(shock_labels)

etable(mods_ireland)

ireland_effect <- bind_rows(lapply(names(mods_ireland), function(shock) {
  
  m <- mods_ireland[[shock]]
  b <- coef(m)
  V <- vcov(m)
  
  pooled_est <- b[shock]
  pooled_se <- sqrt(V[shock, shock])
  
  ireland_diff_est <- b["ireland_shock"]
  ireland_diff_se <- sqrt(V["ireland_shock", "ireland_shock"])
  
  ireland_total_est <- pooled_est + ireland_diff_est
  ireland_total_se <- sqrt(
    V[shock, shock] +
      V["ireland_shock", "ireland_shock"] +
      2 * V[shock, "ireland_shock"]
  )
  
  tibble(
    shock = shock,
    shock_lab = shock_labels[shock],
    pooled_est = pooled_est,
    pooled_se = pooled_se,
    pooled_pval = 2 * pnorm(-abs(pooled_est / pooled_se)),
    ireland_diff_est = ireland_diff_est,
    ireland_diff_se = ireland_diff_se,
    ireland_diff_pval = 2 * pnorm(-abs(ireland_diff_est / ireland_diff_se)),
    ireland_total_est = ireland_total_est,
    ireland_total_se = ireland_total_se,
    ireland_total_pval = 2 * pnorm(-abs(ireland_total_est / ireland_total_se))
  )
}))

ireland_effect
ireland_chart <- ireland_effect %>%
  mutate(
    ireland_est = pooled_est + ireland_diff_est,
    ireland_se = sqrt(pooled_se^2 + ireland_diff_se^2),
    ireland_pval = ireland_total_pval
  ) %>%
  select(shock, shock_lab,
         pooled_est, pooled_se, pooled_pval,
         ireland_est, ireland_se, ireland_pval) %>%
  pivot_longer(
    cols = c(pooled_est, ireland_est),
    names_to = "effect",
    values_to = "estimate"
  ) %>%
  mutate(
    se = ifelse(effect == "pooled_est", pooled_se, ireland_se),
    pval = ifelse(effect == "pooled_est", pooled_pval, ireland_pval),
    ci_low = estimate - 1.645 * se,
    ci_high = estimate + 1.645 * se,
    effect = recode(effect,
                    pooled_est = "Pooled",
                    ireland_est = "Ireland"),
    line_type = ifelse(pval < 0.10, "Significant", "Not significant")
  )

p <- ireland_chart %>%
  ggplot(aes(x = shock_lab, y = estimate, color = effect, linetype = line_type)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.55),
                 linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.55), size = 2) +
  labs(x = "", y = "Coefficient", color = "", linetype = "",
       title = "Pooled and Ireland shock effects") +
  scale_color_manual(values = c(
    "Pooled" = base_cols[8],
    "Ireland" = base_cols[1])) +
  scale_linetype_manual(values = c(
    "Significant" = "solid",
    "Not significant" = "11")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 8))

p

fig_path <- file.path(path, "2.Outcome", "Figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

ggsave("ireland_pooled_shocks.pdf",
       plot = p, path = fig_path,
       width = 10, height = 7)


#4. Rolling volatilities estimation#############################################
#4.1 All models----------------------------------------------------------------
df_export_vol <- dataph %>%
  arrange(country, counterparty, date) %>%
  group_by(country, counterparty) %>%
  mutate(
    exports_yoy = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 4),
    exports_gr  = log(exports_q_sum) - dplyr::lag(log(exports_q_sum), 1),
    ln_gdp_us_gr = ln_gdp_us - dplyr::lag(ln_gdp_us, 1),
    ln_globgdp_yoy = ln_global_gdp - dplyr::lag(ln_global_gdp, 4),
    ln_globgdp_gr = ln_global_gdp - dplyr::lag(ln_global_gdp, 1),
    ln_gdp_gr    = log(gdp) - dplyr::lag(log(gdp), 1),
    
    exports_vol_2y = zoo::rollapply(exports_gr, 8, sd, fill = NA, align = "right", na.rm = TRUE),
    gdp_us_vol_2y  = zoo::rollapply(ln_gdp_us_gr, 8, sd, fill = NA, align = "right", na.rm = TRUE),
    gdp_vol_2y     = zoo::rollapply(ln_gdp_gr, 8, sd, fill = NA, align = "right", na.rm = TRUE),
    vix_avg_2y     = zoo::rollapply(ln_vix, 8, mean, fill = NA, align = "right", na.rm = TRUE),
    globgdp_vol_2y = zoo::rollapply(ln_globgdp_gr, 8, sd, fill = NA, align = "right", na.rm = TRUE),
    
    exports_vol_2y_l1 = dplyr::lag(exports_vol_2y, 1),
    gdp_us_vol_2y_l1  = dplyr::lag(gdp_us_vol_2y, 1),
    gdp_vol_2y_l1     = dplyr::lag(gdp_vol_2y, 1),
    vix_avg_2y_l1     = dplyr::lag(vix_avg_2y, 1),
    globgdp_vol_2y_l1 = dplyr::lag(globgdp_vol_2y, 1),
    
    trend = row_number(),
    year = factor(format(date, "%Y")),
    q = factor(quarters(date))
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(exports_vol_2y, exports_vol_2y_l1,
        gdp_us_vol_2y, gdp_us_vol_2y_l1,
        gdp_vol_2y, gdp_vol_2y_l1,
        vix_avg_2y, vix_avg_2y_l1, 
        globgdp_vol_2y, globgdp_vol_2y_l1),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_std"
    )
  )

shock_labels_vol <- c(
  gdp_us_vol_2y_std = "US GDP volatility",
  globgdp_vol_2y_std = "S&P return volatility", #TO CHANGE
  vix_avg_2y_std = "VIX rolling average"
)

model_grid <- expand.grid(
  shock = names(shock_labels_vol),
  ar = c(FALSE, TRUE),
  stringsAsFactors = FALSE
) %>%
  mutate(
    model = paste0(shock, ifelse(ar, "_ar", "_noar")),
    model_lab = paste0(shock_labels_vol[shock], ifelse(ar, " - AR(1)", "")),
    yvar = "exports_vol_2y_std",
    arvar ="exports_vol_2y_l1_std",
    vc = "DK"
  )
#Descriptive stats----------
desc_vars <- c(
  exports_vol_2y = "Export volatility",
  exports_yoy = "Export YoY growth",
  gdp_us_vol_2y = "US GDP volatility",
  globgdp_vol_2y = "S&P return volatility",
  vix_avg_2y = "VIX rolling average",
  reer_yoy = "Real FX yoy change"
)

desc_table <- df_export_vol %>%
  summarise(across(
    all_of(names(desc_vars)),
    list(
      N = ~ sum(!is.na(.x)),
      Mean = ~ mean(.x, na.rm = TRUE),
      SD = ~ sd(.x, na.rm = TRUE),
      Min = ~ min(.x, na.rm = TRUE),
      Max = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.+)_(N|Mean|SD|Min|Max)") %>%
  mutate(
    variable = recode(variable, !!!desc_vars),
    across(c(Mean, SD, Min, Max), ~ sprintf("%.3f", .x))
  ) %>%
  rename(Variable = variable)

desc_table

tab_path <- file.path(path, "2.Outcome")
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)

desc_table %>%
  kbl(
    format = "html",
    escape = FALSE,
    caption = "Descriptive statistics"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  save_kable(file.path(tab_path, "descriptive_statistics.html"))

#Run model----
run_vol_model <- function(shock, ar, yvar, arvar, vc) {
  
  rhs <- ifelse(
    ar,
    paste0(arvar, " + i(country, ", shock, ")"),
    paste0("i(country, ", shock, ")")
  )
  
  feols(
    as.formula(paste0(yvar, " ~ ", rhs, " + reer_yoy | country")),
    data = df_export_vol,
    vcov = if (vc == "country") ~ country else{"DK"},
    panel.id = ~ country + date
  )
}

mods_vol <- lapply(seq_len(nrow(model_grid)), function(i) {
  run_vol_model(model_grid$shock[i], model_grid$ar[i],
                model_grid$yvar[i], model_grid$arvar[i], model_grid$vc[i])
})

names(mods_vol) <- model_grid$model

get_vcov <- function(m, vc) {
  if (vc == "country") vcov(m, vcov = "hetero") else vcov(m, vcov = "DK")
}

get_ct <- function(m, vc) {
  if (vc == "country") coeftable(m, vcov = "hetero") else coeftable(m, vcov = "DK")
}

stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    p < 0.15 ~ "+",
    TRUE ~ ""
  )
}

slopes_tab_vol <- bind_rows(lapply(seq_len(nrow(model_grid)), function(i) {
  
  m <- mods_vol[[model_grid$model[i]]]
  shock <- model_grid$shock[i]
  
  as.data.frame(get_ct(m, model_grid$vc[i])) %>%
    rownames_to_column("term") %>%
    filter(grepl("country::", term)) %>%
    mutate(
      model = model_grid$model[i],
      model_lab = model_grid$model_lab[i],
      shock = shock,
      shock_lab = shock_labels_vol[shock],
      country = sub("country::", "", term),
      country = sub(paste0(":", shock, ".*"), "", country),
      ci_low = Estimate - 1.645 * `Std. Error`,
      ci_high = Estimate + 1.645 * `Std. Error`,
      line_type = ifelse(`Pr(>|t|)` < 0.10, "Significant", "Not significant / truncated"),
      coef = paste0(sprintf("%.3f", Estimate), stars(`Pr(>|t|)`),
                    " (", sprintf("%.3f", `Std. Error`), ")")
    ) %>%
    select(model, model_lab, shock, shock_lab, country,
           estimate = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`,
           ci_low, ci_high, line_type, coef)
}))

wald_joint <- function(m, vc) {
  b <- coef(m)
  V <- get_vcov(m, vc)
  terms <- names(b)[grepl("country::", names(b))]
  W <- as.numeric(t(b[terms]) %*% solve(V[terms, terms]) %*% b[terms])
  pval <- pchisq(W, df = length(terms), lower.tail = FALSE)
  ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
}

lb_test <- function(m, lag_i) {
  x <- Box.test(resid(m), lag = lag_i, type = "Ljung-Box")
  ifelse(x$p.value < 0.001, "<0.001", sprintf("%.3f", x$p.value))
}

sig_count <- function(m, vc) {
  ct <- as.data.frame(get_ct(m, vc)) %>%
    rownames_to_column("term") %>%
    filter(grepl("country::", term))
  
  paste0(sum(ct$`Pr(>|t|)` < 0.15, na.rm = TRUE), "/", nrow(ct))
}

lb_test <- function(m, lag_i) {
  x <- Box.test(resid(m), lag = lag_i, type = "Ljung-Box")
  paste0(sprintf("%.2f", as.numeric(x$statistic)),
         "; p=", ifelse(x$p.value < 0.001, "<0.001", sprintf("%.3f", x$p.value)))
}

tests_vol <- bind_rows(lapply(seq_len(nrow(model_grid)), function(i) {
  
  m <- mods_vol[[model_grid$model[i]]]
  
  tibble(
    country = c("Joint Wald: all country slopes = 0",
                "Significant slopes at 10%",
                "Ljung-Box autocorrelation lag 4",
                "Ljung-Box autocorrelation lag 8",
                "Observations",
                "R2",
                "Within R2"),
    model_lab = model_grid$model_lab[i],
    coef = c(wald_joint(m, model_grid$vc[i]),
             sig_count(m, model_grid$vc[i]),
             lb_test(m, 4),
             lb_test(m, 8),
             sprintf("%.0f", nobs(m)),
             sprintf("%.3f", r2(m, "r2")),
             sprintf("%.3f", r2(m, "wr2")))
  )
}))

coef_table <- slopes_tab_vol %>%
  select(country, model_lab, coef) %>%
  pivot_wider(names_from = model_lab, values_from = coef)

tests_table <- tests_vol %>%
  pivot_wider(names_from = model_lab, values_from = coef)

final_table <- bind_rows(
  coef_table,
  tibble(country = "", !!!setNames(rep(list(""), ncol(coef_table) - 1), names(coef_table)[-1])),
  tests_table
)

final_table

fig_path <- file.path(path, "2.Outcome", "Figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
tab_path <- file.path(path, "2.Outcome")
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)

final_table_html <- final_table %>%
  rename(
    Variable = country,
    `US GDP volatility` = `US GDP volatility`,
    `AR(1)` = `US GDP volatility - AR(1)`,
    `S&P return volatility ` = `S&P return volatility`,
    `AR(1) ` = `S&P return volatility - AR(1)`,
    `VIX rolling average  ` = `VIX rolling average`,
    `AR(1)  ` = `VIX rolling average - AR(1)`
  )

final_table_html <- final_table_html %>%
  select(-matches("^AR\\(1\\)"))
final_table_html[is.na(final_table_html)] <- "--"

final_table_html %>%
  kbl(
    format = "html",
    escape = FALSE,
    align = c("l", rep("c", 3)),
    caption = "Rolling volatility models: country-specific coefficients and diagnostics"
  ) %>%
  # add_header_above(c(
  #   " " = 1,
  #   "US GDP volatility" = 1,
  #   "S&P return volatility" = 1,
  #   "VIX rolling average" = 1
  # )) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(nrow(coef_table), extra_css = "border-bottom: 2px solid black;") %>%
  save_kable(file.path(tab_path, "rolling_volatility_all_models_tests.html"))

#Chart slopes------------------
for (shock_i in names(shock_labels_vol)) {
  
  p <- slopes_tab_vol %>%
    filter(shock == shock_i,
           model == paste0(shock_i, "_noar")) %>%
    ggplot(aes(x = reorder(country, estimate), y = estimate, linetype = line_type)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_linerange(aes(ymin = ci_low, ymax = ci_high),
                   linewidth = 0.7, color = base_cols[8]) +
    geom_point(size = 2, color = base_cols[1]) +
    coord_flip() +
    labs(x = "", y = "Country-specific coefficient", linetype = "",
         title = paste0(shock_labels_vol[shock_i], " - rolling volatility slopes")) +
    scale_linetype_manual(values = c(
      "Significant" = "solid",
      "Not significant / truncated" = "11")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey90"),
          axis.text.x = element_text(angle = 25, hjust = 1),
          plot.caption = element_text(hjust = 0, size = 8))
  
  print(p)
  
  ggsave(filename = paste0("country_specific_slopes_vol_2y_", shock_i, "_noar.pdf"),
         plot = p, path = fig_path,
         width = 6, height = 4)
  ggsave(filename = paste0("country_specific_slopes_vol_2y_", shock_i, "_noar.png"),
         plot = p, path = fig_path,
         width = 10, height = 8, dpi = 500)
}
#Wald chart--------------------
wald_ireland_chart <- function(shock_i) {
  
  model_i <- paste0(shock_i, "_noar")
  
  if (length(model_i) == 0 || is.na(model_i) || is.null(mods_vol[[model_i]])) {
    stop("Model not found. Check names(mods_vol).")
  }
  
  m <- mods_vol[[model_i]]
  vc_i <- model_grid$vc[model_grid$model == model_i][1]
  
  V <- if (vc_i == "country") vcov(m, vcov = "hetero") else vcov(m, vcov = "DK")
  b <- coef(m)
  
  terms <- names(b)[grepl("^country::", names(b)) &
                      grepl(paste0(":", shock_i, "$"), names(b))]
  
  slope_map <- tibble(term = terms) %>%
    mutate(
      country = sub("country::", "", term),
      country = sub(paste0(":", shock_i, "$"), "", country)
    )
  
  ireland_term <- slope_map %>%
    filter(country == "Ireland") %>%
    pull(term)
  
  if (length(ireland_term) == 0) {
    stop("Ireland term not found. Check shock_i and coefficient names.")
  }
  
  ireland_vs_all <- slope_map %>%
    filter(country != "Ireland") %>%
    rowwise() %>%
    mutate(
      ireland_est = b[ireland_term],
      other_est = b[term],
      diff = ireland_est - other_est,
      se_diff = sqrt(V[ireland_term, ireland_term] + V[term, term] - 2 * V[ireland_term, term]),
      t_stat = diff / se_diff,
      p_one_sided = pnorm(t_stat),
      ireland_lower_10 = diff < 0 & p_one_sided < 0.10
    ) %>%
    ungroup()
  
  p <- ireland_vs_all %>%
    mutate(
      country = reorder(country, diff),
      sig_10 = ifelse(ireland_lower_10,
                      "Ireland significantly lower (10%)",
                      "Not significant")
    ) %>%
    ggplot(aes(x = country, y = diff, color = sig_10)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_linerange(aes(ymin = diff - 1.645 * se_diff,
                       ymax = diff + 1.645 * se_diff),
                   linewidth = 0.7) +
    geom_point(size = 2) +
    coord_flip() +
    labs(x = "", y = "Ireland slope - other country slope", color = "",
         title = paste0("Ireland vs other countries: ",
                        model_grid$model_lab[model_grid$model == model_i][1])) +
    scale_color_manual(values = c(
      "Ireland significantly lower (10%)" = base_cols[1],
      "Not significant" = "grey70")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey90"),
          axis.text.x = element_text(angle = 25, hjust = 1),
          plot.caption = element_text(hjust = 0, size = 8))
  
  print(p)
  
  ggsave(filename = paste0("Waldtest_", model_i, ".pdf"),
         plot = p, path = fig_path,
         width = 6, height = 4)
  
  ggsave(filename = paste0("Waldtest_", model_i, ".png"),
         plot = p, path = fig_path,
         width = 10, height = 8, dpi = 500)
  
  ireland_vs_all
}

ireland_usgdp_noar <- wald_ireland_chart("gdp_us_vol_2y_std")
ireland_globalgdp_noar <- wald_ireland_chart("globgdp_vol_2y_std")
ireland_vix_noar <- wald_ireland_chart("vix_avg_2y_std")

ireland_tests_all <- bind_rows(
  ireland_usgdp_noar %>% mutate(shock_lab = "US GDP volatility"),
  ireland_globalgdp_noar %>% mutate(shock_lab = "S&P volatility"),
  ireland_vix_noar %>% mutate(shock_lab = "VIX rolling average")
)

ireland_summary <- ireland_tests_all %>%
  group_by(shock_lab) %>%
  summarise(
    ireland_beta = first(ireland_est),
    mean_other_beta = mean(other_est, na.rm = TRUE),
    diff_vs_mean = ireland_beta - mean_other_beta,
    n_lower_10 = sum(ireland_lower_10, na.rm = TRUE),
    n_tests = n(),
    share_lower_10 = n_lower_10 / n_tests,
    ireland_lower_than_mean = ireland_beta < mean_other_beta,
    .groups = "drop"
  )

ireland_summary

ireland_summary_text <- ireland_summary %>%
  mutate(
    sentence = paste0(
      shock_lab, ": Ireland's beta is ",
      sprintf("%.3f", ireland_beta),
      ", compared with an average beta of ",
      sprintf("%.3f", mean_other_beta),
      " for the other countries. Ireland is significantly lower than ",
      n_lower_10, " out of ", n_tests,
      " countries at the 10% level."
    )
  ) %>%
  pull(sentence)

cat(paste(ireland_summary_text, collapse = "\n"))

#4.2 Pooled vs Ireland dummy----------------------------------------------------
model_grid_dummy <- model_grid

# run_dummy_model <- function(shock, ar, yvar, arvar, vc) {
# 
#   df <- df_export_vol %>%
#     mutate(
#       ireland = as.integer(country == "Ireland"),
#       ireland_shock = ireland * .data[[shock]]
#     )
# 
#   rhs <- ifelse(
#     ar,
#     paste0(arvar, " + ", shock, " + ireland_shock + reer_yoy "),
#     paste0(shock, " + ireland_shock + reer_yoy")
#   )
# 
#   feols(
#     as.formula(paste0(yvar, " ~ ", rhs, " | country")),
#     data = df,
#     vcov = if (vc == "country") ~ country else{"DK"},
#     panel.id = ~ country + date
#   )
# }
run_dummy_model <- function(shock, ar, yvar, arvar, vc) {
  
  df <- df_export_vol %>%
    mutate(ireland = as.integer(country == "Ireland"))
  
  rhs <- ifelse(
    ar,
    paste0(arvar, " + ", shock, " + i(ireland, ", shock, ", ref = 0) + reer_yoy"),
    paste0(shock, " + i(ireland, ", shock, ", ref = 0) + reer_yoy")
  )
  
  feols(
    as.formula(paste0(yvar, " ~ ", rhs, " | country")),
    data = df,
    vcov = if (vc == "country") ~ country else "DK",
    panel.id = ~ country + date
  )
}
mods_dummy <- lapply(seq_len(nrow(model_grid_dummy)), function(i) {
  run_dummy_model(model_grid_dummy$shock[i], model_grid_dummy$ar[i],
                  model_grid_dummy$yvar[i], model_grid_dummy$arvar[i],
                  model_grid_dummy$vc[i])
})

names(mods_dummy) <- model_grid_dummy$model

get_vcov <- function(m, vc) {
  if (vc == "country") vcov(m, vcov = ~ country) else vcov(m, vcov = "DK")
}

get_ct <- function(m, vc) {
  if (vc == "country") coeftable(m, vcov = ~ country) else coeftable(m, vcov = "DK")
}

stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# get_dummy_effects <- function(m, shock, vc) {
# 
#   b <- coef(m)
#   V <- get_vcov(m, vc)
#   ct <- as.data.frame(get_ct(m, vc)) %>%
#     rownames_to_column("term")
# 
#   pooled_est <- b[shock]
#   pooled_se <- sqrt(V[shock, shock])
#   pooled_p <- ct$`Pr(>|t|)`[ct$term == shock]
# 
#   diff_est <- b["ireland_shock"]
#   diff_se <- sqrt(V["ireland_shock", "ireland_shock"])
#   diff_p <- ct$`Pr(>|t|)`[ct$term == "ireland_shock"]
# 
#   ireland_est <- pooled_est + diff_est
#   ireland_se <- sqrt(V[shock, shock] + V["ireland_shock", "ireland_shock"] + 2 * V[shock, "ireland_shock"])
#   ireland_p <- 2 * pnorm(-abs(ireland_est / ireland_se))
# 
#   tibble(
#     effect = c("Pooled", "Ireland", "Ireland difference"),
#     estimate = c(pooled_est, ireland_est, diff_est),
#     se = c(pooled_se, ireland_se, diff_se),
#     pval = c(pooled_p, ireland_p, diff_p),
#     coef = paste0(sprintf("%.3f", estimate), stars(pval),
#                   " (", sprintf("%.3f", se), ")")
#   )
# }

get_dummy_effects <- function(m, shock, vc) {
  
  b <- coef(m)
  V <- get_vcov(m, vc)
  ct <- as.data.frame(get_ct(m, vc)) %>%
    rownames_to_column("term")
  
  diff_term <- ct$term[grepl("^ireland::1:", ct$term)][1]
  
  pooled_est <- b[shock]
  pooled_se <- sqrt(V[shock, shock])
  pooled_p <- ct$`Pr(>|t|)`[ct$term == shock]
  
  diff_est <- b[diff_term]
  diff_se <- sqrt(V[diff_term, diff_term])
  diff_p <- ct$`Pr(>|t|)`[ct$term == diff_term]
  
  tibble(
    effect = c("Pooled", "Ireland Total Effect"),
    estimate = c(pooled_est, diff_est),
    se = c(pooled_se, diff_se),
    pval = c(pooled_p, diff_p),
    coef = paste0(sprintf("%.3f", estimate), stars(pval),
                  " (", sprintf("%.3f", se), ")")
  )
}

dummy_coef_table <- bind_rows(lapply(seq_len(nrow(model_grid_dummy)), function(i) {
  
  m <- mods_dummy[[model_grid_dummy$model[i]]]
  
  get_dummy_effects(m, model_grid_dummy$shock[i], model_grid_dummy$vc[i]) %>%
    mutate(model_lab = model_grid_dummy$model_lab[i])
})) %>%
  select(effect, model_lab, coef) %>%
  pivot_wider(names_from = model_lab, values_from = coef) %>%
  rename(country = effect)

wald_ireland_diff <- function(m, vc) {
  
  ct <- as.data.frame(get_ct(m, vc)) %>%
    rownames_to_column("term")
  
  diff_term <- ct$term[grepl("^ireland::1:", ct$term)][1]
  
  if (is.na(diff_term)) {
    return("--")
  }
  
  tval <- ct$`t value`[ct$term == diff_term]
  pval <- ct$`Pr(>|t|)`[ct$term == diff_term]
  
  paste0(
    sprintf("%.2f", tval^2),
    "; p=", ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
  )
}

lb_test <- function(m, lag_i) {
  x <- Box.test(resid(m), lag = lag_i, type = "Ljung-Box")
  paste0(sprintf("%.2f", as.numeric(x$statistic)),
         "; p=", ifelse(x$p.value < 0.001, "<0.001", sprintf("%.3f", x$p.value)))
}

dummy_tests <- bind_rows(lapply(seq_len(nrow(model_grid_dummy)), function(i) {
  
  m <- mods_dummy[[model_grid_dummy$model[i]]]
  
  tibble(
    country = c("Wald: Ireland difference = 0",
                "Ljung-Box autocorrelation lag 4",
                "Ljung-Box autocorrelation lag 8",
                "Observations",
                "R2",
                "Within R2"),
    model_lab = model_grid_dummy$model_lab[i],
    coef = c(wald_ireland_diff(m, model_grid_dummy$vc[i]),
             lb_test(m, 4),
             lb_test(m, 8),
             sprintf("%.0f", nobs(m)),
             sprintf("%.3f", r2(m, "r2")),
             sprintf("%.3f", r2(m, "wr2")))
  )
})) %>%
  pivot_wider(names_from = model_lab, values_from = coef)

final_table_dummy <- bind_rows(
  dummy_coef_table,
  tibble(country = "", !!!setNames(rep(list(""), ncol(dummy_coef_table) - 1), names(dummy_coef_table)[-1])),
  dummy_tests
)

tab_path <- file.path(path, "2.Outcome")
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)
final_table_dummy_html <- final_table_dummy %>%
  rename(
    Variable = country,
    `US GDP volatility` = `US GDP volatility`,
    `AR(1)` = `US GDP volatility - AR(1)`,
    `S&P return volatility ` = `S&P return volatility`,
    `AR(1) ` = `S&P return volatility - AR(1)`,
    `VIX rolling average  ` = `VIX rolling average`,
    `AR(1)  ` = `VIX rolling average - AR(1)`
  )

final_table_dummy_html <- final_table_dummy_html %>%
  select(-matches("^AR\\(1\\)"))

final_table_dummy_html %>%
  kbl(
    format = "html",
    escape = FALSE,
    # align = c("l", rep("c", 6)),
    align = c("l", rep("c", 3)),
    caption = "Rolling volatility models: pooled and Ireland dummy coefficients"
  ) %>%
  # add_header_above(c(
  #   " " = 1,
  #   "US GDP volatility" = 2,
  #   "Global GDP volatility" = 2,
  #   "VIX rolling average" = 2
  # )) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(nrow(dummy_coef_table), extra_css = "border-bottom: 2px solid black;") %>%
  save_kable(file.path(tab_path, "rolling_volatility_dummy_ireland_tests.html"))


#run markdown----------------------------------
rmarkdown::render("policy_brief_exports.Rmd", output_format = "html_document")

rmarkdown::render(
  input = file.path(path, "policy_brief_exports.Rmd"),
  output_format = "html_document"
)

