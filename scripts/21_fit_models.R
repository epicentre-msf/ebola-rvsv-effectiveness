
# Script to fit all Bayesian logistic regression models

## Parameter: Number of imputations
m_imputations <- 50

## Parameter: Number of samples (per imputed data set)
nm_matchsample <- 10
## Parameter: Number of samples (raw data)
n_matchsample <- 20


# Set up ------------------------------------------------------------------

library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)

library(brms)

source(here("R/set_paths.R"))
paths <- set_paths()

source(here("R/fit_brms.R"))

dir_matched_output <- here("outputs", "1_data_mat")
dir_model_fit <- here("outputs", "2_model_fit")


# Load data ---------------------------------------------------------------

# Imputed

## Vaccination-onset delay 10+ days

## Unmatched
imp_a1__unmat <- readRDS(
  file = here("outputs", "0_data_imp", paste0("imp_m", m_imputations, "_unmatched_10d.rds"))
) |> 
  filter(contact_evd_case == "Yes") |> 
  mutate(.iter = .imp)
imp_a1__unmat <- list(
  sample_df = imp_a1__unmat,
  sample_list = split(x = data.table::as.data.table(imp_a1__unmat), by = ".imp")
)

### Contact with an EVD case
imp_a1 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1.rds"))
)

### Other EVD exposure definitions
imp_a2 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a2.rds"))
)
imp_a3 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a3.rds"))
)
imp_a4 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a4.rds"))
)

### Vaccination strategy
imp_a1__v1 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__v1.rds"))
)
imp_a1__v2 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__v2.rds"))
)

### Sex
imp_a1__male_df <- imp_a1$sample_df %>%
  filter(x_sex == "Male")
imp_a1__male <- list(
  sample_df = imp_a1__male_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__male_df), by = ".iter")
)
imp_a1__female_df <- imp_a1$sample_df %>%
  filter(x_sex == "Female")
imp_a1__female <- list(
  sample_df = imp_a1__female_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__female_df), by = ".iter")
)

### Age (under/over 5 years)
imp_a1__u5_df <- imp_a1$sample_df %>%
  filter(x_age_gp == "y0_4")
imp_a1__u5 <- list(
  sample_df = imp_a1__u5_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__u5_df), by = ".iter") 
)
imp_a1__o5_df <- imp_a1$sample_df %>%
  filter(x_age_gp != "y0_4")
imp_a1__o5 <- list(
  sample_df = imp_a1__o5_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__o5_df), by = ".iter") 
)
### Age (under/over 15 years)
imp_a1__u15_df <- imp_a1$sample_df %>%
  filter(x_age_gp %in% c("y0_4", "y5_14"))
imp_a1__u15 <- list(
  sample_df = imp_a1__u15_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__u15_df), by = ".iter") 
)
imp_a1__o15_df <- imp_a1$sample_df %>%
  filter(!x_age_gp %in% c("y0_4", "y5_14"))
imp_a1__o15 <- list(
  sample_df = imp_a1__o15_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__o15_df), by = ".iter") 
)

### Excluding Mangina
imp_a1__ex_mangina <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__excl_mangina.rds"))
)
imp_a1__ex_mangina_all <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__excl_mangina_all.rds"))
)

### Excluding long vaccination-onset delays
imp_a1__ex_delay_3m <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__delay_3m.rds"))
)
imp_a1__ex_delay_6m <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__delay_6m.rds"))
)

### Alternative matching definition
imp_a1__alt1 <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__alt1.rds"))
)
imp_a1__alt1$sample_df <- imp_a1__alt1$sample_df %>%
  mutate(
    .iter = (.imp - 1)*10 + .iter,
    x_strata = paste0(stringr::str_remove(adm2_name__res, "_"), "_", stringr::str_remove(month_onset, "-"))
    )
imp_a1__alt1 <- list(
  sample_df = imp_a1__alt1$sample_df,
  sample_list = split(x = data.table::as.data.table(imp_a1__alt1$sample_df), by = ".iter")
)


### Post-hoc applying suspected case definition
imp_a1__susp <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__susp.rds"))
)

## Vaccination-onset delay 3 - 9 days
imp_a1_3d <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_3d_a1.rds"))
)


## Misclassification of vaccination status

### Non-differential
vmisclass_p5 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__p5.rds")
)
vmisclass_p10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__p10.rds")
)

### Differential
vmisclass_evdp_v_p5 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Vaccinated (10+ days)_p5.rds")
)
vmisclass_evdp_v_p10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Vaccinated (10+ days)_p10.rds")
)
vmisclass_evdp_u_p5 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Unvaccinated_p5.rds")
)
vmisclass_evdp_u_p10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Unvaccinated_p10.rds")
)
vmisclass_evdn_v_p5 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Vaccinated (10+ days)_p5.rds")
)
vmisclass_evdn_v_p10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Vaccinated (10+ days)_p10.rds")
)
vmisclass_evdn_u_p5 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Unvaccinated_p5.rds")
)
vmisclass_evdn_u_p10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Unvaccinated_p10.rds")
)
#
vmisclass_evdp_v_n1 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Vaccinated (10+ days)_n1.rds")
)
vmisclass_evdp_v_n10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Vaccinated (10+ days)_n10.rds")
)
vmisclass_evdp_u_n1 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Unvaccinated_n1.rds")
)
vmisclass_evdp_u_n10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-pos_Unvaccinated_n10.rds")
)
vmisclass_evdn_v_n1 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Vaccinated (10+ days)_n1.rds")
)
vmisclass_evdn_v_n10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Vaccinated (10+ days)_n10.rds")
)
vmisclass_evdn_u_n1 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Unvaccinated_n1.rds")
)
vmisclass_evdn_u_n10 <- readRDS(
  file = here(dir_matched_output, "imp_mat_10d_a1__EVD-neg_Unvaccinated_n10.rds")
)


# No imputation (complete-case analysis)

## Vaccination-onset delay 10+ days, exposure def A1
raw_a1 <- readRDS(
  file = here(dir_matched_output, paste0("raw_matched_n", n_matchsample, "_10d_a1.rds"))
)



# Fit models (1) ----------------------------------------------------------

# Imputed

## Unmatched
fit_int <- purrr::map(
  .x = unique(imp_a1__unmat$sample_df$.imp),
  .f = ~ {
    
    out_int <- brms::brm_multiple(
      data = imp_a1__unmat$sample_list[.x],
      formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fit_int, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__unmat.rds")
)

## Vaccination-onset delay 10+ days

### Contact with an EVD case
fit <- fit_brms(data = imp_a1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1.rds")
)

### Other EVD exposure definitions
fit <- fit_brms(data = imp_a2, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a2.rds")
)

fit <- fit_brms(data = imp_a3, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a3.rds")
)

fit <- fit_brms(data = imp_a4, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a4.rds")
)

### Vaccination strategy
fit <- fit_brms(data = imp_a1__v1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__v1.rds")
)

fit <- fit_brms(data = imp_a1__v2, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__v2.rds")
)

### Sex
fit_int <- purrr::map(
  .x = unique(imp_a1__male$sample_df$.imp),
  .f = ~ {
    
    list_vals <- nm_matchsample*(.x - 1) + 1:nm_matchsample
    
    out_int <- brms::brm_multiple(
      data = imp_a1__male$sample_list[list_vals],
      formula = "x_evd_status ~ x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fit_int, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__male.rds")
)

fit_int <- purrr::map(
  .x = unique(imp_a1__female$sample_df$.imp),
  .f = ~ {
    
    list_vals <- nm_matchsample*(.x - 1) + 1:nm_matchsample
    
    out_int <- brms::brm_multiple(
      data = imp_a1__female$sample_list[list_vals],
      formula = "x_evd_status ~ x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fit_int, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__female.rds")
)



### Age (children vs. adults)
excl_iter <- imp_a1__u5$sample_df %>%
  count(.imp, .iter, evd_status, vacc_status) %>%
  count(.imp, .iter) %>%
  filter(n <= 2) %>%
  pull(.iter)
fit_int <- purrr::map(
  .x = unique(imp_a1__u5$sample_df$.imp),
  .f = ~ {
    
    list_vals <- setdiff(nm_matchsample*(.x - 1) + 1:nm_matchsample, excl_iter)
    
    out_int <- brms::brm_multiple(
      data = imp_a1__u5$sample_list[list_vals],
      formula = "x_evd_status ~ x_sex + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fit_int, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__age_u5.rds")
)

fit <- fit_brms(data = imp_a1__u15, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__age_u15.rds")
)

fit <- fit_brms(data = imp_a1__o5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__age_o5.rds")
)

fit <- fit_brms(data = imp_a1__o15, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__age_o15.rds")
)

### Excluding various data
fit <- fit_brms(data = imp_a1__ex_mangina, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__ex_mangina.rds")
)
fit <- fit_brms(data = imp_a1__ex_mangina_all, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__ex_mangina_all.rds")
)
fit <- fit_brms(data = imp_a1__ex_delay_3m, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__ex_delay_3m.rds")
)
fit <- fit_brms(data = imp_a1__ex_delay_6m, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__ex_delay_6m.rds")
)

### Alternative matching definition
fit <- fit_brms(data = imp_a1__alt1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__alt1.rds")
)

imp_a1__sexage <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__alt_sexage.rds"))
)
idx <- 100
fit_int <- purrr::map(
  .x = unique(imp_a1__sexage$sample_df$.iter)[idx:(idx+99)],
  .f = ~ {
    
    out_int <- brms::brm_multiple(
      data = imp_a1__sexage$sample_list[.x],
      formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fit_int, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, paste0("imp_10d_a1__altsexage_", idx, ".rds"))
)

### Post-hoc applying suspected case definition
fit <- fit_brms(data = imp_a1__susp, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__susp.rds")
)

### Severity (possible advanced cases)
imp_a1__adv <- readRDS(
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__adv.rds"))
)
fit <- fit_brms(data = imp_a1__adv, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__adv.rds")
)


## Vaccination-onset delay 3 - 9 days
fits <- purrr::map(
  .x = unique(imp_a1_3d$sample_df$.imp),
  .f = ~ {
    
    list_vals <- 10*(.x - 1) + 1:10
    
    out_int <- brms::brm_multiple(
      data = imp_a1_3d$sample_list[list_vals],
      formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_3_9d")
      ),
      iter = 4000,
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fits, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_3d_a1.rds")
)


# No imputation (complete-case analysis)

## Vaccination-onset delay 10+ days, exposure def A1
fit <- brms::brm_multiple(
  data = raw_a1$sample_list,
  formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
  family = brms::bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = b),
    prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
  ),
  combine = FALSE
)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "raw_10d_a1.rds")
)



# Fit models (2) ----------------------------------------------------------
# (prior distributions)

# phi ~ N(0, 1)
fits <- purrr::map(
  .x = unique(imp_a1$sample_df$.imp),
  .f = ~ {
    
    list_vals <- 10*(.x - 1) + 1:10
    
    out_int <- brms::brm_multiple(
      data = imp_a1$sample_list[list_vals],
      formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(0, 1), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fits, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__prior_n01.rds")
)

# phi ~ N(-2, 1)
fits <- purrr::map(
  .x = unique(imp_a1$sample_df$.imp),
  .f = ~ {
    
    list_vals <- 10*(.x - 1) + 1:10
    
    out_int <- brms::brm_multiple(
      data = imp_a1$sample_list[list_vals],
      formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
      family = brms::bernoulli(),
      prior = c(
        prior(normal(0, 1), class = "Intercept"),
        prior(normal(0, 1), class = b),
        prior(normal(-2, 1), class = b, coef = "x_vacc_statusvaccinated_10d")
      ),
      combine = FALSE
    )
    
    return(out_int)
    
  }
)
fit <- unlist(fits, recursive = FALSE)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__prior_nm21.rds")
)


# Fit models (3) ----------------------------------------------------------
# (vaccination misclassification)

# Non-differential
fit <- fit_brms(data = vmisclass_p5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__mcv_p5.rds")
)
fit <- fit_brms(data = vmisclass_p10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__mcv_p10.rds")
)

# Differential, p (4x infection-vaccination group)
fit <- fit_brms(data = vmisclass_evdp_v_p5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_v_p5.rds")
)
fit <- fit_brms(data = vmisclass_evdp_v_p10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_v_p10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdp_u_p5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_u_p5.rds")
)
fit <- fit_brms(data = vmisclass_evdp_u_p10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_u_p10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdn_v_p5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_v_p5.rds")
)
fit <- fit_brms(data = vmisclass_evdn_v_p10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_v_p10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdn_u_p5, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_u_p5.rds")
)
fit <- fit_brms(data = vmisclass_evdn_u_p10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_u_p10.rds")
)

# Differential, n (4x infection-vaccination group)
fit <- fit_brms(data = vmisclass_evdp_v_n1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_v_n1.rds")
)
fit <- fit_brms(data = vmisclass_evdp_v_n10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_v_n10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdp_u_n1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_u_n1.rds")
)
fit <- fit_brms(data = vmisclass_evdp_u_n10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdp_u_n10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdn_v_n1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_v_n1.rds")
)
fit <- fit_brms(data = vmisclass_evdn_v_n10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_v_n10.rds")
)
#
fit <- fit_brms(data = vmisclass_evdn_u_n1, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_u_n1.rds")
)
fit <- fit_brms(data = vmisclass_evdn_u_n10, n_samples = nm_matchsample)
saveRDS(
  object = fit,
  file = here(dir_model_fit, "imp_10d_a1__evdn_u_n10.rds")
)
