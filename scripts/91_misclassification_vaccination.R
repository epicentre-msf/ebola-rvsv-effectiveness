
m_imputations <- 50
nm_matchsample <- 10

# Set up ------------------------------------------------------------------

library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)

source(here("R/set_paths.R"))
paths <- set_paths()

source(here("R/vacc_misclass.R"))


# Set up ------------------------------------------------------------------

dir_matched_output <- here("outputs", "1_data_mat")
dir_model_fit <- here("outputs", "2_model_fit")

# Load data: all imputed-matched samples
imp_a1 <- readRDS(
  file = here(
    dir_matched_output,
    paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1.rds"))
)

# Grid of n/p to switch in each infection-vaccination group
grid_n <- tidyr::expand_grid(
  gp_evd = c("EVD-pos", "EVD-neg"),
  gp_vacc = c("Unvaccinated", "Vaccinated (10+ days)"),
  n = c(1, 10)
)
grid_p <- tidyr::expand_grid(
  gp_evd = c("EVD-pos", "EVD-neg"),
  gp_vacc = c("Unvaccinated", "Vaccinated (10+ days)"),
  p = c(0.05, 0.1)
)


# Generate data (p) -------------------------------------------------------

# Non-differential
## p = 0.05
out_list <- purrr::map(
  .x = 1:length(imp_a1$sample_list),
  .f = ~ {
                         
    out <- switch_vaccination_int(
      df = imp_a1$sample_list[[.x]],
      p = 0.05
    )
    
  })
out_df <- tibble(bind_rows(out_list))
out <- list(
  sample_df = out_df,
  sample_list = out_list
)
saveRDS(
  object = out,
  file = here(dir_matched_output, "imp_mat_10d_a1__p5.rds")
)
## p = 0.1
out_list <- purrr::map(
  .x = 1:length(imp_a1$sample_list),
  .f = ~ {
    
    out <- switch_vaccination_int(
      df = imp_a1$sample_list[[.x]],
      p = 0.1
    )
    
  })
out_df <- tibble(bind_rows(out_list))
out <- list(
  sample_df = out_df,
  sample_list = out_list
)
saveRDS(
  object = out,
  file = here(dir_matched_output, "imp_mat_10d_a1__p10.rds")
)


# Differential
for(i in 1:nrow(grid_p)) {
  
  tmp_fname <- paste0(
    "imp_mat_10d_a1__",
    grid_p$gp_evd[i], "_", grid_p$gp_vacc[i], "_p", 100*grid_p$p[i]
  )
  message(paste0("Running ", tmp_fname, "..."))
  
  tmp <- switch_vaccination_p(
    obj = imp_a1,
    p = grid_p$p[i],
    gp_evd = grid_p$gp_evd[i], gp_vacc = grid_p$gp_vacc[i]
  )
  
  saveRDS(
    object = tmp,
    file = here(dir_matched_output, paste0(tmp_fname, ".rds"))
  )
  
}


# Generate data (n) -------------------------------------------------------

for(i in 1:nrow(grid_n)) {
  
  tmp <- switch_vaccination_n(
    obj = imp_a1,
    n = grid_n$n[i],
    gp_evd = grid_n$gp_evd[i], gp_vacc = grid_n$gp_vacc[i]
  )
  
  tmp_fname <- paste0(
    "imp_mat_10d_a1__",
    grid_n$gp_evd[i], "_", grid_n$gp_vacc[i], "_n", grid_n$n[i],
    ".rds"
  )
  saveRDS(
    object = tmp,
    file = here(dir_matched_output, tmp_fname)
  )
  
}
