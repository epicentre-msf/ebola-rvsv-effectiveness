
# Script to combine all posterior samples and compute prior/posterior VE


# Set up ------------------------------------------------------------------

library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)

library(brms)

source(here("R/set_paths.R"))
paths <- set_paths()

source(here("R/summarise_model.R"))


# Summarise models (1) ----------------------------------------------------

# List all models in outputs/fitted_models
list_models <- setdiff(
  list.files(
    path = here("outputs", "2_model_fit"),
    recursive = FALSE
  ),
  c("_archive", "_archive2", "imp_3d_a1.rds")
)

list_samples <- setdiff(
  list.files(
    path = here("outputs", "3_model_samples"),
    recursive = FALSE
  ),
  c("_archive", "_archive2", "imp_3d_a1.rds")
)

# Get posterior samples
for(.x in list_models) {
  
  message(paste0("Summarising model ", .x, "..."))
  
  if(!.x %in% list_samples) {
    brms_tmp <- readRDS(file = here("outputs", "2_model_fit", .x))
    brms_samples <- get_posteriors(brms_model = brms_tmp)
    saveRDS(
      object = brms_samples,
      file = here("outputs", "3_model_samples", .x)
    )
    rm(brms_tmp)
  } else {
    message("Model already summarised.")
  }
  
}


## Vaccination-onset delay 3-9 days
brms_tmp <- readRDS(file = here("outputs", "2_model_fit", "imp_3d_a1.rds"))
post <- purrr::map_df(
  .x = seq_along(brms_tmp),
  .f = ~ {
    out_int <- as_draws_df(brms_tmp[[.x]]) %>%
      tibble() %>%
      mutate(
        iter = as.character(.x),
        ve = 100*(1 - exp(b_x_vacc_statusvaccinated_3_9d))
      ) %>%
      select(iter, everything())
    
    return(out_int)
  }
)
saveRDS(
  object = post,
  file = here("outputs", "3_model_samples", "imp_3d_a1.rds")
) 


# Summarise models (2) ----------------------------------------------------

misclass_samples_p <- list_samples[
  which(
    grepl("imp_10d_a1.rds", list_samples) |
      ((grepl("__evdp", list_samples) | grepl("__evdn", list_samples))) & grepl("_p", list_samples))
]

# Make summary table
tb_p <- purrr::map_df(
  .x = misclass_samples_p,
  .f = ~ {
    
    dat_tmp <- readRDS(here("outputs", "3_model_samples", .x))
    out <- tibble(
      file = .x,
      ve_q50 = quantile(dat_tmp$ve, 0.5),
      ve_q025 = quantile(dat_tmp$ve, 0.025),
      ve_q25 = quantile(dat_tmp$ve, 0.25),
      ve_q75 = quantile(dat_tmp$ve, 0.75),
      ve_q975 = quantile(dat_tmp$ve, 0.975)
    )
    return(out)
    
  }
)
saveRDS(object = tb_p, file = here("outputs", "vmisclass_tb_p.rds"))


misclass_samples_n <- list_samples[
  which(
    grepl("imp_10d_a1.rds", list_samples) |
      ((grepl("__evdp", list_samples) | grepl("__evdn", list_samples))) & grepl("_n", list_samples))
]

# Make summary table
tb_n <- purrr::map_df(
  .x = misclass_samples_n,
  .f = ~ {
    
    dat_tmp <- readRDS(here("outputs", "3_model_samples", .x))
    out <- tibble(
      file = .x,
      ve_q50 = quantile(dat_tmp$ve, 0.5),
      ve_q025 = quantile(dat_tmp$ve, 0.025),
      ve_q25 = quantile(dat_tmp$ve, 0.25),
      ve_q75 = quantile(dat_tmp$ve, 0.75),
      ve_q975 = quantile(dat_tmp$ve, 0.975)
    )
    return(out)
    
  }
)
saveRDS(object = tb_n, file = here("outputs", "vmisclass_tb_n.rds"))
