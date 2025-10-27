get_posteriors <- function(brms_model) {
  
  out <- purrr::map_df(
    .x = seq_along(brms_model),
    .f = ~ {
      out_int <- as_draws_df(brms_model[[.x]]) %>%
        tibble() %>%
        mutate(
          iter = as.character(.x),
          ve = 100*(1 - exp(b_x_vacc_statusvaccinated_10d))
        ) %>%
        select(iter, everything())
      
      return(out_int)
    }
  )
  
  return(out)
  
}

summarise_ve <- function(brms_samples, ...) {
  
  out <- brms_samples %>%
    group_by(...) %>%
    summarise(
      ve_50 = quantile(ve, 0.5),
      ve_025 = quantile(ve, 0.025),
      ve_25 = quantile(ve, 0.25),
      ve_75 = quantile(ve, 0.75),
      ve_975 = quantile(ve, 0.975),
      .groups = "drop"
    ) %>%
    mutate(across(.cols = where(is.numeric), .f = ~ round(.x)))
  
  return(out)
  
}
