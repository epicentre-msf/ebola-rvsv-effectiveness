
switch_vaccination_int <- function(df, n, p, gp_evd, gp_vacc) {
  
  # Probability of being vaccinated 10+ days before onset, given vaccinated with known delay
  # p_vacc_10d <- 39/(39 + 135 + 69)
  p_vacc_10d <- 1
  
  if(!missing(gp_evd) & !missing(gp_vacc)) {
    df_swit_old <- df %>%
      dplyr::filter(
        evd_status == gp_evd,
        vacc_status == gp_vacc
      ) 
  } else {
    df_swit_old <- df
  }
  
  if(!missing(n) & !missing(p)) {
    stop("Both n and p are specified. Define exactly one of n and p.")
  }
  
  if(!missing(p)) {
    df_swit_old <- df_swit_old %>%
      dplyr::slice_sample(prop = p)
  } else if(!missing(n)) {
    df_swit_old <- df_swit_old %>%
      dplyr::slice_sample(n = n)
  } else {
    stop("Neither n nor p are specified Define exactly one of n and p.")
  }
  
  df_keep <- df %>%
    anti_join(df_swit_old, by = "patient_id_unique")
  
  df_swit_new <- df_swit_old %>%
    mutate(p = runif(nrow(df_swit_old), 0, 1)) %>%
    mutate(
      vacc_status = case_when(
        vacc_status == "Vaccinated (10+ days)" ~ "Unvaccinated",
        vacc_status == "Unvaccinated" & (p <= p_vacc_10d) ~ "Vaccinated (10+ days)",
        vacc_status == "Unvaccinated" & (p >  p_vacc_10d) ~ "Vaccinated (delay unknown)"
      ),
      x_vacc_status = case_when(
        vacc_status == "Unvaccinated" ~ "unvaccinated",
        vacc_status == "Vaccinated (10+ days)" ~ "vaccinated_10d",
      )
    ) %>%
    select(-p) %>%
    filter(vacc_status != "Vaccinated (delay unknown)")
  
  out <- bind_rows(
    df_keep,
    df_swit_new
  )
  
  return(out)
  
}

switch_vaccination_n <- function(obj, n, gp_evd, gp_vacc) {
  
  out_list <- purrr::map(
    .x = 1:length(obj$sample_list),
    .f = ~ {
      
      out <- switch_vaccination_int(
        df = obj$sample_list[[.x]],
        n = n,
        gp_evd = gp_evd, gp_vacc = gp_vacc
      )
      
    }
  )
  out_df <- tibble(bind_rows(out_list))
  
  out <- list(
    sample_df = out_df,
    sample_list = out_list
  )
  return(out)
  
}

switch_vaccination_p <- function(obj, p, gp_evd, gp_vacc) {
  
  out_list <- purrr::map(
    .x = 1:length(obj$sample_list),
    .f = ~ {
      
      out <- switch_vaccination_int(
        df = obj$sample_list[[.x]],
        p = p,
        gp_evd = gp_evd, gp_vacc = gp_vacc
      )
      
    }
  )
  out_df <- tibble(bind_rows(out_list))
  
  out <- list(
    sample_df = out_df,
    sample_list = out_list
  )
  return(out)
  
}
