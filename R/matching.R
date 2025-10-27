# Get number of cases/controls (matched and unmatched) per strata for a given exposure definition
get_matching_vals <- function(etc_elig_1, alt = NULL) {
  
  # Main matching criteria (sex, age group, adm2 residence, month symptom onset)
  if(is.null(alt)) {
    
    out <- etc_elig_1 %>%
      mutate(
        evd_status = ifelse(evd_status == "EVD-pos", "case_av", "control_av"),
      ) %>%
      count(adm2_name__res, month_onset, sex, age_gp, evd_status) %>%
      complete(
        nesting(adm2_name__res, month_onset, sex, age_gp), evd_status,
        fill = list(n = 0)
      ) %>%
      pivot_wider(names_from = evd_status, values_from = n) %>%
      mutate(
        case = pmin(case_av, floor(control_av / 1)),
        control = case*1,
        case_unmatch = case_av - case,
        control_unmatch = control_av - control,
      ) %>%
      select(-contains("_av")) %>%
      pivot_longer(cols = c(contains("case"), contains("control"))) %>%
      mutate(name = factor(name, c("case", "case_unmatch", "control", "control_unmatch"))) %>%
      drop_na() %>%
      complete(adm2_name__res, month_onset, sex, age_gp, name, fill = list(value = 0))
    
  } else if(alt == 1) {
    
    # Use adm2_name__res_alt1
    out <- etc_elig_1 %>%
      mutate(
        evd_status = ifelse(evd_status == "EVD-pos", "case_av", "control_av"),
      ) %>%
      count(adm2_name__res_alt1, month_onset, sex, age_gp, evd_status) %>%
      complete(
        nesting(adm2_name__res_alt1, month_onset, sex, age_gp), evd_status,
        fill = list(n = 0)
      ) %>%
      pivot_wider(names_from = evd_status, values_from = n) %>%
      mutate(
        case = pmin(case_av, floor(control_av / 1)),
        control = case*1,
        case_unmatch = case_av - case,
        control_unmatch = control_av - control,
      ) %>%
      select(-contains("_av")) %>%
      pivot_longer(cols = c(contains("case"), contains("control"))) %>%
      mutate(name = factor(name, c("case", "case_unmatch", "control", "control_unmatch"))) %>%
      drop_na() %>%
      complete(adm2_name__res_alt1, month_onset, sex, age_gp, name, fill = list(value = 0))
    
  } else if (alt == "sex_age"){
    
    out <- etc_elig_1 %>%
      mutate(
        evd_status = ifelse(evd_status == "EVD-pos", "case_av", "control_av"),
      ) %>%
      count(sex, age_gp, evd_status) %>%
      complete(
        nesting(sex, age_gp), evd_status,
        fill = list(n = 0)
      ) %>%
      pivot_wider(names_from = evd_status, values_from = n) %>%
      mutate(
        case = pmin(case_av, floor(control_av / 1)),
        control = case*1,
        case_unmatch = case_av - case,
        control_unmatch = control_av - control,
      ) %>%
      select(-contains("_av")) %>%
      pivot_longer(cols = c(contains("case"), contains("control"))) %>%
      mutate(name = factor(name, c("case", "case_unmatch", "control", "control_unmatch"))) %>%
      drop_na() %>%
      complete(sex, age_gp, name, fill = list(value = 0))
    
  }
  
  return(out)
  
}


# Make ONE sample of cases/controls (from dat) across all strata (defined in mat_tb)
sample_matching_int <- function(dat, mat_tb) {
  
  # Study population
  match_pool <- dat
  
  # Sample !
  out_sample <- purrr::map_df(
    .x = 1:nrow(mat_tb),
    .f = ~ {
      
      match_pool_int <- match_pool %>%
        filter(
          sex == mat_tb$sex[.x],
          age_gp == mat_tb$age_gp[.x],
          adm2_name__res == mat_tb$adm2_name__res[.x],
          month_onset == mat_tb$month_onset[.x]
        )
      
      match_case <- match_pool_int %>%
        filter(evd_status == "EVD-pos") %>%
        sample_n(mat_tb$case[.x])
      
      match_control <- match_pool_int %>%
        filter(evd_status == "EVD-neg") %>%
        sample_n(mat_tb$control[.x])
      
      out <- bind_rows(
        match_case,
        match_control
        )
      
      return(out)
      
    })
  
  return(out_sample)
  
}


# Make n samples of cases/controls (from etc_elig_1) across all strata (defined in mat_tb)
sample_matching <- function(etc_elig_1, n = 1) {
  
  # Get number of cases and controls to sample
  mat_tb <- get_matching_vals(etc_elig_1 = etc_elig_1) %>%
    filter(name %in% c("case", "control")) %>%
    pivot_wider() %>%
    filter(case > 0)
  
  # Make n matching samples
  out_int <- purrr::map_df(
    .x = 1:n,
    .f = ~ sample_matching_int(
      dat = etc_elig_1, mat_tb = mat_tb
      ) %>%
      mutate(.iter = .x)
  )
  
  out_list <- split(x = data.table::as.data.table(out_int), by = ".iter") 
  
  return(out <- list(
    sample_df = out_int,
    sample_list = out_list
  ))
  
}


# Make n samples of cases/controls (from each imputation) across all strata (defined in mat_tb)
sample_matching_imp <- function(etc_elig_m, n = 1) {
  
  m_vec <- setdiff(unique(etc_elig_m$.imp), NA)
  
  out_sample_df <- purrr::map_df(
    .x = m_vec,
    .f = ~ {
      
      message(paste0("Matching imputation #", .x, ": "))
      
      etc_elig_imp_1 <- etc_elig_m %>%
        filter(.imp == .x | is.na(.imp)) %>%
        mutate(.imp = .x)
      
      out <- sample_matching(
        etc_elig_1 = etc_elig_imp_1,
        n = n
        )
      
      return(out$sample_df)
      
    }
  ) %>%
    mutate(.iter = (.imp - 1)*n + .iter)
  
  out_sample_list <- split(x = data.table::as.data.table(out_sample_df), by = ".iter") 
  
  return(out <- list(
    sample_df = out_sample_df,
    sample_list = out_sample_list
  ))
  
}




# Matching, but only on sex and age ---------------------------------------

# Make ONE sample of cases/controls (from dat) across all strata (defined in mat_tb)
sample_matching_int__SA <- function(dat, mat_tb) {
  
  # Study population
  match_pool <- dat
  
  # Sample !
  out_sample <- purrr::map_df(
    .x = 1:nrow(mat_tb),
    .f = ~ {
      
      match_pool_int <- match_pool %>%
        filter(
          sex == mat_tb$sex[.x],
          age_gp == mat_tb$age_gp[.x]
        )
      
      match_case <- match_pool_int %>%
        filter(evd_status == "EVD-pos") %>%
        sample_n(mat_tb$case[.x])
      
      match_control <- match_pool_int %>%
        filter(evd_status == "EVD-neg") %>%
        sample_n(mat_tb$control[.x])
      
      out <- bind_rows(
        match_case,
        match_control
      )
      
      return(out)
      
    })
  
  return(out_sample)
  
}


# Make n samples of cases/controls (from etc_elig_1) across all strata (defined in mat_tb)
sample_matching__SA <- function(etc_elig_1, n = 1) {
  
  # Get number of cases and controls to sample
  mat_tb <- get_matching_vals(etc_elig_1 = etc_elig_1, alt = "sex_age") %>%
    filter(name %in% c("case", "control")) %>%
    pivot_wider() %>%
    filter(case > 0)
  
  # Make n matching samples
  out_int <- purrr::map_df(
    .x = 1:n,
    .f = ~ sample_matching_int__SA(
      dat = etc_elig_1, mat_tb = mat_tb
    ) %>%
      mutate(.iter = .x)
  )
  
  out_list <- split(x = data.table::as.data.table(out_int), by = ".iter") 
  
  return(out <- list(
    sample_df = out_int,
    sample_list = out_list
  ))
  
}


# Make n samples of cases/controls (from each imputation) across all strata (defined in mat_tb)
sample_matching_imp__SA <- function(etc_elig_m, n = 1) {
  
  m_vec <- setdiff(unique(etc_elig_m$.imp), NA)
  
  out_sample_df <- purrr::map_df(
    .x = m_vec,
    .f = ~ {
      
      message(paste0("Matching imputation #", .x, ": "))
      
      etc_elig_imp_1 <- etc_elig_m %>%
        filter(.imp == .x | is.na(.imp)) %>%
        mutate(.imp = .x)
      
      out <- sample_matching__SA(
        etc_elig_1 = etc_elig_imp_1,
        n = n
      )
      
      return(out$sample_df)
      
    }
  ) %>%
    mutate(.iter = (.imp - 1)*n + .iter)
  
  out_sample_list <- split(x = data.table::as.data.table(out_sample_df), by = ".iter") 
  
  return(out <- list(
    sample_df = out_sample_df,
    sample_list = out_sample_list
  ))
  
}


# Additional --------------------------------------------------------------
# Used only for choosing case-control ratio, r

get_match_counts_r <- function(df, r) {
  
  tb_int_r <- df %>%
    mutate(
      case = pmin(case_av, floor(control_av / r)),
      control = case*r,
      case_drop = case_av - case,
      control_drop = control_av - control,
    ) %>%
    select(-contains("_av"))
  
  out <- tibble(
    r = r,
    total = sum(tb_int_r$case) + sum(tb_int_r$control),
    case = sum(tb_int_r$case),
    control = sum(tb_int_r$control),
    case_unmatch = sum(tb_int_r$case_drop),
    control_unmatch = sum(tb_int_r$control_drop)
  )
  
  return(out)
  
}

get_match_counts <- function(df, r = 1:4, date_threshold, exposure_def = 1) {
  
  if(exposure_def == 1) {
    df_int <- df %>%
      filter(date_onset <= as.Date(date_threshold)) %>%
      filter(contact_evd_case == "Yes")
    exposure_def_nm <- "A1. Contact EVD case"
  } else if(exposure_def == 2) {
    df_int <- df %>%
      filter(date_onset <= as.Date(date_threshold)) %>%
      filter(contact_any == "Yes")
    exposure_def_nm <- "A2. Any contact"
  } else if(exposure_def == 3) {
    df_int <- df %>%
      filter(date_onset <= as.Date(date_threshold)) %>%
      filter(any_adm3 == "Yes")
    exposure_def_nm <- "A3. Any health area cases"
  } else if(exposure_def == 4) {
    df_int <- df %>%
      filter(date_onset <= as.Date(date_threshold)) %>%
      filter(any_adm2 == "Yes")
    exposure_def_nm <- "A4. Any health zone cases"
  }
  
  tb_int <- df_int %>%
    mutate(
      evd_status = ifelse(evd_status == "EVD-pos", "case_av", "control_av"),
      month_onset = lubridate::floor_date(date_onset, unit = "month"),
      month_onset = format.Date(month_onset, format = "%Y_%m"),
      strata = paste0(adm2_name__res, "_", month_onset)
    ) %>%
    count(adm2 = adm2_name__res, month = month_onset, sex, age_gp, evd_status) %>%
    complete(
      nesting(adm2, month, sex, age_gp), evd_status,
      fill = list(n = 0)
    ) %>%
    pivot_wider(names_from = evd_status, values_from = n)
  
  out <- purrr::map_df(.x = r,
                       .f = ~ get_match_counts_r(df = tb_int, r = .x)) %>%
    mutate(exposure_def = exposure_def_nm)
  
  return(out)
  
}
