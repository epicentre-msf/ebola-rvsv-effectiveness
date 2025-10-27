format_etc_mice <- function(etc) {
  
  out <- etc %>%
    mutate(
      evd_status = factor(evd_status, ordered = FALSE),
      sex = factor(sex, ordered = FALSE),
      contact_evd_case_yn = (contact_evd_case == "Yes")
    ) %>%
    mutate(
      time_onset = as.integer(date_onset - as.Date("2018-08-01"))
    ) %>%
    select(
      patient_id_unique,
      contact_evd_case_yn,
      delay_vacc_onset,
      evd_status, vacc,
      sex, age_years, adm2_name__res, adm3_name__res, time_onset
    )
  
  return(out)
  
}


mice_setup <- function(etc_format_mice, evd_status_val = NULL, contact_status_val = NULL) {
  
  # Make and customise predictor matrix
  custom_mat <- make.predictorMatrix(data = etc_format_mice)
  custom_mat[,] <- 0
  ## vaccination-onset delay
  if(is.null(evd_status_val) & is.null(contact_status_val)) {
    # delay ~ c(contact_evd_case, evd_status, sex, age_years, adm2_name__res)
    custom_mat[3, c(2, 4:8)] <- 1
  } else if(is.null(evd_status_val)) {
    # delay ~ c(evd_status, sex, age_years, adm2_name__res)
    custom_mat[3, c(4:8)] <- 1
  } else if(is.null(contact_status_val)) {
    # delay ~ c(contact_evd_case, sex, age_years, adm2_name__res)
    custom_mat[3, c(2, 5:8)] <- 1
  } else {
    # delay ~ c(sex, age_years, adm2_name__res)
    custom_mat[3, c(5:8)] <- 1
  }
  ## sex, age (~ c(vacc))
  custom_mat[6:7, 5] <- 1
  
  # Restrict imputed delays to vacc cases only
  custom_post <- make.post(etc_format_mice)
  custom_post["delay_vacc_onset"] <-
    "imp[[j]][data$vacc[!r[, j]] == FALSE, i] <- NA"
  
  # Customise methods
  custom_methods <- mice(
    data = etc_format_mice,
    predictorMatrix = custom_mat,
    m = 1, maxit = 0,
  )
  
  out <- list(
    custom_mat = custom_mat,
    custom_post = custom_post,
    custom_methods = custom_methods
  )
  
  return(out)
  
}


format_mice_output <- function(mice_output, etc_format) {
  
  # IDs with imputed vaccination-onset delays
  ids_imp <- etc_format %>%
    filter(vacc_status == "Vaccinated (delay unknown)") %>%
    pull(patient_id_unique)
  
  out <- complete(mice_output, action = "long") %>%
    tibble() %>%
    left_join(
      etc_format %>%
        select(-setdiff(intersect(colnames(etc_format), colnames(mice_output$data)), "patient_id_unique")),
      by = "patient_id_unique"
    ) %>%
    # Update vaccination history
    mutate(
      # vaccination-onset delay
      delay_vacc_onset_gp = case_when(
        delay_vacc_onset < 0 ~ "negative",
        delay_vacc_onset < 3 ~ "0 - 2 days",
        delay_vacc_onset < 10 ~ "3 - 9 days",
        delay_vacc_onset >= 10 ~ "10+ days",
        vacc ~ "delay unknown"
      ),
      # vaccination status
      vacc_status = case_when(
        vacc ~ paste0("Vaccinated (", delay_vacc_onset_gp, ")"),
        TRUE ~ "Unvaccinated"
      ),
      vacc_status = factor(
        vacc_status,
        c("Unvaccinated", "Vaccinated (0 - 2 days)", "Vaccinated (3 - 9 days)", "Vaccinated (10+ days)",
          "Vaccinated (delay unknown)", "Vaccinated (negative)"),
        ordered = TRUE),
      vacc_onset_delay_imp = case_when(
        patient_id_unique %in% ids_imp ~ TRUE,
        vacc ~ FALSE
      )
    ) %>%
    # Age group
    mutate(
      age_gp = case_when(
        !is.na(age_gp) ~ age_gp,
        age_years < 5 ~ "0-4",
        age_years < 15 ~ "5-14",
        age_years < 30 ~ "15-29",
        age_years < 60 ~ "30-59",
        age_years >= 60 ~ "60+"
      ),
      age_gp = factor(age_gp, c("0-4", "5-14", "15-29", "30-59", "60+"), ordered = TRUE)
    ) %>%
    select(-c(.id, time_onset))
  
  return(out)
  
}


mice_pipeline <- function(etc, m, evd_status_val = NULL, contact_status_val = NULL) {
  
  # Re-name ETC objects
  if(!is.null(evd_status_val) & !is.null(contact_status_val)) {
    etc_format <- etc$etc_format %>%
      mutate(contact_evd_case_yn = (contact_evd_case == "Yes")) %>%
      filter(
        evd_status == evd_status_val,
        contact_evd_case_yn == contact_status_val
      )
    etc_format_brms <- etc$etc_format_brms %>%
      mutate(contact_evd_case_yn = (contact_evd_case == "Yes")) %>%
      filter(
        evd_status == evd_status_val,
        contact_evd_case_yn == contact_status_val
      )
  } else if(!is.null(evd_status_val)) {
    etc_format <- etc$etc_format %>%
      filter(
        evd_status == evd_status_val
      )
    etc_format_brms <- etc$etc_format_brms %>%
      filter(
        evd_status == evd_status_val
      )
  } else if(!is.null(contact_status_val)) {
    etc_format <- etc$etc_format %>%
      mutate(contact_evd_case_yn = (contact_evd_case == "Yes")) %>%
      filter(
        contact_evd_case_yn == contact_status_val
      )
    etc_format_brms <- etc$etc_format_brms %>%
      mutate(contact_evd_case_yn = (contact_evd_case == "Yes")) %>%
      filter(
        contact_evd_case_yn == contact_status_val
      )
  } else {
    etc_format <- etc$etc_format
    etc_format_brms <- etc$etc_format_brms 
  }
  
  # Format brms -> mice
  etc_format_mice <- format_etc_mice(etc_format_brms)
  
  # Set up mice
  custom_mice <- mice_setup(
    etc_format_mice = etc_format_mice,
    evd_status_val = evd_status_val, contact_status_val = contact_status_val
  )
  
  # MICE!
  mice_out <- mice(
    data = etc_format_mice,
    predictorMatrix = custom_mice$custom_mat,
    m = m, maxit = 5,
    method = custom_mice$custom_methods$method,
    post = custom_mice$custom_post,
    remove.constant = FALSE,
    print = FALSE
  )
  
  # Format output (mice)
  mice_out_format <- format_mice_output(
    mice_output = mice_out,
    etc_format = etc_format
    )
  
  # Format output (brms)
  mice_out_format_brms <- purrr::map_df(
    .x = unique(mice_out_format$.imp),
    .f = ~ {
      
      int <- mice_out_format %>%
        filter(.imp == .x) %>%
        select(-.imp)
      out_int <- format_etc_brms(int) %>%
        mutate(.imp = .x) %>%
        select(.imp, everything())
      
      return(out_int)
      
    }
  )
  
  out <- list(
    mice_out = mice_out,
    mice_out_format = mice_out_format_brms
  )
  
  return(out)
  
}
