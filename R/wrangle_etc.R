source(here("R", "load_data.R"))

# Data wrangling ETC line list
clean_etc_raw <- function(etc_raw) {
  
  # Load MLL
  mll <- load_mll()
  mll_v <- mll$mll_vaccination
  mll_e <- mll$mll_exp
  
  # Load BDC
  bdc <- load_bdc()
  bdc_v <- bdc$bdc_vaccination
  
  # Clean ETC
  etc_clean <- etc_raw %>%
    # rename variables
    rename(
      evd_status = EVD_status,
      etc = ETC,
      age_years = age_in_years,
      date_onset = date_symptom_onset,
      date_vacc_etc = date_vaccination_rvsv,
      contact_evd_case = contact_EVD_case,
      contact_evd_physical = contact_EVD_physical,
      contact_evd_body_fluids = contact_EVD_body_fluids,
      contact_evd_house = contact_EVD_house,
      contact_evd_objects = contact_EVD_objects,
      contact_hf = contact_HF,
      ctr_known = contact_tracing_known_yn,
      ctr_followed = contact_tracing_followed_yn,
      outcome = type_of_exit
    ) %>%
    # recode missing data
    mutate(
      sex = na_if(sex, "Unknown"),
      vaccination_rvsv_yn = na_if(vaccination_rvsv_yn, "Unknown"),
      contact_evd_case = na_if(contact_evd_case, "Unknown"),
      contact_funeral = na_if(contact_funeral, "Unknown"),
      contact_travel = na_if(contact_travel, "Unknown"),
      ctr_known = na_if(ctr_known, "Unknown"),
      ctr_followed = na_if(ctr_followed, "Unknown"),
      outcome = factor(outcome, c("Cured", "Died")),
      across(
        .cols = c(
          fever,
          headache, asthenia_weakness, loss_of_appetite, swallowing_problem, abdominal_pain, breathlessness, nausea, hichups, diarrhoea, joint_pain, bone_muscle_pain,
          contains("bleeding")
        ),
        .fns = ~ na_if(.x, "Unknown")
      ),
      dead_upon_arrival = na_if(dead_upon_arrival, "Unknown")
    ) %>%
    # recode EVD status
    mutate(
      evd_status = case_when(
        evd_status == "Confirmed" ~ "EVD-pos",
        evd_status == "Not a case" ~ "EVD-neg"
      ),
      evd_status = factor(evd_status, c("EVD-pos", "EVD-neg"), ordered = TRUE)
    ) %>%
    # define age groups 
    mutate(
      age_gp = case_when(
        age_years < 5 ~ "0-4",
        age_years < 15 ~ "5-14",
        age_years < 30 ~ "15-29",
        age_years < 60 ~ "30-59",
        age_years >= 60 ~ "60+"
      ),
      age_gp = factor(age_gp, c("0-4", "5-14", "15-29", "30-59", "60+"), ordered = TRUE)
    ) %>%
    # define "is pregnant or breastfeeding" (Yes, No, NA)
    mutate(
      is_pgbf = case_when(
        pregnant == "Yes" | breastfeeding == "Yes" ~ "Yes",
        pregnant == "No" & breastfeeding == "No" ~ "No"
      ),
      is_pgbf = factor(is_pgbf, c("Yes", "No", NA), ordered = TRUE)
    ) %>%
    # define "is HCW" (Yes, No, NA)
    mutate(
      is_hcw = case_when(
        is_hcw ~ "Yes",
        !is_hcw ~ "No"
      ),
      is_hcw = factor(is_hcw, c("Yes", "No", NA), ordered = TRUE)
    ) %>%
    # define clean vaccination status
    mutate(
      vacc_etc = case_when(
        grepl("Yes", vaccination_rvsv_yn) ~ TRUE,
        grepl("No", vaccination_rvsv_yn) ~ FALSE
      )
    ) %>%
    # improve vaccination status (w/ MLL and BDC)
    left_join(mll_v, by = c("vhf_code", "mll_id")) %>%
    left_join(bdc_v, by = "patient_site_id_std") %>%
    mutate(
      vacc = case_when(
        is.na(vacc_etc) & (vacc_mll == vacc_bdc) ~ vacc_mll,
        TRUE ~ vacc_etc
      ),
      date_vacc = case_when(
        vacc & is.na(date_vacc_etc) ~ coalesce(date_vacc_mll, date_vacc_bdc), # CHECK: vacc_etc = FALSE, vacc_mll/bdc = TRUE (w/ date)
        TRUE ~ date_vacc_etc
      )
    ) %>%
    # define vaccination-onset delay and group
    mutate(
      delay_vacc_onset = as.integer(date_onset - date_vacc),
      delay_vacc_onset_gp = case_when(
        delay_vacc_onset < 0 ~ "negative",
        delay_vacc_onset < 3 ~ "0 - 2 days",
        delay_vacc_onset < 10 ~ "3 - 9 days",
        delay_vacc_onset >= 10 ~ "10+ days"
      )
    ) %>%
    # define "vaccination status" (combine vacc and delay_vacc_onset_gp)
    mutate(
      vacc_status = case_when(
        !vacc ~ "Unvaccinated",
        vacc & !is.na(delay_vacc_onset_gp) ~ paste0("Vaccinated (", delay_vacc_onset_gp, ")"),
        vacc & is.na(delay_vacc_onset_gp) ~ "Vaccinated (delay unknown)"
      ),
      vacc_status = factor(vacc_status,
                           c("Unvaccinated",
                             "Vaccinated (0 - 2 days)", "Vaccinated (3 - 9 days)", "Vaccinated (10+ days)",
                             "Vaccinated (delay unknown)", "Vaccinated (negative)"),
                           ordered = TRUE)
    ) %>%
    # join adm3 cases
    left_join(
      mll_e,
      by = c("adm2_name__res" = "adm2_name",
             "adm3_name__res" = "adm3_name",
             "date_onset" = "date")
    ) %>%
    # contact EVD case (Yes, No, Unknown)
    mutate(
      contact_evd_case = ifelse(is.na(contact_evd_case), "Unknown", contact_evd_case),
      contact_evd_case = factor(contact_evd_case, c("Yes", "No", "Unknown"), ordered = TRUE)
    ) %>%
    # define "any contact" (Yes, No, Unknown)
    mutate(
      contact_any = case_when(
        contact_evd_case == "Yes" | contact_tradi == "Yes" | contact_hf == "Yes" | contact_funeral == "Yes" ~ "Yes",
        !is.na(contact_evd_case) & !is.na(contact_tradi) & !is.na(contact_hf) & !is.na(contact_funeral) ~ "No",
        TRUE ~ "Unknown"
      ),
      contact_any = factor(contact_any, c("Yes", "No", "Unknown"), ordered = TRUE)
    )  %>%
    # any adm3 or adm2 cases
    mutate(
      any_adm3 = case_when(
        n_adm3_exp > 0 ~ "Yes",
        TRUE ~ "No"
      ),
      any_adm3 = factor(any_adm3, c("Yes", "No", NA), ordered = TRUE),
      any_adm2 = case_when(
        n_adm2_exp > 0 ~ "Yes",
        n_adm2_exp == 0 | !is.na(adm2_name__res) ~ "No"
      ),
      any_adm2 = factor(any_adm2, c("Yes", "No", NA), ordered = TRUE)
    ) %>%
    mutate(
      # recode symptoms
      across(
        .cols = c(
          fever,
          headache, asthenia_weakness, loss_of_appetite, swallowing_problem, chest_pain, abdominal_pain, breathlessness, nausea, hichups, diarrhoea, joint_pain, bone_muscle_pain, confused_disoriented, coma,
          contains("bleeding")
        ),
        .fns = ~ .x == "Yes"
      ),
      # count non-bleeding symptoms
      n_symptoms = rowSums(across(c(headache, asthenia_weakness, loss_of_appetite, swallowing_problem, chest_pain, abdominal_pain, breathlessness, nausea, hichups, diarrhoea, joint_pain, bone_muscle_pain, confused_disoriented, coma)), na.rm = TRUE),
      # any bleeding symptoms
      bleeding_any = if_any(.cols = contains("bleeding"), .fns = ~ .x),
      # any advanced/severe symptoms
      advanced_any = if_any(.cols = c(asthenia_weakness, nausea, diarrhoea, chest_pain, abdominal_pain, breathlessness, hichups, confused_disoriented, coma, bleeding_any), .fns = ~ .x)
    ) %>%
    # suspected case
    mutate(
      suspected = case_when(
        fever & contact_evd_case == "Yes" ~ TRUE,
        fever & n_symptoms > 0 & (any_adm2 == "Yes") ~ TRUE,
        fever & n_symptoms >= 3 ~ TRUE,
        contact_evd_case == "Yes" & n_symptoms >= 3 ~ TRUE,
        bleeding_any ~ TRUE,
        dead_upon_arrival == "Yes" ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  return(etc_clean)
  
}

clean_etc_repeats <- function(etc_clean) {
  
  int <- etc_clean %>%
    # keep confirmed and non-case only
    filter(!is.na(evd_status)) %>%
    # vaccination status and date mostly not filled at discharge, may be missing
    #   so fill it back with the information from first admission
    arrange(patient_id_unique, confirmed_order) %>%
    group_by(patient_id_unique) %>%
    fill(vacc_etc, .direction = "downup") %>%
    fill(date_vacc_etc, .direction = "downup") %>%
    ungroup()
  
  # Check for PIDs with conflicting vaccination status (T/F - no NA unless all NA)
  conflict_pids <- int %>%
    count(patient_id_unique, vacc_etc) %>%
    count(patient_id_unique) %>%
    filter(n > 1) %>%
    pull(patient_id_unique)
  
  # No conflicts
  out <- int %>%
    filter(!patient_id_unique %in% conflict_pids) %>%
    # remove entries prior to being positive (0) and false EVD+ (-1); keep latest record
    filter(is.na(confirmed_order) | !confirmed_order %in% c(0, -1)) %>%
    filter(latest_record)
  
  # With conflicts; return as separate df
  out_conflict <- int %>%
    filter(patient_id_unique %in% conflict_pids)
  
  return(
    list(
      out = out,
      out_conflict = out_conflict
    )
  )
  
}


# Make ETC clean data (one-step)
assemble_etc <- function(etc_raw, add_vars) {
  
  # Clean raw ETC data
  int_clean <- clean_etc_raw(etc_raw)
  
  # Define subset of variables
  main_vars <- c(
    "patient_id_unique",
    "evd_status",
    "sex", "age_years", "age_gp", "adm1_name__res", "adm2_name__res", "adm3_name__res",
    "is_pgbf", "is_hcw",
    "date_onset",
    "etc", "date_admission_eff", "outcome",
    "vacc_status", "vacc", "date_vacc", "delay_vacc_onset", "delay_vacc_onset_gp",
    # "n_adm3_exp", "n_adm2_exp",
    "contact_evd_case", "contact_any", "any_adm3", "any_adm2", "suspected", "fever", "bleeding_any", "advanced_any", "n_symptoms", "dead_upon_arrival",
    "ctr_known", "ctr_followed",
    "vhf_code", "mll_id"
  )
  # Add additional variables if specified
  if(missing(add_vars)) {
    select_vars <- main_vars
  } else {
    select_vars <- intersect(
      c(main_vars, add_vars),
      names(int_clean)
    )
  }
  
  # Clean for repeat PIDs
  out <- clean_etc_repeats(int_clean)
  
  out$out <- out$out %>%
    select(all_of(select_vars))
  
  return(out)
  
}


# Summarise eligibility 
summarise_eligibility <- function(all_eligible) {
  
  tb_all <- tibble(
    stage = 0,
    n = nrow(all_eligible)
  )
  
  tb_vaccination <- all_eligible %>%
    filter(ex_vaccn == "Y") %>%
    count(ex_detail = ex_vaccn_detail) %>%
    mutate(stage = 1,
           criteria = "Vaccination eligibility") %>%
    select(stage, criteria, ex_detail, n_ex = n) %>%
    bind_rows(
      tibble(
        stage = 1,
        criteria = "Vaccination eligibility",
        n = nrow(all_eligible %>% filter(ex_vaccn == "N"))
      )
    )
  
  tb_study <- all_eligible %>%
    filter(ex_vaccn == "N",
           ex_study == "Y") %>%
    count(ex_detail = ex_study_detail) %>%
    mutate(stage = 2,
           criteria = "Study inclusion") %>%
    select(stage, criteria, ex_detail, n_ex = n) %>%
    bind_rows(
      tibble(
        stage = 2,
        criteria = "Study inclusion",
        n = nrow(all_eligible %>% filter(ex_vaccn == "N", ex_study == "N"))
      )
    )
  
  tb_data <- all_eligible %>%
    filter(ex_vaccn == "N",
           ex_study == "N",
           ex_data == "Y") %>%
    count(ex_detail = ex_data_detail) %>%
    mutate(stage = 3,
           criteria = "Data availability and quality") %>%
    select(stage, criteria, ex_detail, n_ex = n) %>%
    bind_rows(
      tibble(
        stage = 3,
        criteria = "Data availability and quality",
        n = nrow(all_eligible %>% filter(ex_vaccn == "N", ex_study == "N", ex_data == "N"))
      )
    )
  
  tb_eligible <- all_eligible %>%
    filter(ex_vaccn == "N",
           ex_study == "N",
           ex_data == "N")
  tb_eligible <- tibble(
    stage = 99,
    n = nrow(tb_eligible)
  )
  
  out <- bind_rows(
    tb_all,
    tb_vaccination,
    tb_study,
    tb_data,
    tb_eligible
  ) %>%
    select(stage, criteria, n, ex_detail, n_ex)
  
  return(out)
  
}

# Eligibility criteria
filter_eligible <- function(etc_clean) {
  
  int_w_ex <- etc_clean %>%
    # Eligible for vaccination (13 June + 1w)
    mutate(
      ex_pgbf = case_when(
        is_pgbf == "Yes" & date_onset < as.Date("2019-06-20") ~ "Y",
        TRUE ~ "N"
      ),
      ex_under1 = case_when(
        age_years < 1 & date_onset < as.Date("2019-06-20") ~ "Y",
        TRUE ~ "N"
      ),
      ex_under6m = case_when(
        age_years < 0.5 ~ "Y",
        TRUE ~ "N"
      ),
      ex_hcw = case_when(
        is_hcw == "Yes" ~ "Y",
        TRUE ~ "N"
      ),
      ex_vaccn = case_when(
        ex_pgbf == "Y" | ex_under1 == "Y" | ex_under6m == "Y" | ex_hcw == "Y" ~ "Y",
        TRUE ~ "N"
      ),
      ex_vaccn_detail = case_when(
        ex_pgbf == "Y" ~ "Pregnant or breastfeeding (strategy 1)",
        ex_under1 == "Y" ~ "Under 1 year (strategy 1)",
        ex_under6m == "Y" ~ "Under 6 months",
        ex_hcw == "Y" ~ "Health care worker",
        TRUE ~ "N"
      )
    ) %>%
    # Availability of key data
    mutate(
      ex_data = case_when(
        is.na(vacc) ~ "Y",
        is.na(date_onset) ~ "Y",
        is.na(adm2_name__res) ~ "Y",
        TRUE ~ "N"
      ),
      ex_data_detail = case_when(
        is.na(vacc) ~ "Vaccination status unknown",
        is.na(date_onset) ~ "Missing onset date",
        is.na(adm2_name__res) ~ "Missing resident health zone",
        TRUE ~ "N"
      )
    ) %>%
    # Time
    mutate(
      ex_etctime = case_when(
        !between(date_onset, as.Date("2018-08-18"), as.Date("2019-11-30")) ~ "Setting: outside defined time window",
        TRUE ~ "N"
      ),
      ex_study = case_when(
        ex_etctime != "N" ~ "Y",
        TRUE ~ "N"
      ),
      ex_study_detail = case_when(
        ex_etctime != "N" ~ ex_etctime,
        TRUE ~ "N"
      )
    ) %>%
    #
    mutate(
      ex_any = case_when(
        ex_vaccn == "Y" | ex_data == "Y" | ex_study == "Y" ~ "Y",
        ex_vaccn == "N" & ex_data == "N" & ex_study == "N" ~ "N"
      )
    )
  
  # Filter all eligible participants
  etc_eligible <- int_w_ex %>%
    filter(ex_any == "N") %>%
    select(-contains("ex_"))
  
  # Select exclusion criteria
  etc_criteria <- int_w_ex %>%
    select(patient_id_unique,
           ex_any,
           ex_vaccn, ex_vaccn_detail,
           ex_data, ex_data_detail,
           ex_study, ex_study_detail)
  
  # Summarise exclusion criteria
  etc_criteria_sum <- summarise_eligibility(etc_criteria)
  
  out <- list(
    etc_eligible = etc_eligible,
    etc_criteria = etc_criteria,
    etc_criteria_sum = etc_criteria_sum
  )
  
}


# Format ETC eligible data for visualisation
format_etc_pres <- function(etc) {
  
  main_adm2 <- c("Beni", "Katwa", "Mabalako", "Mandima", "Butembo", "Kalunguta", "Mambasa", "Vuhovi", "Komanda")
  
  out <- etc %>%
    mutate(
      # vaccination phase (strategy 1 or 2)
      vacc_phase = case_when(
        !vacc ~ "Unvaccinated",
        vacc & date_vacc <= as.Date("2019-06-13") ~ "Vaccinated (strategy 1)",
        vacc & date_vacc >  as.Date("2019-06-13") ~ "Vaccinated (strategy 2)",
        vacc & is.na(date_vacc) ~ "Vaccinated (strategy unknown)"
      ),
      vacc_phase = factor(vacc_phase,
                          c("Unvaccinated",
                            "Vaccinated (strategy 1)", "Vaccinated (strategy 2)",
                            "Vaccinated (strategy unknown)"),
                          ordered = TRUE)
    ) %>%
    # grouped adm2
    mutate(
      adm2_name__res_gp = case_when(
        adm2_name__res %in% main_adm2 ~ adm2_name__res,
        !adm2_name__res %in% main_adm2 ~ "Other"
      ),
      adm2_name__res_gp = factor(adm2_name__res_gp, c(sort(main_adm2), "Other"), ordered = TRUE)
    ) %>%
    # month symptom onset
    mutate(
      date_onset_month = lubridate::floor_date(date_onset, unit = "month"),
      month_onset = format.Date(date_onset_month, format = "%Y-%m")
    )
  
  return(out)
  
}


# Format ETC eligible data for brms
format_etc_brms <- function(etc) {
  
  out <- etc %>%
    mutate(
      # EVD status
      x_evd_status = ifelse(evd_status == "EVD-pos", 1, 0),
      # Vaccination history
      x_vacc_status = case_when(
        vacc_status == "Unvaccinated" ~ "unvaccinated",
        vacc_status == "Vaccinated (negative)" ~ "vaccinated_neg",
        vacc_status == "Vaccinated (0 - 2 days)" ~ "vaccinated_0_2d",
        vacc_status == "Vaccinated (3 - 9 days)" ~ "vaccinated_3_9d",
        vacc_status == "Vaccinated (10+ days)" ~ "vaccinated_10d"
      ),
      x_vacc_status = factor(x_vacc_status, c("unvaccinated", "vaccinated_neg", "vaccinated_0_2d", "vaccinated_3_9d", "vaccinated_10d")),
      # Sex
      x_sex = factor(sex, ordered = FALSE),
      # Age group
      x_age_gp = stringr::str_replace(age_gp, "-", "_"),
      x_age_gp = stringr::str_replace(x_age_gp, "\\+", "_"),
      x_age_gp = paste0("y", x_age_gp),
      x_age_gp = factor(x_age_gp, c("y0_4", "y5_14", "y15_29", "y30_59", "y60_")),
      # Time-place strata
      x_strata = paste0(adm2_name__res, "_", format.Date(date_onset_month, format = "%Y%m"))
    )
  
  return(out)
  
}


# 1-step ETC
etc_1step <- function() {
  
  # Raw ETC
  etc_raw <- readr::read_csv(
    file = paths$file_data_etc,
    progress = FALSE
  )
  # Clean ETC
  etc_all <- assemble_etc(etc_raw)$out %>%
    filter(is.na(date_onset) | date_onset <= as.Date("2019-11-30"))
  
  # Filter for eligible population
  etc_all_eligible <- filter_eligible(etc_all)
  
  # Format eligible for visualisation
  etc_format <- format_etc_pres(etc_all_eligible$etc_eligible)
  # Format eligible for analysis
  etc_format_brms <- format_etc_brms(etc_format)
  
  out <- list(
    etc_format = etc_format,
    etc_format_brms = etc_format_brms
  )
  
}
