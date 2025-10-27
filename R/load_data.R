# Load BDC data
load_bdc <- function() {
  
  attach(paths$file_data_clinical)
  
  bdc <- df_patients %>% 
    # subset 10th outbreak  
    filter(outbreak == 10) %>%
    # recode missing data
    mutate(
      vacc = na_if(vacc, "NS")
    ) %>%
    # clean vaccination status
    mutate(
      vacc_bdc = case_when(
        vacc == "oui" ~ TRUE,
        vacc == "non" ~ FALSE
      )
    ) %>%
    rename(date_vacc_bdc = vacc1_dt)
  
  # Vaccination status and date
  bdc_vaccination <- bdc %>%
    select(patient_site_id_std, vacc_bdc, date_vacc_bdc)
  
  out <- list(
    bdc = bdc,
    bdc_vaccination = bdc_vaccination
  )
  
  return(out)
  
}

# Load MLL data
load_mll <- function() {
  
  mll_raw <- readRDS(paths$file_data_mll)
  
  mll <- mll_raw %>% 
    # recode missing data
    mutate(
      vacc = na_if(vacc, "Unknown"),
    ) %>%
    # clean vaccination status
    mutate(
      vacc_mll = case_when(
        vacc == "Yes" ~ TRUE,
        vacc == "No" ~ FALSE
      )
    ) %>%
    # clean adm2 and adm3
    mutate(
      adm2_name = stringr::str_to_title(adm2_name),
      adm3_name = stringr::str_to_title(adm3_name)
    ) %>%
    mutate(
      adm3_name = na_if(adm3_name, "Missing"),
      adm3_name = na_if(adm3_name, "Unknown"),
      adm3_name = case_when(
        adm3_name == "Biakato Mayi" ~ "Biakato-Mayi",
        adm3_name == "Biakato Mine" ~ "Biakato-Mine",
        adm3_name == "Idohu" ~ "Idou",
        adm3_name == "Lubero Cite" ~ "Lubero-Cité",
        adm3_name == "Mukulyia" ~ "Mukulya",
        TRUE ~ adm3_name
      )
    ) %>%
    rename(
      date_vacc_mll = vacc_date
    )
  
  # adm3 exposure
  mll_exp <- mll %>%
    filter(!is.na(adm3_name)) %>%
    group_by(adm2_name, adm3_name, date = date_onset) %>%
    summarise(n_adm3 = n(),
              .groups = "drop") %>%
    complete(nesting(adm2_name, adm3_name),
             date = seq.Date(min(mll$date_onset, na.rm = TRUE),
                             as.Date("2019-06-15"),
                             "day"),
             fill = list(n_adm3 = 0)) %>%
    ungroup() %>%
    group_by(adm2_name, adm3_name) %>%
    mutate(
      n_adm3_cum = cumsum(n_adm3),
      n_adm3_19d = zoo::rollsum(n_adm3, k = 19, na.pad = TRUE, align = "right"),
      # n_adm3_exp = lag(n_adm3_19d, 2),
      n_adm3_exp = zoo::rollsum(n_adm3, k = 21, na.pad = TRUE, align = "right")
    ) %>%
    ungroup() %>%
    group_by(adm2_name, date) %>%
    mutate(n_adm2 = sum(n_adm3),
           n_adm2_cum = sum(n_adm3_cum),
           n_adm2_exp = sum(n_adm3_exp)) %>%
    select(date, adm2_name, adm3_name, contains("exp"))
  
  # Vaccination status and vaccination date
  mll_vaccination <- mll %>%
    select(mll_id, vhf_code, vacc_mll, date_vacc_mll)
  
  out <- list(
    mll = mll,
    mll_vaccination = mll_vaccination,
    mll_exp = mll_exp
  )
  
  return(out)
  
}
