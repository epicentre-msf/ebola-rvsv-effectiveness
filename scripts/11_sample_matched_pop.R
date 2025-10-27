
# Script to sample imputed-matched population

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

source(here("R/set_paths.R"))
paths <- set_paths()

source(here("R/load_data.R"))
source(here("R/wrangle_etc.R"))
source(here("R/matching.R"))

source(here("R/cluster_healthzones.R"))

dir_matched_output <- here("outputs", "1_data_mat")


# Load and wrangle data ---------------------------------------------------

etc <- etc_1step()

# "Raw" data (no imputation)
etc_format_brms <- etc$etc_format_brms

# Imputed data (method 2)
etc_format_mice <- readRDS(
  file = here("outputs", "0_data_imp", "imp_m50_unmatched_alleligible__i2.rds")
)$mice_out_format %>%
  # post-hoc add variable for suspected case
  left_join(
    etc_format_brms %>% select(patient_id_unique, suspected),
    by = "patient_id_unique"
  )

## pull number of imputations from data
m_imputations <- max(etc_format_mice$.imp, na.rm = TRUE)


# Unmatched data ----------------------------------------------------------

# Imputed
## Vaccination-onset delay 10+ days
etc_elig_10d_imp <- etc_format_mice %>%
  filter(vacc_status %in% c("Unvaccinated", "Vaccinated (10+ days)"))
saveRDS(
  etc_elig_10d_imp,
  file = here("outputs", "0_data_imp", paste0("imp_m", m_imputations, "_unmatched_10d.rds"))
)
## Vaccination-onset delay 3 - 9 days
etc_elig_3d_imp <- etc_format_mice %>%
  filter(vacc_status %in% c("Unvaccinated", "Vaccinated (3 - 9 days)"))
saveRDS(
  etc_elig_3d_imp,
  file = here("outputs", "0_data_imp", paste0("imp_m", m_imputations, "_unmatched_3d.rds"))
)

# "Raw" (no imputation)
## Vaccination-onset delay 10+ days
etc_elig_10d <- etc_format_brms %>%
  filter(vacc_status %in% c("Unvaccinated", "Vaccinated (10+ days)"))
saveRDS(
  etc_elig_10d,
  file = here("outputs", "0_data_imp", "raw_unmatched_10d.rds")
)


# Sample matched population, imputed data ---------------------------------

#### tmp
etc_elig_21d_imp <- etc_format_mice %>%
  filter(
    vacc_status %in% c("Unvaccinated", "Vaccinated (10+ days)"),
    (delay_vacc_onset >= 21 | is.na(delay_vacc_onset))
  )
matched_21d_a1_imp <- sample_matching_imp(etc_elig_m = etc_elig_21d_imp %>% filter(contact_evd_case == "Yes"), n = nm_matchsample)

# EVD exposure definitions
## A1: Contact with EVD case
matched_10d_a1_imp <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes"), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1.rds"))
)
## A2: Any contact
matched_10d_a2_imp <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_any == "Yes"), n = nm_matchsample)
saveRDS(
  matched_10d_a2_imp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a2.rds"))
)
## A3: Cases in health area
matched_10d_a3_imp <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(any_adm3 == "Yes"), n = nm_matchsample)
saveRDS(
  matched_10d_a3_imp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a3.rds"))
)
## A4: Cases in health zone
matched_10d_a4_imp <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(any_adm2 == "Yes"), n = nm_matchsample)
saveRDS(
  matched_10d_a4_imp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a4.rds"))
)

# Exclude vaccinated cases in Mangina ETC
ids_excl_mangina <- etc_format_brms %>%
  filter(contact_evd_case == "Yes") %>%
  filter((evd_status == "EVD-pos" & vacc_status == "Vaccinated (10+ days)" & etc == "Mangina (IMC)")) %>%
  pull(patient_id_unique)
matched_10d_a1_imp__excl_man <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes", !(patient_id_unique %in% ids_excl_mangina)), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__excl_man,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__excl_mangina.rds"))
)
# Exclude all Mangina ETC
ids_excl_mangina_all <- etc_format_brms %>%
  filter(contact_evd_case == "Yes") %>%
  filter(etc == "Mangina (IMC)") %>%
  pull(patient_id_unique)
matched_10d_a1_imp__excl_man_all <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes", !(patient_id_unique %in% ids_excl_mangina_all)), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__excl_man_all,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__excl_mangina_all.rds"))
)

# Exclude long vaccination-onset delays
## 3 months
ids_excl_delay_3m <- etc_format_brms %>%
  filter(contact_evd_case == "Yes") %>%
  filter(delay_vacc_onset > 13*7) %>%
  pull(patient_id_unique)
matched_10d_a1_imp__delay_3m <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes", !(patient_id_unique %in% ids_excl_delay_3m)), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__delay_3m,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__delay_3m.rds"))
)
## 6 months
ids_excl_delay_6m <- etc_format_brms %>%
  filter(contact_evd_case == "Yes") %>%
  filter(delay_vacc_onset > 26*7) %>%
  pull(patient_id_unique)
matched_10d_a1_imp__delay_6m <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes", !(patient_id_unique %in% ids_excl_delay_6m)), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__delay_6m,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__delay_6m.rds"))
)

# Vaccination strategy
## 1ml dose
dat_tmp <- etc_elig_10d_imp %>%
  filter(
    contact_evd_case == "Yes",
    date_onset < as.Date("2019-06-13")
  )
matched_10d_a1_imp__v1 <-  sample_matching_imp(etc_elig_m = dat_tmp, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__v1,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__v1.rds"))
)
## 0.5ml dose
dat_tmp <- etc_elig_10d_imp %>%
  filter(
    contact_evd_case == "Yes",
    date_onset >= as.Date("2019-06-13"),
    (!vacc | date_vacc > as.Date("2019-06-13") + 7)
  )
matched_10d_a1_imp__v2 <-  sample_matching_imp(etc_elig_m = dat_tmp, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__v2,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__v2.rds"))
)

# Suspected case definition
dat_tmp <- etc_elig_10d_imp %>%
  filter(
    contact_evd_case == "Yes",
    suspected)
matched_10d_a1_imp__susp <-  sample_matching_imp(etc_elig_m = dat_tmp, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__susp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__susp.rds"))
)

# Vaccination-onset delay 3 - 9 days
matched_3d_a1_imp <-  sample_matching_imp(etc_elig_m = etc_elig_3d_imp %>% filter(contact_evd_case == "Yes"), n = nm_matchsample)
saveRDS(
  matched_3d_a1_imp,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_3d_a1.rds"))
)




# Sample matching, raw data -----------------------------------------------

## Exposure definition A1
matched_10d_a1 <- sample_matching(etc_elig_1 = etc_elig_10d %>% filter(contact_evd_case == "Yes"), n = n_matchsample)
saveRDS(
  matched_10d_a1,
  file = here("outputs", "1_data_mat", paste0("raw_matched_n", n_matchsample, "_10d_a1.rds"))
)



# Alternative imputation methods ------------------------------------------

# Method 0
# Load imputed data, filter by vaccination and contact status
etc_elig_10d_imp_i0 <- readRDS(
  file = here("outputs", "0_data_imp", "imp_m50_unmatched_alleligible__i0.rds")
)$mice_out_format %>%
  filter(vacc_status %in% c("Unvaccinated", "Vaccinated (10+ days)")) %>%
  filter(contact_evd_case == "Yes")

# Matching
matched_10d_a1_imp_i0 <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp_i0, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp_i0,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__imp_i0.rds"))
)

# Method 1
# Load imputed data, filter by vaccination and contact status
etc_elig_10d_imp_i1 <- readRDS(
  file = here("outputs", "0_data_imp", "imp_m50_unmatched_alleligible__i1.rds")
)$mice_out_format %>%
  filter(vacc_status %in% c("Unvaccinated", "Vaccinated (10+ days)")) %>%
  filter(contact_evd_case == "Yes")

# Matching
matched_10d_a1_imp_i1 <-  sample_matching_imp(etc_elig_m = etc_elig_10d_imp_i1, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp_i1,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__imp_i1.rds"))
)


# Alternative matching criteria -------------------------------------------

# Relaxed (health zone group)
adm2_names <- etc_format_brms %>%
  filter(evd_status == "EVD-pos") %>%
  pull(adm2_name__res) %>%
  unique()

tree <- cluster_hz(subset_adm2_names = adm2_names)

dat_tmp <- etc_elig_10d_imp %>%
  filter(contact_evd_case == "Yes") %>%
  left_join(
    tibble(
      adm2_name = tree$labels,
      adm2_name__res_alt1 = paste0("group_", cutree(tree, k = 8))
    ),
    by = c("adm2_name__res" = "adm2_name")
  ) %>%
  select(-adm2_name__res) %>%
  rename(adm2_name__res = adm2_name__res_alt1)
matched_10d_a1_imp_alt1 <- sample_matching_imp(etc_elig = dat_tmp, n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp_alt1,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__alt1.rds"))
)

shp_adm2 <- readRDS(here(paths$dir_shp, "sf_adm2_latest.rds")) |> 
  left_join(tibble(adm2_name = names(cutree(tree, k = 8)), adm2_gp = as.character(cutree(tree, k = 8))))

shp_adm2 |> 
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(data = shp_adm2 %>% filter(adm1_name %in% c("Nord-Kivu", "Ituri")), fill = "lightgrey") +
  geom_sf(data = shp_adm2 %>% filter(!is.na(adm2_gp)), aes(fill = adm2_gp)) +
  scale_fill_brewer(palette = "Set2") +
  coord_sf(xlim = c(27,31.5), ylim = c(-3, 4)) +
  labs(fill = "Group") +
  theme_minimal()

# More relaxed (sex and age group only)
matched_10d_a1_imp_alt_sex_age <- sample_matching_imp__SA(etc_elig_m = etc_elig_10d_imp %>% filter(contact_evd_case == "Yes"), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp_alt_sex_age,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__alt_sexage.rds"))
)


# Severity ----------------------------------------------------------------

tmp <- etc_elig_10d_imp |> 
  left_join(etc_clean$etc_format |> select(patient_id_unique, advanced_any), by = "patient_id_unique")
matched_10d_a1_imp__adv <- sample_matching_imp(etc_elig_m = tmp %>% filter(contact_evd_case == "Yes", advanced_any), n = nm_matchsample)
saveRDS(
  matched_10d_a1_imp__adv,
  file = here(dir_matched_output, paste0("imp_m", m_imputations, "_matched_n", nm_matchsample, "_10d_a1__adv.rds"))
)
