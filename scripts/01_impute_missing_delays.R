
# Script to impute missing data

## Parameter: Number of imputations
m_imputations <- 50


# Set up ------------------------------------------------------------------

library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)

library(mice)

source(here("R/set_paths.R"))
paths <- set_paths()

source(here("R/load_data.R"))
source(here("R/wrangle_etc.R"))

source(here("R/imputation.R"))

# Load and wrangle ETC data -----------------------------------------------

etc <- etc_1step()

# Save raw data
saveRDS(
  object = etc_format_brms,
  file = here("outputs", "0_data_imp", "raw_unmatched_alleligible.rds")
)

# MICE! -------------------------------------------------------------------

# Method 0 (combined, without contact variable)
etc_imp_0 <- mice_pipeline(etc = etc, m = m_imputations)
## save imputed data
file_name <- paste0("imp_m", m_imputations, "_unmatched_alleligible__i0.rds")
saveRDS(
  object = etc_imp_0,
  file = here("outputs", "0_data_imp", file_name)
)


# Method 1 (combined)
etc_imp_1 <- mice_pipeline(etc = etc, m = m_imputations)
## save imputed data
file_name <- paste0("imp_m", m_imputations, "_unmatched_alleligible__i1.rds")
saveRDS(
  object = etc_imp_1,
  file = here("outputs", "0_data_imp", file_name)
)


# Method 2 (by EVD status)
etc_imp_pos <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-pos")
etc_imp_neg <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-neg")
etc_imp_2 <- list(
  mice_out_pos = etc_imp_pos$mice_out,
  mice_out_neg = etc_imp_neg$mice_out,
  mice_out_format = bind_rows(etc_imp_pos$mice_out_format, etc_imp_neg$mice_out_format)
)
## save imputed data
file_name <- paste0("imp_m", m_imputations, "_unmatched_alleligible__i2.rds")
saveRDS(
  object = etc_imp_2,
  file = here("outputs", "0_data_imp", file_name)
)


# Method 3 (by EVD status and contact status)
etc_imp_pos_c <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-pos", contact_status_val = TRUE)
etc_imp_pos_nc <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-pos", contact_status_val = FALSE)
etc_imp_neg_c <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-neg", contact_status_val = TRUE)
etc_imp_neg_nc <- mice_pipeline(etc = etc, m = m_imputations, evd_status_val = "EVD-neg", contact_status_val = FALSE)
etc_imp_3 <- list(
  mice_out_pos_c = etc_imp_pos_c$mice_out,
  mice_out_pos_nc = etc_imp_pos_nc$mice_out,
  mice_out_neg_c = etc_imp_neg_c$mice_out,
  mice_out_neg_nc = etc_imp_neg_nc$mice_out,
  mice_out_format = bind_rows(
    etc_imp_pos_c$mice_out_format,
    etc_imp_pos_nc$mice_out_format,
    etc_imp_neg_c$mice_out_format,
    etc_imp_neg_nc$mice_out_format
  )
)
## save imputed data
file_name <- paste0("imp_m", m_imputations, "_unmatched_alleligible__i3.rds")
saveRDS(
  object = etc_imp_3,
  file = here("outputs", "0_data_imp", file_name)
)
