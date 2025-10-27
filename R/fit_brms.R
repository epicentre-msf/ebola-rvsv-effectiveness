fit_brms <- function(data, n_samples) {
  
  fit_int <- purrr::map(
    .x = unique(data$sample_df$.imp),
    .f = ~ {
      
      list_vals <- n_samples*(.x - 1) + 1:n_samples
      
      out_int <- brms::brm_multiple(
        data = data$sample_list[list_vals],
        formula = "x_evd_status ~ x_sex + x_age_gp + x_strata + x_vacc_status",
        family = brms::bernoulli(),
        prior = c(
          prior(normal(0, 1), class = "Intercept"),
          prior(normal(0, 1), class = b),
          prior(normal(-1, 0.7), class = b, coef = "x_vacc_statusvaccinated_10d")
        ),
        iter = 4000,
        combine = FALSE
      )
      
      return(out_int)
      
    }
  )
  out <- unlist(fit_int, recursive = FALSE)
  
  return(out)
  
}


# Run the same brms model on multiple datasets
#
# Simple edit of brm_multiple to take modified stan code (specific application)
# See: https://github.com/paul-buerkner/brms/issues/682

brm_multiple_mod <- function(data, model_str, stan_file) {
  
  # START https://github.com/paul-buerkner/brms/issues/682
  # Generate stan code
  dummy_brmsfit <- brms::brm(
    data = data[[1]],
    formula = model_str,
    family = brms::bernoulli(),
    empty = TRUE
    )
  # Create data for stan
  data_stan <- brms::make_standata(
    data = data[[1]],
    formula = model_string
    )
  # Create a dummy stanfit object via
  dummy_stanfit <- rstan::stan(
    file = here::here("data", "stan", stan_file),
    data = data_stan,
    chains = 0
    )
  # Replace the stanfit object inside a dummy brmsfit object
  dummy_brmsfit$fit <- dummy_stanfit
  dummy_brmsfit$model <- rstan::get_stancode(dummy_stanfit)[1]
  # END
  # First update with new stan code
  dummy_brmsfit <- update(dummy_brmsfit, recompile = FALSE, control = list(adapt_delta = 0.99))
  
  # START brms::brm_multiple
  fits <- futures <- vector("list", length(data))
  for (i in seq_along(data)) {
    futures[[i]] <- future::future(
      update(dummy_brmsfit, newdata = data[[i]], refresh = 0, recompile = FALSE, control = list(adapt_delta = 0.99)),
      packages = "brms", seed = TRUE
      )
    }
  for (i in seq_along(data)) {
    message("Fitting imputed model ", i)
    fits[[i]] <- future::value(futures[[i]])
    }
  ## combine model fits
  fits_combined <- brms::combine_models(mlist = fits, check_data = FALSE)
  class(fits_combined) <- c("brmsfit_multiple", class(fits_combined))
  
  return(list(
    ind = fits,
    com = fits_combined
    ))
  
}
