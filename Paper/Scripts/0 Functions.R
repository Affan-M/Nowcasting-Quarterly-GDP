# Nowcasting: Functions

# Loads all the packages, sets the seed numbers and specifies any
# custom functions to be used in the project.

# Packages
library(tidyverse) # Main data manipulation
library(httr2) # Web requests (used for API)
library(glue) # String manipulation
library(zoo) # Date manipulation
library(lubridate) # Date manipulation
library(glue) # Text manipulation
library(readxl) # Read excel files
library(openxlsx) # Write excel files
library(midasr) # MIDAS regression package
library(fpp3) # FPP3: Main regression package
library(nowcasting) # DFM package
library(tseries) # For stationarity tests
library(vars, include.only = c("VARselect")) # VAR lag selection
library(tidymodels) # Broad machine learning package
library(usemodels) # Used to show sample code
library(vip) # Check importance of variables
library(rsample) # Resampling/Cross-validation folds
library(glmnet) # Required to run Lasso, Ridge, ElasticNet
library(ranger) # Required to run random forest within tidymodels
library(kernlab) # Required to run support vector regression within tidymodels
library(xgboost) # Required to run XGBoost within tidymodels
library(lightgbm) # Required to run LightGBM within tidymodels
library(bonsai) # Required to run LightGBM within tidymodels
library(stacks) # Ensemble model package
library(modeltime) # Package to run the univariates
library(thief) # Package for thief
library(parallel) # Parallelization package
library(doParallel) # Parallelization package
library(arrow) # Package for reading and exporting data
library(janitor) # Package to clean the column names
library(styler) # Package for formatting
library(lintr) # Package for formatting
library(conflicted) # Package for resolving conflicts

# ## How to install `nowcasting` package
# # Either run the commands below to install directly from github:
# install.packages("devtools")
# devtools::install_github('nmecsys/nowcasting')
#
# # Or install from a zip package after installing Rtools
# # Link for Rtools: https://cran.rstudio.com/bin/windows/Rtools/
# # Link to download zip from Github: https://github.com/nmecsys/nowcasting
# # Press "Code" and "Download Zip"

# Setting a seed number that would be used across all the scripts
seed_number <- 1234
set.seed(seed_number)

# Resolve any conflicts that could potentially come with tidymodels
tidymodels_prefer()

# Conflicts
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(vip::vi)

# Import API token
token_mma <- read_lines("config_mma.txt")


# Query DB API function ---------------------------------------------------

query_db <-
  function(series_id,
           frequency = NULL,
           include_incomplete = TRUE,
           force_convert = FALSE,
           force_conversion_method = NULL) {
    ## Function description: Uses query database API to get the data, provides
    ## frequency conversion. In the case of incomplete data, it would also allow
    ## you to aggregate up to the most recent period, or to drop the incomplete
    ## period.

    ## Function arguments:
    # series_id:  Series IDs of the series you want. Easiest to check series IDs
    #             using Viya. Can be a vector or a single series ID.
    #
    # frequency:  Specify the frequency you would like the data in. Options for
    #             frequency are "Quarterly" and "Annual". Default is NULL, in
    #             which case the data is provided at base website frequency.
    #             In the following cases, there would be a warning provided and
    #             a dataframe with base frequency would be provided:
    #               - The data is not convertible
    #               - The data is already in the frequency you want
    #                 (i.e. available in Quarterly frequency and you asked for
    #                 it to be converted to Quarterly frequency)
    #               - The data is not available in a higher frequency
    #                 (i.e. available in Quarterly frequency and you asked for
    #                 it to be converted to Monthly frequency)
    #
    # include_incomplete: Specify whether you would like the aggregate of
    #                     incomplete periods to be included or dropped.
    #                     By default, only complete quarters or years will be
    #                     shown. In the case that you want to see Year to Date
    #                     data for example, you would have to specify TRUE for
    #                     include_incomplete.
    #
    # force_convert:      Even though Query database has deemed it inconvertible,
    #                     you can ask it to be force converted. However, you need
    #                     to provide a conversion method in this case.
    #                     FALSE by default. Not recommended unless certain.
    #
    # force_coversion_method: Provides a conversion method to the function even
    #                         though database sees the series as inconvertible or
    #                         has another method specified.
    #                         Not recommended unless certain.
    #                         Default is NULL. Possible options for this argument
    #                         are:
    #                           - "sum":  Aggregation by summing
    #                           - "avg":  Aggregation by finding the average
    #                           - "ep":   Aggregation by finding end of the
    #                                     period value


    # Setting the authorization header
    bearer_token <- token_mma
    auth_headers <- c(Authorization = glue("Bearer {bearer_token}"))

    # Combining links with series IDs
    collapsed_series_id <- paste(series_id, collapse = ",")

    link <-
      glue("https://database.mma.gov.mv/api/series?ids={collapsed_series_id}") |>
      as.character()

    # Requesting data
    data_list <- list()

    page <- 1

    data_list[[page]] <-
      request(base_url = link) |>
      req_headers(
        Authorization = glue("Bearer {bearer_token}")
      ) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)

    next_page <- data_list[[page]][["links"]][["next"]]

    while (!is.null(next_page)) {
      page <- page + 1

      data_list[[page]] <-
        request(base_url = next_page) |>
        req_headers(
          Authorization = glue("Bearer {bearer_token}")
        ) |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE)

      next_page <- data_list[[page]][["links"]][["next"]]
    }

    processed_list <- list()
    meta_info <- list()

    for (p in seq_along(data_list)) {
      id <- data_list[[p]][["data"]][["id"]]

      for (i in seq_along(id)) {
        # Processing
        selected_id <- id[i]
        name <- data_list[[p]][["data"]][["name"]][i] |> str_squish()

        id_name <- glue("{selected_id} - {name}") |> as.character()

        data_frequency <- data_list[[p]][["data"]][["frequency"]][i]

        if (data_frequency == "Monthly") {
          df <- data_list[[p]][["data"]][["data"]][[i]] |>
            as_tibble() |>
            mutate(
              date = as.yearmon(date),
              name = name,
              id = selected_id
            ) |>
            distinct()
        } else if (data_frequency == "Quarterly") {
          df <- data_list[[p]][["data"]][["data"]][[i]] |>
            as_tibble() |>
            mutate(
              date = as.yearmon(date) |> as.yearqtr(),
              name = name,
              id = selected_id
            ) |>
            distinct()
        } else if (data_frequency == "Annual") {
          df <- data_list[[p]][["data"]][["data"]][[i]] |>
            as_tibble() |>
            mutate(
              date = as.yearmon(date) |> year(),
              name = name,
              id = selected_id
            ) |>
            distinct()
        }

        if (!is.null(frequency)) {
          ### Frequency conversion
          conversion_table <- tibble(
            "Frequency" = c("Monthly", "Quarterly", "Annual"),
            "Frequency_numerical" = c(12, 4, 1)
          )

          data_frequency_numerical <- conversion_table |>
            filter(Frequency == data_list[[p]][["data"]][["frequency"]][i]) |>
            pull(Frequency_numerical)

          frequency_required_numerical <- conversion_table |>
            filter(Frequency == frequency) |>
            pull(Frequency_numerical)

          # Checking if frequency required is lower than the frequency the
          # data is in
          if (frequency_required_numerical < data_frequency_numerical) {
            ## Year to date check
            last_year <- df |>
              pull(date) |>
              year() |>
              unique() |>
              max() |>
              as.character()
            check_dates <- df |>
              filter(str_detect(date, last_year)) |>
              pull(date)

            # Checking force_convert option
            if (force_convert == FALSE) {
              convertible <- data_list[[p]][["data"]][["freq_convertible"]][[i]]
            } else if (force_convert == TRUE) {
              if (!is.null(force_conversion_method)) {
                if (force_conversion_method %in% c("sum", "avg", "ep")) {
                  convertible <- TRUE
                } else {
                  stop(
                    "A valid force_conversion_method not provided. Please provide either \"sum\", \"avg\" or \"ep\" for force_conversion_method if you wish to force_convert."
                  )
                }
              } else {
                stop(
                  "No force_conversion_method provided. Please provide either \"sum\", \"avg\" or \"ep\" as a method if you wish to force_convert."
                )
              }
            }

            # Checking if it is convertible
            if (convertible == TRUE) {
              # Checking force_conversion_method
              if (is.null(force_conversion_method)) {
                conversion_method <-
                  data_list[[p]][["data"]][["conversion_method"]][i]
              } else {
                conversion_method <- force_conversion_method
              }

              # Aggregating data to the target frequency Quarterly
              if (frequency == "Quarterly") {
                # Aggregation based on conversion method
                if (conversion_method == "sum") {
                  converted_df <- df |>
                    mutate(date = as.yearqtr(date)) |>
                    group_by(date, name, id) |>
                    summarise(
                      amount = sum(amount),
                      .groups = "keep"
                    ) |>
                    ungroup()
                } else if (conversion_method == "avg") {
                  converted_df <- df |>
                    mutate(date = as.yearqtr(date)) |>
                    group_by(date, name, id) |>
                    summarise(
                      amount = mean(amount),
                      .groups = "keep"
                    ) |>
                    ungroup()
                } else if (conversion_method == "ep") {
                  df <- df |> mutate(qtr = as.yearqtr(date))

                  dates <- df |>
                    distinct(qtr) |>
                    pull()

                  converted_data <- list()

                  for (d in dates) {
                    int_df <- df |>
                      filter(qtr == d) |>
                      arrange(date) |>
                      tail(n = 1)

                    d2 <- as.character(d)

                    converted_data[[d2]] <- int_df
                  }

                  converted_df <- bind_rows(converted_data) |>
                    select(!date) |>
                    rename(date = qtr) |>
                    relocate(date)
                }


                if (length(check_dates) %% 3 != 0 &&
                  include_incomplete == FALSE) {
                  drop_period <- converted_df |>
                    pull(date) |>
                    unique() |>
                    max() |>
                    as.character()
                  converted_df <-
                    converted_df |> filter(!str_detect(date, drop_period))
                }

                # Aggregating data to the target frequency Annual
              } else if (frequency == "Annual") {
                # Aggregation based on conversion method
                if (conversion_method == "sum") {
                  converted_df <- df |>
                    mutate(date = year(date)) |>
                    group_by(date, name, id) |>
                    summarise(
                      amount = sum(amount),
                      .groups = "keep"
                    ) |>
                    ungroup()
                } else if (conversion_method == "avg") {
                  converted_df <- df |>
                    mutate(date = year(date)) |>
                    group_by(date, name, id) |>
                    summarise(
                      amount = mean(amount),
                      .groups = "keep"
                    ) |>
                    ungroup()
                } else if (conversion_method == "ep") {
                  df <- df |> mutate(year = year(date))

                  dates <- df |>
                    distinct(year) |>
                    pull()

                  converted_data <- list()

                  for (d in dates) {
                    converted_data[[d]] <- df |>
                      filter(year == d) |>
                      arrange(date) |>
                      tail(n = 1)
                  }

                  converted_df <- bind_rows(converted_data) |>
                    select(!date) |>
                    rename(date = year) |>
                    relocate(date)
                }

                # Dropping the most recent period if incomplete and
                # include_incomplete is not required (Monthly data)
                if (data_frequency == "Monthly" &&
                  length(check_dates) %% 12 != 0 &&
                  include_incomplete == FALSE) {
                  drop_period <- converted_df |>
                    pull(date) |>
                    unique() |>
                    max() |>
                    as.character()
                  converted_df <-
                    converted_df |> filter(!str_detect(date, drop_period))
                }

                # Dropping the most recent period if incomplete and
                # include_incomplete is not required (Quarterly data)
                if (data_frequency == "Quarterly" &&
                  length(check_dates) %% 4 != 0 &&
                  include_incomplete == FALSE) {
                  drop_period <- converted_df |>
                    pull(date) |>
                    unique() |>
                    max() |>
                    as.character()
                  converted_df <-
                    converted_df |> filter(!str_detect(date, drop_period))
                }
              }

              df <- converted_df
            } else {
              print(
                glue(
                  "WARNING: Series ID: {id_name} is not convertible. Providing data without conversion in the resulting dataframe."
                )
              )
            }
          }
        }

        processed_list[[id_name]] <- df |> arrange(date)

        weight <-
          data_list[[p]][["data"]] |>
          as_tibble() |>
          filter(id == selected_id) |>
          pull(base_weight) |>
          as.numeric()

        meta_info[[id_name]] <-
          tibble(
            "id" = selected_id,
            "name" = name,
            "original_frequency" = data_frequency,
            "conversion_method" = ifelse(
              exists("conversion_method"),
              conversion_method,
              data_list[[p]][["data"]][["conversion_method"]][i]
            ),
            "converted_frequency" = ifelse(!is.null(frequency), frequency, NA),
            "weight" = weight
          )
      }
    }

    final_df <-
      bind_rows(processed_list) |>
      select(date, id, name, amount)

    meta_df <- bind_rows(meta_info)

    final_list <- list(
      "Data" = final_df,
      "Meta" = meta_df
    )

    return(final_list)
  }


# Reconciliation: Shrinkage function in MinT (Shrink) ---------------------

shrink_estim <- function(x) {
  n <- nrow(x)

  tar <- diag(apply(x, 2, crossprod) / n)

  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE) {
    stop("The data matrix must be numeric!", call. = FALSE)
  }

  n <- nrow(x)

  covm <- crossprod(x) / n
  corm <- cov2cor(covm)

  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))

  v <-
    (1 / (n * (n - 1))) * (crossprod(xs^2) - 1 / n * (crossprod(xs))^2)
  diag(v) <- 0

  corapn <- cov2cor(tar)

  d <- (corm - corapn)^2

  lambda <- sum(v) / sum(d)
  lambda <- max(min(lambda, 1), 0)

  shrink.cov <- lambda * tar + (1 - lambda) * covm

  return(shrink.cov)
}


# Reconciliation: Function to carry out reconciliation --------------------

reconcile <- function(r_forecast_list, r_fitted_list) {
  # Dependent variable order. Used in reconciliation.
  order <-
    df_y[["Meta"]] |>
    pull(name)

  # Skip members if any members are incomplete
  reconcile_members <- c()
  for (i in names(r_fitted_list)) {
    series_needed <-
      length(order)

    fitted_series <-
      names(r_fitted_list[[i]]) |>
      length()

    if (series_needed == fitted_series) {
      reconcile_members <-
        c(reconcile_members, i)
    }
  }

  # Reconciliation
  reconciled_list <- list()

  for (i in reconcile_members) {
    selected_fc_df <-
      bind_rows(r_forecast_list[[i]]) |>
      mutate(model = i)

    selected_fitted_df <-
      bind_rows(r_fitted_list[[i]]) |>
      drop_na(value) |>
      mutate(model = i)

    dates <- selected_fc_df |>
      distinct(date) |>
      pull()

    # Calculating weights
    start_date <-
      selected_fitted_df |>
      group_by(series) |>
      slice_min(date) |>
      pull(date) |>
      max()

    error_covariance_matrix <-
      selected_fitted_df |>
      left_join(
        df_y[["Data"]] |> select(!id) |> rename(actual = amount),
        by = c(
          "date" = "date",
          "series" = "name"
        )
      ) |>
      mutate(error = actual - value) |>
      filter(date >= start_date) |>
      pivot_wider(
        id_cols = date,
        names_from = series,
        values_from = error
      ) |>
      select(date, all_of(order)) |>
      arrange(date) |>
      select(!date) |>
      cov()

    for (t in dates) {
      # S matrix used to sum up reconciliations
      # G matrix used to extract bottom level forecasts
      all_series_length <- length(order)
      bottom_series_length <- all_series_length - 1

      g <-
        matrix(
          c(
            rep(
              0,
              bottom_series_length * (all_series_length - bottom_series_length)
            ),
            diag(nrow = bottom_series_length, ncol = bottom_series_length)
          ),
          nrow = bottom_series_length
        )


      s <-
        matrix(
          c(
            rep(1, bottom_series_length),
            diag(nrow = bottom_series_length, ncol = bottom_series_length)
          ),
          nrow = bottom_series_length
        ) |>
        t()

      s_pr <- s |> t()

      # Base forecasts
      yhat <-
        selected_fc_df |>
        filter(date == t) |>
        arrange(match(series, order)) |>
        pull(value)

      ## Reconciliations

      # Bottom-up
      fc_bottomup <- s %*% g %*% yhat |> as.numeric()

      reconciled_list[[i]][["BU"]][[as.character(t)]] <-
        selected_fc_df |>
        filter(date == t) |>
        arrange(match(series, order)) |>
        mutate(value = fc_bottomup) |>
        mutate(model = paste(model, "Rec_BU"))

      # WLS
      tryCatch(
        {
          w_inv <-
            diag(error_covariance_matrix) |>
            Diagonal(n = length(order)) |>
            solve()

          fc_wls <-
            s %*% solve(s_pr %*% w_inv %*% s) %*% s_pr %*% w_inv %*% yhat |>
            as.numeric()

          reconciled_list[[i]][["WLS"]][[as.character(t)]] <-
            selected_fc_df |>
            filter(date == t) |>
            arrange(match(series, order)) |>
            mutate(value = fc_wls) |>
            mutate(model = paste(model, "Rec_WLS"))
        },
        error = function(e) {
          print(glue("{i} - {as.yearqtr(t)} could not be reconciled using WLS."))
        }
      )

      # MinT (Shrink)
      tryCatch(
        {
          w_inv <- solve(shrink_estim(error_covariance_matrix))

          fc_mint <-
            s %*% solve(s_pr %*% w_inv %*% s) %*% s_pr %*% w_inv %*% yhat |>
            as.numeric()

          reconciled_list[[i]][["MinT"]][[as.character(t)]] <-
            selected_fc_df |>
            filter(date == t) |>
            arrange(match(series, order)) |>
            mutate(value = fc_mint) |>
            mutate(model = paste(model, "Rec_MinT"))
        },
        error = function(e) {
          print(glue("{i} - {as.yearqtr(t)} could not be reconciled using MinT."))
        }
      )
    }
  }

  reconciled_df <-
    unlist(reconciled_list, recursive = FALSE) |>
    unlist(recursive = FALSE) |>
    bind_rows()

  return(reconciled_df)
}


# Growth to levels function -----------------------------------------------

growth_to_level <- function(growth, level) {
  values <- level

  for (i in seq_along(growth)) {
    values <-
      c(
        values,
        values[i] * (1 + growth[i])
      )
  }

  values <- values[(length(level) + 1):length(values)]

  return(values)
}


# Check number of nested lists --------------------------------------------

check_nested_list <- function(check_list) {
  levels <- 0
  check <- all(sapply(check_list, is.list))
  while (check == TRUE) {
    levels <- levels + 1
    check_list <- check_list[[1]]
    check <- all(sapply(check_list, is.list))
  }
  return(levels)
}


# Best model selection ----------------------------------------------------

best_model_selection <- function(df,
                                 h_steps,
                                 windows_back,
                                 pad = c("padded", "not_padded", "all"),
                                 combination_name) {
  # df: refers to either the experiment df
  # h_steps: forecast horizon (steps ahead) to optimize for
  # windows_back: windows to look back to select models
  #               if windows is "all", take all the static models
  # pad: whether to take best models from windows that are padded,
  #      not padded or both
  # forecast_list: List of all forecasts
  # fitted_list: List of all fitted values
  # combination_name: Name given to the combination
  # tidymodels: whether to only use the tidymodels

  order <- df_y[["Meta"]] |> pull(name)

  # How to treat pad if "all" is provided
  if (pad == "all") {
    pad <- "padded|not_padded"
  }

  # How to treat h_steps if greater than 1
  if (h_steps > 1) {
    h_steps <- 1:h_steps
  }

  # How to select models based on whether tidymodels is TRUE or FALSE
  models_vec <-
    df |>
    distinct(model) |>
    pull() |>
    paste(a = _, collapse = "|")

  # Selecting the best models and creating a best_models_df,
  # based on whether windows_back is all or not
  if (windows_back == "all") {
    min_window <- df |> pull(window) |> min()

    best_models_df <-
      df |>
      filter(
        h %in% h_steps &
          !str_detect(model, "Rec") &
          str_detect(padded, pad) &
          window >= (min_window + 12)
      ) |>
      group_by(model, series) |>
      summarise(
        rmse = sqrt(mean(error^2)),
        .groups = "drop"
      ) |>
      group_by(series) |>
      slice_min(order_by = rmse) |>
      ungroup() |>
      arrange(match(series, order)) |>
      mutate(window = "all")
  } else {
    # Ensuring a numeric number was provided in case of a mistake
    windows_back <- as.numeric(windows_back)

    # Selecting the windows
    best_models_list <- list()

    windows <- df |>
      distinct(window) |>
      max()

    for (a in (windows_back + 1):windows) {
      ## Sequence created/used using a rolling window
      # 13th window: 1 - 12 windows
      # 14th window: 2 - 13 windows
      # 15th window: 3 - 14 windows
      selected_windows <-
        seq(
          from = a - windows_back,
          to = a - 1,
          by = 1
        )

      # Take only the models without any missing series in any of the
      # windows missing
      selected_models <-
        df |>
        filter(h %in% h_steps & window %in% selected_windows) |>
        distinct(model, series, window) |>
        count(model) |>
        filter(n == (length(order) * windows_back)) |>
        pull(model)

      # Selecting the best models given the selected windows and
      # selected models
      best_models_list[[as.character(a)]] <-
        df |>
        filter(
          h %in% h_steps &
            window %in% selected_windows &
            model %in% selected_models &
            str_detect(padded, pad) &
            !str_detect(model, "Rec") &
            str_detect(model, models_vec)
        ) |>
        group_by(model, series) |>
        summarise(
          rmse = sqrt(mean(error^2)),
          .groups = "drop"
        ) |>
        group_by(series) |>
        slice_min(order_by = rmse) |>
        ungroup() |>
        arrange(match(series, order)) |>
        mutate(window = a)
    }
    best_models_df <- bind_rows(best_models_list)
  }

  # Doing a check on the best_models_df to drop any two models with the
  # same rmse. This could likely happen with general models that break
  # down to a specific model that was run. For example, ELasticNet
  # may break down to a Lasso depending on the mixture coefficient.
  experiment_windows <-
    best_models_df |>
    distinct(window) |>
    pull()

  for (w in experiment_windows) {
    issues <-
      best_models_df |>
      filter(window == w) |>
      count(series) |>
      filter(n > 1) |>
      pull(series)

    if (length(issues) > 0) {
      for (i in issues) {
        best_models_df <-
          bind_rows(
            best_models_df |>
              filter(window == w & series != i),
            best_models_df |>
              filter(window == w & series == i) |>
              slice(1)
          ) |>
          bind_rows(
            a = best_models_df |>
              filter(window != w),
            b = _
          )
      }
    }
  }

  best_models_df <-
    best_models_df |>
    arrange(window, match(series, order)) |>
    mutate(
      combination = combination_name,
      .before = model
    )

  return(best_models_df)
}


# Reconciliation combination -----------------------------------------------

reconciliation_combination <- function(best_models_df,
                                       forecast_list,
                                       fitted_list) {
  # df: refers to either the experiment df
  # h_steps: forecast horizon (steps ahead) to optimize for
  # windows_back: windows to look back to select models
  #               if windows is "all", take all the static models
  # pad: whether to take best models from windows that are padded,
  #      not padded or both
  # forecast_list: List of all forecasts
  # fitted_list: List of all fitted values
  # combination_name: Name given to the combination
  # tidymodels: whether to only use the tidymodels

  # Specifying order vector
  order <- df_y[["Meta"]] |> pull(name)

  # Creating the required lists for below
  combined_forecast_list <- list()
  combined_fitted_list <- list()

  # Retrieving info from best_models_df
  combination_name <-
    best_models_df |>
    distinct(combination) |>
    pull()

  # Checking type - whether forecast of experiment
  # Experiment will have 3 levels of lists:
  #   - Window -> Model -> Series
  # Forecast will have 2 levels of lists:
  #   - Model -> Series

  list_levels <- check_nested_list(forecast_list)

  if (list_levels == 2) {
    ## Forecast
    # Populating combined forecast and fitted list by selecting the best model
    for (y in order) {
      mod <-
        best_models_df |>
        filter(series == y) |>
        pull(model)

      combined_forecast_list[[combination_name]][[y]] <-
        forecast_list[[mod]][[y]]

      combined_fitted_list[[combination_name]][[y]] <-
        fitted_list[[mod]][[y]]
    }

    # Creating reconciliations for the combined forecasts
    reconciled_df <-
      reconcile(
        r_forecast_list = combined_forecast_list,
        r_fitted_list = combined_fitted_list
      ) |>
      group_by(model, series) |>
      mutate(h = row_number()) |>
      ungroup() |>
      rename(forecast = value)
  } else if (list_levels == 3) {
    ## Experiment
    windows_back_check <-
      best_models_df |>
      distinct(window) |>
      pull()

    if (length(windows_back_check) == 1 &&
      windows_back_check == "all") {
      windows_back <- "all"
    } else {
      windows_back <-
        best_models_df |>
        pull(window) |>
        min() - 1
    }

    # Populating combined forecast and fitted list by selecting the best model
    # for each window
    if (windows_back == "all") {
      experiment_windows <- names(forecast_list)
    } else {
      experiment_windows <-
        best_models_df |>
        distinct(window) |>
        pull() |>
        as.character()
    }

    for (w in experiment_windows) {
      for (y in order) {
        mod <-
          best_models_df |>
          filter(
            series == y,
            window ==
              ifelse(windows_back == "all",
                "all",
                as.numeric(w)
              )
          ) |>
          pull(model)

        combined_forecast_list[[w]][[combination_name]][[y]] <-
          forecast_list[[w]][[mod]][[y]]

        combined_fitted_list[[w]][[combination_name]][[y]] <-
          fitted_list[[w]][[mod]][[y]]
      }
    }

    # Creating reconciliations for the combined forecasts
    reconciled_list <- list()

    for (w in names(combined_forecast_list)) {
      reconciled_list[[w]] <-
        reconcile(
          r_forecast_list = combined_forecast_list[[w]],
          r_fitted_list = combined_fitted_list[[w]]
        ) |>
        group_by(model, series) |>
        mutate(h = row_number()) |>
        ungroup() |>
        mutate(window = as.numeric(w)) |>
        rename(forecast = value) |>
        left_join(
          df_y[["Data"]] |> select(!id) |> rename(actual = amount),
          by = c(
            "date" = "date",
            "series" = "name"
          )
        ) |>
        drop_na(actual)
    }

    first_estimate <- seq(1, windows, by = 3)
    second_estimate <- seq(2, windows, by = 3)
    third_estimate <- seq(3, windows, by = 3)

    reconciled_df <-
      bind_rows(reconciled_list) |>
      mutate(
        error = actual - forecast,
        estimate =
          ifelse(
            window %in% first_estimate,
            1,
            ifelse(
              window %in% second_estimate,
              2,
              ifelse(window %in% third_estimate, 3,
                NA
              )
            )
          ),
        padded = ifelse(estimate == 3, "not_padded", "padded")
      )
  }

  final_list <-
    list(
      "Reconciled" = reconciled_df,
      "Best models" = best_models_df
    )

  return(final_list)
}


# Relative RMSE -----------------------------------------------------------

relative_rmse <- function(df, benchmark) {
  benchmark_rmse <-
    df |>
    filter(model == benchmark) |>
    pull(rmse)

  rrmse_df <-
    df |>
    mutate(rrmse = rmse / benchmark_rmse) |>
    select(model, rrmse) |>
    arrange(rrmse)

  return(rrmse_df)
}
