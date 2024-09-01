# Nowcasting: Experiment

# This script would run the experiment as an expanding window, with end_train
# specifying the end of the first training set.

# Getting the required data
# source("1 Data.R")
load("Data.rdata")
source("0 Functions.R")

# Starting timer
start <- Sys.time()

# Tibble to measure time taken to run each window
window_times <-
  tibble(
    "window" = as.double(),
    "time" = as.double()
  )

# Setting up experiment start date and windows
end_train <- "Dec 2017" |> as.yearmon()

max_date <-
  df_y[["Data"]] |>
  pull(date) |>
  max() |>
  as.yearmon() + 2 / 12

windows <-
  df_x[["Data"]] |>
  distinct(date) |>
  filter(date > end_train & date <= max_date) |>
  nrow()

# Experiment list to store the full tibble
experiment_list <- list()

# Forecast and fitted list
forecast_list <- list()
fitted_list <- list()

# non_padded_windows <- seq(from = 3, to = windows, by = 3)

stack_issues <- c()

for (w in 1:windows) {

  # Preparing training and test sets ----------------------------------------
  window_start <- Sys.time()

  time <- end_train + w / 12

  print(glue("Window: {w}/{windows}, Time: {time}"))

  # Train
  train_y <- df_y[["Data"]] |> filter(date < as.yearqtr(time))
  train_x <- df_x[["Data"]] |> filter(date <= time)

  # Test
  test_y <-
    df_y[["Data"]] |>
    filter(date >= as.yearqtr(time) &
             date <= (as.yearqtr(time) + 1))


  # Imputation for missing data ---------------------------------------------

  print(glue("Imputation"))

  # Fish catch will be removed to be imputed and forecasted later using
  # fish purchases forecasts

  df <-
    train_x |>
    filter(date >= "Jan 2004" & !id %in% c(249, 250, 251, 252)) |>
    mutate(date = as.Date.yearmon(date) |> yearmonth(date)) |>
    as_tsibble(index = date, key = c(id, name)) |>
    fill_gaps() |>
    as_tibble() |>
    mutate(date = as.Date(date)) |>
    pivot_wider(
      id_cols = date,
      names_from = c(id, name),
      values_from = amount,
      names_sep = " "
    ) |>
    arrange(date)


  # Imputation
  set.seed(seed = seed_number)

  rec <-
    recipe(x = df, roles = rep("predictor", ncol(df))) |>
    step_nzv(all_numeric_predictors()) |>
    # step_filter_missing(all_numeric_predictors(), threshold = 0.95) |>
    step_normalize(all_numeric_predictors()) |>
    step_impute_knn(all_predictors())

  rec_object <- prep(rec)

  imputed_df <- juice(rec_object)

  normalization_df <- tidy(rec_object, number = 2)

  ids <- normalization_df |>
    distinct(terms) |>
    pull()

  # Denormalizing after imputing
  for (i in ids) {
    mean <- normalization_df |>
      filter(terms == i & statistic == "mean") |>
      pull(value)

    sd <- normalization_df |>
      filter(terms == i & statistic == "sd") |>
      pull(value)

    imputed_df <-
      imputed_df |>
      rename(i = all_of(i)) |>
      mutate(i = i * sd + mean)

    colnames(imputed_df)[which(colnames(imputed_df) == "i")] <- i
  }

  imputed_df <-
    imputed_df |>
    pivot_longer(
      cols = !date,
      names_to = c("id", "name"),
      values_to = "amount",
      names_sep = " "
    ) |>
    mutate(
      date = as.yearmon(date),
      id = as.numeric(id)
    ) |>
    arrange(id, date)


  # Forecast padding for training set ---------------------------------------

  print(glue("Forecast padding"))

  # Padding the incomplete dates of the quarters with short term forecasts
  ids <- imputed_df |>
    distinct(id) |>
    pull()

  h_list <- list()

  max_y_date <- train_y |>
    distinct(date) |>
    pull() |>
    max()

  max_month <- as.yearmon(max_y_date + 1) + 2 / 12

  start_date <-
    (imputed_df |> pull(date) |> max() - 200 / 12) |>
    year() |>
    paste(a = "Jan", b = _) |>
    as.yearmon()

  tsib <-
    imputed_df |>
    filter(id %in% ids & date >= start_date) |>
    mutate(date = yearmonth(date)) |>
    tsibble(index = date, key = c(id, name))

  set.seed(seed = seed_number)

  fc_df <-
    tsib |>
    group_by_key() |>
    model(arima = ARIMA(amount)) |>
    forecast(h = 12)


  # Data aggregations -------------------------------------------------------

  print(glue("Data aggregation"))

  data_list <-
    list(
      "x_tidy" =
        fc_df |>
        as_tibble() |>
        select(date, id, name, .mean) |>
        rename(amount = .mean) |>
        mutate(date = as.Date(date) |> as.yearmon()) |>
        bind_rows(a = imputed_df, b = _) |>
        filter(date <= max_month & date >= "Jan 2004") |>
        arrange(id, date),
      "y" =
        train_y |>
        filter(date >= "2004 Q1")
    )

  # Adding in fish catch
  fish_catch_reg_df <-
    data_list[["x_tidy"]] |>
    filter(id %in% c(253, 254, 255, 256)) |>
    bind_rows(
      train_x |>
        filter(date >= "Jan 2004" & id %in% c(249, 250, 251, 252))
    ) |>
    pivot_wider(
      id_cols = date,
      names_from = c(id, name),
      values_from = amount,
      names_sep = " "
    ) |>
    mutate(date = yearmonth(date)) |>
    as_tsibble(index = date)

  train <-
    fish_catch_reg_df |>
    filter(!is.na(`249 fish_catch`))

  test <-
    fish_catch_reg_df |>
    filter(is.na(`249 fish_catch`))

  # Regressions
  fish_catch_fit_list <- list()

  fish_catch_fit_list[["249 fish_catch"]] <-
    train |>
    model(
      ARIMA(log(`249 fish_catch`) ~
              log(`253 fresh_fish_purchases`))
    )

  fish_catch_fit_list[["250 fish_catch_skipjack_tuna"]] <-
    train |>
    model(
      ARIMA(log(`250 fish_catch_skipjack_tuna`) ~
              log(`254 fish_purchases_skipjack_tuna`))
    )

  fish_catch_fit_list[["251 fish_catch_yellowfin_tuna"]] <-
    train |>
    model(
      ARIMA(log(`251 fish_catch_yellowfin_tuna`) ~
              log(`255 fish_purchases_yellowfin_tuna`))
    )

  fish_catch_fit_list[["252 fish_catch_other"]] <-
    train |>
    model(
      ARIMA(log(`252 fish_catch_other`) ~
              log(`256 fish_purchases_other`))
    )

  for (f in names(fish_catch_fit_list)) {
    fc <-
      bind_rows(
        train |>
          select(date, all_of(f)) |>
          rename(.mean = f),
        fish_catch_fit_list[[f]] |>
          forecast(new_data = test) |>
          as_tibble() |>
          select(date, .mean)
      )

    colnames(fc)[2] <- f

    fc <-
      fc |>
      pivot_longer(
        cols = !"date",
        names_to = c("id", "name"),
        values_to = "amount",
        names_sep = " "
      ) |>
      as_tibble() |>
      mutate(
        date = as.Date(date) |> as.yearmon(),
        id = as.numeric(id)
      )

    data_list[["x_tidy"]] <-
      bind_rows(
        data_list[["x_tidy"]],
        fc
      )
  }

  # Aggregations
  agg_data_list <- list()

  sum_series_id <-
    df_x[["Meta"]] |>
    filter(conversion_method == "sum") |>
    pull(id)

  if (length(sum_series_id) > 0) {
    agg_data_list[["x_tidy"]][["sum"]] <-
      data_list[["x_tidy"]] |>
      filter(id %in% sum_series_id) |>
      mutate(qtrdate = as.yearqtr(date)) |>
      group_by(qtrdate, id, name) |>
      summarise(amount = sum(amount), .groups = "drop") |>
      rename(date = qtrdate)
  }

  avg_series_id <-
    df_x[["Meta"]] |>
    filter(conversion_method == "avg") |>
    pull(id)

  if (length(avg_series_id) > 0) {
    agg_data_list[["x_tidy"]][["mean"]] <-
      data_list[["x_tidy"]] |>
      filter(id %in% avg_series_id) |>
      mutate(qtrdate = as.yearqtr(date)) |>
      group_by(qtrdate, id, name) |>
      summarise(amount = mean(amount), .groups = "drop") |>
      rename(date = qtrdate)
  }

  ep_series_id <-
    df_x[["Meta"]] |>
    filter(conversion_method == "ep") |>
    pull(id)

  if (length(ep_series_id) > 0) {
    month_dates <-
      data_list[["x_tidy"]] |>
      filter(id %in% ep_series_id) |>
      pull(date) |>
      as.yearqtr() |>
      unique() |>
      as.yearmon() + 2 / 12


    agg_data_list[["x_tidy"]][["ep"]] <-
      data_list[["x_tidy"]] |>
      filter(id %in% ep_series_id & date %in% month_dates) |>
      mutate(date = as.yearqtr(date)) |>
      arrange(match(id, ep_series_id), date)
  }

  agg_data_list[["x_tidy"]] <-
    agg_data_list[["x_tidy"]] |>
    bind_rows() |>
    arrange(id, date)

  agg_data_list[["y"]] <- data_list[["y"]]

  # Converting to a matrix time series
  mat_list <- list()
  for (x in names(data_list)) {
    mat_list[[x]] <-
      data_list[[x]] |>
      pivot_wider(
        id_cols = date,
        names_from = "id",
        values_from = "amount"
      ) |>
      select(!"date") |>
      as.matrix() |>
      ts(
        start = c(2004, 1),
        frequency = ifelse(x == "y", 4, 12)
      )
  }

  agg_mat_list <- list()
  for (x in names(agg_data_list)) {
    agg_mat_list[[x]] <-
      agg_data_list[[x]] |>
      pivot_wider(
        id_cols = date,
        names_from = "id",
        values_from = "amount"
      ) |>
      select(!"date") |>
      as.matrix() |>
      ts(start = c(2004, 1), frequency = 4)
  }


  # Bridge ------------------------------------------------------------------

  print(glue("Bridge"))

  bridge_data <- c("Bridge_levels", "Bridge_growth")

  # Extract the names of the sector series
  y_vec_names <- df_y[["Meta"]] |> pull(name)

  # Extract the last date for y to subset the tsibs train and test
  last_y <-
    agg_data_list[["y"]] |>
    select(date) |>
    pull() |>
    max() |>
    yearquarter()

  # Empty list to populate  with tsibs for bridge regressions and mables
  bridge_models <- list()

  # Loop through the sectors to assign regressors, model and produce forecasts
  # for each sector
  for (y in y_vec_names) {
    if (y == y_vec_names[1]) {
      # Real GDP
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[2]) {
      # Taxes less subsidies
      # selected_ids <- 30010 # Import duty + GST - Subsidies, CPI deflated
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[3]) {
      # Fisheries
      # selected_ids <- 267 # Fish exports - CPI deflated
      selected_ids <- 249 # Fish catch
    } else if (y == y_vec_names[4]) {
      # Trade
      selected_ids <- 23457 # Total imports - CPI deflated
    } else if (y == y_vec_names[5]) {
      # Tourism
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[6]) {
      # Transport and communication
      selected_ids <- 104 # Arrivals
    } else if (y == y_vec_names[7]) {
      # Financial services
      selected_ids <- c(10008, 10009) # Loans and advances, deposits (5SG) - Avg
    } else if (y == y_vec_names[8]) {
      # Construction and real estate
      selected_ids <- 40001 # Construction-related imports - PINDU deflated
    } else if (y == y_vec_names[9]) {
      # Public admin, health and education
      selected_ids <- 30007 # Salaries and allowances - CPI deflated
    } else if (y == y_vec_names[10]) {
      # Miscellaneous
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    }

    # Create a tsib of dependent variables and relevant regressors to use in
    # regressions
    tsib <-
      full_join(
        agg_data_list[["y"]] |>
          filter(name == y) |>
          select(date, amount),
        agg_data_list[["x_tidy"]] |>
          filter(id %in% selected_ids) |>
          pivot_wider(
            id_cols = date,
            names_from = name,
            values_from = amount
          ),
        by = "date"
      ) |>
      mutate(
        series = y,
        date = yearquarter(date)
      ) |>
      as_tsibble(index = date) |>
      relocate(series, .after = date)

    # Rename the dependent variables and regressors uniformly as y and
    # x1, x2...,xn to loop through
    colnames(tsib) <-
      c("date", "series", "y", paste0("x", seq_along(selected_ids)))

    for (b in bridge_data) {
      # For bridge_growth calculate the growth variables
      if (b == bridge_data[[2]]) {
        tsib <-
          tsib |>
          mutate(across(
            .cols = !c(date, series),
            .fns = function(x) {
              x / dplyr::lag(x, 4) - 1
            }
          )) |>
          drop_na(x1)
      }

      # Train and test sets
      train <-
        tsib |> filter(date <= last_y)

      test <-
        tsib |> filter(date > last_y)

      # Train the models using an automated formula
      formula <-
        tsib |>
        colnames() |>
        str_subset(string = _, pattern = "x") |>
        paste(collapse = " + ") |>
        paste(a = "y ~", b = _) |>
        as.formula()

      fit <-
        train |>
        model("Bridge" = TSLM(formula))

      bridge_models[[b]][[y]] <- fit

      # Forecasts
      forecasts <-
        fit |>
        forecast(new_data = test)

      growth_to_level(
        growth =
          forecasts |>
          pull(.mean),
        level =
          agg_data_list[["y"]] |>
          filter(name == y) |>
          tail(4) |>
          pull(amount)
      )

      # Fitted values
      augment_df <-
        fit |>
        augment() |>
        drop_na(.fitted)

      # Exporting to forecast_list and fitted_list
      if (b == bridge_data[2]) {
        # Forecasts
        forecast_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = forecasts$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value =
              growth_to_level(
                growth =
                  forecasts |>
                  pull(.mean),
                level =
                  agg_data_list[["y"]] |>
                  filter(name == y) |>
                  tail(4) |>
                  pull(amount)
              )
          )

        # Fitted values
        fitted_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date =
              augment_df |>
              pull(date) |>
              as.Date() |>
              as.yearqtr(),
            model = b,
            series = y,
            value =
              growth_to_level(
                growth =
                  augment_df |>
                  pull(.fitted),
                level =
                  agg_data_list[["y"]] |>
                  filter(name == y) |>
                  head(4) |>
                  pull(amount)
              )
          )
      } else {
        # Forecast values
        forecast_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = forecasts$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value = forecasts$.mean
          )

        # Fitted values
        fitted_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = augment_df$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value = augment_df$.fitted
          )
      }
    }
  }


  # Bridge with ARIMA errors ------------------------------------------------

  print(glue("Bridge w/ ARIMA errors"))

  bridge_data <- c("Bridge_ARIMA_levels", "Bridge_ARIMA_growth")

  # Extract the names of the sector series
  y_vec_names <- df_y[["Meta"]] |> pull(name)

  # Extract the last date for y to subset the tsibs train and test
  last_y <-
    agg_data_list[["y"]] |>
    select(date) |>
    pull() |>
    max() |>
    yearquarter()

  # Empty list to populate  with tsibs for bridge regressions and mables
  bridge_arima_models <- list()

  # Loop through the sectors to assign regressors, model and produce forecasts
  # for each sector
  for (y in y_vec_names) {
    if (y == y_vec_names[1]) {
      # Real GDP
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[2]) {
      # Taxes less subsidies
      # selected_ids <- 30010 # Import duty + GST - Subsidies, CPI deflated
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[3]) {
      # Fisheries
      # selected_ids <- 267 # Fish exports - CPI deflated
      selected_ids <- 249 # Fish catch
    } else if (y == y_vec_names[4]) {
      # Trade
      selected_ids <- 23457 # Total imports - CPI deflated
    } else if (y == y_vec_names[5]) {
      # Tourism
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y == y_vec_names[6]) {
      # Transport and communication
      selected_ids <- 104 # Arrivals
    } else if (y == y_vec_names[7]) {
      # Financial services
      selected_ids <- c(10008, 10009) # Loans and advances, deposits (5SG) - Avg
    } else if (y == y_vec_names[8]) {
      # Construction and real estate
      selected_ids <- 40001 # Construction-related imports - PINDU deflated
    } else if (y == y_vec_names[9]) {
      # Public admin, health and education
      selected_ids <- 30007 # Salaries and allowances - CPI deflated
    } else if (y == y_vec_names[10]) {
      # Miscellaneous
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    }

    # Create a tsib of dependent variables and relevant regressors to use in
    # regressions
    tsib <-
      full_join(
        agg_data_list[["y"]] |>
          filter(name == y) |>
          select(date, amount),
        agg_data_list[["x_tidy"]] |>
          filter(id %in% selected_ids) |>
          pivot_wider(
            id_cols = date,
            names_from = name,
            values_from = amount
          ),
        by = "date"
      ) |>
      mutate(
        series = y,
        date = yearquarter(date)
      ) |>
      as_tsibble(index = date) |>
      relocate(series, .after = date)

    # Rename the dependent variables and regressors uniformly as y and
    # x1, x2...,xn to loop through
    colnames(tsib) <-
      c("date", "series", "y", paste0("x", seq_along(selected_ids)))

    for (b in bridge_data) {
      # For bridge_growth calculate the growth variables
      if (b == bridge_data[[2]]) {
        tsib <-
          tsib |>
          mutate(across(
            .cols = !c(date, series),
            .fns = function(x) {
              x / dplyr::lag(x, 4) - 1
            }
          )) |>
          drop_na(x1)
      }

      # Train and test sets
      train <-
        tsib |> filter(date <= last_y)

      test <-
        tsib |> filter(date > last_y)

      # Train the models using an automated formula
      formula <-
        tsib |>
        colnames() |>
        str_subset(string = _, pattern = "x") |>
        paste(collapse = " + ") |>
        paste(a = "y ~", b = _) |>
        as.formula()

      fit <-
        train |>
        model("Bridge_ARIMA" = ARIMA(formula))

      bridge_arima_models[[b]][[y]] <- fit

      # Forecasts
      forecasts <-
        fit |>
        forecast(new_data = test)

      # Fitted values
      augment_df <-
        fit |>
        augment() |>
        drop_na(.fitted)

      # Exporting to forecast_list and fitted_list
      if (b == bridge_data[2]) {
        # Forecasts
        forecast_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = forecasts$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value =
              growth_to_level(
                growth =
                  forecasts |>
                  pull(.mean),
                level =
                  agg_data_list[["y"]] |>
                  filter(name == y) |>
                  tail(4) |>
                  pull(amount)
              )
          )

        # Fitted values
        fitted_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date =
              augment_df |>
              pull(date) |>
              as.Date() |>
              as.yearqtr(),
            model = b,
            series = y,
            value =
              growth_to_level(
                growth =
                  augment_df |>
                  pull(.fitted),
                level =
                  agg_data_list[["y"]] |>
                  filter(name == y) |>
                  head(4) |>
                  pull(amount)
              )
          )
      } else {
        # Forecast values
        forecast_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = forecasts$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value = forecasts$.mean
          )

        # Fitted values
        fitted_list[[as.character(w)]][[b]][[y]] <-
          tibble(
            date = augment_df$date |> as.Date() |> as.yearqtr(),
            model = b,
            series = y,
            value = augment_df$.fitted
          )
      }
    }
  }


  # MIDAS -------------------------------------------------------------------

  print(glue("MIDAS"))

  # Empty lis to to store the ts for y and x and the fitted models
  midas_models <- list()

  # String vector to loop through for levels and growth
  midas_data <- c("levels", "growth")

  # String vector to loop through UMIDAS and RMIDAS
  midas_method <- c("UMIDAS", "RMIDAS")

  # Extract the ids of GDP
  y_vec_id <- df_y[["Meta"]] |> pull(id)

  # Extract the names of the sector series
  y_vec_names <- df_y[["Meta"]] |> pull(name)

  for (y in y_vec_id) {
    # Extract the corresponding y name for series id
    y_name <-
      df_y[["Meta"]] |>
      filter(id == y) |>
      pull(name)

    if (y_name == y_vec_names[1]) {
      # Real GDP
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y_name == y_vec_names[2]) {
      # Taxes less subsidies
      # selected_ids <- 30010 # Import duty + GST - Subsidies, CPI deflated
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y_name == y_vec_names[3]) {
      # Fisheries
      # selected_ids <- 267 # Fish exports - CPI deflated
      selected_ids <- 249 # Fish catch
    } else if (y_name == y_vec_names[4]) {
      # Trade
      selected_ids <- 23457 # Total imports - CPI deflated
    } else if (y_name == y_vec_names[5]) {
      # Tourism
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    } else if (y_name == y_vec_names[6]) {
      # Transport and communication
      selected_ids <- 104 # Arrivals
    } else if (y_name == y_vec_names[7]) {
      # Financial services
      selected_ids <- c(10008, 10009) # Loans and advances, deposits (5SG) - Avg
    } else if (y_name == y_vec_names[8]) {
      # Construction and real estate
      selected_ids <- 40001 # Construction-related imports - PINDU deflated
    } else if (y_name == y_vec_names[9]) {
      # Public admin, health and education
      selected_ids <- 30007 # Salaries and allowances - CPI deflated
    } else if (y_name == y_vec_names[10]) {
      # Miscellaneous
      # selected_ids <- 205 # Bednights
      selected_ids <- 208 # Bednights - Resorts
    }

    # Dates for subsetting the ts into train and test

    # First time period (in yearmon) for which the target variable is
    # observed
    y_start <-
      mat_list[["y"]][, as.character(y)] |>
      time() |>
      min() |>
      as.yearqtr() |>
      as.yearmon()

    # Last date for which the target variable is observed
    y_max_time <-
      mat_list[["y"]][, as.character(y)] |>
      time() |>
      max() |>
      as.yearqtr() |>
      as.yearmon() + 2 / 12

    # First date for which the selected regressor is observed
    x_start <-
      mat_list[["x_tidy"]][, as.character(selected_ids)] |>
      na.omit() |>
      time() |>
      min() |>
      as.yearqtr() |>
      as.yearmon()

    # Train start time
    train_start <- max(x_start, y_start)

    # Extract the start time for the test windows
    test_start <- y_max_time + 1 / 12

    # Calculate the level
    x_level <-
      mat_list[["x_tidy"]][, as.character(selected_ids)] |>
      na.omit()

    x_level_df <-
      as_tibble(x_level) |>
      mutate(date = time(x_level) |> as.yearmon() |> yearmonth(),
             .before = 1) |>
      drop_na() |>
      as_tsibble(index = date)

    vars <- paste0("x", 1:(ncol(x_level_df) - 1))
    colnames(x_level_df) <- c("date", vars)

    for (m in midas_data) {
      # Attach the low frequency target variable and corresponding regressors
      # for each sector, in levels and growth
      if (m == "levels") {
        # Train
        midastrain_df <- x_level_df |> filter(date <= yearmonth(y_max_time))

        # Test
        midastest_df <- x_level_df |> filter(date >= yearmonth(test_start))

        if (length(vars) == 1) {
          x1_train <- midastrain_df |>  select(x1) |>  as.ts()
          x1_test <- midastest_df |>  select(x1) |>  as.ts()
        } else if (length(vars) == 2) {
          x1_train <- midastrain_df |>  select(x1) |>  as.ts()
          x2_train <- midastrain_df |>  select(x2) |>  as.ts()

          x1_test <- midastest_df |>  select(x1) |>  as.ts()
          x2_test <- midastest_df |>  select(x2) |>  as.ts()
        }

        # Low frequency target variable
        midasy <-
          mat_list[["y"]][, as.character(y)] |>
          window(start = c(year(train_start), month(train_start)))

      } else if (m == "growth") {
        # Growth df
        x_growth_df <-
          x_level_df |>
          mutate(across(.cols = !date,
                        .fns = function(x) x / dplyr::lag(x, 12) - 1)) |>
          drop_na()

        # Train
        midastrain_df <- x_growth_df |> filter(date <= yearmonth(y_max_time))

        # Test
        midastest_df <- x_growth_df |> filter(date >= yearmonth(test_start))

        if (length(vars) == 1) {
          x1_train <- midastrain_df |>  select(x1) |>  as.ts()
          x1_test <- midastest_df |>  select(x1) |>  as.ts()
        } else if (length(vars) == 2) {
          x1_train <- midastrain_df |>  select(x1) |>  as.ts()
          x2_train <- midastrain_df |>  select(x2) |>  as.ts()

          x1_test <- midastest_df |>  select(x1) |>  as.ts()
          x2_test <- midastest_df |>  select(x2) |>  as.ts()
        }

        # Low frequency target variable in growth
        y_levels <-
          mat_list[["y"]][, as.character(y)] |> as.vector()

        y_growth <- na.omit(y_levels / dplyr::lag(y_levels, 4) - 1)

        y_growth_ts <-
          ts(y_growth,
             start = c(year(y_start) + 1, 1),
             frequency = 4)

        # Train set
        midasy <-
          y_growth_ts |>
          window(start = c(year(train_start) + 1, month(train_start)))
      }

      for (i in midas_method) {
        if (i == "UMIDAS") {
          if (length(vars) == 1) {
            fit <-
              midas_r(
                midasy ~ mls(x1_train, 0:2, 3),
                start = NULL
              )
          } else if (length(vars) == 2) {
            fit <-
              midas_r(
                midasy ~ mls(x1_train, 0:2, 3) + mls(x2_train, 0:2, 3),
                start = NULL
              )
          }
        } else if (i == "RMIDAS") {
          if (length(vars) == 1) {
            fit <-
              midas_r(
                midasy ~ mls(x1_train, 0:2, 3, nealmon),
                start = list(x1_train = c(1, -0.5))
              )
          } else if (length(vars) == 2) {
            fit <-
              midas_r(
                midasy ~ mls(x1_train, 0:2, 3, nealmon) +
                  mls(x2_train, 0:2, 3, nealmon),
                start = list(x1_train = c(1, -0.5),
                             x2_train = c(1, -0.5))
              )
          }
        }

        # Store output
        label <- paste(c(i, m), collapse = "_")

        midas_models[[label]][[y_name]][["fit"]] <- fit

        # Forecasts
        if (length(vars) == 1) {
          forecast <-
            midasr::forecast(fit,
                             newdata = list(x1_train = x1_test))
        } else if (length(vars) == 2) {
          forecast <-
            midasr::forecast(fit,
                             newdata = list(x1_train = x1_test,
                                            x2_train = x2_test))
        }

        # Forecast and fitted list
        if (m == "levels") {
          forecast_list[[as.character(w)]][[label]][[y_name]] <-
            tibble(
              date = forecast$mean |> time() |> as.yearqtr(),
              model = label,
              series = y_name,
              value = forecast$mean |> as.numeric()
            )

          fitted_list[[as.character(w)]][[label]][[y_name]] <-
            tibble(
              date = midasy |> time() |> as.yearqtr(),
              model = label,
              series = y_name,
              value = forecast$fitted
            )
        } else if (m == "growth") {
          forecast_list[[as.character(w)]][[label]][[y_name]] <-
            tibble(
              date = forecast$mean |> time() |> as.yearqtr(),
              model = label,
              series = y_name,
              value = growth_to_level(
                growth = forecast$mean |> as.numeric(),
                level = agg_data_list[["y"]] |>
                  filter(name == y_name) |>
                  tail(4) |>
                  pull(amount)
              )
            )

          fitted_list[[as.character(w)]][[label]][[y_name]] <-
            tibble(
              date = midasy |> time() |> as.yearqtr(),
              model = label,
              series = y_name,
              value = growth_to_level(
                growth = forecast$fitted |> as.numeric(),
                level = agg_data_list[["y"]] |>
                  filter(name == y_name) |>
                  tail(4) |>
                  pull(amount)
              )
            )
        }
      }
    }
  }


  # Tidy models -------------------------------------------------------------

  tidymodels <- list()

  print(glue("Tidymodels"))

  # Setting up parallelization
  all_cores <- detectCores() - 1
  cl <- makePSOCKcluster(all_cores)
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(tidymodels)
    library(glmnet)
    library(ranger)
    library(kernlab)
    library(xgboost)
    library(lightgbm)
    library(bonsai)
    library(stacks)
    library(modeltime)
    library(thief)
  })

  # Dependent variables for tidymodels
  y_vec_names <- df_y[["Meta"]] |> pull(name)

  # Independent variables df
  df <-
    agg_data_list[["x_tidy"]] |>
    group_by(id) |>
    mutate(
      date = as.Date.yearqtr(date),
      amount = amount - lag(amount, 1)
    ) |>
    pivot_wider(
      id_cols = date,
      names_from = name,
      values_from = amount
    ) |>
    drop_na()

  # Preprocessing independent variables df
  proc_df_x <-
    recipe(x = df, roles = rep("predictor", ncol(df))) |>
    step_date(
      date,
      features = c("quarter", "year"),
      keep_original_cols = TRUE
    ) |>
    step_YeoJohnson(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_nzv(all_numeric_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.95) |>
    prep() |>
    juice() |>
    arrange(date)

  # Grid size
  grid_size <- 100

  for (y in y_vec_names) {
    print(glue("{y}"))

    # Training and test sets --------------------------------------------------

    # CS
    df <-
      agg_data_list[["y"]] |>
      filter(name == y) |>
      mutate(
        date = as.Date.yearqtr(date),
        amount = log(amount),
        amount = amount - lag(amount, 1)
      ) |>
      select(date, amount) |>
      rename(y = amount) |>
      arrange(date) |>
      drop_na()

    # With normalization
    recipe_y <-
      recipe(x = df, roles = rep("predictor", ncol(df))) |>
      step_normalize(all_numeric_predictors()) |>
      prep()

    normalization_df_y <-
      tidy(recipe_y, number = 1)

    data_ml_cs <-
      recipe_y |>
      juice() |>
      full_join(
        proc_df_x,
        by = c("date")
      )

    train_cs <-
      data_ml_cs |>
      drop_na(y)

    test_cs <-
      data_ml_cs |>
      filter(
        date > train_cs |>
          pull(date) |>
          max()
      )

    # TS
    data_ml_ts <-
      agg_data_list[["y"]] |>
      filter(name == y) |>
      mutate(
        name = "y",
        amount = log(amount)
      ) |>
      pivot_wider(
        id_cols = date,
        names_from = name,
        values_from = amount
      ) |>
      mutate(date = as.Date.yearqtr(date)) |>
      full_join(
        proc_df_x,
        by = c("date")
      )

    train_ts <-
      data_ml_ts |>
      drop_na(y) |>
      select(date, y)

    test_ts <-
      data_ml_ts |>
      filter(
        date > train_ts |>
          pull(date) |>
          max()
      ) |>
      select(date, y)


    # Setting the recipes and Specifying the models ---------------------------
    recipes_list <- list()

    recipes_list[["CS"]][["ML"]] <-
      recipe(y ~ ., data = train_cs) |>
      step_rm("date")

    recipes_list[["TS"]][["ML"]] <-
      recipe(y ~ date, data = train_ts)

    # CS models
    models_list <- list()

    models_list[["CS"]][["Lasso"]] <-
      linear_reg(
        penalty = tune(),
        mixture = 1
      ) |>
      set_mode("regression") |>
      set_engine("glmnet")

    models_list[["CS"]][["Ridge"]] <-
      linear_reg(
        penalty = tune(),
        mixture = 0
      ) |>
      set_mode("regression") |>
      set_engine("glmnet")

    models_list[["CS"]][["ElasticNet"]] <-
      linear_reg(
        penalty = tune(),
        mixture = tune()
      ) |>
      set_mode("regression") |>
      set_engine("glmnet")

    models_list[["CS"]][["SupportVector"]] <-
      svm_rbf(
        cost = tune(),
        rbf_sigma = tune()
      ) |>
      set_mode("regression") |>
      set_engine("kernlab")

    models_list[["CS"]][["RandomForest"]] <-
      rand_forest(
        mtry = tune(),
        min_n = tune(),
        trees = tune(),
      ) |>
      set_mode("regression") |>
      set_engine("ranger")

    models_list[["CS"]][["XGBoost"]] <-
      boost_tree(
        mtry = tune(),
        trees = tune(),
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        sample_size = tune(),
        stop_iter = tune()
      ) |>
      set_mode("regression") |>
      set_engine("xgboost", validation = 0.2)

    models_list[["CS"]][["LightGBM"]] <-
      boost_tree(
        mtry = tune(),
        trees = tune(),
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        sample_size = tune(),
        stop_iter = tune()
      ) |>
      set_mode("regression") |>
      set_engine("lightgbm", validation = 0.2)

    # TS models
    models_list[["TS"]][["AR1"]] <-
      arima_reg(
        seasonal_period = 4,
        non_seasonal_ar = 1,
        non_seasonal_differences = 0,
        non_seasonal_ma = 0,
        seasonal_ar = 0,
        seasonal_differences = 0,
        seasonal_ma = 0
      ) |>
      set_mode("regression") |>
      set_engine("auto_arima")

    models_list[["TS"]][["AutoARIMA"]] <-
      arima_reg(seasonal_period = 4) |>
      set_mode("regression") |>
      set_engine("auto_arima")

    models_list[["TS"]][["ETS"]] <-
      exp_smoothing(seasonal_period = 4) |>
      set_mode("regression") |>
      set_engine("ets")

    models_list[["TS"]][["NNETAR"]] <-
      nnetar_reg(seasonal_period = 4) |>
      set_mode("regression") |>
      set_engine("nnetar")

    models_list[["TS"]][["Thief"]] <-
      temporal_hierarchy(seasonal_period = 4) |>
      set_mode("regression") |>
      set_engine("thief")


    # Tuning ------------------------------------------------------------------

    # Tune grid - CS
    print(glue("Model training:"))

    tune_start <- Sys.time()

    set.seed(seed = seed_number)

    all_workflows_cs <-
      workflow_set(
        preproc = recipes_list[["CS"]],
        models = models_list[["CS"]],
        cross = TRUE
      ) |>
      workflow_map(
        fn = "tune_grid",
        resamples =
          initial_validation_time_split(train_cs) |>
          validation_set(),
        seed = seed_number,
        grid = grid_size,
        metrics = metric_set(rmse),
        verbose = TRUE,
        control = control_grid(
          parallel_over = "everything",
          save_pred = TRUE,
          save_workflow = TRUE
        )
      )

    # Tune grid - TS
    set.seed(seed = seed_number)

    all_workflows_ts <-
      workflow_set(
        preproc = recipes_list[["TS"]],
        models = models_list[["TS"]],
        cross = TRUE
      ) |>
      workflow_map(
        fn = "fit_resamples",
        resamples =
          initial_validation_time_split(train_ts) |>
          validation_set(),
        seed = seed_number,
        grid = grid_size,
        metrics = metric_set(rmse),
        verbose = TRUE,
        control = control_grid(
          parallel_over = "everything",
          save_pred = TRUE,
          save_workflow = TRUE
        )
      )


    # Base forecasts ----------------------------------------------------------

    all_workflows <-
      list("CS" = all_workflows_cs,
           "TS" = all_workflows_ts)

    for (l in names(all_workflows)) {
      wflow_ids <- all_workflows[[l]] |> pull(wflow_id)

      for (w_id in wflow_ids) {
        label <- str_split(w_id, "_")[[1]][2]

        print(glue("Model prediction and fitted values: {label}"))

        # Extracting workflow, getting the best performing specification,
        # and finalizing model
        wf <-
          all_workflows[[l]] |>
          extract_workflow(w_id)

        lowest_rmse <-
          all_workflows[[l]] |>
          extract_workflow_set_result(w_id) |>
          select_best(metric = "rmse")

        if (l == "CS") {

          # Finalizing model
          model <-
            finalize_workflow(wf, lowest_rmse) |>
            fit(data = train_cs)

          # Actual y_t data
          y_vec <-
            agg_data_list[["y"]] |>
            filter(name == y) |>
            tail(1) |>
            pull(amount) |>
            log()

          # Predicted values
          pred_vec <-
            model |>
            predict(test_cs) |>
            pull(.pred)

          # Back transforming normalization
          mean <- normalization_df_y |> filter(statistic == "mean") |> pull(value)
          sd <- normalization_df_y |> filter(statistic == "sd") |> pull(value)

          pred_vec <- (pred_vec * sd) + mean

          # Back transforming differencing
          for (i in 1:length(pred_vec)) {
            y_vec <-
              c(
                y_vec,
                pred_vec[i] + (y_vec[length(y_vec)])
              )
          }

          forecast_list[[as.character(w)]][[label]][[y]] <-
            tibble(
              date = test_cs |> pull(date) |> as.yearqtr(),
              model = label,
              series = y,
              value = exp(y_vec[2:length(y_vec)])
            )

          # Fitted values

          # Actual y_t data
          y_vec <-
            agg_data_list[["y"]] |>
            filter(name == y) |>
            head(1) |>
            pull(amount) |>
            log()

          # Predicted values
          pred_vec <-
            model |>
            predict(train_cs) |>
            pull()

          # Back transforming normalization
          pred_vec <- (pred_vec * sd) + mean

          # Back transforming differencing
          for (i in 1:length(pred_vec)) {
            y_vec <-
              c(
                y_vec,
                pred_vec[i] + y_vec[length(y_vec)]
              )
          }

          fitted_list[[as.character(w)]][[label]][[y]] <-
            tibble(
              date = train_cs |> pull(date) |> as.yearqtr(),
              model = paste(label),
              series = y,
              value = exp(y_vec[2:length(y_vec)])
            )

          # Best configuration
          tidymodels[[label]][[y]][["best_config"]] <- lowest_rmse

          # Most important variables
          if (str_detect(label, "Lasso|Ridge|ElasticNet")) {
            tidymodels[[label]][[y]][["VIP"]] <-
              model |>
              extract_fit_parsnip() |>
              vip::vi(lambda = lowest_rmse |> pull(penalty)) |>
              mutate(Variable = fct_reorder(Variable, Importance)) |>
              filter(Importance != 0)
          } else if (str_detect(label, "RandomForest")) {
            tidymodels[[label]][[y]][["VIP"]] <-
              finalize_model(models_list[["CS"]][["RandomForest"]], lowest_rmse) |>
              set_engine("ranger", importance = "permutation") |>
              fit(y ~ ., data = prep(recipes_list[["CS"]][["ML"]]) |> juice()) |>
              vip::vi() |>
              mutate(Variable = fct_reorder(Variable, Importance)) |>
              filter(Importance != 0)
          } else if (str_detect(label, "XGBoost|LightGBM")) {
            tidymodels[[label]][[y]][["VIP"]] <-
              model |>
              extract_fit_parsnip() |>
              vip::vi() |>
              mutate(Variable = fct_reorder(Variable, Importance)) |>
              filter(Importance != 0)
          }
        } else if (l == "TS") {
          # Finalizing model
          model <-
            finalize_workflow(wf, lowest_rmse) |>
            fit(data = train_ts)

          # Prediction
          forecast_list[[as.character(w)]][[label]][[y]] <-
            model |>
            predict(test_ts) |>
            mutate(
              date = test_ts |> pull(date) |> as.yearqtr(),
              model = label,
              series = y,
              .pred = exp(.pred),
              .before = .pred
            ) |>
            rename(value = .pred)

          # Fitted values
          fitted_list[[as.character(w)]][[label]][[y]] <-
            model |>
            predict(train_ts) |>
            mutate(
              date = train_ts |> pull(date) |> as.yearqtr(),
              model = paste(label),
              series = y,
              .pred = exp(.pred),
              .before = .pred
            ) |>
            rename(value = .pred)
        }
      }
    }

    # Model stack -------------------------------------------------------------

    ## Stacks
    label <- glue("Model_Stack")
    print(label)

    # Stack
    set.seed(seed = seed_number)

    model_stack <-
      stacks() |>
      add_candidates(
        all_workflows_cs |>
          filter(!wflow_id %in% c("ML_LightGBM", "ML_SupportVector"))
      ) |>
      blend_predictions(
        # penalty = 10^seq(-10, -0.5, by = 0.25),
        mixture = 1,
        metric = metric_set(rmse),
        times = 10,
        control = control_grid(
          parallel_over = "everything",
          save_pred = TRUE,
          save_workflow = TRUE
        )
      ) |>
      fit_members()

    # If function to check whether there are any members in the Lasso
    # Stack. Only go ahead with predictions and fitted list,
    # if there are members.
    if (!is_empty(names(model_stack$member_fits))) {

      # Actual y_t data
      y_vec <-
        agg_data_list[["y"]] |>
        filter(name == y) |>
        tail(1) |>
        pull(amount) |>
        log()

      # Predicted values
      pred_vec <-
        model_stack |>
        predict(test_cs) |>
        pull(.pred)

      # Back transforming normalization
      mean <- normalization_df_y |> filter(statistic == "mean") |> pull(value)
      sd <- normalization_df_y |> filter(statistic == "sd") |> pull(value)

      pred_vec <- (pred_vec * sd) + mean

      # Back transforming differencing
      for (i in 1:length(pred_vec)) {
        y_vec <-
          c(
            y_vec,
            pred_vec[i] + y_vec[length(y_vec)]
          )
      }

      forecast_list[[as.character(w)]][[label]][[y]] <-
        tibble(
          date = test_cs |> pull(date) |> as.yearqtr(),
          model = label,
          series = y,
          value = exp(y_vec[2:length(y_vec)])
        )

      # Fitted values

      # Actual y_t data
      y_vec <-
        agg_data_list[["y"]] |>
        filter(name == y) |>
        head(1) |>
        pull(amount) |>
        log()

      # Predicted values
      pred_vec <-
        model_stack |>
        predict(train_cs) |>
        pull()

      # Back transforming normalization
      pred_vec <- (pred_vec * sd) + mean

      # Back transforming differencing
      for (i in 1:length(pred_vec)) {
        y_vec <-
          c(
            y_vec,
            pred_vec[i] + y_vec[length(y_vec)]
          )
      }

      fitted_list[[as.character(w)]][[label]][[y]] <-
        tibble(
          date = train_cs |> pull(date) |> as.yearqtr(),
          model = paste(label),
          series = y,
          value = exp(y_vec[2:length(y_vec)])
        )
    } else {
      print("Stack did not run.")
      stack_issues <- c(stack_issues, glue("Window {w}: {y}"))
    }

    tune_end <- Sys.time()
    tune_time <- tune_end - tune_start

    print(tune_time)
  }

  stopCluster(cl)

  # Taking the VIP Lasso variables to the series_id_list
  series_list <- list()

  for (y in y_vec_names) {
    series_list[[y]] <-
      tidymodels[["Lasso"]][[y]][["VIP"]] |>
      left_join(df_x[["Meta"]] |> select(id, name),
                by = c("Variable" = "name")
      ) |>
      drop_na(id) |>
      pull(id)
  }

  series_id_list[["DFM Lasso"]] <-
    unlist(series_list) |>
    unname() |>
    unique() |>
    sort()


  # DFM ---------------------------------------------------------------------

  print(glue("DFM"))

  dfm_list <- list()

  non_stationary_models <- c()

  dfm_vec <- names(series_id_list)

  for (dfm in dfm_vec) {
    print(glue("{dfm}"))

    selected_ids <-
      series_id_list[[dfm]] |>
      unique() |>
      setdiff(df_y[["Meta"]] |> pull(id))

    # Recipe
    df <-
      data_list[["x_tidy"]] |>
      filter(id %in% selected_ids) |>
      pivot_wider(
        id_cols = date,
        names_from = c(id, name),
        values_from = amount,
        names_sep = " "
      ) |>
      mutate(date = as.Date.yearmon(date))

    rec <-
      recipe(x = df, roles = rep("predictor", ncol(df))) |>
      step_rm(date) |>
      step_nzv(all_numeric_predictors())

    rec_object <- prep(rec)

    rec_df <- rec_object |> juice()

    start_date <- df |>
      head(1) |>
      pull(date)

    mat <-
      rec_df |>
      as.matrix() |>
      ts(start = c(year(start_date), month(start_date)), frequency = 12)

    # Stationary checks: Carries out ADF and KPSS tests at 10% and 1%
    # respectively. ADF has stationary as alternative, while KPSS has
    # stationarity as null hypothesis
    trans_vec <- c()
    nonstationary <- c()

    checked_id <- c()

    for (i in colnames(mat)) {
      diff_counter <- 1

      tseries <- mat[, i] |> na.trim()

      check_series <- sum(is.na(tseries))

      if (check_series > 0) {
        trans_vec <- c(trans_vec, diff_counter)
      } else {
        tseries <- tseries |> diff()

        test1 <- adf.test(tseries, k = 12)

        test1_pval <-
          ifelse(is.na(test1[["p.value"]]),
                 0.99, test1[["p.value"]]
          )

        result1 <-
          ifelse(test1_pval < 0.10,
                 "Stationary",
                 "Non-stationary"
          )

        test2 <- kpss.test(tseries)

        test2_pval <-
          ifelse(is.na(test2[["p.value"]]),
                 0,
                 test2[["p.value"]]
          )

        result2 <-
          ifelse(test2_pval > 0.01,
                 "Stationary",
                 "Non-stationary"
          )

        if (result1 == "Non-stationary" &&
            result2 == "Non-stationary") {
          diff_counter <- diff_counter + 1

          tseries <- tseries |> diff(lag = 12)

          test1 <- adf.test(tseries, k = 12)

          test1_pval <-
            ifelse(is.na(test1[["p.value"]]),
                   0.99,
                   test1[["p.value"]]
            )

          result1 <-
            ifelse(test1_pval < 0.10,
                   "Stationary",
                   "Non-stationary"
            )

          test2 <- kpss.test(tseries)

          test2_pval <-
            ifelse(is.na(test2[["p.value"]]),
                   0,
                   test2[["p.value"]]
            )

          result2 <-
            ifelse(test2_pval > 0.01,
                   "Stationary",
                   "Non-stationary"
            )

          if (result1 == "Non-stationary" &&
              result2 == "Non-stationary") {
            nonstationary <- c(nonstationary, i)
            trans_vec <- c(trans_vec, 1)
          } else {
            trans_vec <- c(trans_vec, diff_counter)
          }
        } else {
          trans_vec <- c(trans_vec, diff_counter)
        }
      }
    }

    trans_vec <- ifelse(trans_vec == 1, 2, 4)

    # Error if stationarity models don't pass both ADF and KPSS tests at 10%
    # and 1% respectively
    if (length(nonstationary) > 0) {
      print(
        glue(
          "Some of the series in {dfm} are not stationary. Please check to be sure."
        ) |>
          as.character()
      )

      non_stationary_models <-
        c(
          non_stationary_models,
          glue("{dfm}") |> as.character()
        )
    }

    ## Prepring x
    # Doing balanced panel and carrying out the transformations to make the
    # series stationary
    x <-
      Bpanel(
        mat,
        trans = trans_vec,
        aggregate = TRUE,
        h = 0,
        NA.replace = TRUE
      )

    # Cleaning variables
    nan_col_sums <- x |>
      scale() |>
      is.nan() |>
      colSums()
    issues <- nan_col_sums[nan_col_sums > 0] |> names()

    x <- x[, (which(!colnames(mat) %in% as.character(issues)))]

    # Running PCA to determine specific parameters
    pca <- x |>
      scale() |>
      prcomp()

    var_vec <- pca[["sdev"]]^2

    importance_df <-
      tibble(
        "factors" = seq_along(var_vec),
        "variance" = var_vec,
        "proportion" = variance / sum(var_vec)
      ) |>
      mutate(cumulative = cumsum(proportion))

    # Exporting importance dataframe
    if (str_detect(dfm, "Combined")) {
      label <- "DFM Combined"
    } else {
      label <- dfm
    }

    dfm_list[[label]][["PCA"]] <- importance_df

    # Using the importance df, based on two criteria, we select two
    # factor numbers

    # Criteria 1: Checking for the first factor number that explains 90% of
    # the total variation
    r1 <- importance_df |>
      filter(cumulative >= 0.9) |>
      head(1) |>
      pull(factors)

    # Criteria 2: Checking the factor number that explains more than 5% of
    # the variation
    r2 <- importance_df |>
      filter(proportion >= 0.05) |>
      tail(1) |>
      pull(factors)

    # Criteria 3: Kaiser's rule. All factors with a variance greater than 1.
    r3 <- importance_df |>
      filter(variance >= 1) |>
      tail(1) |>
      pull(factors)

    # Taking the unique numbers and the minimum factors (to be parsimonious)
    # (Greater than 1)
    factor_number <- c(r1, r2, r3)

    factor_number <-
      factor_number[factor_number > 1] |>
      unique() |>
      min()

    # Checking the optimal specification based on factors

    # Taking the mean of the lags recommended by difference
    # Information Criteria
    if (dfm == "DFM Lasso") {
      var_select <-
        pca[["rotation"]][, 1:factor_number] |>
        VARselect(lag.max = factor_number)
    } else {
      var_select <-
        pca[["rotation"]][, 1:factor_number] |>
        VARselect()
    }

    # var_select <-
    #   pca[["rotation"]][, 1:factor_number] |>
    #   VARselect(lag.max = factor_number)

    lags <-
      var_select[["selection"]] |>
      as.numeric() |>
      mean() |>
      round(digits = 0)

    shocks <-
      ICshocks(
        x = x[complete.cases(x), ],
        r = factor_number,
        p = lags
      )[["q_star"]]

    ## Preparing y and forecasting
    y_vec <- agg_mat_list[["y"]] |> colnames()

    for (y_i in y_vec) {
      y <-
        agg_mat_list[["y"]][, as.character(y_i)] |>
        diff() |>
        qtr2month()

      y_name <-
        df_y[["Meta"]] |>
        filter(id == y_i) |>
        pull(name)

      print(glue("DFM - {dfm} - {y_name}"))

      # Preparing the data for nowcasting
      data <- cbind(y, x)
      freq <- c(4, rep(12, ncol(x)))

      ## Nowcasting
      nowcast_model <-
        nowcast(
          formula = y ~ .,
          data = data,
          r = factor_number,
          q = shocks,
          p = lags,
          method = "2s_agg",
          frequency = freq
        )

      # Calculating both in-sample and out-of-sample level forecasts
      fc_vec <-
        c(
          nowcast_model[["yfcst"]][, "in"] |>
            na.omit(),
          nowcast_model[["yfcst"]][, "out"][2:length(nowcast_model[["yfcst"]][, "out"])] |>
            na.omit()
        )

      final_vec <-
        head(agg_mat_list[["y"]][, y_i], 1) |> as.numeric()

      for (i in seq_along(fc_vec)) {
        base_value <- tail(final_vec, 1)
        fc_value <- fc_vec[i]

        level_fc <- base_value + fc_value

        final_vec <- c(final_vec, level_fc)
      }

      ## Creating relevant tibbles and attaching to lists
      train_start_time <-
        agg_mat_list[["y"]][, y_i] |>
        time() |>
        min() |>
        as.yearqtr()

      test_start_time <-
        (agg_mat_list[["y"]][, y_i] |>
           time() |>
           max() |>
           as.yearqtr()) + 1 / 4

      in_sample_fc <-
        final_vec[seq_along(agg_mat_list[["y"]][, y_i])] |>
        ts(
          start = c(
            year(train_start_time),
            quarter(train_start_time)
          ),
          frequency = 4
        )

      out_of_sample_fc <-
        final_vec[(length(agg_mat_list[["y"]][, y_i]) + 1):length(final_vec)] |>
        ts(
          start = c(
            year(test_start_time),
            quarter(test_start_time)
          ),
          frequency = 4
        )

      # Prediction
      forecast_list[[as.character(w)]][[label]][[y_name]] <-
        tibble(
          date = time(out_of_sample_fc) |> as.Date() |> as.yearqtr(),
          model = label,
          series = y_name,
          value = as.numeric(out_of_sample_fc)
        )

      fitted_list[[as.character(w)]][[label]][[y_name]] <-
        tibble(
          date = time(in_sample_fc) |> as.Date() |> as.yearqtr(),
          model = label,
          series = y_name,
          value = as.numeric(in_sample_fc)
        )

      # Specification info
      dfm_list[[label]][[y_name]][["Specification"]] <-
        tibble(
          "dataset" = label,
          "series" = y_name,
          "factor_number" = factor_number,
          "lag" = lags,
          "shocks" = shocks
        )
    }
  }


  # Reconciliation ----------------------------------------------------------

  print(glue("Reconciliation"))

  reconciled_fc <-
    reconcile(
      r_forecast_list = forecast_list[[as.character(w)]],
      r_fitted_list = fitted_list[[as.character(w)]]
    )


  # Tidying forecast list and exporting it to out of the loop ---------------

  experiment_list[[as.character(w)]] <-
    bind_rows(
      unlist(forecast_list[[as.character(w)]],
             recursive = FALSE
      ) |>
        bind_rows(),
      reconciled_fc
    ) |>
    group_by(model, series) |>
    mutate(h = row_number()) |>
    ungroup() |>
    mutate(window = w) |>
    rename(forecast = value) |>
    left_join(
      test_y |> select(!id) |> rename(actual = amount),
      by = c(
        "date" = "date",
        "series" = "name"
      )
    ) |>
    drop_na(actual)

  # Checking time and updating
  window_end <- Sys.time()

  window_time <- window_end - window_start

  print(glue("Time for window {w}: {format(window_time)}"))

  window_times <-
    window_times |>
    add_row(
      window = w,
      time = as.numeric(window_time)
    )
}

end <- Sys.time()
print(end - start)

first_estimate <- seq(1, windows, by = 3)
second_estimate <- seq(2, windows, by = 3)
third_estimate <- seq(3, windows, by = 3)

order <- df_y[["Meta"]] |> pull(name)

initial_experiment_df <-
  bind_rows(experiment_list) |>
  mutate(
    error = actual - forecast,
    estimate =
      ifelse(window %in% first_estimate, 1,
             ifelse(window %in% second_estimate, 2,
                    ifelse(window %in% third_estimate, 3, NA)
             )
      ),
    padded = ifelse(estimate == 3, "not_padded", "padded")
  )

# Checking with truncated experiment window
# initial_experiment_df <-
#   initial_experiment_df |>
#   filter(window >= 49 & padded == "not_padded")


# Exporting data ----------------------------------------------------------

save(list = ls(), file = "Experiment.rdata")

load("Experiment.rdata")


# Custom reconciliation combination 1 -------------------------------------

best_model_list <- list()

selected_windows_back <- 12

# All windows are used - padded or not
# Dynamically looking back for 12 months/windows

best_model_list[["Combination_1"]] <-
  best_model_selection(
    df = initial_experiment_df,
    h_steps = 1,
    windows_back = selected_windows_back,
    pad = "all",
    combination_name = "Combination_1"
  )


# Custom reconciliation combination 2 -------------------------------------

# Only the windows that aren't padded are taken
# Dynamically looking back for 12 months/windows

best_model_list[["Combination_2"]] <-
  best_model_selection(
    df = initial_experiment_df,
    h_steps = 1,
    windows_back = selected_windows_back,
    pad = "not_padded",
    combination_name = "Combination_2"
  )

# Custom reconciliation combination 3 -------------------------------------

# All windows

best_model_list[["Combination_3"]] <-
  best_model_selection(
    df = initial_experiment_df,
    h_steps = 1,
    windows_back = "all",
    pad = "all",
    combination_name = "Combination_3"
  )


# Custom reconciliation combination 4 -------------------------------------

# All non-padded windows

best_model_list[["Combination_4"]] <-
  best_model_selection(
    df = initial_experiment_df,
    h_steps = 1,
    windows_back = "all",
    pad = "not_padded",
    combination_name = "Combination_4"
  )

# Reconciliation combinations ---------------------------------------------

custom_reconciliation_list <- list()

for (i in names(best_model_list)) {
  print(i)

  custom_reconciliation_list[[i]] <-
    reconciliation_combination(
      best_models_df = best_model_list[[i]],
      forecast_list = forecast_list,
      fitted_list = fitted_list
    )
}


# Binding initial experiments and custom combinations ---------------------

custom_reconciliation_df <-
  map(
    .x = custom_reconciliation_list,
    .f = `[[`, "Reconciled"
  ) |>
  bind_rows()

# Checking the windows for which the combinations were run. This will be
# based on the number of windows we looked back in the combination
# reconciliation function.
starting_window <-
  custom_reconciliation_df |>
  group_by(model) |>
  slice_min(window) |>
  distinct(window) |>
  pull(window) |>
  max()

# Binding it all together and filtering to get the final models and series
experiment_df <-
  bind_rows(
    initial_experiment_df,
    custom_reconciliation_df
  ) |>
  filter(window >= starting_window)


# Exporting additional data -----------------------------------------------

# Final experiment file
write_parquet(
  x = experiment_df,
  sink = "final_experiment_file.parquet"
)

experiment_df <- read_parquet("final_experiment_file.parquet")

# Best models
for (i in names(best_model_list)) {
  write_parquet(
    x = best_model_list[[i]],
    sink = glue("Best models/{i}.parquet")
  )
}

# Session info
sessionInfo() |> capture.output(file = "session_info.txt")


# Read best models --------------------------------------------------------

best_models_list <- list()

model_names <-
  list.files(path = "Best models/") |>
  str_remove_all(".parquet")

for (i in model_names) {
  best_models_list[[i]] <-
    read_parquet(
      file = glue("Best models/{i}.parquet")
    )
}


# Results - RMSE and MAPE -------------------------------------------------

# experiment_df <- initial_experiment_df

# All series
rmse_all <-
  experiment_df |>
  filter(h == 1) |>
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

# Top level
rmse_top <-
  experiment_df |>
  filter(series == "GDP at market price" & h == 1) |>
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

# Bottom level
rmse_bottom <-
  experiment_df |>
  filter(series != "GDP at market price" & h == 1) |>
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

print(rmse_all)
print(rmse_top)
print(rmse_bottom)

rmse_all |>
  pull(mape) |>
  summary()

error_table <-
  experiment_df |>
  filter(h == 1 & !str_detect(model, "Rec") & window >= 49) |>
  group_by(model, series) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  group_by(series) |>
  slice_min(order_by = rmse) |>
  arrange(match(series, order))

print(error_table)

error_table |>
  pull(mape) |>
  summary()

experiment_df |>
  filter(h == 1 & window >= 49) |>
  group_by(model, series) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  filter(model == "Combination_3 Rec_BU") |>
  arrange(match(series, order))

experiment_df |>
  filter(padded == "not_padded" & h == 1 & !str_detect(model, "Rec")) |>
  mutate(perc_error = abs(error)/actual * 100) |>
  group_by(window, series) |>
  slice_min(perc_error) |>
  arrange(series, date) |>
  ggplot() +
  geom_text(
    aes(
      x = as.Date.yearmon(date),
      y = perc_error,
      color = model,
      label = model
    ),
    size = 3,
    angle = 90
  ) +
  facet_wrap(series ~ ., scales = "free")

custom_reconciliation_list$Combination_1$`Best models` |>
  left_join(
    df_x[["Data"]] |>
      distinct(date) |>
      filter(date > end_train & date <= max_date) |>
      mutate(date = date + 1/12,
             window = row_number()),
    by = "window"
  ) |>
  filter(
    window %in% third_estimate
  ) |>
  ggplot() +
  geom_point(
    aes(
      x = date,
      y = rmse,
      color = model
    )
  ) +
  facet_wrap(series ~ ., scales = "free")


rank_df <-
  experiment_df |>
  filter(!str_detect(model, "Rec") & h == 1) |>
  arrange(window, series, abs(error)) |>
  group_by(series, window) |>
  mutate(rank = row_number()) |>
  select(window, model, series, padded, rank)

avg_rank_df <-
  rank_df |>
  group_by(model, series) |>
  summarise(rank = mean(rank), .groups = "drop") |>
  group_by(series) |>
  slice_min(rank) |>
  arrange(match(series, order))

