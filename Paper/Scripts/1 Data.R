
# Nowcasting: Data

# Import any data and do any cleaning that's required.

source("0 Functions.R")

# Query database ----------------------------------------------------------

# Independent variables
full_inputs <-
  read_excel("Full Inputs.xlsx", sheet = "Indep vars") |>
  mutate(Type = replace(Type, is.na(Type), "Subseries"))

ids <- full_inputs |>
  pull(`Series ID`) |>
  unique()

df_x <- query_db(series_id = ids)

df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  select(!c(converted_frequency, weight))

# Dependent variables
y_vars <-
  read_excel("Full Inputs.xlsx", sheet = "Dep vars") |>
  mutate(Type = replace(Type, is.na(Type), "Subseries"))

ids <- y_vars |>
  pull(`Series ID`) |>
  unique()

df_y <- query_db(series_id = ids)

# Adding in back series for GDP using estimation
# Estimation done by using the ratio of rebased QNA (base year = 2019)
# and QNA (base year = 2014). Also ensured the quarterly numbers summed up to
# annual GDP (base year = 2019).

y_back_series <-
  read_excel(
    "QNA rebased/QNA_backseries - Denton.xlsx",
    sheet = "QNA backseries - Rebased",
    col_names = FALSE,
    skip = 6,
    n_max = 21
  )

cleaned_back_series_list <- list()

for (i in 1:nrow(y_back_series)) {

  series_name <-
    y_back_series |>
    slice(i) |>
    select(2) |>
    pull()

  series_id <-
    df_y[["Meta"]] |>
    filter(name == series_name) |>
    pull(id)

  series <-
    y_back_series |>
    slice(i) |>
    select(3:ncol(y_back_series)) |>
    as.numeric() |>
    ts(start = c(2003, 1), frequency = 4)

  cleaned_back_series_list[[i]] <-
    tibble(
      "date" = time(series) |> as.yearqtr(),
      "id" = series_id,
      "name" = series_name,
      "amount" = series * 1000000
    )
}

df_y[["Data"]] <-
  bind_rows(
    x = bind_rows(cleaned_back_series_list),
    y = df_y[["Data"]]
  ) |>
  arrange(
    match(name, pull(df_y[["Meta"]], name)),
    date
  )

# Cleaning of Query database
df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  mutate(conversion_method = replace(conversion_method, id == 4061, "avg"))

missing_cm <- df_x[["Meta"]] |>
  filter(is.na(conversion_method)) |>
  nrow()

if (missing_cm > 0) {
  stop("conversion_method missing.")
  df_x[["Meta"]] |> filter(is.na(conversion_method))
}

# Name changes
ids <- full_inputs |>
  distinct(`Series ID`) |>
  pull()

for (i in ids) {
  new_name <- full_inputs |>
    filter(`Series ID` == i) |>
    pull(Name)

  df_x[["Data"]] <- df_x[["Data"]] |>
    mutate(name = replace(name, id == i, new_name))

  df_x[["Meta"]] <- df_x[["Meta"]] |>
    mutate(name = replace(name, id == i, new_name))
}


# Additional data sources -------------------------------------------------

# EIA oil prices
token_eia <- read_lines("config_eia.txt")

eia_header <-
  c('{"frequency":"monthly","data": ["value"],"facets":{"seriesId": ["BREPUUS","WTIPUUS"]}}')

eia_data <-
  request(glue("https://api.eia.gov/v2/steo/data?api_key={token_eia}")) |>
  req_headers('X-Params' = eia_header) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

df_x[["Data"]] <-
  bind_rows(
    df_x[["Data"]],
    eia_data[["response"]][["data"]] |>
      as_tibble() |>
      select(period, value) |>
      rename(
        date = period,
        amount = value
      ) |>
      mutate(amount = as.numeric(amount)) |>
      group_by(date = as.yearmon(date, format = "%Y-%m")) |>
      summarise(amount = mean(amount)) |>
      mutate(
        id = 10000,
        name = "Crude oil prices", .before = amount
      )
  )

df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  add_row(
    id = 10000,
    name = "Crude oil prices",
    original_frequency = "Monthly",
    conversion_method = "avg"
  )

# IMF World - Primary Commodity Prices

# Use to get database names and info
# database_list <-
#   request("http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow") |>
#   req_perform() |>
#   resp_body_json(simplifyVector = TRUE)
#
# database_list[["Structure"]][["Dataflows"]][["Dataflow"]] |> View()

imf_api_data <-
  request("http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/M..PALLFNF+PEXGALL+PFANDB+PINDU.IX?startPeriod=2003") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

imf_commodities_order <- imf_api_data$CompactData$DataSet$Series$`@COMMODITY`

id_counter <- 10001

clean_api_list <- list()

for (i in seq_along(imf_commodities_order)) {
  # Retrieving name of commodity index
  name <- imf_commodities_order[i]

  # Retrieving dataframe and tidying
  df <-
    imf_api_data$CompactData$DataSet$Series$Obs[[i]] |>
    as_tibble()

  colnames(df) <- c("date", "amount")

  # Meta
  clean_api_list[["Meta"]][[name]] <-
    tibble(
      id = id_counter,
      name = name,
      original_frequency = "Monthly",
      conversion_method = "avg"
    )

  # Data
  clean_api_list[["Data"]][[name]] <-
    df |>
    mutate(
      date = as.yearmon(date, format = "%Y-%m"),
      id = id_counter,
      name = name,
      amount = as.numeric(amount)
    ) |>
    relocate(id, name, .before = amount)

  # Iterating the id counter
  id_counter <- id_counter + 1
}

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    bind_rows(clean_api_list[["Meta"]])
  )

df_x[["Data"]] <-
  bind_rows(
    df_x[["Data"]],
    bind_rows(clean_api_list[["Data"]])
  )


# Creating additional series ----------------------------------------------

id_counter <-
  df_x[["Meta"]] |>
  filter(id >= 10000) |>
  pull(id) |>
  max()

# Construction-related items
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id %in% c(3501, 3502, 3509, 3513, 3514, 3525)) |>
  # filter(id %in% c(3509, 3513, 3514, 3525)) |>
  group_by(date) |>
  summarise(amount = sum(amount), .groups = "drop") |>
  mutate(
    id = 10005,
    name = "Imports_Construction-related",
    .before = amount
  ) |>
  bind_rows(df_x[["Data"]])

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    tibble(
      id = 10005,
      name = "Imports_Construction-related",
      original_frequency = "Monthly",
      conversion_method = "sum"
    )
  )

# Trade-related items
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id %in% c(3457, 3509, 3513, 3514, 3525)) |>
  pivot_wider(id_cols = date, names_from = id, values_from = amount) |>
  mutate(amount = `3457` - (`3509` + `3513` + `3514` + `3525`)) |>
  select(date, amount) |>
  mutate(
    id = 10006,
    name = "Imports_Trade-related",
    .before = amount
  ) |>
  bind_rows(df_x[["Data"]])

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    tibble(
      id = 10006,
      name = "Imports_Trade-related",
      original_frequency = "Monthly",
      conversion_method = "sum"
    )
  )

# Salaries, wages and allowances
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id %in% c(2037, 2038)) |>
  group_by(date) |>
  summarise(amount = sum(amount), .groups = "drop") |>
  mutate(
    id = 10007,
    name = "GFS_Salaries_wages_allowances",
    .before = amount
  ) |>
  bind_rows(df_x[["Data"]])

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    tibble(
      id = 10007,
      name = "GFS_Salaries_wages_allowances-related",
      original_frequency = "Monthly",
      conversion_method = "sum"
    )
  )

# Average loans and advances
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id == 2458) |>
  mutate(
    id = 10008,
    name = paste0(name, "_avg")
  ) |>
  bind_rows(a = df_x[["Data"]], b = _)

df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  filter(id == 2458) |>
  mutate(
    id = 10008,
    name = paste0(name, "_avg"),
    conversion_method = "avg"
  ) |>
  bind_rows(a = df_x[["Meta"]], b = _)

# Average deposits
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id == 2355) |>
  mutate(
    id = 10009,
    name = paste0(name, "_avg")
  ) |>
  bind_rows(a = df_x[["Data"]], b = _)

df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  filter(id == 2355) |>
  mutate(
    id = 10009,
    name = paste0(name, "_avg"),
    conversion_method = "avg"
  ) |>
  bind_rows(a = df_x[["Meta"]], b = _)

# Import duty + GST - Subsidies
df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id %in% c(2001, 2007, 2053)) |>
  pivot_wider(
    id_cols = "date",
    names_from = "name",
    values_from = "amount"
  ) |>
  mutate(GFS_impduty_gst_subsidies =
           `GFS_Import duty` +
           `GFS_Goods and services tax` -
           GFS_Subsidies,
         id = 10010) |>
  select(date, id, GFS_impduty_gst_subsidies) |>
  pivot_longer(
    cols = !c(date, id),
    names_to = "name",
    values_to = "amount"
  ) |>
  bind_rows(a = df_x[["Data"]], b = _)

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    tibble(
      id = 10010,
      name = "GFS_impduty_gst_subsidies",
      original_frequency = "Monthly",
      conversion_method = "sum"
    )
  )


# Deflating nominal variables ---------------------------------------------

deflate_ids <-
  df_x[["Meta"]] |>
  filter(str_detect(name, "^GFS_") |
    str_detect(name, "^Financial_") |
    (str_detect(name, "(E|e)xp|(I|i)mp") &
       !str_detect(name, "^Fish exports") &
       !str_detect(name, "^FishExp"))) |>
  pull(id)

deflate_df <-
  left_join(
    df_x[["Data"]] |>
      filter(id %in% deflate_ids),
    df_x[["Data"]] |>
      filter(id == 280) |>
      select(date, amount) |>
      rename(cpi = amount),
    by = c("date")
  ) |>
  rename(
    old_id = id,
    nominal = amount
  ) |>
  mutate(
    amount = nominal / cpi * 100,
    id = old_id + 20000,
    name = paste(name, "cpi deflated")
  )

df_x[["Data"]] <-
  deflate_df |>
  select(date, id, name, amount) |>
  bind_rows(df_x[["Data"]])

df_x[["Meta"]] <-
  left_join(
    deflate_df |>
      select(id, old_id, name) |>
      distinct(),
    df_x[["Meta"]] |>
      select(id, original_frequency, conversion_method),
    by = c("old_id" = "id")
  ) |>
  select(!old_id) |>
  bind_rows(df_x[["Meta"]])


# Special deflations ------------------------------------------------------

df_x[["Data"]] <-
  df_x[["Data"]] |>
  filter(id %in% c(10005, 10004)) |>
  pivot_wider(id_cols = date, names_from = id, values_from = amount) |>
  mutate(amount = `10005` / `10004` * 100) |>
  select(date, amount) |>
  filter(!is.na(amount)) |>
  mutate(
    id = 40001,
    name = "Imports_Construction-related PINDU deflated",
    .before = amount
  ) |>
  bind_rows(df_x[["Data"]])

df_x[["Meta"]] <-
  bind_rows(
    df_x[["Meta"]],
    tibble(
      id = 40001,
      name = "Imports_Construction-related PINDU deflated",
      original_frequency = "Monthly",
      conversion_method = "sum"
    )
  )


# Cleaning df_x -----------------------------------------------------------

clean_names_df <-
  df_x[["Data"]] |>
  distinct(id, name) |>
  mutate(amount = 0) |>
  pivot_wider(id_cols = id, names_from = name, values_from = amount) |>
  clean_names() |>
  pivot_longer(cols = !c(id), names_to = "name", values_to = "amount") |>
  filter(!is.na(amount)) |>
  select(!amount)

df_x[["Data"]] <-
  df_x[["Data"]] |>
  rename(old_name = name) |>
  left_join(
    clean_names_df,
    by = "id"
  ) |>
  select(date, id, name, amount) |>
  arrange(id, date) |>
  filter(!is.na(amount))

df_x[["Meta"]] <-
  df_x[["Meta"]] |>
  rename(old_name = name) |>
  left_join(
    clean_names_df,
    by = "id"
  ) |>
  select(id, name, original_frequency, conversion_method) |>
  arrange(id)


# Creating dependent variables --------------------------------------------

aggregation_list <-
  list(
    "Construction and real estate" = c(90, 96),
    "Public administration, health and education" = c(98, 99, 100),
    "Miscellaneous" = c(85, 88, 89, 97, 101)
  )

for (i in names(aggregation_list)) {
  id_vec <- aggregation_list[[i]]

  # Meta data
  df_y[["Meta"]] <-
    df_y[["Meta"]] |>
    mutate(
      id = replace(id, id %in% id_vec, id_vec[length(id_vec)]),
      name = replace(name, id %in% id_vec, i)
    )

  # Data
  df_y[["Data"]] <-
    df_y[["Data"]] |>
    mutate(
      id = replace(id, id %in% id_vec, id_vec[length(id_vec)]),
      name = replace(name, id %in% id_vec, i)
    )
}

df_y[["Meta"]] <-
  df_y[["Meta"]] |>
  distinct() |>
  arrange(id)

df_y[["Data"]] <-
  df_y[["Data"]] |>
  group_by(date, id, name) |>
  summarise(amount = sum(amount), .groups = "drop")

y_vec <-
  df_y[["Data"]] |>
  distinct(id) |>
  pull()


# Creating series ID lists ------------------------------------------------

# This is used to specify different groups of series IDs which will be used
# to run DFMs. The list would include:
# 1. All series
# 2. All series without the deflated series
# 3. All series without the non-deflated/nominal series

# Nominal and real variables - no deflations
nominal_real_vec <-
  df_x[["Meta"]] |>
  filter(id <= 11000) |>
  pull(id)

# Deflated variables
deflated_vec <-
  df_x[["Meta"]] |>
  filter(id >= 20000) |>
  pull(id)

deflated_name_vec <-
  df_x[["Meta"]] |>
  filter(id %in% deflated_vec) |>
  mutate(
    name =
      str_remove(
        name,
        "_cpi_deflated|_PINDU_deflated"
      ) |>
        str_squish()
  ) |>
  pull(name) |>
  unique()

# Nominal variables
nominal_vec <-
  df_x[["Meta"]] |>
  filter(name %in% deflated_name_vec) |>
  pull(id)

# Real variables
real_only_vec <-
  df_x[["Meta"]] |>
  filter(!id %in% nominal_vec) |>
  pull(id)

# Full series
full_series_id_vec <- df_x[["Meta"]] |>
  distinct(id) |>
  pull()

# Series ID lists
series_id_list <- list()

series_id_list[["Combined"]] <- c(full_series_id_vec, y_vec) |> unique()
series_id_list[["DFM1"]] <- c(nominal_real_vec, y_vec) |> unique()
series_id_list[["DFM2"]] <- c(real_only_vec, y_vec) |> unique()

# Save Data
save(list = ls(), file = "Data.rdata")
