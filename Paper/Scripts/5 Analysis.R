source("0 Functions.R")
load("Data.rdata")


experiment_df <- read_parquet("final_experiment_file.parquet")

# Results - RMSE and MAPE -------------------------------------------------

# All series --------------------------------------------------------------


# padded and not padded
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

write.xlsx(rmse_all,
           file = "rmse_all.xlsx")


# not padded
rmse_all_not_padded <-
  experiment_df |>
  filter(h == 1) |>
  filter(padded == "not_padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

write.xlsx(rmse_all_not_padded,
           file = "rmse_all_not_padded.xlsx")


# padded
rmse_all_padded <-
  experiment_df |>
  filter(h == 1) |>
  filter(padded == "padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

write.xlsx(rmse_all_padded,
           file = "rmse_all_padded.xlsx")



# RGDP --------------------------------------------------------------------


# both padded and not padded
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

write.xlsx(rmse_top, 
           file = "rmse_top.xlsx")


# not_padded
rmse_top_not_padded <-
  experiment_df |>
  filter(series == "GDP at market price" & h == 1) |>
  filter(padded == "not_padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

write.xlsx(rmse_top_not_padded, 
           file = "rmse_top_not_padded.xlsx")

# padded
rmse_top_padded <-
  experiment_df |>
  filter(series == "GDP at market price" & h == 1) |>
  filter(padded == "padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)


write.xlsx(rmse_top_padded,
           file = "rmse_top_padded.xlsx")



# Bottom series -----------------------------------------------------------

# both padded and not padded
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

write.xlsx(rmse_bottom,
           file = "rmse_bottom.xlsx")

# not padded

rmse_bottom_not_padded <-
  experiment_df |>
  filter(series != "GDP at market price" & h == 1) |>
  filter(padded == "not_padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

write.xlsx(rmse_bottom_not_padded,
           file = "rmse_bottom_not_padded.xlsx")


rmse_bottom_padded <-
  experiment_df |>
  filter(series != "GDP at market price" & h == 1) |>
  filter(padded == "padded") |> 
  group_by(model) |>
  summarise(
    mape = mean(abs(error / actual * 100)),
    rmse = sqrt(mean(error^2)),
    .groups = "drop"
  ) |>
  arrange(rmse)

write.xlsx(rmse_bottom_padded,
           file = "rmse_bottom_padded.xlsx")



# Best model --------------------------------------------------------------


best_model <- read_parquet("Best models/Combination_5.parquet")

comb_5_BU  <- experiment_df |>
  filter(model == "Combination_5 Rec_BU") |>
  filter(h == 1 & padded == "not_padded") 

write.xlsx(comb_5_BU,
           file = "comb5_BU.xlsx")


# Chart -------------------------------------------------------------------

order <- df_y[["Meta"]] |> pull(name) |> str_wrap(width = 25)

chart_df <-
  experiment_df |> 
  filter(model == "Combination_5 Rec_BU" & h == 1 & padded == "not_padded") |> 
  select(date, series, forecast, actual) |> 
  rename(nowcast = forecast) |> 
  pivot_longer(cols = !c(date, series), names_to = "type", values_to = "value") |> 
  mutate(series = str_wrap(series, width = 25),
         series = factor(series, levels = order),
         value = value/1000000)

chart_df |> 
  filter(series != "GDP at market price") |> 
  ggplot() +
  geom_line(aes(x = as.Date.yearqtr(date), y = value, color = type)) +
  facet_wrap(. ~ series, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_color_fivethirtyeight() +
  theme(legend.title = element_blank())

chart_df |>
  filter(series == "GDP at market price") |>
  ggplot() +
  geom_line(aes(x = as.Date.yearqtr(date), y = value, color = type)) +
  scale_y_continuous(labels = scales::comma) +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_color_fivethirtyeight() +
  theme(legend.title = element_blank())

