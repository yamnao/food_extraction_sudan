## Clean Item price data
lentils_prices_data <- rio::import('data/clean_item_price_time.xlsx') |>
  dplyr::filter(type_of_food %in% c('lentils')) |>
  dplyr::filter(kitchen_locality == 'Jebel Awlia') |>
  dplyr::select(date, price)|>
  dplyr::mutate(date = as.Date(paste(lubridate::year(date),
                                     lubridate::month(date),
                                     lubridate::day(date), sep='-'), 
                               format = '%Y-%m-%d')) |>
  dplyr::mutate(price = price/100)

rice_prices_data <- rio::import('data/clean_item_price_time.xlsx') |>
  dplyr::filter(type_of_food %in% c('rice')) |>
  dplyr::filter(kitchen_locality == 'Jebel Awlia') |>
  dplyr::select(date, price)|>
  dplyr::mutate(price = price/100)

## Attendances data
attendances <- rio::import('data/attendances_completed_imputed.xlsx') |>
  dplyr::filter(stringr::str_detect(kitchen_code, "JA")) |>
  dplyr::mutate(
    date = as.Date(date),
    Year = year(date),
    Month = month(date),
    HalfMonth = if_else(day(date) <= 15, 1, 2),
    period_date = as.Date(paste0(
      if_else(HalfMonth == 1, "01", "15"), "-", 
      sprintf("%02d", Month), "-", 
      Year
    ), format = "%d-%m-%Y")
  ) |>
  dplyr::group_by(period_date) |>
  dplyr::summarise(attendances = sum(est_benef, na.rm = TRUE), .groups = "drop") |>
  dplyr::rename(date = period_date)

##Review date
rice_prices_data <- rice_prices_data |>
  dplyr::mutate(date = attendances$date)

lentils_prices_data <- lentils_prices_data |>
  dplyr::mutate(date = attendances$date)

## Funding per time
funding_time_data <- rio::import('data/funding_stats.xlsx') |>
  dplyr::select(mean, ci_upper, ci_lower, date) |>
  dplyr::rename(funding_mean = mean, 
                funding_ci_upper = ci_upper, 
                funding_ci_lower = ci_lower) |>
  dplyr::mutate(funding_mean = funding_mean/1000,
                funding_ci_upper = funding_ci_upper/1000, 
                funding_ci_lower = funding_ci_lower/1000)

## kcal per time
## Then we can add the data from the community kitchens 
kcal_kitchens <- rio::import('data/final_nut_val_for_malnutrition.xlsx') |>
  dplyr::mutate(year = 2024) |>
  dplyr::select(Month, year, scen_0_mean_tot_cal_per_p, scen_0_cal_lower_ci, scen_0_cal_upper_ci, half_date) |>
  dplyr::mutate(date = as.Date(half_date)) |>
  dplyr::rename(mean_kcal = scen_0_mean_tot_cal_per_p, 
                mean_low_ci_kcal = scen_0_cal_lower_ci, 
                mean_up_ci_kcal = scen_0_cal_upper_ci) |>
  dplyr::select(date, mean_kcal, mean_low_ci_kcal, mean_up_ci_kcal)

##--- Merge everything on the date ---##

combined <- lentils_prices_data |>
  #full_join(lentils_prices_data, by = "date") |>
  full_join(rice_prices_data, by = "date") |>
  full_join(funding_time_data, by = "date") |>
  full_join(kcal_kitchens, by = "date") |>
  arrange(date)

##--- Reshape to long format for plotting ---##

combined_long <- combined |>
  tidyr::pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

##--- Plot ---##

ggplot(combined_long, aes(x = date, y = value, color = variable)) +
  geom_line(na.rm = TRUE, linewidth = 1) +
  theme_minimal() +
  labs(title = "Attendances, Prices, Funding, and kcal over Time",
       x = "Date", y = "Value",
       color = "Variable") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
