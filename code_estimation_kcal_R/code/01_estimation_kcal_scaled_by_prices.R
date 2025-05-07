library(ggplot2)
library(dplyr)
library(lubridate)
library(rio)
library(zoo)

## TODO ___ INCLUDE INFLATION USD !!

# Clean Sorghum data
data_sorghum_price <- rio::import('data/wfp_food_prices_sdn_2025.csv') |>
  dplyr::filter(admin2 == 'Jebal Aulya') |>
  dplyr::filter(commodity %in% c('Sorghum', 'Sorghum (white)')) |>
  dplyr::mutate(
    usdprice = as.numeric(usdprice),
    usdprice = dplyr::case_when(
      commodity == 'Sorghum' ~ usdprice * 90 / 3,
      TRUE ~ usdprice
    )
  ) |>
  dplyr::select(date, usdprice) |>
  dplyr::group_by(date) |>
  dplyr::summarise(mean_usdprice = mean(usdprice, na.rm = TRUE)) |>
  dplyr::mutate(month = lubridate::month(date),
                year = lubridate::year(date)) |>
  dplyr::select(year, month, mean_usdprice) |>
  dplyr::mutate(date = as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d'))

## Plot sorghum
plot_sorghum <- ggplot2::ggplot(data_sorghum_price, aes(date, mean_usdprice)) +
  geom_line() +
  theme_bw()
ggsave('visualization/sorghum_price.png')
print(plot_sorghum)

# Clean Sorghum data
data_millet_price <- rio::import('data/wfp_food_prices_sdn_2025.csv') |>
  dplyr::filter(admin2 == 'Jebal Aulya') |>
  dplyr::filter(commodity %in% c('Millet')) |>
  dplyr::mutate(
    usdprice = as.numeric(usdprice),
    usdprice = dplyr::case_when(
      unit == '3.5 KG' ~ usdprice * 90 / 3.5,
      TRUE ~ usdprice
    )
  ) |>
  dplyr::select(date, usdprice) |>
  dplyr::group_by(date) |>
  dplyr::summarise(mean_usdprice_millet = mean(usdprice, na.rm = TRUE)) |>
  dplyr::mutate(month = lubridate::month(date),
                year = lubridate::year(date)) |>
  dplyr::select(year, month, mean_usdprice_millet) |>
  dplyr::mutate(date = as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d'))

## Plot Millet
plot_millet <- ggplot2::ggplot(data_millet_price, aes(date, mean_usdprice_millet)) +
  geom_line() +
  theme_bw()
ggsave('visualization/millet_price.png')
print(plot_millet)

data_millet_sorghum_price <- data_sorghum_price |> 
  dplyr::left_join(data_millet_price) |>
  dplyr::group_by(date, year, month) |>
  dplyr::summarise(mean_usdprice = mean(mean_usdprice, mean_usdprice_millet))

## Plot Mean Millet and Sorghum
plot_millet_sorghum <- ggplot2::ggplot(data_millet_sorghum_price, aes(date, mean_usdprice)) +
  geom_line() +
  theme_bw()
ggsave('visualization/sorghum_millet_price.png')
print(plot_millet_sorghum)

# Create a sequence of half-monthly dates from 2009 to 2025
all_dates <- seq(as.Date("2009-01-01"), as.Date("2025-03-01"), by = "month")
all_dates <- sort(c(all_dates, all_dates + 14))
all_dates_df <- data.frame(date = all_dates)

# Merge the sequence of dates with the sorghum price data
complete_data <- all_dates_df |>
  dplyr::left_join(data_millet_sorghum_price, by = "date") |>
  dplyr::mutate(mean_usdprice = ifelse(is.na(mean_usdprice), NA_real_, mean_usdprice))

# Interpolate missing sorghum price data using na.spline
complete_data <- complete_data |>
  dplyr::mutate(mean_usdprice = na.approx(mean_usdprice, na.rm = FALSE))

# Fill any remaining NA values using forward and backward fill
complete_data <- complete_data |>
  dplyr::mutate(mean_usdprice = na.locf(mean_usdprice, na.rm = FALSE)) |>
  dplyr::mutate(mean_usdprice = na.locf(mean_usdprice, na.rm = FALSE, fromLast = TRUE))

# Normalize the sorghum prices
min_price <- min(complete_data$mean_usdprice, na.rm = TRUE)
max_price <- max(complete_data$mean_usdprice, na.rm = TRUE)

complete_data <- complete_data |>
  dplyr::mutate(norm_usd_price = (mean_usdprice - min_price) / (max_price - min_price)) |>
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date))

plot_complete_data_norm <- ggplot2::ggplot(complete_data, aes(date, norm_usd_price)) +
  geom_line() +
  theme_bw()
ggsave('visualization/norm_price_usd_millet_sorghum.png')
print(plot_complete_data_norm)

# Kcal from the different documents
kcal_2017 <- 1893
kcal_2014 <- 2247

# Linear interpolation - slope
m <- (kcal_2017 - kcal_2014) / (as.numeric(as.Date("2017-05-01") - as.Date("2014-05-01")) / 365.25)

# Function to estimate kcal based on date and normalized price
estimer_kcal <- function(date, prix_normalise) {
  jours_depuis_2014 <- as.numeric(date - as.Date("2014-05-01")) / 365.25
  kcal <- kcal_2014 + m * jours_depuis_2014 * prix_normalise
  return(kcal)
}

# Estimate kcal for each month
complete_data <- complete_data |>
  dplyr::mutate(kcal_est = sapply(1:nrow(complete_data), function(i) {
    estimer_kcal(complete_data$date[i], complete_data$norm_usd_price[i])
  }))

# Visualization of results
plot <- ggplot(complete_data, aes(x = date)) +
  geom_line(aes(y = kcal_est), color = "#33638dff") +
  geom_point(aes(y = kcal_est), color = "#B12A90FF") +
  labs(title = "kcal estimation based on 2014/2017 kcal",
       x = "Year",
       y = "kcal estimates") +
  theme_bw()
ggsave('visualization/kcal_scaled_on_sorghum.png')
print(plot)

# Filter data for 2022-2025
data_sorghum_price_2023_2025 <- complete_data |>
  dplyr::filter(date >= '2023-04-01') |>
  dplyr::filter(date <= '2025-03-01')

plot_2022_2025 <- ggplot(data_sorghum_price_2023_2025, aes(x = date)) +
  geom_line(aes(y = kcal_est), color = "#33638dff") +
  geom_point(aes(y = kcal_est), color = "#B12A90FF") +
  labs(title = "kcal estimation based on 2014/2017 kcal (2022-2025)",
       x = "Year",
       y = "kcal estimates") +
  theme_bw()
ggsave('visualization/kcal_scaled_on_sorghum_war.png')
print(plot_2022_2025)

## Then we can add the data from the community kitchens 
kcal_kitchens <- rio::import('data/final_nut_val_for_malnutrition.xlsx') |>
  #dplyr::mutate(year = 2024) |>
  dplyr::select(Month, scen_0_mean_tot_cal_per_p, scen_0_cal_lower_ci, scen_0_cal_upper_ci, half_date) |>
  dplyr::mutate(date = as.Date(half_date)) |>
  dplyr::rename(mean_kcal = scen_0_mean_tot_cal_per_p, 
                mean_low_ci_kcal = scen_0_cal_lower_ci, 
                mean_up_ci_kcal = scen_0_cal_upper_ci) |>
  dplyr::select(date, mean_kcal, mean_low_ci_kcal, mean_up_ci_kcal) 
kcal_kitchens$date <- as.Date(kcal_kitchens$date)
kcal_kitchens <- kcal_kitchens[order(kcal_kitchens$date), ]
  
# Merge the datasets based on the date
merged_data <- data_sorghum_price_2023_2025 |>
  left_join(kcal_kitchens, by = c("date"))

## Now we can addition and remove some
merged_data <- merged_data |>
  dplyr::mutate(kcal_per_p_mean_baseline = rowSums(across(c(kcal_est, mean_kcal)), na.rm = TRUE), 
                kcal_per_p_low_ci_baseline = rowSums(across(c(kcal_est, mean_low_ci_kcal)), na.rm = TRUE), 
                kcal_per_p_up_ci_baseline = rowSums(across(c(kcal_est, mean_up_ci_kcal)), na.rm = TRUE))
merged_data$date <- as.Date(merged_data$date)
merged_data <- merged_data[order(merged_data$date), ]

## Scale decreease linearly over time
n <- nrow(merged_data) 
merged_data <- merged_data |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    scale_factor = seq(from = 1, to = 0.25, length.out = n),
    kcal_per_p_mean_gradual_25 = scale_factor * kcal_est
  ) |>
  dplyr::mutate(
    scale_factor = seq(from = 1, to = 0.5, length.out = n),
    kcal_per_p_mean_gradual_50 = scale_factor * kcal_est
  )

## tHen calculate the value of the data 
merged_data <- merged_data |>
  dplyr::mutate(
    ## 25%
    kcal_per_p_mean_25_baseline = rowSums(across(c(kcal_per_p_mean_gradual_25, mean_kcal)), na.rm = TRUE), 
    kcal_per_p_low_ci_25_baseline = rowSums(across(c(kcal_per_p_mean_gradual_25, mean_low_ci_kcal)), na.rm = TRUE), 
    kcal_per_p_up_ci_25_baseline = rowSums(across(c(kcal_per_p_mean_gradual_25, mean_up_ci_kcal)), na.rm = TRUE), 
    ## 50%
    kcal_per_p_mean_50_baseline = rowSums(across(c(kcal_per_p_mean_gradual_50, mean_kcal)), na.rm = TRUE), 
    kcal_per_p_low_ci_50_baseline = rowSums(across(c(kcal_per_p_mean_gradual_50, mean_low_ci_kcal)), na.rm = TRUE), 
    kcal_per_p_up_ci_50_baseline = rowSums(across(c(kcal_per_p_mean_gradual_50, mean_up_ci_kcal)), na.rm = TRUE))


plot_final <- ggplot(merged_data, aes(x = date)) +
  # First line and ribbon
  geom_line(aes(y = kcal_per_p_mean_baseline, 
                color = "Full pre-war caloric intake, with access to community kitchens."), 
            size = 1) +
  geom_ribbon(aes(ymin = kcal_per_p_low_ci_baseline, ymax = kcal_per_p_up_ci_baseline, 
                  fill = "Full pre-war caloric intake, with access to community kitchens."), 
              alpha = 0.1) +
  
  # Second line and ribbon
  geom_line(aes(y = kcal_per_p_mean_25_baseline, 
                color = "25% of pre-war caloric intake, with access to community kitchens."), size = 1) +
  geom_ribbon(aes(ymin = kcal_per_p_low_ci_25_baseline, ymax = kcal_per_p_up_ci_25_baseline, 
                  fill = "25% of pre-war caloric intake, with access to community kitchens."), 
              alpha = 0.1) +
  
  # Third line and ribbon
  geom_line(aes(y = kcal_per_p_mean_50_baseline, 
                color = "50% of pre-war caloric intake, with access to community kitchens."), size = 1) +
  geom_ribbon(aes(ymin = kcal_per_p_low_ci_50_baseline, ymax = kcal_per_p_up_ci_50_baseline, 
                  fill = "50% of pre-war caloric intake, with access to community kitchens."), 
              alpha = 0.1) +
  
  # Horizontal lines + annotations
  geom_hline(yintercept = 2100, linetype = "dashed", color = "#cb0012", size = 0.8) +
  annotate("text", x = as.Date("2023-04-01"), y = 2160, 
           label = "Emergency minimum recommended by Sphere standard (2100kcal)", 
           color = "#cb0012", hjust = 0, size = 3) +
  
  geom_hline(yintercept = 1050, linetype = "dashed", color = "#c2150c", size = 0.8) +
  annotate("text", x = as.Date("2023-04-01"), y = 1100, 
           label = "50% of emergency minimum recommended by Sphere standard (1050kcal)", 
           color = "#c2150c", hjust = 0, size = 3) +
  
  # Vertical line for start of community kitchens
  geom_vline(xintercept = as.Date("2024-07-01"), linetype = "dashed", color = "#000000", size = 0.3) +
  annotate("text", x = as.Date("2024-07-01"), y = 1250, 
           label = "NGO support begins", 
           color = "#000000", angle = 90, vjust = -0.4, hjust = 0, size = 3) +
  
  # Axes and theme
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(add = 5)) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500), limits = c(400, NA)) +
  
  # Customizing the color and fill for the legend
  scale_color_manual(name = element_blank(), 
                     values = c("Full pre-war caloric intake, with access to community kitchens." = "#238A8DFF", 
                                "50% of pre-war caloric intake, with access to community kitchens." = "#33639DFF", 
                                "25% of pre-war caloric intake, with access to community kitchens." = "#453781FF")) +
  scale_fill_manual(name = element_blank(), 
                    values = c("Full pre-war caloric intake, with access to community kitchens." = "#238A8DFF", 
                               "50% of pre-war caloric intake, with access to community kitchens." = "#33639DFF", 
                               "25% of pre-war caloric intake, with access to community kitchens." = "#453781FF")) +
  
  labs(x = "date",
       y = "Estimated daily caloric intake per person (kcal) - Jebel Awlia") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = c(0.02, 0.98),        # Near top-left corner
    legend.justification = c("left", "top"), 
    legend.direction = "vertical",
    legend.box.just = "left"
  )


ggsave('visualization/last_curve_sudan_food_intake.png', height=5)

# First, rescale norm_usd_price to the same range as kcal_est
range_kcal <- range(complete_data$kcal_est, na.rm = TRUE)
range_price <- range(complete_data$mean_usdprice, na.rm = TRUE)

# Compute scaling factor
price_scale_factor <- diff(range_kcal) / diff(range_price) *2.5

complete_data <- complete_data |> dplyr::filter(year >= 2014)

# Plot with improved colors and vertical line
combined_plot <- ggplot(complete_data, aes(x = date)) +
  # Left Y-axis: kcal estimate (with updated color)
  geom_line(aes(y = kcal_est), color = "#1f77b4", size = 1) + # Deep blue for kcal estimate
  
  # Right Y-axis: normalized price (rescaled to kcal axis, updated color)
  geom_line(aes(y = mean_usdprice * price_scale_factor), color = "#ff8300", size = 1) + # Tomato red for price
  
  # Vertical line for the start of the war (April 2023)
  geom_vline(xintercept = as.Date("2023-04-01"), linetype = "dashed", color = "#000000", size = 0.6) +
  annotate("text", x = as.Date("2023-04-01"), y = max(complete_data$kcal_est, na.rm = TRUE) * 1.2,
           label = "Start of war (April 2023)", color = "#000000", angle = 90, vjust = -0.4, size = 2.5) +
  
  # Vertical line for the reference (Jan 2014)
  #geom_hline(yintercept = 2247, linetype = "dashed", color = "#000000", size = 0.2) +
  #annotate("text", x = as.Date("2024-01-01"), y = 2300,
  #         label = "2014 kcal reference", color = "#000000", hjust = 0, size = 2) +
  
  # Horizontal lines + annotations
  geom_hline(yintercept = 2100, linetype = "dashed", color = "#cb0012", size = 0.2) +
  annotate("text", x = as.Date("2014-01-01"), y = 2160, 
           label = "Emergency minimum recommended by Sphere standard (2100kcal)", 
           color = "#cb0012", hjust = 0, size = 2) +
  
  # Axis scaling and labels
  scale_y_continuous(
    name = "Modeled kcal/person/day over time – Jebel Awlia",
    sec.axis = sec_axis(~ . / price_scale_factor, name = "Millet & sorghum price (USD)")
  ) +
  
  labs(
       x = "Date") +
  # Theme settings for clarity
  theme_bw() +
  # X-axis scale with 6-month intervals
  scale_x_date(
    breaks = seq(as.Date("2014-01-01"), max(complete_data$date), by = "6 months"),
    labels = scales::date_format("%b %Y")
  ) +
  

  theme(
    axis.title.y = element_text(color = "#33639DFF", size=8),
    axis.text.y = element_text(size=7),
    axis.title.y.right = element_text(color = "#ff8300", size=8),
    axis.text.y.right = element_text(size=7),
    axis.text.x = element_text(angle = 45, hjust = 1, size=7), 
    axis.title.x = element_blank()
  )

ggsave('visualization/sudan_price_over_kcal.png', height = 4)
