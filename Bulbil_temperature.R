

# Bulbil data analysis - Temperature data ---------------------------------
# Date: 12.02.2026

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)

theme_set(theme_bw(base_size = 25))

# import data -------------------------------------------------------------
# example to see structure
CaD <- read.csv("Data/Temperature data/CaD.csv")

# now import all files

# set path to data
path <- "Data/Temperature data/"

# make list of the files
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# loop through all csv files and import them separately
for (f in files) {
  name <- tools::file_path_sans_ext(basename(f))
  assign(name, read.csv(f))
}



# make one dataframe with data from all treatments ------------------------
process_file <- function(file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # remove metadata rows
  df <- df[-c(1:4), ]
  
  # add treatment column from file name
  df$origin_treatment <- tools::file_path_sans_ext(basename(file))
  
  return(df)
}

# combine the files with the function above
temperature_data <- bind_rows(lapply(files, process_file))

# fix time into time not character
temperature_data$Time <- as.POSIXct(temperature_data$Time, format = "%Y-%m-%d %H:%M:%S")

num_cols <- grep("^X[0-9]+$", names(temperature_data), value = TRUE)
temperature_data[num_cols] <- lapply(temperature_data[num_cols], as.numeric)


# filter time to exclude lab in January ---------------------------------------------
# beginning looks ok? leave for now
temp_data<- temperature_data |>
  filter(Time <= as.POSIXct("2026-01-19 23:59:59"))


# delete first column X ---------------------------------------------------
temp_data <- temp_data |> 
  select(-X)



# bring data into long format ---------------------------------------------
temp_data_long <- temp_data |>
  pivot_longer(
    cols = matches("^X[0-9]+$"),   # only X1, X2, ...
    names_to = "sensor",
    values_to = "temperature"
  )


# test plot ---------------------------------------------------------------
# first filter by one origin_treatment
test_data <- temp_data_long |> 
  filter(origin_treatment == "SbN")

ggplot(test_data, aes(Time, temperature, colour = sensor))+
  geom_line()



# group per treatment -----------------------------------------------------
# combine the three loggers of the origin treatment combinations to only the treatment
# D = deep snow
# S = shallow snow
# N = no snow
temp_data_long <- temp_data_long |>
  mutate(Treatment = case_when(
    grepl("S$", origin_treatment) ~ "Shallow snow",
    grepl("N$", origin_treatment) ~ "No snow",
    grepl("D$", origin_treatment) ~ "Deep snow",
    TRUE ~ NA_character_
  ))


# plot temp per origin treatment combination -------------------------------------------------
temp_data <- temp_data |>
  rowwise() |>   # rowwise for rowMeans
  mutate(black_mean = mean(c_across(matches("^X[1-3]$")), na.rm = TRUE)) |>
  ungroup()

group_summary <- temp_data |>
  group_by(origin_treatment, Time) |>
  summarise(mean_temp = mean(black_mean, na.rm = TRUE),
            .groups = "drop")


ggplot(group_summary, aes(Time, mean_temp, colour = origin_treatment)) +
  geom_line()


# boxplot per origin treatment -----------------------------------------------------------------
daily_mean_ot <- temp_data |> 
  group_by(origin_treatment, Date = as.Date(Time)) |> 
  summarise(daily_temp = mean(black_mean, na.rm = TRUE), .groups = "drop")

ggplot(daily_mean_ot, aes(origin_treatment, daily_temp, fill = origin_treatment)) +
  geom_boxplot()



# calculate daily mean temperatures ---------------------------------------
daily_mean <- temp_data_long |>
  mutate(Date = as.Date(Time)) |>
  group_by(Treatment, Date) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE),
            .groups = "drop")
daily_mean


# plot timeline of daily mean per treatment -------------------------------
temp_timeline <- ggplot(daily_mean, aes(Date, mean_temp, colour = Treatment)) +
  geom_line(size = 1) +
  labs(y = "Daily mean temperature (°C)",
       x = "Time")
temp_timeline

ggsave(filename = "Bulbil_temperature_daily_mean_timeline_treatment.png", 
       plot = temp_timeline, 
       path = "Output/", 
       width = 12, height = 7)

# now with sd and se ------------------------------------------------------
daily_summary <- temp_data_long |>
  mutate(Date = as.Date(Time)) |>
  group_by(Treatment, Date) |>
  summarise(mean_temp = mean(temperature, na.rm = TRUE),
            sd_temp = sd(temperature, na.rm = TRUE),
            n = sum(!is.na(temperature)),
            se_temp = sd_temp / sqrt(n),
            .groups = "drop")
daily_summary

ggplot(daily_summary,
       aes(Date, mean_temp, colour = Treatment, fill = Treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_temp - se_temp,
                  ymax = mean_temp + se_temp),
              alpha = 0.2, colour = NA)


# First compute daily mean per logger
# Then compute mean + SE across loggers
daily_logger <- temp_data_long |>
  mutate(Date = as.Date(Time)) |>
  group_by(origin_treatment, Treatment, Date) |>
  summarise(daily_temp = mean(temperature, na.rm = TRUE),
            .groups = "drop")

daily_summary <- daily_logger |>
  group_by(Treatment, Date) |>
  summarise(mean_temp = mean(daily_temp),
            sd_temp = sd(daily_temp),
            n = n(),
            se_temp = sd_temp / sqrt(n),
            .groups = "drop")

ggplot(daily_summary,
       aes(Date, mean_temp, colour = Treatment, fill = Treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_temp - se_temp,
                  ymax = mean_temp + se_temp),
              alpha = 0.2, colour = NA)

# still not very visible



# daily mean, min and max -------------------------------------------------
daily_stats <- temp_data_long |>
  mutate(Date = as.Date(Time)) |>
  group_by(origin_treatment, Treatment, Date) |>
  summarise(
    daily_mean = mean(temperature, na.rm = TRUE),
    daily_min  = min(temperature, na.rm = TRUE),
    daily_max  = max(temperature, na.rm = TRUE),
    .groups = "drop"
  )
daily_stats

# group by treatment and date
daily_summary2 <- daily_stats |>
  group_by(Treatment, Date) |>
  summarise(
    mean_temp = mean(daily_mean),
    min_temp  = mean(daily_min),
    max_temp  = mean(daily_max),
    .groups = "drop"
  )
daily_summary2

plot_data <- daily_summary2 |>
  pivot_longer(cols = c(mean_temp, min_temp, max_temp),
               names_to = "metric",
               values_to = "temperature")

ggplot(plot_data,
       aes(Date, temperature, colour = Treatment)) +
  geom_line() +
  facet_wrap(~metric, ncol = 1, scales = "free_y")


# plot min, max, mean temp per treatment ----------------------------------
temp_timeline_mean_min_max <- ggplot(daily_summary2,
       aes(Date, mean_temp, colour = Treatment, fill = Treatment)) +
  geom_ribbon(aes(ymin = min_temp,
                  ymax = max_temp),
              alpha = 0.15,
              colour = NA) +
  geom_line(size = 1) +
  labs(y = "Daily mean temperature (°C)",
       x = NULL)+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 22),     # axes label
    axis.title = element_text(size = 24),
    legend.text = element_text(size = 20),   # legend
    legend.title = element_text(size = 22) 
  )
temp_timeline_mean_min_max

ggsave(filename = "Bulbil_temperature_daily_mean_min_max_timeline_treatment2.png", 
       plot = temp_timeline_mean_min_max, 
       path = "Output/", 
       width = 12, height = 7)

ggsave(filename = "Bulbil_temperature_daily_mean_min_max_timeline_treatment3.png", 
       plot = temp_timeline_mean_min_max, 
       path = "Output/", 
       width = 18, height = 5)


# boxplot for daily mean per treatment ------------------------------------
ggplot(daily_summary2,
       aes(x = Treatment,
           y = mean_temp,
           fill = Treatment)) +
  geom_boxplot() +
  labs(y = "Daily mean temperature (°C)")

ggplot(daily_summary2,
       aes(x = Treatment,
           y = min_temp,
           fill = Treatment)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1, alpha = 0.3)+
  labs(y = "Daily min temperature (°C)")

ggplot(daily_summary2,
       aes(x = Treatment,
           y = max_temp,
           fill = Treatment)) +
  geom_boxplot() +
  labs(y = "Daily max temperature (°C)")



# model_min <- lmer(daily_min ~ Treatment + (1 | origin_treatment),
#                   data = daily_stats)
# summary(model_min)



# Fit model for temperature differences -----------------------------------
# # daily min
# model_min_lm <- lm(daily_min ~ Treatment, data = daily_stats)
# summary(model_min_lm)
# 
# # daily mean
# model_mean_lm <- lm(daily_mean ~ Treatment, data = daily_stats)
# summary(model_mean_lm)

# include date as random factor to account for daily weather variation
model_min2 <- lmerTest::lmer(daily_min ~ Treatment + (1 | Date),
                   data = daily_stats)
summary(model_min2)

model_mean2 <- lmerTest::lmer(daily_mean ~ Treatment + (1 | Date),
                             data = daily_stats)
summary(model_mean2)

# test different treatments against each other
emmeans(model_mean2, pairwise ~ Treatment)

ggplot(daily_summary2, aes(Treatment, min_temp, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  labs(y = "Daily minimum temperature (°C)")


preds <- data.frame(
  Treatment = c("Deep snow", "Shallow snow", "No snow"),
  pred_min = c(-4.67, -9.83, -9.76)
)


# bosplot of min daily temp with predicted values from model --------------
temp_boxplot_daily_min <- ggplot(daily_summary2, aes(Treatment, min_temp, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  geom_text(data = preds, aes(x = Treatment, y = max(daily_summary2$min_temp)+1, 
                              label = paste0(pred_min, "°C")),
            inherit.aes = FALSE)+
  labs(y = "Daily minimum temperature (°C)")
temp_boxplot_daily_min

ggsave(filename = "Bulbil_temperature_daily_min_boxplot_treatment_pred_values.png", 
       plot = temp_boxplot_daily_min, 
       path = "Output/", 
       width = 10, height = 8)



# filter time in the climate room -----------------------------------------
temp_data_lab <- temperature_data |>
  filter(Time >= as.POSIXct("2026-01-20 23:59:59"))

# delete first column X ---------------------------------------------------
temp_data_lab <- temp_data_lab |> 
  select(-X)



# bring data into long format ---------------------------------------------
temp_data_lab_long <- temp_data_lab |>
  pivot_longer(
    cols = matches("^X[0-9]+$"),   # only X1, X2, ...
    names_to = "sensor",
    values_to = "temperature"
  )


# plot --------------------------------------------------------------------
ggplot(temp_data_lab_long, aes(Time, temperature, colour = sensor))+
  geom_line()




# check day vs night time temp --------------------------------------------
# day is from 8-20
temp_data_lab_long <- temp_data_lab_long |>
  mutate(
    hour = as.integer(format(Time, "%H")),
    day_night = if_else(hour >= 8 & hour < 20, "Day", "Night")
  )

ggplot(temp_data_lab_long,
       aes(day_night, temperature, fill = day_night)) +
  geom_boxplot() +
  labs(x = "", y = "Temperature (°C)")

day_night <- ggplot(temp_data_lab_long,
       aes(Time, temperature, colour = day_night)) +
  geom_point(size = 1) 
day_night

ggsave(filename = "Bulbil_temperature_climate_room_day_night.png", 
       plot = day_night, 
       path = "Output/", 
       width = 10, height = 8)



# check heater ------------------------------------------------------------
# define start and end
start_time <- min(temp_data_lab_long$Time, na.rm = TRUE)
end_time   <- start_time + 6*24*60*60   # + 6 days

# filter first two days
temp_2days <- temp_data_lab_long |>
  filter(Time >= start_time,
         Time <= end_time)

# plot each logger separately
ggplot(temp_2days,
       aes(Time, temperature, colour = origin_treatment)) +
  geom_line()










