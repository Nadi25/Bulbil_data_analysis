

# Bulbil data analysis - Temperature data ---------------------------------


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


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
  
  # add treatment column
  df$origin_treatment <- tools::file_path_sans_ext(basename(file))
  
  return(df)
}

# combine the files with the function above
temp_data <- bind_rows(lapply(files, process_file))

# fix time into time not character
temp_data$Time <- as.POSIXct(temp_data$Time, format = "%Y-%m-%d %H:%M:%S")

num_cols <- grep("^X[0-9]+$", names(temp_data), value = TRUE)
temp_data[num_cols] <- lapply(temp_data[num_cols], as.numeric)




# test plot ---------------------------------------------------------------
# first filter by one origin_treatment
test_data <- temp_data |> 
  filter(origin_treatment == "SbD")

ggplot(test_data, aes(x = Time)) +
  geom_line(aes(y = X1)) +
  geom_line(aes(y = X2)) +
  geom_line(aes(y = X3)) +
  theme_bw()


# long format and exclude the first index column
# coumn 1 (x) can be excluded before
test_long <- test_data |>
  pivot_longer(
    cols = matches("^X[0-9]+$"),   # only X1, X2, ...
    names_to = "sensor",
    values_to = "temperature"
  )


ggplot(test_long, aes(Time, temperature, colour = sensor)) +
  geom_line() +
  theme_bw()



###########
temp_data <- temp_data |>
  mutate(treatment = case_when(
    grepl("S$", origin_treatment) ~ "S",
    grepl("N$", origin_treatment) ~ "N",
    grepl("D$", origin_treatment) ~ "D",
    TRUE ~ NA_character_
  ))

# plot temp per treatment -------------------------------------------------
temp_data <- temp_data |>
  rowwise() |>   # rowwise for rowMeans
  mutate(black_mean = mean(c_across(matches("^X[1-3]$")), na.rm = TRUE)) |>
  ungroup()

group_summary <- temp_data |>
  group_by(treatment, Time) |>
  summarise(mean_temp = mean(black_mean, na.rm = TRUE),
            .groups = "drop")


ggplot(group_summary, aes(Time, mean_temp, colour = treatment)) +
  geom_line() +
  theme_bw()




















