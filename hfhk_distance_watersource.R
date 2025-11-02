# Load libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load dataset
data <- read_csv("C:/Users/Hp/Downloads/hfhk_distance_watersource.csv")

# Focus only on HFHK kiosks
hf_data <- data %>%
  filter(primary_water_source %in% c("HFHK Solar Borehole/Kiosk", "Piped HFHK Water"))

# --- Descriptive statistics ---
mean_dist <- mean(hf_data$distance_in_meters, na.rm = TRUE)
median_dist <- median(hf_data$distance_in_meters, na.rm = TRUE)
n_sample <- nrow(hf_data)

cat("Among households using HFHK kiosks:\n")
cat(" Mean distance:", round(mean_dist, 1), "m\n")
cat(" Median distance:", round(median_dist, 1), "m\n")
cat(" Sample size (n):", n_sample, "\n\n")

# --- Absolute frequencies by distance categories ---
hf_data <- hf_data %>%
  mutate(distance_cat = case_when(
    distance_in_meters < 500 ~ "<500m",
    distance_in_meters >= 500 & distance_in_meters <= 1000 ~ "500m-1km",
    distance_in_meters > 1000 ~ ">1km"
  ))

freq_table <- hf_data %>%
  group_by(village, distance_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = distance_cat, values_from = count, values_fill = 0)

print(freq_table)

# --- Average distance for >1km in Kabong'o and Kokech ---
avg_far <- hf_data %>%
  filter(distance_in_meters > 1000, village %in% c("Kabong'o", "Kokech")) %>%
  group_by(village) %>%
  summarise(mean_above1km = mean(distance_in_meters, na.rm = TRUE),
            n_cases = n())

print(avg_far)

# --- Visualization: distribution of distance categories per village ---
ggplot(hf_data, aes(x = village, fill = distance_cat)) +
  geom_bar(position = "dodge") +
  labs(title = "HFHK Kiosk Users: Distance to Water Source",
       x = "Village", y = "Number of Households",
       fill = "Distance Category") +
  theme_minimal()

# --- Visualization: histogram of distances ---
ggplot(hf_data, aes(x = distance_in_meters)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Distance to HFHK Kiosks",
       x = "Distance (meters)", y = "Number of Households") +
  theme_minimal()




# Load libraries
library(dplyr)
library(readr)

# Load dataset
data_path <- "C:/Users/Hp/Downloads/hfhk_distance_watersource.csv"
df <- read_csv(data_path)

# Create distance categories
df <- df %>%
  mutate(
    distance_category = case_when(
      distance_in_meters < 500 ~ "<500m",
      distance_in_meters >= 500 & distance_in_meters <= 1000 ~ "500m-1km",
      distance_in_meters > 1000 ~ ">1km",
      TRUE ~ "Unknown"
    )
  )

# -------------------------------
# 1. Frequency counts by village and water source
# -------------------------------
freq_by_village_source <- df %>%
  group_by(village, primary_water_source, distance_category) %>%
  summarise(households = n(), .groups = "drop") %>%
  arrange(village, primary_water_source, distance_category)

print("=== Frequency Counts by Village, Source, and Distance Category ===")
print(freq_by_village_source)

# -------------------------------
# 2. Overall totals (all villages combined, by water source only)
# -------------------------------
freq_overall_source <- df %>%
  group_by(primary_water_source, distance_category) %>%
  summarise(households = n(), .groups = "drop") %>%
  arrange(primary_water_source, distance_category)

print("=== Overall Frequency Counts by Source and Distance Category ===")
print(freq_overall_source)

# -------------------------------
# 3. Optional: Overall totals (all villages + all sources)
# -------------------------------
freq_overall_total <- df %>%
  group_by(distance_category) %>%
  summarise(households = n(), .groups = "drop")

print("=== Grand Totals Across All Sources and Villages ===")
print(freq_overall_total)







