
# ===========================
# BMZ WASH & Climate Resilience - RELEVANCE Analysis
# Data: Demographics + Relevance (280 obs)
# Author: DATAUPSKLL CONSULTING KENYA LTD
# ===========================

# ===============================
# BMZ DAC Criteria: EFFECTIVENESS
# ===============================

# Load libraries
library(tidyverse)
library(readr)
library(openxlsx)
library(janitor)
library(ggplot2)
library(dplyr)

# Define working directories
input_file <- "C:/Users/Hp/Downloads/effectiveness_demographics.csv"
output_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
if(!dir.exists(output_dir)) dir.create(output_dir)

# Load dataset
df <- read_csv(input_file) %>% clean_names()

# Load dataset with encoding fix
df <- read_csv(input_file, locale = locale(encoding = "UTF-8")) %>% clean_names()
df[] <- lapply(df, function(x) if(is.character(x)) iconv(x, from="latin1", to="UTF-8") else x)


# -------------------------------
# Helper function to save plots
# -------------------------------
save_plot <- function(plot, filename){
  ggsave(filename = file.path(output_dir, filename), 
         plot = plot, width = 8, height = 6, dpi = 300)
}

# Helper: summary table export
write_summary <- function(table, filename){
  write.xlsx(table, file.path(output_dir, filename), overwrite = TRUE)
}

# =======================================================
# 1. WATER ACCESS & TIME USE
# =======================================================
water_summary <- df %>%
  select(village, gender_hh, water_l_per_day, kiosk_use_frequency, time_fetch_before, 
         time_fetch_now, time_change, women_less_time_fetch)

# Bar chart: kiosk usage frequency
p1 <- ggplot(df, aes(x = kiosk_use_frequency, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Kiosk Use Frequency by Village", x = "Frequency", y = "Count")
save_plot(p1, "effectiveness_kiosk_use.png")

# Change in time fetching water
p2 <- ggplot(df, aes(x = time_change, fill = gender_hh)) +
  geom_bar(position = "fill") +
  labs(title = "Change in Water Fetching Time by Gender", x = "Change", y = "Proportion")
save_plot(p2, "effectiveness_time_change.png")

# =======================================================
# 2. HOUSEHOLD INCOME & LIVELIHOODS
# =======================================================
income_summary <- df %>%
  select(village, gender_hh, income_increased, income_increase_where)

# Pie chart: income increased
income_tab <- df %>% count(income_increased) %>%
  mutate(perc = n/sum(n)*100)

p3 <- ggplot(income_tab, aes(x = "", y = perc, fill = income_increased)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Households Reporting Increased Income")
save_plot(p3, "effectiveness_income_increase.png")

# =======================================================
# 3. WATER QUALITY & INFRASTRUCTURE RELIABILITY
# =======================================================
infra_summary <- df %>%
  select(village, water_quality_change, borehole_functional, borehole_breakdowns, 
         breakdown_frequency, repairs_water, repairs_latrines, user_fees_effective)

# Bar: borehole functionality
p4 <- ggplot(df, aes(x = borehole_functional, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Borehole Functionality by Village", x = "Functional?", y = "Count")
save_plot(p4, "effectiveness_borehole_functional.png")

# =======================================================
# 4. SANITATION & HYGIENE EFFECTIVENESS
# =======================================================
sanitation_summary <- df %>%
  select(village, hfkh_latrine_use, latrine_satisfaction, latrine_functional_during_flood, 
         girls_comfortable_using_latrine_night, women_latrine_use_night_and_floods, 
         place_to_wash_hands, soap_available_today, hygiene_training_reduced_illness, 
         diarrhea_past_2wks)

# Stacked bar: handwashing places
p5 <- ggplot(df, aes(x = place_to_wash_hands, fill = soap_available_today)) +
  geom_bar(position = "fill") +
  labs(title = "Handwashing Facilities & Soap Availability", x = "Facility", y = "Proportion")
save_plot(p5, "effectiveness_handwashing.png")

# =======================================================
# 5. FLOOD PREPAREDNESS & RESILIENCE
# =======================================================
flood_summary <- df %>%
  select(village, flood_prep_actions_stored_water, aware_canal_desilting, canal_works_helped_reduce_flooding, 
         community_committee_still_active, effective_desilted_canals_and_embankments_reducing_floods, 
         household_prepared_for_seasonal_flooding, village_better_protected)

# Bar chart: household flood preparedness
p6 <- ggplot(df, aes(x = household_prepared_for_seasonal_flooding, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Household Preparedness for Seasonal Flooding", x = "Prepared?", y = "Count")
save_plot(p6, "effectiveness_flood_prep.png")

# =======================================================
# 6. YOUTH PARTICIPATION & CAPACITY BUILDING
# =======================================================
youth_summary <- df %>%
  select(village, youth_participation_training, youth_jobs_roles_water_committee, usefulness_awareness_sessions)

# Stacked bar: youth participation
p7 <- ggplot(df, aes(x = youth_participation_training, fill = village)) +
  geom_bar(position = "fill") +
  labs(title = "Youth Participation by Village", x = "Participation", y = "Proportion")
save_plot(p7, "effectiveness_youth_participation.png")

# =======================================================
# EXPORT SUMMARY TABLES
# =======================================================
write_summary(water_summary, "effectiveness_water_summary.xlsx")
write_summary(income_summary, "effectiveness_income_summary.xlsx")
write_summary(infra_summary, "effectiveness_infra_summary.xlsx")
write_summary(sanitation_summary, "effectiveness_sanitation_summary.xlsx")
write_summary(flood_summary, "effectiveness_flood_summary.xlsx")
write_summary(youth_summary, "effectiveness_youth_summary.xlsx")

# =======================================================
# AUTO-NARRATIVE POINTS (Skeleton)
# =======================================================
cat("\n EFFECTIVENESS: AUTO-NARRATIVE NOTES \n")
cat("- Majority of households reported changes in water access and reduced fetching time.\n")
cat("- A share of households indicated increased income, mainly from farming and small businesses.\n")
cat("- Borehole functionality varied across villages, with breakdown frequency influencing perceptions.\n")
cat("- Sanitation facilities improved but challenges remain during floods and at night for women and girls.\n")
cat("- Flood preparedness actions and canal desilting awareness indicate strengthened resilience.\n")
cat("- Youth are participating in project activities, though roles differ across villages.\n")




# ===============================
# BMZ DAC Criteria: EFFECTIVENESS (with Console Outputs)
# ===============================

# Load libraries
library(tidyverse)
library(readr)
library(openxlsx)
library(janitor)
library(ggplot2)
library(dplyr)

# Define working directories
input_file <- "C:/Users/Hp/Downloads/effectiveness_demographics.csv"
output_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
if(!dir.exists(output_dir)) dir.create(output_dir)

# Load dataset with encoding fix
df <- read_csv(input_file, locale = locale(encoding = "UTF-8")) %>% clean_names()
df[] <- lapply(df, function(x) if(is.character(x)) iconv(x, from="latin1", to="UTF-8") else x)

# -------------------------------
# Helper functions
# -------------------------------
save_plot <- function(plot, filename){
  ggsave(filename = file.path(output_dir, filename), 
         plot = plot, width = 8, height = 6, dpi = 300)
}

write_summary <- function(table, filename){
  write.xlsx(table, file.path(output_dir, filename), overwrite = TRUE)
}

print_freq <- function(data, var, group=NULL){
  if(!is.null(group)){
    tab <- data %>% count(!!sym(group), !!sym(var)) %>%
      group_by(!!sym(group)) %>%
      mutate(perc = round(n/sum(n)*100,1))
  } else {
    tab <- data %>% count(!!sym(var)) %>%
      mutate(perc = round(n/sum(n)*100,1))
  }
  print(tab)
  return(tab)
}

# =======================================================
# 1. WATER ACCESS & TIME USE
# =======================================================
cat("\n--- WATER ACCESS & TIME USE ---\n")

# Frequencies
tab_kiosk <- print_freq(df, "kiosk_use_frequency", "village")
tab_time_change <- print_freq(df, "time_change", "gender_hh")

# Save to Excel
write_summary(tab_kiosk, "tab_kiosk_use.xlsx")
write_summary(tab_time_change, "tab_time_change.xlsx")

# Plots
p1 <- ggplot(df, aes(x = kiosk_use_frequency, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Kiosk Use Frequency by Village", x = "Frequency", y = "Count")
save_plot(p1, "effectiveness_kiosk_use.png")

p2 <- ggplot(df, aes(x = time_change, fill = gender_hh)) +
  geom_bar(position = "fill") +
  labs(title = "Change in Water Fetching Time by Gender", x = "Change", y = "Proportion")
save_plot(p2, "effectiveness_time_change.png")

# =======================================================
# 2. HOUSEHOLD INCOME & LIVELIHOODS
# =======================================================
cat("\n--- HOUSEHOLD INCOME & LIVELIHOODS ---\n")

tab_income <- print_freq(df, "income_increased")
tab_income_village <- print_freq(df, "income_increased", "village")

write_summary(tab_income, "tab_income_increase.xlsx")

# Pie chart
income_tab <- df %>% count(income_increased) %>%
  mutate(perc = n/sum(n)*100)

p3 <- ggplot(income_tab, aes(x = "", y = perc, fill = income_increased)) +
  geom_col(width = 1) + coord_polar("y") +
  labs(title = "Proportion of Households Reporting Increased Income")
save_plot(p3, "effectiveness_income_increase.png")

# =======================================================
# 3. WATER QUALITY & INFRASTRUCTURE RELIABILITY
# =======================================================
cat("\n--- WATER QUALITY & INFRASTRUCTURE RELIABILITY ---\n")

tab_borehole <- print_freq(df, "borehole_functional", "village")
tab_water_quality <- print_freq(df, "water_quality_change")

write_summary(tab_borehole, "tab_borehole_functional.xlsx")

p4 <- ggplot(df, aes(x = borehole_functional, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Borehole Functionality by Village", x = "Functional?", y = "Count")
save_plot(p4, "effectiveness_borehole_functional.png")

# =======================================================
# 4. SANITATION & HYGIENE EFFECTIVENESS
# =======================================================
cat("\n--- SANITATION & HYGIENE EFFECTIVENESS ---\n")

tab_latrine_use <- print_freq(df, "hfkh_latrine_use", "village")
tab_handwash <- print_freq(df, "place_to_wash_hands", "soap_available_today")

write_summary(tab_latrine_use, "tab_latrine_use.xlsx")
write_summary(tab_handwash, "tab_handwash.xlsx")

p5 <- ggplot(df, aes(x = place_to_wash_hands, fill = soap_available_today)) +
  geom_bar(position = "fill") +
  labs(title = "Handwashing Facilities & Soap Availability", x = "Facility", y = "Proportion")
save_plot(p5, "effectiveness_handwashing.png")

# =======================================================
# 5. FLOOD PREPAREDNESS & RESILIENCE
# =======================================================
cat("\n--- FLOOD PREPAREDNESS & RESILIENCE ---\n")

tab_flood_prep <- print_freq(df, "household_prepared_for_seasonal_flooding", "village")
tab_canal_awareness <- print_freq(df, "aware_canal_desilting")

write_summary(tab_flood_prep, "tab_flood_prep.xlsx")

p6 <- ggplot(df, aes(x = household_prepared_for_seasonal_flooding, fill = village)) +
  geom_bar(position = "dodge") +
  labs(title = "Household Preparedness for Seasonal Flooding", x = "Prepared?", y = "Count")
save_plot(p6, "effectiveness_flood_prep.png")

# =======================================================
# 6. YOUTH PARTICIPATION & CAPACITY BUILDING
# =======================================================
cat("\n--- YOUTH PARTICIPATION & CAPACITY BUILDING ---\n")

tab_youth <- print_freq(df, "youth_participation_training", "village")
tab_awareness <- print_freq(df, "usefulness_awareness_sessions")

write_summary(tab_youth, "tab_youth_participation.xlsx")

p7 <- ggplot(df, aes(x = youth_participation_training, fill = village)) +
  geom_bar(position = "fill") +
  labs(title = "Youth Participation by Village", x = "Participation", y = "Proportion")
save_plot(p7, "effectiveness_youth_participation.png")

# =======================================================
# AUTO-NARRATIVE POINTS
# =======================================================
cat("\n EFFECTIVENESS: AUTO-NARRATIVE NOTES \n")
cat("- Majority of households reported changes in water access and reduced fetching time.\n")
cat("- A share of households indicated increased income, mainly from farming and small businesses.\n")
cat("- Borehole functionality varied across villages, with breakdown frequency influencing perceptions.\n")
cat("- Sanitation facilities improved but challenges remain during floods and at night for women and girls.\n")
cat("- Flood preparedness actions and canal desilting awareness indicate strengthened resilience.\n")
cat("- Youth are participating in project activities, though roles differ across villages.\n")



# ===============================
# BMZ Project - DAC: Effectiveness
# Comprehensive R Analysis Script
# ===============================

# Load libraries
library(tidyverse)
library(readr)
library(openxlsx)
library(ggplot2)

# -------------------------------
# 1. Load and prepare data
# -------------------------------
data <- read_csv("C:/Users/Hp/Downloads/effectiveness_demographics.csv")

# -------------------------------
# Define demographics + effectiveness variables
# -------------------------------
demographics <- c("village", "gender_hh", "income_band", "pwd")

effectiveness_vars <- c(
  # Section 1: Water access & time use
  "kiosk_use_frequency", "time_fetch_before", "time_fetch_now", 
  "time_change", "women_less_time_fetch",
  
  # Section 2: Household income & livelihoods
  "income_increased", "income_increase_where",
  
  # Section 3: Water quality & infrastructure reliability
  "water_quality_change", "borehole_functional", "borehole_breakdowns", 
  "breakdown_frequency", "repairs_water", "repairs_latrines", "user_fees_effective",
  
  # Section 4: Sanitation & hygiene effectiveness
  "hfkh_latrine_use", "latrine_satisfaction", "latrine_functional_during_flood",
  "girls_comfortable_using_latrine_night", "women_latrine_use_night_and_floods",
  "place_to_wash_hands", "soap_available_today", "hygiene_training_reduced_illness", 
  "diarrhea_past_2wks",
  
  # Section 5: Flood preparedness & resilience
  "flood_prep_actions_stored_water", "aware_canal_desilting", 
  "canal_works_helped_reduce_flooding", "community_committee_still_active", 
  "effective_desilted_canals_and_embankments_reducing_floods", 
  "household_prepared_for_seasonal_flooding", "village_better_protected",
  
  # Section 6: Youth participation & capacity building
  "youth_participation_training", "youth_jobs_roles_water_committee", 
  "usefulness_awareness_sessions"
)

# Convert to factors safely (only if exists)
factor_vars <- intersect(effectiveness_vars, names(data))
data[factor_vars] <- lapply(data[factor_vars], factor)
data[demographics] <- lapply(data[demographics], factor)

# -------------------------------
# Create output folder
# -------------------------------
output_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
if(!dir.exists(output_dir)) dir.create(output_dir)

save_plot <- function(plot, filename) {
  ggsave(filename = file.path(output_dir, filename),
         plot = plot, width = 7, height = 5, dpi = 300)
}

# -------------------------------
# 2. Descriptive stats + plots
# -------------------------------
descriptive_summary <- list()

for (var in factor_vars) {
  tab <- as.data.frame(table(data[[var]], useNA = "ifany"))
  colnames(tab) <- c("Category", "Count")
  tab$Percent <- round(100 * tab$Count / sum(tab$Count), 1)
  
  descriptive_summary[[var]] <- tab
  
  p <- ggplot(tab, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  
  save_plot(p, paste0(var, "_bar.png"))
}

write.xlsx(descriptive_summary, file = file.path(output_dir, "Effectiveness_Descriptive.xlsx"))

# -------------------------------
# 3. Cross-tabs + Chi-square tests
# -------------------------------
chi_results <- list()

for (var in factor_vars) {
  for (demo in demographics) {
    if (all(!is.na(data[[var]])) & all(!is.na(data[[demo]]))) {
      tab <- table(data[[var]], data[[demo]])
      chi <- tryCatch(chisq.test(tab), error = function(e) NULL)
      
      chi_results[[paste(var, demo, sep = "_vs_")]] <- list(
        Crosstab = tab,
        Chi_Square = chi
      )
      
      df <- as.data.frame(prop.table(tab, 2))
      colnames(df) <- c(var, demo, "Percent")
      
      p <- ggplot(df, aes_string(x = demo, y = "Percent", fill = var)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste(var, "by", demo), y = "Percentage") +
        theme_minimal()
      
      save_plot(p, paste0(var, "_by_", demo, ".png"))
    }
  }
}

# Save chi-square outputs
sink(file.path(output_dir, "Chi_Square_Results.txt"))
for (name in names(chi_results)) {
  cat("\n\n==== ", name, " ====\n")
  print(chi_results[[name]]$Crosstab)
  if (!is.null(chi_results[[name]]$Chi_Square)) {
    print(chi_results[[name]]$Chi_Square)
  } else {
    cat("Chi-square test could not be computed (low counts).\n")
  }
}
sink()

cat("Effectiveness analysis complete.\nAll outputs saved to:", output_dir)
