# ===========================
# BMZ WASH & Climate Resilience - RELEVANCE Analysis
# Data: Demographics + Relevance (280 obs)
# Author: DATAUPSKLL CONSULTING KENYA LTD
# ===========================

tinytex::install_tinytex()

install.packages("webshot2")
# ---- 0) Setup ----
# This line creates the 'packages' object.
packages <- c("tidyverse","janitor","skimr","gt","forcats","stringr")

# This line silently installs any missing packages.
invisible(lapply(packages, function(p) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)))

# This line loads the packages into the current R session.
lapply(packages, library, character.only = TRUE)


# Set your CSV path here:
data_path <- "C:/Users/Hp/Downloads/relevance_demographics.csv"   # <--- CHANGE THIS to your actual file name
dir.create("C:/Users/Hp/Downloads/BMZ_OUTPUTS", showWarnings = FALSE)

# ---- 1) Import & basic checks ----
df <- readr::read_csv(data_path, show_col_types = FALSE) %>% 
  janitor::clean_names()

# quick structure & missing scan
skimr::skim(df)

# ensure expected columns exist (will stop if any is missing)
expected_cols <- c(
  # Demographics
  "enumerator","village","gender_hh","widow_single_mother","pwd","pwd_involved_planning",
  "age_group","livelihood","livelihood_other","income_band","aware_hfhk",
  # Relevance
  "primary_water_source","primary_water_other","water_year_round","pay_for_water",
  "pay_frequency_20l","water_cost_affordable","pwd_ease_use","pwd_barriers",
  "latrine_type","latrine_other","hfhk_latrine_satisfaction","latrine_flood_function",
  "latrine_issue","latrine_safe_heavy_rain","all_members_use_latrine","who_not_use_latrine",
  "handwashing_station","hws_components","hws_has_soap_and_water","hws_has_ash_and_water",
  "hws_has_water","hws_has_nothing","handwash_after_toilet","handwash_before_eating",
  "handwash_before_cooking","handwash_after_waste","handwash_after_cleaning",
  "handwash_other","handwash_other_specify","hygiene_practices_improved",
  "hygiene_improvement_desc","flood_damage_decreased","flood_decrease_desc",
  "flood_prep_actions","flood_prep_other","canal_desilt_helped",
  "activities_still_useful","useful_interventions","useful_access_kiosks",
  "useful_latrines_sanitization","useful_training","useful_flood_awareness",
  "useful_committees","useful_other","why_not_useful","most_positive_intervention",
  "remaining_challenges","most_important_change","life_different_if_no_project"
)

missing_cols <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing expected columns in CSV:", paste(missing_cols, collapse=", ")))
}

# Helper: safe % function
pct <- function(x) round(100 * x, 1)

# Helper: write a quick gt table to PNG via gtsave
save_gt <- function(tbl, file){
  gt::gtsave(tbl, filename = file, path = "C:/Users/Hp/Downloads/BMZ_OUTPUTS")
}

# ---- 2) DEMOGRAPHICS PROFILE ----
# 2.1 Overall frequencies
demo_vars <- c("village","gender_hh","widow_single_mother","pwd","age_group",
               "livelihood","income_band","aware_hfhk")

demo_summary <- purrr::map_dfr(
  demo_vars,
  ~ df %>% 
    count(.data[[.x]], name = "n") %>%
    mutate(variable = .x, pct = pct(n/sum(n))) %>%
    relocate(variable)
)

# Print summary
demo_summary %>%
  group_by(variable) %>%
  arrange(variable, desc(n)) %>%
  gt::gt() %>% gt::tab_header(title = "Demographics: Frequencies & Percentages") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/demographics_summary.png"); .}

# 2.2 Disaggregation examples
# Village x Gender of HH Head
tbl_village_gender <- df %>% 
  count(village, gender_hh, name = "n") %>%
  group_by(village) %>%
  mutate(pct = pct(n/sum(n))) %>% ungroup()

tbl_village_gender %>%
  arrange(village, desc(n)) %>%
  gt::gt() %>% gt::tab_header(title = "Village × Gender of HH Head") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/village_by_gender.png"); .}

# Plot: Gender distribution by village (stacked %)
ggplot(tbl_village_gender, aes(x = village, y = pct, fill = gender_hh)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Gender of HH Head by Village (Stacked %)", x=NULL, y=NULL, fill="Gender HH") +
  coord_flip() +
  theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_gender_by_village_stacked.png", width=8, height=5, dpi=300)

# ---- 3) RELEVANCE (DAC) ----
# A) Is the project still useful?
# 3.1 % of respondents who find project activities still useful
useful_tbl <- df %>% 
  filter(!is.na(activities_still_useful)) %>% 
  count(activities_still_useful, name="n") %>%
  mutate(pct = pct(n/sum(n)))

useful_tbl %>% 
  gt::gt() %>% gt::tab_header(title = "% who say project activities are still useful") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/relevance_useful_overall.png"); .}

# Plot: pie chart of "still useful?" (uses bar + coord_polar)
ggplot(useful_tbl, aes(x = "", y = n, fill = fct_infreq(activities_still_useful))) +
  geom_col(width = 1) + coord_polar(theta = "y") +
  labs(title = "Are project activities still useful?", fill = "Response") +
  theme_void()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_useful_pie.png", width=5, height=5, dpi=300)

# Disaggregate by village
useful_by_village <- df %>% 
  filter(!is.na(activities_still_useful)) %>%
  count(village, activities_still_useful, name="n") %>%
  group_by(village) %>% mutate(pct = n/sum(n)) %>% ungroup()

ggplot(useful_by_village, aes(village, pct, fill = activities_still_useful)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Activities still useful? by Village (Stacked %)", x=NULL, y=NULL, fill=NULL) +
  coord_flip() + theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_useful_by_village_stacked.png", width=8, height=5, dpi=300)

# 3.2 Ranking of most useful interventions
# NOTE: We assume binary indicator columns for each intervention (TRUE/Yes/1). 
# If your data stores "Yes"/"No" strings, convert as needed.
bin_to_num <- function(x) {
  # Accepts: TRUE/FALSE, 1/0, "Yes"/"No", "yes"/"no", "Y"/"N"
  if (is.logical(x)) return(as.integer(x))
  y <- tolower(as.character(x))
  as.integer(y %in% c("yes","y","true","1","t"))
}

useful_cols <- c("useful_access_kiosks","useful_latrines_sanitization",
                 "useful_training","useful_flood_awareness","useful_committees","useful_other")

df_useful_long <- df %>%
  mutate(across(all_of(useful_cols), bin_to_num)) %>%
  pivot_longer(all_of(useful_cols), names_to = "intervention", values_to = "selected") %>%
  filter(!is.na(selected)) %>% group_by(intervention) %>%
  summarise(n = sum(selected, na.rm=TRUE), .groups="drop") %>%
  mutate(pct = pct(n/sum(n)))

# Clean labels
label_map <- c(
  useful_access_kiosks = "Access to clean water (kiosks)",
  useful_latrines_sanitization = "Improved sanitation facilities",
  useful_training = "Hygiene education / training",
  useful_flood_awareness = "Flood preparedness awareness",
  useful_committees = "Active community committees",
  useful_other = "Other"
)
df_useful_long$intervention <- recode(df_useful_long$intervention, !!!label_map)

df_useful_long %>%
  arrange(desc(n)) %>%
  gt::gt() %>% gt::tab_header(title = "Most Useful Interventions (Ranked)") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/relevance_useful_interventions.png"); .}

ggplot(df_useful_long, aes(x = reorder(intervention, n), y = n)) +
  geom_col() + coord_flip() +
  labs(title="Most Useful Interventions", x=NULL, y="Responses") +
  theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_useful_interventions_bar.png", width=7, height=5, dpi=300)

# 3.3 Thematic summary of remaining challenges (simple keyword frequency)
# NOTE: Replace/extend keywords as needed for your context.
keywords <- c("cost","breakdown","queue","distance","quality","flood","sanitation","latrine",
              "hygiene","soap","maintenance","committee","water","borehole","kiosk","access")

challenge_terms <- df %>%
  select(remaining_challenges) %>%
  mutate(text = tolower(remaining_challenges)) %>%
  filter(!is.na(text), str_trim(text)!="") %>%
  summarise(across(everything(), ~ .)) %>% pull()

if (length(challenge_terms) > 0) {
  tok <- tibble(text = tolower(df$remaining_challenges)) %>%
    filter(!is.na(text)) %>%
    mutate(text = str_replace_all(text, "[^a-z\\s]", " ")) %>%
    separate_rows(text, sep="\\s+") %>%
    filter(text != "")
  
  freq_keywords <- tok %>%
    filter(text %in% keywords) %>%
    count(text, name="n") %>%
    arrange(desc(n))
  
  if (nrow(freq_keywords) > 0) {
    freq_keywords %>%
      gt::gt() %>% gt::tab_header(title = "Remaining Challenges: Keyword Frequency (quick scan)") %>%
      {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/relevance_challenges_keyword_freq.png"); .}
  }
}

# B) Is the water intervention functional and meeting needs?
#  - % using HFHK water as primary source
#  - % source available year-round
#  - % who pay & find cost affordable
# NOTE: Adjust the pattern below to match your actual labels for HFHK water sources.
is_hfhk_water <- function(x){
  str_detect(tolower(x), "hfhk|kiosk|borehole")
}

water_primary_hfhk <- df %>% 
  mutate(hfhk_primary = is_hfhk_water(primary_water_source)) %>%
  summarise(pct_hfhk_primary = pct(mean(hfhk_primary, na.rm=TRUE)))

# Year-round availability
water_year_tbl <- df %>%
  count(water_year_round, name="n") %>%
  mutate(pct = pct(n/sum(n)))

# Paying and affordability
pay_tbl <- df %>% count(pay_for_water, name="n") %>% mutate(pct = pct(n/sum(n)))
afford_tbl <- df %>% 
  filter(pay_for_water %in% c("Yes","yes","Y","y",1,TRUE)) %>%
  count(water_cost_affordable, name="n") %>%
  mutate(pct = pct(n/sum(n)))

# Save tables
water_primary_hfhk %>% 
  gt::gt() %>% gt::tab_header(title = "% using HFHK water as primary source (pattern-based)") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/water_hfhk_primary_pct.png"); .}

water_year_tbl %>% 
  gt::gt() %>% gt::tab_header(title = "Water source available year-round?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/water_year_round.png"); .}

pay_tbl %>% 
  gt::gt() %>% gt::tab_header(title = "Do you pay for water?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/water_pay.png"); .}

afford_tbl %>% 
  gt::gt() %>% gt::tab_header(title = "Is water cost affordable (among those who pay)?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/water_affordable.png"); .}

# Plots
ggplot(water_year_tbl, aes(x = fct_infreq(water_year_round), y = n)) +
  geom_col() + coord_flip() + labs(title="Year-round availability", x=NULL, y="Responses") +
  theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_water_year_round_bar.png", width=6, height=4, dpi=300)

ggplot(afford_tbl, aes(x = fct_infreq(water_cost_affordable), y = n)) +
  geom_col() + coord_flip() + labs(title="Water cost affordability (payers)", x=NULL, y="Responses") +
  theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_water_affordable_bar.png", width=6, height=4, dpi=300)

# PWD accessibility to water points/latrines
pwd_access_tbl <- df %>% count(pwd_ease_use, name="n") %>% mutate(pct=pct(n/sum(n)))
pwd_access_tbl %>%
  gt::gt() %>% gt::tab_header(title = "Can PWDs easily use water points/latrines?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/pwd_ease_use.png"); .}

# C) Are sanitation facilities still relevant and functional?
latrine_type_tbl <- df %>% count(latrine_type, name="n") %>% mutate(pct = pct(n/sum(n)))
latrine_type_tbl %>%
  gt::gt() %>% gt::tab_header(title = "Current Household Latrine Type") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/latrine_type.png"); .}

# Among HFHK Flood-Resilient Latrine users: satisfaction & flood functionality
# NOTE: adjust pattern as needed to match your HFHK label in 'latrine_type'
is_hfhk_latrine <- function(x){
  str_detect(tolower(x), "hfhk|flood")
}
latrine_hfhk <- df %>% filter(is_hfhk_latrine(latrine_type))

if (nrow(latrine_hfhk) > 0) {
  lat_sat <- latrine_hfhk %>% count(hfhk_latrine_satisfaction, name="n") %>% mutate(pct = pct(n/sum(n)))
  lat_func <- latrine_hfhk %>% count(latrine_flood_function, name="n") %>% mutate(pct = pct(n/sum(n)))
  
  lat_sat %>% gt::gt() %>% gt::tab_header(title = "HFHK Latrine Satisfaction") %>%
    {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/latrine_hfhk_satisfaction.png"); .}
  lat_func %>% gt::gt() %>% gt::tab_header(title = "HFHK Latrine Functionality During Floods") %>%
    {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/latrine_hfhk_flood_function.png"); .}
}

# Safety during heavy rain/flooding (all)
latrine_safe_tbl <- df %>% count(latrine_safe_heavy_rain, name="n") %>% mutate(pct = pct(n/sum(n)))
latrine_use_all_tbl <- df %>% count(all_members_use_latrine, name="n") %>% mutate(pct=pct(n/sum(n)))

latrine_safe_tbl %>% gt::gt() %>% gt::tab_header(title = "Is latrine safe & usable during heavy rain/flooding?") %>%
  {save_gt(., "latrine_safe_heavy_rain.png"); .}
latrine_use_all_tbl %>% gt::gt() %>% gt::tab_header(title = "Do all household members use the latrine?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/latrine_all_members_use.png"); .}

# Plot: Safety by village (stacked %)
latrine_safe_by_village <- df %>%
  count(village, latrine_safe_heavy_rain, name="n") %>%
  group_by(village) %>% mutate(pct = n/sum(n)) %>% ungroup()

ggplot(latrine_safe_by_village, aes(village, pct, fill = latrine_safe_heavy_rain)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Latrine Safety During Heavy Rain by Village", x=NULL, y=NULL, fill=NULL) +
  coord_flip() + theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_latrine_safety_by_village_stacked.png", width=8, height=5, dpi=300)

# D) Did climate resilience components address flood problems?
flood_decrease_tbl <- df %>% count(flood_damage_decreased, name="n") %>% mutate(pct = pct(n/sum(n)))
canal_help_tbl <- df %>% count(canal_desilt_helped, name="n") %>% mutate(pct = pct(n/sum(n)))

flood_decrease_tbl %>% gt::gt() %>% gt::tab_header(title = "Has flood damage decreased since the project?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/flood_damage_decreased.png"); .}
canal_help_tbl %>% gt::gt() %>% gt::tab_header(title = "Have canal works helped reduce flooding?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/canal_desilt_helped.png"); .}

# Plot: Canal works helped (by village, stacked %)
canal_by_village <- df %>% 
  count(village, canal_desilt_helped, name="n") %>% 
  group_by(village) %>% mutate(pct = n/sum(n)) %>% ungroup()

ggplot(canal_by_village, aes(village, pct, fill = canal_desilt_helped)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Canal Works Helped Reduce Flooding? (by Village)", x=NULL, y=NULL, fill=NULL) +
  coord_flip() + theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_canal_helped_by_village_stacked.png", width=8, height=5, dpi=300)

# E) Hygiene relevance (quick snapshot for triangulation later)
hws_tbl <- df %>% count(handwashing_station, name="n") %>% mutate(pct = pct(n/sum(n)))
hws_comp_tbl <- df %>%
  mutate(across(c(hws_has_soap_and_water,hws_has_ash_and_water,hws_has_water,hws_has_nothing), bin_to_num)) %>%
  summarise(
    soap_and_water = sum(hws_has_soap_and_water, na.rm=TRUE),
    ash_and_water  = sum(hws_has_ash_and_water,  na.rm=TRUE),
    water_only     = sum(hws_has_water,          na.rm=TRUE),
    nothing        = sum(hws_has_nothing,        na.rm=TRUE)
  ) %>%
  pivot_longer(everything(), names_to="component", values_to="n") %>%
  mutate(pct = pct(n/sum(n)),
         component = recode(component,
                            soap_and_water="Soap & water",
                            ash_and_water="Ash & water",
                            water_only="Water only",
                            nothing="Nothing"))

hws_tbl %>% gt::gt() %>% gt::tab_header(title = "Handwashing Station at Home?") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/hws_presence.png"); .}
hws_comp_tbl %>% gt::gt() %>% gt::tab_header(title = "HWS: Observed Components") %>%
  {save_gt(., "C:/Users/Hp/Downloads/BMZ_OUTPUTS/hws_components.png"); .}

ggplot(hws_comp_tbl, aes(x = reorder(component, n), y = n)) +
  geom_col() + coord_flip() +
  labs(title="Handwashing Station Components (Observed)", x=NULL, y="Households") +
  theme_minimal()
ggsave("C:/Users/Hp/Downloads/BMZ_OUTPUTS/plot_hws_components_bar.png", width=6, height=4, dpi=300)

# ---- 4) Brief auto-generated narrative bullets (optional draft text) ----
# These are simple placeholders that you can refine in reporting.
n_total <- nrow(df)
p_useful <- useful_tbl %>% filter(tolower(as.character(activities_still_useful)) %in% c("yes","y","true","1")) %>% summarise(pct=sum(n)/sum(useful_tbl$n)) %>% pull(pct)

cat("\n----- DRAFT RELEVANCE SUMMARY (auto) -----\n")
cat(sprintf("- Sample size: %s households.\n", n_total))
cat(sprintf("- Project activities still useful: %s%% (overall).\n", round(100*p_useful,1)))
cat("- Most frequently cited useful interventions:\n")
df_useful_long %>% arrange(desc(n)) %>% head(5) %>% 
  mutate(line = paste0("   • ", intervention, " (n=", n, ")")) %>% pull(line) %>% cat(sep="\n")
cat("\n- Water source availability year-round and affordability tables saved to outputs/.\n")
cat("- HFHK latrine satisfaction and flood functionality (if applicable) saved to outputs/.\n")
cat("- Canal works & flood damage change: summary tables and stacked plots saved to outputs/.\n")
cat("- Handwashing station presence and components summarized for triangulation later.\n")
cat("----- END SUMMARY -----\n")
