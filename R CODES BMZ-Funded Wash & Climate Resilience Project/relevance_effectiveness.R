############################################################
# relevance_effectiveness.R
# Master: RELEVANCE & EFFECTIVENESS analysis + outputs
# BMZ-Funded WASH & Climate Resilience Project (HFHK)
# ---------------------------------------------------------
# Inputs:
#   Relevance & Demographics: "C:/Users/Hp/Downloads/relevance_demographics.csv"
#   Effectiveness & Demographics: "C:/Users/Hp/Downloads/effectiveness_demographics.csv"
# Outputs (tables + charts + text summaries):
#   "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
############################################################

# ----------------------------
# Libraries
# ----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(janitor)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(glue)
  library(purrr)
  library(forcats)
})

# ----------------------------
# Paths
# ----------------------------
out_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path_rel <- "C:/Users/Hp/Downloads/relevance_demographics.csv"
data_path_eff <- "C:/Users/Hp/Downloads/effectiveness_demographics.csv"

# ----------------------------
# Helpers
# ----------------------------

log_msg <- function(...) cat(glue(...), "\n\n")

save_tbl <- function(tbl, fn){
  readr::write_csv(tbl, file.path(out_dir, fn))
  invisible(tbl)
}

save_plot <- function(p, fn, w=9, h=6){
  ggsave(filename = file.path(out_dir, fn), plot = p, width = w, height = h, dpi = 300, bg = "white")
  invisible(p)
}

# Count + percentage helper
count_pct <- function(df, group_vars, var){
  df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(across(all_of(group_vars)), .data[[var]]) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(pct = round(100*n/sum(n), 1)) %>%
    ungroup() %>%
    rename(value = !!var)
}

# Binary flag for HFHK kiosk users
is_hfhk_kiosk_source <- function(x){
  str_detect(str_to_lower(x %||% ""), "(kiosk|hfhk|borehole|piped hfhk)")
}

convert_time_to_minutes <- function(x) {
  s <- str_to_lower(as.character(x))
  case_when(
    str_detect(s, "<30|min|less than 30") ~ 15,
    str_detect(s, "30[-–]60|30 to 60|30 60|30 - 60|30–60") ~ 45,
    str_detect(s, ">1|> 1|more than 60|1 hour|>1 hour|> 60") ~ 90,
    suppressWarnings(!is.na(as.numeric(s))) ~ as.numeric(s),
    TRUE ~ NA_real_
  )
}


# Safe `%||%`
`%||%` <- function(a, b) if(!is.null(a)) a else b

# ----------------------------
# Load Data
# ----------------------------
rel <- readr::read_csv(data_path_rel, show_col_types = FALSE) %>% clean_names()
eff <- readr::read_csv(data_path_eff, show_col_types = FALSE) %>% clean_names()

log_msg("Loaded relevance+demographics: {nrow(rel)} rows, {ncol(rel)} cols.")
log_msg("Loaded effectiveness+demographics: {nrow(eff)} rows, {ncol(eff)} cols.")

# ----------------------------
# Derived flags
# ----------------------------
rel <- rel %>%
  mutate(
    village = village,
    gender = gender_hh,
    aware_hfhk_flag = str_to_lower(aware_hfhk) %in% c("yes","y","1", "aware","true"),
    primary_source = primary_water_source,
    hfhk_water_user = is_hfhk_kiosk_source(primary_water_source),
    pay_for_water_flag = str_to_lower(pay_for_water) %in% c("yes","y","1","true"),
    water_year_flag = str_to_lower(water_year_round) %in% c("yes","y","1","true"),
    hfhk_latrine_user = str_detect(str_to_lower(latrine_type), "hfhk|flood"),
    pay_frequency_20l = pay_frequency_20l
  )

eff <- eff %>%
  mutate(
    village = village,
    gender = gender_hh,
    kiosk_use_frequency = kiosk_use_frequency,
    time_before_min = convert_time_to_minutes(time_fetch_before),
    time_now_min = convert_time_to_minutes(time_fetch_now),
    time_change = time_change,
    borehole_functional = borehole_functional,
    borehole_breakdowns = borehole_breakdowns,
    breakdown_frequency = breakdown_frequency,
    place_to_wash_hands = place_to_wash_hands,
    water_available_at_hws = water_available_at_hws,
    soap_available_today = soap_available_today,
    hfkh_latrine_use = hfkh_latrine_use,
    latrine_functional_during_flood = latrine_functional_during_flood,
    girls_comfortable_night = girls_comfortable_using_latrine_night,
    women_safe_night_floods = women_latrine_use_night_and_floods,
    home_affected_by_flood = home_affected_by_flood_last_year,
    before_eating_handwash_often = before_eating_handwash_often
  )

# ---------------------------------------------------------
# A) OVERALL CONCERNS (Relevance + Effectiveness)
# ---------------------------------------------------------

## A1) Awareness
aware_tbl <- rel %>%
  transmute(aware = ifelse(aware_hfhk_flag, "Yes", "No")) %>%
  count(aware) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(aware_tbl, "A1_awareness_hfhk_counts_pct.csv")
log_msg("A1) Awareness: {aware_tbl$n[aware_tbl$aware=='Yes']} households ({aware_tbl$pct[aware_tbl$aware=='Yes']}%) knew of the HFHK project, while {aware_tbl$n[aware_tbl$aware=='No']} ({aware_tbl$pct[aware_tbl$aware=='No']}%) did not.")

## A2) Why interventions not useful
why_tbl <- rel %>% 
  filter(!is.na(why_not_useful), why_not_useful != "") %>%
  count(why = why_not_useful) %>% 
  arrange(desc(n)) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(why_tbl, "A2_why_interventions_not_useful.csv")
log_msg("A2) Top cited reasons interventions not useful: {paste0(head(why_tbl$why,3), collapse=', ')}.")

## A3) Greatest positive impact
impact_tbl <- rel %>% 
  filter(!is.na(most_positive_intervention), most_positive_intervention != "") %>%
  count(intervention = most_positive_intervention) %>%
  mutate(pct = round(100*n/sum(n),1)) %>% 
  arrange(desc(n))
save_tbl(impact_tbl, "A3_greatest_positive_impact.csv")
log_msg("A3) Interventions with greatest positive impact (top 3): {paste0(head(impact_tbl$intervention,3), collapse=', ')}.")

## A4) Challenges
ch_tbl <- rel %>% 
  filter(!is.na(remaining_challenges), remaining_challenges != "") %>%
  count(challenge = remaining_challenges) %>% 
  arrange(desc(n)) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(ch_tbl, "A4_remaining_challenges.csv")
log_msg("A4) Main challenges: {paste0(head(ch_tbl$challenge,3), collapse=', ')} were the most cited.")

## A7) Year-round water availability
year_tbl <- rel %>% 
  count(year_round = ifelse(water_year_flag,"Yes","No")) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(year_tbl, "A7_year_round_availability.csv")
log_msg("A7) Year-round availability: {year_tbl$n[year_tbl$year_round=='Yes']} households ({year_tbl$pct[year_tbl$year_round=='Yes']}%) reported consistent water access.")

## A9) Paying for water (overall)
pay_tbl <- rel %>%
  transmute(pays = ifelse(pay_for_water_flag,"Yes","No")) %>%
  count(pays) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(pay_tbl, "A9_pay_water_overall.csv")
log_msg("A9) Payment: {pay_tbl$n[pay_tbl$pays=='Yes']} households ({pay_tbl$pct[pay_tbl$pays=='Yes']}%) pay for water; {pay_tbl$n[pay_tbl$pays=='No']} ({pay_tbl$pct[pay_tbl$pays=='No']}%) do not.")

## A14) Affordability
afford_tbl <- rel %>% 
  filter(!is.na(water_cost_affordable), water_cost_affordable != "") %>%
  group_by(village, gender_hh, water_cost_affordable) %>% 
  summarise(n=n(), .groups="drop_last") %>%
  mutate(pct = round(100*n/sum(n),1)) %>% 
  ungroup()
save_tbl(afford_tbl, "A14_cost_affordability_by_village_gender.csv")
log_msg("A14) Cost affordability varied by village and gender. Example: in {afford_tbl$village[1]}, {afford_tbl$pct[1]}% of {afford_tbl$gender_hh[1]} respondents said costs were '{afford_tbl$water_cost_affordable[1]}'.")

## A16) Flood impact
flood_aff_tbl <- eff %>% 
  filter(!is.na(home_affected_by_flood_last_year), home_affected_by_flood_last_year != "") %>%
  count(home_affected_by_flood = home_affected_by_flood_last_year) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(flood_aff_tbl, "A16_home_affected_by_flood.csv")
log_msg("A16) Flood impact: {flood_aff_tbl$n[flood_aff_tbl$home_affected_by_flood=='Yes']} households ({flood_aff_tbl$pct[flood_aff_tbl$home_affected_by_flood=='Yes']}%) reported being affected by flooding in the past year.")

# ---------------------------------------------------------
# B) RELEVANCE UPDATES
# ---------------------------------------------------------

## B_insurance payment
ins_tbl <- rel %>% 
  filter(!is.na(pay_for_water), pay_for_water != "") %>%
  count(paying_insurance = pay_for_water) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(ins_tbl, "B_insurance_payment_counts_pct.csv")
log_msg("B) Insurance proxy (paying water): {ins_tbl$n[1]} households ({ins_tbl$pct[1]}%) pay, others do not.")

## B_q3_2 Handwash knowledge
hw_knowledge_tbl <- rel %>%
  transmute(
    after_toilet = str_to_lower(handwash_after_toilet) %in% c("yes","y","1","true"),
    before_eating = str_to_lower(handwash_before_eating) %in% c("yes","y","1","true"),
    before_cooking = str_to_lower(handwash_before_cooking) %in% c("yes","y","1","true"),
    after_waste = str_to_lower(handwash_after_waste) %in% c("yes","y","1","true"),
    after_cleaning = str_to_lower(handwash_after_cleaning) %in% c("yes","y","1","true")
  ) %>%
  mutate(
    n_critical = after_toilet + before_eating + before_cooking + after_waste + after_cleaning,
    at_least_3 = n_critical >= 3
  )

hw3_tbl <- hw_knowledge_tbl %>%
  count(at_least_3) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(hw3_tbl, "B_q3_2_handwash_knowledge_at_least3.csv")
log_msg("B_q3.2) Handwashing: {hw3_tbl$n[hw3_tbl$at_least_3==TRUE]} households ({hw3_tbl$pct[hw3_tbl$at_least_3==TRUE]}%) could name ≥3 critical times for handwashing.")

## B_q3_6 Affordability overall
q36_tbl <- rel %>% 
  filter(!is.na(water_cost_affordable), water_cost_affordable != "") %>%
  count(cost_affordable = water_cost_affordable) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(q36_tbl, "B_q3_6_cost_affordability_overall.csv")
log_msg("B_q3.6) Affordability overall: Most common response was '{q36_tbl$cost_affordable[which.max(q36_tbl$n)]}' ({max(q36_tbl$pct)}%).")

## B_q3_9 PWD ease use
q39_tbl <- rel %>% 
  filter(!is.na(pwd_ease_use), pwd_ease_use != "") %>%
  count(pwd_ease = pwd_ease_use) %>% 
  mutate(pct = round(100*n/sum(n),1))
save_tbl(q39_tbl, "B_q3_9_pwd_ease_use.csv")
log_msg("B_q3.9) PWD accessibility: '{q39_tbl$pwd_ease[which.max(q39_tbl$n)]}' was most reported ({max(q39_tbl$pct)}%).")

# ---------------------------------------------------------
# (You can continue similarly for Effectiveness "C" block)
# ---------------------------------------------------------
