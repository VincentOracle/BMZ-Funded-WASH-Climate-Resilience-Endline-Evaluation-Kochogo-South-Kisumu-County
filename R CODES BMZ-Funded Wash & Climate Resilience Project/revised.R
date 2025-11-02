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
############################################################
# relevance_effectiveness_partA.R
# Part A: Header, helpers, load data, derived flags, A) Overall Concerns
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
# Paths (edit if needed)
# ----------------------------
out_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path_rel <- "C:/Users/Hp/Downloads/relevance_demographics.csv"
data_path_eff <- "C:/Users/Hp/Downloads/effectiveness_demographics.csv"

# ----------------------------
# Helpers
# ----------------------------
log_msg <- function(...) cat(glue(...), "\n")

save_tbl <- function(tbl, fn){
  path <- file.path(out_dir, fn)
  readr::write_csv(tbl, path)
  invisible(path)
}

save_plot <- function(p, fn, w=9, h=6){
  path <- file.path(out_dir, fn)
  ggsave(filename = path, plot = p, width = w, height = h, dpi = 300, bg = "white")
  invisible(path)
}

# Save narrative paragraph(s) to text file
save_narrative <- function(text_lines, fn){
  path <- file.path(out_dir, fn)
  writeLines(text_lines, con = path)
  invisible(path)
}

# Print & save table as a short narrative summary (top rows)
print_and_save_tbl_narrative <- function(tbl, title, fn_table, fn_plot = NULL, fn_text = NULL, top_n = 6){
  # save table
  path_tbl <- save_tbl(tbl, fn_table)
  log_msg("Saved table: {path_tbl}")
  # print table head to console
  log_msg("---- {title} ----")
  print(head(tbl, n = top_n))
  # narrative summary
  if(nrow(tbl) == 0){
    narrative <- glue("{title}: No records found.")
  } else {
    # build a short narrative: top item(s)
    top <- tbl %>% arrange(desc(n)) %>% slice_head(n = 3)
    top_lines <- top %>% mutate(line = glue("{row_number()}. {ifelse(!is.na(value),value, as.character(.[[1]]))} — n={n} ({ifelse(!is.na(pct),paste0(pct,'%'), 'NA')})")) %>% pull(line)
    narrative <- c(glue("{title} (top {min(3,nrow(tbl))}):"), top_lines)
  }
  # print narrative
  message(paste(narrative, collapse = "\n"))
  # save narrative text if requested
  if(!is.null(fn_text)){
    save_narrative(narrative, fn_text)
    log_msg("Saved narrative text: {file.path(out_dir, fn_text)}")
  }
  # save plot path if passed (caller will generate plot and pass fn_plot)
  invisible(list(table = path_tbl, narrative = narrative))
}

# Safe `%||%`
`%||%` <- function(a, b) if(!is.null(a)) a else b

# Identify the first existing column name from candidates
first_existing_col <- function(df, candidates){
  hit <- candidates[candidates %in% names(df)][1]
  if(is.na(hit)) return(NULL)
  return(hit)
}

# Flag HFHK kiosk users by text match
is_hfhk_kiosk_source <- function(x){
  str_detect(str_to_lower(x %||% ""), "(kiosk|hfhk|hfhk kiosk|hfhk borehole|project kiosk|project borehole)")
}

clean_text_safe <- function(x) {
  s <- iconv(x, from = "", to = "UTF-8", sub = " ")   # drop invalid bytes
  s <- stringr::str_replace_all(s, "[[:space:]]+", " ") # normalize spaces
  s <- stringr::str_replace_all(s, "[–—]", "-")         # replace en/em dashes
  s
}

# Convert time descriptors to minutes (for categories <30, 30-60, >1 hour)
convert_time_to_minutes <- function(x) {
  s <- clean_text_safe(as.character(x)) %>% str_to_lower()
  case_when(
    str_detect(s, "<30|min|less than 30") ~ 15,
    str_detect(s, "30[-–]60|30 to 60|30 60|30 - 60|30–60") ~ 45,
    str_detect(s, ">1|> 1|more than 60|1 hour|>1 hour|> 60") ~ 90,
    suppressWarnings(!is.na(as.numeric(s))) ~ as.numeric(s),
    TRUE ~ NA_real_
  )
}

# ----------------------------
# Load Data
# ----------------------------
rel <- readr::read_csv(data_path_rel, show_col_types = FALSE) %>% clean_names()
eff <- readr::read_csv(data_path_eff, show_col_types = FALSE) %>% clean_names()

log_msg("Loaded relevance data: {nrow(rel)} rows, {ncol(rel)} columns.")
log_msg("Loaded effectiveness data: {nrow(eff)} rows, {ncol(eff)} columns.")

# ----------------------------
# Derived flags & canonical fields
# ----------------------------

# Attempt to find distance column candidates
dist_candidates <- c("distance_to_kiosk_m","distance_to_water_source_m","distance_m","distance_hfhk_kiosk_m","distance_in_meters")
dist_col_rel <- first_existing_col(rel, dist_candidates)
dist_col_eff <- first_existing_col(eff, dist_candidates)
dist_col <- dist_col_rel %||% dist_col_eff  # use first available

# Attempt to find kiosk price/amount column candidates
price_candidates <- c("kiosk_price_per_20l","pay_amount_kiosk","amount_paid_20l","amount_paid","kiosk_cost_kes","pay_frequency_20l")
price_col_rel <- first_existing_col(rel, price_candidates)
price_col_eff <- first_existing_col(eff, price_candidates)
price_col <- price_col_rel %||% price_col_eff

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
    time_now_min    = convert_time_to_minutes(time_fetch_now),
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
# ----------------------------
# A) OVERALL CONCERNS (Relevance + Effectiveness)
# Each result: save CSV, produce plot (if sensible), print narrative to console and save short narrative text.
# ----------------------------

# A1) Number & % aware of HFHK project
aware_tbl <- rel %>%
  transmute(aware = ifelse(is.na(aware_hfhk_flag), "Unknown", ifelse(aware_hfhk_flag, "Yes","No"))) %>%
  count(aware) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(aware_tbl, "A1_awareness_hfhk_counts_pct.csv")

p_aware <- ggplot(aware_tbl, aes(x=aware, y=pct, fill=aware)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=paste0(pct, "%")), vjust=-0.2) +
  labs(title="Awareness of HFHK Project", x=NULL, y="Percent") + theme_minimal()
save_plot(p_aware, "A1_awareness_hfhk.png")

narr_A1 <- c(
  "A1: Awareness of HFHK project",
  paste0("Total respondents (relevance dataset): ", nrow(rel)),
  paste0("Breakdown:"),
  paste0(apply(aware_tbl, 1, function(r) paste0(" - ", r["aware"], ": ", r["n"], " (", r["pct"], "%)")), collapse = "\n")
)
cat(paste(narr_A1, collapse = "\n"), "\n")
save_narrative(narr_A1, "A1_awareness_hfhk_narrative.txt")

# A2) Why interventions not useful (open text)
why_tbl <- rel %>%
  filter(!is.na(why_not_useful), str_trim(why_not_useful) != "") %>%
  count(why = why_not_useful) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(why_tbl, "A2_why_interventions_not_useful.csv")

p_why <- ggplot(head(why_tbl,12), aes(x=reorder(why,n), y=n)) +
  geom_col(fill="coral") + coord_flip() +
  labs(title="Top Reasons Interventions Not Useful", x=NULL, y="Count") + theme_minimal()
save_plot(p_why, "A2_why_interventions_not_useful.png")

narr_A2 <- c("A2: Reasons interventions not useful (top items):",
             if(nrow(why_tbl)==0) " - No open-text responses found." else apply(head(why_tbl,8), 1, function(r) glue(" - {r['why']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_A2, collapse = "\n"), "\n")
save_narrative(narr_A2, "A2_why_interventions_not_useful_narrative.txt")

# A3) Which intervention has the greatest positive impact
impact_tbl <- rel %>%
  filter(!is.na(most_positive_intervention), str_trim(most_positive_intervention) != "") %>%
  count(intervention = most_positive_intervention) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(impact_tbl, "A3_greatest_positive_impact.csv")

p_impact <- ggplot(head(impact_tbl, 12), aes(x=reorder(intervention,n), y=n)) +
  geom_col(fill="darkgreen") + coord_flip() +
  labs(title="Greatest Positive Impact (self-reported)", x=NULL, y="Count") + theme_minimal()
save_plot(p_impact, "A3_greatest_positive_impact.png")

narr_A3 <- c("A3: Greatest positive impact (top items):",
             if(nrow(impact_tbl)==0) " - No responses." else apply(head(impact_tbl,6), 1, function(r) glue(" - {r['intervention']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_A3, collapse = "\n"), "\n")
save_narrative(narr_A3, "A3_greatest_positive_impact_narrative.txt")

# A4) Challenges faced (open text)
ch_tbl <- rel %>%
  filter(!is.na(remaining_challenges), str_trim(remaining_challenges) != "") %>%
  count(challenge = remaining_challenges) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(ch_tbl, "A4_remaining_challenges.csv")

p_ch <- ggplot(head(ch_tbl,12), aes(x=reorder(challenge,n), y=n)) +
  geom_col(fill="darkred") + coord_flip() +
  labs(title="Top Reported Challenges", x=NULL, y="Count") + theme_minimal()
save_plot(p_ch, "A4_remaining_challenges.png")

narr_A4 <- c("A4: Key remaining challenges (top items):",
             if(nrow(ch_tbl)==0) " - No responses." else apply(head(ch_tbl,8), 1, function(r) glue(" - {r['challenge']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_A4, collapse = "\n"), "\n")
save_narrative(narr_A4, "A4_remaining_challenges_narrative.txt")

# A5) & A6) Average distance to HFHK kiosk (metres) among HFHK primary source users & non-users
if(!is.null(dist_col)) {
  dist_hfhk <- rel %>%
    filter(hfhk_water_user) %>%
    summarise(
      avg_m = round(mean(distance_in_meters, na.rm=TRUE),1),
      median_m = round(median(distance_in_meters, na.rm=TRUE),1),
      n = sum(!is.na(distance_in_meters))
    )
  save_tbl(dist_hfhk, "A5_avg_distance_hfhk_users_m.csv")
  narr_A5 <- c(
    "A5: Average distance to HFHK kiosk among HFHK primary source users",
    if(dist_hfhk$n==0) " - No distance data for HFHK users." else glue(" - Mean: {dist_hfhk$avg_m} m; Median: {dist_hfhk$median_m} m; N (with distance): {dist_hfhk$n}")
  )
  cat(paste(narr_A5, collapse = "\n"), "\n")
  save_narrative(narr_A5, "A5_avg_distance_hfhk_users_narrative.txt")
  
  dist_non <- rel %>%
    filter(!hfhk_water_user | is.na(hfhk_water_user)) %>%
    summarise(
      avg_m = round(mean(distance_in_meters, na.rm=TRUE),1),
      median_m = round(median(distance_in_meters, na.rm=TRUE),1),
      n = sum(!is.na(distance_in_meters))
    )
  save_tbl(dist_non, "A6_avg_distance_non_hfhk_users_m.csv")
  narr_A6 <- c(
    "A6: Average distance to HFHK kiosk among non-HFHK primary source users",
    if(dist_non$n==0) " - No distance data for non-HFHK users." else glue(" - Mean: {dist_non$avg_m} m; Median: {dist_non$median_m} m; N (with distance): {dist_non$n}")
  )
  cat(paste(narr_A6, collapse = "\n"), "\n")
  save_narrative(narr_A6, "A6_avg_distance_non_hfhk_users_narrative.txt")
} else {
  log_msg("Distance column not found in either dataset; skipping A5 and A6.")
  save_narrative("Distance column not found in either dataset; A5/A6 skipped.", "A5_A6_distance_missing.txt")
}

# A7) Frequencies of water availability year-round
year_tbl <- rel %>%
  transmute(year_round = ifelse(is.na(water_year_flag), "Unknown", ifelse(water_year_flag, "Yes", "No"))) %>%
  count(year_round) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(year_tbl, "A7_year_round_availability.csv")

p_year <- ggplot(year_tbl, aes(x=year_round, y=pct, fill=year_round)) +
  geom_col(show.legend = FALSE) + geom_text(aes(label=paste0(pct,"%")), vjust=-0.2) +
  labs(title = "Year-round Water Availability (relevance dataset)", x=NULL, y="Percent") + theme_minimal()
save_plot(p_year, "A7_year_round_availability.png")

narr_A7 <- c(
  "A7/A8: Year-round water availability",
  paste0(" - Yes: ", (year_tbl %>% filter(year_round=="Yes") %>% pull(n) %||% 0), " (", (year_tbl %>% filter(year_round=="Yes") %>% pull(pct) %||% 0), "%)"),
  paste0(" - No: ", (year_tbl %>% filter(year_round=="No") %>% pull(n) %||% 0), " (", (year_tbl %>% filter(year_round=="No") %>% pull(pct) %||% 0), "%)"),
  paste0(" - Unknown/missing: ", (year_tbl %>% filter(year_round=="Unknown") %>% pull(n) %||% 0))
)
cat(paste(narr_A7, collapse = "\n"), "\n")
save_narrative(narr_A7, "A7_year_round_availability_narrative.txt")

# A9) How many pay and not pay for water (overall)
pay_tbl <- rel %>%
  transmute(pays = ifelse(is.na(pay_for_water_flag), "Unknown", ifelse(pay_for_water_flag, "Yes", "No"))) %>%
  count(pays) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(pay_tbl, "A9_pay_water_overall.csv")
narr_A9 <- c("A9: Payment for water (overall)",
             paste0(apply(pay_tbl, 1, function(r) paste0(" - ", r["pays"], ": ", r["n"], " (", r["pct"], "%)")), collapse = "\n"))
cat(paste(narr_A9, collapse = "\n"), "\n")
save_narrative(narr_A9, "A9_pay_water_overall_narrative.txt")

# A10 & A11) How many pay / not pay among HFHK kiosk users
pay_kiosk_tbl <- rel %>%
  filter(hfhk_water_user) %>%
  transmute(pays = ifelse(is.na(pay_for_water_flag), "Unknown", ifelse(pay_for_water_flag, "Yes", "No"))) %>%
  count(pays) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(pay_kiosk_tbl, "A10_A11_pay_water_hfhk_kiosk_users.csv")
narr_A10 <- c("A10/A11: Payment for water among HFHK kiosk primary-source households",
              if(nrow(pay_kiosk_tbl)==0) " - No HFHK kiosk users identified." else paste0(apply(pay_kiosk_tbl,1,function(r) paste0(" - ", r["pays"], ": ", r["n"], " (", r["pct"], "%)")), collapse = "\n"))
cat(paste(narr_A10, collapse = "\n"), "\n")
save_narrative(narr_A10, "A10_A11_pay_water_hfhk_kiosk_users_narrative.txt")

# A12) Frequencies of HFHK pay (per 20L) among HFHK users who pay
pay_freq_tbl <- rel %>%
  filter(hfhk_water_user, pay_for_water_flag) %>%
  { if("pay_frequency_20l" %in% names(.)) count(., pay_frequency = pay_frequency_20l) else tibble(pay_frequency = character(), n = integer()) } %>%
  mutate(pct = round(100*n/sum(n),1)) %>%
  arrange(desc(n))
save_tbl(pay_freq_tbl, "A12_pay_frequency_20l_hfhk_users.csv")
narr_A12 <- c("A12: Payment frequency per 20L among HFHK users who pay",
              if(nrow(pay_freq_tbl)==0) " - No records" else apply(head(pay_freq_tbl,8), 1, function(r) glue(" - {r['pay_frequency']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_A12, collapse = "\n"), "\n")
save_narrative(narr_A12, "A12_pay_frequency_20l_hfhk_users_narrative.txt")

# A13) Average they pay for HFHK kiosks (per 20L)
if(!is.null(price_col)){
  price_tbl <- rel %>%
    filter(hfhk_water_user, pay_for_water_flag) %>%
    summarise(
      avg_price_20l = round(mean(.data[[price_col]], na.rm = TRUE),2),
      median_price_20l = round(median(.data[[price_col]], na.rm = TRUE),2),
      n = sum(!is.na(.data[[price_col]]))
    )
  save_tbl(price_tbl, "A13_avg_kiosk_price_20l_hfhk_users.csv")
  narr_A13 <- c("A13: Average HFHK kiosk price per 20L (among HFHK users who pay)",
                if(price_tbl$n==0) " - No numeric price data found." else glue(" - Mean: {price_tbl$avg_price_20l}; Median: {price_tbl$median_price_20l}; N: {price_tbl$n}"))
} else {
  price_tbl <- tibble(note = "price column not found")
  save_tbl(price_tbl, "A13_avg_kiosk_price_20l_hfhk_users_missing.csv")
  narr_A13 <- c("A13: Kiosk price per 20L - price column not found in datasets.")
}
cat(paste(narr_A13, collapse = "\n"), "\n")
save_narrative(narr_A13, "A13_avg_kiosk_price_20l_hfhk_users_narrative.txt")

# A14) Cost affordability by village, gender (relevance dataset)
if("water_cost_affordable" %in% names(rel)){
  afford_tbl <- rel %>%
    filter(!is.na(water_cost_affordable) & water_cost_affordable != "") %>%
    group_by(village, gender, water_cost_affordable) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(afford_tbl, "A14_cost_affordability_by_village_gender.csv")
  # narrative: show top response per village
  top_afford <- afford_tbl %>%
    group_by(village) %>%
    arrange(village, desc(n)) %>%
    slice_head(n = 1) %>%
    ungroup()
  narr_A14 <- c("A14: Cost affordability by village and gender (top response per village):",
                if(nrow(top_afford)==0) " - No affordability data." else apply(top_afford,1,function(r) glue(" - {r['village']}: Most common = {r['water_cost_affordable']} ({r['n']} households, {r['pct']}%)")))
  cat(paste(narr_A14, collapse = "\n"), "\n")
  save_narrative(narr_A14, "A14_cost_affordability_by_village_gender_narrative.txt")
} else {
  log_msg("A14: water_cost_affordable column not found; skipping.")
  save_narrative("A14: water_cost_affordable column not found; A14 skipped.", "A14_missing.txt")
}

# A15) HFHK latrines functioning well? (Yes/No), segregate by using HFHK latrine vs not (effectiveness dataset preferred)
if("latrine_functional_during_flood" %in% names(eff) & "hfkh_latrine_use" %in% names(eff)){
  lat_func_tbl <- eff %>%
    filter(!is.na(latrine_functional_during_flood), str_trim(latrine_functional_during_flood) != "") %>%
    mutate(hfkh_latrine_user_flag = tolower(as.character(hfkh_latrine_use)) %in% c("yes","y","1","true")) %>%
    group_by(hfkh_latrine_user_flag, latrine_functional_during_flood) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(lat_func_tbl, "A15_latrine_function_flood_by_hfkh_use.csv")
  narr_A15 <- c("A15: Latrine functional during flood (effectiveness dataset):",
                if(nrow(lat_func_tbl)==0) " - No data." else apply(lat_func_tbl,1,function(r) glue(" - HFHK user = {r['hfkh_latrine_user_flag']}; Functional = {r['latrine_functional_during_flood']}; n={r['n']}; pct={r['pct']}%")))
  cat(paste(narr_A15, collapse = "\n"), "\n")
  save_narrative(narr_A15, "A15_latrine_function_flood_by_hfkh_use_narrative.txt")
} else {
  log_msg("A15: Needed latrine fields not present in effectiveness dataset; skipping.")
  save_narrative("A15: latrine_functional_during_flood or hfkh_latrine_use not found; skipped.", "A15_missing.txt")
}

# A16) Has home/farm been affected by flood (yes/no) - effectiveness dataset
if("home_affected_by_flood" %in% names(eff) | "home_affected_by_flood_last_year" %in% names(eff)){
  flood_col <- first_existing_col(eff, c("home_affected_by_flood","home_affected_by_flood_last_year"))
  flood_aff_tbl <- eff %>%
    filter(!is.na(.data[[flood_col]]), str_trim(.data[[flood_col]]) != "") %>%
    count(home_affected_by_flood = .data[[flood_col]]) %>%
    mutate(pct = round(100*n/sum(n),1))
  save_tbl(flood_aff_tbl, "A16_home_affected_by_flood.csv")
  p_flood_aff <- ggplot(flood_aff_tbl, aes(x=reorder(home_affected_by_flood, -pct), y=pct)) +
    geom_col(fill="orange") + geom_text(aes(label=paste0(pct,"%")), vjust=-0.2) +
    labs(title="Home/Farm affected by flood (past year)", x=NULL, y="Percent") + theme_minimal()
  save_plot(p_flood_aff, "A16_home_affected_by_flood.png")
  narr_A16 <- c("A16: Home/farm affected by flood (effectiveness dataset):",
                if(nrow(flood_aff_tbl)==0) " - No data" else apply(flood_aff_tbl,1,function(r) glue(" - {r['home_affected_by_flood']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_A16, collapse = "\n"), "\n")
  save_narrative(narr_A16, "A16_home_affected_by_flood_narrative.txt")
} else {
  log_msg("A16: home_affected_by_flood variable not found; skipping.")
  save_narrative("A16: home_affected_by_flood variable not found; skipped.", "A16_missing.txt")
}

log_msg("PART A complete: overall concerns results saved to {out_dir}")




############################################################
# relevance_effectiveness_partB.R
# Part B: Updates on Relevance Criteria (B block)
############################################################

# --- Assumes Part A executed above in the same R session (rel, eff, helpers, out_dir available) ---

log_msg("Starting PART B: Relevance updates")

# B1) Paying for 'insurance' or payment breakdown placeholder (you indicated Q2.8 / 3.20-3.22)
# We'll use 'pay_for_water' as proxy if specific 'insurance' column not present.
ins_col_candidates <- c("paying_insurance","insurance_payment","pay_insurance","pay_for_water")
ins_col <- first_existing_col(rel, ins_col_candidates)
if(!is.null(ins_col)){
  ins_tbl <- rel %>% filter(!is.na(.data[[ins_col]])) %>%
    count(pays_insurance = .data[[ins_col]]) %>%
    mutate(pct = round(100*n/sum(n),1))
  save_tbl(ins_tbl, "B_insurance_payment_counts_pct.csv")
  narr_B1 <- c("B1: Insurance / payment breakdown (proxy column used: {ins_col}):",
               if(nrow(ins_tbl)==0) " - No records." else apply(ins_tbl,1,function(r) glue(" - {r['pays_insurance']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B1, collapse = "\n"), "\n")
  save_narrative(narr_B1, "B1_insurance_payment_narrative.txt")
} else {
  log_msg("B1: No insurance-related columns found; skipping.")
  save_narrative("B1: No insurance-related columns found; skipped.", "B1_insurance_missing.txt")
}

# B2) 3.2 Handwash knowledge - already saved in PART A? Recompute with demographics breakdown
if(all(c("handwash_after_toilet","handwash_before_eating","handwash_before_cooking","handwash_after_waste","handwash_after_cleaning") %in% names(rel))){
  hw_knowledge_df <- rel %>%
    transmute(
      village = if("village" %in% names(rel)) village else NA_character_,
      gender = if("gender_hh" %in% names(rel)) gender_hh else NA_character_,
      after_toilet = str_to_lower(handwash_after_toilet) %in% c("yes","y","1","true"),
      before_eating = str_to_lower(handwash_before_eating) %in% c("yes","y","1","true"),
      before_cooking = str_to_lower(handwash_before_cooking) %in% c("yes","y","1","true"),
      after_waste = str_to_lower(handwash_after_waste) %in% c("yes","y","1","true"),
      after_cleaning = str_to_lower(handwash_after_cleaning) %in% c("yes","y","1","true")
    ) %>%
    mutate(n_critical = after_toilet + before_eating + before_cooking + after_waste + after_cleaning,
           at_least_3 = n_critical >= 3)
  hw3_by_demo <- hw_knowledge_df %>%
    group_by(village, gender) %>%
    summarise(total = n(), at_least_3 = sum(at_least_3, na.rm=TRUE), pct_at_least_3 = round(100*at_least_3/total,1), .groups="drop")
  save_tbl(hw3_by_demo, "B_q3_2_handwash_knowledge_at_least3_by_village_gender.csv")
  narr_B2 <- c("B2 (Q3.2): Handwash knowledge - % households that know >=3 critical times (by village & gender):",
               if(nrow(hw3_by_demo)==0) " - No data" else apply(hw3_by_demo,1,function(r) glue(" - {r['village']} / {r['gender']}: {r['at_least_3']}/{r['total']} ({r['pct_at_least_3']}%)")))
  cat(paste(narr_B2, collapse = "\n"), "\n")
  save_narrative(narr_B2, "B2_handwash_knowledge_by_village_gender_narrative.txt")
} else {
  log_msg("B2: Handwash knowledge columns missing in relevance dataset; skipping detailed breakdown.")
  save_narrative("B2: Handwash knowledge columns missing; skipped.", "B2_missing.txt")
}

# B3) 3.3: Among HFHK water users, how many report water_year_round = Yes
q33_tbl <- rel %>%
  filter(hfhk_water_user) %>%
  transmute(year_round = ifelse(is.na(water_year_flag), "Unknown", ifelse(water_year_flag, "Yes","No"))) %>%
  count(year_round) %>%
  mutate(pct = round(100*n/sum(n),1))
save_tbl(q33_tbl, "B_q3_3_hfhk_users_year_round_yes.csv")
narr_B3 <- c("B3 (Q3.3): Among HFHK water primary-source households, year-round availability:",
             if(nrow(q33_tbl)==0) " - No HFHK users or no year-round data." else apply(q33_tbl,1,function(r) glue(" - {r['year_round']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_B3, collapse = "\n"), "\n")
save_narrative(narr_B3, "B3_hfhk_users_year_round_narrative.txt")

# B4) 3.4: How many that use HFHK pay? How many that do NOT use HFHK pay?
q34_tbl <- rel %>%
  transmute(hfhk_user = ifelse(hfhk_water_user, "HFHK user","Non-HFHK"), pays = ifelse(is.na(pay_for_water_flag),"Unknown", ifelse(pay_for_water_flag,"Yes","No"))) %>%
  count(hfhk_user, pays) %>%
  group_by(hfhk_user) %>%
  mutate(pct = round(100*n/sum(n),1)) %>%
  ungroup()
save_tbl(q34_tbl, "B_q3_4_pay_by_hfhk_use.csv")
# narrative
narr_B4 <- c("B4 (Q3.4): Payment status by HFHK user vs Non-user",
             if(nrow(q34_tbl)==0) " - No data" else apply(q34_tbl,1,function(r) glue(" - {r['hfhk_user']} / Pays={r['pays']}: {r['n']} ({r['pct']}%)")))
cat(paste(narr_B4, collapse = "\n"), "\n")
save_narrative(narr_B4, "B4_pay_by_hfhk_use_narrative.txt")

# B5) 3.5: Payment frequency distribution overall (already saved earlier, but produce narrative)
if("pay_frequency_20l" %in% names(rel)){
  q35_tbl <- rel %>% filter(!is.na(pay_frequency_20l) & pay_frequency_20l!="") %>%
    count(pay_frequency = pay_frequency_20l) %>% mutate(pct = round(100*n/sum(n),1)) %>% arrange(desc(n))
  save_tbl(q35_tbl, "B_q3_5_pay_frequency_overall.csv")
  narr_B5 <- c("B5 (Q3.5): Payment frequency per 20L (overall)",
               if(nrow(q35_tbl)==0) " - No data." else apply(head(q35_tbl,10),1,function(r) glue(" - {r['pay_frequency']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B5, collapse = "\n"), "\n")
  save_narrative(narr_B5, "B5_pay_frequency_overall_narrative.txt")
} else {
  log_msg("B5: pay_frequency_20l column not found; skipping.")
  save_narrative("B5: pay_frequency_20l not found; skipped.", "B5_missing.txt")
}

# B6) 3.6 Cost affordability overall (already saved earlier but with narrative)
if("water_cost_affordable" %in% names(rel)){
  q36_tbl <- rel %>% filter(!is.na(water_cost_affordable) & water_cost_affordable!="") %>%
    count(cost_affordable = water_cost_affordable) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    arrange(desc(n))
  save_tbl(q36_tbl, "B_q3_6_cost_affordability_overall.csv")
  narr_B6 <- c("B6 (Q3.6): Cost affordability overall",
               if(nrow(q36_tbl)==0) " - No data." else apply(head(q36_tbl,10),1,function(r) glue(" - {r['cost_affordable']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B6, collapse = "\n"), "\n")
  save_narrative(narr_B6, "B6_cost_affordability_overall_narrative.txt")
} else {
  log_msg("B6: water_cost_affordable column not found; skipping.")
  save_narrative("B6: water_cost_affordable not found; skipped.", "B6_missing.txt")
}

# B7) 3.7: Those who use HFHK — is it affordable? Those who don't use — is it affordable?
if("water_cost_affordable" %in% names(rel)){
  q37_tbl <- rel %>% filter(!is.na(water_cost_affordable) & water_cost_affordable!="") %>%
    transmute(hfhk_user = ifelse(hfhk_water_user, "HFHK user","Non-HFHK"), cost_affordable = water_cost_affordable) %>%
    count(hfhk_user, cost_affordable) %>%
    group_by(hfhk_user) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(q37_tbl, "B_q3_7_affordability_by_hfhk_use.csv")
  narr_B7 <- c("B7 (Q3.7): Affordability among HFHK users vs non-users (top categories)",
               if(nrow(q37_tbl)==0) " - No data." else apply(head(q37_tbl,20),1,function(r) glue(" - {r['hfhk_user']} / {r['cost_affordable']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B7, collapse = "\n"), "\n")
  save_narrative(narr_B7, "B7_affordability_by_hfhk_use_narrative.txt")
} else {
  log_msg("B7: water_cost_affordable not found; skipping B7.")
  save_narrative("B7: water_cost_affordable not found; skipped.", "B7_missing.txt")
}

# B9) 3.9 PWD ease use overall
if("pwd_ease_use" %in% names(rel)){
  q39_tbl <- rel %>% filter(!is.na(pwd_ease_use) & pwd_ease_use != "") %>%
    count(pwd_ease = pwd_ease_use) %>%
    mutate(pct = round(100*n/sum(n),1))
  save_tbl(q39_tbl, "B_q3_9_pwd_ease_use.csv")
  narr_B9 <- c("B9 (Q3.9): Can PWDs use water points/latrines easily?",
               if(nrow(q39_tbl)==0) " - No data." else apply(q39_tbl,1,function(r) glue(" - {r['pwd_ease']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B9, collapse = "\n"), "\n")
  save_narrative(narr_B9, "B9_pwd_ease_use_narrative.txt")
} else {
  log_msg("B9: pwd_ease_use column not found; skipping B9.")
  save_narrative("B9: pwd_ease_use not found; skipped.", "B9_missing.txt")
}

# B10/B11) 3.10/3.11 Safe & usable latrines by HFHK user vs non-user (relevance dataset)
if("latrine_flood_function" %in% names(rel) | "latrine_safe_heavy_rain" %in% names(rel)){
  lat_func_field <- first_existing_col(rel, c("latrine_flood_function","latrine_safe_heavy_rain","latrine_safe_heavy_rain"))
  q310_tbl <- rel %>% filter(!is.na(.data[[lat_func_field]]), str_trim(.data[[lat_func_field]])!="") %>%
    transmute(hfhk_user = ifelse(hfhk_water_user,"HFHK user","Non-HFHK"), latrine_status = .data[[lat_func_field]]) %>%
    count(hfhk_user, latrine_status) %>%
    group_by(hfhk_user) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(q310_tbl, "B_q3_10_11_latrine_safe_by_hfhk_user.csv")
  narr_B10 <- c("B10/B11 (Q3.10/3.11): Latrine safe/usable during floods by HFHK user vs non-user",
                if(nrow(q310_tbl)==0) " - No data." else apply(q310_tbl,1,function(r) glue(" - {r['hfhk_user']} / {r['latrine_status']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B10, collapse = "\n"), "\n")
  save_narrative(narr_B10, "B10_11_latrine_safe_by_hfhk_user_narrative.txt")
} else {
  log_msg("B10/B11: latrine safe/function columns not found in relevance dataset; skipping.")
  save_narrative("B10/B11: latrine safe not found; skipped.", "B10_B11_missing.txt")
}

# B12) 3.12 For those who use HFHK latrine, how many households 'all_members_use_latrine'
if("all_members_use_latrine" %in% names(rel)){
  rel_q312 <- rel %>%
    filter(hfhk_latrine_user) %>%
    filter(!is.na(all_members_use_latrine), all_members_use_latrine != "") %>%
    count(all_use = all_members_use_latrine) %>%
    mutate(pct = round(100*n/sum(n),1))
  save_tbl(rel_q312, "B_q3_12_all_members_use_latrine_among_hfhk_latrines.csv")
  narr_B12 <- c("B12 (Q3.12): Among HFHK latrine users, do all household members use the latrine?",
                if(nrow(rel_q312)==0) " - No data." else apply(rel_q312,1,function(r) glue(" - {r['all_use']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_B12, collapse = "\n"), "\n")
  save_narrative(narr_B12, "B12_all_members_use_hfhk_latrines_narrative.txt")
} else {
  log_msg("B12: all_members_use_latrine column not found; skipping.")
  save_narrative("B12: all_members_use_latrine not found; skipped.", "B12_missing.txt")
}

log_msg("PART B complete: relevance updates results saved to {out_dir}")





############################################################
# relevance_effectiveness_partC.R
# Part C: Updates on Effectiveness Criteria (C block) + final summary
############################################################

# --- Assumes Parts A & B executed above in the same R session (rel, eff, helpers, out_dir available) ---

log_msg("Starting PART C: Effectiveness updates")

# C1) Disaggregate by village & gender - kiosk use frequency
if("kiosk_use_frequency" %in% names(eff)) {
  kiosk_disagg <- eff %>%
    filter(!is.na(kiosk_use_frequency), str_trim(kiosk_use_frequency)!="") %>%
    group_by(village, gender_hh, kiosk_use_frequency) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(kiosk_disagg, "C_kiosk_use_frequency_by_village_gender.csv")
  p_kiosk_use <- ggplot(kiosk_disagg, aes(x=kiosk_use_frequency, y=pct, fill=gender_hh)) +
    geom_col(position = position_dodge()) + facet_wrap(~village) +
    labs(title="HFHK Kiosk Use Frequency by Village & Gender", x=NULL, y="Percent") + theme_minimal()
  save_plot(p_kiosk_use, "C_kiosk_use_frequency_by_village_gender.png", w=12, h=8)
  narr_C1 <- c("C1: Kiosk use frequency by village & gender (top lines):",
               if(nrow(kiosk_disagg)==0) " - No data." else apply(head(kiosk_disagg,20),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['kiosk_use_frequency']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C1, collapse = "\n"), "\n")
  save_narrative(narr_C1, "C1_kiosk_use_by_village_gender_narrative.txt")
} else {
  log_msg("C1: kiosk_use_frequency column missing in effectiveness dataset; skipping.")
  save_narrative("C1: kiosk_use_frequency missing; skipped.", "C1_missing.txt")
}

# C2) 4.25: Avg time to fetch water per gender per village (hours/min bins) - numeric table + categorical %
if("time_now_min" %in% names(eff)){
  eff_time_cat <- eff %>%
    filter(!is.na(time_now_min)) %>%
    mutate(time_cat = case_when(
      time_now_min < 30 ~ "<30 min",
      time_now_min <= 60 ~ "30-60 min",
      time_now_min > 60 ~ ">1 hour",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(time_cat))
  time_avg_tbl <- eff %>%
    group_by(village, gender_hh) %>%
    summarise(
      avg_min = round(mean(time_now_min, na.rm = TRUE),1),
      median_min = round(median(time_now_min, na.rm = TRUE),1),
      n = sum(!is.na(time_now_min)),
      .groups = "drop"
    ) %>%
    mutate(avg_hh_mm = paste0(floor(avg_min/60), "h ", round(avg_min %% 60), "m"))
  save_tbl(time_avg_tbl, "C_4_25_avg_time_fetch_by_village_gender.csv")
  time_pct_tbl <- eff_time_cat %>%
    count(village, gender_hh, time_cat) %>%
    group_by(village, gender_hh) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(time_pct_tbl, "C_4_25_time_category_pct_by_village_gender.csv")
  p_time_cat <- ggplot(time_pct_tbl, aes(x=time_cat, y=pct, fill=gender_hh)) +
    geom_col(position = position_dodge()) + facet_wrap(~village) +
    labs(title="Time to Fetch Water (now): % by Category and Gender", x=NULL, y="Percent") + theme_minimal()
  save_plot(p_time_cat, "C_4_25_time_category_pct_by_village_gender.png", w=12, h=8)
  narr_C2 <- c("C2 (4.25): Average time to fetch water now (by village and gender):",
               if(nrow(time_avg_tbl)==0) " - No numeric time data." else apply(time_avg_tbl,1,function(r) glue(" - {r['village']} / {r['gender_hh']}: Mean={r['avg_min']} min (n={r['n']}); {r['avg_hh_mm']}")))
  cat(paste(narr_C2, collapse = "\n"), "\n")
  save_narrative(narr_C2, "C2_time_fetch_by_village_gender_narrative.txt")
} else {
  log_msg("C2: time_fetch_now/min column missing; skipping time analysis.")
  save_narrative("C2: time_fetch_now missing; skipped.", "C2_missing.txt")
}

# C3) 4.30: water quality change by gender & village (if present)
if("water_quality_change" %in% names(eff)){
  q430_tbl <- eff %>% filter(!is.na(water_quality_change), water_quality_change != "") %>%
    count(village, gender_hh, water_quality = water_quality_change) %>%
    group_by(village, gender_hh) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(q430_tbl, "C_4_30_water_quality_by_village_gender.csv")
  narr_C3 <- c("C3 (4.30): Water quality change (by village & gender):",
               if(nrow(q430_tbl)==0) " - No data." else apply(head(q430_tbl,20),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['water_quality']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C3, collapse = "\n"), "\n")
  save_narrative(narr_C3, "C3_water_quality_change_narrative.txt")
} else {
  log_msg("C3: water_quality_change column missing; skipping.")
  save_narrative("C3: water_quality_change missing; skipped.", "C3_missing.txt")
}

# C4) 4.31: Borehole functional by village
if("borehole_functional" %in% names(eff)){
  q431_tbl <- eff %>% filter(!is.na(borehole_functional), borehole_functional!="") %>%
    count(village, borehole_func = borehole_functional) %>%
    group_by(village) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(q431_tbl, "C_4_31_borehole_functional_by_village.csv")
  narr_C4 <- c("C4 (4.31): Borehole functional by village",
               if(nrow(q431_tbl)==0) " - No data." else apply(head(q431_tbl,50),1,function(r) glue(" - {r['village']} / {r['borehole_func']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C4, collapse = "\n"), "\n")
  save_narrative(narr_C4, "C4_borehole_functional_by_village_narrative.txt")
} else {
  log_msg("C4: borehole_functional column missing; skipping.")
  save_narrative("C4: borehole_functional missing; skipped.", "C4_missing.txt")
}

# C5) 4.32: For HFHK kiosks & piped water get % experienced shortages & frequency
if("borehole_breakdowns" %in% names(eff)){
  shortages_tbl <- eff %>% filter(!is.na(borehole_breakdowns) & borehole_breakdowns != "") %>%
    count(borehole_breakdowns) %>%
    mutate(pct = round(100*n/sum(n),1))
  save_tbl(shortages_tbl, "C_4_32_shortages_overall.csv")
  narr_C5 <- c("C5 (4.32): Reported shortages/breakdowns (overall):",
               if(nrow(shortages_tbl)==0) " - No records." else apply(shortages_tbl,1,function(r) glue(" - {r['borehole_breakdowns']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C5, collapse = "\n"), "\n")
  save_narrative(narr_C5, "C5_shortages_overall_narrative.txt")
  
  if("breakdown_frequency" %in% names(eff)){
    how_often_tbl <- eff %>%
      filter(tolower(borehole_breakdowns) %in% c("yes","y","true","1"), !is.na(breakdown_frequency), breakdown_frequency != "") %>%
      count(breakdown_frequency) %>%
      mutate(pct = round(100*n/sum(n),1))
    save_tbl(how_often_tbl, "C_4_32_breakdown_frequency.csv")
    narr_how_often <- c("C5b: Breakdown frequency among those reporting breakdowns:",
                        if(nrow(how_often_tbl)==0) " - No frequency data." else apply(how_often_tbl,1,function(r) glue(" - {r['breakdown_frequency']}: {r['n']} ({r['pct']}%)")))
    cat(paste(narr_how_often, collapse = "\n"), "\n")
    save_narrative(narr_how_often, "C5b_breakdown_frequency_narrative.txt")
  } else {
    log_msg("C5b: breakdown_frequency not present; skipping frequency breakdown.")
    save_narrative("C5b: breakdown_frequency not present; skipped.", "C5b_missing.txt")
  }
} else {
  log_msg("C5: borehole_breakdowns column not present; skipping.")
  save_narrative("C5: borehole_breakdowns not present; skipped.", "C5_missing.txt")
}

# C6) 3.13: % with handwashing station at home by gender & village
if("place_to_wash_hands" %in% names(eff)){
  q313_tbl <- eff %>% filter(!is.na(place_to_wash_hands) & place_to_wash_hands!="") %>%
    count(village, gender_hh, place_to_wash_hands) %>%
    group_by(village, gender_hh) %>%
    mutate(pct = round(100*n/sum(n),1)) %>%
    ungroup()
  save_tbl(q313_tbl, "C_3_13_place_to_wash_hands_by_gender_village.csv")
  narr_C6 <- c("C6 (3.13): Place to wash hands by village and gender:",
               if(nrow(q313_tbl)==0) " - No data." else apply(head(q313_tbl,40),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['place_to_wash_hands']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C6, collapse = "\n"), "\n")
  save_narrative(narr_C6, "C6_place_to_wash_hands_narrative.txt")
} else {
  log_msg("C6: place_to_wash_hands not present; skipping.")
  save_narrative("C6: place_to_wash_hands not present; skipped.", "C6_missing.txt")
}

# C7) 3.14: % who wash hands at least 3 critical times (effectiveness)
if(all(c("after_defecation_handwash_often","before_eating_handwash_often","before_preparing_food_handwash_often","after_cleaning_environment_handwash_often") %in% names(eff))){
  hw_often_df <- eff %>%
    transmute(
      village,
      gender = gender_hh,
      after_def = str_to_lower(after_defecation_handwash_often) %in% c("often","always","yes","y","1","true"),
      before_eat = str_to_lower(before_eating_handwash_often) %in% c("often","always","yes","y","1","true"),
      before_prep = str_to_lower(before_preparing_food_handwash_often) %in% c("often","always","yes","y","1","true"),
      after_clean = str_to_lower(after_cleaning_environment_handwash_often) %in% c("often","always","yes","y","1","true")
    ) %>%
    mutate(n_often = after_def + before_eat + before_prep + after_clean, at_least_3 = n_often >= 3)
  q314_tbl <- hw_often_df %>% count(village, gender, at_least_3) %>%
    group_by(village, gender) %>% mutate(pct = round(100*n/sum(n),1)) %>% ungroup()
  save_tbl(q314_tbl, "C_3_14_at_least3_critical_times_by_gender_village.csv")
  narr_C7 <- c("C7 (3.14): % practicing handwashing 'often' at >=3 critical times (by village & gender):",
               if(nrow(q314_tbl)==0) " - No data." else apply(head(q314_tbl,50),1,function(r) glue(" - {r['village']} / {r['gender']}: at_least_3={r['at_least_3']}; n={r['n']}; pct={r['pct']}%")))
  cat(paste(narr_C7, collapse = "\n"), "\n")
  save_narrative(narr_C7, "C7_at_least3_handwash_narrative.txt")
} else {
  log_msg("C7: Not all handwash 'often' columns present; skipping.")
  save_narrative("C7: handwash 'often' columns missing; skipped.", "C7_missing.txt")
}

# C8) 3.15: Hygiene training reduced illness by gender & village
if("hygiene_training_reduced_illness" %in% names(eff)){
  q315_tbl <- eff %>% filter(!is.na(hygiene_training_reduced_illness) & hygiene_training_reduced_illness!="") %>%
    count(village, gender_hh, reduced_illness = hygiene_training_reduced_illness) %>%
    group_by(village, gender_hh) %>% mutate(pct = round(100*n/sum(n),1)) %>% ungroup()
  save_tbl(q315_tbl, "C_3_15_hygiene_training_reduced_illness_by_gender_village.csv")
  narr_C8 <- c("C8 (3.15): Reported reduction in illness attributed to hygiene training (by village & gender):",
               if(nrow(q315_tbl)==0) " - No data." else apply(head(q315_tbl,50),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['reduced_illness']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C8, collapse = "\n"), "\n")
  save_narrative(narr_C8, "C8_hygiene_training_reduced_illness_narrative.txt")
} else {
  log_msg("C8: hygiene_training_reduced_illness missing; skipping.")
  save_narrative("C8: hygiene_training_reduced_illness missing; skipped.", "C8_missing.txt")
}

# C9) 4.60 - 4.66: awareness, canal works, effectiveness (if present)
if("usefulness_awareness_sessions" %in% names(eff)){
  q460_tbl <- eff %>% filter(!is.na(usefulness_awareness_sessions) & usefulness_awareness_sessions!="") %>%
    count(village, gender_hh, usefulness = usefulness_awareness_sessions) %>%
    group_by(village, gender_hh) %>% mutate(pct = round(100*n/sum(n),1)) %>% ungroup()
  save_tbl(q460_tbl, "C_4_60_usefulness_awareness_by_village_gender.csv")
  narr_C9a <- c("C9a (4.60): Usefulness of awareness sessions by village & gender (top lines):",
                if(nrow(q460_tbl)==0) " - No data." else apply(head(q460_tbl,40),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['usefulness']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C9a, collapse = "\n"), "\n")
  save_narrative(narr_C9a, "C9a_usefulness_awareness_narrative.txt")
}

if("canal_works_helped_reduce_flooding" %in% names(eff)){
  q461_tbl <- eff %>% filter(!is.na(canal_works_helped_reduce_flooding) & canal_works_helped_reduce_flooding!="") %>%
    count(village, gender_hh, helped = canal_works_helped_reduce_flooding) %>%
    group_by(village, gender_hh) %>% mutate(pct = round(100*n/sum(n),1)) %>% ungroup()
  save_tbl(q461_tbl, "C_4_61_canal_works_helped_by_village_gender.csv")
  narr_C9b <- c("C9b (4.61): Did canal works help reduce flooding? (by village & gender)",
                if(nrow(q461_tbl)==0) " - No data." else apply(head(q461_tbl,60),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['helped']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C9b, collapse = "\n"), "\n")
  save_narrative(narr_C9b, "C9b_canal_works_helped_narrative.txt")
}

if("effective_desilted_canals_and_embankments_reducing_floods" %in% names(eff)){
  q466_tbl <- eff %>% filter(!is.na(effective_desilted_canals_and_embankments_reducing_floods) & effective_desilted_canals_and_embankments_reducing_floods!="") %>%
    count(village, gender_hh, effectiveness = effective_desilted_canals_and_embankments_reducing_floods) %>%
    group_by(village, gender_hh) %>% mutate(pct = round(100*n/sum(n),1)) %>% ungroup()
  save_tbl(q466_tbl, "C_4_66_effectiveness_category_by_village_gender.csv")
  narr_C9c <- c("C9c (4.66): Effectiveness of desilted canals and embankments (by village & gender)",
                if(nrow(q466_tbl)==0) " - No data." else apply(head(q466_tbl,80),1,function(r) glue(" - {r['village']} / {r['gender_hh']} / {r['effectiveness']}: {r['n']} ({r['pct']}%)")))
  cat(paste(narr_C9c, collapse = "\n"), "\n")
  save_narrative(narr_C9c, "C9c_desilted_canals_effectiveness_narrative.txt")
}

# ----------------------------
# Final consolidated short summary (text file + console)
# ----------------------------
summary_text <- list()
summary_text <- c(summary_text, "Consolidated Summary: Relevance & Effectiveness (selected highlights)")
# Awareness
if(exists("aware_tbl")) {
  ay <- aware_tbl %>% filter(aware=="Yes") %>% pull(pct) %||% NA
  summary_text <- c(summary_text, glue(" - Awareness of HFHK project: {ay}% reported 'Yes'."))
}
# Year-round water
if(exists("year_tbl")) {
  yr <- year_tbl %>% filter(year_round=="Yes") %>% pull(pct) %||% NA
  summary_text <- c(summary_text, glue(" - Year-round water availability (Yes): {yr}%."))
}
# Paying overall
if(exists("pay_tbl")) {
  py <- pay_tbl %>% filter(pays=="Yes") %>% pull(pct) %||% NA
  summary_text <- c(summary_text, glue(" - Overall households paying for water: {py}%."))
}
# HFHK users paying
if(exists("pay_kiosk_tbl")) {
  pk <- pay_kiosk_tbl %>% filter(pays=="Yes") %>% pull(pct) %||% NA
  summary_text <- c(summary_text, glue(" - HFHK kiosk users paying for water: {pk}%."))
}
# Flood affected
if(exists("flood_aff_tbl")) {
  fy <- flood_aff_tbl %>% filter(str_to_lower(home_affected_by_flood) %in% c("yes","y","1","true")) %>% pull(pct) %||% NA
  summary_text <- c(summary_text, glue(" - Proportion reporting home/farm affected by flood in past year: {fy}%."))
}
# Time-fetch average (if computed)
if(exists("time_avg_tbl")) {
  overall_avg <- time_avg_tbl %>% summarise(avg = round(mean(avg_min, na.rm=TRUE),1)) %>% pull(avg) %||% NA
  summary_text <- c(summary_text, glue(" - Average time to fetch water now (mean minutes across groups): {overall_avg} minutes."))
}
# Save consolidated summary
save_narrative(summary_text, "CONSOLIDATED_SUMMARY_relevance_effectiveness.txt")
cat(paste(summary_text, collapse = "\n"), "\n")
log_msg("PART C complete and consolidated summary saved to {out_dir}")
log_msg("All parts A, B, C finished. Check the output folder for CSVs, PNGs and TXT narratives.")


