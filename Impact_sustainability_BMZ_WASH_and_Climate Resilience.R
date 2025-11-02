# ============================
# Combined Impact & Sustainability / Efficiency Analysis
# Outputs -> C:/Users/Hp/Downloads/BMZ_OUTPUTS
# ============================

# load libs
library(tidyverse)
library(janitor)
library(ggplot2)
library(wordcloud)
library(tm)
library(openxlsx)
library(sf)
library(ggthemes)
library(RColorBrewer)

# -----------------------------
# Set Output Directory
# -----------------------------
out_dir <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -----------------------------
# Load Data
# -----------------------------
# adjust filename/path if needed
data_path <- "C:/Users/Hp/Downloads/community_impact_sustainability_deomographic.csv"
df <- readr::read_csv(data_path, show_col_types = FALSE) %>% clean_names()

# -----------------------------
# Optional: rename very long column names if present
# Keep original names otherwise
# -----------------------------
# If your dataset contains a very long label like "4. In your opinion, was it worth the investment to build:"
# replace with a short safe name. Adjust mapping if actual column names differ.
rename_map <- list(
  # example mapping - change left side if your raw csv had that exact name (pre-clean_names)
  # after clean_names names usually are snake_case; nevertheless check presence and rename if needed
  "4_in_your_opinion_was_it_worth_the_investment_to_build" = "worth_invest_building",
  "what_delayed__took_longer" = "what_delayed_took_longer"
)

existing_renames <- intersect(names(df), names(rename_map))
if (length(existing_renames) > 0) {
  for (n in existing_renames) {
    df <- df %>% rename(!!rename_map[[n]] := !!sym(n))
  }
}

# -----------------------------
# Variables of interest
# -----------------------------
demographic_vars <- c("enumerator", "village", "gender_hh", "widow_single_mother",
                      "pwd", "pwd_involved_planning", "age_group", "livelihood",
                      "income_band", "aware_hfhk")

# Impact & sustainability variables (as provided earlier)
impact_vars <- c("improvements_will_last", "maintenance_systems_in_place",
                 "willing_pay_more_for_water", "project_strengthened_cohesion",
                 "marginalized_involved", "community_afford_maintain")

# Open-text fields to analyze (automatically include those present)
open_text_candidates <- c("improvements_last_reasons_no",
                          "community_not_afford_needs_whats_needed",
                          "activities_unnecessary_explain",
                          "recommend_why",
                          "activities_unnecessary_explain") # add more candidates if needed

open_text_vars <- intersect(open_text_candidates, names(df))

# Distance & GPS fields
distance_field <- "distance_to_kiosk_m"
gps_fields <- c("gps_lat", "gps_lon", "gps_alt", "gps_accuracy")

# -----------------------------
# Prepare workbook for combined summary
# -----------------------------
wb <- createWorkbook()
addWorksheet(wb, "Overview")

# We'll store objects to write later
freq_tables <- list()
impact_freq_tables <- list()
demog_freq_tables <- list()
chi_summary <- data.frame(Var = character(), Demo = character(), P_value = numeric(), stringsAsFactors = FALSE)
chi_details <- list()
open_text_wordfreqs <- list()

# -----------------------------
# 1) Demographics: frequencies, plots, write to workbook + console
# -----------------------------
cat("=== DEMOGRAPHICS ===\n")
for (var in demographic_vars) {
  if (!var %in% names(df)) {
    message("Skipping demographic var (not found): ", var)
    next
  }
  tbl <- df %>%
    count(.data[[var]]) %>%
    arrange(desc(n)) %>%
    mutate(percent = round(100 * n / sum(n), 1))
  print(paste("Distribution of", var))
  print(tbl)
  
  demog_freq_tables[[var]] <- tbl
  
  # write CSV/Excel per table
  write.xlsx(tbl, file = file.path(out_dir, paste0("freq_demog_", var, ".xlsx")), overwrite = TRUE)
  
  # plot
  p <- tbl %>%
    ggplot(aes(x = reorder(!!sym(var), -n), y = n, fill = !!sym(var))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(n, " (", percent, "%)")), vjust = -0.4, size = 3) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(filename = file.path(out_dir, paste0("demog_", var, ".png")), plot = p, width = 8, height = 5)
  
  # add worksheet
  addWorksheet(wb, paste0("demog_", var))
  writeData(wb, sheet = paste0("demog_", var), x = tbl)
}

# -----------------------------
# 2) Impact & sustainability: frequencies, plots, write to workbook + console
# -----------------------------
cat("\n=== IMPACT & SUSTAINABILITY ===\n")
for (var in impact_vars) {
  if (!var %in% names(df)) {
    message("Skipping impact var (not found): ", var)
    next
  }
  tbl <- df %>%
    count(.data[[var]]) %>%
    arrange(desc(n)) %>%
    mutate(percent = round(100 * n / sum(n), 1))
  print(paste("Responses for", var))
  print(tbl)
  
  impact_freq_tables[[var]] <- tbl
  write.xlsx(tbl, file = file.path(out_dir, paste0("freq_impact_", var, ".xlsx")), overwrite = TRUE)
  
  # plot
  p <- tbl %>%
    ggplot(aes(x = reorder(!!sym(var), -n), y = n, fill = !!sym(var))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(n, " (", percent, "%)")), vjust = -0.4, size = 3) +
    labs(title = paste("Responses for", var), x = var, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(filename = file.path(out_dir, paste0("impact_", var, ".png")), plot = p, width = 8, height = 5)
  
  # add worksheet
  addWorksheet(wb, paste0("impact_", var))
  writeData(wb, sheet = paste0("impact_", var), x = tbl)
}

# -----------------------------
# 3) Cross-tabs & Chi-square tests vs key demographics (village, gender_hh, income_band)
#    Save p-values and push full cross-tab into workbook
# -----------------------------
cat("\n\n=== CROSS-TABS & CHI-SQUARE ===\n")
demo_tests <- c("village", "gender_hh", "income_band")
for (var in impact_vars) {
  if (!var %in% names(df)) next
  for (demo in demo_tests) {
    if (!demo %in% names(df)) next
    # build contingency table: drop NA rows in both
    sub <- df %>% select(all_of(c(var, demo))) %>% filter(!is.na(.data[[var]]) & !is.na(.data[[demo]]))
    if (nrow(sub) == 0) next
    tab <- table(sub[[var]], sub[[demo]])
    # safe chi-square
    chi <- tryCatch(chisq.test(tab), error = function(e) e)
    p_val <- NA
    chi_text <- NULL
    if (inherits(chi, "error")) {
      chi_text <- paste("Chi-square error:", chi$message)
      message(chi_text)
    } else {
      p_val <- chi$p.value
      chi_text <- paste0("X-squared=", round(chi$statistic,3), ", df=", chi$parameter, ", p=", signif(chi$p.value,4))
    }
    cat(sprintf("Cross-tab: %s vs %s  -> %s\n", var, demo, chi_text))
    
    # store summary
    chi_summary <- rbind(chi_summary,
                         data.frame(Var = var, Demo = demo, P_value = p_val, stringsAsFactors = FALSE))
    chi_details[[paste(var, "_vs_", demo, sep="")]] <- list(crosstab = tab, chi_result = if(!inherits(chi,"error")) chi else NULL, chi_note = if(inherits(chi,"error")) chi$message else NA)
    
    # write cross-tab to workbook
    sheet_name <- paste0(substr(var,1,12),"_by_", substr(demo,1,8))
    # ensure unique sheet names
    sheet_name <- make.names(sheet_name)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = as.data.frame.matrix(tab), rowNames = TRUE)
    # write chi text below table
    writeData(wb, sheet = sheet_name, x = data.frame(Note = chi_text), startRow = nrow(tab) + 3, colNames = FALSE)
  }
}

# write chi_summary sheet and csv
if (nrow(chi_summary) > 0) {
  print("Chi-square summary (p-values):")
  print(chi_summary)
  addWorksheet(wb, "Chi_summary")
  writeData(wb, "Chi_summary", chi_summary)
  write.csv(chi_summary, file.path(out_dir, "chi_summary.csv"), row.names = FALSE)
}

# -----------------------------
# 4) Open-text analysis for all present open-text fields automatically
#    Produce word frequency tables, wordclouds, and write to workbook
# -----------------------------
cat("\n\n=== OPEN-TEXT (QUALITATIVE) ANALYSIS ===\n")
for (var in open_text_vars) {
  if (!var %in% names(df)) next
  text_vec <- df[[var]] %>% as.character() %>% na.omit()
  text_vec <- text_vec[trimws(text_vec) != ""]
  if (length(text_vec) == 0) {
    message("No responses for open-text var: ", var)
    next
  }
  
  # build corpus safely
  corpus <- Corpus(VectorSource(text_vec)) %>%
    tm_map(content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte"))) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
  
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  if (nrow(m) == 0) {
    message("TermDocumentMatrix empty for: ", var)
    next
  }
  word_freq <- sort(rowSums(m), decreasing = TRUE)
  word_df <- data.frame(word = names(word_freq), freq = as.integer(word_freq), stringsAsFactors = FALSE)
  print(paste("Top words for", var))
  print(utils::head(word_df, 20))
  
  # save word freq table and full corpus text
  write.xlsx(word_df, file.path(out_dir, paste0(var, "_word_freq.xlsx")), overwrite = TRUE)
  open_text_wordfreqs[[var]] <- word_df
  
  # add worksheet with top words
  addWorksheet(wb, paste0("words_", substr(var,1,12)))
  writeData(wb, sheet = paste0("words_", substr(var,1,12)), x = head(word_df, 200))
  
  # wordcloud (only if there are >=2 distinct words with freq>=1)
  if (nrow(word_df) >= 1 && max(word_df$freq, na.rm = TRUE) > 0) {
    # restrict min.freq to 2 if many words, else 1
    minf <- ifelse(nrow(word_df) > 20, 2, 1)
    png(file.path(out_dir, paste0(var, "_wordcloud.png")), width = 1000, height = 800)
    tryCatch({
      wordcloud(words = word_df$word, freq = word_df$freq, min.freq = minf,
                random.order = FALSE, scale = c(4,0.6), colors = brewer.pal(8, "Dark2"))
    }, error = function(e) {
      message("Wordcloud generation failed for ", var, " : ", e$message)
    })
    dev.off()
  }
}

# -----------------------------
# 5) Distance to kiosk: descriptive + histogram
# -----------------------------
if (distance_field %in% names(df)) {
  d <- df[[distance_field]]
  if (is.numeric(d) || is.double(d)) {
    dist_summary <- tibble(
      mean_distance = mean(d, na.rm = TRUE),
      median_distance = median(d, na.rm = TRUE),
      sd_distance = sd(d, na.rm = TRUE),
      min_distance = min(d, na.rm = TRUE),
      max_distance = max(d, na.rm = TRUE),
      n_non_na = sum(!is.na(d))
    )
    print("Summary statistics for distance to kiosk (m):")
    print(dist_summary)
    write.xlsx(dist_summary, file.path(out_dir, "distance_summary.xlsx"), overwrite = TRUE)
    
    # histogram
    p <- ggplot(df, aes(x = .data[[distance_field]])) +
      geom_histogram(bins = 20, fill = "steelblue", color = "white", na.rm = TRUE) +
      labs(title = "Distribution of Distance to Kiosk (m)", x = "Distance (m)", y = "Frequency") +
      theme_minimal()
    ggsave(file.path(out_dir, "distance_histogram.png"), p, width = 8, height = 5)
    
    # add to workbook
    addWorksheet(wb, "distance_summary")
    writeData(wb, "distance_summary", dist_summary)
  } else {
    message("Distance field exists but is not numeric: ", distance_field)
  }
}

# -----------------------------
# 6) GPS mapping (plotting scatter of points; requires sf)
# -----------------------------
if (all(c("gps_lat","gps_lon") %in% names(df))) {
  gps_df <- df %>% filter(!is.na(gps_lat) & !is.na(gps_lon))
  if (nrow(gps_df) > 0) {
    gps_points <- tryCatch({
      st_as_sf(gps_df, coords = c("gps_lon", "gps_lat"), crs = 4326, remove = FALSE)
    }, error = function(e) {
      message("sf conversion failed: ", e$message); NULL
    })
    if (!is.null(gps_points)) {
      p <- ggplot() +
        geom_sf(data = gps_points, aes(color = village), size = 2, alpha = 0.8, show.legend = TRUE) +
        theme_minimal() +
        labs(title = "Household GPS locations (by village)", color = "village")
      ggsave(file.path(out_dir, "gps_map.png"), p, width = 9, height = 6)
      message("Saved gps_map.png to output folder")
    }
  }
}

# -----------------------------
# 7) Combined summary workbook: categorical frequencies + chi p-values + top words summary
# -----------------------------
# Add categorical frequencies to workbook in aggregated sheets
addWorksheet(wb, "Demographics_Frequencies")
start_row <- 1
for (nm in names(demog_freq_tables)) {
  tbl <- demog_freq_tables[[nm]]
  writeData(wb, "Demographics_Frequencies", x = paste("Variable:", nm), startRow = start_row, startCol = 1, colNames = FALSE)
  writeData(wb, "Demographics_Frequencies", x = tbl, startRow = start_row + 1, startCol = 1)
  start_row <- start_row + nrow(tbl) + 3
}

addWorksheet(wb, "Impact_Frequencies")
start_row <- 1
for (nm in names(impact_freq_tables)) {
  tbl <- impact_freq_tables[[nm]]
  writeData(wb, "Impact_Frequencies", x = paste("Variable:", nm), startRow = start_row, startCol = 1, colNames = FALSE)
  writeData(wb, "Impact_Frequencies", x = tbl, startRow = start_row + 1, startCol = 1)
  start_row <- start_row + nrow(tbl) + 3
}

# Write chi_summary (p-values) already added earlier; ensure exists
if (exists("chi_summary") && nrow(chi_summary) > 0) {
  # already added above as "Chi_summary" sheet, keep duplicate write for clarity in combined workbook
  addWorksheet(wb, "Chi_pvalues")
  writeData(wb, "Chi_pvalues", chi_summary)
}

# Top words summary sheet (first 20 words per open-text var)
if (length(open_text_wordfreqs) > 0) {
  addWorksheet(wb, "Top_Words_Summary")
  cur_row <- 1
  for (nm in names(open_text_wordfreqs)) {
    wf <- open_text_wordfreqs[[nm]]
    writeData(wb, "Top_Words_Summary", x = paste("Field:", nm), startRow = cur_row, startCol = 1, colNames = FALSE)
    writeData(wb, "Top_Words_Summary", x = head(wf, 30), startRow = cur_row + 1, startCol = 1)
    cur_row <- cur_row + min(32, nrow(wf) + 3)
  }
}

# Final message sheet
writeData(wb, "Overview", x = data.frame(Note = c("Combined summary workbook for Impact & Sustainability / Efficiency analysis",
                                                  paste("Created:", Sys.time()),
                                                  paste("Source file:", data_path))))
# Save workbook
wb_file <- file.path(out_dir, "BMZ_Impact_Sustainability_Efficiency_Summary.xlsx")
saveWorkbook(wb, file = wb_file, overwrite = TRUE)
message("Combined Excel summary saved to: ", wb_file)

# Also save some RDS objects for later inspection
saveRDS(list(demog = demog_freq_tables, impact = impact_freq_tables, chi = chi_details, words = open_text_wordfreqs),
        file = file.path(out_dir, "BMZ_analysis_objects.rds"))

cat("\nAll done. Outputs written to:", out_dir, "\n")



