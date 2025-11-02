# ============================================================
# BMZ DAC CRITERIA: EFFICIENCY - Comprehensive Analysis Script
# - Reads CSV, renames very long column names only
# - Descriptives, cross-tabs + chi-square/Fisher (vs demographics)
# - Text analysis for open responses (word freq + wordcloud)
# - Saves tables (Excel), plots (PNG) and console outputs
# ============================================================

# --------------------------
# Required libraries
# --------------------------
library(tidyverse)
library(readr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(tidytext)    # for text tokenization
library(stopwords)   # for stopwords lists
library(wordcloud)   # for creating wordcloud png
library(stringi)     # for safe encoding / string ops

# --------------------------
# File paths / folders
# --------------------------
input_file <- "C:/Users/Hp/Downloads/effectiveness_demographics.csv"   # change if needed
output_dir  <- "C:/Users/Hp/Downloads/BMZ_OUTPUTS"
plots_dir   <- file.path(output_dir, "plots_efficiency")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# --------------------------
# Load data with encoding safeguards
# --------------------------
# Try reading with UTF-8, fallback to latin1 then convert to UTF-8
try({
  df <- read_csv(input_file, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
}, silent = TRUE)

if (!exists("df")) {
  df <- read_csv(input_file, locale = locale(encoding = "latin1"), show_col_types = FALSE)
  # convert char columns to UTF-8
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      # try convert safely
      enc2utf8(iconv(col, from = "latin1", to = "UTF-8", sub = ""))
    } else col
  })
}

# Clean names (snake_case, unique). We'll rename only very long names later.
df <- df %>% clean_names()

# Print first few column names (console)
cat("\nLoaded data with", nrow(df), "rows and", ncol(df), "columns.\n")
cat("Columns (first 40):\n")
print(head(names(df), 40))

# --------------------------
# Rename only very long / problematic column names
# Approach:
# - Find column names with length > 30 or that start with a digit or contain spaces
# - Create a safe short name: replace non-alnum with underscore, truncate to 28 chars, ensure uniqueness
# - Keep a mapping log (old -> new)
# --------------------------
orig_names <- names(df)
bad_idx <- which(nchar(orig_names) > 30 | grepl("^\\d", orig_names) | grepl("\\s", orig_names) |
                   grepl("[^A-Za-z0-9_]", orig_names))

rename_map <- list()
if (length(bad_idx) > 0) {
  new_names <- orig_names
  for (i in bad_idx) {
    old <- orig_names[i]
    # create sanitized short name
    tmp <- old %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%  # non-alnum -> underscore
      str_replace_all("^_+|_+$", "")             # trim leading/trailing underscores
    # truncate to 28 chars then ensure unique
    base <- substr(tmp, 1, 28)
    candidate <- base
    k <- 1
    while (candidate %in% new_names) {
      candidate <- paste0(base, "_", k)
      k <- k + 1
    }
    new_names[i] <- candidate
    rename_map[[old]] <- candidate
  }
  # apply rename
  names(df) <- new_names
  cat("\nRenamed", length(rename_map), "very long/problematic columns. Mapping:\n")
  print(rename_map)
} else {
  cat("\nNo very long/problematic column names detected; no renaming performed.\n")
}

# Save the renaming map to a CSV for record
if (length(rename_map) > 0) {
  map_df <- tibble(old_name = names(rename_map), new_name = unlist(rename_map))
  write_csv(map_df, file.path(output_dir, "column_rename_map_efficiency.csv"))
}

# --------------------------
# Define demographics and efficiency variables (as provided)
# If some names don't exist in df we will skip them but report it.
# --------------------------
demographics <- c("village", "gender_hh", "income_band", "pwd")
efficiency_vars_proposed <- c(
  "project_timely_completed", "what_delayed_took_longer", "project_used_resources_wisely",
  "community_unpaid_work", "unpaid_hours", "contribution_recognized",
  # the long "worth investment" variable from your spec likely got cleaned/renamed earlier;
  # look for sensible column names (worth_invest_*)
  "worth_invest_water", "worth_invest_latrines", "worth_invest_canals",
  "activities_unnecessary", "activities_unnecessary_explain",
  "community_contributed", "what_contributed", "contrib_labour", "contrib_materials",
  "contrib_land", "contrib_cash", "contrib_other", "contrib_other_specify",
  "contrib_voluntary", "value_for_money", "compare_other_projects",
  "recommend_similar_project", "recommend_why", "project_used_resources_wisely"
)

# Try to detect columns that match (including variations). Build final efficiency var list based on df names
available_names <- names(df)

# helper to fuzzy find a target among available names (exact or partial)
find_name <- function(target_patterns, avail) {
  # target_patterns: vector of possible substrings to match
  for (pat in target_patterns) {
    # exact match
    if (pat %in% avail) return(pat)
  }
  # substring match (case-insensitive)
  for (pat in target_patterns) {
    m <- avail[str_detect(avail, regex(pat, ignore_case = TRUE))]
    if (length(m) >= 1) return(m[1])
  }
  return(NA_character_)
}

# build a list of canonical patterns to locate actual columns in this dataset
patterns_list <- list(
  project_timely_completed = c("project_timely_completed", "project_completed", "timely"),
  what_delayed_took_longer = c("what_delayed", "what_delayed_took_longer", "delayed"),
  project_used_resources_wisely = c("project_used_resources_wisely", "used_its_resources", "efficient"),
  community_unpaid_work = c("community_unpaid_work", "unpaid_community", "unpaid_work"),
  unpaid_hours = c("unpaid_hours", "how_many_hours", "unpaid_hours_total"),
  contribution_recognized = c("contribution_recognized", "recognised", "contribution_recognized"),
  worth_invest_water = c("worth_invest_water", "worth_invest_water_kiosks", "worth_invest_water"),
  worth_invest_latrines = c("worth_invest_latrines", "worth_invest_latrines"),
  worth_invest_canals = c("worth_invest_canals", "worth_invest_canals"),
  activities_unnecessary = c("activities_unnecessary", "activities_unnecessary"),
  activities_unnecessary_explain = c("activities_unnecessary_explain", "activities_unnecessary_explain"),
  community_contributed = c("community_contributed", "community_contributed"),
  what_contributed = c("what_contributed", "what_contributed"),
  contrib_labour = c("contrib_labour", "labour", "contrib_labour"),
  contrib_materials = c("contrib_materials", "materials", "contrib_materials"),
  contrib_land = c("contrib_land", "land", "contrib_land"),
  contrib_cash = c("contrib_cash", "cash", "contrib_cash"),
  contrib_other = c("contrib_other", "contrib_other"),
  contrib_other_specify = c("contrib_other_specify", "contrib_other_specify"),
  contrib_voluntary = c("contrib_voluntary", "contrib_voluntary"),
  value_for_money = c("value_for_money", "value_for_money"),
  compare_other_projects = c("compare_other_projects", "compare_other_projects"),
  recommend_similar_project = c("recommend_similar_project", "recommend_similar_project"),
  recommend_why = c("recommend_why", "recommend_why"),
  # text fields commonly present
  activities_unnecessary_explain_alt = c("activities_unnecessary_explain", "activities_unnecessary_explain"),
  what_delayed_text = c("what_delayed_took_longer", "what_delayed", "delayed")
)

# Build final list of found variables
eff_vars_found <- c()
for (name_key in names(patterns_list)) {
  found <- find_name(patterns_list[[name_key]], available_names)
  if (!is.na(found)) eff_vars_found <- unique(c(eff_vars_found, found))
}

# also ensure some obvious columns from your posted dataset are included
explicit_adds <- c("project_timely_completed", "project_used_resources_wisely", "what_delayed_took_longer",
                   "activities_unnecessary_explain", "contrib_other_specify", "unpaid_hours",
                   "worth_invest_water", "worth_invest_latrines", "worth_invest_canals")
for (x in explicit_adds) if (x %in% available_names && !(x %in% eff_vars_found)) eff_vars_found <- c(eff_vars_found, x)

cat("\nIdentified", length(eff_vars_found), "efficiency-related variables found in dataset. They are:\n")
print(eff_vars_found)

# --------------------------
# Convert selected categorical vars + demographics to factors (if present)
# --------------------------
to_factor <- unique(c(demographics, eff_vars_found))
for (v in to_factor) {
  if (v %in% names(df)) {
    # only convert non-numeric or small numeric categories to factor
    if (!is.factor(df[[v]])) {
      # If character or few unique values -> factor
      if (is.character(df[[v]]) || (is.numeric(df[[v]]) && length(unique(df[[v]])) < 10)) {
        df[[v]] <- as.factor(df[[v]])
      }
    }
  }
}

# --------------------------
# Prepare Excel workbook to capture summary tables and chi results
# --------------------------
wb <- createWorkbook()
addWorksheet(wb, "Overview")

writeData(wb, "Overview", tibble(
  generated_on = Sys.time(),
  rows = nrow(df),
  columns = ncol(df),
  efficiency_vars_detected = paste(eff_vars_found, collapse = ", "),
  demographics_used = paste(intersect(demographics, names(df)), collapse = ", ")
), startRow = 1)

# --------------------------
# Helper functions
# --------------------------
safe_sheet_name <- function(name, maxlen = 28) {
  # remove problematic characters and truncate
  n <- gsub("[^A-Za-z0-9_]", "_", name)
  if (nchar(n) > maxlen) n <- substr(n, 1, maxlen)
  # ensure uniqueness by appending short hash if needed
  return(n)
}

save_freq_plot <- function(tab_df, varname) {
  # tab_df: data.frame with columns 'category' and 'n' or 'percent'
  p <- ggplot(tab_df, aes(x = reorder(category, -n), y = n)) +
    geom_col(fill = "#2b8cbe") +
    geom_text(aes(label = n), vjust = -0.3, size = 3) +
    labs(title = paste("Frequency:", varname), x = varname, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  fname <- file.path(plots_dir, paste0("freq_", varname, ".png"))
  ggsave(fname, p, width = 7, height = 4)
  return(fname)
}

# --------------------------
# 1) Descriptive frequencies & plots for each efficiency categorical variable
# --------------------------
cat("\n*** DESCRIPTIVE FREQUENCIES FOR EFFICIENCY VARIABLES ***\n")
for (v in eff_vars_found) {
  if (!(v %in% names(df))) next
  # For text fields (long open text) we treat later; skip here if clearly long text
  # Heuristic: skip if > 100 unique values and is character -> treat as text later
  unique_vals <- length(unique(na.omit(df[[v]])))
  if (is.character(df[[v]]) && unique_vals > 100) next
  
  tab <- df %>%
    filter(!is.na(.data[[v]])) %>%
    count(category = as.character(.data[[v]])) %>%
    arrange(desc(n)) %>%
    mutate(percent = round(100 * n / sum(n), 1))
  
  cat("\nVariable:", v, "\n")
  print(tab)
  
  # write to workbook
  sheet <- safe_sheet_name(paste0("freq_", v))
  addWorksheet(wb, sheet)
  writeData(wb, sheet, tab, startRow = 1, startCol = 1)
  
  # plot and save PNG
  if (nrow(tab) > 0) {
    pngfile <- save_freq_plot(tab, v)
    # place reference in sheet (path)
    writeData(wb, sheet, tibble(plot_png = pngfile), startRow = nrow(tab) + 4, startCol = 1)
  }
}

# --------------------------
# 2) Cross-tabs & Chi-square / Fisher tests (each efficiency categorical vs demographics)
#    - For each (eff_var x demo_var) produce cross-tab, print to console, and write to workbook
# --------------------------
cat("\n*** CROSS-TABS & STATISTICAL TESTS (Chi-square/Fisher) ***\n")
demo_in_df <- intersect(demographics, names(df))
if (length(demo_in_df) == 0) {
  cat("No demographic variables (village/gender_hh/income_band/pwd) found in dataset. Skipping cross-tabs.\n")
} else {
  for (v in eff_vars_found) {
    if (!(v %in% names(df))) next
    # skip long text fields (>100 unique chars) — treat in text section
    if (is.character(df[[v]]) && length(unique(na.omit(df[[v]]))) > 100) next
    
    for (d in demo_in_df) {
      # both must be non-NA and not continuous numeric
      if (all(is.na(df[[v]])) || all(is.na(df[[d]]))) next
      # create contingency table
      tbl <- table(df[[v]], df[[d]], useNA = "no")
      if (any(dim(tbl) <= 1)) {
        # nothing to test
        next
      }
      cat("\nCross-tab:", v, "BY", d, "\n")
      print(tbl)
      
      # Decide on test: Chi-square if expected freq >= 5 mostly, else Fisher
      chi_try <- tryCatch(chisq.test(tbl), error = function(e) e)
      if (inherits(chi_try, "error")) {
        test_name <- "fisher"
        test_res <- tryCatch(fisher.test(tbl), error = function(e) e)
      } else {
        # check expected counts
        exp_counts <- suppressWarnings(chi_try$expected)
        if (any(exp_counts < 5)) {
          test_name <- "fisher"
          test_res <- tryCatch(fisher.test(tbl), error = function(e) e)
        } else {
          test_name <- "chisq"
          test_res <- chi_try
        }
      }
      
      # print test results to console
      if (inherits(test_res, "error")) {
        cat("Statistical test error:", test_res$message, "\n")
      } else {
        if (test_name == "chisq") {
          cat("Chi-square p-value:", test_res$p.value, "statistic:", test_res$statistic, "\n")
        } else {
          cat("Fisher p-value:", test_res$p.value, "\n")
        }
      }
      
      # write cross-tab and result to workbook sheet (safe sheet name)
      sheet_base <- safe_sheet_name(paste0(v, "_by_", d))
      # ensure unique sheet name
      if (sheet_base %in% names(wb)) sheet_base <- paste0(sheet_base, "_2")
      addWorksheet(wb, sheet_base)
      writeData(wb, sheet_base, as.data.frame.matrix(tbl), startRow = 1, startCol = 1)
      # write test summary
      if (!inherits(test_res, "error")) {
        test_summary <- tibble(test = test_name, p_value = test_res$p.value)
      } else {
        test_summary <- tibble(test = "error", message = test_res$message)
      }
      writeData(wb, sheet_base, test_summary, startRow = nrow(tbl) + 3, startCol = 1)
      
      # also save a simple percent table
      prop_df <- as.data.frame(prop.table(tbl, 2)) %>%
        set_names(c("eff_cat", "demo_cat", "proportion"))
      writeData(wb, sheet_base, prop_df, startRow = nrow(tbl) + 7, startCol = 1)
      
      # save a stacked percent bar plot png
      plot_df <- prop_df %>% mutate(percent = round(proportion * 100, 1))
      if (nrow(plot_df) > 0) {
        p <- ggplot(plot_df, aes(x = demo_cat, y = percent, fill = eff_cat)) +
          geom_col(position = "fill") +
          scale_y_continuous(labels = scales::percent_format(scale = 1)) +
          labs(title = paste(v, "by", d), y = "Percent", x = d) +
          theme_minimal()
        fname <- file.path(plots_dir, paste0("stacked_", v, "_by_", d, ".png"))
        ggsave(fname, p, width = 7, height = 4)
        writeData(wb, sheet_base, tibble(plot_png = fname), startRow = nrow(tbl) + 15, startCol = 1)
      }
    }
  }
}

# --------------------------
# 3) Open-text variables: detect likely text columns and run word frequency + wordcloud
# We'll look for columns that contain 'explain', 'why', 'reason', 'delayed' etc OR those in your spec
# --------------------------
text_patterns <- c("explain", "why", "recommend_why", "activities_unnecessary_explain",
                   "what_delayed", "delayed", "recommend_why", "project_delay", "what_delayed_took_longer")
text_cols <- names(df)[sapply(names(df), function(x) any(str_detect(x, regex(paste(text_patterns, collapse="|"), ignore_case = TRUE))))]

# Also include explicit names if present
explicit_texts <- c("activities_unnecessary_explain", "recommend_why", "what_delayed_took_longer", "what_delayed")
text_cols <- unique(c(text_cols, intersect(explicit_texts, names(df))))

cat("\nText columns identified for qualitative analysis:\n")
print(text_cols)

# Prepare stopwords
sw <- c(stopwords::stopwords("en"), stopwords::stopwords("en", source = "snowball"))
sw <- unique(tolower(sw))

# helper: clean a single text column and produce top words
analyze_text_col <- function(colname, df, top_n = 50) {
  if (!(colname %in% names(df))) return(NULL)
  colvec <- df[[colname]]
  if (!is.character(colvec)) colvec <- as.character(colvec)
  tokens <- tibble(text = colvec) %>%
    filter(!is.na(text) & str_trim(text) != "") %>%
    mutate(text = str_to_lower(text),
           text = stri_enc_toutf8(text, is_unknown_8bit = TRUE)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw, str_detect(word, "[a-z]")) %>%
    count(word, sort = TRUE)
  
  # Save frequency table to workbook and CSV
  sheet <- safe_sheet_name(paste0("words_", colname))
  if (sheet %in% names(wb)) sheet <- paste0(sheet, "_2")
  addWorksheet(wb, sheet)
  writeData(wb, sheet, tokens, startRow = 1, startCol = 1)
  write_csv(tokens, file.path(output_dir, paste0("wordfreq_", colname, ".csv")))
  
  # create a wordcloud PNG (if there are enough words)
  if (nrow(tokens) >= 3) {
    wc_file <- file.path(plots_dir, paste0("wordcloud_", colname, ".png"))
    png(wc_file, width = 800, height = 600)
    # wordcloud expects vectors
    wordcloud(words = tokens$word, freq = tokens$n, max.words = top_n, random.order = FALSE)
    dev.off()
    writeData(wb, sheet, tibble(wordcloud_png = wc_file), startRow = nrow(tokens) + 4, startCol = 1)
  } else {
    wc_file <- NA_character_
  }
  
  return(list(freq = tokens, wordcloud = wc_file))
}

text_analysis_results <- list()
for (tc in text_cols) {
  res <- tryCatch(analyze_text_col(tc, df), error = function(e) {
    message("Error while analyzing text column ", tc, ": ", e$message)
    NULL
  })
  text_analysis_results[[tc]] <- res
  if (!is.null(res)) {
    cat("\nTop words for", tc, ":\n")
    print(head(res$freq, 15))
  }
}

# --------------------------
# 4) Save workbook with tables and test results
# --------------------------
wb_file <- file.path(output_dir, paste0("Efficiency_Analysis_Results_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
saveWorkbook(wb, wb_file, overwrite = TRUE)
cat("\nExcel workbook saved to:", wb_file, "\n")

# --------------------------
# 5) Save some quick summary CSVs for easy viewing
# --------------------------
# Save the descriptive frequency tables (built earlier) as separate CSVs if present in workbook
# (We already saved word freq CSVs)
cat("\nSaving quick CSVs for each categorical efficiency var...\n")
for (v in eff_vars_found) {
  if (!(v %in% names(df))) next
  # skip long text fields (handled separately)
  if (is.character(df[[v]]) && length(unique(na.omit(df[[v]]))) > 100) next
  tab <- df %>% count(category = as.character(.data[[v]])) %>% arrange(desc(n)) %>% mutate(percent = round(100*n/sum(n),1))
  write_csv(tab, file.path(output_dir, paste0("freq_", v, ".csv")))
}

cat("\nAll PNG plots saved to:", plots_dir, "\n")
cat("\nAnalysis complete. Key outputs:\n")
cat("- Excel workbook with tables and test results:", wb_file, "\n")
cat("- PNG plots directory:", plots_dir, "\n")
cat("- Individual CSVs for frequency tables and word frequencies in:", output_dir, "\n")

# End of script
