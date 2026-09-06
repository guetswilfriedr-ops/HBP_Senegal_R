# ============================================================
# DHS variable dictionary builder (data-preparation utility)
#
# Not part of the main pipeline (main.R / main_dcea.R never source
# this) - a one-off tool to turn DHS .DO dictionary files (Stata
# variable/value-label definitions, downloaded alongside the
# Senegal Continuous DHS 2023 datasets from dhsprogram.com) into a
# searchable spreadsheet, before any actual survey microdata (.dta)
# has been loaded. Its job is to help identify, for each Table S1
# intervention, which DHS variable (if any) supplies a prevalence or
# utilisation/coverage indicator - the manual curation step that has
# to happen before R/10-17_dcea_*.R can use real Senegal survey data
# instead of the current placeholders in
# data/dcea_prep/DCEA_preparatory_data.xlsx.
#
# Run with: Rscript scripts/build_dhs_variable_dictionary.R
# Reads:  data/dcea_prep/dhs_do_files/*.DO
#         output/tables/league_table_final.xlsx (for the intervention list)
# Writes: data/dcea_prep/dhs_variable_dictionary.xlsx
# ============================================================

library(dplyr)
library(openxlsx)
library(readxl)

source("R/08_export.R")  # write_xlsx_sheet(), save_xlsx() - generic, no pipeline state needed

#' Parse one DHS .DO file into its variable labels and value-label
#' definitions
#'
#' @param path Path to a .DO file
#' @param recode_name Short label for the source recode (e.g. "HR"),
#'   attached to every row so a combined dictionary stays traceable
#' @return A list with two data frames:
#'   variables    - variable, label, recode (one row per "label
#'                  variable" statement)
#'   value_labels - label_name, code, value_label, recode (one row per
#'                  coded category inside a "label define" block)
parse_do_file <- function(path, recode_name) {
  # DHS .DO files are exported in Windows-1252/Latin-1, not UTF-8 -
  # French accented text and the odd corrupted apostrophe (e.g.
  # "body'??s processes") come from that encoding, not from this
  # parser; read as latin1 and re-encode to UTF-8 so labels display
  # correctly downstream instead of introducing a second layer of
  # mojibake.
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8")

  var_lines <- grep('^label variable\\s+\\S+\\s+"', lines, value = TRUE)
  var_match <- regmatches(var_lines, regexec('^label variable\\s+(\\S+)\\s+"(.*)"\\s*$', var_lines))
  variables <- bind_rows(lapply(var_match, function(m) {
    if (length(m) < 3) return(NULL)
    data.frame(variable = tolower(m[2]), label = m[3], stringsAsFactors = FALSE)
  }))
  variables$recode <- recode_name
  variables$is_not_applicable <- grepl("^NA -", variables$label)

  define_start <- grep('^label define\\s+\\S+', lines)
  value_label_blocks <- lapply(define_start, function(start) {
    label_name <- regmatches(lines[start], regexec('^label define\\s+(\\S+)', lines[start]))[[1]][2]
    collected <- character(0)
    end <- start
    repeat {
      end <- end + 1
      if (end > length(lines)) break
      line <- lines[end]
      if (trimws(line) == ";") break
      collected <- c(collected, line)
      if (grepl(";\\s*$", line)) break
    }
    entry_match <- regmatches(collected, regexec('^\\s*(-?[0-9]+)\\s+"([^"]*)"', collected))
    entries <- bind_rows(lapply(entry_match, function(m) {
      if (length(m) < 3) return(NULL)
      data.frame(code = as.integer(m[2]), value_label = m[3], stringsAsFactors = FALSE)
    }))
    if (nrow(entries) == 0) return(NULL)
    entries$label_name <- label_name
    entries
  })
  value_labels <- bind_rows(value_label_blocks)
  if (nrow(value_labels) > 0) value_labels$recode <- recode_name

  list(variables = variables, value_labels = value_labels)
}

#' Build the combined variable dictionary across every recode
#'
#' @param do_files Named character vector: recode label -> .DO file path
#' @return A list with combined `variables` and `value_labels` data frames
build_dhs_variable_dictionary <- function(do_files) {
  parsed <- lapply(names(do_files), function(recode_name) {
    parse_do_file(do_files[[recode_name]], recode_name)
  })
  list(
    variables    = bind_rows(lapply(parsed, `[[`, "variables")),
    value_labels = bind_rows(lapply(parsed, `[[`, "value_labels"))
  )
}

# Words too generic to drive a keyword match (they appear in nearly
# every DHS variable label, or every intervention name, and would
# swamp the real signal)
stopwords <- c(
  "the", "and", "for", "with", "from", "last", "during", "current",
  "number", "type", "other", "any", "have", "has", "had", "was", "were",
  "not", "who", "when", "where", "what", "does", "did", "before", "after",
  "children", "child", "women", "woman", "treatment", "care", "health",
  "years", "adult", "adults", "case", "cases"
)

tokenize <- function(text) {
  tokens <- tolower(unlist(strsplit(text, "[^A-Za-z]+")))
  tokens[nchar(tokens) >= 4 & !(tokens %in% stopwords)]
}

#' Suggest candidate DHS variables for one intervention/category, by
#' counting shared keywords between the intervention's own text and
#' each dictionary variable's label. A heuristic lead for the analyst
#' to check by hand, never a claim that the match is correct - Table
#' S1's "Survey/Source" and imputation columns are for the analyst,
#' not this function, to fill in with confidence.
#'
#' @param query_text Free text to match against (e.g. intervention
#'   name + category)
#' @param variables The `variables` data frame from
#'   build_dhs_variable_dictionary()
#' @param n_max Maximum candidates to return
#' @return A single string: "RECODE:variable (label); ..." for the
#'   top matches, or "" if nothing scored above zero
suggest_dhs_variables <- function(query_text, variables, n_max = 3) {
  query_tokens <- unique(tokenize(query_text))
  if (length(query_tokens) == 0) return("")

  # HR in particular repeats the same semantic variable once per
  # household-member roster slot (hv121_01, hv121_02, ...) - every
  # slot shares the exact same label text, so de-duplicating by label
  # keeps the top matches from being three copies of one indicator
  candidates <- variables %>%
    filter(!is_not_applicable) %>%
    distinct(label, .keep_all = TRUE)
  label_tokens <- lapply(tolower(candidates$label), function(lbl) unique(tokenize(lbl)))
  scores <- vapply(label_tokens, function(toks) length(intersect(toks, query_tokens)), integer(1))

  top <- order(scores, decreasing = TRUE)[seq_len(min(n_max, sum(scores > 0)))]
  if (length(top) == 0) return("")

  paste0(
    candidates$recode[top], ":", candidates$variable[top],
    " (", candidates$label[top], ")",
    collapse = "; "
  )
}

#' Build a Table S1 draft for Senegal, in the format of the Malawi EHP
#' package's own Table S1 (Disease Area | Intervention | Survey/Source
#' | Prevalence | Utilisation | Mean imputation?) - the Prevalence and
#' Utilisation columns are pre-filled with candidate DHS variables
#' where the keyword match found any, left blank otherwise; both are
#' starting points for manual verification, not final answers
#'
#' @param league_table The "League table" sheet of league_table_final.xlsx
#' @param variables The `variables` data frame from
#'   build_dhs_variable_dictionary()
#' @return A data frame with one row per league-table intervention
build_table_s1_draft <- function(league_table, variables) {
  league_table %>%
    transmute(
      `Disease Area`  = main_category,
      `Sub-category`  = sub_category,
      `Intervention`  = intervention,
      `Survey / Source` = "",
      `Prevalence (candidate DHS variable - verify)`  = mapply(
        function(i, c) suggest_dhs_variables(paste(i, c), variables),
        intervention, sub_category
      ),
      `Utilisation (candidate DHS variable - verify)` = mapply(
        function(i, c) suggest_dhs_variables(paste(i, c, "utilisation coverage visited attended"), variables),
        intervention, sub_category
      ),
      `Mean imputation?` = ""
    )
}

# ------------------------------------------------------------
# Run
# ------------------------------------------------------------
do_files <- c(
  HR = "data/dcea_prep/dhs_do_files/SNHR8SFL.DO",
  IR = "data/dcea_prep/dhs_do_files/SNIR8SFL.DO",
  BR = "data/dcea_prep/dhs_do_files/SNBR8SFL.DO",
  KR = "data/dcea_prep/dhs_do_files/SNKR8SFL.DO",
  SR = "data/dcea_prep/dhs_do_files/SNSR8SFL.DO"
)

dictionary <- build_dhs_variable_dictionary(do_files)

cat(
  "Parsed ", nrow(dictionary$variables), " variable labels and ",
  nrow(dictionary$value_labels), " value-label entries across ",
  length(do_files), " recodes.\n", sep = ""
)

league_table <- read_excel("output/tables/league_table_final.xlsx", sheet = "League table") %>%
  rename(
    intervention = Intervention, main_category = Category,
    sub_category = `Sub-category`
  )

table_s1_draft <- build_table_s1_draft(league_table, dictionary$variables)

wb <- createWorkbook()
write_xlsx_sheet(
  wb, "Variable dictionary",
  dictionary$variables %>% select(recode, variable, label, is_not_applicable),
  freeze_col = 2
)
write_xlsx_sheet(
  wb, "Value labels",
  dictionary$value_labels %>% select(recode, label_name, code, value_label),
  freeze_col = 2
)
write_xlsx_sheet(wb, "Table S1 draft", table_s1_draft, freeze_col = 3)
save_xlsx(wb, "dhs_variable_dictionary", "data/dcea_prep")

cat("\nWritten to data/dcea_prep/dhs_variable_dictionary.xlsx\n")
cat(
  "'Table S1 draft' is a STARTING POINT: the Prevalence/Utilisation columns\n",
  "are keyword-matched candidates from the variable LABELS only (no actual\n",
  "survey data has been loaded yet) - each one needs to be checked against\n",
  "the real .dta/.sav file and the DHS final report before being treated as\n",
  "confirmed, exactly like every other tiered value in DCEA_preparatory_data.xlsx.\n",
  sep = ""
)
