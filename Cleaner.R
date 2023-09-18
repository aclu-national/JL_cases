# --------------------------------- Loading Libraries and data -------------------------------

# Importing Libraries
library(gsheet)
library(janitor)
library(lubridate)
library(tidyverse)
library(googlesheets4)

# Linking to the spreadsheet
spreadsheet_link <- "https://docs.google.com/spreadsheets/d/1xfCP7Dg-txaAwgIQs2sjjaQZ0Ur37-2ZCi6TzFj2GJk/edit#gid=585513752"


# -------------------------------- Data Cleaning ----------------------------------------------- 

# Importing data from a Google sheet
df <- gsheet2tbl(spreadsheet_link) %>%
  clean_names()

# Defining a function to split by ", " and unest
split_and_unnest <- function(column_name) {
  result <- df %>%
    mutate(new_column = strsplit(as.character(str_replace_all({{ column_name }}, ", ", ",")), ",")) %>%
    unnest(new_column)
  
  return(result)
}

# Splitting data by department
unested_department <- split_and_unnest(police_departments)

# Splitting data by entity
unested_entity <- split_and_unnest(other_entities)

# Splitting data by officer
unested_officers <- split_and_unnest(police_officerss_names)

# Splitting data by plaintiff
unested_plaintiffs <- split_and_unnest(plaintiffs)

# Splitting data by count
unested_counts <- df %>%
  distinct(lawsuit_number, .keep_all = TRUE) %>%
  mutate(counts = gsub("\\([^\\)]+\\)", "", counts)) %>% 
  mutate(counts = strsplit(counts, ";")) %>%
  unnest(counts) %>%
  mutate(
    counts_classification = tolower(counts),
    counts_classification = case_when(
    str_detect(counts_classification, "force|excessive|battery|assault") ~ "Excessive Force",
    str_detect(counts_classification, "discrimin|civil right|disabilit|accommod|title iv|title 5") ~ "Discrimination", 
    str_detect(counts_classification, "wrongful arrest|false arrest|unlawful arrest|false imprison|unlawful imprison|detention") ~ "Unlawful Arrest and Imprisonment",
    str_detect(counts_classification, "Article 2315|Art. 2315|negligence|indifference|negligent|supervision") ~ "Negligence",
    str_detect(counts_classification, "intervene|bystander") ~ "Failure to Intervene",
    str_detect(counts_classification, "vicarious liability") ~ "Vicarious Liability",
    str_detect(counts_classification, "emotion|distress") ~ "Infliction of Emotional Distress",
    str_detect(counts_classification, "stop|search|seizure|privacy|taking") ~ "Unlawful Stop, Search, or Seizure",
    str_detect(counts_classification, "first amendment|1 amendment|speech|first|article 1") ~ "First Amendment Violation",
    str_detect(counts_classification, "malicious") ~ "Malicious Prosecution",
    str_detect(counts_classification, "conspiracy") ~ "Conspiracy",
    str_detect(counts_classification, "death") ~ "Wrongful Death",
    str_detect(counts_classification, "due process") ~ "Violation of Due Process",
    str_detect(counts_classification, "train|supervise|investigate") ~ "Failure to Train, Supervise, or Investigate",
    str_detect(counts_classification, "public records") ~ "Violation of Public Records law",
    TRUE ~ "Other"
  ))


# --------------------------------------- Counts ----------------------------------------------------

# Number of police departments involved in JL lawsuits
n_involved <- length(unique(unested_department$new_column %>% 
                              tolower()))

# Number of police departments sued
n_sued <- length(unique(filter(unested_department, pd_is_a_named_defendant == "Y")$new_column %>% 
                          tolower()))

# Number of govenmental entities or other agencies sued
n_entities <- length(unique(unested_entity$new_column %>% tolower()))-1

# Number of parishes where we have filed suit
n_parishes <- length(unique(df$parishes %>% tolower()))

# Number of police officers sued
n_officers <- length(unique(unested_officers$new_column %>% tolower()))

# Number of plaintiffs represented
n_plaintiffs <- length(unique(unested_plaintiffs$new_column %>% tolower())) 

# Total number of lawsuits
n_lawsuits <- length(unique(df$lawsuit_number))

# Total number of cases
n_cases <- nrow(df)

# Number of cases we took to appeal stage
n_appeal <- sum(df$appeal_stage == "Y", na.rm = TRUE)

# Defining a function to calculate the distinct sum across lawsuits
calculate_sum <- function(column_name) {
  result <- df %>%
    pull({{ column_name }}) %>%
    sum(na.rm = TRUE)
  
  return(result)
}

# Number of Qualified Immunity trial wins
n_qi_trial_wins <- calculate_sum("qi_trial_win_number")

# Number of Qualified Immunity appellate wins
n_qi_appellate_wins <- calculate_sum("qi_appellate_win_number")

# Number of absolute immunity trial wins
n_absolute_trial_wins <- calculate_sum("absolute_immunity_trial_win_number")

# Number of absolute immunity appellate wins
n_absolute_appellate_wins <- calculate_sum("absolute_immunity_appelate_win_number")

# Number Heck trial wins
n_heck_trial_wins <- calculate_sum("heck_trial_win_number")

# Number Heck appellate wins
n_heck_appellate_wins <-calculate_sum("heck_appellate_win_number")

# Number Lyons trial wins 
n_lyons_trial_wins <- calculate_sum("lyons_trial_win_number")

# Number Lyons appellate wins 
n_lyons_appellate_wins <- calculate_sum("lyons_appellate_win_number")

# Number SOL trial wins 
n_sol_trial_wins <- calculate_sum("sol_trial_win_number")

# Number SOL appellate wins 
n_sol_appellate_wins <- calculate_sum("sol_appellate_win_number")

# Number of Monell trial wins
n_monell_trial_wins <- calculate_sum("monell_trial_win_number")

# Number of Monell appellate wins
n_monell_appellate_wins <- calculate_sum("monell_appellate_win_number")

# Number of Settlements
n_settlements <- sum(distinct(df, lawsuit_number, .keep_all = TRUE)$settlement == "Y", na.rm = TRUE)

# Number of Individual Defendants Named in Settlements
n_defendants_in_settlements <- length(unique(filter(unested_officers, settlement == "Y")$new_column))

# Number of offers of judgment
n_judgement <- calculate_sum("offer_of_judgement")

# Number of cases won or pending
n_won_or_pending <- df %>%
  distinct(lawsuit_number, .keep_all = TRUE) %>%
  filter(case_status %in% c("Won", "Pending")) %>%
  summarize(won_or_pending = nrow(.)) %>%
  pull(won_or_pending)

# Number of legal wins
n_legal_wins <- sum(n_qi_appellate_wins,
                    n_qi_trial_wins,
                    n_defendants_in_settlements,
                    n_heck_appellate_wins,
                    n_heck_trial_wins,
                    n_absolute_appellate_wins, 
                    n_absolute_trial_wins,
                    n_monell_appellate_wins,
                    n_monell_trial_wins,
                    n_lyons_appellate_wins, 
                    n_lyons_trial_wins,
                    n_judgement,
                    n_sol_appellate_wins,
                    n_sol_trial_wins)

# --------------------------------------- Tables ----------------------------------------------------


# Defining all categories
item <- c("Number of police departments involved in Justice Lab lawsuits",
          "Number of police departments sued",
          "Number of govenmental entities or other agencies sued",
          "Number of parishes where we have filed suit",
          "Number of police officers sued",
          "Number of plaintiffs represented",
          "Total number of lawsuits",
          "Total number of cases",
          "Number of cases we took to appeal stage",
          "Number of Qualified Immunity trial wins",
          "Number of Qualified Immunity appellate wins",
          "Number of absolute immunity trial wins",
          "Number of absolute immunity appellate wins",
          "Number Heck trial wins",
          "Number Heck appellate wins",
          "Number Lyons trial wins",
          "Number Lyons appellate wins",
          "Number SOL trial wins",
          "Number SOL appellate wins",
          "Number of Monell trial wins",
          "Number of Monell appellate wins",
          "Number of Settlements",
          "Number of Individual Defendants Named in Settlements",
          "Number of offers of judgment",
          "Number won or pending",
          "Number of legal wins"
          )

# Defining all values for the categories
value <- c(n_involved,
          n_sued,
          n_entities,
          n_parishes,
          n_officers,
          n_plaintiffs,
          n_lawsuits,
          n_cases,
          n_appeal,
          n_qi_trial_wins,
          n_qi_appellate_wins,
          n_absolute_trial_wins,
          n_absolute_appellate_wins,
          n_heck_trial_wins,
          n_heck_appellate_wins,
          n_lyons_trial_wins,
          n_lyons_appellate_wins,
          n_sol_trial_wins,
          n_sol_appellate_wins,
          n_monell_trial_wins,
          n_monell_appellate_wins,
          n_settlements,
          n_defendants_in_settlements,
          n_judgement,
          n_won_or_pending,
          n_legal_wins)

# Turning the two vectors above into a dataframe
values_df <- data.frame(item,value)

# Police departments involved table
pd_table <- unested_department %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Police departments sued table
pd_sued_table <- unested_department %>% 
  filter(pd_is_a_named_defendant == "Y") %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Entities sued table
entity_sued_table <- unested_entity %>%
  tabyl(new_column, show_na = FALSE) %>%
  arrange(desc(n))

# Parish filed suit table
parish_sued_table <- df %>%
  tabyl(parishes) %>%
  arrange(desc(n))

# Officers sued table
officer_sued_table <- unested_officers %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Plaintiffs represented table
plaintiff_represented_table <- unested_plaintiffs %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Lawsuits table
lawsuits_table <- df %>%
  distinct(lawsuit_number, .keep_all = TRUE) %>%
  select(c("case_number", "case_name"))

# Lawsuits outcomes
lawsuit_outcome_table <- df %>%
  tabyl(case_status)

# Cases taken to appeal table
appeal_table <- df %>%
  filter(appeal_stage == "Y") %>%
  select(c("case_number", "case_name"))

# Location of lawsuits 
location_table <- df %>%
  distinct(lawsuit_number, .keep_all = TRUE) %>%
  tabyl(case_location) %>%
  arrange(desc(n))

# Initial lawsuit counts 
counts_table <- unested_counts %>%
  tabyl(counts_classification)

# Defining a function to count wins
win_table <- function(variable) {
  result <- df %>%
    filter(!is.na({{variable}})) %>%
    select(c("case_number", "case_name", {{ variable }}))
  
  print(result)
}

# QI trial wins table
qi_trial_win_table <- win_table(qi_trial_win_number)

# QI appellate win table
qi_appellate_win_table <- win_table(qi_appellate_win_number)

# Absolute immunity trial win table
absolute_trial_win_table <- win_table(absolute_immunity_trial_win_number)

# Absolute immunity appellate win table
absolute_appellate_win_table <- win_table(absolute_immunity_appelate_win_number)

# Heck trial win table 
heck_trial_win_table <- win_table(heck_trial_win_number)

# Heck appellate win table
heck_appellate_win_table <- win_table(heck_appellate_win_number)

# Lyons trial win table 
lyons_trial_win_table <- win_table(lyons_trial_win_number)

# Lyons appellate win table
lyons_appellate_win_table <- win_table(lyons_appellate_win_number)

# SOL trial win table 
sol_trial_win_table <- win_table(sol_trial_win_number)

# SOL appellate win table
sol_appellate_win_table <- win_table(sol_appellate_win_number)

# Monell trial win table
monell_trial_win_table <- win_table(monell_trial_win_number)

# Monell appellate win table
monell_appellate_win_table <- win_table(monell_appellate_win_number)

# Lawsuits with settlements table
lawsuit_settlements_table <- df %>%
  distinct(lawsuit_number, .keep_all = TRUE) %>%
  filter(settlement == "Y") %>%
  select(c("case_number", "case_name"))

# Defendants named in settlements table
officers_in_settlements_table <- unested_officers %>%
  filter(settlement == "Y") %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Offers of judgement table
judgement_offer_table <- df %>%
  filter(!is.na(offer_of_judgement)) %>%
  select(c("case_number", "case_name"))

# All legal wins
legal_win_table <- data.frame(
  win_type = c("Qualified Immunity",
    "Settlement",
    "Heck",
    "Absolute Immunity",
    "Monell",
    "Lyons",
    "Offer of Judgement",
    "Statute of Limitations"),
  win_count = c(sum(n_qi_appellate_wins,n_qi_trial_wins),
                n_defendants_in_settlements,
                sum(n_heck_appellate_wins,n_heck_trial_wins),
                sum(n_absolute_appellate_wins, n_absolute_trial_wins),
                sum(n_monell_appellate_wins,n_monell_trial_wins),
                sum(n_lyons_appellate_wins, n_lyons_trial_wins),
                n_judgement,
                sum(n_sol_appellate_wins,n_sol_trial_wins)
  )
) %>%
  filter(win_count != "0") %>%
  arrange(win_count)


# -------------------------_ Exporting content into a Google Sheet --------------------------------------

# Defining sheets for the spreadsheet
sheets = c("Values", 
           "Police departments involved", 
           "Police departments sued", 
           "Parishes where we filed suit", 
           "Officers sued",
           "Entities sued",
           "Plaintiffs represented",
           "All Lawsuits",
           "Lawsuit Outcomes",
           "Cases taken to appeal",
           "QI trial wins",
           "QI appellate wins",
           "Absolute immunity trial wins",
           "Absolute immunity appellate wins",
           "Heck trial wins",
           "Heck appellate wins",
           "Lyons trial wins",
           "Lyons appellate wins",
           "SOL trial wins",
           "SOL appellate wins",
           "Monell trial wins",
           "Monell appellate wins",
           "Lawsuits with settlements",
           "Officers named in settlements",
           "Offers of judgement",
           "Legal win table",
           "Location table",
           "Counts table")

# Defining sheet values
data_frames = list(values_df, 
                   pd_table, 
                   pd_sued_table, 
                   parish_sued_table, 
                   officer_sued_table,
                   entity_sued_table,
                   plaintiff_represented_table,
                   lawsuits_table,
                   lawsuit_outcome_table,
                   appeal_table,
                   qi_trial_win_table,
                   qi_appellate_win_table,
                   absolute_trial_win_table,
                   absolute_appellate_win_table,
                   heck_trial_win_table,
                   heck_appellate_win_table,
                   lyons_trial_win_table,
                   lyons_appellate_win_table,
                   sol_trial_win_table,
                   sol_appellate_win_table,
                   monell_trial_win_table,
                   monell_appellate_win_table,
                   lawsuit_settlements_table,
                   officers_in_settlements_table,
                   judgement_offer_table,
                   legal_win_table,
                   location_table,
                   counts_table)

# Adding the new sheets to the spreadsheet
for (i in seq_along(sheets)) {
  sheet_name <- sheets[i]
  data_frame <- data_frames[[i]]
  
  # Append the data frame to the sheet using the provided URL
  write_sheet(data_frame, ss = spreadsheet_link, sheet = sheet_name)
}
