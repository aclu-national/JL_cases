# Importing Libraries
library(gsheet)
library(janitor)
library(lubridate)
library(tidyverse)
library(googlesheets4)

# Linking to the spreadsheet
spreadsheet_link <- "https://docs.google.com/spreadsheets/d/1xfCP7Dg-txaAwgIQs2sjjaQZ0Ur37-2ZCi6TzFj2GJk/edit#gid=585513752"

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

# Number of police departments involved in JL lawsuits
n_involved <- length(unique(unested_department$departments %>% 
                              tolower()))

# Number of police departments sued
n_sued <- length(unique(filter(unested_department, pd_is_a_named_defendant == "Y")$departments %>% 
                          tolower()))

# Number of govenmental entities or other agencies sued
n_entities <- length(unique(unested_entity$new_column %>% tolower()))-1

# Number of parishes where we have filed suit
n_parishes <- length(unique(unested_department$new_column %>% tolower()))

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

# Defining a function to calculate the destinct sum across lawsuits
calculate_distinct_sum <- function(column_name) {
  result <- df %>%
    distinct(lawsuit_number, .keep_all = TRUE) %>%
    pull({{ column_name }}) %>%
    sum(na.rm = TRUE)
  
  return(result)
}

# Number of Qualified Immunity trial wins
n_qi_trial_wins <- calculate_distinct_sum("qi_trial_win_number")

# Number of Qualified Immunity appellate wins
n_qi_appellate_wins <- calculate_distinct_sum("qi_appellate_win_number")

# Number of absolute immunity trial wins
n_absolute_trial_wins <- calculate_distinct_sum("absolute_immunity_trial_win_number")

# Number of absolute immunity appellate wins
n_absolute_appellate_wins <- calculate_distinct_sum("absolute_immunity_appelate_win_number")

# Number Heck trial wins
n_heck_trial_wins <- calculate_distinct_sum("heck_trial_win_number")

# Number Heck appellate wins
n_heck_appellate_wins <-calculate_distinct_sum("heck_appellate_win_number")

# Number Lyons trial wins 
n_lyons_trial_wins <- calculate_distinct_sum("lyons_trial_win_number")

# Number Lyons appellate wins 
n_lyons_appellate_wins <- calculate_distinct_sum("lyons_appellate_win_number")

# Number SOL trial wins 
n_sol_trial_wins <- calculate_distinct_sum("sol_trial_win_number")

# Number SOL appellate wins 
n_sol_appellate_wins <- calculate_distinct_sum("sol_appellate_win_number")

# Number of Monell trial wins
n_monell_trial_wins <- calculate_distinct_sum("monell_trial_win_number")

# Number of Monell appellate wins
n_monell_appellate_wins <- calculate_distinct_sum("monell_appellate_win_number")

# Number of Settlements
n_settlements <- sum(df$settlement == "Y", na.rm = TRUE)

# Number of Individual Defendants Named in Settlements
n_defendants_in_settlements <- length(unique(filter(unested_officers, settlement == "Y")$new_column))

# Number of offers of judgment
n_judgement <- sum(df$offer_of_judgement, na.rm = TRUE)

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
          "Number of offers of judgment"
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
          n_judgement
          )

# Turning the two vectors above into a dataframe
values_df <- data.frame(item,value)

# Distribution of courts the cases are in
court_distribution <- df %>%
  tabyl(court_type) %>%
  arrange(desc(n))

# Distribution of parishes the cases are in 
parish_distribution <- df %>%
  tabyl(parishes) %>%
  arrange(desc(n))

# Distribution of police departments the cases are in
pd_distribution <- unested_department %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Distribution of officers in the cases
officer_distribution <- unested_officers %>%
  tabyl(new_column) %>%
  arrange(desc(n))

# Distribution of entities in the cases
entity_distribution <- unested_entity %>%
  tabyl(new_column, show_na = FALSE) %>%
  arrange(desc(n))

# Defining sheets for the spreadsheet
sheets = c("Values", 
           "Court Distribution", 
           "Parish Distribution", 
           "Officers Distribution", 
           "Entity Distribution")

# Defining sheet values
data_frames = list(values_df, 
                   court_distribution, 
                   parish_distribution, 
                   officer_distribution, 
                   entity_distribution)

# Adding the new sheets to the spreadsheet
for (i in seq_along(sheets)) {
  sheet_name <- sheets[i]
  data_frame <- data_frames[[i]]
  
  # Append the data frame to the sheet using the provided URL
  write_sheet(data_frame, ss = spreadsheet_link, sheet = sheet_name)
}
