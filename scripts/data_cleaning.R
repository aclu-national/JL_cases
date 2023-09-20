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
      str_detect(counts_classification, "wrongful arrest|false arrest|unlawful arrest|false imprison|unlawful imprison|detention") ~ "Unlawful Arrest or Confinement",
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