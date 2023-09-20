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

# Initial lawsuit counts simplified
counts_table_simplified <- unested_counts %>%
  mutate(
    counts_classification_simplified = case_when(
      counts_classification == "Excessive Force" ~ "Excessive Force",
      counts_classification == "Discrimination" ~ "Discrimination",
      counts_classification == "Unlawful Arrest or Confinement" ~ "Unlawful Arrest or Confinement",
      counts_classification == "Unlawful Stop, Search, or Seizure" ~ "Unlawful Stop, Search, or Seizure",
      counts_classification == "First Amendment Violation" ~ "First Amendment Violation",
      counts_classification == "Negligence" ~ "Negligence",
      counts_classification == "Infliction of Emotional Distress" ~ "Infliction of Emotional Distress",
      counts_classification == "Wrongful Death" ~ "Wrongful Death",
      TRUE ~ "Other Counts"
    )
  ) %>%
  tabyl(counts_classification_simplified)

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