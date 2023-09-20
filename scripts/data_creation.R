# ------------------------- Exporting content into a Google Sheet --------------------------------------

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
                   counts_table_simplified)

# Adding the new sheets to the spreadsheet
for (i in seq_along(sheets)) {
  sheet_name <- sheets[i]
  data_frame <- data_frames[[i]]
  
  # Append the data frame to the sheet using the provided URL
  write_sheet(data_frame, ss = spreadsheet_link, sheet = sheet_name)
}
