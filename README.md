# Justice Lab Case Statistics

## Overview
In this project, we analyze court cases conducted by [Justice Lab](https://www.aclujusticelab.org/cases/) to create a dashboard to help people understand the breadth of Justice Lab's work in Louisiana. The raw data is collected in [this google sheet](https://docs.google.com/spreadsheets/d/e/2PACX-1vRL9lA_QjTdNAp-X6aoDFF0p9WUPd72VzvW48AHAxYHDrEkTsjuaR9sMoF3bWd4SDtHRIDPnWx1Wk9y/pub?gid=585513752&single=true&output=csv) and is updated regularly.

## Replication
To replicate the analysis simply clone the repository and run `master_script.R`. This will prompt you to sign into an Gmail account to edit the sheet, which you cannot do. Rather than doing this you may download the raw sheet and run both `data_cleaning.R`, uploading the raw sheet rather than using the Google sheet, and `data_analysis.R`. 
