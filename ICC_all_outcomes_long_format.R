# ICC_all_outcomes_long_format.R
# Input file: ICC_Raw_Validation_Data_Long.xlsx
# Required sheet: ICC_Long_Data
# Required columns:
# Specimen_ID | Outcome | Examiner1_Trial1 | Examiner1_Trial2 | Examiner2_Trial1

# Install packages automatically if they are not already installed
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("irr", quietly = TRUE)) install.packages("irr")

library(readxl)
library(irr)

# Select ICC_Raw_Validation_Data_Long.xlsx when the file browser opens
input_file <- file.choose()

# Import long-format ICC data
dat <- read_excel(input_file, sheet = "ICC_Long_Data")

# Confirm that all required columns exist
required_columns <- c(
  "Specimen_ID",
  "Outcome",
  "Examiner1_Trial1",
  "Examiner1_Trial2",
  "Examiner2_Trial1"
)

if (!all(required_columns %in% names(dat))) {
  stop(
    "The Excel sheet must contain these columns: ",
    paste(required_columns, collapse = ", ")
  )
}

# Function: ICC(2,1), two-way random-effects, absolute agreement,
# single-measurement model, with 95% confidence interval
compute_icc <- function(x, outcome_label) {
  
  fit <- irr::icc(
    x,
    model = "twoway",
    type = "agreement",
    unit = "single"
  )
  
  data.frame(
    Outcome = outcome_label,
    ICC_2_1 = fit$value,
    CI_lower_95 = fit$lbound,
    CI_upper_95 = fit$ubound,
    F_value = fit$Fvalue,
    p_value = fit$p.value,
    stringsAsFactors = FALSE
  )
}

# Identify the four outcomes automatically
outcomes <- unique(dat$Outcome)

# Create an empty output table
results <- data.frame()

# Run ICC analysis for every outcome
for (outcome_name in outcomes) {
  
  # Keep data only for the current outcome
  d <- dat[dat$Outcome == outcome_name, ]
  
  # Intra-examiner reliability:
  # Examiner 1, first measurement vs Examiner 1, repeated measurement
  intra <- compute_icc(
    d[, c("Examiner1_Trial1", "Examiner1_Trial2")],
    outcome_name
  )
  
  intra$Reliability <- "Intra-examiner"
  intra$N <- nrow(d)
  
  # Inter-examiner reliability:
  # Examiner 1, first measurement vs Examiner 2, independent measurement
  inter <- compute_icc(
    d[, c("Examiner1_Trial1", "Examiner2_Trial1")],
    outcome_name
  )
  
  inter$Reliability <- "Inter-examiner"
  inter$N <- nrow(d)
  
  # Add both results to the final results table
  results <- rbind(results, intra, inter)
}

# Organize output columns
results <- results[, c(
  "Outcome",
  "Reliability",
  "N",
  "ICC_2_1",
  "CI_lower_95",
  "CI_upper_95",
  "F_value",
  "p_value"
)]

# Round numerical results for clear reporting
results[, c(
  "ICC_2_1",
  "CI_lower_95",
  "CI_upper_95",
  "F_value",
  "p_value"
)] <- lapply(
  results[, c(
    "ICC_2_1",
    "CI_lower_95",
    "CI_upper_95",
    "F_value",
    "p_value"
  )],
  round,
  digits = 3
)

# Display the eight ICC results in the RStudio Console
print(results, row.names = FALSE)

# Save the results in the current R working directory
write.csv(
  results,
  "ICC_reliability_results_all_outcomes.csv",
  row.names = FALSE
)