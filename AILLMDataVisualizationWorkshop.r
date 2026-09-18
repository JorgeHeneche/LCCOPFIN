# ============================================================
# AI DATA VISUALIZATION WORKSHOP
# R SCRIPT
# By: Jorge Heneche
# ============================================================


# ============================================================
# 1. INSTALL REQUIRED PACKAGES IF MISSING
# ============================================================

required_packages <- c(
  "readxl",
  "sf",
  "DBI",
  "RSQLite",
  "ggplot2"
)

missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(readxl)
library(sf)
library(DBI)
library(RSQLite)
library(ggplot2)


# ============================================================
# 3. DEFINE DATA LOCATION
# ============================================================

# Main workshop folder
folder <- "/Users/jorgeheneche/Downloads/AI-for-data-visualization-workshop-main"

# Workshop datasets are stored inside the data folder
data_folder <- file.path(
  folder,
  "data"
)


# Make sure R can find the folders
if (!dir.exists(folder)) {
  stop(
    paste(
      "Workshop folder was not found:",
      folder
    )
  )
}

if (!dir.exists(data_folder)) {
  stop(
    paste(
      "Data folder was not found:",
      data_folder
    )
  )
}


# ============================================================
# 4. LOAD CPS DATA
# ============================================================

CPS_merged <- read.csv(
  file.path(
    data_folder,
    "CPS_merged.csv"
  )
)


# ============================================================
# 5. LOAD FRENCH STATES DATA
# ============================================================

data_on_french_states <- read.csv(
  file.path(
    data_folder,
    "data_on_french_states.csv"
  )
)


# ============================================================
# 6. LOAD FINANCIAL SAMPLE DATA
# ============================================================

`Financial Sample` <- read_excel(
  file.path(
    data_folder,
    "Financial Sample.xlsx"
  )
)


# ============================================================
# 7. LOAD COMMUNES GEOJSON DATA
# ============================================================

communes <- st_read(
  file.path(
    data_folder,
    "communes.geojson"
  )
)


# ============================================================
# 8. CONNECT TO CHINOOK DATABASE
# ============================================================

chinook <- dbConnect(
  SQLite(),
  file.path(
    data_folder,
    "chinook.db"
  )
)


# ============================================================
# 9. CONFIRM EVERYTHING IS IN ENVIRONMENT
# ============================================================

print(
  ls()
)


# ============================================================
# 10. CHECK DATASET DIMENSIONS
# ============================================================

cat("\n========================================\n")
cat("DATA LOADED SUCCESSFULLY\n")
cat("========================================\n")

cat(
  "\nCPS_merged:",
  nrow(CPS_merged),
  "rows x",
  ncol(CPS_merged),
  "columns\n"
)

cat(
  "data_on_french_states:",
  nrow(data_on_french_states),
  "rows x",
  ncol(data_on_french_states),
  "columns\n"
)

cat(
  "Financial Sample:",
  nrow(`Financial Sample`),
  "rows x",
  ncol(`Financial Sample`),
  "columns\n"
)

cat(
  "communes:",
  nrow(communes),
  "rows x",
  ncol(communes),
  "columns\n"
)


# ============================================================
# 11. CHECK CHINOOK DATABASE TABLES
# ============================================================

chinook_tables <- dbListTables(
  chinook
)

cat("\nChinook database tables:\n")

print(
  chinook_tables
)


# ============================================================
# 12. CPS SCATTER PLOT:
#     MOBILITY VS. COLLEGE ENROLLMENT
# ============================================================

# Variables selected from CPS_merged
#
# X = school mobility percentage
# Y = school college enrollment percentage for Year 2

x_column <- "Mobility_Rate_Pct"

y_column <- "College_Enrollment_School_Pct_Year_2"


# Keep only variables needed for this analysis

CPS_plot_data <- CPS_merged[
  ,
  c(
    x_column,
    y_column
  )
]


# Convert variables to numeric

CPS_plot_data[[x_column]] <- as.numeric(
  CPS_plot_data[[x_column]]
)

CPS_plot_data[[y_column]] <- as.numeric(
  CPS_plot_data[[y_column]]
)


# Remove schools missing either variable

CPS_plot_data <- CPS_plot_data[
  complete.cases(CPS_plot_data),
]


cat("\n========================================\n")
cat("CPS VISUALIZATION\n")
cat("========================================\n")

cat(
  "\nX variable:",
  x_column,
  "\n"
)

cat(
  "Y variable:",
  y_column,
  "\n"
)

cat(
  "Schools included in plot:",
  nrow(CPS_plot_data),
  "\n"
)


# ============================================================
# 13. FIT LINEAR REGRESSION
# ============================================================

# Model:
#
# college enrollment = intercept + slope * mobility

CPS_model <- lm(
  College_Enrollment_School_Pct_Year_2 ~ Mobility_Rate_Pct,
  data = CPS_plot_data
)


# Display regression results

cat("\nLinear regression model:\n")

print(
  summary(CPS_model)
)


# ============================================================
# 14. CALCULATE 1-SIGMA CONFIDENCE INTERVAL
# ============================================================

# Create smooth mobility values across observed range

prediction_data <- data.frame(
  Mobility_Rate_Pct = seq(
    min(
      CPS_plot_data$Mobility_Rate_Pct
    ),
    max(
      CPS_plot_data$Mobility_Rate_Pct
    ),
    length.out = 300
  )
)


# Obtain fitted values and standard errors

model_predictions <- predict(
  CPS_model,
  newdata = prediction_data,
  se.fit = TRUE
)


# Add fitted regression line

prediction_data$fit <- as.numeric(
  model_predictions$fit
)


# Add 1-sigma confidence limits
#
# 1 sigma = fitted value +/- 1 standard error

prediction_data$lower_1sigma <- (
  prediction_data$fit -
    model_predictions$se.fit
)

prediction_data$upper_1sigma <- (
  prediction_data$fit +
    model_predictions$se.fit
)


# ============================================================
# 15. CREATE SCATTER PLOT
# ============================================================

CPS_mobility_plot <- ggplot(
  CPS_plot_data,
  aes(
    x = Mobility_Rate_Pct,
    y = College_Enrollment_School_Pct_Year_2
  )
) +
  
  # Individual schools
  geom_point(
    size = 2,
    alpha = 0.55
  ) +
  
  # 1-sigma confidence interval
  geom_ribbon(
    data = prediction_data,
    aes(
      x = Mobility_Rate_Pct,
      ymin = lower_1sigma,
      ymax = upper_1sigma
    ),
    inherit.aes = FALSE,
    alpha = 0.22
  ) +
  
  # Linear regression trendline
  geom_line(
    data = prediction_data,
    aes(
      x = Mobility_Rate_Pct,
      y = fit
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  # Axis labels and title
  labs(
    x = "Mobility percentage (%)",
    y = "College enrollment percentage (%)",
    title = "Mobility vs. College Enrollment in Chicago Public Schools"
  ) +
  
  # Publication-friendly theme
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5
    )
  )


# ============================================================
# 16. DISPLAY FIGURE
# ============================================================

print(
  CPS_mobility_plot
)


# ============================================================
# 17. SAVE FIGURE AS PDF
# ============================================================

output_pdf <- file.path(
  folder,
  "CPS_mobility_vs_college_enrollment_R.pdf"
)


ggsave(
  filename = output_pdf,
  plot = CPS_mobility_plot,
  device = "pdf",
  width = 7.2,
  height = 5.2,
  units = "in"
)


# ============================================================
# 18. FINAL CONFIRMATION
# ============================================================

cat("\n========================================\n")
cat("FIGURE CREATED SUCCESSFULLY\n")
cat("========================================\n")

cat(
  "\nPDF saved to:\n",
  output_pdf,
  "\n"
)
