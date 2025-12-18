# ==========================================================
# HGICC Clinic Metrics & Demographics Report
#
# Author: Jorge Heneche
# Date:   12/17/2025
#
# Description:
# This script generates weekly and monthly clinic volume
# metrics, patient demographics summaries, and
# publication-ready figures for the Hispanic GI Cancer
# Clinic (HGICC).
#
# Data objects expected in the environment:
#   1) HGICCAppts25  (Date, NumberPts)
#   2) HGICCDIPts    (DOB, Zip, Gender)
#
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ----------------------------
# Global settings
# ----------------------------
CLINIC_TAG <- "HGICC"
NU_PURPLE  <- "#4E2A84"

# ==========================================================
# PART A — CLINIC VOLUME (Weekly + Monthly)
# ==========================================================

# Prepare appointments dataset
appts <- HGICCAppts25 %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date))

# ---- Weekly metrics table ----
weekly_metrics <- appts %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarize(
    TotalPts = sum(NumberPts, na.rm = TRUE),
    AvgPtsPerClinicDay = mean(NumberPts, na.rm = TRUE),
    MaxPtsInADay = max(NumberPts, na.rm = TRUE),
    ClinicDays = sum(!is.na(NumberPts)),
    .groups = "drop"
  ) %>%
  arrange(Week)

print(weekly_metrics)

# ---- Monthly metrics table ----
monthly_metrics <- appts %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarize(
    TotalPts = sum(NumberPts, na.rm = TRUE),
    AvgPtsPerClinicDay = mean(NumberPts, na.rm = TRUE),
    MaxPtsInADay = max(NumberPts, na.rm = TRUE),
    ClinicDays = sum(!is.na(NumberPts)),
    .groups = "drop"
  ) %>%
  arrange(Month)

print(monthly_metrics)

# ---- Weekly total plot ----
p_weekly_total <- ggplot(weekly_metrics, aes(x = Week, y = TotalPts)) +
  geom_line(color = NU_PURPLE, linewidth = 1) +
  geom_point(color = NU_PURPLE, size = 2) +
  geom_text(aes(label = TotalPts), vjust = -0.5, size = 3) +
  scale_x_date(
    breaks = sort(unique(weekly_metrics$Week)),
    date_labels = "%b %d"
  ) +
  labs(
    title = paste0("Weekly Total Patient Appointments (", CLINIC_TAG, ")"),
    x = "Week Starting",
    y = "Total Patients"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p_weekly_total)

# ---- Weekly average per clinic day plot ----
p_weekly_avg <- ggplot(weekly_metrics, aes(x = Week, y = AvgPtsPerClinicDay)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(
    aes(label = round(AvgPtsPerClinicDay, 1)),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_date(
    breaks = sort(unique(weekly_metrics$Week)),
    date_labels = "%b %d"
  ) +
  labs(
    title = paste0(
      "Weekly Average Patients per Clinic Day (", CLINIC_TAG, ")"
    ),
    x = "Week Starting",
    y = "Avg Patients / Clinic Day"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p_weekly_avg)

# ---- Monthly total plot ----
p_monthly_total <- ggplot(monthly_metrics, aes(x = Month, y = TotalPts)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "firebrick", size = 2) +
  geom_text(aes(label = TotalPts), vjust = -0.5, size = 3) +
  scale_x_date(
    breaks = sort(unique(monthly_metrics$Month)),
    date_labels = "%b %Y"
  ) +
  labs(
    title = paste0(
      "Monthly Total Patient Appointments (", CLINIC_TAG, ")"
    ),
    x = "Month",
    y = "Total Patients"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p_monthly_total)

# ---- Monthly average per clinic day plot ----
p_monthly_avg <- ggplot(monthly_metrics, aes(x = Month, y = AvgPtsPerClinicDay)) +
  geom_line(color = NU_PURPLE, linewidth = 1) +
  geom_point(color = NU_PURPLE, size = 2) +
  geom_text(
    aes(label = round(AvgPtsPerClinicDay, 1)),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_date(
    breaks = sort(unique(monthly_metrics$Month)),
    date_labels = "%b %Y"
  ) +
  labs(
    title = paste0(
      "Monthly Average Patients per Clinic Day (", CLINIC_TAG, ")"
    ),
    x = "Month",
    y = "Avg Patients / Clinic Day"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p_monthly_avg)

# ---- Overall clinic volume ----
total_patients <- sum(appts$NumberPts, na.rm = TRUE)
print(paste("Total patients across all dates:", total_patients))

# ==========================================================
# PART B — PATIENT DEMOGRAPHICS
# ==========================================================

pts <- HGICCDIPts %>%
  mutate(
    DOB = as.Date(DOB),
    Age = floor(interval(DOB, Sys.Date()) / years(1))
  )

# ---- Age summary ----
age_summary <- pts %>%
  summarize(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE)
  )

print(age_summary)

# ---- Age histogram ----
p_age_hist <- ggplot(pts, aes(x = Age)) +
  geom_histogram(
    binwidth = 5,
    fill = NU_PURPLE,
    color = "white"
  ) +
  labs(
    title = paste0(
      "Age Distribution of Patients (", CLINIC_TAG, ")"
    ),
    x = "Age (Years)",
    y = "Number of Patients"
  ) +
  theme_minimal()

print(p_age_hist)

# ---- Gender summary + plot ----
gender_summary <- pts %>%
  group_by(Gender) %>%
  summarize(
    Count = n(),
    Percent = round(100 * n() / sum(n()), 1),
    .groups = "drop"
  )

print(gender_summary)

p_gender <- ggplot(gender_summary, aes(x = Gender, y = Count)) +
  geom_col(fill = NU_PURPLE) +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  labs(
    title = paste0(
      "Gender Distribution of Patients (", CLINIC_TAG, ")"
    ),
    x = "Gender",
    y = "Number of Patients"
  ) +
  theme_minimal()

print(p_gender)

# ---- ZIP summary + top 10 plot ----
zip_summary <- pts %>%
  group_by(Zip) %>%
  summarize(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

num_unique_zips <- n_distinct(pts$Zip)
print(paste("Number of unique ZIP codes:", num_unique_zips))

top_zips <- zip_summary %>% slice_head(n = 10)

p_zip <- ggplot(top_zips, aes(x = reorder(Zip, Count), y = Count)) +
  geom_col(fill = NU_PURPLE) +
  geom_text(aes(label = Count), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = paste0(
      "Top 10 ZIP Codes by Patient Volume (", CLINIC_TAG, ")"
    ),
    x = "ZIP Code",
    y = "Number of Patients"
  ) +
  theme_minimal()

print(p_zip)

# ==========================================================
# PART C — COPY-READY NARRATIVE TEXT
# ==========================================================

weekly_overview <- weekly_metrics %>%
  summarize(
    NumWeeks = n(),
    AvgWeeklyPts = round(mean(TotalPts), 1),
    MinWeeklyPts = min(TotalPts),
    MaxWeeklyPts = max(TotalPts)
  )

cat(
  "During the study period, the", CLINIC_TAG,
  "clinic saw a total of", total_patients,
  "patients across", weekly_overview$NumWeeks, "weeks.",
  "The clinic averaged", weekly_overview$AvgWeeklyPts,
  "patients per week, with weekly volume ranging from",
  weekly_overview$MinWeeklyPts, "to",
  weekly_overview$MaxWeeklyPts, "patients.\n\n"
)

cat(
  "The median age of patients seen in the", CLINIC_TAG,
  "clinic was", age_summary$Median_Age,
  "years (range:", age_summary$Min_Age, "–", age_summary$Max_Age,
  "), with a mean age of", round(age_summary$Mean_Age, 1),
  "years.\n\n"
)

gender_text <- gender_summary %>%
  mutate(Text = paste0(Count, " ", Gender, " (", Percent, "%)")) %>%
  pull(Text)

cat(
  "The gender distribution of patients was:",
  paste(gender_text, collapse = ", "),
  ".\n\n"
)

cat(
  "Patients seen in the", CLINIC_TAG,
  "clinic resided across", num_unique_zips,
  "unique ZIP codes, with the highest patient volume originating from the top 10 ZIP codes shown in the accompanying figure.\n"
)
