# ==========================================================
# HCC Clinic Leadership Report — Volume + Demographics
#
# Author: Jorge Heneche
# Date:   12/18/2025
#
# Description:
# Combined script for HCC clinic volume metrics (weekly/monthly)
# and patient demographics summaries + figures, formatted for
# leadership reporting.
#
# Data objects expected in the environment:
#   1) HCCappts  (Date, NumAppt)
#   2) HCCPts    (dob, Zip, Gender, CancerType)
#
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ----------------------------
# Global settings
# ----------------------------
CLINIC_TAG <- "HCC"
NU_PURPLE  <- "#4E2A84"

# ==========================================================
# PART A — CLINIC VOLUME (Weekly + Monthly)
# ==========================================================

if (!exists("HCCappts")) {
  warning("HCCappts not found. Skipping PART A (Volume).")
} else {
  
  appts <- HCCappts %>%
    mutate(Date = as.Date(Date)) %>%
    filter(!is.na(Date))
  
  weekly_metrics <- appts %>%
    mutate(Week = floor_date(Date, "week")) %>%
    group_by(Week) %>%
    summarize(
      TotalAppts = sum(NumAppt, na.rm = TRUE),
      AvgApptsPerClinicDay = mean(NumAppt, na.rm = TRUE),
      MaxApptsInADay = max(NumAppt, na.rm = TRUE),
      ClinicDays = sum(!is.na(NumAppt)),
      .groups = "drop"
    ) %>%
    arrange(Week)
  
  print(weekly_metrics)
  
  monthly_metrics <- appts %>%
    mutate(Month = floor_date(Date, "month")) %>%
    group_by(Month) %>%
    summarize(
      TotalAppts = sum(NumAppt, na.rm = TRUE),
      AvgApptsPerClinicDay = mean(NumAppt, na.rm = TRUE),
      MaxApptsInADay = max(NumAppt, na.rm = TRUE),
      ClinicDays = sum(!is.na(NumAppt)),
      .groups = "drop"
    ) %>%
    arrange(Month)
  
  print(monthly_metrics)
  
  p_weekly_total <- ggplot(weekly_metrics, aes(x = Week, y = TotalAppts)) +
    geom_line(color = NU_PURPLE, linewidth = 1) +
    geom_point(color = NU_PURPLE, size = 2) +
    geom_text(aes(label = TotalAppts), vjust = -0.5, size = 3) +
    scale_x_date(
      breaks = sort(unique(weekly_metrics$Week)),
      date_labels = "%b %d"
    ) +
    labs(
      title = paste0("Weekly Total Appointments (", CLINIC_TAG, ")"),
      x = "Week Starting",
      y = "Total Appointments"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  print(p_weekly_total)
  
  p_weekly_avg <- ggplot(weekly_metrics, aes(x = Week, y = AvgApptsPerClinicDay)) +
    geom_line(color = NU_PURPLE, linewidth = 1) +
    geom_point(color = NU_PURPLE, size = 2) +
    geom_text(aes(label = round(AvgApptsPerClinicDay, 1)), vjust = -0.5, size = 3) +
    scale_x_date(
      breaks = sort(unique(weekly_metrics$Week)),
      date_labels = "%b %d"
    ) +
    labs(
      title = paste0("Weekly Average Appointments per Clinic Day (", CLINIC_TAG, ")"),
      x = "Week Starting",
      y = "Avg Appointments / Clinic Day"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  print(p_weekly_avg)
  
  p_monthly_total <- ggplot(monthly_metrics, aes(x = Month, y = TotalAppts)) +
    geom_line(color = NU_PURPLE, linewidth = 1) +
    geom_point(color = NU_PURPLE, size = 2) +
    geom_text(aes(label = TotalAppts), vjust = -0.5, size = 3) +
    scale_x_date(
      breaks = sort(unique(monthly_metrics$Month)),
      date_labels = "%b %Y"
    ) +
    labs(
      title = paste0("Monthly Total Appointments (", CLINIC_TAG, ")"),
      x = "Month",
      y = "Total Appointments"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  print(p_monthly_total)
  
  p_monthly_avg <- ggplot(monthly_metrics, aes(x = Month, y = AvgApptsPerClinicDay)) +
    geom_line(color = NU_PURPLE, linewidth = 1) +
    geom_point(color = NU_PURPLE, size = 2) +
    geom_text(aes(label = round(AvgApptsPerClinicDay, 1)), vjust = -0.5, size = 3) +
    scale_x_date(
      breaks = sort(unique(monthly_metrics$Month)),
      date_labels = "%b %Y"
    ) +
    labs(
      title = paste0("Monthly Average Appointments per Clinic Day (", CLINIC_TAG, ")"),
      x = "Month",
      y = "Avg Appointments / Clinic Day"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  print(p_monthly_avg)
  
  total_appts <- sum(appts$NumAppt, na.rm = TRUE)
  print(paste("Total appointments across all dates:", total_appts))
  
  weekly_overview <- weekly_metrics %>%
    summarize(
      NumWeeks = n(),
      AvgWeeklyAppts = round(mean(TotalAppts), 1),
      MinWeeklyAppts = min(TotalAppts),
      MaxWeeklyAppts = max(TotalAppts)
    )
}

# ==========================================================
# PART B — PATIENT DEMOGRAPHICS
# ==========================================================

if (!exists("HCCPts")) {
  warning("HCCPts not found. Skipping PART B (Demographics).")
} else {
  
  pick_col <- function(df, choices) {
    hit <- choices[choices %in% names(df)]
    if (length(hit) == 0) return(NULL)
    hit[[1]]
  }
  
  dob_col <- pick_col(HCCPts, c("dob","DOB"))
  zip_col <- pick_col(HCCPts, c("Zip","ZIP","zip"))
  gen_col <- pick_col(HCCPts, c("Gender","gender","SEX","sex"))
  can_col <- pick_col(HCCPts, c("CancerType","cancer_type","Cancer_Type","cancertype"))
  
  if (is.null(dob_col) || is.null(zip_col) || is.null(gen_col) || is.null(can_col)) {
    stop("Missing required columns in HCCPts. Need dob/Zip/Gender/CancerType (case-insensitive).")
  }
  
  pts <- HCCPts %>%
    transmute(
      DOB = .data[[dob_col]],
      Zip = .data[[zip_col]],
      Gender = .data[[gen_col]],
      CancerType = .data[[can_col]]
    ) %>%
    mutate(
      DOB = case_when(
        inherits(DOB, "Date") ~ DOB,
        inherits(DOB, "POSIXt") ~ as.Date(DOB),
        TRUE ~ suppressWarnings(parse_date_time(
          as.character(DOB),
          orders = c("ymd","mdy","dmy","Ymd","m/d/Y","d/m/Y","m-d-Y","d-b-Y")
        )) %>% as.Date()
      ),
      Age = ifelse(is.na(DOB), NA_integer_, floor(interval(DOB, Sys.Date()) / years(1))),
      Age = ifelse(!is.na(Age) & Age < 0, NA_integer_, Age),
      Zip = str_extract(as.character(Zip), "\\d{5}"),
      Gender = na_if(trimws(as.character(Gender)), ""),
      CancerType = na_if(trimws(as.character(CancerType)), "")
    )
  
  total_patients <- nrow(pts)
  print(paste("Total patients in dataset:", total_patients))
  
  age_summary <- pts %>%
    summarize(
      N_with_DOB = sum(!is.na(DOB)),
      Mean_Age   = mean(Age, na.rm = TRUE),
      Median_Age = median(Age, na.rm = TRUE),
      Min_Age    = min(Age, na.rm = TRUE),
      Max_Age    = max(Age, na.rm = TRUE)
    )
  print(age_summary)
  
  p_age_hist <- ggplot(pts, aes(x = Age)) +
    geom_histogram(binwidth = 5, fill = NU_PURPLE, color = "white", na.rm = TRUE) +
    labs(
      title = paste0("Age Distribution of Patients (", CLINIC_TAG, ")"),
      x = "Age (Years)",
      y = "Number of Patients"
    ) +
    theme_minimal()
  print(p_age_hist)
  
  gender_summary <- pts %>%
    filter(!is.na(Gender)) %>%
    group_by(Gender) %>%
    summarize(
      Count = n(),
      Percent = round(100 * n() / sum(n()), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Count))
  print(gender_summary)
  
  p_gender <- ggplot(gender_summary, aes(x = reorder(Gender, Count), y = Count)) +
    geom_col(fill = NU_PURPLE) +
    geom_text(aes(label = Count), vjust = -0.3, size = 4) +
    labs(
      title = paste0("Gender Distribution of Patients (", CLINIC_TAG, ")"),
      x = "Gender",
      y = "Number of Patients"
    ) +
    theme_minimal()
  print(p_gender)
  
  zip_summary <- pts %>%
    filter(!is.na(Zip)) %>%
    group_by(Zip) %>%
    summarize(Count = n(), .groups = "drop") %>%
    arrange(desc(Count))
  num_unique_zips <- n_distinct(pts$Zip, na.rm = TRUE)
  print(paste("Number of unique ZIP codes:", num_unique_zips))
  
  top_zips <- zip_summary %>% slice_head(n = 10)
  p_zip <- ggplot(top_zips, aes(x = reorder(Zip, Count), y = Count)) +
    geom_col(fill = NU_PURPLE) +
    geom_text(aes(label = Count), hjust = -0.1, size = 4) +
    coord_flip() +
    labs(
      title = paste0("Top 10 ZIP Codes by Patient Volume (", CLINIC_TAG, ")"),
      x = "ZIP Code",
      y = "Number of Patients"
    ) +
    theme_minimal()
  print(p_zip)
  
  cancer_summary <- pts %>%
    filter(!is.na(CancerType)) %>%
    group_by(CancerType) %>%
    summarize(Count = n(), .groups = "drop") %>%
    arrange(desc(Count)) %>%
    mutate(Percent = round(100 * Count / sum(Count), 1))
  print(cancer_summary)
  
  top_cancers <- cancer_summary %>% slice_head(n = 10)
  other_count <- if (nrow(cancer_summary) > 10) sum(cancer_summary$Count[-seq_len(10)], na.rm = TRUE) else 0
  
  cancer_plot_df <- top_cancers
  if (nrow(cancer_summary) > 10) {
    cancer_plot_df <- bind_rows(
      top_cancers,
      tibble(CancerType = "Other", Count = other_count,
             Percent = round(100 * other_count / sum(cancer_summary$Count), 1))
    )
  }
  
  p_cancer <- ggplot(cancer_plot_df, aes(x = reorder(CancerType, Count), y = Count)) +
    geom_col(fill = NU_PURPLE) +
    geom_text(aes(label = Count), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = paste0("Cancer Type Distribution (Top 10 + Other) (", CLINIC_TAG, ")"),
      x = "Cancer Type",
      y = "Number of Patients"
    ) +
    theme_minimal()
  print(p_cancer)
}

# ==========================================================
# PART C — COPY-READY NARRATIVE TEXT (Combined Report)
# ==========================================================

cat("\n================ COPY-READY TEXT ================\n\n")

if (exists("HCCappts")) {
  cat(
    "Since its launch, the", CLINIC_TAG,
    "clinic has completed a total of", total_appts,
    "appointments across", weekly_overview$NumWeeks, "weeks of operation.",
    "The clinic averaged", weekly_overview$AvgWeeklyAppts,
    "appointments per week, with weekly volume ranging from",
    weekly_overview$MinWeeklyAppts, "to",
    weekly_overview$MaxWeeklyAppts, "appointments.\n\n"
  )
}

if (exists("HCCPts")) {
  cat(
    "The", CLINIC_TAG, "patient dataset included", total_patients, "patients.",
    "The median age was", age_summary$Median_Age,
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
    "Patients resided across", num_unique_zips,
    "unique ZIP codes, with the highest patient volume originating from the top 10 ZIP codes shown in the accompanying figure.\n"
  )
  
  if (exists("cancer_summary") && nrow(cancer_summary) > 0) {
    top_ct <- cancer_summary %>%
      slice_head(n = 3) %>%
      mutate(Text = paste0(CancerType, " (", Count, ", ", Percent, "%)")) %>%
      pull(Text)
    
    cat(
      "\nThe most common cancer types were:",
      paste(top_ct, collapse = ", "),
      ".\n"
    )
  }
}
