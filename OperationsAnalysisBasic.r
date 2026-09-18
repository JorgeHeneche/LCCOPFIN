#By: Jorge Heneche
#Date: August 2026
#Description: Basic script for operations analysis

# ------------------------------------------------------------
# 1. INSTALL AND LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------

# Run this line only once if the packages are not already installed
install.packages(c("tidyverse", "readxl", "janitor"))

# Load required libraries
library(tidyverse)   # Data cleaning, manipulation, and visualization
library(readxl)      # Import Excel files
library(janitor)     # Clean and standardize column names


# ------------------------------------------------------------
# 2. IMPORT DATA FROM NMDemographicsforSites.xlsx
# ------------------------------------------------------------

# Define the location of the Excel workbook
file_path <- "/Users/jorgeheneche/Downloads/NMDemographicsforSites.xlsx"

# Confirm that R can locate the file
file.exists(file_path)

# View all sheet names in the Excel workbook
excel_sheets(file_path)


# Import each Excel sheet as a separate R dataframe

departments <- read_excel(
  file_path,
  sheet = "departments"
)

pt_by_department <- read_excel(
  file_path,
  sheet = "pt_by_department"
)

ptgender_by_department <- read_excel(
  file_path,
  sheet = "ptgender_by_department"
)

ptre_by_department <- read_excel(
  file_path,
  sheet = "ptre_by_department"
)

pt_by_group <- read_excel(
  file_path,
  sheet = "pt_by_group"
)

ptgender_by_group <- read_excel(
  file_path,
  sheet = "ptgender_by_group"
)

ptre_by_group <- read_excel(
  file_path,
  sheet = "ptre_by_group"
)


# Clean and standardize column names in each dataframe

departments <- departments |> clean_names()
pt_by_department <- pt_by_department |> clean_names()
ptgender_by_department <- ptgender_by_department |> clean_names()
ptre_by_department <- ptre_by_department |> clean_names()

pt_by_group <- pt_by_group |> clean_names()
ptgender_by_group <- ptgender_by_group |> clean_names()
ptre_by_group <- ptre_by_group |> clean_names()

# ------------------------------------------------------------
# 3. INSPECT GROUP-LEVEL DATA BEFORE ANALYSIS
# ------------------------------------------------------------

# These are the three datasets that will be used for
# requested analysis:
#   pt_by_group       = total patients by location group
#   ptgender_by_group = gender distribution by location group
#   ptre_by_group     = race/ethnicity distribution by location group


# ------------------------------------------------------------
# 3A. CHECK DATASET STRUCTURE
# ------------------------------------------------------------

glimpse(pt_by_group)
glimpse(ptgender_by_group)
glimpse(ptre_by_group)


# ------------------------------------------------------------
# 3B. CHECK LOCATION GROUPS
# ------------------------------------------------------------

unique(pt_by_group$location_group)

unique(ptgender_by_group$location_group)

unique(ptre_by_group$location_group)


# ------------------------------------------------------------
# 3C. CHECK GENDER CATEGORIES
# ------------------------------------------------------------

unique(ptgender_by_group$legal_sex)


# ------------------------------------------------------------
# 3D. CHECK RACE/ETHNICITY CATEGORIES
# ------------------------------------------------------------

unique(ptre_by_group$race_ethnicity)


# ------------------------------------------------------------
# 3E. CHECK FOR MISSING VALUES
# ------------------------------------------------------------

colSums(is.na(pt_by_group))

colSums(is.na(ptgender_by_group))

colSums(is.na(ptre_by_group))

# ------------------------------------------------------------
# 3. CREATE THE 3 ANALYSIS GROUPS
# ------------------------------------------------------------

# Requested analysis groups:
#   Bronzeville = Bronzeville
#   Breast = Oak Brook + Orland Park + Palos Heights
#   Gyne Onc = Prentice (GYN Oncology)


# ------------------------------------------------------------
# 3A. CREATE ANALYSIS-READY GENDER DATASET
# ------------------------------------------------------------

gender_analysis <- ptgender_by_group |>
  mutate(
    analysis_group = case_when(
      location_group == "Bronzeville" ~ "Bronzeville",
      
      location_group %in% c(
        "Oak Brook (Breast)",
        "Orland Park (Breast)",
        "Palos Heights (Breast)"
      ) ~ "Breast",
      
      location_group == "Prentice (GYN Oncology)" ~ "Gyne Onc",
      
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 3B. CREATE ANALYSIS-READY RACE/ETHNICITY DATASET
# ------------------------------------------------------------

race_ethnicity_analysis <- ptre_by_group |>
  mutate(
    analysis_group = case_when(
      location_group == "Bronzeville" ~ "Bronzeville",
      
      location_group %in% c(
        "Oak Brook (Breast)",
        "Orland Park (Breast)",
        "Palos Heights (Breast)"
      ) ~ "Breast",
      
      location_group == "Prentice (GYN Oncology)" ~ "Gyne Onc",
      
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 3C. CREATE ANALYSIS-READY PATIENT TOTALS DATASET
# ------------------------------------------------------------

patient_totals_analysis <- pt_by_group |>
  mutate(
    analysis_group = case_when(
      location_group == "Bronzeville" ~ "Bronzeville",
      
      location_group %in% c(
        "Oak Brook (Breast)",
        "Orland Park (Breast)",
        "Palos Heights (Breast)"
      ) ~ "Breast",
      
      location_group == "Prentice (GYN Oncology)" ~ "Gyne Onc",
      
      TRUE ~ NA_character_
    )
  )
# ------------------------------------------------------------
# 3D. VERIFY GROUP ASSIGNMENTS
# ------------------------------------------------------------

gender_analysis |>
  distinct(location_group, analysis_group)

race_ethnicity_analysis |>
  distinct(location_group, analysis_group)

patient_totals_analysis |>
  distinct(location_group, analysis_group)

# Check that no rows failed to receive an analysis group

sum(is.na(gender_analysis$analysis_group))

sum(is.na(race_ethnicity_analysis$analysis_group))

sum(is.na(patient_totals_analysis$analysis_group))


# ------------------------------------------------------------
# 4. GENDER ANALYSIS
# ------------------------------------------------------------

# Goal:
# Generate gender counts and percentages for
# three requested analysis groups:
#   1. Bronzeville
#   2. Breast
#   3. Gyne Onc
#
# Breast combines:
#   - Oak Brook (Breast)
#   - Orland Park (Breast)
#   - Palos Heights (Breast)
#
# Because the three Breast locations are being combined,
# percentages are recalculated from the summed counts
# rather than averaging the original percentages.


# ------------------------------------------------------------
# 4A. SUM GENDER COUNTS WITHIN EACH ANALYSIS GROUP
# ------------------------------------------------------------

gender_summary <- gender_analysis |>
  group_by(analysis_group, legal_sex) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 4B. CALCULATE TOTAL PATIENTS AND GENDER PERCENTAGES
# ------------------------------------------------------------

gender_summary <- gender_summary |>
  group_by(analysis_group) |>
  mutate(
    total_patients = sum(n),
    pct = (n / total_patients) * 100
  ) |>
  ungroup()


# ------------------------------------------------------------
# 4C. FORMAT GENDER LABELS AND PERCENTAGES
# ------------------------------------------------------------

gender_summary <- gender_summary |>
  mutate(
    
    # Convert abbreviated legal sex values into readable labels
    legal_sex = recode(
      legal_sex,
      "F" = "Female",
      "M" = "Male",
      "X" = "X"
    ),
    
    # Round percentages to one decimal place
    pct = round(pct, 1),
    
    # Set the order of the three requested groups
    analysis_group = factor(
      analysis_group,
      levels = c(
        "Bronzeville",
        "Breast",
        "Gyne Onc"
      )
    )
  ) |>
  
  # Arrange results by analysis group
  arrange(analysis_group, desc(n))


# ------------------------------------------------------------
# 4D. VIEW FINAL GENDER SUMMARY
# ------------------------------------------------------------

gender_summary

# ------------------------------------------------------------
# 5. RACE/ETHNICITY ANALYSIS
# ------------------------------------------------------------

# Goal:
# Generate race/ethnicity counts and percentages for
# three requested analysis groups:
#   1. Bronzeville
#   2. Breast
#   3. Gyne Onc
#
# Breast combines:
#   - Oak Brook (Breast)
#   - Orland Park (Breast)
#   - Palos Heights (Breast)
#
# Because the three Breast locations are being combined,
# percentages are recalculated from the summed counts
# rather than averaging the original percentages.


# ------------------------------------------------------------
# 5A. SUM RACE/ETHNICITY COUNTS WITHIN EACH ANALYSIS GROUP
# ------------------------------------------------------------

race_ethnicity_summary <- race_ethnicity_analysis |>
  group_by(analysis_group, race_ethnicity) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 5B. CALCULATE TOTAL PATIENTS AND RACE/ETHNICITY PERCENTAGES
# ------------------------------------------------------------

race_ethnicity_summary <- race_ethnicity_summary |>
  group_by(analysis_group) |>
  mutate(
    total_patients = sum(n),
    pct = (n / total_patients) * 100
  ) |>
  ungroup()


# ------------------------------------------------------------
# 5C. FORMAT PERCENTAGES AND ANALYSIS GROUP ORDER
# ------------------------------------------------------------

race_ethnicity_summary <- race_ethnicity_summary |>
  mutate(
    
    # Round percentages to one decimal place
    pct = round(pct, 1),
    
    # Set the order of Denisha's three requested groups
    analysis_group = factor(
      analysis_group,
      levels = c(
        "Bronzeville",
        "Breast",
        "Gyne Onc"
      )
    )
  ) |>
  
  # Arrange race/ethnicity categories from largest to smallest
  # within each analysis group
  arrange(analysis_group, desc(n))


# ------------------------------------------------------------
# 5D. VIEW FINAL RACE/ETHNICITY SUMMARY
# ------------------------------------------------------------

race_ethnicity_summary


# ------------------------------------------------------------
# 5E. QUALITY CONTROL CHECKS
# ------------------------------------------------------------

# Check total race/ethnicity counts for each analysis group

race_ethnicity_summary |>
  group_by(analysis_group) |>
  summarise(
    total_n = sum(n)
  )


# Check that percentages total approximately 100%
# Small differences from 100% may occur because of rounding.

race_ethnicity_summary |>
  group_by(analysis_group) |>
  summarise(
    total_pct = sum(pct)
  )

# ------------------------------------------------------------
# 6. CREATE RACE/ETHNICITY GRAPHS
# ------------------------------------------------------------

# Goal:
# Create both horizontal bar charts and pie charts for:
#   1. Bronzeville
#   2. Breast
#   3. Gyne Onc
#
# All race/ethnicity categories are retained.
#
# Bar charts:
#   - Each race/ethnicity category receives its own color.
#   - The same colors are used across all three groups.
#   - Percentages are displayed at the end of each bar.
#
# Pie charts:
#   - All race/ethnicity categories are retained.
#   - Larger percentages are displayed inside the pie.
#   - Small percentages are displayed outside the pie so
#     they are not lost or hidden.
#
# Percentages below 0.1% are displayed as <0.1%.
# The underlying numeric percentage values are not changed.


# ------------------------------------------------------------
# 6A. PREPARE GRAPH-READY DATA
# ------------------------------------------------------------

race_ethnicity_plot <- race_ethnicity_summary |>
  mutate(
    
    # --------------------------------------------------------
    # Create percentage labels for display
    # --------------------------------------------------------
    
    # Because pct was previously rounded to one decimal place,
    # use the original counts and total patients here to identify
    # true percentages below 0.1%.
    
    pct_exact = (n / total_patients) * 100,
    
    pct_label = case_when(
      pct_exact > 0 & pct_exact < 0.1 ~ "<0.1%",
      TRUE ~ paste0(sprintf("%.1f", pct_exact), "%")
    ),
    
    
    # --------------------------------------------------------
    # Create presentation-friendly race/ethnicity labels
    # --------------------------------------------------------
    
    # This changes only the graph labels.
    # The original race_ethnicity variable remains unchanged.
    
    race_ethnicity_label = recode(
      race_ethnicity,
      
      "Asian NH" =
        "Asian (Non-Hispanic)",
      
      "Black or African American NH" =
        "Black or African American (Non-Hispanic)",
      
      "White NH" =
        "White (Non-Hispanic)",
      
      "Native Hawaiian or Other Pacific Islander NH" =
        "Native Hawaiian or Other Pacific Islander (Non-Hispanic)",
      
      "American Indian or Alaska Native NH" =
        "American Indian or Alaska Native (Non-Hispanic)",
      
      "Hispanic or Latino" =
        "Hispanic or Latino",
      
      "More than One Race" =
        "More than One Race",
      
      "Unknown or Not Reported" =
        "Unknown or Not Reported"
    )
  )


# ------------------------------------------------------------
# 6B. SET CONSISTENT COLORS FOR RACE/ETHNICITY CATEGORIES
# ------------------------------------------------------------

# Using a named color vector ensures that a category has the
# same color in Bronzeville, Breast, and Gyne Onc.

race_colors <- c(
  "Asian (Non-Hispanic)" = "#56B4E9",
  "Black or African American (Non-Hispanic)" = "#0072B2",
  "Hispanic or Latino" = "#E69F00",
  "More than One Race" = "#CC79A7",
  "Unknown or Not Reported" = "#999999",
  "White (Non-Hispanic)" = "#009E73",
  "Native Hawaiian or Other Pacific Islander (Non-Hispanic)" = "#F0E442",
  "American Indian or Alaska Native (Non-Hispanic)" = "#D55E00"
)


# ------------------------------------------------------------
# 6C. BRONZEVILLE BAR CHART
# ------------------------------------------------------------

bronzeville_bar <- race_ethnicity_plot |>
  filter(analysis_group == "Bronzeville") |>
  ggplot(
    aes(
      x = reorder(race_ethnicity_label, pct_exact),
      y = pct_exact,
      fill = race_ethnicity_label
    )
  ) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(label = pct_label),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Race and Ethnicity - Bronzeville",
    subtitle = "N = 490",
    x = NULL,
    y = "Percent of Patients"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

bronzeville_bar


# ------------------------------------------------------------
# 6D. BREAST BAR CHART
# ------------------------------------------------------------

breast_bar <- race_ethnicity_plot |>
  filter(analysis_group == "Breast") |>
  ggplot(
    aes(
      x = reorder(race_ethnicity_label, pct_exact),
      y = pct_exact,
      fill = race_ethnicity_label
    )
  ) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(label = pct_label),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Race and Ethnicity - Breast",
    subtitle = "N = 2,000",
    x = NULL,
    y = "Percent of Patients"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

breast_bar


# ------------------------------------------------------------
# 6E. GYNE ONC BAR CHART
# ------------------------------------------------------------

gyne_onc_bar <- race_ethnicity_plot |>
  filter(analysis_group == "Gyne Onc") |>
  ggplot(
    aes(
      x = reorder(race_ethnicity_label, pct_exact),
      y = pct_exact,
      fill = race_ethnicity_label
    )
  ) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(label = pct_label),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Race and Ethnicity - Gyne Onc",
    subtitle = "N = 3,655",
    x = NULL,
    y = "Percent of Patients"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

gyne_onc_bar


# ------------------------------------------------------------
# 6F. PREPARE BRONZEVILLE PIE DATA
# ------------------------------------------------------------

bronzeville_pie_data <- race_ethnicity_plot |>
  filter(analysis_group == "Bronzeville") |>
  arrange(desc(race_ethnicity_label)) |>
  mutate(
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0),
    label_position = (ymax + ymin) / 2,
    
    # Larger categories will be labeled inside the pie.
    # Small categories will be labeled outside.
    inside_label = if_else(
      pct_exact >= 3,
      pct_label,
      ""
    ),
    
    outside_label = if_else(
      pct_exact < 3,
      pct_label,
      ""
    )
  )


# ------------------------------------------------------------
# 6G. BRONZEVILLE PIE CHART
# ------------------------------------------------------------

bronzeville_pie <- ggplot(
  bronzeville_pie_data,
  aes(
    x = 1,
    y = n,
    fill = race_ethnicity_label
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  
  # Percentages for larger slices
  geom_text(
    aes(label = inside_label),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold"
  ) +
  
  # Percentages for small slices
  geom_text(
    data = bronzeville_pie_data |>
      filter(pct_exact < 3),
    aes(
      x = 1.35,
      y = label_position,
      label = outside_label
    ),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  
  coord_polar(
    theta = "y",
    clip = "off"
  ) +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  xlim(0.5, 1.6) +
  labs(
    title = "Race and Ethnicity - Bronzeville",
    subtitle = "N = 490",
    fill = "Race/Ethnicity"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "right"
  )

bronzeville_pie


# ------------------------------------------------------------
# 6H. PREPARE BREAST PIE DATA
# ------------------------------------------------------------

breast_pie_data <- race_ethnicity_plot |>
  filter(analysis_group == "Breast") |>
  arrange(desc(race_ethnicity_label)) |>
  mutate(
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0),
    label_position = (ymax + ymin) / 2,
    
    inside_label = if_else(
      pct_exact >= 3,
      pct_label,
      ""
    ),
    
    outside_label = if_else(
      pct_exact < 3,
      pct_label,
      ""
    )
  )


# ------------------------------------------------------------
# 6I. BREAST PIE CHART
# ------------------------------------------------------------

breast_pie <- ggplot(
  breast_pie_data,
  aes(
    x = 1,
    y = n,
    fill = race_ethnicity_label
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  geom_text(
    aes(label = inside_label),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    data = breast_pie_data |>
      filter(pct_exact < 3),
    aes(
      x = 1.35,
      y = label_position,
      label = outside_label
    ),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  coord_polar(
    theta = "y",
    clip = "off"
  ) +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  xlim(0.5, 1.6) +
  labs(
    title = "Race and Ethnicity - Breast",
    subtitle = "N = 2,000",
    fill = "Race/Ethnicity"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "right"
  )

breast_pie


# ------------------------------------------------------------
# 6J. PREPARE GYNE ONC PIE DATA
# ------------------------------------------------------------

gyne_onc_pie_data <- race_ethnicity_plot |>
  filter(analysis_group == "Gyne Onc") |>
  arrange(desc(race_ethnicity_label)) |>
  mutate(
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0),
    label_position = (ymax + ymin) / 2,
    
    inside_label = if_else(
      pct_exact >= 3,
      pct_label,
      ""
    ),
    
    outside_label = if_else(
      pct_exact < 3,
      pct_label,
      ""
    )
  )


# ------------------------------------------------------------
# 6K. GYNE ONC PIE CHART
# ------------------------------------------------------------

gyne_onc_pie <- ggplot(
  gyne_onc_pie_data,
  aes(
    x = 1,
    y = n,
    fill = race_ethnicity_label
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  geom_text(
    aes(label = inside_label),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    data = gyne_onc_pie_data |>
      filter(pct_exact < 3),
    aes(
      x = 1.35,
      y = label_position,
      label = outside_label
    ),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  coord_polar(
    theta = "y",
    clip = "off"
  ) +
  scale_fill_manual(
    values = race_colors,
    drop = FALSE
  ) +
  xlim(0.5, 1.6) +
  labs(
    title = "Race and Ethnicity - Gyne Onc",
    subtitle = "N = 3,655",
    fill = "Race/Ethnicity"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10),
    legend.position = "right"
  )

gyne_onc_pie


# ------------------------------------------------------------
# 6L. VIEW ALL SIX GRAPHS INDIVIDUALLY
# ------------------------------------------------------------

bronzeville_bar
breast_bar
gyne_onc_bar

bronzeville_pie
breast_pie
gyne_onc_pie


# ------------------------------------------------------------
# 6M. FINAL CHECK OF GRAPH DATA AND LABELS
# ------------------------------------------------------------

race_ethnicity_plot |>
  select(
    analysis_group,
    race_ethnicity_label,
    n,
    pct_exact,
    pct_label
  ) |>
  arrange(
    analysis_group,
    desc(pct_exact)
  )

# ------------------------------------------------------------
# 7. COMBINED RACE/ETHNICITY COMPARISON GRAPH
# ------------------------------------------------------------

# Goal:
# Create one horizontal grouped bar chart comparing
# race/ethnicity percentages across:
#   1. Bronzeville
#   2. Breast
#   3. Gyne Onc
#
# All race/ethnicity categories are retained.
# Missing group/category combinations are explicitly set to 0.
# Percentages below 0.1% are displayed as <0.1%.
#
# This graph is designed for direct use in PowerPoint.


# ------------------------------------------------------------
# 7A. PREPARE DATA FOR COMPARISON GRAPH
# ------------------------------------------------------------

race_comparison <- race_ethnicity_plot |>
  
  # Keep the variables needed for the graph
  select(
    analysis_group,
    race_ethnicity_label,
    n,
    total_patients,
    pct_exact
  ) |>
  
  # Add explicit zero rows when a race/ethnicity category
  # does not occur in one of the three analysis groups
  complete(
    analysis_group,
    race_ethnicity_label,
    fill = list(
      n = 0,
      pct_exact = 0
    )
  ) |>
  
  # Restore the total sample size for each analysis group
  mutate(
    total_patients = case_when(
      analysis_group == "Bronzeville" ~ 490,
      analysis_group == "Breast" ~ 2000,
      analysis_group == "Gyne Onc" ~ 3655
    ),
    
    # Create percentage labels
    pct_label = case_when(
      pct_exact > 0 & pct_exact < 0.1 ~ "<0.1%",
      pct_exact == 0 ~ "0%",
      TRUE ~ paste0(sprintf("%.1f", pct_exact), "%")
    ),
    
    # Keep analysis groups in the desired order
    analysis_group = factor(
      analysis_group,
      levels = c(
        "Bronzeville",
        "Breast",
        "Gyne Onc"
      )
    )
  )


# ------------------------------------------------------------
# 7B. ORDER RACE/ETHNICITY CATEGORIES
# ------------------------------------------------------------

# Order categories based on their overall percentage across
# the three analysis groups so the largest categories appear
# toward the top of the graph.

race_order <- race_comparison |>
  group_by(race_ethnicity_label) |>
  summarise(
    overall_pct = sum(pct_exact),
    .groups = "drop"
  ) |>
  arrange(overall_pct) |>
  pull(race_ethnicity_label)

race_comparison <- race_comparison |>
  mutate(
    race_ethnicity_label = factor(
      race_ethnicity_label,
      levels = race_order
    )
  )


# ------------------------------------------------------------
# 7C. CREATE FINAL COMBINED GROUPED BAR CHART
# ------------------------------------------------------------

race_comparison_bar <- ggplot(
  race_comparison,
  aes(
    x = race_ethnicity_label,
    y = pct_exact,
    fill = analysis_group
  )
) +
  
  # Side-by-side bars for the three patient groups
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  # Percentage labels
  # True zero values are not labeled.
  geom_text(
    aes(
      label = if_else(
        pct_exact == 0,
        "",
        pct_label
      )
    ),
    position = position_dodge(width = 0.8),
    hjust = -0.15,
    size = 3.8
  ) +
  
  # Horizontal bars
  coord_flip() +
  
  # Use 0-80% instead of 0-100% to reduce empty space
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Titles and labels
  labs(
    title = "Race and Ethnicity by Patient Group",
    subtitle = paste0(
      "Bronzeville (N = 490) | ",
      "Breast (N = 2,000) | ",
      "Gyne Onc (N = 3,655)"
    ),
    x = NULL,
    y = "Percent of Patients",
    fill = "Patient Group"
  ) +
  
  # PowerPoint-friendly formatting
  theme_minimal(base_size = 13) +
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 10)
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      size = 11
    ),
    
    axis.title.x = element_text(
      size = 12
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 11
    ),
    
    panel.grid.major.y = element_blank()
  )


# ------------------------------------------------------------
# 7D. DISPLAY FINAL COMPARISON GRAPH
# ------------------------------------------------------------

race_comparison_bar
