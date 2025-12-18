## Code for Environmental Scan graphs
## By Jorge Heneche
# ---- Step 1: Install required packages (run only once) ----
install.packages("tidyverse")   # includes dplyr, ggplot2, tibble, tidyr, readr, etc.
install.packages("lubridate")
install.packages("writexl")
install.packages("gtsummary")
install.packages("broom")       # for model summaries
install.packages("gt")          # for rendering tables
install.packages("officer")     # for exporting to Word
install.packages("webshot2")    # for saving gt tables as images

# ---- Step 2: Load the libraries (every session) ----
library(tidyverse)  # loads dplyr, ggplot2, tibble, tidyr, etc.
library(lubridate)
library(writexl)
library(gtsummary)
library(broom)
library(gt)
library(officer)
library(webshot2)
library(ggplot2)
library(scales)


# Assuming your dataset is named df and is already loaded into your R environment
# Extract the disease team and name the new variable
df<- APilotEnvironmentalS_Update4302024_DATA_2025_07_09_1131
df <- df %>%
  mutate(disease_team = str_extract(physpatid, "^[A-Z]{2}")) %>%
  mutate(disease_team = recode(disease_team, 
                               "BR" = "Breast", 
                               "GI" = "Gastrointestinal", 
                               "GU" = "Genitourinary", 
                               "LU" = "Lung"))

# View the updated dataset
print(df)

#############################

# Gather the options columns into a long format
df_long <- df %>%
  pivot_longer(cols = question2___1 :question2___8, names_to = "option", values_to = "response") %>%
  filter(response == 1) # Keep only the rows where the option was selected


###### Bar with counts
# Create the bar chart with count labels
ggplot(df_long, aes(x = option, fill = disease_team)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Count of Options Selected by Disease Team",
       x = "Option",
       y = "Count",
       fill = "Disease Team") +
  scale_x_discrete(labels = c(
    question2___1 = "No trial is available",
    question2___2 = "Does not meet eligibility criteria",
    question2___3 = "Need more information to determine eligibility",
    question2___4 = "Visit for consultation only",
    question2___5 = "Patient referred to another provider",
    question2___6 = "Not enough time during visit",
    question2___7 = "Appointments are a burden",
    question2___8 = "Other"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

ggplot(df_long, aes(x = option, fill = disease_team)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Count of Options Selected by Disease Team",
       x = "Option",
       y = "Count",
       fill = "Disease Team") +
  scale_x_discrete(labels = c(
    question2___1 = "No trial is available",
    question2___2 = "Does not meet eligibility criteria",
    question2___3 = "Need more information to determine eligibility",
    question2___4 = "Visit for consultation only",
    question2___5 = "Patient referred to another provider",
    question2___6 = "Not enough time during visit",
    question2___7 = "Appointments are a burden",
    question2___8 = "Other"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))  # Increase size for x-axis text


#####
# Create the plot with color and counts

ggplot(df, aes(x = disease_team, fill = factor(question1))) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Count of Disease Teams", 
       x = "Disease Team", 
       y = "Count", 
       fill = "Question 1 (Yes/No)") +
  scale_fill_manual(values = c("0" = "blue", "1" = "purple"), 
                    labels = c("No", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12))  # Increase x-axis text size


###

# Filter the data for "No" selections and the "No trial is available" option
df_no_trial <- df_long %>%
  filter(question1 == 0, option == "question2___1")

# Plot the data
ggplot(df_no_trial, aes(x = disease_team, fill = disease_team)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Count of 'No Trial is Available' by Disease Team",
       x = "Disease Team",
       y = "Count",
       fill = "Disease Team") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#####

# Create a new column indicating if the option is "No Trial is available"
df_long <- df_long %>%
  mutate(trial_status = ifelse(option == "question2___1", "No Trial is Available", "Other"))

# Plot the data
ggplot(df_long, aes(x = disease_team, fill = trial_status)) +
  geom_bar(position = "fill") +  # 'fill' for proportional bars
  geom_text(stat = "count", aes(label = ..count..), position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of 'No Trial is Available' by Disease Team",
       x = "Disease Team",
       y = "Proportion",
       fill = "Option Selected") +
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
#proportion of "No Trial is Available" by Disease team
# Load your dataset (assumes it's already in the environment)
df <- APilotEnvironmentalS_Update4302024_DATA_2025_07_09_1131

# Extract disease team from physpatid
df <- df %>%
  mutate(disease_team = str_extract(physpatid, "^[A-Z]{2}")) %>%
  mutate(disease_team = recode(disease_team, 
                               "BR" = "Breast", 
                               "GI" = "Gastrointestinal", 
                               "GU" = "Genitourinary", 
                               "LU" = "Lung"))

# Gather checkbox responses into long format
df_long <- df %>%
  pivot_longer(
    cols = starts_with("question2___"),
    names_to = "option",
    values_to = "response"
  ) %>%
  filter(response == 1)

# Create trial_status and reverse the order for legend and stacking
df_long <- df_long %>%
  mutate(
    trial_status = ifelse(option == "question2___1", "No Trial is Available", "Other"),
    trial_status = factor(trial_status, levels = c("Other", "No Trial is Available"))  # THIS is the fix
  )

# Plot with flipped stacking order and legend match
ggplot(df_long, aes(x = disease_team, fill = trial_status)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count",
            aes(label = ..count..),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "black") +
  labs(title = "Proportion of 'No Trial is Available' by Disease Team",
       x = "Disease Team",
       y = "Proportion",
       fill = "Option Selected") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

## Counts per DT
# Count occurrences of each group in df$diseaseteam
team_counts <- df %>%
  count(disease_team)

# View result
print(team_counts)

###############
#new graphs for Cary and Deva 05/10/2025
# Assuming your dataset is named df and is already loaded into your R environment
# Extract the disease team and name the new variable
df <- df %>%
  mutate(disease_team = str_extract(physpatid, "^[A-Z]{2}")) %>%
  mutate(disease_team = recode(disease_team, 
                               "BR" = "Breast", 
                               "GI" = "Gastrointestinal", 
                               "GU" = "Genitourinary", 
                               "LU" = "Lung"))

# View the updated dataset
print(df)

# Gather the options columns into a long format
df_long <- df %>%
  pivot_longer(cols = question2___1:question2___8, names_to = "option", values_to = "response") %>%
  filter(response == 1)  # Keep only the rows where the option was selected

# Define the option labels
option_labels <- c(
  "question2___1" = "No trial is available",
  "question2___2" = "Does not meet eligibility criteria",
  "question2___3" = "Need more information to determine eligibility",
  "question2___4" = "Visit for consultation only",
  "question2___5" = "Patient referred to another provider",
  "question2___6" = "Not enough time during visit",
  "question2___7" = "Appointments are a burden",
  "question2___8" = "Other"
)

df_long$option <- factor(df_long$option, levels = names(option_labels), labels = option_labels)

# Function to create pie chart with percentages
create_pie_chart <- function(data, title) {
  data <- data %>%
    mutate(percentage = n / sum(n) * 100)
  
  ggplot(data, aes(x = "", y = n, fill = option)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
    labs(title = title, fill = "Option") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
}

# Breast Pie Chart
df_breast <- df_long %>% filter(disease_team == "Breast")
pie_data_breast <- df_breast %>% count(option)
create_pie_chart(pie_data_breast, "Options Distribution for Breast")

# Gastrointestinal Pie Chart
df_gi <- df_long %>% filter(disease_team == "Gastrointestinal")
pie_data_gi <- df_gi %>% count(option)
create_pie_chart(pie_data_gi, "Options Distribution for Gastrointestinal")

# Genitourinary Pie Chart
df_gu <- df_long %>% filter(disease_team == "Genitourinary")
pie_data_gu <- df_gu %>% count(option)
create_pie_chart(pie_data_gu, "Options Distribution for Genitourinary")

# Lung Pie Chart
df_lung <- df_long %>% filter(disease_team == "Lung")
pie_data_lung <- df_lung %>% count(option)
create_pie_chart(pie_data_lung, "Options Distribution for Lung")

##### Appt Summary
##################

#####Monthly Appointments per Disease Team
#####
# Load your dataset
df <- ESapptData

# Create full sequence of months from March 2024 to April 2025
full_months <- tibble(Month = seq(as.Date("2024-03-01"), as.Date("2025-04-01"), by = "1 month"))

# Function to generate a bar plot for one disease team
plot_team_appointments <- function(team_code, bar_color = "steelblue") {
  
  df_team <- df %>%
    mutate(Date = mdy(Date)) %>%
    filter(!is.na(Date)) %>%
    filter(DiseaseTeam == team_code) %>%
    mutate(Month = floor_date(Date, "month"))
  
  team_counts <- df_team %>%
    group_by(Month) %>%
    summarise(Appointments = n(), .groups = "drop")
  
  team_full <- full_months %>%
    left_join(team_counts, by = "Month") %>%
    mutate(Appointments = replace_na(Appointments, 0))
  
  ggplot(team_full, aes(x = Month, y = Appointments)) +
    geom_col(fill = bar_color) +
    geom_text(aes(label = Appointments), vjust = 1.5, color = "white", size = 4) +
    labs(title = paste("Monthly Appointments -", team_code, "Disease Team"),
         x = "Month",
         y = "Number of Appointments") +
    scale_x_date(date_labels = "%b %Y", breaks = full_months$Month) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1)  # Enlarged month labels
    )
}

# Generate and print plots for each team
plot_team_appointments("BR", bar_color = "steelblue")
plot_team_appointments("GI", bar_color = "seagreen")
plot_team_appointments("GU", bar_color = "purple")
plot_team_appointments("LU", bar_color = "darkorange")

####
####

####Monthly Appointments By Disease Team (grouped bar charts)

df<- ESapptData

# Step 1: Parse Date column (mm/dd/yy) and prepare data
df_clean <- df %>%
  mutate(Date = mdy(Date)) %>%                     # Parse mm/dd/yy format
  filter(!is.na(Date)) %>%                         # Drop rows where date parsing failed
  filter(DiseaseTeam %in% c("BR", "GI", "GU", "LU")) %>%  # Keep only 4 teams
  mutate(Month = floor_date(Date, "month"))        # Extract month for grouping

# Step 2: Create full set of all Month × DiseaseTeam combinations
full_months <- tibble(Month = seq(as.Date("2024-03-01"), as.Date("2025-04-01"), by = "1 month"))
teams <- tibble(DiseaseTeam = c("BR", "GI", "GU", "LU"))
full_grid <- crossing(full_months, teams)

# Step 3: Count appointments per DiseaseTeam per Month
team_month_counts <- df_clean %>%
  group_by(DiseaseTeam, Month) %>%
  summarise(Appointments = n(), .groups = "drop")

# Step 4: Merge with full grid to ensure months with zero appointments are included
plot_data <- full_grid %>%
  left_join(team_month_counts, by = c("DiseaseTeam", "Month")) %>%
  mutate(Appointments = replace_na(Appointments, 0))

# Step 5: Plot - Grouped bar chart with black, large labels above bars and larger X-axis labels
ggplot(plot_data, aes(x = Month, y = Appointments, fill = DiseaseTeam)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, color = "black", size = 6) +
  scale_x_date(date_labels = "%b %Y", breaks = full_months$Month) +
  labs(title = "Monthly Appointments by Disease Team (Mar 2024 – Apr 2025)",
       x = "Month",
       y = "Number of Appointments") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)  # <-- this increases month label size
  )

####
####
####Monthly Appointments by Physician Bar Graph
####
# Load your dataset
df <- ESapptData

# Step 1: Parse date and filter relevant teams
df_clean <- df %>%
  mutate(Date = mdy(Date)) %>%
  filter(!is.na(Date), DiseaseTeam %in% c("BR", "GI", "GU", "LU")) %>%
  mutate(Month = floor_date(Date, "month"))

# Create full list of months from Mar 2024 to Apr 2025
full_months <- tibble(Month = seq(as.Date("2024-03-01"), as.Date("2025-04-01"), by = "1 month"))

# ------------------------- BR TEAM -------------------------
df_br <- df_clean %>% filter(DiseaseTeam == "BR")
br_providers <- df_br %>% distinct(Provider)
br_counts <- df_br %>% count(Month, Provider, name = "Appointments")
br_plot_data <- crossing(full_months, br_providers) %>%
  left_join(br_counts, by = c("Month", "Provider")) %>%
  mutate(Appointments = replace_na(Appointments, 0)) %>%
  group_by(Provider) %>%
  filter(sum(Appointments) > 0) %>%
  ungroup()

plot_br <- ggplot(br_plot_data, aes(x = Month, y = Appointments, fill = Provider)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, size = 4, color = "black") +
  labs(title = "BR Team – Monthly Appointments by Physician",
       x = "Month", y = "Appointments") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# ------------------------- GI TEAM -------------------------
df_gi <- df_clean %>% filter(DiseaseTeam == "GI")
gi_providers <- df_gi %>% distinct(Provider)
gi_counts <- df_gi %>% count(Month, Provider, name = "Appointments")
gi_plot_data <- crossing(full_months, gi_providers) %>%
  left_join(gi_counts, by = c("Month", "Provider")) %>%
  mutate(Appointments = replace_na(Appointments, 0)) %>%
  group_by(Provider) %>%
  filter(sum(Appointments) > 0) %>%
  ungroup()

plot_gi <- ggplot(gi_plot_data, aes(x = Month, y = Appointments, fill = Provider)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, size = 4, color = "black") +
  labs(title = "GI Team – Monthly Appointments by Physician",
       x = "Month", y = "Appointments") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# ------------------------- GU TEAM -------------------------
df_gu <- df_clean %>% filter(DiseaseTeam == "GU")
gu_providers <- df_gu %>% distinct(Provider)
gu_counts <- df_gu %>% count(Month, Provider, name = "Appointments")
gu_plot_data <- crossing(full_months, gu_providers) %>%
  left_join(gu_counts, by = c("Month", "Provider")) %>%
  mutate(Appointments = replace_na(Appointments, 0)) %>%
  group_by(Provider) %>%
  filter(sum(Appointments) > 0) %>%
  ungroup()

plot_gu <- ggplot(gu_plot_data, aes(x = Month, y = Appointments, fill = Provider)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, size = 4, color = "black") +
  labs(title = "GU Team – Monthly Appointments by Physician",
       x = "Month", y = "Appointments") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# ------------------------- LU TEAM -------------------------
df_lu <- df_clean %>% filter(DiseaseTeam == "LU")
lu_providers <- df_lu %>% distinct(Provider)
lu_counts <- df_lu %>% count(Month, Provider, name = "Appointments")
lu_plot_data <- crossing(full_months, lu_providers) %>%
  left_join(lu_counts, by = c("Month", "Provider")) %>%
  mutate(Appointments = replace_na(Appointments, 0)) %>%
  group_by(Provider) %>%
  filter(sum(Appointments) > 0) %>%
  ungroup()

plot_lu <- ggplot(lu_plot_data, aes(x = Month, y = Appointments, fill = Provider)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, size = 4, color = "black") +
  labs(title = "LU Team – Monthly Appointments by Physician",
       x = "Month", y = "Appointments") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# ------------------------- View Plots -------------------------
plot_br
plot_gi
plot_gu
plot_lu

#### Appt all Disease Teams

# Load dataset
df <- ESapptData

# Step 1: Parse Date column and prepare data
df_clean <- df %>%
  mutate(Date = mdy(Date)) %>%                      # Parse mm/dd/yy format
  filter(!is.na(Date)) %>%                          # Remove bad/missing dates
  filter(DiseaseTeam %in% c("BR", "GI", "GU", "LU")) %>%  # Only keep these 4 teams
  mutate(Month = floor_date(Date, "month"))         # Extract month

# Step 2: Create full grid of all Month × DiseaseTeam combinations
full_months <- tibble(Month = seq(as.Date("2024-03-01"), as.Date("2025-04-01"), by = "1 month"))
teams <- tibble(DiseaseTeam = c("BR", "GI", "GU", "LU"))
full_grid <- crossing(full_months, teams)

# Step 3: Count appointments per Month per DiseaseTeam
team_month_counts <- df_clean %>%
  group_by(DiseaseTeam, Month) %>%
  summarise(Appointments = n(), .groups = "drop")

# Step 4: Join with full grid to ensure zero-count months are included
plot_data <- full_grid %>%
  left_join(team_month_counts, by = c("DiseaseTeam", "Month")) %>%
  mutate(Appointments = replace_na(Appointments, 0))

# Step 5A: Grouped Bar Chart
ggplot(plot_data, aes(x = Month, y = Appointments, fill = DiseaseTeam)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  geom_text(aes(label = Appointments),
            position = position_dodge(width = 25),
            vjust = -0.3, color = "black", size = 6) +
  scale_x_date(date_labels = "%b %Y", breaks = full_months$Month) +
  labs(title = "Monthly Appointments by Disease Team (Bar Chart)",
       x = "Month",
       y = "Number of Appointments",
       fill = "Disease Team") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# Step 5B: Line Chart Version
ggplot(plot_data, aes(x = Month, y = Appointments, color = DiseaseTeam, group = DiseaseTeam)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_date(date_labels = "%b %Y", breaks = full_months$Month) +
  labs(title = "Monthly Appointments by Disease Team (Line Chart)",
       x = "Month",
       y = "Number of Appointments",
       color = "Disease Team") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    legend.position = "right"
  )

#### Monthly appointments by physicians Line graph
####
# Load your dataset
df <- ESapptData

# Step 1: Parse date and filter relevant teams
df_clean <- df %>%
  mutate(Date = mdy(Date)) %>%
  filter(!is.na(Date), DiseaseTeam %in% c("BR", "GI", "GU", "LU")) %>%
  mutate(Month = floor_date(Date, "month"))

# Step 2: Create full list of months from Mar 2024 to Apr 2025
full_months <- tibble(Month = seq(as.Date("2024-03-01"), as.Date("2025-04-01"), by = "1 month"))

### Function to prepare and plot line chart by DiseaseTeam
plot_team_line <- function(team_code, title_prefix) {
  df_team <- df_clean %>% filter(DiseaseTeam == team_code)
  providers <- df_team %>% distinct(Provider)
  counts <- df_team %>% count(Month, Provider, name = "Appointments")
  
  plot_data <- crossing(full_months, providers) %>%
    left_join(counts, by = c("Month", "Provider")) %>%
    mutate(Appointments = replace_na(Appointments, 0)) %>%
    group_by(Provider) %>%
    filter(sum(Appointments) > 0) %>%
    ungroup()
  
  ggplot(plot_data, aes(x = Month, y = Appointments, color = Provider, group = Provider)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = paste(title_prefix, "Team – Monthly Appointments by Physician"),
         x = "Month", y = "Appointments", color = "Provider") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
}

# Generate line plots for each disease team
plot_br <- plot_team_line("BR", "BR")
plot_gi <- plot_team_line("GI", "GI")
plot_gu <- plot_team_line("GU", "GU")
plot_lu <- plot_team_line("LU", "LU")

# View plots
plot_br
plot_gi
plot_gu
plot_lu


####Counts per DT
####

df <- ESapptData

# Parse date and group by DiseaseTeam
df %>%
  mutate(Date = mdy(Date)) %>%
  filter(!is.na(Date), DiseaseTeam %in% c("BR", "GI", "GU", "LU")) %>%
  group_by(DiseaseTeam) %>%
  summarise(TotalAppointments = n(), .groups = "drop")

############################################################################
########################### Analysis for Masha Using the cleaned_dataset##############################
#######################(only registered appts)#############################

# Assume you've already read in your data:
df1 <- APilotEnvironmentalS_Update4302024_DATA_2025_07_09_1131
df2 <- patients_report_20250626

# Make sure the ID columns are character type
df1 <- df1 %>%
  mutate(physpatid = as.character(physpatid))

df2 <- df2 %>%
  rename(CaseNumber = `CASE NUM`) %>%  # <- This fixes the column name for joining
  mutate(CaseNumber = as.character(CaseNumber))

# Perform the join using matching keys
merged_df <- left_join(df1, df2, by = c("physpatid" = "CaseNumber"))

# Step 3: Remove PHI column (PATIENT NAME)
cleaned_df <- merged_df %>%
  select(-`PATIENT NAME`)

# Optional: View result
glimpse(cleaned_df)

# Create disease_team variable
cleaned_df <- cleaned_df %>%
  mutate(disease_team = str_extract(physpatid, "^[A-Z]{2}")) %>%
  mutate(disease_team = recode(disease_team,
                               "BR" = "Breast", 
                               "GI" = "Gastrointestinal", 
                               "GU" = "Genitourinary", 
                               "LU" = "Lung"))

# View structure to confirm
glimpse(cleaned_df)

# Step 6: Create new variable indicating if patient was offered a trial
cleaned_df <- cleaned_df %>%
  mutate(offered_trial = ifelse(question1 == 1, "Yes", "No"))

# Step 7: Optional – View structure and confirm new columns
glimpse(cleaned_df)
table(cleaned_df$offered_trial)
table(cleaned_df$disease_team)


# Step 4: Create birth_decade variable based on PATIENT BIRTH DATE
cleaned_df <- cleaned_df %>%
  mutate(
    birth_year = year(`PATIENT BIRTH DATE`),
    birth_decade = paste0(floor(birth_year / 10) * 10, "s")
  )

# Optional: Check output
glimpse(cleaned_df)

# Save as CSV
#write_csv(cleaned_df, "cleaned_dataset.csv")

# Save as Excel (make sure writexl is installed)
# install.packages("writexl")  # Run once if not installed

#write_xlsx(cleaned_df, "cleaned_dataset.xlsx")

######################################
# Descriptive summary by a grouping variable
cleaned_df %>%
  select(birth_decade, disease_team, offered_trial) %>%
  tbl_summary(by = offered_trial, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_p() %>%
  bold_labels()

cleaned_df %>%
  select(birth_decade, disease_team, offered_trial) %>%
  tbl_summary(by = offered_trial) %>%
  add_p()

##################################################################
################### GT SUMMARY TABLES#############################
##################################################################
# Assuming `cleaned_df` is your working dataset
# Each table compares 'offered_trial' (Yes/No) across different variables

# Table 1: Disease Team
tbl_disease_team <- cleaned_df %>%
  select(offered_trial, disease_team) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 2: Patient Race
tbl_race <- cleaned_df %>%
  select(offered_trial, `PATIENT RACE`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 3: Patient Gender
tbl_gender <- cleaned_df %>%
  select(offered_trial, `PATIENT GENDER`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 4: Birth Decade
tbl_decade <- cleaned_df %>%
  select(offered_trial, birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 5: Patient Ethnicity
tbl_ethnicity <- cleaned_df %>%
  select(offered_trial, `PATIENT ETHNICITY`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# View all tables in R
tbl_disease_team
tbl_race
tbl_gender
tbl_decade
tbl_ethnicity

# Combine and export tables to Word
tbl_combined <- tbl_stack(list(
  tbl_disease_team,
  tbl_race,
  tbl_gender,
  tbl_decade,
  tbl_ethnicity
))

tbl_combined %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "trial_offer_summary_tables.docx")

####################################################################
####################### Logistic Regression for Masha###############
####################################################################
# Make sure offered_trial is a factor with levels "No", "Yes"
cleaned_df <- cleaned_df %>%
  mutate(offered_trial = factor(offered_trial, levels = c("No", "Yes")))

# Fit the logistic regression model
logistic_model <- glm(offered_trial ~ disease_team + `PATIENT RACE` + `PATIENT GENDER` +
                        `PATIENT ETHNICITY` + birth_decade,
                      data = cleaned_df,
                      family = binomial)

# Summarize the model using gtsummary
logistic_table <- tbl_regression(
  logistic_model,
  exponentiate = TRUE  # This gives Odds Ratios instead of log-odds
) %>%
  bold_labels() %>%
  bold_p(t = 0.05)  %>% # Bold statistically significant p-values
  add_glance_source_note()


# View the summary table
logistic_table

######################Masha's input########################

cleaned_df %>%
  select(offered_trial, disease_team, `PATIENT RACE` , `PATIENT GENDER` ,
           `PATIENT ETHNICITY` , birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

####################################################################################
#This new code is to run the dataset that includes all screened appointments to be
#offered participation per Masha's request
####################################################################################

######################################
# Descriptive summary by a grouping variable
cleaned_datasetV2 %>%
  select(birth_decade, disease_team, offered_trial) %>%
  tbl_summary(by = offered_trial, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_p() %>%
  bold_labels()

cleaned_datasetV2 %>%
  select(birth_decade, disease_team, offered_trial) %>%
  tbl_summary(by = offered_trial) %>%
  add_p()

##################################################################
################### GT SUMMARY TABLES#############################
##################################################################
# Each table compares 'offered_trial' (Yes/No) across different variables

# Table 1: Disease Team
tbl_disease_team <- cleaned_datasetV2 %>%
  select(offered_trial, disease_team) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 2: Patient Race
tbl_race <- cleaned_datasetV2 %>%
  select(offered_trial, `PATIENT RACE`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 3: Patient Gender
tbl_gender <- cleaned_datasetV2 %>%
  select(offered_trial, `PATIENT GENDER`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 4: Birth Decade
tbl_decade <- cleaned_datasetV2 %>%
  select(offered_trial, birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# Table 5: Patient Ethnicity
tbl_ethnicity <- cleaned_datasetV2 %>%
  select(offered_trial, `PATIENT ETHNICITY`) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()

# View all tables in R
tbl_disease_team
tbl_race
tbl_gender
tbl_decade
tbl_ethnicity

# Combine and export tables to Word
tbl_combined <- tbl_stack(list(
  tbl_disease_team,
  tbl_race,
  tbl_gender,
  tbl_decade,
  tbl_ethnicity
))

tbl_combined %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "trial_offer_summary_tables.docx")

####################################################################
####################### Logistic Regression for Masha###############
####################################################################


# Make sure offered_trial is a factor with levels "No", "Yes"
cleaned_datasetV2 <- cleaned_datasetV2 %>%
  mutate(offered_trial = factor(offered_trial, levels = c("No", "Yes")))

# Fit the logistic regression model
logistic_model <- glm(offered_trial ~ disease_team + `PATIENT RACE` + `PATIENT GENDER` +
                        `PATIENT ETHNICITY` + birth_decade,
                      data = cleaned_datasetV2,
                      family = binomial)

# Summarize the model using gtsummary
logistic_table <- tbl_regression(
  logistic_model,
  exponentiate = TRUE  # This gives Odds Ratios instead of log-odds
) %>%
  bold_labels() %>%
  bold_p(t = 0.05)  %>% # Bold statistically significant p-values
  add_glance_source_note()


# View the summary table
logistic_table

######################Masha's input########################

cleaned_datasetV2 %>%
  select(offered_trial, disease_team, `PATIENT RACE` , `PATIENT GENDER` ,
         `PATIENT ETHNICITY` , birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()


#figure out how to print both table completely

cleaned_datasetV2 %>%
  select(offered_trial, disease_team, `PATIENT RACE` , `PATIENT GENDER` ,
         `PATIENT ETHNICITY` , birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()


cleaned_df %>%
  select(offered_trial, disease_team, `PATIENT RACE` , `PATIENT GENDER` ,
         `PATIENT ETHNICITY` , birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_labels()
#####################################################################################
# Build tables
tbl_v2 <- cleaned_datasetV2 %>%
  select(offered_trial, disease_team, `PATIENT RACE`, `PATIENT GENDER`,
         `PATIENT ETHNICITY`, birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_p(t = 0.05) %>%    
  bold_labels()

tbl_df <- cleaned_df %>%
  select(offered_trial, disease_team, `PATIENT RACE`, `PATIENT GENDER`,
         `PATIENT ETHNICITY`, birth_decade) %>%
  tbl_summary(by = offered_trial, missing = "no", percent = "row") %>%
  add_p() %>%
  bold_p(t = 0.05) %>%    
  bold_labels()

# Convert to gt at fixed table width
to_gt_600 <- function(tbl) {
  tbl %>%
    as_gt() %>%
    tab_options(
      table.width = px(600),
      data_row.padding = px(2),
      table.font.size = 10
    )
}

gt_v2 <- to_gt_600(tbl_v2)
gt_df <- to_gt_600(tbl_df)

# Function to compute viewport height
vh_needed <- function(tbl, base = 250, per_row = 30) {
  n_rows <- tryCatch(nrow(tbl$table_body), error = function(e) 40)
  max(1200, base + n_rows * per_row)  # taller = more room
}

vh_v2 <- vh_needed(tbl_v2)
vh_df <- vh_needed(tbl_df)

# --- Save high-resolution PNGs ---
# render at 3x density (1800px viewport width) then shrink visually to 600px
gtsave(gt_v2, "table_cleaned_datasetV2.png", vwidth = 1800, vheight = vh_v2 * 3, zoom = 3)
gtsave(gt_df, "table_cleaned_df.png",         vwidth = 1800, vheight = vh_df * 3, zoom = 3)
