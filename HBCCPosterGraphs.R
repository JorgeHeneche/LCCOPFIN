# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
df <- HBCCData
# Step 1: Clean the DOB and calculate Age
df <- df %>%
  mutate(DOB_cleaned = mdy(DOB),  # Assuming DOB is in mm/dd/yyyy or mm/dd/yy format
         Age = floor(interval(start = DOB_cleaned, end = today()) / years(1))) %>%
  filter(Age >= 0 & Age <= 100)  # Ensure realistic ages (0-100)

# Step 2: Create age groups
df <- df %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 25, 50, 75, 100), right = FALSE, 
                        labels = c("0-25", "25-50", "50-75", "75-100")))

# Step 3: Create the bar plot with purple color, count labels size 25, and x-axis label size 30
ggplot(df, aes(x = AgeGroup)) +
  geom_bar(fill = "purple", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 4) +  # Add count labels with size 25 (8 in ggplot units)
  labs(title = "Counts per Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20))  # Set x-axis label size to 30

# Create age groups
df <- df %>%
  mutate(Age = floor(interval(start = DOB, end = today()) / years(1)),
         AgeGroup = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, 70, 100), right = FALSE))

# Bar chart showing app download counts by age group
ggplot(df, aes(x = AgeGroup, fill = AppDownloaded)) +
  geom_bar(position = "dodge") +
  labs(title = "App Download Rate by Age Group", x = "Age Group", y = "Count", fill = "App Downloaded") +
  theme_minimal()


##########

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Step 1: Clean the DOB and calculate Age
df <- df %>%
  mutate(DOB_cleaned = mdy(DOB),  # Convert DOB to date format
         Age = floor(interval(start = DOB_cleaned, end = today()) / years(1))) %>%
  filter(Age >= 0 & Age <= 100)  # Ensure ages are within a reasonable range (0-100)

# Step 2: Create age groups
df <- df %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 25, 50, 75, 100), right = FALSE, 
                        labels = c("0-25", "25-50", "50-75", "75-100")))

# Step 3: Reclassify AppDownloaded variable
df <- df %>%
  mutate(AppDownloaded = ifelse(tolower(AppDownloaded) == "Yes", 
                                "Downloaded", 
                                "Educated/Not Interested in App"))

# Step 4: Create the bar chart with purple and gray colors
ggplot(df, aes(x = AgeGroup, fill = AppDownloaded)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_dodge(0.9), size = 6) +  # Add count labels
  labs(title = "App Download Status by Age Group", x = "Age Group", y = "Count", fill = "App Downloaded Status") +
  scale_fill_manual(values = c("Downloaded" = "purple", "Educated/Not Interested in App" = "gray")) +  # Set purple and gray colors
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20))  # Set x-axis label size to 30


########
# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Reclassify the AppDownloaded variable
df <- df %>%
  mutate(AppDownloaded = ifelse(tolower(AppDownloaded) == "yes", 
                                "Downloaded", 
                                "Educated/Not Interested in App"))

# Step 2: Create a simple bar chart of counts for AppDownloaded
ggplot(df, aes(x = AppDownloaded, fill = AppDownloaded)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 6) +  # Add count labels
  labs(title = "Counts of App Download Status", x = "App Download Status", y = "Count") +
  scale_fill_manual(values = c("Downloaded" = "purple", "Educated/Not Interested in App" = "gray")) +  # Set bar colors
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10))  # Set x-axis label size


##########

df <- HBCCData

# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Ensure DOB is read as text and then convert to date using base R
df <- df %>%
  mutate(DOB_cleaned = as.Date(DOB, format = "%m/%d/%Y"))  # Convert DOB to date with specified format

# Step 2: Flag invalid DOB entries (where DOB_cleaned is NA)
df <- df %>%
  mutate(is_invalid_dob = is.na(DOB_cleaned))  # Flag NA values in DOB_cleaned

# Step 3: Show the invalid DOB entries (optional for debugging)
invalid_dob_entries <- df %>%
  filter(is_invalid_dob) %>%
  select(DOB)  # List of invalid DOBs

# Display invalid DOB entries for review
print(invalid_dob_entries)

# Now proceed with valid entries for further analysis
df_valid <- df %>%
  filter(!is_invalid_dob)  # Keep only valid DOB entries

# Step 4: Calculate Age and Create custom age groups (0-24, 25-49, 50-74, 75+)
df_valid <- df_valid %>%
  mutate(Age = as.numeric(difftime(Sys.Date(), DOB_cleaned, units = "weeks")) / 52.25,  # Calculate age
         AgeGroup = cut(Age, 
                        breaks = c(0, 25, 50, 75, Inf),  # Age groups: 0-24, 25-49, 50-74, 75+
                        labels = c("0-24", "25-49", "50-74", "75+"),
                        right = FALSE))  # Ensure each age is placed in the correct group

# Step 5: Create the bar chart to show counts for each age group
ggplot(df_valid, aes(x = AgeGroup)) +
  geom_bar(fill = "lightblue", color = "black") +  # Create the bars
  labs(title = "Patient Age Distribution", x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15))  # Set x-axis label size





##################################
#09/14/24 new dataset in csv plus new code for bar
View(HBCC2_1)
df <- HBCC2_1

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
#data <- read.csv("your_file_path.csv")
data <- HBCC2_1
# Convert DOB to Date format
data$DOB <- as.Date(data$DOB, format = "%m/%d/%y")

# Create DOB_Group by classifying the DOB into 5 groups (1930-2010)
data <- data %>%
  mutate(DOB_Group = cut(DOB,
                         breaks = as.Date(c("1930-01-01", "1950-01-01", "1970-01-01", 
                                            "1990-01-01", "2010-01-01", "2020-01-01")),
                         labels = c("1930-1949", "1950-1969", "1970-1989", "1990-2009", "2010+"),
                         right = FALSE))

# Remove NA values from AppDownloaded and DOB_Group
data_clean <- data %>%
  filter(!is.na(AppDownloaded), !is.na(DOB_Group))

# Create the bar chart
ggplot(data_clean, aes(x = DOB_Group, fill = AppDownloaded)) +
  geom_bar(position = "stack") +
  labs(title = "App Downloaded Counts by DOB Group", 
       x = "DOB Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- HBCC2_1




# Convert DOB to Date format
data$DOB <- as.Date(data$DOB, format = "%m/%d/%Y")

# Make sure there are no leading/trailing spaces or inconsistencies in AppDownloaded
data$AppDownloaded <- trimws(data$AppDownloaded)

# Create DOB_Group by classifying the DOB into 5 groups (1930-2010)
data <- data %>%
  mutate(DOB_Group = cut(DOB,
                         breaks = as.Date(c("1930-01-01", "1950-01-01", "1970-01-01", 
                                            "1990-01-01", "2010-01-01", "2020-01-01")),
                         labels = c("1930-1949", "1950-1969", "1970-1989", "1990-2009", "2010+"),
                         right = FALSE))

# Remove NA values from AppDownloaded and DOB_Group
data_clean <- data %>%
  filter(!is.na(AppDownloaded), !is.na(DOB_Group))

# Create the bar chart, stack by AppDownloaded
ggplot(data_clean, aes(x = DOB_Group, fill = AppDownloaded)) +
  geom_bar(position = "stack", stat = "count") +  # Stack the bars
  labs(title = "App Downloaded Counts by DOB Group", 
       x = "DOB Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



