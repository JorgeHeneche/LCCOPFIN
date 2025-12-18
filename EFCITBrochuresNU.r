##EFCITGraphTool
##By: Jorge Heneche
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)

##EFCITtest <- read_csv("Downloads/EFCITtest.csv")
EFCITtest2 <- read_csv("Downloads/EFCITtest2.csv")
view(EFCITtest2)
# Load the dataset
data <- EFCITtest2

# Convert Week column to Date type and ensure proper ordering
data <- data %>%
  mutate(Week = as.character(Week)) %>%  # Ensure it's a character
  mutate(Week = mdy(Week)) %>%  # MM/DD/YY format parsing
  drop_na(Week) %>%  # Remove any NA values that result from conversion errors
  arrange(Week)  # Arrange in ascending order

# Debugging step: Print a preview of Week column to check formatting issues
print(unique(data$Week))

##### "Should I participate in a Clinical Trial?" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Should I participate in a clinical trial", "Deberia participar en un ensayo clinico?")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - Clinical Trials (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### "Finding Treatments Together AA" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Encontrando tratamientos juntos AA", "Finding Treatments together AA")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Encontrando tratamientos juntos AA" = "blue", "Finding Treatments together AA" = "green")) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - AA (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### "Finding Treatments Together Hispanics" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Encontrando tratamientos juntos Hispanos", "Finding Treatments together Hispanic")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Encontrando tratamientos juntos Hispanos" = "orange", "Finding Treatments together Hispanic" = "gold")) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - Hispanic (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### ALL TOGETHER

# Define colors for each brochure
target_colors <- c("Deberia participar en un ensayo clinico?" = "orange",
                   "Encontrando tratamientos juntos AA" = "brown",
                   "Encontrando tratamientos juntos Hispanos" = "red",
                   "Finding Treatments together AA" = "pink",
                   "Finding Treatments together Hispanic" = "skyblue",
                   "Should I participate in a clinical trial" = "blue")

# Sort before plotting
data <- data %>% arrange(Week)

# Create the plot
ggplot(data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = target_colors) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week by Type",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))


##################
#Bar Charts Version of Brochures
#################
##Updated EFCIT Graphs
install.packages("tidyverse")

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Load the data
##df <- read_csv("EFCITtest2.csv")
df <- EFCITtest2

# Convert 'Week' to Date format
df$Week <- mdy(df$Week)


################################################################################

# Extract relevant brochures
brochures <- c("Finding Treatments together Hispanic",
               "Encontrando tratamientos juntos Hispanos")
df_filtered <- df %>% filter(`Brochure Name/ID` %in% brochures)

# Extract month-year from 'Week' and ensure chronological order
df_filtered <- df_filtered %>%
  mutate(Month = format(Week, "%Y-%m")) %>%  # Format as "YYYY-MM" for chronological order
  filter(Month >= "2024-12")  # Ensure only December 2024 onwards

# Aggregate Stock Used by Month and Brochure
df_summary <- df_filtered %>%
  group_by(Month, `Brochure Name/ID`) %>%
  summarise(Stock_Used = sum(`Stock Used`, na.rm = TRUE))

# Convert Month to a factor with chronological order
df_summary$Month <- factor(df_summary$Month, levels = unique(df_summary$Month), ordered = TRUE)

# Plot using ggplot2 with chronological order
ggplot(df_summary, aes(x = Month, y = Stock_Used, fill = `Brochure Name/ID`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  geom_text(aes(label = Stock_Used), position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "white", size = 5, fontface = "bold") +  # Count labels inside bars
  scale_fill_manual(values = c("Finding Treatments together Hispanic" = "#4daf4a", 
                               "Encontrando tratamientos juntos Hispanos" = "#ff7f00")) +  # Custom colors
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%B %Y")) +  # Convert YYYY-MM to Month Year format
  labs(title = "Monthly Distribution of Brochures (From Dec 2024 Onwards)",
       x = "Month", y = "Stock Used", fill = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
############################################################################
# Extract relevant brochures
brochures <- c("Finding Treatments together AA",
               "Encontrando tratamientos juntos AA")
df_filtered <- df %>% filter(`Brochure Name/ID` %in% brochures)

# Extract month-year from 'Week' and ensure chronological order
df_filtered <- df_filtered %>%
  mutate(Month = format(Week, "%Y-%m")) %>%  # Format as "YYYY-MM" for chronological order
  filter(Month >= "2024-12")  # Ensure only December 2024 onwards

# Aggregate Stock Used by Month and Brochure
df_summary <- df_filtered %>%
  group_by(Month, `Brochure Name/ID`) %>%
  summarise(Stock_Used = sum(`Stock Used`, na.rm = TRUE))

# Convert Month to a factor with chronological order
df_summary$Month <- factor(df_summary$Month, levels = unique(df_summary$Month), ordered = TRUE)

# Plot using ggplot2 with chronological order
ggplot(df_summary, aes(x = Month, y = Stock_Used, fill = `Brochure Name/ID`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  geom_text(aes(label = Stock_Used), position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "white", size = 5, fontface = "bold") +  # Count labels inside bars
  scale_fill_manual(values = c("Finding Treatments together AA" = "#984ea3", 
                               "Encontrando tratamientos juntos AA" = "#ff7f00")) +  # Custom colors
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%B %Y")) +  # Convert YYYY-MM to Month Year format
  labs(title = "Monthly Distribution of Brochures (From Dec 2024 Onwards)",
       x = "Month", y = "Stock Used", fill = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
###############################################################################
# Extract relevant brochures
brochures <- c("Should I participate in a clinical trial",
               "Deberia participar en un ensayo clinico?")
df_filtered <- df %>% filter(`Brochure Name/ID` %in% brochures)

# Extract month-year from 'Week' and ensure chronological order
df_filtered <- df_filtered %>%
  mutate(Month = format(Week, "%Y-%m")) %>%  # Format as "YYYY-MM" for chronological order
  filter(Month >= "2024-12")  # Ensure only December 2024 onwards

# Aggregate Stock Used by Month and Brochure
df_summary <- df_filtered %>%
  group_by(Month, `Brochure Name/ID`) %>%
  summarise(Stock_Used = sum(`Stock Used`, na.rm = TRUE))

# Convert Month to a factor with chronological order
df_summary$Month <- factor(df_summary$Month, levels = unique(df_summary$Month), ordered = TRUE)

# Plot using ggplot2 with chronological order
ggplot(df_summary, aes(x = Month, y = Stock_Used, fill = `Brochure Name/ID`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  geom_text(aes(label = Stock_Used), position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "white", size = 5, fontface = "bold") +  # Count labels inside bars
  scale_fill_manual(values = c("Should I participate in a clinical trial" = "#377eb8", 
                               "Deberia participar en un ensayo clinico?" = "#e41a1c")) +  # Custom colors
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%B %Y")) +  # Convert YYYY-MM to Month Year format
  labs(title = "Monthly Distribution of Brochures (From Dec 2024 Onwards)",
       x = "Month", y = "Stock Used", fill = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
