install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
install.packages("ggridges")
install.packages("grid")
install.packages("broom")
install.packages("plotly")

library(dplyr)
library(ggplot2)
library(scales)
library(ggridges)
library(grid)
library(broom)
library(tidyr)
library(stats)
library(plotly)

## DATA IMPORT

# Import data from csv file
data_set = read.csv("C:\\Users\\YONG FATT\\Documents\\Work\\PFDA\\House_Rent_Dataset.csv",
                    header=TRUE)

View(data_set)      #Display whole dataset


## DATA CLEANING

# Rename Column Name
names(data_set)=c("POSTED_DATE","B_H_K","RENT_AMOUNT","SIZE","FLOOR","AREA_TYPE","AREA_LOCALITY",
                  "CITY","FURNISHING_STATUS","TENANT_PREFERRED","BATHROOM","CONTACT_PERSON")

# Remove leading and trailing white space from categorical column 
data_set$FLOOR <- trimws(data_set$FLOOR)
data_set$AREA_TYPE <- trimws(data_set$AREA_TYPE)
data_set$AREA_LOCALITY <- trimws(data_set$AREA_LOCALITY)
data_set$CITY <- trimws(data_set$CITY) 
data_set$FURNISHING_STATUS <- trimws(data_set$FURNISHING_STATUS)
data_set$TENANT_PREFERRED <- trimws(data_set$TENANT_PREFERRED)
data_set$CONTACT_PERSON <- trimws(data_set$CONTACT_PERSON)

# Delete row with missing values
data_set <- na.omit(data_set) 

# Exclude duplicates data
data_set <- unique(data_set)

# Replace Zero value to NULL 
data_set <- data_set %>% mutate_all(~ replace(., . == 0, NA))

# Remove special character in numeric value
# Remove comma in RENT_AMOUNT and SIZE value
data_set$RENT_AMOUNT <- as.numeric(gsub(",", "", data_set$RENT_AMOUNT))
data_set$SIZE <- as.numeric(gsub(",", "", data_set$SIZE))
# Remove decimal and convert to integer value
data_set$B_H_K <- as.integer(round(data_set$B_H_K))
data_set$BATHROOM <- as.integer(round(data_set$BATHROOM))

# Replace misspelling value to correct value
# AREA_TYPE column
# Correct values
correct_values <- c("Built Area", "Super Area", "Carpet Area")
# Function to find the most similar correct value
find_most_similar <- function(value, correct_values) {
  distances <- sapply(correct_values, function(correct_value) {
    adist(tolower(value), tolower(correct_value))
  })
  closest_index <- which.min(distances)
  return(correct_values[closest_index])
}
# Mutate the dataset to correct spelling in the CONTACT_PERSON column
data_set <- data_set %>%
  mutate(AREA_TYPE = ifelse(tolower(AREA_TYPE) %in% tolower(correct_values),
                            AREA_TYPE,
                            find_most_similar(tolower(AREA_TYPE), tolower(correct_values))))

# CITY column
correct_values <- c("Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai")
find_most_similar <- function(value, correct_values) {
  distances <- sapply(correct_values, function(correct_value) {
    adist(tolower(value), tolower(correct_value))
  })
  closest_index <- which.min(distances)
  return(correct_values[closest_index])
}
data_set <- data_set %>%
  mutate(CITY = ifelse(tolower(CITY) %in% tolower(correct_values),
                       CITY,
                       find_most_similar(tolower(CITY), tolower(correct_values))))

# FURNISHING_STATUS column
correct_values <- c("Furnished", "Semi-Furnished", "Unfurnished")
find_most_similar <- function(value, correct_values) {
  distances <- sapply(correct_values, function(correct_value) {
    adist(tolower(value), tolower(correct_value))
  })
  closest_index <- which.min(distances)
  return(correct_values[closest_index])
}
data_set <- data_set %>%
  mutate(FURNISHING_STATUS = ifelse(tolower(FURNISHING_STATUS) %in% tolower(correct_values),
                                    FURNISHING_STATUS,
                                    find_most_similar(tolower(FURNISHING_STATUS), tolower(correct_values))))

# TENANT_PREFERRED column
correct_values <- c("Bachelors", "Bachelors/Family", "Family")
find_most_similar <- function(value, correct_values) {
  distances <- sapply(correct_values, function(correct_value) {
    adist(tolower(value), tolower(correct_value))
  })
  closest_index <- which.min(distances)
  return(correct_values[closest_index])
}
data_set <- data_set %>%
  mutate(TENANT_PREFERRED = ifelse(tolower(TENANT_PREFERRED) %in% tolower(correct_values),
                                   TENANT_PREFERRED,
                                   find_most_similar(tolower(TENANT_PREFERRED), tolower(correct_values))))

# CONTACT_PERSON column
correct_values <- c("Contact Agent", "Contact Owner", "Contact Builder")
find_most_similar <- function(value, correct_values) {
  distances <- sapply(correct_values, function(correct_value) {
    adist(tolower(value), tolower(correct_value))
  })
  closest_index <- which.min(distances)
  return(correct_values[closest_index])
}
data_set <- data_set %>%
  mutate(CONTACT_PERSON = ifelse(tolower(CONTACT_PERSON) %in% tolower(correct_values),
                                 CONTACT_PERSON,
                                 find_most_similar(tolower(CONTACT_PERSON), tolower(correct_values))))

# Function to replace anomalies "Floor" values with NA
replace_incorrect_floor <- function(x) {
  if (grepl("\\d+ out of \\d+", x)) {   # Check if the format is "X out of Y"
    floor_values <- strsplit(x, " out of ")[[1]]
    floor_values <- as.numeric(floor_values)
    if (floor_values[1] > floor_values[2]) {
      cat("Incorrect 'FLOOR' value:", x, "\n")
      return(NA)  # Replace with NA if the condition is met
    }
  }
  return(x)  # Keep the original value for other cases
}
# Apply the function to the "Floor" column
data_set$FLOOR <- sapply(data_set$FLOOR, replace_incorrect_floor)

# Replace Outlier for numeric column by CITY
# Function to replace outliers with 5 and 95 percentiles
replace_outliers <- function(x, lower_percentile, upper_percentile) {
  lower_bound <- quantile(x, lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(x, upper_percentile, na.rm = TRUE)
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Group by CITY and replace outlier in each column using 5 and 95 percentiles
data_set <- data_set %>%
  group_by(CITY) %>%
  mutate(
    RENT_AMOUNT = replace_outliers(RENT_AMOUNT, 0.05, 0.95),  # Replace outliers using 5th and 95th percentiles
    SIZE = round(replace_outliers(SIZE, 0.05, 0.95)),
    B_H_K = replace_outliers(B_H_K, 0.05, 0.95),
    BATHROOM = replace_outliers(BATHROOM, 0.05, 0.95)
  )

View(data_set)


## DATA PRE-PROCESSING

# Convert a date column to proper date format
data_set$POSTED_DATE <- as.Date(data_set$POSTED_DATE, format = "%m/%d/%Y")

# Function to convert floor descriptions to numeric values
convert_floor_to_numeric <- function(x) {
  if (grepl("Ground out of \\d+", x)) {
    return(0)
  } else if (grepl("Ground", x)) {
    return(0)
  } else if (grepl("Upper Basement out of \\d+", x)) {
    return(-1)
  } else if (grepl("Lower Basement out of \\d+", x)) {
    return(-2)
  } else if (grepl("\\d+ out of \\d+", x)) {
    floor_values <- strsplit(x, " out of ")[[1]]
    return(as.numeric(floor_values[1]))
  }
  return(x)# Return NA for other cases (e.g., invalid or unknown descriptions)
}
# Apply the function to the "Floor" column
data_set$FLOOR <- sapply(data_set$FLOOR, convert_floor_to_numeric)

# Convert data type to numeric for numeric column
data_set$B_H_K <- as.numeric(data_set$B_H_K)
data_set$RENT_AMOUNT <- as.numeric(data_set$RENT_AMOUNT)
data_set$SIZE <- as.numeric(data_set$SIZE)
data_set$BATHROOM <- as.numeric(data_set$BATHROOM)
data_set$FLOOR <- as.numeric(data_set$FLOOR)


## DATA EXPLORATION

# View only title\heading
names(data_set) 

# To know number of records and attributes in dataset
dim(data_set)

# Print first 10 lines
head(data_set,10)   

# Print last 10 lines
tail(data_set,10)

# Class of dataset
class(data_set) 

# Total no.of row
nrow(data_set)

# Total no.of column
ncol(data_set)

# To know data type of each column
class(data_set$POSTED_DATE)
class(data_set$B_H_K)
class(data_set$RENT_AMOUNT)
class(data_set$SIZE)
class(data_set$FLOOR)
class(data_set$AREA_TYPE)
class(data_set$AREA_LOCALITY)
class(data_set$CITY)
class(data_set$FURNISHING_STATUS)
class(data_set$TENANT_PREFERRED)
class(data_set$BATHROOM)
class(data_set$CONTACT_PERSON)

# Access column data
data_set$POSTED_DATE
data_set$B_H_K
data_set$RENT_AMOUNT
data_set$SIZE
data_set$FLOOR
data_set$AREA_TYPE
data_set$AREA_LOCALITY
data_set$CITY
data_set$FURNISHING_STATUS
data_set$TENANT_PREFERRED
data_set$BATHROOM
data_set$CONTACT_PERSON

# Summary of descriptive statistics of numeric column
summary(data_set)

# List out the categories of column
factor(data_set$POSTED_DATE)
factor(data_set$B_H_K) 
factor(data_set$RENT_AMOUNT)
factor(data_set$SIZE)
factor(data_set$FLOOR) 
factor(data_set$AREA_TYPE) 
factor(data_set$AREA_LOCALITY) 
factor(data_set$CITY) 
factor(data_set$FURNISHING_STATUS) 
factor(data_set$TENANT_PREFERRED) 
factor(data_set$BATHROOM) 
factor(data_set$CONTACT_PERSON) 

# Number of levels(categories)
nlevels(factor(data_set$POSTED_DATE))  
nlevels(factor(data_set$B_H_K))  
nlevels(factor(data_set$RENT_AMOUNT))  
nlevels(factor(data_set$SIZE))  
nlevels(factor(data_set$FLOOR))  
nlevels(factor(data_set$AREA_TYPE))  
nlevels(factor(data_set$AREA_LOCALITY))  
nlevels(factor(data_set$CITY))  
nlevels(factor(data_set$FURNISHING_STATUS)) 
nlevels(factor(data_set$TENANT_PREFERRED))  
nlevels(factor(data_set$BATHROOM))  
nlevels(factor(data_set$CONTACT_PERSON))  

# Access row data
# Access 500th row and 7th column
data_set[500,7]

# Access whole 500th row
data_set[500, ]

# Access row from 500th until 510th 
data_set[500:510, ]

# Access row by writing condition with categorical data
# Get all data "Unfurnished"
data_set[data_set$FURNISHING_STATUS == "Unfurnished", ]

# Writing condition with continuous data
# Get all data where rent amount less than 10000
data_set[data_set$RENT_AMOUNT < 10000, ]

## Question and Analysis

# Hypothesis : Smaller sized houses with less furnishing leads to lower rental prices. 

# Objective 1: To study the impact of lower size and less furnishing on rental price

### Question 1 - What is the differences of rent prices on different house size and furnishing status?
### Analysis 1.1 – Find the average rent prices on each size categories. 

# Determine small size and medium size and large size
# three size category, SIZE < q1 size as small size, 
# q1 size < SIZE < q3 size as medium size , SIZE > q3 size as large size

# Calculate Q1 as small size, Q3 as large size
q1_size = quantile(data_set$SIZE, 0.25)
q3_size = quantile(data_set$SIZE, 0.75)

# Calculate the mean rental prices based on each size category
size_price_stats <- data_set %>%
  mutate(Size_Category = cut(SIZE, breaks = c(-Inf, q1_size, q3_size, Inf), 
                             labels = c("Small Size", "Medium Size", "Large size"), include.lowest = TRUE)) %>%
  group_by(Size_Category) %>%
  summarize(
    Mean_Rent = mean(RENT_AMOUNT),
  )

# Print the results
View(size_price_stats)

# Create a bar plot for mean rent by each size category
ggplot(size_price_stats, aes(x = Size_Category, y = Mean_Rent, label = round(Mean_Rent, 2))) +
  geom_bar(stat = "identity", fill = c("pink", "purple", "maroon")) +
  labs(x = "Size Category", y = "Mean Rent Amount") +
  ggtitle("Mean Rent Amount by Size Category") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) 

### Analysis 1.2 – Find the average rent prices on each size categories and furnishing status.

# Calculate the mean rental prices based on each size category and furnishing status
size_price_furnishing_stats <- data_set %>%
  mutate(Size_Category = cut(SIZE, breaks = c(-Inf, q1_size, q3_size, Inf), 
                             labels = c("Small Size", "Medium Size", "Large size"), include.lowest = TRUE)) %>%
  group_by(Size_Category, FURNISHING_STATUS) %>%
  summarize(
    Mean_Rent = mean(RENT_AMOUNT),
  )

# Print the results
View(size_price_furnishing_stats)

# Create a bar chart for mean rent by each size category and furnishing status
ggplot(size_price_furnishing_stats, aes(x = Size_Category, y = Mean_Rent, fill = FURNISHING_STATUS, label = round(Mean_Rent, 2))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Size Category", y = "MEAN RENT AMOUNT") +
  ggtitle("Mean Rent Amount by Size Category and Furnishing Status") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Furnished" = "lightgreen", "Semi-Furnished" = "lightyellow", "Unfurnished" = "lightblue"))

### Question 2 – Which city have the greatest number of unfurnished houses with lower size and less furnishing? 
### Analysis 2.1 – Find the frequency of unfurnished houses below Q1 rent and size based on each city. 

## To find the frequency of houses that below Q1 rent and size based on each city
# Calculate Q1 as small rent
q1_rent = quantile(data_set$RENT_AMOUNT, 0.25)

# Create a subset of data for unfurnished houses, rent below Q1, and size below Q1
subset_data <- data_set %>%
  filter(FURNISHING_STATUS == "Unfurnished", RENT_AMOUNT < q1_rent, SIZE < q1_size)

# Calculate the frequency of each city in the subset
city_frequency <- subset_data %>%
  group_by(CITY) %>%
  summarize(Frequency = n())

# Sort the cities by frequency in descending order
city_frequency <- city_frequency %>%
  arrange(Frequency)

View(city_frequency)

# Create a bar plot for city frequency
ggplot(city_frequency, aes(x = reorder(CITY, Frequency), y = Frequency, fill = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "City", y = "Frequency", title = "Frequency of Unfurnished Houses Below Q1 Rent and Size by City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  scale_fill_gradient(low = "yellow", high = "red")

### Question 3 - How does the rent amount relate to the area size for different cities? 
### Analysis 3.1 - Find the relationship between rent and area size based on city.

# Find the relationship between rent and area size based on city
# Scatter plot of Rent Amount vs Size by City
ggplot(data_set, aes(x = SIZE, y = RENT_AMOUNT, color = CITY)) +
  geom_point(aes(shape=factor(CITY), colour =factor(CITY))) +
  labs(x = "AREA SIZE", y = "RENT AMOUNT") +
  ggtitle("Rent Amount vs Area Size by City") +
  facet_wrap(~ CITY, ncol = 2) +
  geom_smooth(method= lm)+
  scale_y_continuous(labels = scales::comma)  # This line formats y-axis labels with commas

### Question 4 - How do the rent distributions vary across different cities? 
### Analysis 4.1 - Find the distribution of rent amounts across different cities.

### Additional Feature: Density Plot
# Find the distribution of rent amounts across different cities
# Create a density plot for rent by city
ggplot(data_set, aes(x = RENT_AMOUNT, fill = CITY)) +
  geom_density(alpha = 0.5) +
  labs(x = "Rent Amount", y = "Density") +
  ggtitle("Density Plot of Rent Amount by City") +
  scale_fill_discrete(name = "City") +
  facet_wrap(~ CITY) +
  scale_x_continuous(labels = scales::comma) +  # This line formats x-axis labels with commas
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

### Question 5 - How have the mean rent amounts changed over different time periods (monthly) for each city? 
### Analysis 5.1 – Find mean rent prices of every month based on cities.

# Investigate how rental prices change over different time periods(Monthly) based on cities
# Aggregate data to monthly frequency and calculate mean rent for each city and month
monthly_mean_rent <- data_set %>%
  group_by(CITY, Year_Month = format(POSTED_DATE, "%Y-%m")) %>%
  summarize(Mean_Rent = mean(RENT_AMOUNT))

# Print the resulting data
View(monthly_mean_rent)

# Convert Year_Month to Date object
monthly_mean_rent$Year_Month <- as.Date(paste(monthly_mean_rent$Year_Month, "-01", sep = ""), format = "%Y-%m-%d")

# Create a time series plot
ggplot(monthly_mean_rent, aes(x = Year_Month, y = Mean_Rent, color = CITY, label = round(Mean_Rent,2))) +
  geom_line(size = 0.5) +
  geom_text() +
  labs(x = "Year_Month", y = "Mean Rent Amount", title = "Mean Rent Amount by City and Month") +
  scale_color_discrete(name = "CITY") +
  scale_x_date(labels = scales::date_format("%Y-%m")) +  # Custom date format
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 100000, by = 20000)) +
  facet_wrap(~ CITY, ncol=2)

### Testing
# Additional Feature: Multiple Linear Regression Analysis

# Perform Multiple Linear Regression Analysis
model <- lm(RENT_AMOUNT ~ SIZE + CITY + FURNISHING_STATUS, data = data_set)

# Print the summary of the regression model
summary(model)