library(dplyr)
library(ggplot2)
library(tidyr)

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

### Objective 2
# To investigate the relationship between area size, furnishing status, and rent prices

### ***Question 1: How many houses are there that are small, medium, and large categorized by furnishing status?***
### Analysis 1.1 - Find frequency of houses base on furnishing status.

total_furnished <- sum(data_set$FURNISHING_STATUS == "Furnished")
total_semi_furnished <- sum(data_set$FURNISHING_STATUS == "Semi-Furnished")
total_unfurnished <- sum(data_set$FURNISHING_STATUS == "Unfurnished")

furnishing_freq <- data.frame(
  Furnishing_Status = c("Unfurnished", "Semi-Furnished", "Furnished"),
  Frequency = c(total_unfurnished, total_semi_furnished, total_furnished)
)
View(furnishing_freq)


## Analysis 1.2 - Find value of house area that are considered small, medium, 
#                 and large then categorize it by furnishing status.

# determine small size and medium size and large size
small_size <- quantile(data_set$SIZE, 0.25)
large_size <- quantile(data_set$SIZE, 0.75)
## Unfurnished
# small (<= q1)
total_unfurnished_small <- sum(data_set$FURNISHING_STATUS == "Unfurnished" & data_set$SIZE <= small_size)
# medium ( q1< x < q3)
total_unfurnished_medium <- sum(data_set$FURNISHING_STATUS == "Unfurnished" & between(data_set$SIZE, small_size + 1, large_size))
# large (>= q3)
total_unfurnished_large <- sum(data_set$FURNISHING_STATUS == "Unfurnished" & data_set$SIZE >= large_size)

## Semi-Furnished
# small (<= q1)
total_semi_furnished_small <- sum(data_set$FURNISHING_STATUS == "Semi-Furnished" & data_set$SIZE <= small_size)
# medium ( q1 < x < q3)
total_semi_furnished_medium <- sum(data_set$FURNISHING_STATUS == "Semi-Furnished" & between(data_set$SIZE, small_size + 1, large_size))
# large (>= q3)
total_semi_furnished_large <- sum(data_set$FURNISHING_STATUS == "Semi-Furnished" & data_set$SIZE >= large_size)


## Furnished
# small (<= q1)
total_furnished_small <- sum(data_set$FURNISHING_STATUS == "Furnished" & data_set$SIZE <= small_size)
# medium ( q1 < x < q3)
total_furnished_medium <- sum(data_set$FURNISHING_STATUS == "Furnished" & between(data_set$SIZE, small_size + 1, large_size))
# large (>= q3)
total_furnished_large <- sum(data_set$FURNISHING_STATUS == "Furnished" & data_set$SIZE >= large_size)

# Furnishing Status and House Size
furnishing_size_freq <- data.frame(
  Furnishing_Status = rep(c("Unfurnished", "Semi-Furnished", "Furnished"), each = 3),
  Size_Category = rep(c("Small", "Medium", "Large"), times = 3),
  Frequency = c(
    total_unfurnished_small, total_unfurnished_medium, total_unfurnished_large,
    total_semi_furnished_small, total_semi_furnished_medium, total_semi_furnished_large,
    total_furnished_small, total_furnished_medium, total_furnished_large
  )
)
View(furnishing_size_freq)

# Create the grouped bar plot
ggplot(furnishing_size_freq, aes(x = Furnishing_Status, y = Frequency, fill = Size_Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Furnishing Status", y = "Frequency", fill = "Size Category") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Small" = "skyblue", "Medium" = "lightgreen", "Large" = "salmon")) +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.8))



### ***Question 2: Among the houses that are considered small, how many houses are considered cheap based on the furnishing status? ***
### Analysis 2.1 - Find value of rent for houses that are considered cheap according to the dataset

# Find price of rent that are considered cheap and expensive
cheap <- quantile(data_set$RENT_AMOUNT, 0.25)
expensive <- quantile(data_set$RENT_AMOUNT, 0.75)

# semi + unfurnished < q1
total_small_cheap_unfurnished <- sum(data_set$FURNISHING_STATUS == "Unfurnished" & data_set$SIZE <= small_size & data_set$RENT_AMOUNT <= cheap)
total_small_cheap_semi_furnished <- sum(data_set$FURNISHING_STATUS == "Semi-Furnished" & data_set$SIZE <= small_size & data_set$RENT_AMOUNT <= cheap)

# furnished < q1
total_small_cheap_furnished <- sum(data_set$FURNISHING_STATUS == "Furnished" & data_set$SIZE <= small_size & data_set$RENT_AMOUNT <= cheap)

### Analysis 2.2 - Find frequency of houses that are cheap, small and not fully furnished.
# Furnishing Status and Rent Amount
furnishing_rent_freq <- data.frame(
  Furnishing_Status = rep(c("Unfurnished", "Semi-Furnished", "Furnished"), each = 1),
  Frequency = c(
    total_small_cheap_unfurnished, total_small_cheap_semi_furnished, total_small_cheap_furnished
  )
)
View(furnishing_rent_freq)

plot_data <- data.frame(
  Furnishing_Status = c("Unfurnished", "Semi-Furnished", "Furnished"),
  Frequency = c(total_small_cheap_unfurnished, total_small_cheap_semi_furnished, total_small_cheap_furnished)
)

# Create the bar plot
ggplot(plot_data, aes(x = Furnishing_Status, y = Frequency, fill = Furnishing_Status)) +
  geom_bar(stat = "identity") +
  labs(x = "Houses below Q1 square feet", y = "Number of houses below Q1 rent", fill = "Furnishing Status") +
  theme(panel.background = element_rect(fill = "grey")) +
  scale_fill_manual(values = c("Furnished" = "pink", "Semi-Furnished" = "violet", "Unfurnished" = "purple")) +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.8))


### *** Question 3: What are the proportion of house rent above and below mean based on each furnishing status? ***
### Analysis 3.1 - Find mean price of house rent
# Mean rent amount
mean_rent <- mean(data_set$RENT_AMOUNT)
mean_rent

### Analysis 3.2 - Find the proportion of houses rent above and below mean based on each furnishing status.

# Furnished with rent price below & above mean
furnished_below_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Furnished" & data_set$RENT_AMOUNT < mean_rent]
furnished_above_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Furnished" & data_set$RENT_AMOUNT > mean_rent]


# Semi Furnished with rent price below and above mean
semi_furnished_below_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Semi-Furnished" & data_set$RENT_AMOUNT < mean_rent]
semi_furnished_above_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Semi-Furnished" & data_set$RENT_AMOUNT > mean_rent]


# Unfurnished with rent price below and above mean
unfurnished_below_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Unfurnished" & data_set$RENT_AMOUNT < mean_rent]
unfurnished_above_mean <- data_set$RENT_AMOUNT[data_set$FURNISHING_STATUS == "Unfurnished" & data_set$RENT_AMOUNT > mean_rent]


# Create a data frame for tabulation
result_table <- data.frame(
  Furnishing_Status = c("Furnished", "Semi-Furnished", "Unfurnished"),
  Freq_below_mean = c(length(furnished_below_mean), length(semi_furnished_below_mean), length(unfurnished_below_mean)),
  Freq_above_mean = c(length(furnished_above_mean), length(semi_furnished_above_mean), length(unfurnished_above_mean))
)

# Print the result table
View(result_table)


# Visualize the data (proportion of rent price (mean) and furnished status (mean))
ggplot(data_set, aes(x = FURNISHING_STATUS, fill = factor(RENT_AMOUNT >= mean_rent))) +
  geom_bar(position = "fill") +  # ensure the bar has a height of 1 when set to 'fill'
  labs(x = "Furnishing Status", y = "Proportion", fill = "Rent Above Mean") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Below Mean", "Above Mean")) +
  geom_text(
    aes(label = scales::percent(..count.. / sum(..count..))),  # Calculate and format the proportion
    stat = "count",
    position = position_fill(vjust = 0.5),  # Adjust vertical position
    show.legend = FALSE  # Hide the legend for geom_text
  )


### Additional feature: Create violin plot to visualize the distribution of houses based on their rent amount and furnishing status.
ggplot(data_set, aes(x = FURNISHING_STATUS, y = RENT_AMOUNT, fill = FURNISHING_STATUS)) +
  geom_violin() +
  labs(x = "Furnishing Status", y = "Rent Amount", fill = "Furnishing Status") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = mean_rent, color = "red", linetype = "dashed", size = 1.5) +  # Add a dashed line for mean
  geom_point(aes(y = mean_rent), color = "red", shape = 18, size = 4)  # Add a red point for mean


### *** Question 4: How does the rent price varies in different cities and area sizes based on furnishing status?***
### Analysis 4.1 - Visualize rent amount against area size for different cities based on furnishing status.

# subset the data for furnishing status
furnished_status <- "Unfurnished" # change furnishing status to view their respective graph
furnished_data <- data_set %>%filter (FURNISHING_STATUS == furnished_status)

# Create scatter plot with line of best fit
scatter_plot <- ggplot(furnished_data, aes(x = SIZE, y = RENT_AMOUNT, color = CITY)) +
  geom_point( size = 1) +
  labs(title = paste("Rent Amount vs Area Size by City"),
       x = "Area Size", y = "Rent Amount") +
  ggtitle("Rent Amount vs Area Size by City") +
  facet_wrap(~ CITY, ncol = 2) +
  geom_smooth(method = "lm", color = "black") +
  scale_y_continuous(labels = scales::comma)

# Display the plot
scatter_plot


### Testing 
### Additional Feature: ANoVA testing
# Subset the data for the desired analysis
analysis_data <- data_set %>% select(CITY, FURNISHING_STATUS, RENT_AMOUNT) # rent amount is dependent variable

# Perform ANOVA
anova_result <- aov(RENT_AMOUNT ~ CITY * FURNISHING_STATUS, data = analysis_data)
#"***": Highly significant (p-value < 0.001)
#"**": Significantly significant (0.001 ≤ p-value < 0.01)
#"*": Moderately significant (0.01 ≤ p-value < 0.05)
#".": Marginally significant (0.05 ≤ p-value < 0.1)
#No star: Not significant (p-value ≥ 0.1)

# Summarize the ANOVA results
anova_summary <- summary(anova_result)
anova_summary

## Additional Feature: Correlation Heatmap
# Select numeric attributes from the dataset
numeric_attributes <- sapply(data_set, is.numeric)
numeric_data <- data_set[, numeric_attributes]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Convert the correlation matrix to a data frame for ggplot
correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("Var1", "Var2", "Correlation")


# Plot the heatmap
ggplot(correlation_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +  # Add correlation labels
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numeric Attributes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

