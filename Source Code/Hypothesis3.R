
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("scales")
library(scales)

install.packages("ggridges")
library(ggridges)

install.packages("grid")
library(grid)

install.packages("broom")
library(broom)

library(tidyr)
library(stats)

install.packages("plotly")
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

#objective 3 --------------------------------------------------------------------------------


# Objective 3 : To investigate the relationship between rental price based on cities

" Question 1 : What is the price range for affordable rental properties across various 
cities, considering the first quartile (Q1) rental prices and the frequency of these
low-cost rentals?" 

#Analysis 1.1 To determine the Q1 for rent amount in different cities. 
unique(data_set$CITY) # 6 cities

#Calculate the frequencies of houses less than Q1 
# Calculate Q1 price for each city
q1_kolkata <- calculate_q1(data_set[data_set$CITY == "Kolkata", ]$RENT_AMOUNT)
q1_mumbai <- calculate_q1(data_set[data_set$CITY == "Mumbai", ]$RENT_AMOUNT)
q1_bangalore <- calculate_q1(data_set[data_set$CITY == "Bangalore", ]$RENT_AMOUNT)
q1_delhi <- calculate_q1(data_set[data_set$CITY == "Delhi", ]$RENT_AMOUNT)
q1_chennai <- calculate_q1(data_set[data_set$CITY == "Chennai", ]$RENT_AMOUNT)
q1_hyderabad <- calculate_q1(data_set[data_set$CITY == "Hyderabad", ]$RENT_AMOUNT)

# Create a data frame for Q1 values
cheap_rent_by_city <- data.frame(
  City = unique(data_set$CITY),
  Q1 = c(q1_kolkata, q1_mumbai, q1_bangalore, q1_delhi, q1_chennai, q1_hyderabad)
)
print("Q1 values for each city:")
print(cheap_rent_by_city)

city_plot <- ggplot(cheap_rent_by_city, aes(x = City, y = Q1, fill = Q1)) +
  geom_bar(stat = "identity") +
  labs(x = "City", y = "Q1 Value", title = "Q1 Values for Rental Prices in Each City") +
  theme_minimal() +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "red")

print(city_plot)

#Analysis 1.2 To find frequencies of houses cost below or equal to Q1 based on city. 
# Calculate Frequency of houses that falls below q1 price in each city-----------------------------
freq_cheap_kolkata <- sum(data_set$CITY == "Kolkata" & data_set$RENT_AMOUNT <= q1_kolkata)
freq_cheap_mumbai <- sum(data_set$CITY == "Mumbai" & data_set$RENT_AMOUNT <= q1_mumbai)
freq_cheap_bangalore <- sum(data_set$CITY == "Bangalore" & data_set$RENT_AMOUNT <= q1_bangalore)
freq_cheap_delhi <- sum(data_set$CITY == "Delhi" & data_set$RENT_AMOUNT <= q1_delhi)
freq_cheap_chennai <- sum(data_set$CITY == "Chennai" & data_set$RENT_AMOUNT <= q1_chennai)
freq_cheap_hyderabad <- sum(data_set$CITY == "Hyderabad" & data_set$RENT_AMOUNT <= q1_hyderabad)

freq_table <- data.frame(
  City = unique(data_set$CITY),
  Frequency = c(freq_cheap_kolkata,
                freq_cheap_mumbai, freq_cheap_bangalore,
                freq_cheap_chennai, freq_cheap_delhi, freq_cheap_hyderabad)
)


print(freq_table)

#create a data frame 
city_data <- data.frame(
  City = unique(data_set$CITY),
  Q1 = c(q1_kolkata, q1_mumbai, q1_bangalore, q1_delhi, q1_chennai, q1_hyderabad),
  Frequency = c(
    freq_cheap_kolkata, freq_cheap_mumbai, freq_cheap_bangalore,
    freq_cheap_delhi, freq_cheap_chennai, freq_cheap_hyderabad
  )
)

# Calculate the total frequency
total_frequency <- sum(city_data$Frequency)

# Calculate the percentage for each city
city_data$Percentage <- (city_data$Frequency / total_frequency) * 100

# Create a pie chart with percentage labels
pie_chart <- ggplot(city_data, aes(x = "", y = Percentage, fill = City)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to polar coordinates
  labs(fill = "City", x = NULL, y = NULL, title = "Pie Chart of Frequency Distribution of Q1 price by City") +
  theme_void() +
  theme(legend.position = "right") +  # Adjust legend position if needed
  geom_text(aes(label = paste(sprintf("%.2f%%", Percentage))), position = position_stack(vjust = 0.5))

# Print the pie chart with percentage labels
print(pie_chart)


#Filter Cities with rent lower than Q1 price-----------------------------------------------------
summary_stats <- data_set %>%
  group_by(CITY) %>%
  summarize(Q1_Price = quantile(RENT_AMOUNT, 0.25))

filtered_data <- data_set %>%
  left_join(summary_stats, by = "CITY") %>%
  filter(RENT_AMOUNT <= Q1_Price)


#Additional Feature : Violin Plot
# Create a violin plot for rent amounts and Q1 prices
violin_plot <- ggplot(filtered_data, aes(x = CITY, y = RENT_AMOUNT, fill = "Rent Amount")) +
  geom_violin(trim = FALSE, scale = "width", fill = "lightgreen", color = "grey") +  # Adjust fill and border color
  geom_boxplot(width = 0.15, fill = "white", color = "black") +  # Add a white boxplot inside the violin plot
  geom_point(data = filtered_data %>% distinct(CITY, .keep_all = TRUE),
             aes(y = Q1_Price, color = "Q1 Price"), alpha = 0.7, size = 3) +
  labs(x = "City", y = "Rent Price", title = "Violin Plot with Boxplot of Rent Amounts and Q1 Prices by City") +
  scale_fill_manual(values = c("Rent Amount" = "lightblue")) +
  scale_color_manual(values = c("Q1 Price" = "black")) +
  theme_minimal() +
  theme(legend.position = "top")

# Print the modified violin plot
print(violin_plot)


"Question 2 – How do various aspects of a houses’ infrastructure significantly influence 
rental prices, particularly within the range of the first quarter (Q1) rental price?" 

#Analysis 2.1.  Determine the impact of furnishing status on rental prices based on cities. 
#Q1 group by city and furnishing status  
summary_stats <- data_set %>%
  group_by(CITY, FURNISHING_STATUS) %>%
  summarize(
    Q1_Price = quantile(RENT_AMOUNT, 0.25),
    Price = mean(RENT_AMOUNT)
  )

View(summary_stats)


#Additional Feature : Facet wrap 
# Create the facetted bar chart with adjusted y-axis limits
facetted_bar_chart <- ggplot(summary_stats, aes(x = FURNISHING_STATUS, y = Q1_Price, fill = FURNISHING_STATUS)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("$%.2f", Q1_Price)), vjust = -0.5) +  # Add mean labels
  facet_wrap(~ CITY, nrow = 3, ncol = 2, scales = "free_x") +
  labs(
    x = "Furnishing Status",
    y = "Q1 Rent Amount",
    title = "Q1 Rent Amount by Furnishing Status Faceted by City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, 60000))  # Adjust y-axis limits here

# Print the facetted bar chart
print(facetted_bar_chart)


##Additional Feature : Kruskal-Wallis Testing   
#TESTING---------------------------------------------------------------------------------------
# Perform Kruskal-Wallis test for CITY
kruskal_city <- kruskal.test(Price ~ CITY, data = summary_stats)

# Perform Kruskal-Wallis test for FURNISHING_STATUS
kruskal_furnishing <- kruskal.test(Price  ~ FURNISHING_STATUS, data = summary_stats)

# Print Kruskal-Wallis test results for CITY
print("Kruskal-Wallis test results for CITY:")
print(kruskal_city)

# Print Kruskal-Wallis test results for FURNISHING_STATUS
print("Kruskal-Wallis test results for FURNISHING_STATUS:")
print(kruskal_furnishing)

#Analysis 2.2 Determine the impact of furnishing status and area types on rental prices based on cities. 
#area type, furnishing status, city, on rental price-------------------------------------------------------------------------
summary_stats <- data_set %>%
  group_by(CITY, FURNISHING_STATUS,AREA_TYPE) %>%
  summarize(
    Q1_Price = quantile(RENT_AMOUNT, 0.25),
    Price = mean(RENT_AMOUNT)
  )

View(summary_stats)


#overall 
# Create a faceted scatter plot with larger dots, a separating line, and label
facetted_scatter_plot <- ggplot(summary_stats, aes(x = Price, y = Q1_Price, color = FURNISHING_STATUS)) +
  geom_point(alpha = 0.7, size = 5) +  # Increase the size of the dots
  geom_hline(yintercept = mean(range(summary_stats$Q1_Price)), linetype = "dashed", color = "black") +  # Add dashed line
  geom_text(aes(x = Inf, y = mean(range(Q1_Price)), label = paste("Median: $", sprintf("%.2f", mean(range(Q1_Price))))),
            hjust = 1.3, vjust = 1.5, size = 2.5, color = "black") +  # Add smaller label on the dashed line
  labs(
    x = "Mean Rent Amount",
    y = "Q1 Rent Amount",
    title = "Q1 vs. Mean Rent Amount Faceted by Area Type and City"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = c("#E0EAF3", "#F3ECD1")),  # Set custom background colors
    panel.grid.major = element_line(color = "grey"),  # Adjust grid line color
    panel.grid.minor = element_line(color = "white"),  # Adjust minor grid line color
    text = element_text(size = 10)  # Adjust text size
  ) +
  facet_grid(AREA_TYPE ~ CITY, scales = "free")


# Print the faceted scatter plot
print(facetted_scatter_plot)


#Analysis 2.3 Indicate the impact of number of BHK on the rental price based on furnishing status and area types. 
#Area_types vs BHK  vs furnishing status--------------------------------------------------
summary_stats <- data_set %>%
  group_by(AREA_TYPE, B_H_K, FURNISHING_STATUS) %>%
  summarize(
    Q1_Price = quantile(RENT_AMOUNT, 0.25)
  )

View(summary_stats)

# Create a grouped bar chart using ggplot
grouped_bar_chart <- ggplot(summary_stats, aes(x = FURNISHING_STATUS, y = Q1_Price, fill = factor(B_H_K))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_text(aes(label = scales::number_format(scale = 1e-2, suffix = "K")(Q1_Price)),
            position = position_dodge(width = 0.9), vjust = -0.5, alpha = 0.7, size = 3) +  # Adding value labels
  facet_wrap(~ AREA_TYPE, ncol = 2, scales = "free") +
  labs(
    x = "Furnishing Status",
    y = "Q1 Rent Amount",
    title = "Grouped Bar Chart of Q1 Rent Amount by Furnishing Status Grouped by BHK and Faceted by Area Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-2, suffix = "K"),
                     limits = c(0, 30000)) +  # Adjust the limits according to your desired range
  scale_fill_manual(values = c("lightblue", "blue", "lightgreen", "green"))


# Convert ggplot to plotly
grouped_bar_chart_3d <- ggplotly(grouped_bar_chart)

# Print the interactive 3D grouped bar chart
print(grouped_bar_chart)

