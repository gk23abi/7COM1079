# Load required library
library(tidyverse)
library(ggplot2)

# Define path to CSV files
FilePath <- list.files(pattern = "*.csv", full.names = TRUE)

# Combine all CSVs into one DataFrame
HotelPriceData <- FilePath %>% map_dfr(read_csv, show_col_types = FALSE)

# Check the first few rows of the combined data
head(HotelPriceData)

# Check the last few rows of the combined data
tail(HotelPriceData)

# Check missing values
colSums(is.na(HotelPriceData))

# Impute missing 'tax' with the median
HotelPriceData$Tax <- ifelse(is.na(HotelPriceData$Tax), median(HotelPriceData$Tax, na.rm = TRUE), HotelPriceData$Tax)

# Impute missing 'Rating' with the median
HotelPriceData$Rating[is.na(HotelPriceData$Rating)] <- median(HotelPriceData$Rating, na.rm = TRUE)

# Impute missing 'Rating Description' with the mode
ModeOfRatingDescription <- names(sort(table(HotelPriceData$`Rating Description`), decreasing = TRUE))[1]
HotelPriceData$`Rating Description`[is.na(HotelPriceData$`Rating Description`)] <- ModeOfRatingDescription

# Impute missing 'Reviews' with the median
HotelPriceData$Reviews[is.na(HotelPriceData$Reviews)] <- median(HotelPriceData$Reviews, na.rm = TRUE)

# Impute missing 'Star Rating' with the mode
ModeOfStarRating <- names(sort(table(HotelPriceData$`Star Rating`), decreasing = TRUE))[1]
HotelPriceData$`Star Rating`[is.na(HotelPriceData$`Star Rating`)] <- ModeOfStarRating

# Remove missing values for 'Nearest Landmark' and 'Distance to Landmark'
HotelPriceData <- HotelPriceData %>% filter(!is.na(`Nearest Landmark`) & !is.na(`Distance to Landmark`))

# Print the structure of the data
str(HotelPriceData)

# Print the summary of the data
summary(HotelPriceData)

# Print the columns of the data
colnames(HotelPriceData)

# Plotting a histogram for Rating with Bell Curve
ggplot(HotelPriceData, aes(x = Rating)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) + 
  geom_density(color = "red", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(HotelPriceData$Rating), sd = sd(HotelPriceData$Rating)), color = "black", size = 1) +
  labs(title = "Distribution of Hotel Ratings", x = "Rating", y = "Density") +
  theme_minimal()

# Plotting a histogram for Price with Bell Curve
ggplot(HotelPriceData, aes(x = Price)) + 
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "green", color = "black", alpha = 0.7) + 
  geom_density(color = "red", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(HotelPriceData$Price), sd = sd(HotelPriceData$Price)), color = "black", size = 1) +
  labs(title = "Distribution of Hotel Prices", x = "Price", y = "Density") +
  theme_minimal()

# Scatter Plot between Rating and Price with Regression Line
ggplot(HotelPriceData, aes(x = Rating, y = Price)) + 
  geom_point(color = "red", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Scatter Plot of Rating vs Price", x = "Rating", y = "Price") +
  theme_minimal()
