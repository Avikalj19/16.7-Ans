A) 

# Load necessary libraries

library(ggplot2)
library(readr)
library(forecast)

# Load the data (ensure the path to the CSV file is correct)

data <- read.csv("~/TCD Study/Data Mining/ShampooSales.csv")

# Inspect the first few rows of the data

print(head(data))

# Check the column names to ensure they are correct

print(names(data))

# Correct the date conversion to handle "Jan-95" format

data$Month <- as.Date(paste0("01-", data$Month), format="%d-%b-%y")

# Check for any missing or infinite values

missing_month <- which(is.na(data$Month))
missing_sales <- which(is.na(data$Shampoo.Sales))
infinite_sales <- which(!is.finite(data$Shampoo.Sales))

# Print rows with missing or infinite values

if (length(missing_month) > 0) print(data[missing_month, ])
if (length(missing_sales) > 0) print(data[missing_sales, ])
if (length(infinite_sales) > 0) print(data[infinite_sales, ])

# Clean the data by removing rows with missing or infinite values

data_clean <- data[complete.cases(data) & is.finite(data$Shampoo.Sales), ]

# Verify the cleaned data

print(head(data_clean))

# Create the time plot

ggplot(data_clean, aes(x = Month, y = Shampoo.Sales)) +
  geom_line(color = "blue") +
  labs(
    title = "Shampoo Sales Over Time",
    x = "Month",
    y = "Shampoo Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


B) 

#Convert the data to a time series object 

shampoo_ts <- ts(data_clean$Shampoo.Sales, frequency = 12, start = c(1980, 1))

#Decompose the time series

shampoo_decomp <- decompose(shampoo_ts)

#Plot the decomposed time series

autoplot(shampoo_decomp) +
  labs(
    title = "Decomposition of Shampoo Sales Time Series",
    x = "Year",
    y = "Shampoo Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

