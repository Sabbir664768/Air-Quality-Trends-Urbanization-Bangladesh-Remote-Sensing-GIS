library(readr)
Sylhet_AQV_yearly <- read_csv("Sylhet_AQV_yearly.csv")
View(Sylhet_AQV_yearly)



library(dplyr)

colnames(Sylhet_AQV)

# Multiply specified columns by 10^9
columns_to_multiply <- c("pm10", "pm2p5", "go3", "co", "no2", "so2")
Sylhet_AQV[, columns_to_multiply] <- Sylhet_AQV[, columns_to_multiply] * 10^9

# Export the modified dataset as a CSV file
write.csv(Sylhet_AQV, "Sylhet_AQV_modified.csv", row.names = FALSE)

# Calculate annual averages
Sylhet_AQV_yearly <- Sylhet_AQV %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year) %>%
  summarize_at(columns_to_multiply, mean)

# Export the annual average dataset as a CSV file
write.csv(Sylhet_AQV_yearly, "Sylhet_AQV_yearly.csv", row.names = FALSE)

# Load required libraries
library(ggplot2)
library(zoo)
library(trend)

library(Kendall)
MK1<-MannKendall(Sylhet_AQV_yearly$pm10)
MK2<-MannKendall(Sylhet_AQV_yearly$pm2p5)
MK3<-MannKendall(Sylhet_AQV_yearly$go3)
MK4<-MannKendall(Sylhet_AQV_yearly$co)
MK5<-MannKendall(Sylhet_AQV_yearly$no2)
MK6<-MannKendall(Sylhet_AQV_yearly$so2)

summary(MK1)
summary(MK2)
summary(MK3)
summary(MK4)
summary(MK5)
summary(MK6)

library(trend)

sens.slope(Sylhet_AQV_yearly$pm10)
sens.slope(Sylhet_AQV_yearly$pm2p5)
sens.slope(Sylhet_AQV_yearly$go3)
sens.slope(Sylhet_AQV_yearly$co)
sens.slope(Sylhet_AQV_yearly$no2)
sens.slope(Sylhet_AQV_yearly$so2)


# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = pm2p5)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM2.5 in Sylhet From 2003 to 2022",
       x = "Year", y = "PM2.5")


# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = pm10)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM10 in Sylhet From 2003 to 2022",
       x = "Year", y = "PM10")

# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = go3)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Ozone in Sylhet From 2003 to 2022",
       x = "Year", y = "Ozone")




# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = co)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Carbon Monoxide in Sylhet From 2003 to 2022",
       x = "Year", y = "Carbon Monoxide")



# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = no2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Nitrogen Dioxide in Sylhet From 2003 to 2022",
       x = "Year", y = "Nitrogen Dioxide")



# Visualize the dataset
ggplot(Sylhet_AQV_yearly, aes(x =year, y = so2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Sulfur Dioxide in Sylhet From 2003 to 2022",
       x = "Year", y = "Sulfur Dioxide")



# Increase the title size using cex.main parameter
title(main = "My Plot Title", cex.main = 1.5) # You can adjust the value of cex.main to change the font size



library(ggplot2)

ggplot(Sylhet_AQV_yearly, aes(x = year)) +
  geom_line(aes(y = pm2p5, color = "PM2.5")) +
  geom_line(aes(y = pm10, color = "PM10")) +
  geom_line(aes(y = go3, color = "Ozone")) +
  geom_line(aes(y = co, color = "Carbon Monoxide")) +
  geom_line(aes(y = no2, color = "Nitrogen Dioxide")) +
  geom_line(aes(y = so2, color = "Sulfur Dioxide")) +
  labs(title = "Air Quality Trends in Sylhet From 2003 to 2022",
       x = "Year", y = "Value") +
  scale_color_manual(values = c("PM2.5" = "blue", "PM10" = "red", "Ozone" = "green", "Carbon Monoxide" = "orange", "Nitrogen Dioxide" = "purple", "Sulfur Dioxide" = "brown")) +
  theme_minimal()


library(ggplot2)
library(reshape2)

# Create a subset of the data with the variables of interest
subset_data <- Sylhet_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)
correlation_matrix 


# Melt the correlation matrix for plotting
melted_correlation <- melt(correlation_matrix)

# Create a correlation matrix plot
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Correlation Matrix Plot",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Install and load the 'car' package if not already installed
if (!require(car)) {
  install.packages("car")
  library(car)
}

# Create a subset of the data with the variables of interest
subset_data <- Sylhet_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

# Create the scatterplot matrix
scatterplotMatrix(subset_data, diagonal = "histogram")



colnames(Sylhet_AQV_yearly)







# Define the dataset  best
data <- read.table(text = "
Year    pm10    pm2p5   go3     co      no2     so2
2003    82.06057875     58.71950787     42.33499815     637.9827395     8.069430886     3.467593334
2004    76.75531958     54.85177265     42.06882443     553.105462      7.876209031     3.557119938
2005    81.26192015     58.1189511      45.19125216     591.9050429     9.011769875     3.980708471
2006    90.7112544      64.78936251     46.93471762     663.7732405     10.27904793     4.369441955
2007    84.82459252     60.52047687     43.47224814     638.7354077     9.184124084     4.419576165
2008    82.46201576     58.93387555     43.01998777     590.09771       8.519497385     4.642593482
2009    92.12503901     65.93928029     46.18470478     664.7675058     9.198584997     5.11612717
2010    80.78946965     57.54504418     42.68443752     591.4845709     8.728016951     5.159077253
2011    84.8991729      60.66237565     44.8483082      615.281895      9.281898684     5.535092425
2012    93.46359471     66.97535176     47.56117943     647.8975168     9.693054082     5.738430733
2013    86.78054407     61.86793084     44.15392096     628.8136611     9.896918961     6.027445777
2014    93.19607813     66.60856563     47.46927625     675.7532092     10.45019925     6.311150066
2015    93.86730156     67.1915896      45.60744226     655.7215492     10.701134       6.306629909
2016    93.63772369     66.78108836     45.03591506     645.8973708     11.05512565     6.366354414
2017    87.56785348     62.4247725      44.62476927     602.7049023     11.33929697     6.266446919
2018    100.2439241      71.64912815     48.66083895     680.2831567     12.19280464     6.739739989
2019    93.19542961     66.55362953     47.93763696     636.5168949     12.32956879     7.118092581
2020    96.06288312     68.23070009     48.0616717      676.8241353     12.80978616     7.506224523
2021    106.4272862     75.98113186     49.28156545     727.8243748     12.87829898     7.656489659
2022    92.14027935     65.7553028      45.43524473     615.9579025     11.73724665     7.296509872
", header = TRUE)

# Define the AQI breakpoints and corresponding AQI values for each pollutant (simplified, for demonstration)
breakpoints <- data.frame(
  Pollutant = c("pm10", "pm2p5", "go3", "co", "no2", "so2"),
  Low = c(0, 0, 0, 0, 0, 0),
  High = c(50, 50, 50, 50, 50, 50),
  AQI_Low = c(0, 0, 0, 0, 0, 0),
  AQI_High = c(100, 100, 100, 100, 100, 100)
)

# Function to calculate AQI for a single pollutant
calculate_AQI <- function(concentration, pollutant) {
  breakpoints_row <- breakpoints[breakpoints$Pollutant == pollutant, ]
  C_low <- breakpoints_row$Low
  C_high <- breakpoints_row$High
  I_low <- breakpoints_row$AQI_Low
  I_high <- breakpoints_row$AQI_High
  
  AQI <- (I_high - I_low) * (concentration - C_low) / (C_high - C_low) + I_low
  return(AQI)
}

# Calculate AQI for each pollutant and add columns to the dataset
for (pollutant in c("pm10", "pm2p5", "go3", "co", "no2", "so2")) {
  data[paste(pollutant, "AQI", sep = "_")] <- sapply(data[, pollutant], calculate_AQI, pollutant)
}

# Calculate the overall AQI for each year
data$Overall_AQI <- apply(data[, c("pm10_AQI", "pm2p5_AQI", "go3_AQI", "co_AQI", "no2_AQI", "so2_AQI")], 1, max)

# Print the resulting dataset with AQI values
print(data)


write.csv(data, "AQI_data_Sylhet.csv", row.names = FALSE)

# Assuming you have a dataframe named "AQI_data_Sylhet" with the specified variables

# Assuming you have a dataframe named "your_data" with the specified variables
your_data<-Urban_Sylhet

# Scatterplot of Urbanization vs. pm10_AQI
plot(your_data$Urbanization, your_data$pm10_AQI, 
     xlab = "Urbanization", ylab = "pm10_AQI", 
     main = "Scatterplot of Urbanization vs. pm10_AQI")

# Scatterplot of Urbanization vs. pm2.5_AQI
plot(your_data$Urbanization, your_data$pm2p5_AQI, 
     xlab = "Urbanization", ylab = "pm2.5_AQI", 
     main = "Scatterplot of Urbanization vs. pm2.5_AQI")

# Scatterplot of Urbanization vs. go3_AQI
plot(your_data$Urbanization, your_data$go3_AQI, 
     xlab = "Urbanization", ylab = "go3_AQI", 
     main = "Scatterplot of Urbanization vs. go3_AQI")

# Scatterplot of Urbanization vs. co_AQI
plot(your_data$Urbanization, your_data$co_AQI, 
     xlab = "Urbanization", ylab = "co_AQI", 
     main = "Scatterplot of Urbanization vs. co_AQI")

# Scatterplot of Urbanization vs. no2_AQI
plot(your_data$Urbanization, your_data$no2_AQI, 
     xlab = "Urbanization", ylab = "no2_AQI", 
     main = "Scatterplot of Urbanization vs. no2_AQI")

# Scatterplot of Urbanization vs. so2_AQI
plot(your_data$Urbanization, your_data$so2_AQI, 
     xlab = "Urbanization", ylab = "so2_AQI", 
     main = "Scatterplot of Urbanization vs. so2_AQI")

# Visualize the dataset
ggplot(your_data, aes(x =Year, y =Urbanization )) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Urbanization in Sylhet From 2003 to 2020",
       x = "Year", y = "Urbanization")





install.packages("corrplot")

# Create a data frame
data <- data.frame(
  Urbanization = c(1, 0.861868964, 0.856688388, 0.981177587, 0.355925735, 0.959214518, 0.951358791),
  PM10 = c(0.861868964, 1, 0.99990083, 0.905948367, 0.777557978, 0.92975058, 0.96616798),
  PM2.5 = c(0.856688388, 0.99990083, 1, 0.90337849, 0.782707666, 0.925552909, 0.964758555),
  O3 = c(0.981177587, 0.905948367, 0.90337849, 1, 0.445021102, 0.94710503, 0.98307371),
  CO = c(0.355925735, 0.777557978, 0.782707666, 0.445021102, 1, 0.534061766, 0.594695626),
  NO2 = c(0.959214518, 0.92975058, 0.925552909, 0.94710503, 0.534061766, 1, 0.965349217),
  SO2 = c(0.951358791, 0.96616798, 0.964758555, 0.98307371, 0.594695626, 0.965349217, 1)
)

# Convert the data frame to a correlation matrix
cor_matrix <- cor(data)

# Load the corrplot package
library(corrplot)

# Customize the correlation matrix plot
corrplot(cor_matrix, 
         method = "color",  # Color method
         col = colorRampPalette(c("blue", "white", "red"))(100),  # Custom color palette
         type = "upper",     # Display only the upper triangle
         tl.cex = 0.8,       # Text label size
         tl.col = "black",    # Text label color
         mar = c(2, 2, 2, 2)
)

title("Correlation Matrix Plot", cex.main = 1.5)





# Create a data frame
data <- data.frame(
  PM10 = c(1.0000000, 0.9996785, 0.8966671, 0.8918797, 0.8554090, 0.8098177),
  PM2.5 = c(0.9996785, 1.0000000, 0.8976478, 0.8926146, 0.8459315, 0.8009589),
  O3 = c(0.8966671, 0.8976478, 1.0000000, 0.7969792, 0.8013961, 0.7095390),
  CO = c(0.8918797, 0.8926146, 0.7969792, 1.0000000, 0.6419842, 0.5625149),
  NO2 = c(0.8554090, 0.8459315, 0.8013961, 0.6419842, 1.0000000, 0.9139949),
  SO2 = c(0.8098177, 0.8009589, 0.7095390, 0.5625149, 0.9139949, 1.0000000)
)

# Convert the data frame to a correlation matrix
cor_matrix <- cor(data)

# Load the corrplot package
library(corrplot)

# Customize the correlation matrix plot
corrplot(cor_matrix, 
         method = "color",  # Color method
         col = colorRampPalette(c("blue", "white", "red"))(100),  # Custom color palette
         type = "upper",     # Display only the upper triangle
         tl.cex = 0.8,       # Text label size
         tl.col = "black",   # Text label color
         mar = c(2, 2, 2, 2)
)

title("Correlation Matrix Plot", cex.main = 1.5)




