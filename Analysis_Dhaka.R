
library(dplyr)

colnames(Dhaka_AQV)  # Assuming you have a Dhaka_AQV dataset

# Multiply specified columns by 10^9
columns_to_multiply <- c("pm10", "pm2p5", "go3", "co", "no2", "so2")
Dhaka_AQV[, columns_to_multiply] <- Dhaka_AQV[, columns_to_multiply] * 10^9

# Export the modified dataset as a CSV file
write.csv(Dhaka_AQV, "Dhaka_AQV_modified.csv", row.names = FALSE)

# Calculate annual averages
Dhaka_AQV_yearly <- Dhaka_AQV %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year) %>%
  summarize_at(columns_to_multiply, mean)

# Export the annual average dataset as a CSV file
write.csv(Dhaka_AQV_yearly, "Dhaka_AQV_yearly.csv", row.names = FALSE)

# Load required libraries
library(ggplot2)
library(zoo)
library(trend)

library(Kendall)
MK1 <- MannKendall(Dhaka_AQV_yearly$pm10)
MK2 <- MannKendall(Dhaka_AQV_yearly$pm2p5)
MK3 <- MannKendall(Dhaka_AQV_yearly$go3)
MK4 <- MannKendall(Dhaka_AQV_yearly$co)
MK5 <- MannKendall(Dhaka_AQV_yearly$no2)
MK6 <- MannKendall(Dhaka_AQV_yearly$so2)

summary(MK1)
summary(MK2)
summary(MK3)
summary(MK4)
summary(MK5)
summary(MK6)

library(trend)

sens.slope(Dhaka_AQV_yearly$pm10)
sens.slope(Dhaka_AQV_yearly$pm2p5)
sens.slope(Dhaka_AQV_yearly$go3)
sens.slope(Dhaka_AQV_yearly$co)
sens.slope(Dhaka_AQV_yearly$no2)
sens.slope(Dhaka_AQV_yearly$so2)

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = pm2p5)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM2.5 in Dhaka From 2003 to 2022",
       x = "Year", y = "PM2.5")

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = pm10)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM10 in Dhaka From 2003 to 2022",
       x = "Year", y = "PM10")

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = go3)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Ozone in Dhaka From 2003 to 2022",
       x = "Year", y = "Ozone")

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = co)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Carbon Monoxide in Dhaka From 2003 to 2022",
       x = "Year", y = "Carbon Monoxide")

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = no2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Nitrogen Dioxide in Dhaka From 2003 to 2022",
       x = "Year", y = "Nitrogen Dioxide")

# Visualize the dataset
ggplot(Dhaka_AQV_yearly, aes(x = year, y = so2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Sulfur Dioxide in Dhaka From 2003 to 2022",
       x = "Year", y = "Sulfur Dioxide")


# Additional code for visualization
library(ggplot2)

ggplot(Dhaka_AQV_yearly, aes(x = year)) +
  geom_line(aes(y = pm2p5, color = "PM2.5")) +
  geom_line(aes(y = pm10, color = "PM10")) +
  geom_line(aes(y = go3, color = "Ozone")) +
  geom_line(aes(y = co, color = "Carbon Monoxide")) +
  geom_line(aes(y = no2, color = "Nitrogen Dioxide")) +
  geom_line(aes(y = so2, color = "Sulfur Dioxide")) +
  labs(title = "Air Quality Trends in Dhaka From 2003 to 2022",
       x = "Year", y = "Value") +
  scale_color_manual(values = c("PM2.5" = "blue", "PM10" = "red", "Ozone" = "green", "Carbon Monoxide" = "orange", "Nitrogen Dioxide" = "purple", "Sulfur Dioxide" = "brown")) +
  theme_minimal()



library(ggplot2)
library(reshape2)

# Create a subset of the data with the variables of interest
subset_data <- Dhaka_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

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
subset_data <- Dhaka_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

# Create the scatterplot matrix
scatterplotMatrix(subset_data, diagonal = "histogram")



# Define the new dataset
data <- read.table(text = "
Year    pm10    pm2p5   go3     co      no2     so2
2003    107.3721872     76.34553442     48.53196243     624.447985     10.97269154     5.16495168
2004    112.8647875     80.36127659     47.29661201     632.3037515     11.746598     5.738534476
2005    107.6288399     76.67362863     50.99013414     596.5194688     12.18458114     6.02352434
2006    117.4328983     83.51693152     51.94908931     658.7684602     13.78633765     6.87890481
2007    119.4333255     84.80583409     47.93584836     693.2748157     13.61665611     7.216550006
2008    124.7764043     89.05631921     48.01333766     695.1754419     13.67076158     7.804229006
2009    130.4990672     93.09198725     51.15296229     732.4026204     14.26025621     8.51763604
2010    116.6698766     82.74719301     47.94415079     660.2950022     12.62013621     8.234163256
2011    131.5245222     93.82924388     50.38768005     736.3688414     14.82328489     9.283666799
2012    131.3595862     93.85626662     53.31199515     701.3211095     14.76905802     9.241852271
2013    128.9633897     91.69499374     48.69463963     715.6617752     15.10526461     9.70500426
2014    136.7589283     97.40634429     53.33408463     762.2387332     16.24099379     10.29685523
2015    141.4183705     100.924486      50.02232306     763.6328355     16.72474602     10.79524773
2016    135.9858871     96.65462094     48.78908599     763.3958381     17.01498118     10.63642838
2017    134.0829571     95.5049264      49.10105587     693.8371037     17.77189115     10.07352836
2018    150.873674      107.5587039     52.13315154     806.6037155     18.87123976     11.31048614
2019    143.9976151     102.5900691     52.90457255     772.6247954     19.32456021     12.05331822
2020    147.0705117     104.2269918     51.10455664     817.4080084     21.09029229     12.86934941
2021    157.4522315     112.0575798     52.90406937     862.240015      20.85080378     13.04375044
2022    142.8322729     101.6202532     49.67729444     757.7474003     19.13254043     12.17686545
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
write.csv(data, "AQI_data_Dhaka.csv", row.names = FALSE)


# Create a data frame for the new data
data <- data.frame(
  Urbanization = c(1, 0.982174279, 0.982190961, 0.916970719, 0.950399477, 0.987825622, 0.987416801),
  PM10 = c(0.982174279, 1, 0.999962411, 0.933823751, 0.990756222, 0.980371765, 0.999516416),
  PM2.5 = c(0.982190961, 0.999962411, 1, 0.936784335, 0.990262794, 0.979234108, 0.99954127),
  O3 = c(0.916970719, 0.933823751, 0.936784335, 1, 0.904992907, 0.875745612, 0.935649837),
  CO= c(0.950399477, 0.990756222, 0.990262794, 0.904992907, 1, 0.959273316, 0.986233523),
  NO2 = c(0.987825622, 0.980371765, 0.979234108, 0.875745612, 0.959273316, 1, 0.983231625),
  SO2 = c(0.987416801, 0.999516416, 0.99954127, 0.935649837, 0.986233523, 0.983231625, 1)
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

# Add a title
title("Correlation Matrix Plot", cex.main = 1.5)






# Create a data frame
data <- data.frame(
  PM10 = c(1.0000000, 0.9998053, 0.5312562, 0.9683678, 0.9472536, 0.9616819),
  PM2.5 = c(0.9998053, 1.0000000, 0.5365835, 0.9659757, 0.9430371, 0.9584216),
  O3 = c(0.5312562, 0.5365835, 1.0000000, 0.4919567, 0.4907063, 0.4735592),
  CO = c(0.9683678, 0.9659757, 0.4919567, 1.0000000, 0.9017734, 0.9249427),
  NO2 = c(0.9472536, 0.9430371, 0.4907063, 0.9017734, 1.0000000, 0.9639267),
  SO2 = c(0.9616819, 0.9584216, 0.4735592, 0.9249427, 0.9639267, 1.0000000)
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

