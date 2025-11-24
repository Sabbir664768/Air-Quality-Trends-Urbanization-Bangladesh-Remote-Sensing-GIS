

library(dplyr)

colnames(Chittagong_AQV)  # Assuming you have a Chittagong_AQV dataset

# Multiply specified columns by 10^9
columns_to_multiply <- c("pm10", "pm2p5", "go3", "co", "no2", "so2")
Chittagong_AQV[, columns_to_multiply] <- Chittagong_AQV[, columns_to_multiply] * 10^9

# Export the modified dataset as a CSV file
write.csv(Chittagong_AQV, "Chittagong_AQV_modified.csv", row.names = FALSE)

# Calculate annual averages
Chittagong_AQV_yearly <- Chittagong_AQV %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year) %>%
  summarize_at(columns_to_multiply, mean)

# Export the annual average dataset as a CSV file
write.csv(Chittagong_AQV_yearly, "Chittagong_AQV_yearly.csv", row.names = FALSE)

# Load required libraries
library(ggplot2)
library(zoo)
library(trend)

library(Kendall)
MK1 <- MannKendall(Chittagong_AQV_yearly$pm10)
MK2 <- MannKendall(Chittagong_AQV_yearly$pm2p5)
MK3 <- MannKendall(Chittagong_AQV_yearly$go3)
MK4 <- MannKendall(Chittagong_AQV_yearly$co)
MK5 <- MannKendall(Chittagong_AQV_yearly$no2)
MK6 <- MannKendall(Chittagong_AQV_yearly$so2)

summary(MK1)
summary(MK2)
summary(MK3)
summary(MK4)
summary(MK5)
summary(MK6)

library(trend)

sens.slope(Chittagong_AQV_yearly$pm10)
sens.slope(Chittagong_AQV_yearly$pm2p5)
sens.slope(Chittagong_AQV_yearly$go3)
sens.slope(Chittagong_AQV_yearly$co)
sens.slope(Chittagong_AQV_yearly$no2)
sens.slope(Chittagong_AQV_yearly$so2)

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = pm2p5)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM2.5 in Chittagong From 2003 to 2022",
       x = "Year", y = "PM2.5")

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = pm10)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of PM10 in Chittagong From 2003 to 2022",
       x = "Year", y = "PM10")

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = go3)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Ozone in Chittagong From 2003 to 2022",
       x = "Year", y = "Ozone")

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = co)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Carbon Monoxide in Chittagong From 2003 to 2022",
       x = "Year", y = "Carbon Monoxide")

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = no2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Nitrogen Dioxide in Chittagong From 2003 to 2022",
       x = "Year", y = "Nitrogen Dioxide")

# Visualize the dataset
ggplot(Chittagong_AQV_yearly, aes(x = year, y = so2)) +
  geom_point() +
  geom_line() +
  labs(title = "Trends of Sulfur Dioxide in Chittagong From 2003 to 2022",
       x = "Year", y = "Sulfur Dioxide")



# Additional code for visualization
library(ggplot2)

ggplot(Chittagong_AQV_yearly, aes(x = year)) +
  geom_line(aes(y = pm2p5, color = "PM2.5")) +
  geom_line(aes(y = pm10, color = "PM10")) +
  geom_line(aes(y = go3, color = "Ozone")) +
  geom_line(aes(y = co, color = "Carbon Monoxide")) +
  geom_line(aes(y = no2, color = "Nitrogen Dioxide")) +
  geom_line(aes(y = so2, color = "Sulfur Dioxide")) +
  labs(title = "Air Quality Trends in Chittagong From 2003 to 2022",
       x = "Year", y = "Value") +
  scale_color_manual(values = c("PM2.5" = "blue", "PM10" = "red", "Ozone" = "green", "Carbon Monoxide" = "orange", "Nitrogen Dioxide" = "purple", "Sulfur Dioxide" = "brown")) +
  theme_minimal()


library(ggplot2)
library(reshape2)

# Create a subset of the data with the variables of interest
subset_data <- Chittagong_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

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
subset_data <- Chittagong_AQV_yearly[, c("pm10", "pm2p5", "go3", "co", "no2", "so2")]

# Create the scatterplot matrix
scatterplotMatrix(subset_data, diagonal = "histogram")


# Define the new dataset
data <- read.table(text = "
Year    pm10    pm2p5   go3     co      no2     so2
2003    60.99735162     43.6872884      54.04896662     449.4049129     2.971736005     1.790839445
2004    55.40869764     39.65502543     52.55691984     384.3652724     2.734577233     1.706100549
2005    59.08429396     42.38510848     55.13775752     420.2075108     3.261106665     1.954757552
2006    65.55122485     46.85251712     58.41656466     462.9156466     3.736173829     2.098575845
2007    59.33446074     42.3080243      53.42948959     431.5336408     3.260531375     2.024338579
2008    54.9056709      39.33908883     53.90899279     377.5037095     2.930245423     1.853988224
2009    73.2196146      52.60886022     59.09622784     504.8717412     3.593292816     2.39869084
2010    52.63202596     37.37811054     52.52289782     399.9630829     2.712768528     2.109602557
2011    62.10484832     44.55453979     58.22236284     408.5606255     3.120439646     2.28348177
2012    69.25255603     49.82840102     61.99578447     441.0067587     3.25235529      2.509467604
2013    62.02884071     44.22600915     58.53797317     400.045439      3.204431925     2.551597677
2014    64.11815224     45.82588603     62.5277023      427.7669332     3.27372817      2.664775917
2015    61.2032305      43.81170879     58.72276281     394.937194      3.284327444     2.469877999
2016    64.19894773     45.90991865     55.75170813     436.4576682     3.744227883     2.457360982
2017    55.60440227     39.662191       60.06309212     340.4196279     3.235445263     2.252957543
2018    68.30904425     48.90795068     65.45299132     411.6554808     3.589457552     2.522139058
2019    66.01594854     47.27485161     66.69462225     412.6719283     3.769279129     2.859310911
2020    63.56275806     45.11844862     66.08852951     438.692119      4.065779893     3.044456211
2021    70.93280298     50.71519488     65.56939152     439.8169299     4.219050984     3.011677223
2022    59.23960085     42.31627556     62.68304649     371.2186393     3.679900048     2.856191309
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



write.csv(data, "AQI_data_Chittagong.csv", row.names = FALSE)



# Create a data frame for the new data
data <- data.frame(
  Urbanization = c(1, 0.794219, 0.788699579, 0.973435152, -0.51899618, 0.84799722, 0.995238952),
  PM10 = c(0.794219, 1, 0.999553406, 0.819123341, 0.094057869, 0.676183904, 0.832033186),
  PM2.5 = c(0.788699579, 0.999553406, 1, 0.811466516, 0.098248666, 0.656826186, 0.82501156),
  O3 = c(0.973435152, 0.819123341, 0.811466516, 1, -0.453015636, 0.90098703, 0.978976066),
  CO = c(-0.51899618, 0.094057869, 0.098248666, -0.453015636, 1, -0.380282804, -0.448601395),
  NO2 = c(0.84799722, 0.676183904, 0.656826186, 0.90098703, -0.380282804, 1, 0.878454813),
  SO2 = c(0.995238952, 0.832033186, 0.82501156, 0.978976066, -0.448601395, 0.878454813, 1)
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



# Create a data frame
data <- data.frame(
  PM10 = c(1.0000000, 0.9994299, 0.63595399, 0.72846081, 0.6888252, 0.5576978),
  PM2.5 = c(0.9994299, 1.0000000, 0.62454166, 0.72684911, 0.6729972, 0.5404448),
  O3 = c(0.6359540, 0.6245417, 1.00000000, 0.09301851, 0.7580943, 0.8876440),
  CO = c(0.7284608, 0.7268491, 0.09301851, 1.00000000, 0.3858974, 0.1024895),
  NO2 = c(0.6888252, 0.6729972, 0.75809425, 0.38589743, 1.0000000, 0.7810918),
  SO2 = c(0.5576978, 0.5404448, 0.88764403, 0.10248946, 0.7810918, 1.0000000)
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


