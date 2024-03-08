install.packages("graphics")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("caret")
install.packages("corrplot")




library(graphics)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(corrplot)


# Read in the data
df <- read.csv("F:/VisualStudio/Car_Price/CarPrice_Assignment.csv", header = TRUE, sep = ",")

head(df)
str(df)
summary(df)

#plot price according to fuel type
barplot(table(df$fueltype), xlab = "Fuel Type", ylab = "Price", main = "Price vs Fuel Type")
table(df$fueltype)

#check unique values of each categorical columns
unique(df$fueltype)             #gas, diesel
unique(df$aspiration)           # std, turbo
unique(df$carbody)              #convertible, hatchback, sedan, wagon, hardtop
unique(df$enginelocation)       # front, rear
unique(df$enginetype)           # dohc, dohcv, l, ohc, ohcf, ohcv, rotor
unique(df$cylindernumber)       # two, three, four, five, six, eight, twelve
unique(df$fuelsystem)           # 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi
unique(df$doornumber)           # two, four
unique(df$drivewheel)           # 4wd, fwd, rwd


#show the relationship between price and cylinder number using legend as fueltype
ggplot(df, aes(x = cylindernumber, y = price, fill = fueltype)) + 
geom_boxplot() + 
labs(title = "Price vs Number of Cylinder", x = "Number of Cylinders", y = "Price") + 
theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("blue", "red"))
 
#show the relationship between price and cylinder number using legend as aspiration
ggplot(df, aes(x = cylindernumber, y = price, fill = aspiration)) +
geom_boxplot() +
labs(title = "Price vs Number of Cylinder", x = "Number of Cylinders", y = "Price") +
theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("blue", "red"))



# Convert the Category column to a factor to ensure proper ordering

#check for missing values
colSums(is.na(df))


#select columns that are numeric
df_num <- df[,sapply(df, is.numeric)]   
df_num <- df_num[,-c(1,2)]              #remove car_ID and symboling columns
str(df_num)


#check correlation
corrplot(cor(df_num), method = "circle")
corrplot(cor(df_num), method = "number")

#price and engine size are highly correlated
#secondly, price and curbweight are highly correlated followed by price and horsepower


#check for outliers
boxplot(df_num)

#plot distribution of price
hist(df_num$price, xlab = "Price", ylab = "Frequency", main = "Distribution of Price")

#understand the correlation of other variables with price using one chart
par(mfrow = c(2,3))
plot(df_num$price ~ df_num$enginesize, xlab = "Engine Size", ylab = "Price", main = "Price vs Engine Size")
plot(df_num$price ~ df_num$curbweight, xlab = "Curb Weight", ylab = "Price", main = "Price vs Curb Weight")
plot(df_num$price ~ df_num$horsepower, xlab = "Horsepower", ylab = "Price", main = "Price vs Horsepower")
plot(df_num$price ~ df_num$carlength, xlab = "Car Length", ylab = "Price", main = "Price vs Car Length")
plot(df_num$price ~ df_num$carwidth, xlab = "Car Width", ylab = "Price", main = "Price vs Car Width")
plot(df_num$price ~ df_num$carheight, xlab = "Car Height", ylab = "Price", main = "Price vs Car Height")
#columns that has high correlation with price in order

#remove par settings
par(mfrow = c(1,1))




dim(df_num)

#convert categorical variables to dummy variables

# Specify the columns to one-hot encode
columns_to_encode <- c('fueltype', 'aspiration', 'doornumber', 'carbody', 'drivewheel', 'enginelocation', 'enginetype', 'cylindernumber', 'fuelsystem')
str(df)


# Assuming 'cars' is your data frame

# Convert categorical variables to numerical using one-hot encoding
cars <- model.matrix(~.+0, data = df[, columns_to_encode])
colnames(df) <- gsub(".*\\.", "", colnames(df))

head(cars)
dim(cars)

# Feature Selection
X <- df[, c('horsepower', 'enginesize', 'carlength', 'curbweight')]
y <- df$price

# Train-Test Split
set.seed(42)  # Set seed for reproducibility
split_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[split_index, ]
X_test <- X[-split_index, ]
y_train <- y[split_index]
y_test <- y[-split_index]

dim(X_train)
dim(X_test)

#Linear Rgression model
model <- lm(y_train ~ ., data = X_train)
summary(model)


# Assuming 'model' is your linear regression model and 'x_test' and 'y_test' are your test data
# Make predictions
ypred <- predict(model, newdata = X_test)

# Calculate mean squared error (MSE)
mse <- mean((y_test - ypred)^2)
print(paste("Mean Squared Error: ", mse))

# Calculate R-squared (R²)
r2 <- 1 - sum((y_test - ypred)^2) / sum((y_test - mean(y_test))^2)
print(paste("R-squared: ", r2))

# Plot actual vs predicted prices
plot(y_test, ypred, pch = 16, col = "blue", main = "Actual vs Predicted Prices", xlab = "Actual Prices", ylab = "Predicted Prices", cex.main = 3)
abline(0, 1, col = "red", lty = 2, lwd = 3)


#create a dataframe of actual and predicted values
predf <- data.frame(Actual = y_test, Predictions = ypred)
predf$Predictions <- round(predf$Predictions, 2)
head(predf)






