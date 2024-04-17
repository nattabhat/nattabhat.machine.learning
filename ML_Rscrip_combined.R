#Loading the data
Data <- read.csv("Hotel Reservations.csv")
D <- Data
message("Data Upload Complete")

library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)
library(kernlab)
library(randomForest)
library(reshape2)
library(skimr)

message("-------------Initiating Data Cleaning-------------")

atr <- attributes(Data)
message("Type of the variable data is : ", atr[2])

str(Data)

#Duplicates
cat("Checking for Duplicate Recods\n")
cat("-----------------------------\n")
DistinctData <- Data %>% distinct()
cat("There are no duplicates\n")

#Missing Records
cat("Checking for Missing Recods\n")
cat("---------------------------\n")
naIndex <- is.na(Data)
naWhere <- which(naIndex)
cat("There are no missing records\n")

#Dimension of the data
dm <- dim(Data)
message("Training data dimension : Row - ", dm[1], " column - ", dm[2])

#Summary of Data
cat("Summary of all the attributes\n")
cat("-----------------------------\n")
summary(Data)


#Skim
cat("Summary of all the attributes\n")
cat("-----------------------------\n")
library(skimr)
skim(Data)

#Unique Value for each categorical column
cat("Distribution of Mean Plan type\n")
cat("------------------------------\n")
table(Data$type_of_meal_plan)

cat("Distribution of Room Type\n")
cat("-------------------------\n")
table(Data$room_type_reserved)

cat("Distribution of Market Segment Types\n")
cat("------------------------------------\n")
table(Data$market_segment_type)


#Percentage of target variable
percentage <- prop.table(table(Data$booking_status))*100


#Frequency and percentage of target field
cat("Distribution of Target Field\n")
cat("----------------------------\n")
cbind(frequency=table(Data$booking_status), percentage)

#Create new date column

cat("Creating a New Date Column\n")
cat("--------------------------\n")
Data <- cbind(Data[, 1:11], Arrival_date = as.Date(paste(Data$arrival_date, Data$arrival_month, Data$arrival_year, sep="-"), format="%d-%m-%Y"), Data[, 12:ncol(Data)])
cat("Column Created\n")

#Detect and remove invalid date
cat("Identifying Invalid Dates and removing it from data\n")
cat("---------------------------------------------------\n")
subset(Data, is.na(Arrival_date), c(arrival_year, arrival_month, arrival_date, Arrival_date))

#37 rows has been removed
Data <- Data[complete.cases(Data$Arrival_date), ]
cat("Invalid data removed\n")

#Changing "repeated_guest" and "required_car_parking_space" to Logical type
cat("Changing 'repeated guest' column to Logical type\n")
cat("------------------------------------------------\n")
Data$repeated_guest <- ifelse(Data$repeated_guest == 1, TRUE, FALSE)

cat("Changing 'required_car_parking_space' column to Logical type\n")
cat("------------------------------------------------------------\n")
Data$required_car_parking_space <- ifelse(Data$required_car_parking_space == 1, TRUE, FALSE)

head(select(Data, repeated_guest, required_car_parking_space))
tail(select(Data, repeated_guest, required_car_parking_space))
cat("Column Transformation Complete\n")


cat("Box Plots to determine outliers in 'lead_time' and 'avg_price_per_room'\n")
cat("-----------------------------------------------------------------------\n")

boxplot(Data[, 9], main = names(Data)[9], medcol = "red",outpch = 19, outcex = 1.5)
boxplot(Data[, 18], main = names(Data)[18], medcol = "red",outpch = 19, outcex = 1.5)


cat("List of outliers in 'lead_time'\n") 
cat("-------------------------------\n")
Outlier_Index_Leadtime <- boxplot.stats(Data$lead_time)$out
Outlier_Index_Leadtime

cat("List of outliers in 'avg_price_per_room'\n") 
cat("----------------------------------------\n")
Outlier_Index_Avgprice <- boxplot.stats(Data$avg_price_per_room)$out
Outlier_Index_Avgprice

message("-------------Data Cleaning Ended-------------")


message("-------------Initiating Exploratory Data Analysis-------------")

cat("Average Price of Room for each market segment\n")
cat("---------------------------------------------\n")
Average_Price_per_Market <- aggregate(avg_price_per_room ~ market_segment_type, data=Data, FUN=mean)
print(Average_Price_per_Market)

ggplot(Average_Price_per_Market, aes(x = market_segment_type, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink", width = 0.5) + 
  labs(x = "Market Segment", y = "Average Price (EUR)", title = "Average price by Market Segment") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)


cat("Average Price of Room for each Room type\n")
cat("----------------------------------------\n")
Average_Price_per_Roomtype <- aggregate(avg_price_per_room ~ room_type_reserved, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Roomtype)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room")

ggplot(Average_Price_per_Roomtype, aes(x = room_type_reserved, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink", width = 0.5) + 
  labs(x = "Room Type", y = "Average Price (EUR)", title = "Average price by Room Type") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)


cat("Average Price of Room for each meal plan\n")
cat("----------------------------------------\n")
Average_Price_per_Mealplan <- aggregate(avg_price_per_room ~ type_of_meal_plan, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Mealplan)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room per meal plans")

ggplot(Average_Price_per_Mealplan, aes(x = type_of_meal_plan, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink", width = 0.5) + 
  labs(x = "Meal Plan Type", y = "Average Price (EUR)", title = "Average price by Meal Plan") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)

cat("Average Price of Room on the basis of number of special requests\n")
cat("----------------------------------------------------------------\n")
Average_Price_per_Specialrequest <- aggregate(avg_price_per_room ~ no_of_special_requests, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Specialrequest)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room")

ggplot(Average_Price_per_Specialrequest, aes(x = no_of_special_requests, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink", width = 0.5) + 
  labs(x = "Number of Special Requests", y = "Average Price (EUR)", title = "Average price by Special Requests") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5) +
  scale_x_continuous(breaks = seq(0, max(Data$no_of_special_requests), 1))


#3.1 Distribution of Cancelled Booking

cat("Distribution of Canceled Bookings")
cat("---------------------------------")
ggplot(Data, aes(x = "", fill = booking_status)) +
  geom_bar(width = 1, color = "white") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Booking Status") +
  scale_fill_manual(values = c("lightskyblue", "plum")) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Percentage of Booking Status") +
  theme(plot.title = element_text(hjust = 0.5))

#3.2 Variation of Average Price per room

cat("Variation of Average Price Per Room overtime")
cat("--------------------------------------------")
ggplot(Data, aes(x = Arrival_date, y = avg_price_per_room)) +
  geom_smooth(method = "auto")+
  geom_smooth(method = "lm", color="palevioletred2")+
  labs(x = "Arrival Month", y = "Average Price (EUR)") +
  ggtitle(" Variation of Average Price Per Room over Time")+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")

#3.3 Variation of Booking count

cat("Variation of Bookings Count over Time")
cat("-------------------------------------")

ggplot(Data, aes(x = Arrival_date)) + 
  geom_bar(aes(fill = booking_status)) +
  geom_density(data = subset(Data, booking_status=="canceled"), aes(y = after_stat(count)),linewidth=0.8)+
  labs(x = "Arrival Date", y = "ReservationCount", fill = "Booking Status") +
  ggtitle("Variation of Reservations count over Time") +
  theme(legend.position = "bottom") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")

#3.4 Distribution of meal plan by cancellation status

cat("Meal Plan Types distribution by Cancellation Status")
cat("---------------------------------------------------")

ggplot(Data, aes(x = type_of_meal_plan, fill = booking_status)) +
  geom_bar(position = "dodge") +
  labs(x = "Mean Plan Type", y = "Reservation Count", fill = "Booking Status") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("lightskyblue", "plum")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Reservation Count by Mean Plan Type and Booking Status")

#3.5 Distribution of Room Types by Cancellation Status

cat("Distribution of Room Type by Cancellation Status\n")
cat("------------------------------------------------\n")
ggplot(Data, aes(x = room_type_reserved, fill = booking_status)) + 
  geom_bar(position = "dodge", color = "White") + 
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("Not_Canceled" = "lightskyblue", "Canceled" = "plum")) + 
  labs(x = "Room Type", y = "Reservation Count", fill = "Booking Status") + 
  ggtitle("Distribution of Room Type by Cancellation Status")


#3.6 Distribution of Market Segment by Cancellation Status

cat("Distribution of Market Segment by Cancellation Status\n")
cat("-----------------------------------------------------\n")
ggplot(Data, aes(x = market_segment_type, fill = booking_status)) + 
  geom_bar(position = "dodge", color="White") + 
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("Not_Canceled" = "lightskyblue", "Canceled" = "plum")) + 
  labs(x = "Market Segment", y = "Reservation Count", fill = "Booking Status") + 
  ggtitle("Distribution of Market Segment by Cancellation Status")


#3.7 Lead time by booking status

cat("Variation of Lead Time by Booking Status\n")
cat("----------------------------------------\n")

ggplot(Data, aes(x = lead_time, fill = booking_status, group = booking_status)) + 
  geom_density(alpha = 0.8) + 
  labs(x = "Lead Time (Days)", y = "Density", fill = "Booking Status") +
  ggtitle("Variation of Lead Time by Booking Status")

#3.8 Number of children and Adults

cat("Distribution of Number of Adults and Children\n")
cat("---------------------------------------------\n")

Adult_count <- Data %>% 
  count(no_of_adults) %>%
  mutate(percentage = sprintf("%.1f%%", n / sum(n) * 100))

ggplot(Data) + 
  geom_histogram(aes(x = no_of_adults), binwidth = 1, color = "white", fill = "maroon3") +
  labs(x = "Number of Adults", y = "Reservation Count") +
  ggtitle("Distribution of Number of Adults") +
  geom_text(data = Adult_count, aes(x = no_of_adults, y = n, label = percentage), vjust = -0.5)


Children_count <- Data %>% 
  count(no_of_children) %>%
  mutate(percentage = sprintf("%.1f%%", n / sum(n) * 100))

ggplot(Data) + 
  geom_histogram(aes(x = no_of_children), binwidth = 1, color = "white", fill = "lavenderblush2") +
  labs(x = "Number of Children", y = "Reservation Count") +
  ggtitle("Distribution of Number of Children") +
  geom_text(data = Children_count, aes(x = no_of_children, y = n, label = percentage), vjust = -0.5) +
  scale_x_continuous(breaks = seq(0, max(Data$no_of_children), 1))

#3.9 Percentage of Weekday and Weekend nights

cat("Distribution of Number of Weekday Nights\n")
cat("----------------------------------------\n")
ggplot(Data, aes(x = no_of_week_nights)) +
  geom_histogram(binwidth = 1, fill = "orchid1", color = "white") +
  stat_bin(binwidth = 1, geom = "text", aes(label = paste0(sprintf("%.1f", (..count../sum(..count..))*100), "%")), vjust = -0.5) +
  labs(x = "Number of Week Nights", y = "Reservation Count") + 
  ggtitle("Distribution of Number of Weekday Nights")

cat("Distribution of Number of Weekend Nights\n")
cat("----------------------------------------\n")
ggplot(Data, aes(x = no_of_weekend_nights)) +
  geom_histogram(binwidth = 1, fill = "lavender", color = "white") +
  stat_bin(binwidth = 1, geom = "text", aes(label = paste0(sprintf("%.1f", (..count../sum(..count..))*100), "%")), vjust = -0.5) +
  labs(x = "Number of Weekend Nights", y = "Reservation Count") + 
  ggtitle("Distribution of Number of Weekend Nights")

#3.10 Distribution of Special Requests

cat("Distribution of Number of Special Requests\n")
cat("------------------------------------------\n")
ggplot(Data, aes(x = no_of_special_requests)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "white") +
  stat_bin(binwidth = 1, geom = "text", aes(label = paste0(sprintf("%.1f", (..count../sum(..count..))*100), "%")), vjust = -0.5) + 
  labs(x = "Number of Special Requests", y = "Reservation Count") + 
  ggtitle("Distribution of Number of Special Requests")

#3.11 Pie-Chart of Repeated Guests

cat("Percentage of Repeated Guests\n")
cat("-----------------------------\n")  
freq_repeated_guest <- table(Data$repeated_guest)
pie_data_RG <- data.frame(values = as.numeric(freq_repeated_guest), labels = names(freq_repeated_guest))
pie_data_RG$percent <- round(100 * pie_data_RG$values / sum(pie_data_RG$values), 1)
pie_chart_RG <- ggplot(pie_data_RG, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Repeated Guest") +
  ggtitle("Percentage of Repeated Guest") +
  scale_fill_manual(values = c("lightpink2", "navajowhite")) +
  geom_text(aes(label = paste0(percent, "%")), 
            position = position_stack(vjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5))
pie_chart_RG

#3.12 Pie-chart of Request of Parking space

cat("Percentage of Guests requiring parking Space\n")
cat("--------------------------------------------\n")
freq_parking_space <- table(Data$required_car_parking_space)
pie_data_PS <- data.frame(values = as.numeric(freq_parking_space), labels = names(freq_parking_space))
pie_data_PS$percent <- round(100 * pie_data_PS$values / sum(pie_data_PS$values), 1)
pie_chart_PS <- ggplot(pie_data_PS, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Required Car Parking Space") +
  ggtitle("Percentage of Required Car Parking Space") + 
  scale_fill_manual(values = c("lightcoral", "seashell2")) +
  geom_text(aes(label = paste0(percent, "%")), 
            position = position_stack(vjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5))
pie_chart_PS

message("-------------Exploratory Data Analysis Ended-------------")


message("-------------Initiating Pre Training Steps-------------")

Data_Numeric <- Data

## Pre-processing the dataset to convert all the features to numeric value calculating ASCII values
for (i in 1:(ncol(Data_Numeric)-1)) {
  if (is.character(Data_Numeric[, i])==TRUE){
    for(j in 1:nrow(Data_Numeric)){
      ascis <- as.numeric(charToRaw(Data_Numeric[j, i]))
      Data_Numeric[ j, i] <- sum(ascis)
    }
  }
  Data_Numeric[,i] <- as.numeric(Data_Numeric[,i])
}

#Converting Target column to Binary Format
Data_Numeric$booking_status <- ifelse(Data_Numeric$booking_status == "Canceled", 0, 1)

cat("Normalizing the Dataset\n")
cat("-----------------------\n")
Data_Numeric_Norm <- Data_Numeric
Data_Numeric_Norm[,1:(ncol(Data_Numeric_Norm)-1)] <- scale(Data_Numeric_Norm[,1:(ncol(Data_Numeric_Norm) - 1)])

cat("Correlation Check\n")
cat("-----------------\n")
cor_data <- round(cor(Data_Numeric), 2)
melted_cormat <- melt(cor_data)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", high = "darkred",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  geom_text(aes(Var2, Var1, label = value),size = 2) +
  labs(x = NULL, y = NULL)

sorted_cor <- sort(cor_data[,"booking_status"], decreasing = TRUE)
sorted_cor

message("Lead_time (44%), Arrival_Date (17%) and Avg_price_per_room (14%)  are positively correlated and no of special requests (25%), market segment type (15%), and repeated guests (11%) are negatively correlated with booking status")

message("-------------Pre Training Steps Completed-------------")


message("-------------Initiating Data Training and Testing-------------")

cat("Creating Dataset with 7 Attributes\n")
cat("----------------------------------\n")
New_Data_Numeric <- Data_Numeric[,c("lead_time", "Arrival_date", "avg_price_per_room", "no_of_special_requests", "market_segment_type", "repeated_guest", "booking_status")]
New_Data_Numeric[,ncol(New_Data_Numeric)] <- as.factor(New_Data_Numeric[,ncol(New_Data_Numeric)])

New_Data_Numeric_Norm <- Data_Numeric_Norm[,c("lead_time", "Arrival_date", "avg_price_per_room", "no_of_special_requests", "market_segment_type", "repeated_guest", "booking_status")]
New_Data_Numeric_Norm[,ncol(New_Data_Numeric_Norm)] <- as.factor(New_Data_Numeric_Norm[,ncol(New_Data_Numeric_Norm)])

cat("Creating DataFrame\n")
cat("------------------\n")
df <- data.frame(matrix(ncol = 6, nrow = 4))
colnames(df) <- c("Algorithms", "Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision")
df$Algorithms <- c("SVM", "Log Regression", "K-Nearest Neighbour", "Random Forest")



#-------------------SVM----------------------------
#Creating Data Partitions
indices60 <- createDataPartition(y = New_Data_Numeric$booking_status, p = 0.6, list = FALSE)
train_data60 <- New_Data_Numeric[indices60, ]
test_data60 <- New_Data_Numeric[-indices60, ]

# Train the model on the training data
SVM_model <- ksvm(booking_status ~ ., data = train_data60, kernel = "rbfdot",  C = 7)

# Make predictions on the test data
SVM_predictions <- predict(SVM_model, test_data60[,1:ncol(test_data60)-1])
SVM_cm <- confusionMatrix(SVM_predictions, test_data60$booking_status)

df[1,"Accuracy"] <- format(round(SVM_cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
df[1,"Kappa"] <- format(round(SVM_cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
df[1,"Sensitivity"] <- format(round(SVM_cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
df[1,"Specificity"] <- format(round(SVM_cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
df[1,"Precision"] <- format(round(SVM_cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)


#---------------KNN--------------------
indices_norm70 <- createDataPartition(y = New_Data_Numeric_Norm$booking_status, p = 0.7, list = FALSE)
train_data_norm70 <- New_Data_Numeric_Norm[indices_norm70, ]
test_data_norm70 <- New_Data_Numeric_Norm[-indices_norm70, ]

control <- trainControl(method = "repeatedcv", repeats = 10)
metric <- "Accuracy"

# Train the model on the training data
KNN_model <- train(booking_status ~ ., data = train_data_norm70, method = "knn", metric = metric, trControl = control, 
                 tuneGrid = data.frame(k = 5))

# Make predictions on the test data
KNN_predictions <- predict(KNN_model, newdata = test_data_norm70[, 1:ncol(test_data_norm70) - 1])
KNN_cm <- confusionMatrix(KNN_predictions, test_data_norm70$booking_status)

df[3,"Accuracy"] <- format(round(KNN_cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
df[3,"Kappa"] <- format(round(KNN_cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
df[3,"Sensitivity"] <- format(round(KNN_cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
df[3,"Specificity"] <- format(round(KNN_cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
df[3,"Precision"] <- format(round(KNN_cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)


#---------------Random Forest--------------------
indices_norm60 <- createDataPartition(y = New_Data_Numeric_Norm$booking_status, p = 0.6, list = FALSE)
train_data_norm60 <- New_Data_Numeric_Norm[indices_norm60, ]
test_data_norm60 <- New_Data_Numeric_Norm[-indices_norm60, ]

# Specify the desired value for nodesize (minimum size of terminal nodes)
nodesize_value <- 30

# Train a Random Forest model with Nodesize of 10
RF_model <- randomForest(booking_status~., data = train_data_norm60, ntree = 100, nodesize=nodesize_value)

# Make predictions on the testing data
RF_predictions <- predict(RF_model, newdata = test_data_norm60[,1:ncol(test_data_norm60)-1])
RF_cm <- confusionMatrix(RF_predictions, test_data_norm60$booking_status)

df[4,"Accuracy"] <- format(round(RF_cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
df[4,"Kappa"] <- format(round(RF_cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
df[4,"Sensitivity"] <- format(round(RF_cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
df[4,"Specificity"] <- format(round(RF_cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
df[4,"Precision"] <- format(round(RF_cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)

#---------------Log Regression--------------------
indices_norm_log70 <- createDataPartition(y = New_Data_Numeric_Norm$booking_status, p = 0.7, list = FALSE)
train_data_norm_log70 <- New_Data_Numeric_Norm[indices_norm_log70, ]
test_data_norm_log70 <- New_Data_Numeric_Norm[-indices_norm_log70, ]


train_data_norm_log70$lead_time2 <- train_data_norm_log70$lead_time^2
train_data_norm_log70$Arrival_date2 <- train_data_norm_log70$Arrival_date^2
train_data_norm_log70$market_segment_type2 <- train_data_norm_log70$market_segment_type^2
train_data_norm_log70$avg_price_per_room2 <- train_data_norm_log70$avg_price_per_room^2
train_data_norm_log70$repeated_guest2 <- train_data_norm_log70$repeated_guest^2
train_data_norm_log70$no_of_special_requests2 <- train_data_norm_log70$no_of_special_requests^2

test_data_norm_log70$lead_time2<- test_data_norm_log70$lead_time^2
test_data_norm_log70$Arrival_date2 <- test_data_norm_log70$Arrival_date^2
test_data_norm_log70$market_segment_type2 <- test_data_norm_log70$market_segment_type^2
test_data_norm_log70$avg_price_per_room2 <- test_data_norm_log70$avg_price_per_room^2
test_data_norm_log70$repeated_guest2 <- test_data_norm_log70$repeated_guest^2
test_data_norm_log70$no_of_special_requests2 <- test_data_norm_log70$no_of_special_requests^2

LR_model <- train(booking_status~., data = train_data_norm_log70, method = "glm", family = "binomial")
LR_predictions <- predict(LR_model, newdata = test_data_norm_log70[,-7])
LR_cm <- confusionMatrix(LR_predictions, test_data_norm_log70$booking_status)

df[2,"Accuracy"] <- format(round(LR_cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2)
df[2,"Kappa"] <- format(round(LR_cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
df[2,"Sensitivity"] <- format(round(LR_cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2)
df[2,"Specificity"] <- format(round(LR_cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
df[2,"Precision"] <- format(round(LR_cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)


#Summarising Results

df[,2] <- as.numeric(df[,2])
df[,3] <- as.numeric(df[,3])
df[,4] <- as.numeric(df[,4])
df[,5] <- as.numeric(df[,5])
df[,6] <- as.numeric(df[,6])

Results <- ggplot(df, aes(x = Algorithms, y = Kappa)) + 
  geom_bar(stat = "identity", fill = "maroon3", width = 0.5) + 
  geom_text(aes(label = paste0(Kappa, "%"), y = Kappa), vjust = -0.5, size = 3.5) + 
  labs(x = "ML Algorithms", y = "Kappa", title = "Kappa for all Algorithms") +
  ylim(0, max(df$Kappa) * 1.1)
  
print(Results)
print(df)

