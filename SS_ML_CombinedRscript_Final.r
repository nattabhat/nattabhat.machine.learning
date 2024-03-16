#Loading the data
Data <- read.csv("Hotel Reservations.csv")
D <- Data
message("Data Upload Complete")

message("-------------Initiating Data Cleaning-------------")

atr <- attributes(Data)
message("Type of the variable data is : ", atr[2])

str(Data)

#Duplicates
install.packages("dplyr")
library(dplyr)
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
install.packages("skimr")
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
par(mfrow=c(1,5))
boxplot(Data[, 9], main = names(Data)[9], medcol = "red",outpch = 19, outcex = 1.5)
boxplot(Data[, 19], main = names(Data)[18], medcol = "red",outpch = 19, outcex = 1.5)


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
  geom_bar(stat = "identity", fill = "pink") + 
  labs(x = "market Segment", y = "Average Price", title = "Average price by Market Segment") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)


cat("Average Price of Room for each Room type\n")
cat("----------------------------------------\n")
Average_Price_per_Roomtype <- aggregate(avg_price_per_room ~ room_type_reserved, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Roomtype)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room")

ggplot(Average_Price_per_Roomtype, aes(x = room_type_reserved, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  labs(x = "Room Type", y = "Average Price", title = "Average price by Room Type") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)


cat("Average Price of Room for each meal plan\n")
cat("----------------------------------------\n")
Average_Price_per_Mealplan <- aggregate(avg_price_per_room ~ type_of_meal_plan, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Mealplan)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room per meal plans")

ggplot(Average_Price_per_Mealplan, aes(x = type_of_meal_plan, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  labs(x = "Meal Plan Type", y = "Average Price", title = "Average price by Meal Plan") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)

cat("Average Price of Room on the basis of number of special requests\n")
cat("----------------------------------------------------------------\n")
Average_Price_per_Specialrequest <- aggregate(avg_price_per_room ~ no_of_special_requests, data=subset(Data, market_segment_type != "Complementary"), FUN=mean)
print(Average_Price_per_Specialrequest)
message("We have removed 'Complementary' market segment from the data to showcase the correct avg. prices of room")

ggplot(Average_Price_per_Specialrequest, aes(x = no_of_special_requests, y = avg_price_per_room)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  labs(x = "Number of Special Requets", y = "Average Price", title = "Average price by Special Requests") + 
  geom_text(aes(label = round(avg_price_per_room, 2)), vjust = -0.5)




#3.1 Distribution of Cancelled Booking

cat("Distribution of Canceled Bookings")
cat("---------------------------------")
#BAR CHART
ggplot(Data, aes(x=booking_status, fill=booking_status))+
  geom_bar(color = "white")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.64) +
  labs(x="Booking status", y="Reservation Count", fill = "Booking Status")+
  scale_fill_manual(values = c("lightskyblue", "plum"))

#PIE CHART
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
  labs(x = "Arrival Month", y = "Average Price per Room") +
  ggtitle(" Variation of Average Price Per Room over Time")+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")

#3.3 Variation of Booking count

cat("Variation of Bookings Count over Time")
cat("-------------------------------------")

ggplot(Data, aes(x = Arrival_date)) + 
  geom_bar(aes(fill = booking_status)) +
  geom_density(data = subset(Data, booking_status=="canceled"), aes(y = after_stat(count)),linewidth=0.8)+
  labs(x = "Arrival Date", y = "ReservationCount", fill = "Booking Status") +
  ggtitle("Variation of Reservaions count over Time") +
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
  scale_fill_manual(values = c("Not_Canceled" = "lightskyblue", "Canceled" = "plum")) + 
  labs(x = "Room Type", y = "Reservation Count", fill = "Booking Status") + 
  ggtitle("Distribution of Room Type by Cancellation Status") + 
  geom_text(stat='count', aes(label=after_stat(count)),position=position_dodge(width = 0.9), vjust = -0.5, size = 3.0)


#3.6 Distribution of Market Segment by Cancellation Status

cat("Distribution of Market Segment by Cancellation Status\n")
cat("-----------------------------------------------------\n")
ggplot(Data, aes(x = market_segment_type, fill = booking_status)) + 
  geom_bar(position = "dodge", color="White") + 
  scale_fill_manual(values = c("Not_Canceled" = "lightskyblue", "Canceled" = "plum")) + 
  labs(x = "Market Segment", y = "Reservation Count", fill = "Booking Status") + 
  ggtitle("Distribution of Market Segment by Cancellation Status") + 
  geom_text(stat='count', aes(label=after_stat(count)),position=position_dodge(width = 0.9), vjust = -0.5, size = 3.3)


#3.7 Lead time by booking status

cat("Variation of Lead Time by Booking Status\n")
cat("----------------------------------------\n")

ggplot(Data, aes(x = lead_time, fill = booking_status, group = booking_status)) + 
  geom_density(alpha = 0.8) + 
  labs(x = "Lead Time", y = "Density", fill = "Booking Status") +
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
  geom_text(data = Children_count, aes(x = no_of_children, y = n, label = percentage), vjust = -0.5)

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
  labs(x = "Number of Weekend Nights", y = "Reservation Count")
ggtitle("Distribution of Number of Weekend Nights")

#3.10 Distribution of Special Requests

cat("Distribution of Number of Special Requests\n")
cat("------------------------------------------\n")
ggplot(Data, aes(x = no_of_special_requests)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "white") +
  stat_bin(binwidth = 1, geom = "text", aes(label = paste0(sprintf("%.1f", (..count../sum(..count..))*100), "%")), vjust = -0.5) + 
  labs(x = "Number of Special Requests", y = "Reservation Count")
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
  scale_fill_manual(values = c("lightpink2", "navajowhite")) +
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

# after converting the whole dataset (except last column) to numeric, converting the last column to factor to be used as class label
Data_Numeric[,ncol(Data_Numeric)] <- as.factor(Data_Numeric[,ncol(Data_Numeric)])


message("-------------Correlation check-------------")
#Correlation check
Data_Numeric$booking_status <- ifelse(Data_Numeric$booking_status == "Canceled", 0, 1)

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

#******************************************************************************
#****(C5.0)Model with ASCII Encoding including all features(WON) ***
#******************************************************************************
message("---(C5.0)Model with ASCII Encoding including all features(Without Normalization)--")
#C5.0 Decision Tree Classification Model without using data normalization
library(C50)

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)
# creating a blank data frame to store performance metrics scores
   pf = data.frame(matrix(
   vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                    c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc <- 0 # pfc-performance frame counter
 
  FD <- 5
  
  Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
  for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~., data = Training_Set) # for C5.0 model creation
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
   message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                         nsmall = 2), "\t\t ",
               format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
               format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
               format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
               format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
               format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
    pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
 }
message("---------------------------------------------------------------------------------------------------------------------------------")

#
#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(5 Folds) (WON)**
#******************************************************************************
message("---(C5.0)Model with ASCII Encoding includingselected features(Without Normalization)--")
# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)
# creating a blank data frame to store performance metrics scores
pf1 = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 5

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set) # for C5.0 model creation
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pf1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pf1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pf1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pf1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pf1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pf1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(5 Folds) (WN)**
#******************************************************************************
message("---(C5.0)Model with ASCII Encoding including selected features(With Normalization)--")
# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#Converting the last column to factor to be used as class label
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfN = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 5

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set) # for C5.0 model creation
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfN[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfN[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfN[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfN[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfN[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfN[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#*************************************************************************************
#**(C5.0)Model-Boosting(3T) with ASCII Encoding having selected features(5 Folds)(WN)**
#*************************************************************************************
message("---(C5.0)Model-Boosting(3 Trials) with all features(With Normalization)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#Converting the last column to factor to be used as class label
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfN1 = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 5

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=3) # for C5.0 model creation with trials
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfN1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfN1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfN1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfN1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfN1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfN1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#*************************************************************************************
#**(C5.0)Model-Boosting(2T) with ASCII Encoding having selected features(5 Folds)(WN)**
#*************************************************************************************
message("---(C5.0)Model-Boosting(2 Trials) with all features(With Normalization)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#Converting the last column to factor to be used as class label
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfN2 = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 5

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=2) # for C5.0 model creation with trials
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfN2[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfN2[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfN2[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfN2[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfN2[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfN2[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#*************************************************************************************
#**(C5.0)Model-Boosting(5T) with ASCII Encoding having selected features(5 Folds)(WN)**
#*************************************************************************************
message("---(C5.0)Model-Boosting(5 Trials) with all features(With Normalization)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#Converting the last column to factor to be used as class label
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfN3 = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 5

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=5) # for C5.0 model creation with trials
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfN3[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfN3[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfN3[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfN3[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfN3[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfN3[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(10 Folds) (W0N)**
#******************************************************************************
message("---(C5.0)Model-(10 Folds) with selected features(Without Normalization)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

# creating a blank data frame to store performance metrics scores
pfT = data.frame(matrix(
  vector(), 10, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5","Fold-6","Fold-7","Fold-8","Fold-9","Fold-10"), 
                                 c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 10

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set) # for C5.0 model creation
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfT[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfT[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfT[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfT[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfT[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfT[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(10 Folds) (WN)**
#******************************************************************************
message("---(C5.0)Model-(10 Folds) with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfT1 = data.frame(matrix(
  vector(), 10, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5","Fold-6","Fold-7","Fold-8","Fold-9","Fold-10"), 
                                 c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 10

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set) # for C5.0 model creation
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfT1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfT1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfT1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfT1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfT1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfT1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#**********************************************************************************************
#**(C5.0)Model (Boosting-1 Trial) with ASCII Encoding having selected features(10 Folds) (WN)**
#**********************************************************************************************
message("---(C5.0)Model-(1 Trial) with selected features(With Normalization-10Folds)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfT2 = data.frame(matrix(
  vector(), 10, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5","Fold-6","Fold-7","Fold-8","Fold-9","Fold-10"), 
                                 c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 10

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=1) # for C5.0 model with 1 trial
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfT2[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfT2[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfT2[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfT2[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfT2[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfT2[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#**********************************************************************************************
#**(C5.0)Model (Boosting-3 Trials) with ASCII Encoding having selected features(10 Folds) (WN)**
#**********************************************************************************************
message("---(C5.0)Model-(3 Trials) with selected features(With Normalization-10Folds)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfT3 = data.frame(matrix(
  vector(), 10, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5","Fold-6","Fold-7","Fold-8","Fold-9","Fold-10"), 
                                 c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 10

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=3) # for C5.0 model with 3 trials
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfT3[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfT3[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfT3[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfT3[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfT3[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfT3[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#**********************************************************************************************
#**(C5.0)Model (Boosting-5 Trials) with ASCII Encoding having selected features(10 Folds) (WN)**
#**********************************************************************************************
message("---(C5.0)Model-(5 Trials) with selected features(With Normalization-10Folds)--")

# header line of on-screen performance metrics
message("\t Folds \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)])

# creating a blank data frame to store performance metrics scores
pfT4 = data.frame(matrix(
  vector(), 10, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5","Fold-6","Fold-7","Fold-8","Fold-9","Fold-10"), 
                                 c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter

FD <- 10

Folds <- createFolds(Data_Numeric$booking_status, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){
  pfc <- pfc+1
  Held_Out_Indices = Folds[[i]]
  Training_Set = Data_Numeric[Held_Out_Indices,]
  Testing_Set = Data_Numeric[-Held_Out_Indices,]
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+booking_status, data = Training_Set, trials=5) # for C5.0 model with 5 trials
  
  Predictions <- predict(TrainedClassifier, newdata=Testing_Set)
  cm <- confusionMatrix(Testing_Set$booking_status, Predictions)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",i, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2), 
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  pfT4[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  pfT4[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pfT4[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  pfT4[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  pfT4[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  pfT4[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WON)**
#******************************************************************************
message("-(C5.0)Model(Different Test & Train %) -with selected features(Without Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

# creating a blank data frame to store performance metrics scores
PF = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
# c5.0 Decision Tree Model Without Normalization 

  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set) # for C5.0 model creation
  TrainedClassifier
# Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])

# Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PF[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PF[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PF[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PF[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PF[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PF[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#******************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)**
#******************************************************************************
message("-(C5.0)Model(Different Test & Train %) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD1 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model Without Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set) # for C5.0 model creation
  TrainedClassifier
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#********************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-1 Trial**
#********************************************************************************
message("-(C5.0)Model(1 Trial-Rloop) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD2 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model With Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set, trials=1) # for C5.0 model creation with 1 trial
  summary(TrainedClassifier)
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD2[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD2[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD2[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD2[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD2[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD2[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#********************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-3 Trials**
#********************************************************************************
message("-(C5.0)Model(3 Trials-Rloop) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD3 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model With Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set, trials=3) # for C5.0 model creation with 3 trials
  summary(TrainedClassifier)
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD3[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD3[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD3[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD3[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD3[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD3[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#********************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-5 Trials**
#********************************************************************************
message("-(C5.0)Model(5 Trials-Rloop) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD4 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model With Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set, trials=5) # for C5.0 model creation with 5 trials
  summary(TrainedClassifier)
  #Summary of TrainedClassifier
  summary(TrainedClassifier)
  
  # Plotting of TrainedClassifier
  #plot(TrainedClassifier)
  
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD4[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD4[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD4[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD4[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD4[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD4[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#********************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-10 Trials**
#********************************************************************************
message("-(C5.0)Model(10 Trials-Rloop) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD6 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model With Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set, trials=10) # for C5.0 model creation with 10 trials
  TrainedClassifier# To Check the average size of tree
  #Summary of TrainedClassifier
  summary(TrainedClassifier)# To check Error Rates and Model Performance 
  
  # Plotting of TrainedClassifier
  #plot(TrainedClassifier)
  
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  #********************Installation of Library gmodels************************
  #library(gmodels)
  CrossTable(testing_data$booking_status, Predicted_outcomes, prop.chisq = FALSE, prop.c = FALSE,prop.r = FALSE, dnn = c('Actual Booking Status', 'Predicted Booking Status'))
  
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD6[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD6[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD6[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD6[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD6[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD6[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#********************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-7 Trials**
#********************************************************************************
message("-(C5.0)Model(7 Trials-Rloop) -with selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD5 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  # c5.0 Decision Tree Model With Normalization 
  
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = Training_Set, trials=7) # for C5.0 model creation with 7 trials
  
  #Summary of TrainedClassifier
  summary(TrainedClassifier)
  
  # Plotting of TrainedClassifier
  #plot(TrainedClassifier)
  
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD5[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD5[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD5[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD5[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD5[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD5[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#************************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-Cost Matrix**
#************************************************************************************
message("-(C5.0)Model(With Cost Matrix-Rloop) having selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD7 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset

#Creation of Cost Matrix
  cost_mat <- matrix(c(0, 1, 2, 0), nrow = 2)
  rownames(cost_mat) <- colnames(cost_mat) <- c("0", "1")
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, costs=cost_mat)
  
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  PFD7[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  PFD7[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  PFD7[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  PFD7[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  PFD7[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  PFD7[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("---------------------------------------------------------------------------------------------------------------------------------")

#************************************************************************************
#**(C5.0)Model with ASCII Encoding having selected features(RLoop) (WN)-2ndCost Matrix**
#************************************************************************************
message("-(C5.0)Model(2nd Cost Matrix-Rloop) having selected features(With Normalization)--")

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t 
Precision% \t Recall%")

#header line of TXT file's performance metrics 
cat("---------------------------------------------------------------------------------
------------------------------------------------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep ="\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append =TRUE) # Apply cat & append
cat("------", file = "C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = "\n", append = TRUE)

#Converting last column as factor
Data_Numeric$booking_status <- as.factor(Data_Numeric$booking_status)

#Normalization of Data
Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 

# creating a blank data frame to store performance metrics scores
PFD8 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70","TR-80","TR-90"), 
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc <- 0 # pfc-performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # Sequence Creation to represent training data ratio

#loop to vary training data and check performance of c5.0 Performance
for (t in training_data_percentages){
  pfc <- pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p=t, list=FALSE) # index of training data
  training_data <- Data_Numeric[indx_Partition,] # training dataset
  testing_data <- Data_Numeric[-indx_Partition,] # testing dataset
  
  #Creation of Cost Matrix
  cost_mat <- matrix(c(0, 5, 5, 0), nrow = 2)
  rownames(cost_mat) <- colnames(cost_mat) <- c("0", "1")
  TrainedClassifier <- C5.0(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, costs=cost_mat)
  
  # Prediction on Test Data
  Predicted_outcomes <- predict(TrainedClassifier, newdata =testing_data[,1:ncol(testing_data)-1])
  
  # Confusion Matrix 
  cm <- confusionMatrix(testing_data$booking_status, Predicted_outcomes)
  print(cm)# You can access performance metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
}
message("---------------------------------------------------------------------------------------------------------------------------------")


#************************************************************************************
#**********************Random Forest Model with all features (WON)*********************
#************************************************************************************
message("-----Random Forest Model having all the features (Without Normalization)----")
# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)

message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")

# creating a blank data frame to store performance metrics scores
RF = data.frame(matrix(
   vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc<-0 # performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility

for (t in training_data_percentages) {
  pfc<-pfc+1
indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
training_data <- Data_Numeric[indx_Partition, ]
testing_data <- Data_Numeric[-indx_Partition, ]

# Train a Random Forest model
RFModel <- randomForest(booking_status ~ ., data = training_data, ntree = 100)

# Make predictions on the testing data
predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])

# Evaluate the model
cm <- confusionMatrix(testing_data$booking_status, predictions)
print(cm)


# below message() function shows the performance metrics on-screen
message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))

# below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
    format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
    format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
    format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
    format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
    format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
    format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)


# --------- assigning the performance metrics to the dataframe created ----------------
RF[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
RF[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
RF[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
RF[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
RF[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
RF[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}

#************************************************************************************
#**********************Random Tree Model with selected features(WON)*****************
#************************************************************************************
message("-----Random Forest Model having selected features (Without Normalization)----")


message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")

# creating a blank data frame to store performance metrics scores
RF1 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)

pfc<-0 # performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility

for (t in training_data_percentages){
  pfc<-pfc+1
  indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
  training_data <- Data_Numeric[indx_Partition, ]
  testing_data <- Data_Numeric[-indx_Partition, ]
  
  # Train a Random Forest model
  RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100)# Model with Selected features
  
  RFModel
  # Make predictions on the testing data
  predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
  
  # Evaluate the model
  cm <- confusionMatrix(testing_data$booking_status, predictions)
  print(cm)
  
  
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
  
  
  # --------- assigning the performance metrics to the dataframe created ----------------
  RF1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
  RF1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  RF1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
  RF1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
  RF1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
  RF1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
} 
  #************************************************************************************
  #**********************Random Tree Model with selected features(WN)*****************
  #************************************************************************************
message("-----Random Forest Model having selected features (With Normalization)----")
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # creating a blank data frame to store performance metrics scores
  RF2 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    # Train a Random Forest model
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RF2[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RF2[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RF2[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RF2[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RF2[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RF2[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }

  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI(5 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(5 Terminal Nodes--")
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value <- 5
  
  # creating a blank data frame to store performance metrics scores
  RFTN = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    # Train a Random Forest model with Nodesize of 5
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100, nodesize=nodesize_value)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }

  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI(10 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(10 Terminal Nodes--")
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value1 <- 10
  
  # creating a blank data frame to store performance metrics scores
  RFTN1 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    # Train a Random Forest model with Nodesize of 10
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100, nodesize=nodesize_value1)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN1[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN1[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN1[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN1[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN1[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN1[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }

  
  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI(15 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(15 Terminal Nodes--")
  
  # Install and load the randomForest package
  #install.packages("randomForest")
 # library(randomForest)
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value2 <- 15
  
  # creating a blank data frame to store performance metrics scores
  RFTN2 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    #Checking the Proportion of Data Split
    prop.table(table(Data_Numeric$booking_status))
    prop.table(table(training_data$booking_status))
    prop.table(table(testing_data$booking_status))
    
    # Train a Random Forest model with Nodesize of 15
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+avg_price_per_room+no_of_special_requests, data = training_data, ntree = 100, nodesize=nodesize_value2)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN2[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN2[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN2[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN2[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN2[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN2[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }
  
  
  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI (20 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(20 Terminal Nodes--")
  
  # Install and load the randomForest package
  #install.packages("randomForest")
  # library(randomForest)
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value3 <- 20
  
  # creating a blank data frame to store performance metrics scores
  RFTN3 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    #Checking the Proportion of Data Split
    prop.table(table(Data_Numeric$booking_status))
    prop.table(table(training_data$booking_status))
    prop.table(table(testing_data$booking_status))
    
    
    # Train a Random Forest model with Nodesize of 15
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100, nodesize=nodesize_value3)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN3[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN3[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN3[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN3[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN3[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN3[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }
  
  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI(25 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(25 Terminal Nodes--")
  
  # Install and load the randomForest package
  #install.packages("randomForest")
  # library(randomForest)
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value4 <- 25
  
  # creating a blank data frame to store performance metrics scores
  RFTN4 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    #Checking the Proportion of Data Split
    prop.table(table(Data_Numeric$booking_status))
    prop.table(table(training_data$booking_status))
    prop.table(table(testing_data$booking_status))
    
    
    # Train a Random Forest model with Nodesize of 15
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100, nodesize=nodesize_value4)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN4[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN4[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN4[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN4[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN4[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN4[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }

  
  #*****************************************************************************
  #****Random Tree Model with selected features(WN) and PFI (30 Terminal Nodes)****
  #*****************************************************************************
  message("--Random Forest Model having selected features and PFI(30 Terminal Nodes--")
  
  # Install and load the randomForest package
  #install.packages("randomForest")
  # library(randomForest)
  
  message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
  
  #Data Normalization
  Data_Numeric[,1:(ncol(Data_Numeric)-1)] <- scale(Data_Numeric[,1:(ncol(Data_Numeric)-1)]) 
  
  # Specify the desired value for nodesize (minimum size of terminal nodes)
  nodesize_value5 <- 30
  
  # creating a blank data frame to store performance metrics scores
  RFTN5 = data.frame(matrix(
    vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),stringsAsFactors=F)
  
  pfc<-0 # performance frame counter
  training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)# Sequence for Training Data Ratio
  
  # Split the data into training and testing sets
  set.seed(123)  # Set seed for reproducibility
  
  for (t in training_data_percentages) {
    pfc<-pfc+1
    indx_Partition <- createDataPartition(Data_Numeric$booking_status, p = t, list = FALSE)
    training_data <- Data_Numeric[indx_Partition, ]
    testing_data <- Data_Numeric[-indx_Partition, ]
    
    #Checking the Proportion of Data Split
    prop.table(table(Data_Numeric$booking_status))
    prop.table(table(training_data$booking_status))
    prop.table(table(testing_data$booking_status))
    
    
    # Train a Random Forest model with Nodesize of 15
    RFModel <- randomForest(booking_status~lead_time+Arrival_date+market_segment_type+repeated_guest+no_of_special_requests+avg_price_per_room, data = training_data, ntree = 100, nodesize=nodesize_value5)# Model with Selected features
    
    # Make predictions on the testing data
    predictions <- predict(RFModel, newdata = testing_data[,1:ncol(testing_data)-1])
    
    # Evaluate the model
    cm <- confusionMatrix(testing_data$booking_status, predictions)
    print(cm)
    
    
    # below message() function shows the performance metrics on-screen
    message("\t ", t*100,"\t\t", (1-t)*100,"\t\t ", 
            format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
            format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
    
    # below cat() function writes or prints the performance metrics in TXT file named Results.txt, check your code folder 
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file ="C:/Users/sagar/OneDrive - University of Surrey/Machine Learning/Assignment/Results.txt", sep = " ", append = TRUE)
    
    
    # --------- assigning the performance metrics to the dataframe created ----------------
    RFTN5[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =  2)
    RFTN5[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
    RFTN5[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),  nsmall = 2)
    RFTN5[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2)
    RFTN5[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2)
    RFTN5[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
  }
 