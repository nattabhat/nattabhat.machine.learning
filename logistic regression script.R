#Loading the data
Data <- read.csv("Hotel Reservations.csv")
D <- Data
message("Data Upload Complete")

message("-------------Initiating Data Cleaning-------------")

atr <- attributes(Data)
message("Type of the variable data is : ", atr[2])

str(Data)

#Duplicates
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

Data_Numeric$booking_status <- ifelse(Data_Numeric$booking_status == "Canceled", 0, 1)


# after converting the whole dataset (except last column) to numeric, converting the last column to factor to be used as class label
Data_Numeric[,ncol(Data_Numeric)] <- as.factor(Data_Numeric[,ncol(Data_Numeric)])


############################ Logistic Regression ########################################

library(caret)

training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)

# Logistic Regression Model with all fields

pf = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0

for (t in training_data_percentages) {
  
  pfc <- pfc+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  lr_model1 <- train(booking_status ~ ., data = train.data, method = "glm", family = "binomial")
  lr_predictions1 <- predict(lr_model1, newdata = test.data)
  cm1 <- confusionMatrix(lr_predictions1, test.data$booking_status)
  #print(cm1)
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm1[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm1[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm1[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm1[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm1[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm1[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf[pfc, "Accuracy"] <- format(round(cm1[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf[pfc, "Kappa"] <- format(round(cm1[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf[pfc, "Sensitivity"] <- format(round(cm1[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf[pfc, "Specificity"] <- format(round(cm1[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf[pfc, "Precision"] <- format(round(cm1[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf[pfc, "Recall"] <- format(round(cm1[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
----------------------------------------------------")
}



#Logistic Regression 7 fields

pf3 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc3 <- 0

for (t in training_data_percentages) {
  pfc3 <- pfc3+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  lr_model3 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests, data = train.data, method = "glm", family = "binomial")
  lr_predictions3 <- predict(lr_model3, newdata = test.data)
  cm3 <- confusionMatrix(lr_predictions3, test.data$booking_status)
  #print(cm3)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm3[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm3[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm3[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm3[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm3[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm3[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf3[pfc3, "Accuracy"] <- format(round(cm3[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf3[pfc3, "Kappa"] <- format(round(cm3[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf3[pfc3, "Sensitivity"] <- format(round(cm3[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf3[pfc3, "Specificity"] <- format(round(cm3[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf3[pfc3, "Precision"] <- format(round(cm3[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf3[pfc3, "Recall"] <- format(round(cm3[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
----------------------------------------------------")
}


#Logistic Regression 7 fields lead_time non-linear

pf4 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc4 <- 0

for (t in training_data_percentages) {
  pfc4 <- pfc4+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^2
  test.data_nl$lead_time2 <- test.data_nl$lead_time^2
  
  lr_model4 <- train(booking_status ~ lead_time + lead_time2 + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests, data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions4 <- predict(lr_model4, newdata = test.data_nl)
  cm4 <- confusionMatrix(lr_predictions4, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm4[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm4[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm4[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm4[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm4[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm4[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf4[pfc4, "Accuracy"] <- format(round(cm4[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf4[pfc4, "Kappa"] <- format(round(cm4[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf4[pfc4, "Sensitivity"] <- format(round(cm4[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf4[pfc4, "Specificity"] <- format(round(cm4[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf4[pfc4, "Precision"] <- format(round(cm4[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf4[pfc4, "Recall"] <- format(round(cm4[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields lead_time & Arrival_date non-linear

pf5 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc5 <- 0

for (t in training_data_percentages) {
  pfc5 <- pfc5+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^2
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^2
  test.data_nl$lead_time2 <- test.data_nl$lead_time^2
  test.data_nl$Arrival_date2  <- test.data_nl$Arrival_date^2
  
  lr_model5 <- train(booking_status ~ lead_time +lead_time2 + Arrival_date + Arrival_date2 + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests, data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions5 <- predict(lr_model5, newdata = test.data_nl)
  cm5 <- confusionMatrix(lr_predictions5, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm5[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm5[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm5[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm5[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm5[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm5[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf5[pfc5, "Accuracy"] <- format(round(cm5[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf5[pfc5, "Kappa"] <- format(round(cm5[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf5[pfc5, "Sensitivity"] <- format(round(cm5[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf5[pfc5, "Specificity"] <- format(round(cm5[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf5[pfc5, "Precision"] <- format(round(cm5[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf5[pfc5, "Recall"] <- format(round(cm5[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

# Logistic Regression 7 fields all non-linear

pf6 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc6 <- 0

for (t in training_data_percentages) {
  pfc6 <- pfc6+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^2
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^2
  train.data_nl$market_segment_type2 <- train.data_nl$market_segment_type^2
  train.data_nl$avg_price_per_room2 <- train.data_nl$avg_price_per_room^2
  train.data_nl$repeated_guest2 <- train.data_nl$repeated_guest^2
  train.data_nl$no_of_special_requests2 <- train.data_nl$no_of_special_requests^2
  
  test.data_nl$lead_time2<- test.data_nl$lead_time^2
  test.data_nl$Arrival_date2 <- test.data_nl$Arrival_date^2
  test.data_nl$market_segment_type2 <- test.data_nl$market_segment_type^2
  test.data_nl$avg_price_per_room2 <- test.data_nl$avg_price_per_room^2
  test.data_nl$repeated_guest2 <- test.data_nl$repeated_guest^2
  test.data_nl$no_of_special_requests2 <- test.data_nl$no_of_special_requests^2
  
  lr_model6 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests + lead_time2 + Arrival_date2 + market_segment_type2 + avg_price_per_room2 + repeated_guest2 + no_of_special_requests2 , data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions6 <- predict(lr_model6, newdata = test.data_nl)
  cm6 <- confusionMatrix(lr_predictions6, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm6[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm6[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm6[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm6[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm6[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm6[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf6[pfc6, "Accuracy"] <- format(round(cm6[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf6[pfc6, "Kappa"] <- format(round(cm6[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf6[pfc6, "Sensitivity"] <- format(round(cm6[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf6[pfc6, "Specificity"] <- format(round(cm6[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf6[pfc6, "Precision"] <- format(round(cm6[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf6[pfc6, "Recall"] <- format(round(cm6[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with Normalization
Data_Numeric3 <- Data_Numeric

Data_Numeric3[,1:(ncol(Data_Numeric3)-1)] <- scale(Data_Numeric3[,1:(ncol(Data_Numeric3)-1)])

pf7 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc7 <- 0

for (t in training_data_percentages) {
  
  pfc7 <- pfc7+1
  set.seed(123)
  training.samples <- Data_Numeric3$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric3[training.samples, ]
  test.data <- Data_Numeric3[-training.samples, ]
  
  lr_model7 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests, data = train.data, method = "glm", family = "binomial")
  lr_predictions7 <- predict(lr_model7, newdata = test.data)
  cm7 <- confusionMatrix(lr_predictions7, test.data$booking_status)
  #print(cm7)
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm7[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm7[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm7[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm7[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm7[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm7[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf7[pfc7, "Accuracy"] <- format(round(cm7[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf7[pfc7, "Kappa"] <- format(round(cm7[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf7[pfc7, "Sensitivity"] <- format(round(cm7[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf7[pfc7, "Specificity"] <- format(round(cm7[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf7[pfc7, "Precision"] <- format(round(cm7[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf7[pfc7, "Recall"] <- format(round(cm7[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
----------------------------------------------------")
}


#Logistic Regression with 7 fields with interaction of lead_time and Arrival_date
pf8 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc8 <- 0

for (t in training_data_percentages) {
  pfc8 <- pfc8+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model8 <- train(booking_status ~ market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests + lead_time*Arrival_date, data = train.data, method = "glm", family = "binomial")
  lr_predictions8 <- predict(lr_model8, newdata = test.data)
  cm8 <- confusionMatrix(lr_predictions8, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm8[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm8[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm8[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm8[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm8[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm8[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf8[pfc8, "Accuracy"] <- format(round(cm8[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf8[pfc8, "Kappa"] <- format(round(cm8[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf8[pfc8, "Sensitivity"] <- format(round(cm8[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf8[pfc8, "Specificity"] <- format(round(cm8[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf8[pfc8, "Precision"] <- format(round(cm8[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf8[pfc8, "Recall"] <- format(round(cm8[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of lead_time, Arrival_date, and repeated_guest.
pf9 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc9 <- 0

for (t in training_data_percentages) {
  pfc9 <- pfc9+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model9 <- train(booking_status ~ market_segment_type + avg_price_per_room + no_of_special_requests + lead_time*Arrival_date*repeated_guest, data = train.data, method = "glm", family = "binomial")
  lr_predictions9 <- predict(lr_model9, newdata = test.data)
  cm9 <- confusionMatrix(lr_predictions9, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm9[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm9[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm9[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm9[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm9[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm9[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf9[pfc9, "Accuracy"] <- format(round(cm9[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf9[pfc9, "Kappa"] <- format(round(cm9[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf9[pfc9, "Sensitivity"] <- format(round(cm9[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf9[pfc9, "Specificity"] <- format(round(cm9[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf9[pfc9, "Precision"] <- format(round(cm9[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf9[pfc9, "Recall"] <- format(round(cm9[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}


#Logistic Regression 7 fields with interaction of no_of_special_requests and avg_room_price
pf10 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc10 <- 0

for (t in training_data_percentages) {
  pfc10 <- pfc10+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model10 <- train(booking_status ~ market_segment_type + avg_price_per_room*no_of_special_requests + lead_time + Arrival_date + repeated_guest, data = train.data, method = "glm", family = "binomial")
  lr_predictions10 <- predict(lr_model10, newdata = test.data)
  cm10 <- confusionMatrix(lr_predictions10, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm10[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm10[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm10[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm10[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm10[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm10[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf10[pfc10, "Accuracy"] <- format(round(cm10[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf10[pfc10, "Kappa"] <- format(round(cm10[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf10[pfc10, "Sensitivity"] <- format(round(cm10[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf10[pfc10, "Specificity"] <- format(round(cm10[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf10[pfc10, "Precision"] <- format(round(cm10[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf10[pfc10, "Recall"] <- format(round(cm10[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of all fields

pf11 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc11 <- 0

for (t in training_data_percentages) {
  pfc11 <- pfc11+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model11 <- train(booking_status ~ market_segment_type*avg_price_per_room*no_of_special_requests*lead_time*Arrival_date*repeated_guest, data = train.data, method = "glm", family = "binomial")
  lr_predictions11 <- predict(lr_model11, newdata = test.data)
  cm11 <- confusionMatrix(lr_predictions11, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm11[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm11[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm11[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm11[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm11[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm11[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf11[pfc11, "Accuracy"] <- format(round(cm11[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf11[pfc11, "Kappa"] <- format(round(cm11[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf11[pfc11, "Sensitivity"] <- format(round(cm11[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf11[pfc11, "Specificity"] <- format(round(cm11[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf11[pfc11, "Precision"] <- format(round(cm11[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf11[pfc11, "Recall"] <- format(round(cm11[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of lead_time and repeated_guest

pf12 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc12 <- 0

for (t in training_data_percentages) {
  pfc12 <- pfc12+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model12 <- train(booking_status ~ market_segment_type*repeated_guest + avg_price_per_room + no_of_special_requests + lead_time + Arrival_date, data = train.data, method = "glm", family = "binomial")
  lr_predictions12 <- predict(lr_model12, newdata = test.data)
  cm12 <- confusionMatrix(lr_predictions12, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm12[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm12[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm12[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm12[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm12[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm12[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf12[pfc12, "Accuracy"] <- format(round(cm12[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf12[pfc12, "Kappa"] <- format(round(cm12[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf12[pfc12, "Sensitivity"] <- format(round(cm12[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf12[pfc12, "Specificity"] <- format(round(cm12[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf12[pfc12, "Precision"] <- format(round(cm12[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf12[pfc12, "Recall"] <- format(round(cm12[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of avg_price_per_room and no_of_special_requests

pf13 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc13 <- 0

for (t in training_data_percentages) {
  pfc13 <- pfc13+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model13 <- train(booking_status ~ market_segment_type + repeated_guest + avg_price_per_room*no_of_special_requests + lead_time + Arrival_date, data = train.data, method = "glm", family = "binomial")
  lr_predictions13 <- predict(lr_model13, newdata = test.data)
  cm13 <- confusionMatrix(lr_predictions13, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm13[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm13[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm13[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm13[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm13[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm13[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf13[pfc13, "Accuracy"] <- format(round(cm13[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf13[pfc13, "Kappa"] <- format(round(cm13[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf13[pfc13, "Sensitivity"] <- format(round(cm13[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf13[pfc13, "Specificity"] <- format(round(cm13[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf13[pfc13, "Precision"] <- format(round(cm13[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf13[pfc13, "Recall"] <- format(round(cm13
                                        [["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of lead_time, Arrival_date, and avg_price_per_room

pf14 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc14 <- 0

for (t in training_data_percentages) {
  pfc14 <- pfc14+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model14 <- train(booking_status ~ market_segment_type + repeated_guest + no_of_special_requests + lead_time*Arrival_date*avg_price_per_room , data = train.data, method = "glm", family = "binomial")
  lr_predictions14 <- predict(lr_model14, newdata = test.data)
  cm14 <- confusionMatrix(lr_predictions14, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm14[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm14[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm14[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm14[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm14[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm14[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf14[pfc14, "Accuracy"] <- format(round(cm14[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf14[pfc14, "Kappa"] <- format(round(cm14[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf14[pfc14, "Sensitivity"] <- format(round(cm14[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf14[pfc14, "Specificity"] <- format(round(cm14[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf14[pfc14, "Precision"] <- format(round(cm14[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf14[pfc14, "Recall"] <- format(round(cm14
                                        [["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of market_segment_type, repeated_guest, lead_time, and avg_price_per_room

pf15 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc15 <- 0

for (t in training_data_percentages) {
  pfc15 <- pfc15+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model15 <- train(booking_status ~ market_segment_type*repeated_guest*lead_time*avg_price_per_room + no_of_special_requests + Arrival_date, data = train.data, method = "glm", family = "binomial")
  lr_predictions15 <- predict(lr_model15, newdata = test.data)
  cm15 <- confusionMatrix(lr_predictions15, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm15[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm15[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm15[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm15[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm15[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm15[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf15[pfc15, "Accuracy"] <- format(round(cm15[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf15[pfc15, "Kappa"] <- format(round(cm15[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf15[pfc15, "Sensitivity"] <- format(round(cm15[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf15[pfc15, "Specificity"] <- format(round(cm15[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf15[pfc15, "Precision"] <- format(round(cm15[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf15[pfc15, "Recall"] <- format(round(cm15
                                        [["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Logistic Regression 7 fields with interaction of market_segment_type*repeated_guest*lead_time*avg_price_per_room + no_of_special_requests*Arrival_date


pf16 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc16 <- 0

for (t in training_data_percentages) {
  pfc16 <- pfc16+1
  set.seed(123)
  training.samples <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data <- Data_Numeric[training.samples, ]
  test.data <- Data_Numeric[-training.samples, ]
  
  
  lr_model16 <- train(booking_status ~ market_segment_type*repeated_guest*lead_time*avg_price_per_room + no_of_special_requests*Arrival_date, data = train.data, method = "glm", family = "binomial")
  lr_predictions16 <- predict(lr_model16, newdata = test.data)
  cm16 <- confusionMatrix(lr_predictions16, test.data$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm16[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm16[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm16[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm16[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm16[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm16[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf16[pfc16, "Accuracy"] <- format(round(cm16[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf16[pfc16, "Kappa"] <- format(round(cm16[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf16[pfc16, "Sensitivity"] <- format(round(cm16[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf16[pfc16, "Specificity"] <- format(round(cm16[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf16[pfc16, "Precision"] <- format(round(cm16[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf16[pfc16, "Recall"] <- format(round(cm16
                                        [["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}

#Putting all together
pf17 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc17 <- 0

for (t in training_data_percentages) {
  pfc17 <- pfc17+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^2
  train.data_nl$market_segment_type2 <- train.data_nl$market_segment_type^2
  train.data_nl$no_of_special_requests2 <- train.data_nl$no_of_special_requests^2
  
  test.data_nl$Arrival_date2 <- test.data_nl$Arrival_date^2
  test.data_nl$market_segment_type2 <- test.data_nl$market_segment_type^2
  test.data_nl$no_of_special_requests2 <- test.data_nl$no_of_special_requests^2
  
  avg_avg_room_price <- mean(train.data_nl$avg_price_per_room)
  std_avg_room_price <- sd(train.data_nl$avg_price_per_room)
  avg_room_price_threshold <- avg_avg_room_price+3*std_avg_room_price
  
  train.data_nl$avg_price_per_room_BINARIZED <- ifelse( train.data_nl$avg_price_per_room >= avg_room_price_threshold, 1, 0)
  test.data_nl$avg_price_per_room_BINARIZED <- ifelse(test.data_nl$avg_price_per_room >= avg_room_price_threshold, 1, 0)
  
  
  lr_model17 <- train(booking_status ~ lead_time*repeated_guest + Arrival_date + market_segment_type + avg_price_per_room_BINARIZED + repeated_guest + no_of_special_requests + Arrival_date2 + market_segment_type2 + no_of_special_requests2 , data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions17 <- predict(lr_model17, newdata = test.data_nl)
  cm17 <- confusionMatrix(lr_predictions17, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm17[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm17[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm17[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm17[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm17[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm17[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf17[pfc17, "Accuracy"] <- format(round(cm17[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf17[pfc17, "Kappa"] <- format(round(cm17[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf17[pfc17, "Sensitivity"] <- format(round(cm17[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf17[pfc17, "Specificity"] <- format(round(cm17[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf17[pfc17, "Precision"] <- format(round(cm17[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf17[pfc17, "Recall"] <- format(round(cm17[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ----------------------------------------------------")
}


# Logistic Regression 7 fields all non-linear only lead_time^3

pf18 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc18 <- 0

for (t in training_data_percentages) {
  pfc18 <- pfc18+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^3
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^2
  train.data_nl$market_segment_type2 <- train.data_nl$market_segment_type^2
  train.data_nl$avg_price_per_room2 <- train.data_nl$avg_price_per_room^2
  train.data_nl$repeated_guest2 <- train.data_nl$repeated_guest^2
  train.data_nl$no_of_special_requests2 <- train.data_nl$no_of_special_requests^2
  
  test.data_nl$lead_time2<- test.data_nl$lead_time^3
  test.data_nl$Arrival_date2 <- test.data_nl$Arrival_date^2
  test.data_nl$market_segment_type2 <- test.data_nl$market_segment_type^2
  test.data_nl$avg_price_per_room2 <- test.data_nl$avg_price_per_room^2
  test.data_nl$repeated_guest2 <- test.data_nl$repeated_guest^2
  test.data_nl$no_of_special_requests2 <- test.data_nl$no_of_special_requests^2
  
  lr_model18 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests + lead_time2 + Arrival_date2 + market_segment_type2 + avg_price_per_room2 + repeated_guest2 + no_of_special_requests2 , data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions18 <- predict(lr_model18, newdata = test.data_nl)
  cm18 <- confusionMatrix(lr_predictions18, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm18[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm18[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm18[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm18[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm18[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm18[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf18[pfc18, "Accuracy"] <- format(round(cm18[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf18[pfc18, "Kappa"] <- format(round(cm18[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf18[pfc18, "Sensitivity"] <- format(round(cm18[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf18[pfc18, "Specificity"] <- format(round(cm18[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf18[pfc18, "Precision"] <- format(round(cm18[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf18[pfc18, "Recall"] <- format(round(cm18[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ------------------")
}


# Logistic Regression 7 fields all non-linear all ^ 3

pf19 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc19 <- 0

for (t in training_data_percentages) {
  pfc19 <- pfc19+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^3
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^3
  train.data_nl$market_segment_type2 <- train.data_nl$market_segment_type^3
  train.data_nl$avg_price_per_room2 <- train.data_nl$avg_price_per_room^3
  train.data_nl$repeated_guest2 <- train.data_nl$repeated_guest^3
  train.data_nl$no_of_special_requests2 <- train.data_nl$no_of_special_requests^3
  
  test.data_nl$lead_time2<- test.data_nl$lead_time^3
  test.data_nl$Arrival_date2 <- test.data_nl$Arrival_date^3
  test.data_nl$market_segment_type2 <- test.data_nl$market_segment_type^3
  test.data_nl$avg_price_per_room2 <- test.data_nl$avg_price_per_room^3
  test.data_nl$repeated_guest2 <- test.data_nl$repeated_guest^3
  test.data_nl$no_of_special_requests2 <- test.data_nl$no_of_special_requests^3
  
  lr_model19 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests + lead_time2 + Arrival_date2 + market_segment_type2 + avg_price_per_room2 + repeated_guest2 + no_of_special_requests2 , data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions19 <- predict(lr_model19, newdata = test.data_nl)
  cm19 <- confusionMatrix(lr_predictions19, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm19[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm19[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm19[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm19[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm19[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm19[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf19[pfc19, "Accuracy"] <- format(round(cm19[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf19[pfc19, "Kappa"] <- format(round(cm19[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf19[pfc19, "Sensitivity"] <- format(round(cm19[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf19[pfc19, "Specificity"] <- format(round(cm19[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf19[pfc19, "Precision"] <- format(round(cm19[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf19[pfc19, "Recall"] <- format(round(cm19[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ------------------")
}

# Logistic Regression 7 fields all non-linear all ^ 4

pf20 = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc20 <- 0

for (t in training_data_percentages) {
  pfc20 <- pfc20+1
  set.seed(123)
  training.samples_nl <- Data_Numeric$booking_status %>% createDataPartition(p = t, list = FALSE)
  train.data_nl <- Data_Numeric[training.samples_nl, ]
  test.data_nl <- Data_Numeric[-training.samples_nl, ]
  
  train.data_nl$lead_time2 <- train.data_nl$lead_time^4
  train.data_nl$Arrival_date2 <- train.data_nl$Arrival_date^4
  train.data_nl$market_segment_type2 <- train.data_nl$market_segment_type^4
  train.data_nl$avg_price_per_room2 <- train.data_nl$avg_price_per_room^4
  train.data_nl$repeated_guest2 <- train.data_nl$repeated_guest^4
  train.data_nl$no_of_special_requests2 <- train.data_nl$no_of_special_requests^4
  
  test.data_nl$lead_time2<- test.data_nl$lead_time^4
  test.data_nl$Arrival_date2 <- test.data_nl$Arrival_date^4
  test.data_nl$market_segment_type2 <- test.data_nl$market_segment_type^4
  test.data_nl$avg_price_per_room2 <- test.data_nl$avg_price_per_room^4
  test.data_nl$repeated_guest2 <- test.data_nl$repeated_guest^4
  test.data_nl$no_of_special_requests2 <- test.data_nl$no_of_special_requests^4
  
  lr_model20 <- train(booking_status ~ lead_time + Arrival_date + market_segment_type + avg_price_per_room + repeated_guest + no_of_special_requests + lead_time2 + Arrival_date2 + market_segment_type2 + avg_price_per_room2 + repeated_guest2 + no_of_special_requests2 , data = train.data_nl, method = "glm", family = "binomial")
  lr_predictions20 <- predict(lr_model20, newdata = test.data_nl)
  cm20<- confusionMatrix(lr_predictions20, test.data_nl$booking_status)
  
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm20[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm20[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm20[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm20[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm20[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm20[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  pf20[pfc20, "Accuracy"] <- format(round(cm20[["overall"]][["Accuracy"]] * 100, 2), nsmall = 2)
  pf20[pfc20, "Kappa"] <- format(round(cm20[["overall"]][["Kappa"]] * 100, 2), nsmall = 2)
  pf20[pfc20, "Sensitivity"] <- format(round(cm20[["byClass"]][["Sensitivity"]] * 100, 2), nsmall = 2)
  pf20[pfc20, "Specificity"] <- format(round(cm20[["byClass"]][["Specificity"]] * 100, 2), nsmall = 2)
  pf20[pfc20, "Precision"] <- format(round(cm20[["byClass"]][["Precision"]] * 100, 2), nsmall = 2)
  pf20[pfc20, "Recall"] <- format(round(cm20[["byClass"]][["Recall"]] * 100, 2), nsmall = 2)
  
  message("-----------------------------------------------------------------------------
  ------------------")
}
