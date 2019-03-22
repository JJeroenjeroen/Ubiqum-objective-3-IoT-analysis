setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 0")
source(file = "3) Building 0 analysis.R")
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 1")
source(file = "3) Building 1 analysis.R")
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Code/Seperate building analysis/Floor predictions/Building 2")
source(file = "3) Building 2 analysis.R")



#change working directory to map where all results are stalled
setwd("C:/Users/Jeroen/Desktop/Ubiqum/IoT Analytics/Task 3 - Techniques for Wifi Locationing/Excel datafiles/Results/total")

#read results into R
predictions0 <- readRDS(paste(Sys.Date(), "BUilding 0 total"))
predictions1 <- readRDS(paste(Sys.Date(), "BUilding 1 total"))
predictions2 <- readRDS(paste(Sys.Date(), "BUilding 2 total"))


#combine predictions
all_y_values <- bind_rows(predictions0, predictions1, predictions2)

#Turn floor into factor variable
all_y_values$knn_predict_FLOOR <- as.factor(all_y_values$knn_predict_FLOOR)
all_y_values$FLOOR <- as.factor(all_y_values$FLOOR)

#Confusion matrix of the floors
confusionMatrix(data = all_y_values$knn_predict_FLOOR, all_y_values$FLOOR)
