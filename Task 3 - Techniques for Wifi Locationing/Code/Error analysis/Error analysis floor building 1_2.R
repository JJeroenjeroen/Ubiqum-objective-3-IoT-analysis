#####################################################
# Date:      06-03-2019                             #
# Author:    Jeroen Meij                            #
# File:      Wifi localization modeling             #
# Version:   2.0                                    #    
#####################################################


#This file will give the results and plot the predictions to see where the errors occur  
#For more information, visit http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#########################################################################################

#change filter the predictions per building
all_y_values0 <- all_y_values %>% filter(BUILDINGID == 0)
all_y_values1 <- all_y_values %>% filter(BUILDINGID == 1)
all_y_values2 <- all_y_values %>% filter(BUILDINGID == 2)

confusionMatrix(data = all_y_values0$knn_predict_FLOOR, all_y_values0$FLOOR)
confusionMatrix(data = all_y_values1$knn_predict_FLOOR, all_y_values1$FLOOR)
confusionMatrix(data = all_y_values2$knn_predict_FLOOR, all_y_values2$FLOOR)

prop.table(table(all_y_values1$knn_predict_FLOOR, all_y_values1$FLOOR))
confusionMatrix(throwaway_fit)


ggplot(all_y_values1) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE, colour = knn_predict_FLOOR)) +
  facet_wrap("FLOOR")


#create table for the floors where the misprediction happens
wifi_train_f <- filter(wifi_train, wifi_train$BUILDINGID == 1 & 
                                wifi_train$FLOOR == 2 & 
                                wifi_train$LONGITUDE > -7550 & 
                                wifi_train$LONGITUDE < -7475 & 
                                wifi_train$LATITUDE > 4864820 & 
                                wifi_train$LATITUDE < 4864875)


wifi_train_f <- wifi_train_f[-which(apply(wifi_train_f, 2, var) == 0)]

wifi_train_g <- filter(wifi_train, wifi_train$BUILDINGID == 1 & 
                         wifi_train$FLOOR == 1 & 
                         wifi_train$LONGITUDE > -7550 & 
                         wifi_train$LONGITUDE < -7475 & 
                         wifi_train$LATITUDE > 4864820 & 
                         wifi_train$LATITUDE < 4864875)


ggplot(wifi_train_f) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE, colour = FLOOR))

ggplot(wifi_train_g) +
  geom_point(aes(x = LATITUDE, y = LONGITUDE, colour = FLOOR))


#make the dataframes longer
wifi_train_f <- gather(wifi_train_f, WPA, dnB, 1:520)
train_f <- gather(train_f, WPA, dnB, 10:ncol(train_f))
test_c <- gather(test_c, WPA, dnB, 10:ncol(test_c))
test_f <- gather(test_f, WPA, dnB, 10:ncol(test_f))

#filter the values where no signal was obtained
wifi_train_f <- wifi_train_f %>% filter(dnB < 100)
train_f <- train_f %>% filter(dnB < 100)
test_c <- test_c %>% filter(dnB < 100)
test_f <- test_f %>% filter(dnB < 100)


search_tr_c <- wifi_train_f %>% filter(dnB > -80 & dnB < -70)
search_tr_f <- train_f %>% filter(dnB > -80 & dnB < -70)


ggplot(wifi_train_f) +
  geom_density(aes(x = dnB))
ggplot(test_c) +
  geom_density(aes(x = dnB))



ggplot(train_f) +
  geom_density(aes(x = dnB))
ggplot(test_f) +
  geom_density(aes(x = dnB))





