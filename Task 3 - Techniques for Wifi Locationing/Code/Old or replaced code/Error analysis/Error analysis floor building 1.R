all_y_values0 <- all_y_values %>% filter(BUILDINGID == 0)
all_y_values1 <- all_y_values %>% filter(BUILDINGID == 1)
all_y_values2 <- all_y_values %>% filter(BUILDINGID == 2)

confusionMatrix(data = all_y_values0$knn_predict_FLOOR, all_y_values0$FLOOR)
confusionMatrix(data = all_y_values1$knn_predict_FLOOR, all_y_values1$FLOOR)
confusionMatrix(data = all_y_values2$knn_predict_FLOOR, all_y_values2$FLOOR)

prop.table(table(all_y_values1$knn_predict_FLOOR, all_y_values1$FLOOR))
confusionMatrix(throwaway_fit)



#create table for the floors where the misprediction happens
wifi_train_f <- wifi_train %>% filter(BUILDINGID == 1 & FLOOR == 1 | BUILDINGID == 1 & FLOOR == 2)
wifi_test_f <- wifi_test %>% filter(BUILDINGID == 1 & FLOOR == 1 | BUILDINGID == 1 & FLOOR == 2)

#create table for the floors where prediction is correct
wifi_train_c <- wifi_train %>% filter(BUILDINGID == 1 & FLOOR == 0 | BUILDINGID == 1 & FLOOR == 3)
wifi_test_c <- wifi_test %>% filter(BUILDINGID == 1 & FLOOR == 0 | BUILDINGID == 1 & FLOOR == 3)


#create frames with the WAPS for both correct and wrong floors
train_c <- wifi_train_c[ , c((ncol(wifi_train_c)-8):(ncol(wifi_train_c)), 1:(ncol(wifi_train_c)-9))]
train_f <- wifi_train_f[ , c((ncol(wifi_train_f)-8):(ncol(wifi_train_f)), 1:(ncol(wifi_train_f)-9))]
test_c <- wifi_test_c[ , c((ncol(wifi_test_c)-8):(ncol(wifi_test_c)), 1:(ncol(wifi_test_c)-9))]
test_f <- wifi_test_f[ , c((ncol(wifi_test_f)-8):(ncol(wifi_test_f)), 1:(ncol(wifi_test_f)-9))]

train_c <- gather(train_c, WPA, dnB, 10:ncol(train_c))
train_f <- gather(train_f, WPA, dnB, 10:ncol(train_f))
test_c <- gather(test_c, WPA, dnB, 10:ncol(test_c))
test_f <- gather(test_f, WPA, dnB, 10:ncol(test_f))


train_c <- train_c %>% filter(dnB < 100)
train_f <- train_f %>% filter(dnB < 100)
test_c <- test_c %>% filter(dnB < 100)
test_f <- test_f %>% filter(dnB < 100)

search_tr_c <- train_c %>% filter(dnB > -80 & dnB < -70)
search_tr_f <- train_f %>% filter(dnB > -80 & dnB < -70)


ggplot(train_c) +
  geom_density(aes(x = dnB))
ggplot(test_c) +
  geom_density(aes(x = dnB))



ggplot(train_f) +
  geom_density(aes(x = dnB))
ggplot(test_f) +
  geom_density(aes(x = dnB))





train_c <- wifi_train_c[c((ncol(wifi_train_c)-9):(ncol(wifi_train_c)))]
train_f <- wifi_train_f[c((ncol(wifi_train_f)-9):(ncol(wifi_train_f)))]


test_c <- wifi_test_c[c((ncol(wifi_test_c)-9):(ncol(wifi_test_c)))]
test_f <- wifi_test_f[c((ncol(wifi_test_f)-9):(ncol(wifi_test_f)))]



table(train_c$RELATIVEPOSITION)
table(train_f$RELATIVEPOSITION)

table(train_c$PHONEID)
table(train_f$PHONEID)


table(train_c$USERID)
table(train_f$USERID)


