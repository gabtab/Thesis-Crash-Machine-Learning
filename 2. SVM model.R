
describe(vehdatclSVM)
describe(tstsetSVM)
vehdatclSVM = vehdatclSVM[(vehdatclSVM$TSTNO %in% tstsetSVM$TSTNO),]
vehdatclSVM %>% group_by(` VEHNO`) %>% summarise(count =n())

vehdatclSVM = vehdatclSVM[vehdatclSVM$TSTNO %in% tstsetSVM$TSTNO,]
describe(vehdatclSVM)

library(e1071)
train = car1

plot(car1)

model_svm = svm(y)
