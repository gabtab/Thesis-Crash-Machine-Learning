
describe(vehdatclSVM)
describe(validate)

vehdatclSVM = vehdatclSVM[vehdatclSVM$TSTNO %in% tstsetSVM$TSTNO,]
validate = vehdatclSVM[(vehdatclSVM$TSTNO %in% dfmag$TSTNO),]
vehdatclSVM = vehdatclSVM[!(vehdatclSVM$TSTNO %in% dfmag$TSTNO),]

##remove the data being used in the engineering model
summary(vehdatclSVM$DamLev)
head(vehdatclSVM)
vehdatclSVM = vehdatclSVM[,colSums(!is.na(vehdatclSVM))>=2300]
vehdatclSVM = vehdatclSVM[complete.cases(vehdatclSVM),]
describe(vehdatclSVM)

  crashdatapoints= NULL
###get all the x,y,z data from the crash into a dataframe
for (i in dfmag$TSTNO){
  
  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  results = datapoints(ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z)
  results$TSTNO = i
  
  if (exists('crashdatapoints') == TRUE) {
    crashdatapoints = rbind(crashdatapoints,results)
  }  else{
    crashdatapoints = results
  } 
}
###reshape the data so they all follow each othe
  
# vehdatclSVM = vehdatclSVM[c(1,3,5,7,9,12,14,17,19,20,46,48,81)]
 colnames(validate)
 validate = validate[c(1,3,5,7,9,12,13,17,20,51,53,95)]
 summary(crashdatapoints)
alldata = merge(crashdatapoints, validate, by = 'TSTNO')
set.seed(1001)
intrain <- createDataPartition(y = alldata$DamLev, p= 0.5, list = FALSE)
training = alldata[intrain,]
testing <- alldata[-intrain,]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)

svm_Linear <- train(DamLev ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred = predict(svm_Linear, newdata = testing)
confusionMatrix(svm_pred, testing$DamLev)

validate = validate[c(3,5,7,9,12,14,17,19,20,51,53,95)]
validate = validate[complete.cases(validate),]
svm_final <- train(DamLev ~., data = validate, method = "svmLinear",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
summary(svm_final)
svm_pred = predict(svm_final, newdata = validate)
confusionMatrix(svm_pred, validate$DamLev)

##tuning parameter C was set to 1, using grid see which is the best tuning parameter
grid = expand.grid(C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))

set.seed(2345)
svm_Linear_grid <- train(DamLev ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneLength = 10,
                         tuneGrid = grid)
plot(svm_Linear_grid)

test_pred_grid = predict(svm_Linear_grid, newdata = testing)
confusionMatrix(test_pred_grid, testing$DamLev)

####check to see if a nonlinear model is more accurate
set.seed(3456)
svm_radial = train(DamLev ~., data = training, method = "svmRadial",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
plot(svm_radial)

test_pred_Radial <- predict(svm_radial, newdata = testing)
confusionMatrix(test_pred_Radial, testing$DamLev )

grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid <- train(DamLev ~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)
confusionMatrix(test_pred_Radial_Grid, testing$DamLev )


write_csv(outhead, path = "D:/College/Proposal 2/R/Thesis/headtst")
getwd()
describe(vehdatclean)
anyNA(vehdatclSVM)
describe(vehdatclSVM)
write_csv(head(sensorout),path = "D:/College/Proposal 2/R/Thesis/sensorout")
