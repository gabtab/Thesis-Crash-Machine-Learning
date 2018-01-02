
##need to use only features that will be available to insurance pre crash
clSVMall = vehdatclSVM[c(3,5,7,9,12,14,17,19,20,50,52,88)]
#vehdatclSVMall[,c(1:6)] = lapply(vehdatclSVMall[,c(1:6)],factor)
clSVMall[,2] = as.numeric(clSVMall$` MODEL`)

intrain <- createDataPartition(y = clSVMall$DamLev, p= 0.5, list = FALSE)
training = clSVMall[intrain,]
testing <- clSVMall[-intrain,]

####other option for selecting features is use a random forest method here #######################
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
set.seed(1234)
results <- rfe(clSVMall[,1:11], clSVMall[,12], sizes=c(1:11), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
################################
#NEED TO LOOK AT REDUCING THE PREDICTORS TO THE ONES FROM 2 LINES ABOVE " VEHSPD" " YEAR"   " BODY"   " ENGINE" " VEHWID" " VEHTWT"
#DID AND IT DIDNT WORK
########################################################################
levels(training$DamLev)
set.seed(1001)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(DamLev ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_pred = predict(svm_Linear, newdata = testing)

confusionMatrix(svm_pred, testing$DamLev)
summary(svm_Linear)
#######next bit looks to test on the engineering dataset but this is incorrect as the data is included in training 
validate1 = validate[c(3,5,7,9,12,14,17,20,21,51,53,95)]
validate1 = na.omit(validate1)
svm_predfinal = predict(svm_Linear, newdata = validate1)
confusionMatrix(svm_predfinal, validate1$DamLev)


nrow(svm_predfinal)
ncol(validate)



######################################################################################################################
########################################################################################################################
#####################################################################################################################
#####################################################################################################################


describe(validate1)
validate1 = validate[c(3,5,7,9,12,14,17,20,21,51,53,95)]
#validate1 = validate[complete.cases(validate),]
svm_final <- train(DamLev ~., data = validate1, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_pred = predict(svm_final, newdata = validate)
confusionMatrix(svm_pred, validate$DamLev)

##tuning parameter C was set to 1, using grid see which is the best tuning parameter
grid = expand.grid(C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))

summary(training)
set.seed(2345)
svm_Linear_grid <- train(DamLev ~ ., data = training, method = "svmLinear",
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
