#devtools::install_github('topepo/caret/pkg/caret')
##need to use only features that will be available to insurance pre crash
clSVMall = vehdatclSVM[c(3,5,7,9,12,14,17,19,20,50,52,88)]
#vehdatclSVMall[,c(1:6)] = lapply(vehdatclSVMall[,c(1:6)],factor)
clSVMall[,2] = as.numeric(clSVMall$` MODEL`)

describe(clSVMall)

intrain <- createDataPartition(y = clSVMall$DamLev, p= 0.5, list = FALSE)
training = clSVMall[intrain,]
testing <- clSVMall[-intrain,]

####first see what the results are with all of the descriptive features selected
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)


svm_Linear <- train(DamLev ~ ., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
  
svm_pred = predict(svm_Linear, newdata = testing)
mod1 = confusionMatrix(svm_pred, testing$DamLev)

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
ggplot(data = results, aes(x = Variables, y = Accuracy))+
  geom_line() + geom_point() + ylab("Accuracy (Cross Validated)")+ xlab('Variables') + labs(title = "Recursive Feature Selection")+
  theme_economist()

summary(results$optVariables)


trainX <-training[,results$optVariables] # Create training feature data frame
testX <- testing[,results$optVariables] # Create test feature data frame 
y=training$DamLev # Target variable for training
ytest = testing$DamLev

trainX = cbind(trainX, y)
testX = cbind(testX, ytest)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)


svm_Linear <- train(y ~ ., data = trainX, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear$coefnames
svm_pred = predict(svm_Linear, newdata = testX)
mod2 = confusionMatrix(svm_pred, testX$ytest)


######################################################################################################################
########################################################################################################################
#####################################################################################################################
#####################################################################################################################



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
mod3 = confusionMatrix(test_pred_grid, testing$DamLev)
############################################################################
####check to see if a nonlinear model is more accurate
set.seed(3456)
svm_radial = train(DamLev ~., data = training, method = "svmRadial",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
plot(svm_radial)

test_pred_Radial <- predict(svm_radial, newdata = testing)
mod4 = confusionMatrix(test_pred_Radial, testing$DamLev )
#################################################################
####check the non linear and tune sigma and c 
grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid <- train(DamLev ~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)
mod5 = confusionMatrix(test_pred_Radial_Grid, testing$DamLev )

##################################################################
#### next try it on the reduced dataset as determined by the random forest

set.seed(3456)
svm_radial = train(y ~., data = trainX, method = "svmRadial",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
plot(svm_radial)

test_pred_Radial <- predict(svm_radial, newdata = testX)
confusionMatrix(test_pred_Radial, testX$ytest)

grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid <- train(y ~., data = trainX, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testX)
mod6 = confusionMatrix(test_pred_Radial_Grid, testX$ytest )

awake4
  
