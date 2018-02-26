#devtools::install_github('topepo/caret/pkg/caret')
##need to use only features that will be available to insurance pre crash
clSVMall = vehdatclSVM[c(3,5,7,9,12,14,17,19,20,50,52,88)]
#vehdatclSVMall[,c(1:6)] = lapply(vehdatclSVMall[,c(1:6)],factor)
clSVMall[,2] = as.numeric(clSVMall$` MODEL`)
clSVMall = clSVMall[,-11]
describe(clSVMall)

intrain <- createDataPartition(y = clSVMall$DamLev, p= 0.5, list = FALSE)
training = clSVMall[intrain,]
testing <- clSVMall[-intrain,]

####first see what the results are with all of the descriptive features selected
trctrl = trainControl(method = "repeatedcv", number = 10, classProbs = T, repeats = 3)


svm_Linear1 <- train(DamLev ~ ., data = training, method = "svmLinear",
                     trControl=trctrl,
                     metric = 'ROC',
                     preProcess = c("center", "scale"),
                     tuneLength = 10)

svm_pred1 = predict(svm_Linear1, newdata = testing)
svm_pred1 <- factor(svm_pred1, levels=c("Low", "High"), ordered=TRUE)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
mod1 = confusionMatrix(svm_pred1, testing$DamLev)
mod1roc = roc(testing$DamLev, svm_pred1)

svmprob1 <- predict(svm_Linear1, newdata = testing %>% select(-DamLev), type = "prob")
mod1rox <- roc(testing$DamLev, svmprob1, predictor = svmprob1[, "High"])

#####################################################################################################################################################
####other option for selecting features is use a random forest method here #######################
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
set.seed(1234)
results <- rfe(clSVMall[,1:10], clSVMall[,11], sizes=c(1:10), rfeControl=control)
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

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE)


svm_Linear2 <- train(y ~ ., data = trainX, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
svm_Linear2$coefnames
svm_pred2 = predict(svm_Linear2, newdata = testX)
mod2 = confusionMatrix(svm_pred2, testX$ytest)
levels(svm_pred2); levels(testX$ytest)
svm_pred2 <- factor(svm_pred2, levels=c("Low", "High"), ordered=TRUE)
testX$ytest <- factor(testX$ytest, levels=c("Low", "High"), ordered=TRUE)
mod2roc = roc(testX$ytest, svm_pred2)

svmprob2 <- predict(svm_Linear2, newdata = testX %>% select(-ytest), type = "prob")
mod2rox <- roc(testX$ytest, svmprob2, predictor = svmprob2[, "High"])

######################################################################################################################
########################################################################################################################
#####################################################################################################################
#####################################################################################################################



##tuning parameter C was set to 1, using grid see which is the best tuning parameter
grid = expand.grid(C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))

summary(training)
set.seed(2345)
svm_Linear_grid3 <- train(DamLev ~ ., data = training, method = "svmLinear",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneLength = 10,
                          tuneGrid = grid)
plot(svm_Linear_grid3)

test_pred_grid3 = predict(svm_Linear_grid3, newdata = testing)
mod3 = confusionMatrix(test_pred_grid3, testing$DamLev)
test_pred_grid3 <- factor(test_pred_grid3, levels=c("Low", "High"), ordered=TRUE)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
mod3roc = roc(testing$DamLev, test_pred_grid3)

svmprob3 <- predict(svm_Linear_grid3, newdata = testing %>% select(-DamLev), type = "prob")
mod3rox <- roc(testing$DamLev, svmprob3, predictor = svmprob3[, "High"])


############################################################################
####check to see if a nonlinear model is more accurate
set.seed(3456)
svm_radial4 = train(DamLev ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
plot(svm_radial)

test_pred_Radial4 <- predict(svm_radial4, newdata = testing)
mod4 = confusionMatrix(test_pred_Radial4, testing$DamLev )


test_pred_Radial4 <- factor(test_pred_Radial4, levels=c("Low", "High"), ordered=TRUE)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
mod4roc = roc(testing$DamLev, test_pred_Radial4)

svmprob4 <- predict(svm_radial4, newdata = testing %>% select(-DamLev), type = "prob")
mod4rox <- roc(testing$DamLev, svmprob4, predictor = svmprob4[, "High"])




#################################################################
####check the non linear and tune sigma and c 
grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid5 <- train(DamLev ~., data = training, method = "svmRadial",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneGrid = grid_radial,
                          tuneLength = 10)

test_pred_Radial_Grid5 <- predict(svm_Radial_Grid5, newdata = testing)
mod5 = confusionMatrix(test_pred_Radial_Grid5, testing$DamLev )

test_pred_Radial_Grid5 <- factor(test_pred_Radial_Grid5, levels=c("Low", "High"), ordered=TRUE)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
mod5roc = roc(testing$DamLev, test_pred_Radial_Grid5)

svmprob5 <- predict(svm_Radial_Grid5, newdata = testing %>% select(-DamLev), type = "prob")
mod5rox <- roc(testing$DamLev, svmprob5, predictor = svmprob5[, "High"])


##################################################################
#### next try it on the reduced dataset as determined by the random forest

set.seed(3456)
svm_radial6 = train(y ~., data = trainX, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
plot(svm_radial6)

test_pred_Radial6 <- predict(svm_radial6, newdata = testX)
confusionMatrix(test_pred_Radial6, testX$ytest)

grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid6 <- train(y ~., data = trainX, method = "svmRadial",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneGrid = grid_radial,
                          tuneLength = 10)

test_pred_Radial_Grid6 <- predict(svm_Radial_Grid6, newdata = testX)
mod6 = confusionMatrix(test_pred_Radial_Grid6, testX$ytest )

test_pred_Radial_Grid6 <- factor(test_pred_Radial_Grid6, levels=c("Low", "High"), ordered=TRUE)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
mod6roc = roc(testing$DamLev, test_pred_Radial_Grid6)

svmprob6 <- predict(svm_Radial_Grid6, newdata = testing %>% select(-DamLev), type = "prob")
mod6rox <- roc(testing$DamLev, svmprob6, predictor = svmprob6[, "High"])





##############################

mod1 
svmmod1 = data.frame(matrix(unlist(c("SVM Linear not Momentum Data", round(as.numeric(mod1$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod1$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod1$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod1$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod1$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod1)   = c("model", "Acc","Lwr","Upr","Sen","Spc")  
mod2 
svmmod2 = data.frame(matrix(unlist(c("SVM Linear not Momentum Data less features", round(as.numeric(mod2$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod2$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod2$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod2$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod2$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod2)   = c("model", "Acc","Lwr","Upr","Sen","Spc")  
mod3
svmmod3 = data.frame(matrix(unlist(c("SVM Linear not Momentum Data less features tuned", round(as.numeric(mod3$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod3$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod3$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod3$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod3$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod3)   = c("model", "Acc","Lwr","Upr","Sen","Spc") 
mod4
svmmod4 = data.frame(matrix(unlist(c("SVM non Linear not Momentum Data", round(as.numeric(mod4$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod4$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod4$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod4$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod4$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod4)   = c("model", "Acc","Lwr","Upr","Sen","Spc") 

mod5
svmmod5 = data.frame(matrix(unlist(c("SVM non Linear not Momentum Data tuned", round(as.numeric(mod5$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod5$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod5$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod5$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod5$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod5)   = c("model", "Acc","Lwr","Upr","Sen","Spc") 


mod6
svmmod6 = data.frame(matrix(unlist(c("SVM non Linear not Momentum Data tuned less features", round(as.numeric(mod6$overall['Accuracy']),digits =2), 
                                     round(as.numeric(mod6$overall['AccuracyLower']),digits = 2),
                                     round(as.numeric(mod6$overall['AccuracyUpper']),digits = 2),
                                     round(as.numeric(mod6$byClass['Sensitivity']),digits = 2), 
                                     round(as.numeric(mod6$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(svmmod6)   = c("model", "Acc","Lwr","Upr","Sen","Spc") 

svmresults = rbind(svmmod1,svmmod2,svmmod3,svmmod4,svmmod5,svmmod6)

data_long <- gather(svmresults, metrics, measurement, Acc:Spc, factor_key=TRUE)


####
chartin = data_long[data_long$metrics %in% c("Acc","Sen","Spc"),]
chartlong <- chartin %>% arrange(model) %>%
  group_by(model) %>% 
  mutate(linecol = rank(model, ties.method = "first"))


chartlong <- chartlong %>% arrange(model) %>%
  group_by(model) %>% 
  mutate(linwid = rank(measurement, ties.method = "first"))

chartin = chartlong[chartlong$metrics %in% c("Acc","Sen","Spc"),]


ggplot(chartlong, aes(x =chartlong$metrics, y =chartlong$measurement,group = chartlong$model)) + 
  geom_line(aes(color = chartlong$model), size = 1) + theme_economist()+ scale_colour_hue(name="model", l=50) +
  scale_linetype_discrete(name="linwid") +
  xlab("Statistics") + ylab("% results") + # Set axis labels
  ggtitle("SVM model comparison") 

g = plot.roc(mod1rox) + theme_economist()
plot.roc(mod2rox, add = TRUE, col = 'Blue')
plot.roc(mod3rox, add = TRUE, col = 'steel blue')
plot.roc(mod4rox, add = TRUE, col = 'yellow')
plot.roc(mod5rox, add = TRUE, col = 'red')
plot.roc(mod6rox, add = TRUE, col = 'purple')

###look at adding the below AUC to the report   
mod1rox  
mod2rox  
mod3rox  
mod4rox   
mod5rox  
mod6rox   

ls()
keep(c(mod1rox,mod2rox,mod3rox,mod4rox, mod5rox, mod6rox ))

#   sens_spec1 <- data.frame(spec=rev(mod1roc$specificities),
#                           sens=rev(mod1roc$sensitivities))
#   sens_spec2 <- data.frame(spec=rev(mod2roc$specificities),
#                            sens=rev(mod2roc$sensitivities))
#   sens_spec3 <- data.frame(spec=rev(mod3roc$specificities),
#                            sens=rev(mod3roc$sensitivities))
#   sens_spec4 <- data.frame(spec=rev(mod4roc$specificities),
#                            sens=rev(mod4roc$sensitivities))
#   sens_spec5 <- data.frame(spec=rev(mod5roc$specificities),
#                            sens=rev(mod5roc$sensitivities))
#   sens_spec6 <- data.frame(spec=rev(mod6roc$specificities),
#                            sens=rev(mod6roc$sensitivities))
#   
#   
# 
# plot(mod1roc) 
# g <- ggplot() + geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0)) +
#   scale_x_reverse(name = "Specificity",limits = c(1,0), expand = c(0.001,0.001)) + 
#   scale_y_continuous(name = "Sensitivity", limits = c(0,1), expand = c(0.001, 0.001)) +
#   theme_economist()
# g = g + geom_roc(aes(x=spec, y=sens), data=sens_spec1,  lwd=1)  
# g = g + geom_step(aes(x=spec, y=sens), data=sens_spec2,  lwd=1)   
# g = g + geom_step(aes(x=spec, y=sens), data=sens_spec3,  lwd=1)   
# g = g + geom_step(aes(x=spec, y=sens), data=sens_spec4,  lwd=1)
# g = g + geom_step(aes(x=spec, y=sens), data=sens_spec5,  lwd=1)   
# g = g + geom_step(aes(x=spec, y=sens), data=sens_spec6,  lwd=1)   
# ?plot.roc
