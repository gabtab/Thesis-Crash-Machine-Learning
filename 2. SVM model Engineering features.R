
####This range of experiments were aimed at testing the SVM using only the data that was available to the engineering experiement
### then at the end added in descriptive features to try to improve the accuracy. There are a number of upsampling sections as the 
### data was too space and heavily biased
joindatcarfeatures= NULL
joindatengineering = NULL
alldataengineering = NULL
alldataengineeringup = NULL
results = NULL
svmengall = NULL
crashdatapoints= NULL
 
############################################# EACH OF THESE BLOCKS OF CODE REPRESENT 2 MODELS THAT NEED TO BE RUN. CHANGE THE 
############################################  method = "svmRadial" line to "svmLinear" or back for different results ########
 datap = 5
  n=30
###get all the x,y,z data from the crash into a dataframe
for (i in dfmag$TSTNO){
  
  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  results = datapoints(datap,n, ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z)
  results$TSTNO = i
  
  if (exists('crashdatapoints') == TRUE) {
    crashdatapoints = rbind(crashdatapoints,results)
  }  else{
    crashdatapoints = results
  } 
}

 joindatcarfeatures = validate[c(1,3,5,7,9,12,13,17,20,51,53,95)]
 joindatengineering = validate[c(1,17,51,95)]


alldataengineering =  merge(crashdatapoints, joindatengineering, by = 'TSTNO')


set.seed(1001)
intrain <- createDataPartition(y = alldataengineering$DamLev, p= 0.5, list = FALSE)
training = alldataengineering[intrain,]
testing <- alldataengineering[-intrain,]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)

svm_Linear <- train(DamLev ~., data = training, method = "svmRadial",  ######change here and re run to line 59 for another model
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred = predict(svm_Linear, newdata = testing)
mod1sE = confusionMatrix(svm_pred, testing$DamLev)
testing$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
svm_pred <- factor(svm_pred, levels=c("Low", "High"), ordered=TRUE)
mod1rocsE = roc(testing$DamLev, svm_pred)

####################### 2nd group of models############################################################################
####################### RUN AS FAR AS LINE 115 CHANGING THE METHOD TO CHANGE THE MODEL ##############################
set.seed(12345)

correlationMatrix <- cor(alldataengineering[,2:51])  ## this bit needs to change to the number of columns in the dataframe being measured

print(correlationMatrix)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

#####above gives each of the values at highly correlated so i then reduced increased the step to choose the length of crash and 

##############Up sample above data to see if i can get a better answer  #####

if (datap == 5 & n == 60) {coldel = c(80,81)
  
}else if (datap == 10 & n == 60){coldel = c(44,45)
}else if (datap == 1 & n == 60){coldel = c(368,369)
}else if (datap == 50 & n == 60){coldel = c(14,15)
}else if (datap == 5 & n == 30){coldel = c(44,45)
}else if (datap == 10 & n == 30){coldel = c(26,27)
}else if (datap == 1 & n == 30){coldel = c(188,189)
}else if (datap == 50 & n == 30){coldel = c(11,12)}

colnames(alldataengineeringup)
colnames(validate)
alldataengineeringup = data.frame(alldataengineering[,], damrat=validate[match(alldataengineering$TSTNO, validate$TSTNO), 94])
alldataengineeringup = upSample(alldataengineeringup, alldataengineeringup$damrat)
colnames(alldataengineeringup)
summary(as.factor(alldataengineeringup$Class))
alldataengineeringup = alldataengineeringup[,-coldel]
set.seed(1001)
intrainup <- createDataPartition(y = alldataengineeringup$DamLev, p= 0.5, list = FALSE)
trainingup = alldataengineeringup[intrainup,]
testingup <- alldataengineeringup[-intrainup,]
trctrlup = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)
colnames(testingup)
svm_Linearup <- train(DamLev ~., data = trainingup[,-1], method = "svmRadial",
                    trControl=trctrlup,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_predup = predict(svm_Linearup, newdata = testingup[,!names(testingup) %in% c("TSTNO", "DamLev")])
mod2sE = confusionMatrix(svm_predup, testingup$DamLev)

testingup$DamLev <- factor(testingup$DamLev, levels=c("Low", "High"), ordered=TRUE)
svm_predup <- factor(svm_predup, levels=c("Low", "High"), ordered=TRUE)
mod2rocsE = roc(testingup$DamLev, svm_predup)



############################THIRD GROUP OF MODELS##############################################################
########################### Removed the sensor data and just compared the small sample space using descriptive features


alldatacarfeatures = NULL

alldatacarfeatures = data.frame(joindatcarfeatures[,], damrat=validate[match(joindatcarfeatures$TSTNO, validate$TSTNO), 94])
alldatacarfeatures = upSample(alldatacarfeatures, alldatacarfeatures$damrat)
colnames(alldatacarfeatures) ###need to remove the right columns next
alldatacarfeatures = alldatacarfeatures[,-c(13,14)]
set.seed(1001)
intrain <- createDataPartition(y = alldatacarfeatures$DamLev, p= 0.5, list = FALSE)
training = alldatacarfeatures[intrain,]
testing <- alldatacarfeatures[-intrain,]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)

svm_Linear <- train(DamLev ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred = predict(svm_Linear, newdata = testing)
predictors(svm_pred)
mod3sE = confusionMatrix(svm_pred, testing$DamLev)

testingup$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
svm_pred <- factor(svm_pred, levels=c("Low", "High"), ordered=TRUE)
mod3rocsE = roc(testing$DamLev, svm_pred)



##################################FOURTH GROUP OF MODELS##################################
####try same model as above but remove the PDOF


alldatacarfeatures = NULL

alldatacarfeatures = data.frame(joindatcarfeatures[,], damrat=validate[match(joindatcarfeatures$TSTNO, validate$TSTNO), 94])
alldatacarfeatures = upSample(alldatacarfeatures, alldatacarfeatures$damrat)
colnames(alldatacarfeatures) ###need to remove the right columns next
alldatacarfeatures = alldatacarfeatures[,-c(13,14)]
alldatacarfeatures = alldatacarfeatures[,-11]
set.seed(1001)
intrain <- createDataPartition(y = alldatacarfeatures$DamLev, p= 0.5, list = FALSE)
training = alldatacarfeatures[intrain,]
testing <- alldatacarfeatures[-intrain,]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)

svm_Linear <- train(DamLev ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred = predict(svm_Linear, newdata = testing)
predictors(svm_pred)
confusionMatrix(svm_pred, testing$DamLev)
testingup$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
svm_pred <- factor(svm_pred, levels=c("Low", "High"), ordered=TRUE)
mod4rocsE = roc(testing$DamLev, svm_pred)



###################FIFTH GROUP OF MODELS###############################################################
###### re-ran this multiple times to get the different results for the varying timesteps in the crash
loopdata = c(1,5,10,50)

for (j in loopdata){
    crashdatapoints= NULL
    alldatacarfeatures = NULL
    datap = j
    n = 30
    if (datap == 5 & n == 60) {coldel = c(88,89)
    }else if (datap == 10 & n == 60){coldel = c(52,53)
    }else if (datap == 1 & n == 60){coldel = c(376,377)
    }else if (datap == 50 & n == 60){coldel = c(22,23)
    }else if (datap == 5 & n == 30){coldel = c(52,53)
    }else if (datap == 10 & n == 30){coldel = c(34,35)
    }else if (datap == 1 & n == 30){coldel = c(196,197)
    }else if (datap == 50 & n == 30){coldel = c(19,20)}
    
    ###get all the x,y,z data from the crash into a dataframe
    for (i in dfmag$TSTNO){
      
      ind.dat = testdat[testdat$TSTNO == i ,]
      ind.imp = which.max(ind.dat$mag)
      f.imp <- ind.dat[ind.imp,]
      results = datapoints(datap,n,ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z)
      results$TSTNO = i
      
      if (exists('crashdatapoints') == TRUE) {
        crashdatapoints = rbind(crashdatapoints,results)
      }  else{
        crashdatapoints = results
      } 
    }
    alldatacarfeatures = data.frame(joindatcarfeatures[,], damrat=validate[match(joindatcarfeatures$TSTNO, validate$TSTNO), 94])
    alldatacarfeatures = merge(crashdatapoints, alldatacarfeatures, by ='TSTNO')
    alldatacarfeatures = upSample(alldatacarfeatures, alldatacarfeatures$damrat)
    colnames(alldatacarfeatures)
    alldatacarfeatures = alldatacarfeatures[,-coldel]
    set.seed(1001)
    intrain <- createDataPartition(y = alldatacarfeatures$DamLev, p= 0.5, list = FALSE)
    training = alldatacarfeatures[intrain,]
    testing <- alldatacarfeatures[-intrain,]
    
    trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
    set.seed(1234)
    
    svm_Linear <- train(DamLev ~., data = training, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
    svm_pred = predict(svm_Linear, newdata = testing)
    predictors(svm_pred)
    svmmod = confusionMatrix(svm_pred, testing$DamLev)
    
    testingup$DamLev <- factor(testing$DamLev, levels=c("Low", "High"), ordered=TRUE)
    svm_pred <- factor(svm_pred, levels=c("Low", "High"), ordered=TRUE)
    modrocsE = roc(testing$DamLev, svm_pred)
    
    
    
    
    svmmod$byClass
    svmengmod = data.frame(matrix(unlist(c(paste("SVM all",j, sep = " "), round(as.numeric(svmmod$overall['Accuracy']),digits =2), 
                                         round(as.numeric(svmmod$overall['AccuracyLower']),digits = 2),
                                         round(as.numeric(svmmod$overall['AccuracyUpper']),digits = 2),
                                         round(as.numeric(svmmod$byClass['Sensitivity']),digits = 2), 
                                         round(as.numeric(svmmod$byClass['Specificity']),digits = 2),
                                         round(as.numeric(svmmod$overall['AccuracyNull']),digits = 2),
                                         round(as.numeric(modrocsE$auc),digits = 2))),nrow =1 , byrow =T))
    colnames(svmengmod)   = c("model", "Acc","Lwr","Upr","Sen","Spc","no inf")  
    if (exists('svmengall') == TRUE) {
      svmengall = rbind(svmengall,svmengmod)
    }  else{
      svmengall = svmengmod
    } 
   
    
}
svmengall
