################First steps in this code were to run checks on the data set that the engineering model was used on
################it used the same data, then reduced the number of cases taken from the x,y,z data by changing the step in the 



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
 joindatcarfeatures = validate[c(1,3,5,7,9,12,13,17,20,51,53,95)]
 joindatengineering = validate[c(1,17,51,95)]


alldataengineering =  merge(crashdatapoints, joindatengineering, by = 'TSTNO')

set.seed(1001)
intrain <- createDataPartition(y = alldataengineering$DamLev, p= 0.5, list = FALSE)
training = alldataengineering[intrain,]
testing <- alldataengineering[-intrain,]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1234)

svm_Linear <- train(DamLev ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred = predict(svm_Linear, newdata = testing)
confusionMatrix(svm_pred, testing$DamLev)

###########################once the above code was ran multiple times it was then decided to add in features relating the the car 
########################### and gradually reduce these features to see if they were important


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
alldatacarfeatures = merge(crashdatapoints, joindatcarfeatures, by ='TSTNO')
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
confusionMatrix(svm_pred, testing$DamLev)

#######################After putting all of the data together it is important to remove all of the features that have a high correlatio
set.seed(12345)
# calculate correlation matrix
colnames(alldatacarfeatures)
correlationMatrix <- cor(alldatacarfeatures[,2:6])  ## this bit needs to change to the number of columns in the dataframe being measured
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#####above gives each of the values at highly correlated so i then reduced increased the step to choose the length of crash and 
##### then re-ran the model to check accuracy and then re-ran the above to check correlation



