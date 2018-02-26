###need to remove the Nas first

sensoutReg = sensoutReg[!is.na(sensoutReg$AXIS),]
sensoutReg$AXIS = as.factor(sensoutReg$AXIS)
sensoutReg$VEHNO = as.factor(sensoutReg$VEHNO)
summary(sensoutReg$AXIS)

reshReg <- reshape(data = sensoutReg, timevar = "AXIS",
                   idvar = c("TSTNO","Time","VEHNO"),
                   drop = c("Signal","AXISD","CURNO", "SENATT"), 
                   direction = "wide")
reshReg = reshReg[complete.cases(reshReg), ]

reshReg$mag = sqrt( (reshReg$Force.X^ 2) + (reshReg$Force.Y ^ 2) + (reshReg$Force.Z ^ 2))

dfReg = dfmag
result = NULL
summary(as.factor(dfReg$pdof))


dfReg = data.frame(dfReg[,c(1:10)], vehlen =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 20])
dfReg = data.frame(dfReg[,c(1:11)], vehwid =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 21])
dfReg = data.frame(dfReg[,c(1:12)], pdof =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 53])
#dfVal = dfReg[sample(nrow(dfmag)),]
#dfReg = dfReg %>% filter(!(TSTNO %in% dfVal$TSTNO))

dfReg =  dfReg[complete.cases(dfReg), ]
#########################################################################################################################
###next bit is for the models that will use the sensor data as input variables
datap = 5   ##############################  CHANGE HERE TO CHANGE THE STEP OF THE CRASH  50 = 2 data points 1 = 50 #################################
results = NULL
regcrash = NULL
resdata = NULL
n = 30
for (i in dfReg$TSTNO){
  
  ind.dat = reshReg[reshReg$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  resdata = datapoints(datap,n,ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z)
  resdata$TSTNO = i
  
  if (exists('regcrash') == TRUE) {
    regcrash = rbind(regcrash,resdata)
  }  else{
    regcrash = resdata
  } 
}
alldatareg = merge(dfReg, regcrash, by ='TSTNO')
alldatareg = alldatareg[complete.cases(alldatareg),]
colnames(alldatareg)
describe(alldatareg)
##after this section you can re-run the models by highlighted them below and running
#######################################################################################
######################################################################################
########################################################################################

####split the data into training and testing data sets
set.seed(1001)
intrainreg <- createDataPartition(y = dfReg$pdof, p= 0.5, list = FALSE)
traingsvm = dfReg[intrainreg,]
trainingreg = dfReg[intrainreg,]
testingreg <- dfReg[-intrainreg,]
testingsvm <- dfReg[-intrainreg,]
###Try two methods of fitting the model 


for (i in testingreg$TSTNO){
  if (testingreg$results[testingreg$TSTNO == i] > 360) { testingreg$results[testingreg$TSTNO == i] = testingreg$results[testingreg$TSTNO == i]- 360}
  results = DirectionCat(testingreg$results[testingreg$TSTNO == i])
  results$TSTNO = i
  gt = DirectionCat(testingreg$pdof[testingreg$TSTNO == i])
  results = cbind(results,gt)
  if (exists('allresults') == TRUE) {
    allresults = rbind(allresults,results)
  }  else{
    allresults = results
    
  } 
}


###################################################################################################################################################
##################mulitnomial logistic regression #######

trainingreg$pdof = as.factor(trainingreg$pdof)

summary(trainingreg$pdof2)
trainingreg$pdof2 <- relevel(trainingreg$pdof, ref = "270")



test <- multinom(pdof2 ~ Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwht + vehlen + vehwid, data = trainingreg)
summary(test)

fit = multinom(as.factor(pdof) ~ 1, data = trainingreg)
fit1b = multinom(as.factor(pdof) ~Force.X + Force.Y + Force.Z + mag + initialspeed + vehwht + vehlen + vehwid, data = trainingreg)
testmodel = step(fit, scope = list(lower = ~1, upper = ~Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwht + vehlen + vehwid),
                 direction = "both", trace =1)
summary(testmodel)
z <- summary(testmodel)$coefficients/summary(testmodel)$standard.errors

####
testingreg$nomresults = predict(testmodel, newdata = testingreg, se.fit = T)
nomresults = NULL
allnomresults = NULL
for (i in testingreg$TSTNO){
  
  nomresults = DirectionCat(as.numeric(as.character(testingreg$nomresults[testingreg$TSTNO == i])))
  nomresults$TSTNO = i
  gt = DirectionCat(testingreg$pdof[testingreg$TSTNO == i])
  nomresults = cbind(nomresults,gt)
  if (exists('allnomresults') == TRUE) {
    allnomresults = rbind(allnomresults,nomresults)
  }  else{
    allnomresults = nomresults
    
  } 
}                   
colnames(allnomresults) = c('nomangle','nomtype','TSTNO','GTangle','gttype')
##check the results v the pdof in the dataset
u = union(allnomresults$nomangle, allnomresults$GTangle)
t = table(factor(allnomresults$nomangle, u), factor(allnomresults$GTangle, u))
modmom1 = confusionMatrix(t)

#######################################################################################################################################################
###Run linear and radial models first 

datap = 5  
results = NULL
regcrash = NULL
resdata = NULL
n = 30

for (i in dfReg$TSTNO){
  
  ind.dat = reshReg[reshReg$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  resdata = datapoints(datap,n, ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z)
  resdata$TSTNO = i
  
  if (exists('regcrash') == TRUE) {
    regcrash = rbind(regcrash,resdata)
  }  else{
    regcrash = resdata
  } 
}
func = lapply(dfReg$pdof, DirectionCat)
dfReg$func = sapply(func, "[[", 1)
summary(dfReg$func)
alldatareg = merge(dfReg, regcrash, by ='TSTNO')
alldatareg = alldatareg[complete.cases(alldatareg),]
alldatareg = alldatareg[,-c(1:8)]
colnames(alldatareg)

#alldatareg = alldatareg[,-5]
svmdata = createDataPartition(y = alldatareg$func, p= 0.5, list = FALSE)
traingsvm = alldatareg[svmdata,]
testingsen <- alldatareg[-svmdata,]
testingsen = testingsen[,-5]
set.seed(3456)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)


svmradiallin = train(func ~., data = traingsvm[,-5], method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
plot(svmradialreg)

testpredsvmlin <- predict(svmradiallin, newdata = testingsen)

u = union(testpredsvmlin, testingsen$func)

t = table(factor(testingsen$func, u), factor(testpredsvmlin, u))
modmom2 = confusionMatrix(t)
modmom2
#################################################
grid_radial = expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                          C = c(0, 0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Radial_Grid6 <- train(func ~., data = traingsvm[,-5], method = "svmRadial",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneGrid = grid_radial,
                          tuneLength = 10)

test_pred_Radial_Grid6 <- predict(svm_Radial_Grid6, newdata = testingsen)
modmom3 = confusionMatrix(test_pred_Radial_Grid6, testingsen$func )
modmom3

######################################################################################
