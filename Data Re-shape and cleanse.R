dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-PAPDF3KG\\SQLEXPRESS;database=CrashData;trusted_connection=true')
sensorout <- sqlQuery(dbhandle, 'select * from dbo.Sensor_Output')
tstset <- sqlQuery(dbhandle, 'select * from dbo.tst')
vehdat <- sqlQuery(dbhandle, 'select * from dbo.veh')
instdat <- sqlQuery(dbhandle, 'select * from dbo.instr')

#tstset %>% describe()
#vehdat %>% describe()
#instdat %>% describe()
############################ NEED TOO FINISH THIS ************************************
##########DAta exploration ###################################
#thisChannel_summary <- left_join(thisChannel_summary,m_sumTMP,by="date")
sensorzero = sensorout[(sensorout$Force == 0),]
summarise(sensorzero)
#colSums(sapply(tstset, tstset$CLSSPD == 0 ))
colSums(sapply(tstset, is.na))
###check for any duplicated rows in the dataset tables -
#cat("Sensor Output duplicated rows are", nrow(sensorout) - nrow(unique(sensorout)))
#cat("Test duplicated rows are", nrow(tstset) - nrow(unique(tstset)))
#cat("Vehicle duplicated rows are", nrow(vehdat) - nrow(unique(vehdat)))
#cat("Sensor duplicated rows are", nrow(instdat) - nrow(unique(instdat)))
####do plots for categorical variables
doPlots(tstset, fun = plotHist, ii = 12, ncol = 1)


# 
# tablesum <- thisChannel %>% group_by(date) %>% summarise(min = min(power)
#                                                          ,max = max(power)
#                                                          ,Q1 = quantile(power,.25)
#                                                          ,Q2 = quantile(power,.50)
#                                                          ,Q3 = quantile(power,.75)
#                                                          ,sum = sum(power)
#                                                          ,count = n()
#                                                          ,range = (max(power) - min(power))
# )
# m_sumTMP <- thisChannel %>% group_by(date) %>% filter(power > min(power)) %>% 
#   summarise(sCountAboveDailymin = n())
# #fold that into the summarisation data
# thisChannel_summary <- left_join(thisChannel_summary,m_sumTMP,by="date")




##################START DATACLEANING ###########################################################################
#find out if the cases at the end of the script are actually in the data at the beginning i.e. is there 4000 rows in 
##sensor out for these 2663,6928,5408,6979

sensorout = sensorout[(!sensorout$Force == 0),]
tstsetSVM = tstset[(tstset$` TSTCFN` %in% c('VTB','VTI','VTP')),]
tstReg = tstset[(tstset$` TSTCFN` %in% c('VTB','VTI','VTP','VTV','ITV')),]
tstset = tstset[(tstset$` TSTCFN` %in% c('VTB','VTI','VTP')),]
sensoutReg = sensorout

#find out how many cases do not have the correct format engineering category
vehdattest = data.frame(summary(vehdat$` VDI`))

#remove any tests with no correct engineering rating

vehdatclean = vehdat[(vehdat$` VDI` %in% regmatches(vehdat$` VDI`,regexpr("^[0-9]{1,2}[A-Za-z]{3,4}[0-9]{1,2}", vehdat$` VDI`))),]
#remove cars that arent driving,have nas or are barrier information
vehdatReg = vehdat
####check how many vehicles that have 0 initial speed have an impact velocity in the test 
#vehdatclean = vehdatclean[!(vehdatclean$` VEHSPD` == 0), ]
vehdatclean = vehdatclean[!(vehdatclean$` MAKED` == 'NHTSA'),]
vehdatReg = vehdatReg[!(vehdatReg$` MAKED` == 'NHTSA'),]
##create a variable with the VDI converted to a number between 0 and 9 and remove 0 (as it is an odd result and had no report)
vehdatclean$damrat = str_sub(vehdatclean$` VDI`,-1,-1)
vehdatclean = vehdatclean[!vehdatclean$damrat == 0,]
vehdatclean = vehdatclean[vehdatclean$TSTNO %in% tstset$TSTNO,]
#vehdatReg = vehdatReg[vehdatReg$TSTNO %in% tstReg$TSTNO,]
##extract any tests where there has been 2 cars crashed into each other at speed

#vehdatmean = vehdatclean[!vehdatclean$TSTNO %in% twoveh,]

#calculate means with t-test
summary(as.factor(vehdatclean$damrat))

summary(vehdatclean$damrat)
spdmean = by(vehdatclean$` CRHDST`,vehdatclean$damrat,t.test)
spdmean = by(vehdatclean$` VEHSPD`,vehdatclean$damrat,t.test)
spdmean <- matrix(c(unlist(spdmean[[1]][5:4]),unlist(spdmean[[2]][5:4]),unlist(spdmean[[3]][5:4]),unlist(spdmean[[4]][5:4]),
                    unlist(spdmean[[5]][5:4]),unlist(spdmean[[6]][5:4]),unlist(spdmean[[7]][5:4])),nrow = 7, byrow = T)
spdmean <- data.frame(cbind(spdmean, c('1','2','3','4','5','6','7+'))); colnames(spdmean)=c('mean','lcl','ucl','Rating')
spdmean$mean = as.numeric(as.character(spdmean$mean));spdmean$lcl = as.numeric(as.character(spdmean$lcl))
spdmean$ucl = as.numeric(as.character(spdmean$ucl)) 
#graph the means
ggplot(data = spdmean, aes(x = Rating, y = spdmean[,1]))+geom_errorbar(aes(ymin =lcl, ymax =ucl),width = .1) +
  geom_line() + geom_point() + ylab("Average Speed")+ xlab('Rating') + labs(title = "Average Speed by Rating with CI 95%")+
  theme_economist()+
  geom_hline(aes(yintercept = 49),colour="steelblue", size = 1.5)
#geom_hline(aes(yintercept = 40),colour="red", linetype="dashed", size = 1.5)

##because of the split in the data I will now categorise level 1-3 as low and 4-9 as high but due to other line i will do 2 levels
vehdatclean$DamLev[vehdatclean$damrat %in% c(1,2,3)] = "Low"   ###based on the average crush in a t-test
vehdatclean$DamLev[vehdatclean$damrat %in% c(4,5,6,7,8,9)] = "High"
vehdatclean$DamLev = as.factor(vehdatclean$DamLev)
###create SVM dataframe for SVM model
vehdatclSVM = vehdatclean
test = instdat
instdat = instdat[instdat$` SENATT` =='VECG',]
instdat = instdat[instdat$` SENTYPD` == "ACCELEROMETER",]
instdat = instdat[instdat$` CHSTATD` == "PRIMARY",]
instdatReg = instdat
#Next remove all of the tests that are not contained in the cleansed data for engineer model
#combine columns to remove
sensorout$vehid = paste(sensorout$TSTNO,sensorout$` VEHNO`)
sensorout$instid = paste(sensorout$TSTNO,sensorout$` CURNO`)
sensoutReg$vehid = paste(sensoutReg$TSTNO,sensoutReg$` VEHNO`)
sensoutReg$instid = paste(sensoutReg$TSTNO,sensoutReg$` CURNO`)

head(sensoutReg)
head(instdatReg)

vehdatclean$vehid = paste(vehdatclean$TSTNO,vehdatclean$` VEHNO`)
vehdatReg$vehid = paste(vehdatReg$TSTNO,vehdatReg$` VEHNO`)
instdat$instid = paste(instdat$TSTNO,instdat$` CURNO`)
instdatReg$instid = paste(instdatReg$TSTNO, instdatReg$` CURNO`)


sensorout = sensorout[sensorout$vehid %in% vehdatclean$vehid,]
sensorout = sensorout[sensorout$instid %in% instdat$instid,]
sensorout = sensorout[sensorout$TSTNO %in% tstset$TSTNO,]

sensoutReg = sensoutReg[sensoutReg$vehid %in% vehdatReg$vehid,]
sensoutReg = sensoutReg[sensoutReg$instid %in% instdatReg$instid,] ###this line is the problem 
sensoutReg = sensoutReg[sensoutReg$TSTNO %in% tstReg$TSTNO,]

unique(sensoutReg$TSTNO)
unique(sensorout$TSTNO)

colnames(sensorout) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD", "VEHNO")
colnames(sensoutReg) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD", "VEHNO")
sensorout <- subset(sensorout, select = c(1:9) )
sensoutReg <- subset(sensoutReg, select = c(1:9) )
summary(sensorout$AXIS)
summary(sensoutReg$AXIS)
sensorout$AXIS = as.character(sensorout$AXIS)
sensoutReg$AXIS = as.character(sensoutReg$AXIS)

sensorout$AXIS[startsWith(sensorout$AXIS, "X")] = "X"
sensorout$AXIS[startsWith(sensorout$AXIS, "Y")] = "Y"
sensorout$AXIS[startsWith(sensorout$AXIS, "Z")] = "Z"

sensoutReg$AXIS[startsWith(sensoutReg$AXIS, "X")] = "X"
sensoutReg$AXIS[startsWith(sensoutReg$AXIS, "Y")] = "Y"
sensoutReg$AXIS[startsWith(sensoutReg$AXIS, "Z")] = "Z"

###################################Data Re-Shaping##################################################################
resh <- reshape(data = sensorout, timevar = "AXIS",
                idvar = c("TSTNO","Time","VEHNO"),
                drop = c("Signal","AXISD","CURNO", "SENATT"), 
                direction = "wide")
summary(as.factor(resh$Force.X))

testdat =  resh[complete.cases(resh), ]
unique(testdat$TSTNO)

#create a column that has the sum of the absolute value of the xyz data
testdat$absum = abs(testdat$Force.X) + abs(testdat$Force.Y)+ abs(testdat$Force.Z)
testdat$mag = sqrt( (testdat$Force.X^ 2) + (testdat$Force.Y ^ 2) + (testdat$Force.Z ^ 2))

dfmag = testdat %>% group_by(TSTNO) %>% top_n(1, mag)
##got some strange results with repeat max showing there was duplicates in 700 and 703
checkdata = instdat[instdat$TSTNO == 700,]
checkdata
checkdata = sensorout[sensorout$TSTNO == 709,]
##two examples of questionable data below -- need to label correctly etc.
plot(checkdata$Time, checkdata$Force,xlab = "Time", ylab = "G-force")
plot(testdat$Time[testdat$TSTNO == 5470], testdat$Force.Z[testdat$TSTNO == 5470], xlab = "Time", ylab = "G-force")
#example of a good plot of data
plot(testdat$Time[testdat$TSTNO == 3845], testdat$Force.X[testdat$TSTNO == 3845], xlab = "Time", ylab = "G-force")
testdat = testdat[!(testdat$TSTNO %in% c(700,709) ),]
#dfabsum = testdat %>% group_by(TSTNO) %>% top_n(1, absum)
dfmag = testdat %>% group_by(TSTNO) %>% top_n(1, mag)
dfmag$vehid = paste(dfmag$TSTNO,dfmag$VEHNO)
dfmag = data.frame(dfmag[,c(1:9)], initialspeed=tstset[match(dfmag$TSTNO, tstset$TSTNO), 23])
dfmag = data.frame(dfmag[,c(1:10)], vehwht=vehdatclean[match(dfmag$vehid, vehdatclean$vehid), 17])

##need to check if the dataframe has enough occurances 
checktest = testdat %>% group_by(TSTNO) %>% summarise(no_rows = length(TSTNO))
testdat = testdat[!(testdat$TSTNO %in% c(2663,6928,5408,6979,5405, 5408,6286,1804,5470) ),]
dfmag = dfmag[!(dfmag$TSTNO %in% c(2663,6928,5408,6979, 5405,5408,6286,1804,5470)),]


for (i in dfmag$TSTNO){
  
  ##use the model to get the velocity and trajectory
  DatVelTraj = VelTraj(testdat$Force.X[testdat$TSTNO == i ], testdat$Force.Y[testdat$TSTNO == i ],
                       dfmag$initialspeed[dfmag$TSTNO == i],testdat$Time[testdat$TSTNO == i])
  DatVelTraj = DatVelTraj[1:length(testdat$Force.X[testdat$TSTNO == i]),]
  
  testdat$vel[testdat$TSTNO == i] = DatVelTraj$vel
  testdat$traj[testdat$TSTNO == i] = DatVelTraj$traj
  
  
  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  results = momentum(ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z,
                     dfmag$vehwht[dfmag$TSTNO == i],f.imp$mag)
  results$TSTNO = i
  results$GTAngle = tstset$` IMPANG`[tstset$TSTNO == i]
  angle = DirectionCat(tstset$` IMPANG`[tstset$TSTNO == i])
  pdof = DirectionCat(vehdatclean$` PDOF`[vehdatclean$vehid == dfmag$vehid[dfmag$TSTNO == i]])
  results$GTAngCat = angle$impact_zone
  results$PDOF = pdof$impact_zone
  results$GTseverity = vehdatclean$DamLev[vehdatclean$TSTNO == i]
  
  
  if (exists('totalresults') == TRUE) {
    totalresults = rbind(totalresults,results)
  }  else{
    totalresults = results
  } 
}