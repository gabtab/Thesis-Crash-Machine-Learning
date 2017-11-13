
library(dplyr)
library(reshape2)
library(data.table)
library('RODBC')
library('stringr')
library('ggplot2')
library('ggthemes')
library('data.table')

##################EXTRACT DATA FROM DATABASE#####################################################################

dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-PAPDF3KG\\SQLEXPRESS;database=CrashData;trusted_connection=true')
sensorout <- sqlQuery(dbhandle, 'select * from dbo.Sensor_Output')
tstset <- sqlQuery(dbhandle, 'select * from dbo.tst')
vehdat <- sqlQuery(dbhandle, 'select * from dbo.veh')
instdat <- sqlQuery(dbhandle, 'select * from dbo.instr')

##################START DATACLEANING ###########################################################################
#remove any tests with no rating
vehdatclean = vehdat[(vehdat$` VDI` %in% regmatches(vehdat$` VDI`,regexpr("^[0-9]{1,2}[A-Za-z]{3,4}[0-9]{1,2}", vehdat$` VDI`))),]
#remove cars that arent driving,have nas or are barrier information
vehdatclean = vehdatclean[complete.cases(vehdatclean[ , 51]),]
vehdatclean = vehdatclean[!(vehdatclean$` VEHSPD` == 0), ]
vehdatclean = vehdatclean[!(vehdatclean$` MAKED` == 'NHTSA'),]
##create a variable with the VDI converted to a number between 0 and 9 and remove 0 (as it is an odd result and had no report)
vehdatclean$damrat = str_sub(vehdatclean$` VDI`,-1,-1)
vehdatclean = vehdatclean[!vehdatclean$damrat == 0,]

#####need to chart out the differences and determine what is high and low############
#calculate means with t-test
summary(as.factor(vehdatclean$damrat))

summary(vehdatclean$damrat)
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
  geom_hline(aes(yintercept = 50),colour="steelblue", linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 45),colour="red", linetype="dashed", size = 1.5)

##because of the split in the data I will now categorise level 1-3 as low and 4-9 as high but due to other line i will do 2 levels
vehdatclean$DamLevhigh[vehdatclean$damrat %in% c(1,2,3)] = "Low"
vehdatclean$DamLevhigh[vehdatclean$damrat %in% c(4,5,6,7,8,9)] = "High"
vehdatclean$DamLevlow[vehdatclean$damrat %in% c(1)] = "Low"
vehdatclean$DamLevlow[vehdatclean$damrat %in% c(2,3,4,5,6,7,8,9)] = "High"
###sensor data###
instdat = instdat[instdat$` SENATT` =='VECG',]
instdat = instdat[instdat$` SENTYPD` == "ACCELEROMETER",]
instdat = instdat[instdat$` CHSTATD` == "PRIMARY",]

#Next remove all of the tests that are not contained in the cleansed data
#combine columns to remove
sensorout$vehid = paste(sensorout$TSTNO,sensorout$` VEHNO`)
sensorout$instid = paste(sensorout$TSTNO,sensorout$` CURNO`)

vehdatclean$vehid = paste(vehdatclean$TSTNO,vehdatclean$` VEHNO`)
instdat$instid = paste(instdat$TSTNO,instdat$` CURNO`)

###
sensorout = sensorout[sensorout$vehid %in% vehdatclean$vehid,]
sensorout = sensorout[sensorout$instid %in% instdat$instid,]

unique(sensorout$TSTNO)


colnames(sensorout) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD", "VEHNO")
sensorout <- subset(sensorout, select = c(1:9) )
summary(sensorout$AXIS)
sensorout$AXIS = as.character(sensorout$AXIS)

sensorout$AXIS[startsWith(sensorout$AXIS, "X")] = "X"
sensorout$AXIS[startsWith(sensorout$AXIS, "Y")] = "Y"
sensorout$AXIS[startsWith(sensorout$AXIS, "Z")] = "Z"

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
plot(checkdata$Time, checkdata$Force)

testdat = testdat[!(testdat$TSTNO %in% c(700,709) ),]
#dfabsum = testdat %>% group_by(TSTNO) %>% top_n(1, absum)
dfmag = testdat %>% group_by(TSTNO) %>% top_n(1, mag)
dfmag$vehid = paste(dfmag$TSTNO,dfmag$VEHNO)
dfmag = data.frame(dfmag[,c(1:9)], initialspeed=vehdatclean[match(dfmag$vehid, vehdatclean$vehid), 51])
dfmag = data.frame(dfmag[,c(1:10)], vehwht=vehdatclean[match(dfmag$vehid, vehdatclean$vehid), 17])

source('Functions.R')
source('Numeric Operations and Engineering Model.R')


##################have all the data together just need to get it all through the functions --------



##use the model to get the velocity and trajectory
DatVelTraj = VelTraj(Force.X, Force.Y,initialspeed,Time)
DatVelTraj = DatVelTraj[1:length(Force.X),]
newdata = cbind(car1,DatVelTraj)

ind.imp <- which.max(mag)
f.imp <- newdata[(which.max(mag)),]

results = momentum(ind.imp,Time,accelX ,accelY ,accelZ ,mass,mag)

resultscom = tstset[tstset$TSTNO == 6832,]
resultsveh = vehdat[vehdat$TSTNO == 6832,]


