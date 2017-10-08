
library(dplyr)
library(reshape2)
library(data.table)
library('RODBC')

dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-PAPDF3KG\\SQLEXPRESS;database=CrashData;trusted_connection=true')
test <- sqlQuery(dbhandle, 'select * from dbo.Sensor_Output')
colnames(test) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD", "VEHNO")


#write.table(test, "test1.txt", sep="\t")
test1 = test[test$TSTNO == 6832,]
resh <- reshape(data = test1, timevar = "AXIS",
                idvar = c("TSTNO","Time","VEHNO"),
                drop = c("Signal","AXISD","CURNO", "SENATT"), 
                direction = "wide")

summary(resh)
unique(test1$AXIS)
unique(test$TSTNO)
#memory.limit(test)

#create a subset that will be of an individual test
testdat = subset(resh, TSTNO == 6832)
#create a column that has the sum of the absolute value of the xyz data
testdat$abssum = abs(testdat$Force.XG) + abs(testdat$Force.YG) +abs(testdat$Force.ZG)
#get the row that absolute value of the xyz is at a maximum for the impact

car1 = testdat[which(testdat$VEHNO == 1),]
car2 = testdat[which(testdat$VEHNO == 2),]
maxallforce1 = car1[which.max(car1$abssum),]
maxallforce2 = car2[which.max(car2$abssum),]

#car1[250:280,]
############velocity and trajectory of vehicle###################

source('Functions.R')
source('Numeric Operations and Engineering Model.R')
initialspeed = 96.8
attach(car1)

DatVelTraj = VelTraj(Force.XG, Force.YG,initialspeed) 
DatVelTraj = DatVelTraj[1:length(car1$Force.XG),]
newdata = cbind(car1,DatVelTraj)

##print out these charts to show how the data looks
attach(newdata)
plot(newdata$Time, newdata$vel)
plot(newdata$Time, newdata$traj)
head(car1)


summary(y_vel)
summary(x_vel)
summary(trajectory)
max(velocity)

