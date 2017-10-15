
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
testdat$absum = abs(testdat$Force.XG) + abs(testdat$Force.YG)+ abs(testdat$Force.ZG)
testdat$mag = sqrt( (testdat$Force.XG^ 2) + (testdat$Force.YG ^ 2) + (testdat$Force.ZG ^ 2))
#get the row that absolute value of the xyz is at a maximum for the impact

car1 = testdat[which(testdat$VEHNO == 1),]
car2 = testdat[which(testdat$VEHNO == 2),]
maxallforceabssum = car1[which.max(car1$absum),]
maxallforcemag = car1[which.max(car1$mag),]


#car1[250:280,]
############velocity and trajectory of vehicle###################

source('Functions.R')
source('Numeric Operations and Engineering Model.R')
initialspeed = 96.8

attach(car1)
gravity = 9.80665
##use the model to get the velocity and trajectory
DatVelTraj = VelTraj(Force.XG, Force.YG,initialspeed,Time)
DatVelTraj = DatVelTraj[1:length(Force.XG),]
newdata = cbind(car1,DatVelTraj)

ind.imp <- which.max(mag)
f.imp <- newdata[(which.max(mag)),]

# Index of impact
ind.imp <- which.max(mag)  
## this is really important and need to get full understanding of the meaning behind the above function/logic
crash.points <- sort(c(ind.imp + seq(from = 0, to = floor(0.04 / timeit(Time,ind.imp)), by = 1)))
##i have intitially set this up so it will have an end time that looks like when the momentum is decreasing
endtime = newdata[which.max(mag) + floor(0.04/ timeit(Time,ind.imp)),]
# Get accelerometer data for crash window
crash.acc.x <- gravity * Force.XG[c(crash.points)]
crash.acc.y <- gravity * Force.YG[c(crash.points)]
crash.acc.z <- gravity * Force.ZG[c(crash.points)]
##integrate each axis using the trapizoid rule step 1
traps.x <- 2 * sum(crash.acc.x[2:(length(crash.acc.x) - 1)])
traps.x <- traps.x + crash.acc.x[1] + crash.acc.x[length(crash.acc.x)]
traps.y <- 2 * sum(crash.acc.y[2:(length(crash.acc.y) - 1)])
traps.y <- traps.y + crash.acc.y[1] + crash.acc.y[length(crash.acc.y)]
traps.z <- 2 * sum(crash.acc.z[2:(length(crash.acc.z) - 1)])
traps.z <- traps.y + crash.acc.z[1] + crash.acc.z[length(crash.acc.z)]

mass = 1736
##integrate each axis using the trapizoid rule step 2
mom.x.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.x
mom.y.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.y
mom.z.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.z
#calculate the direciton of the momentum vector
direction.post <- atan2(y = mom.x.crash, x = mom.y.crash)
direction.post <- direction.post * 180 / pi
##this handles negative angles
if (direction.post < 0) {
  direction.post <- 360 - abs(direction.post)
}

# Where was the car hit from
if (direction.post > 0 && direction.post < 53) {
  impact_zone <- "SL"
  crash_type <- "Side Impact"
} else if (direction.post >= 53 && direction.post <= 80) {
  impact_zone <- "BL"
  crash_type <- "Corner Impact"
} else if (direction.post > 80 && direction.post < 100) {
  impact_zone <- "BC"
  crash_type <- "Rear Impact"
} else if (direction.post >= 100 && direction.post <= 127) {
  impact_zone <- "BR"
  crash_type <- "Corner Impact"
} else if (direction.post > 127 && direction.post < 233) {
  impact_zone <- "SR"
  crash_type <- "Side Impact"
} else if (direction.post >= 233 && direction.post <= 260) {
  impact_zone <- "FR"
  crash_type <- "Corner Impact"
} else if (direction.post > 260 && direction.post < 280) {
  impact_zone <- "FC"
  crash_type <- "Front Impact"
} else if (direction.post >= 280 && direction.post <= 307) {
  impact_zone <- "FL"
  crash_type <- "Corner Impact"
} else if (direction.post > 307 && direction.post <= 360) {
  impact_zone <- "SL"
  crash_type <- "Side Impact"
}
crash_type
impact_zone

finalmag = sqrt(mom.x.crash ^ 2 + mom.y.crash ^ 2 + mom.z.crash ^2) / mass 


if (finalmag > 2.5) {severity = "high"}
{severity = "not high"}



R = sqrt(p_x .^ 2 + p_y .^ 2); 





?sort
?floor
##print out these charts to show how the data looks
attach(newdata)
plot(newdata$Time, newdata$vel)
plot(newdata$Time, newdata$traj)
head(car1)


summary(y_vel)
summary(x_vel)
summary(trajectory)
max(velocity)

