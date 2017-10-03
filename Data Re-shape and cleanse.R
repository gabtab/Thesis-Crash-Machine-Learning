getwd()
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
#test1 = test[test$TSTNO == 6:100,]
unique(test1$AXIS)
unique(test$TSTNO)
#memory.limit(test)

#############Create impact time#################
#create a subset that will be of an individual test
testdat = subset(resh, TSTNO == 6832)
#create a column that has the sum of the absolute value of the xyz data
testdat$abssum = abs(testdat$Force.XG) + abs(testdat$Force.YG) +abs(testdat$Force.ZG)
#get the row that absolute value of the xyz is at a maximum for the impact

car1 = testdat[which(testdat$VEHNO == 1),]
car2 = testdat[which(testdat$VEHNO == 2),]
maxallforce1 = testdat[which.max(car1$abssum),]
maxallforce2 = testdat[which.max(car2$abssum),]

maxxforce = testdat[which.max(abs(testdat$Force.XG)),]
maxyforce = testdat[which.max(abs(testdat$Force.YG)),]

max(testdat$Time)
impacttime = maxrow$Time
impacttim <- function(timp, fimp)