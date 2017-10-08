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
unique(test1$AXIS)
unique(test$TSTNO)
#memory.limit(test)



