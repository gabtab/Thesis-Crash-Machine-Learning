getwd()

library('RODBC')

dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-PAPDF3KG\\SQLEXPRESS;database=CrashData;trusted_connection=true')
test <- sqlQuery(dbhandle, 'select * from dbo.Sensor_Ouput')
colnames(test) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD")

#test = test[test$TSTNO == 6,]
#write.table(test, "test1.txt", sep="\t")
test1 = test[test$TSTNO == 6,]
resh <- reshape(data = test1, timevar = "AXIS",
                idvar = c("TSTNO","Time"),
                drop = c("Signal","AXISD","CURNO", "SENATT"), 
                direction = "wide")
?reshape


test1 = test[test$TSTNO == 6:100,]

memory.limit(test)
resh = NULL
