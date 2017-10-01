library('RODBC')
?RODBC
dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-PAPDF3KG\\SQLEXPRESS;database=CrashData;trusted_connection=true')
test <- sqlQuery(dbhandle, 'select * from dbo.Sensor_Ouput')
colnames(test) = c("Signal", "Time","Force", "TSTNO","CURNO","SENATT","AXIS","AXISD")

test = test[test$TSTNO == 6,]

test
