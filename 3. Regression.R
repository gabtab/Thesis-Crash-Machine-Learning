###need to remove the Nas first
sensoutReg = sensoutReg[!is.na(sensoutReg$AXIS),]
sensoutReg$AXIS = as.factor(sensoutReg$AXIS)
summary(sensoutReg$AXIS)

reshReg <- reshape(data = sensoutReg, timevar = "AXIS",
                idvar = c("TSTNO","Time","VEHNO"),
                drop = c("Signal","AXISD","CURNO", "SENATT"), 
                direction = "wide")

dfReg =  reshReg[complete.cases(reshReg), ]
unique(dfReg$TSTNO)

#create a column that has the sum of the absolute value of the xyz data
dfReg$mag = sqrt( (dfReg$Force.X^ 2) + (dfReg$Force.Y ^ 2) + (dfReg$Force.Z ^ 2))

dfReg = dfReg %>% group_by(TSTNO) %>% top_n(1, mag)
dfReg$vehid = paste(dfReg$TSTNO,dfReg$VEHNO)
dfReg = data.frame(dfReg[,c(1:8)], initialspeed=tstReg[match(dfReg$TSTNO, tstReg$TSTNO), 23])
dfReg = data.frame(dfReg[,c(1:9)], vehwt=vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 17])
dfReg = data.frame(dfReg[,c(1:10)], vehlen =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 20])
dfReg = data.frame(dfReg[,c(1:11)], vehwid =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 21])
dfReg = data.frame(dfReg[,c(1:12)], impangle =tstReg[match(dfReg$TSTNO, tstReg$TSTNO), 24])

describe(dfReg)
describe(tstReg)
describe(vehdatReg)


testReg = lm(impangle ~ Force.X * Force.Y * Force.Z * mag * initialspeed * vehwt * vehlen * vehwid, data = dfReg)
summary(testReg)
drop1(testReg, test = 'F')
update(testReg, .~. -Force.X:Force.Y:Force.Z:mag:initialspeed:vehwt:vehlen:vehwid)
drop1(testReg, test = 'F')
update(testReg, .~. -Force.X:Force.Y:Force.Z:mag:initialspeed:vehwt:vehlen:vehwid)
drop1(testReg, test = 'F')

unique(dfReg$impangle)
