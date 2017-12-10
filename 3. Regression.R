###need to remove the Nas first

sensoutReg = sensoutReg[!is.na(sensoutReg$AXIS),]
sensoutReg$AXIS = as.factor(sensoutReg$AXIS)
sensoutReg$VEHNO = as.factor(sensoutReg$VEHNO)
summary(sensoutReg$AXIS)

reshReg <- reshape(data = sensoutReg, timevar = "AXIS",
                idvar = c("TSTNO","Time","VEHNO"),
                drop = c("Signal","AXISD","CURNO", "SENATT"), 
                direction = "wide")
describe(reshReg)

head(reshReg)
dfReg =  reshReg[complete.cases(reshReg), ]
unique(dfReg$TSTNO)
describe(dfReg)
#create a column that has the sum of the absolute value of the xyz data
dfReg$mag = sqrt( (dfReg$Force.X^ 2) + (dfReg$Force.Y ^ 2) + (dfReg$Force.Z ^ 2))

dfReg = dfReg %>% group_by(TSTNO) %>% top_n(1, mag)
dfReg$vehid = paste(dfReg$TSTNO,dfReg$VEHNO)
dfReg = data.frame(dfReg[,c(1:8)], initialspeed=tstReg[match(dfReg$TSTNO, tstReg$TSTNO), 23])
dfReg = data.frame(dfReg[,c(1:9)], vehwt=vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 17])
dfReg = data.frame(dfReg[,c(1:10)], vehlen =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 20])
dfReg = data.frame(dfReg[,c(1:11)], vehwid =vehdatReg[match(dfReg$vehid, vehdatReg$vehid), 21])
dfReg = data.frame(dfReg[,c(1:12)], impangle =tstReg[match(dfReg$TSTNO, tstReg$TSTNO), 24])

##instead of taking out the exact cases that were in the engineering model i need to randomise the data and then test on the same number as the 
## engineering model
# dfVal = dfReg[(dfReg$TSTNO %in% dfmag$TSTNO),]
# dfReg = dfReg[!(dfReg$TSTNO %in% dfmag$TSTNO),]

dfVal = dfReg[sample(nrow(dfmag)),]
dfReg = dfReg %>% filter(!(TSTNO %in% dfVal$TSTNO))

describe(dfReg)
describe(tstReg)
describe(vehdatReg)
dfReg =  dfReg[complete.cases(dfReg), ]
###Try two methods of fitting the model 

#####method 1 stepwise -- not great but it gives a reasonable model
# fit = lm(impangle ~ 1, data = dfReg)
# fit1b = lm(impangle ~Force.X + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid, data = dfReg)
# step(fit1b, scope = list(lower = ~1, upper = ~Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid),
#      direction = "backward", trace =1)
# step(fit, scope = list(lower = ~1, upper = ~Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid),
#      direction = "forward", trace =1)
# testmodel = step(fit, scope = list(lower = ~1, upper = ~Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid),
#      direction = "both", trace =1)
# 
# coef(testmodel)
# 
# RegResults$fit= predict(testmodel, newdata = dfVal, se.fit = T)
# testdf <- data.frame(matrix(unlist(RegResults), byrow=T))
# finalresult = data.frame()

# ##########################NEED TO FINISH GETTING THE RESULTS INTO A DATA FRAMe
# for (i in dfVal$TSTNO){
#   i = 1
#   finalresult$TSTNO[i] = dfVal$TSTNO[i]
#   finalresult$GTAngle = tstset$` IMPANG`[tstset$TSTNO == dfVal$TSTNO[i]]
#   angle = DirectionCat(RegResults$se.fit[i]) ##gives one result but need to loop through it 
#   pdof = DirectionCat(vehdatclean$` PDOF`[vehdatclean$vehid == dfmag$vehid[dfmag$TSTNO == dfVal$TSTNO[i]]])
#   finalresult$GTAngCat = angle$impact_zone
#   results$PDOF = pdof$impact_zone
#   results$GTseverity = vehdatclean$DamLev[vehdatclean$TSTNO == i]
#   
# }


##################Getting bad results so look at mulitnomial logistic regression #######
dfReg$impangle = as.factor(dfReg$impangle)

dfReg$impangle2 <- relevel(dfReg$impangle, ref = "270")
test <- multinom(impangle2 ~ Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid, data = dfReg)
summary(test)

fit = multinom(as.factor(impangle) ~ 1, data = dfReg)
fit1b = multinom(as.factor(impangle) ~Force.X + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid, data = dfReg)
testmodel = step(fit, scope = list(lower = ~1, upper = ~Force.X  + Force.Y + Force.Z + mag + initialspeed + vehwt + vehlen + vehwid),
                 direction = "both", trace =1)
summary(testmodel)
z <- summary(testmodel)$coefficients/summary(testmodel)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2 ## not sure this is relevant
p  
####
RegResults1 = predict(testmodel, newdata = dfVal, se.fit = T)


 
 for (i in dfVal$TSTNO){
   resultreg = dfVal[dfVal$TSTNO == i,]
   i = 2515
   resultreg$TSTNO = i
   resultreg$GTAngle = tstReg$` IMPANG`[tstReg$TSTNO ==  i]
   angle = DirectionCat(as.numeric(RegResults1[1])) ##gives one result but need to loop through it 
   pdof = DirectionCat(vehdat$` PDOF`[vehdat$vehid == dfVal$vehid[dfVal$TSTNO == i]])
   resultreg$RegAngCat = angle$impact_zone
   resultreg$PDOF = pdof$impact_zone
   resultreg$GTseverity = vehdat$DamLev[vehdat$TSTNO == i]
   if (exists('RegTotalResults') == TRUE) {
     RegTotalResults = rbind(RegTotalResults,resultreg)
   }  else{
     RegTotalResults = resultreg
   } 
 }
 
                     
                     
                     
# vehdat[vehdat$vehid == 2515,]                    
#                      
# tstset[tstset$TSTNO == 6,]                     
# tstset[(tstset$TSTNO ==1 ),]                     
#                      
#####method 2 Lasso 
# preds = cbind(dfReg$Force.X, dfReg$Force.Y, dfReg$Force.Z, dfReg$mag, dfReg$initialspeed, dfReg$vehwt, dfReg$vehlen, dfReg$vehwid)
# 
# fit_OLS = lm(dfReg$impangle ~ preds)
# ###lambda = 0
# lasso_fit1 = glmnet(preds, dfReg$impangle, family = "gaussian", alpha = 1, lambda  = 0)
# ##compare with ordinary least squares
# cbind(fit_OLS$coefficients, coef(lasso_fit1))
# ##large lambda
# lasso_fit2 = glmnet(preds,dfReg$impangle, family = "gaussian", alpha =1, lambda = 10000)
# 
# cbind(fit_OLS$coefficients, coef(lasso_fit1),coef(lasso_fit2))
# ##use cross validation to find optimal lambda value
# set.seed(12345)
# cv = cv.glmnet(preds, dfReg$impangle, family = "gaussian", alpha = 1)
# plot(cv)
# cv$lambda.min; cv$lambda.1se;abline(h = 3.79, col = 'blue')
# lasso_fit= glmnet(preds, dfReg$impangle, family = "gaussian", alpha = 1, lambda = cv$lambda.1se)
# coef(lasso_fit)
# 
# 
# 
# #### not sure if I can use the above coz i don't really understand it
# 
# testReg = lm(impangle ~ Force.X * Force.Y * Force.Z * mag * initialspeed * vehwt * vehlen * vehwid, data = dfReg)
# summary(testReg)
# drop1(testReg, test = 'F')
# update(testReg, .~. -Force.X:Force.Y:Force.Z:mag:initialspeed:vehwt:vehlen:vehwid)
# drop1(testReg, test = 'F')
# update(testReg, .~. -Force.X:Force.Y:Force.Z:mag:initialspeed:vehwt:vehlen:vehwid)
# drop1(testReg, test = 'F')
# 
 summary(as.factor(dfVal$impangle))
