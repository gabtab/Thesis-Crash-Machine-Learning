

results = NULL
totalresults = NULL
outputtable = NULL
outputtable1 = NULL
outputtable2 = NULL
output3= NULL


for (i in dfmag$TSTNO){

  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  if (dfmag$vehwht[dfmag$TSTNO == i] == 0) {
        dfmag$vehwht[dfmag$TSTNO == i] = mean(vehdatclean$` VEHTWT`[vehdatclean$` BODY`[vehdatclean$TSTNO == i]])
    }
  
    results = momentum(ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z,
                     dfmag$vehwht[dfmag$TSTNO == i],f.imp$mag)
  results$TSTNO = i
  results$GTAngle = tstset$` IMPANG`[tstset$TSTNO == i]
  
  results$pdof = vehdatclean$` PDOF`[vehdatclean$vehid == dfmag$vehid[dfmag$TSTNO == i]]
  angle = DirectionCat(results$pdof)
  #pdof = DirectionCat(vehdatclean$` PDOF`[vehdatclean$vehid == dfmag$vehid[dfmag$TSTNO == i]])
  results$GTAngCat = angle$impact_zone
  #results$PDOF = pdof$impact_zone
  results$GTseverity = vehdatclean$DamLev[vehdatclean$TSTNO == i]
  results$EngSev = vehdatclean$damrat[vehdatclean$TSTNO == i]
  results$maxmag = dfmag$mag[dfmag$TSTNO == i]
  results$initialspeed = dfmag$initialspeed[dfmag$TSTNO == i]
  
  if (exists('totalresults') == TRUE) {
    totalresults = rbind(totalresults,results)
  }  else{
    totalresults = results
  } 
}
#write.xlsx(totalresults, "engresults.xlsx", sheetName = "Test")

####This is after talking to Sean -- need to get the relationship between the classificaiton and the magnitude
ggplot(data = totalresults, aes(x = EngSev, y = crash_mag)) + geom_point() + 
  ylab("Magnitude")+ xlab('Engineering Severity') + labs(title = "Magnitude (m/s) versus Engineering Severity (Rating)")+
  theme_economist()

ggplot(data = totalresults, aes(x = EngSev, y = maxmag)) + geom_point() + 
  ylab("Maximum g-force")+ xlab('Engineering Severity') + labs(title = "Maximum g-force versus Engineering Severity (Rating)")+
  theme_economist()

ggplot(data = totalresults, aes(x = EngSev, y = initialspeed)) + geom_point() + 
  ylab("Initial Speed")+ xlab('Engineering Severity') + labs(title = "Initial Speed versus Engineering Severity (Rating)")+
  theme_economist()


###### mean graph requested by sean####################
totalresults$EngSev[totalresults$EngSev %in% c(6,7,8,9,10)] ="6+"

magmean = by(totalresults$maxmag,totalresults$EngSev,t.test)
outputlevels = data.frame(summary(as.factor(totalresults$EngSev)))

magmean <- matrix(c(unlist(magmean[[1]][5:4]),unlist(magmean[[2]][5:4]),unlist(magmean[[3]][5:4]),unlist(magmean[[4]][5:4]),
                    unlist(magmean[[5]][5:4]),unlist(magmean[[6]][5:4])),nrow = 6, byrow = T)
magmean <- data.frame(cbind(magmean, c('1','2','3','4','5','6+'))); colnames(magmean)=c('mean','lcl','ucl','Rating')
magmean$mean = as.numeric(as.character(magmean$mean));magmean$lcl = as.numeric(as.character(magmean$lcl))
magmean$ucl = as.numeric(as.character(magmean$ucl)) 
ggplot(data = magmean, aes(x = Rating, y = mean)) + geom_point() + geom_errorbar(aes(ymin =lcl, ymax =ucl)) +
  ylab("Magnitude")+ xlab('Engineering Severity') + labs(title = "Magnitude (m/s) versus Engineering Severity (Rating)")+
  theme_economist()

levelmod = data.frame(summary(as.factor(totalresults$EngSev)))
outputtable = confusionMatrix(totalresults$severity,totalresults$GTseverity)
outputtable

totalresults$severity <- factor(totalresults$severity, levels=c("Low", "High"), ordered=TRUE)
totalresults$GTseverity <- factor(totalresults$GTseverity, levels=c("Low", "High"), ordered=TRUE)
outputroc = roc(totalresults$GTseverity, totalresults$severity)


############# Want to see what the % correct is in the total results tab for engineering model when obs are set to 125 and then for 5

####try up sampling because there is not enough highs

upsampledat = upSample(totalresults, as.factor(totalresults$EngSev))
summary(as.factor(upsampledat$EngSev))
outputtable2 = confusionMatrix(upsampledat$severity,upsampledat$GTseverity)
outputtable2

upsampledat$severity <- factor(upsampledat$severity, levels=c("Low", "High"), ordered=TRUE)
upsampledat$GTseverity <- factor(upsampledat$GTseverity, levels=c("Low", "High"), ordered=TRUE)
  outputroc2 = roc(upsampledat$GTseverity, upsampledat$severity)

# ####try dropping the levels 5 6+
# upsampledatlesshigh = upsampledat[-(upsampledat$EngSev %in% c("5","6+")),]
# outputtable = confusionMatrix(upsampledatlesshigh$severity,upsampledatlesshigh$GTseverity)
# outputtable

##graph this -
upsampledat$EngSev[upsampledat$EngSev %in% c(6,7,8,9,10)] ="6+"
upmagmean = by(upsampledat$maxmag,upsampledat$EngSev,t.test)
uplevels = data.frame(summary(as.factor(upsampledat$EngSev)))

upmagmean <- matrix(c(unlist(upmagmean[[1]][5:4]),unlist(upmagmean[[2]][5:4]),unlist(upmagmean[[3]][5:4]),unlist(upmagmean[[4]][5:4]),
                    unlist(upmagmean[[5]][5:4]),unlist(upmagmean[[6]][5:4])),nrow = 6, byrow = T)
upmagmean <- data.frame(cbind(upmagmean, c('1','2','3','4','5','6+'))); colnames(upmagmean)=c('mean','lcl','ucl','Rating')
upmagmean$mean = as.numeric(as.character(upmagmean$mean));upmagmean$lcl = as.numeric(as.character(upmagmean$lcl))
upmagmean$ucl = as.numeric(as.character(upmagmean$ucl)) 
ggplot(data = upmagmean, aes(x = Rating, y = mean)) + geom_point() + geom_errorbar(aes(ymin =lcl, ymax =ucl)) +
  ylab("Magnitude")+ xlab('Engineering Severity') + labs(title = "Magnitude (m/s) versus Engineering Severity (Rating)")+
  theme_economist()

#################################Next check the angle of impact
summary(as.factor(totalresults$pdof))
  outputtable = confusionMatrix(totalresults$impact_zone,totalresults$GTAngCat)

  summary(totalresults$GTAngCat)
  summary(totalresults$impact_zone)
  u = union(totalresults$impact_zone, totalresults$GTAngCat)
  t = table(factor(totalresults$impact_zone, u), factor(totalresults$GTAngCat, u))
  output3 = confusionMatrix(t)
  output3
  levels(totalresults$impact_zone)
  
  totalresults$impact_zone <- factor(totalresults$impact_zone, levels=c("FC", "SL","SR","BC"), ordered=TRUE)
  totalresults$GTAngCat <- factor(totalresults$GTAngCat, levels=c("FC", "SL","SR","BC"), ordered=TRUE)
  outputroc3 = multiclass.roc(totalresults$GTAngCat, totalresults$impact_zone)
  

finalresultsev = data.frame(matrix(unlist(c("Severity no upsample", round(as.numeric(outputtable$overall['Accuracy']),digits =2), 
                 round(as.numeric(outputtable$overall['AccuracyLower']),digits = 2),
                 round(as.numeric(outputtable$overall['AccuracyUpper']),digits = 2),
                 round(as.numeric(outputtable$byClass['Sensitivity']),digits = 2), 
                 round(as.numeric(outputtable$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(finalresultsev)   = c("model", "Acc","Lwr","Upr","Sen","Spc")  
model2 = data.frame(matrix(unlist(c("Severity w upsample", round(as.numeric(outputtable2$overall['Accuracy']),digits =2), 
                                          round(as.numeric(outputtable2$overall['AccuracyLower']),digits = 2),
                                          round(as.numeric(outputtable2$overall['AccuracyUpper']),digits = 2),
                                          round(as.numeric(outputtable2$byClass['Sensitivity']),digits = 2), 
                                          round(as.numeric(outputtable2$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(model2)   = c("model", "Acc","Lwr","Upr","Sen","Spc")#

model3 = data.frame(matrix(unlist(c("PDOF w upsample", round(as.numeric(output3$overall['Accuracy']),digits =2), 
                                    round(as.numeric(output3$overall['AccuracyLower']),digits = 2),
                                    round(as.numeric(output3$overall['AccuracyUpper']),digits = 2),
                                    round(as.numeric(output3$byClass['Sensitivity']),digits = 2), 
                                    round(as.numeric(output3$byClass['Specificity']),digits = 2))),nrow =1 , byrow =T))
colnames(model3)   = c("model", "Acc","Lwr","Upr","Sen","Spc")
finalresultsev = rbind(finalresultsev, model2,model3)
finalresultsev





