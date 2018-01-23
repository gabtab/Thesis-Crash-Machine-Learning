

results = NULL
totalresults = NULL
outputtable = NULL
outputtable1 = NULL

for (i in dfmag$TSTNO){
  
  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
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

testdat1 = testdat[testdat$TSTNO == 7,]
summary(as.factor(diff(testdat$Time)))

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
totalresults$EngSev[totalresults$EngSev %in% c(6,7,8,9,10)] ="7+"
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


upsampledat = upSample(totalresults, totalresults$severity)
summary(upsampledat$severity)
outputtable = confusionMatrix(totalresults$severity,totalresults$GTseverity)
############# Want to see what the % correct is in the total results tab for engineering model when obs are set to 125 and then for 5

outputtable = confusionMatrix(totalresults$severity,totalresults$GTseverity)
no
outputtable1 = as.data.frame(t(data.frame(cbind(t(outputtable$byClass),t(outputtable$overall)))))
names(outputtable1) = "Statistics"
outputtable1$Statistics = round(outputtable1$Statistics, digits = 2)

write.csv2(outputtable1, "D:/College/Proposal 2/5. Results/EngineeringConfusion")


####try up sampling because there is not enough highs

upsampledat = upSample(totalresults, as.factor(totalresults$EngSev))
summary(as.factor(upsampledat$EngSev))
outputtable = confusionMatrix(upsampledat$severity,upsampledat$GTseverity)

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
  confusionMatrix(t)
  