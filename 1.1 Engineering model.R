


totalresults = NULL
outputtable = NULL
outputtable1 = NULL

for (i in dfmag$TSTNO){
  
  ##use the model to get the velocity and trajectory
  # DatVelTraj = VelTraj(testdat$Force.X[testdat$TSTNO == i ], testdat$Force.Y[testdat$TSTNO == i ],
  #                      dfmag$initialspeed[dfmag$TSTNO == i],testdat$Time[testdat$TSTNO == i])
  # DatVelTraj = DatVelTraj[1:length(testdat$Force.X[testdat$TSTNO == i]),]
  # 
  # testdat$vel[testdat$TSTNO == i] = DatVelTraj$vel
  # testdat$traj[testdat$TSTNO == i] = DatVelTraj$traj
  
  
  ind.dat = testdat[testdat$TSTNO == i ,]
  ind.imp = which.max(ind.dat$mag)
  f.imp <- ind.dat[ind.imp,]
  results = momentum(ind.imp,ind.dat$Time,ind.dat$Force.X, ind.dat$Force.Y, ind.dat$Force.Z,
                     dfmag$vehwht[dfmag$TSTNO == i],f.imp$mag)
  results$TSTNO = i
  results$GTAngle = tstset$` IMPANG`[tstset$TSTNO == i]
  angle = DirectionCat(tstset$` IMPANG`[tstset$TSTNO == i])
  pdof = DirectionCat(vehdatclean$` PDOF`[vehdatclean$vehid == dfmag$vehid[dfmag$TSTNO == i]])
  results$GTAngCat = angle$impact_zone
  results$PDOF = pdof$impact_zone
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


############# Want to see what the % correct is in the total results tab for engineering model when obs are set to 125 and then for 5

outputtable = confusionMatrix(totalresults$severity,totalresults$GTseverity)
outputtable
outputtable1 = as.data.frame(t(data.frame(cbind(t(outputtable$byClass),t(outputtable$overall)))))
names(outputtable1) = "Statistics"
outputtable1$Statistics = round(outputtable1$Statistics, digits = 2)

write.csv2(outputtable1, "D:/College/Proposal 2/5. Results/EngineeringConfusion")



