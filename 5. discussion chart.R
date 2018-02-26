

###Create data source based on results

name = c("Mod 1","Mod 2", "Mod 3", "Mod 4", "Mod 5", "Mod 6")
Acc = c(65, 99, 99, 98, 78.4, 79.6)
upr = c(69,100,100,99.5,81,82 )
lwr = c(61,97,97,96.5,76,77)
sens = c(37,100,100,100,81,82)
spec = c(92,96,97,95,74,76)
auc= c(65,99,99,99,77,79)

svmgrap = data.frame(name,Acc,upr,lwr,sens,spec,auc)

ggplot(data = svmgrap, aes(x = name, y = Acc)) + geom_point() + geom_errorbar(aes(ymin =lwr, ymax =upr)) +
  ylab("Accuracy")+ xlab('Model Name') + labs(title = "SVM Models with 95% Confidence")+
  theme_economist()

###pdof
name = c("Mod 1","Mod 2", "Mod 3", "Mod 4", "Mod 5")
Acc = c(65, 91, 84.78, 78, 75)
upr = c(72,96,91,84,81 )
lwr = c(58,83.41,76,71,68)


pdofgrap = data.frame(name,Acc,upr,lwr)

ggplot(data = pdofgrap, aes(x = name, y = Acc)) + geom_point() + geom_errorbar(aes(ymin =lwr, ymax =upr)) +
  ylab("Accuracy")+ xlab('Model Name') + labs(title = "PDOF Models with 95% Confidence")+
  theme_economist()
