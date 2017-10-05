
#############Create impact time#################

#create a subset that will be of an individual test
testdat = subset(resh, TSTNO == 6)
#create a column that has the sum of the absolute value of the xyz data
testdat$abssum = abs(testdat$Force.XG) + abs(testdat$Force.YG) +abs(testdat$Force.ZG)
#get the row that absolute value of the xyz is at a maximum for the impact

car1 = testdat[which(testdat$VEHNO == 1),]
car2 = testdat[which(testdat$VEHNO == 2),]
maxallforce1 = car1[which.max(car1$abssum),]
maxallforce2 = car2[which.max(car2$abssum),]


############velocity and trajectory of vehicle###################
dt = 0.01
##km/h to m/s
v0 = function(v0) { v0 *1000 /3600 } 
g = 9.81
## create a matrix of zeros the size of the vector for velocity and heading
matU = matrix(nrow = length(car1$Force.XG))  ##POSSIBLE PROBLEM AS IT IS LENGTH +1 IN MATLAB
matTheta = matrix(nrow = length(car1$Force.XG))
## create a matrix of zeros the size of the vector for velocity variables
x_vel = matrix(nrow = length(car1$Force.XG))
y_vel = matrix(nrow = length(car1$Force.XG))
## create a matrix of zeros the size of the vector for trajectory variables
x = matrix(nrow = length(car1$Force.XG))
y = matrix(nrow = length(car1$Force.XG))

#1. intial conditions
matTheta[] = 0
matU[1] = v0
x[] = 0
y[] = 0

#2.compute trajectory and velocity of vehicle
for (n in 1: length(car2$Force.XG)){
  delu = g * (n) * dt
  deltheta = g * car2$Force.XG[n] / matU[n]* dt
  u(n+1) 
  
}