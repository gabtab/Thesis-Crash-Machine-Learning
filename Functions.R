##create multiple hisotgrams
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}
##create multiple bars in grid
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

##convert kmh to m/s
v0 = function(v0) { v0 *1000 /3600 }

##time step of data set
timeit = function(time, n) {time[n+1]- time[n]}

##create a crash angle
DirectionCat = function(direction.post){

  if (direction.post >= 0 && direction.post < 30) {
    impact_zone <- "FC"
    crash_type <- "Front Impact"
  } else if (direction.post >= 30 && direction.post <= 150) {
    impact_zone <- "SR"
    crash_type <- "Side Right"
  } else if (direction.post > 150 && direction.post <= 210) {
    impact_zone <- "BC"
    crash_type <- "Rear Impact"
  } else if (direction.post >= 210 && direction.post <= 330) {
    impact_zone <- "SL"
    crash_type <- "Side Left"
  } else if (direction.post > 330 && direction.post <= 360) {
    impact_zone <- "FC"
    crash_type <- "Front Impact"
  }
  angle <- data.frame(impact_zone,crash_type)
  colnames(angle) <- c('impact_zone', 'crash_type')
  return(angle)
}

##create a crash angle for momentum
DirectionMom = function(direction.post){
  
  if (direction.post >= 0 && direction.post < 30) {
    impact_zone <- "SL"
    crash_type <- "Side Left"
  } else if (direction.post >= 30 && direction.post <= 150) {
    impact_zone <- "BC"
    crash_type <- "Rear Impact"
  } else if (direction.post > 150 && direction.post <= 210) {
    impact_zone <- "SR"
    crash_type <- "Side Left"
  } else if (direction.post >= 210 && direction.post <= 330) {
    impact_zone <- "FC"
    crash_type <- "Front Centre"
  } else if (direction.post > 330 && direction.post <= 360) {
    impact_zone <- "SL"
    crash_type <- "Side Left"
  }
  angle <- data.frame(impact_zone,crash_type)
  colnames(angle) <- c('impact_zone', 'crash_type')
  return(angle)
}

##################velocity and trajectory of car##########################
VelTraj = function(accelX, accely,initialspeed, Time) 
{
  v0(initialspeed)
  g = 9.81
  ## create a matrix of zeros the size of the vector for velocity and heading
  matU = matrix(nrow = length(accelX))
  matTheta = matrix(nrow = length(accelX))
  ## create a matrix of zeros the size of the vector for velocity variables
  x_vel = matrix(nrow = length(accelX))
  y_vel = matrix(nrow = length(accelX))
  ## create a matrix of zeros the size of the vector for trajectory variables
  x = matrix(nrow = length(accelX))
  y = matrix(nrow = length(accelX))
  
  #1. intial conditions
  matTheta[] = 0
  matU[1] = v0(initialspeed)
  x[] = 0
  y[] = 0
  #n = 1
  #2.compute trajectory and velocity of vehicle
  for (n in 1: length(accelX)){
    delu = g * accely[n] * timeit(Time, n)  #this gives me the acceleration at each point in time
    deltheta = (g * accelX[n]) / (matU[n]* timeit(Time,n )) #this give me the acceletation at each point in time in the y direction
    matU[n+1] = matU[n] + delu  #gives me the displacement at vectorn plus i believe this is rt + delta t in lecture
    matTheta[n+1] = matTheta[n] + deltheta  
    #velocity at t = n+1
    x_vel[n+1] = matU[n+1] * cos(matTheta[n+1])
    y_vel[n+1] = matU[n+1] * sin(matTheta[n+1])
    #trajectory at t = n+1
    x[n+1] = x[n] + matU[n+1] * cos(matTheta[n+1]) * timeit(Time, n)
    y[n+1] = y[n] + matU[n+1] * sin(matTheta[n+1]) * timeit(Time, n)
    
  }
  velocity = x_vel + y_vel
  trajectory = x + y
  df <- data.frame(velocity,trajectory)
  colnames(df) <- c('vel', 'traj')
  return(df)
  
}

################# Calculate Magnitude of Acceleration #####################

acc.mag <- function(accelx, accely, accelz) {
  sqrt(accelx ^ 2 + accely ^ 2 + accelz ^ 2)
}

##################   Momentum Model   ##########################
momentum = function(ind.imp,Time,accelX ,accelY ,accelZ ,mass,mag) {
  gravity = 9.80665

  ## In order to get 10 observations the 2nd function needs to be used
  #crash.points <- sort(c(ind.imp + seq(from = 0, to = floor(0.005 / timeit(Time,ind.imp)), by = 1)))
  crash.points <- sort(c(ind.imp + seq(from = 0, to = 50, by = 1)))
  #crash.points <- ind.imp
  # Get accelerometer data for crash window
  crash.acc.x <- gravity * accelX[c(crash.points)]
  crash.acc.y <- gravity * accelY[c(crash.points)]
  crash.acc.z <- gravity * accelZ[c(crash.points)]
  ##integrate each axis using the trapizoid rule step 1
  traps.x <- 2 * sum(crash.acc.x[2:(length(crash.acc.x) - 1)])
  traps.x <- traps.x + crash.acc.x[1] + crash.acc.x[length(crash.acc.x)]
  traps.y <- 2 * sum(crash.acc.y[2:(length(crash.acc.y) - 1)])
  traps.y <- traps.y + crash.acc.y[1] + crash.acc.y[length(crash.acc.y)]
  traps.z <- 2 * sum(crash.acc.z[2:(length(crash.acc.z) - 1)])
  traps.z <- traps.y + crash.acc.z[1] + crash.acc.z[length(crash.acc.z)]
  
  ##integrate each axis using the trapizoid rule step 2
  mom.x.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.x
  mom.y.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.y
  mom.z.crash <- mass * timeit(Time,ind.imp) * 0.5 * traps.z
  #calculate the direction of the momentum vector
  direction.post <- atan2(y = mom.x.crash, x = mom.y.crash)
  direction.post <- direction.post * 180 / pi
  #direction.post = direction.post - 180
  ##################################################################
  ##this handles negative angles
  if (direction.post < 0) {
    direction.post <- 360 - abs(direction.post)
  } 
  
  # Where was the car hit from
  angle = DirectionMom(direction.post)
  
  finalmag = acc.mag(mom.x.crash, mom.y.crash, mom.z.crash) / mass
  
  if(finalmag > 2.5){
    severity = "High"
  }
  else{
    severity = "Low"
  }
  results <- data.frame(angle$impact_zone,direction.post,angle$crash_type, finalmag, severity)
  colnames(results) <- c('impact_zone', 'crash angle','crash_type', 'crash_mag', 'severity')
  return(results)
  
}

####################feed in all the crashpoints into the SVM model #################
datapoints = function(datap, startpoint,Time,accelX ,accelY ,accelZ, TSTNO) {

 # crash.points <- ind.imp   ### for 1 datapoint
 crash.points <- sort(c(ind.imp + seq(from = 0, to = 50, by = datap))) #for multiple datapoints
  # Get accelerometer data for crash window
  crash.acc.x <- data.frame(t(accelX[c(crash.points)]))
  crash.acc.y <- data.frame(t(accelY[c(crash.points)]))
  colnames(crash.acc.y) <- gsub(x = colnames(crash.acc.y), pattern = "X", replacement = "Y") 
  
  crash.acc.z <- data.frame(t(accelZ[c(crash.points)]))
  colnames(crash.acc.z) <- gsub(x = colnames(crash.acc.z), pattern = "X", replacement = "Z") 
  
  results <- cbind(crash.acc.x, crash.acc.y, crash.acc.z)
  #colnames(results) <- c('crashx', 'crashy','crashz')
  return(results)
  
}