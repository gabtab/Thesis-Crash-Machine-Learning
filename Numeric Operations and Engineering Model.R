
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
  matU [1] = v0(initialspeed)
  x[] = 0
  y[] = 0
  #n = 1
  #2.compute trajectory and velocity of vehicle
  for (n in 1: length(accelX)){
    delu = g * accely[n] * timeit(Time, n)   #difference in velocity
    deltheta = g * accelX[n] / matU[n]* timeit(Time,n ) #differnce in displacement
    matU[n+1] = matU[n] + delu  # next velocity derivative
    matTheta[n+1] = matTheta[n] + deltheta  #next displacement derivative
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
## easy example of why http://www.dummies.com/education/science/physics/calculating-net-force-and-acceleration/
acc.mag <- function(accelx, accely, accelz) {
  sqrt(accelx ^ 2 + accely ^ 2 + accelz ^ 2)
}

##################   Momentum Model   ##########################
momentum = function(ind ,N,accelX ,accelY ,accelZ ,m,crash) {
# get a window time window of the crash N = -epsilon = + epsilon
  g = 9.81
  crash.acc.x = g * accelx
  crash.acc.y = g * accely
  #
  
  crash.points <- sort(c(ind.imp + seq(from = 0, to = floor(0.05 / time.step), by = 1)))
  
  # Get accelerometer data for crash window
  crash.acc.x <- grav.constant * acc.x[c(crash.points)]
  crash.acc.y <- grav.constant * acc.y[c(crash.points)]
  
  
  
  traps.x <- 2 * sum(crash.acc.x[2:(length(crash.acc.x) - 1)])
  traps.x <- traps.x + crash.acc.x[1] + crash.acc.x[length(crash.acc.x)]
  traps.y <- 2 * sum(crash.acc.y[2:(length(crash.acc.y) - 1)])
  traps.y <- traps.y + crash.acc.y[1] + crash.acc.y[length(crash.acc.y)]
  mass = 1095
  angle = 
    
  mom.x.crash <- mass * time.step * 0.5 * traps.x
  mom.y.crash <- mass * time.step * 0.5 * traps.y
    
}

#####
#variables that need to go on the project sheet
accelX = Force.XG
accelY = Force.YG
accelz = Force.ZG
initialspeed = 96.8
Time = Time
