
#velocity and trajectory
VelTraj = function(accelX, accely,initialspeed) 
{
  v0(initialspeed)
  g = 9.81
  ## create a matrix of zeros the size of the vector for velocity and heading
  matU = matrix(nrow = length(accelX))  ##POSSIBLE PROBLEM AS IT IS LENGTH +1 IN MATLAB
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
    delu = g * accely[n] * (Time[n+1]- Time[n])   #difference in velocity
    deltheta = g * accelX[n] / matU[n]* (Time[n+1]- Time[n]) #differnce in displacement
    matU[n+1] = matU[n] + delu  # next velocity derivative
    matTheta[n+1] = matTheta[n] + deltheta  #next displacement derivative
    #velocity at t = n+1
    x_vel[n+1] = matU[n+1] * cos(matTheta[n+1])
    y_vel[n+1] = matU[n+1] * sin(matTheta[n+1])
    #trajectory at t = n+1
    x[n+1] = x[n] + matU[n+1] * cos(matTheta[n+1]) * (Time[n+1]- Time[n]) 
    y[n+1] = y[n] + matU[n+1] * sin(matTheta[n+1]) * (Time[n+1]- Time[n])

  }
  velocity = x_vel + y_vel
  trajectory = x + y
  df <- data.frame(velocity,trajectory)
  colnames(df) <- c('vel', 'traj')
  return(df)
  
}
