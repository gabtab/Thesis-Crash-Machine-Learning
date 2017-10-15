##convert kmh to m/s
v0 = function(v0) { v0 *1000 /3600 }

##time step of data set
timeit = function(time, n) {time[n+1]- time[n]}