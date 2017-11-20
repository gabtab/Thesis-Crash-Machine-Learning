##convert kmh to m/s
v0 = function(v0) { v0 *1000 /3600 }

##time step of data set
timeit = function(time, n) {time[n+1]- time[n]}

##create a crash angle
DirectionCat = function(direction.post){
if (direction.post >= 0 && direction.post < 53) {
  impact_zone <- "SL"
  crash_type <- "Side Impact"
} else if (direction.post >= 53 && direction.post <= 80) {
  impact_zone <- "BL"
  crash_type <- "Corner Impact"
} else if (direction.post > 80 && direction.post < 100) {
  impact_zone <- "BC"
  crash_type <- "Rear Impact"
} else if (direction.post >= 100 && direction.post <= 127) {
  impact_zone <- "BR"
  crash_type <- "Corner Impact"
} else if (direction.post > 127 && direction.post < 233) {
  impact_zone <- "SR"
  crash_type <- "Side Impact"
} else if (direction.post >= 233 && direction.post <= 260) {
  impact_zone <- "FR"
  crash_type <- "Corner Impact"
} else if (direction.post > 260 && direction.post < 280) {
  impact_zone <- "FC"
  crash_type <- "Front Impact"
} else if (direction.post >= 280 && direction.post <= 307) {
  impact_zone <- "FL"
  crash_type <- "Corner Impact"
} else if (direction.post > 307 && direction.post <= 360) {
  impact_zone <- "SL"
  crash_type <- "Side Impact"
}
  angle <- data.frame(impact_zone,crash_type)
  colnames(angle) <- c('impact_zone', 'crash_type')
  return(angle)
}