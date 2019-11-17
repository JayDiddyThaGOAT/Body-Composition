bodyfatpercentage <- function(Abdomen, Weight, Wrist, Forearm){
  # your code goes here
  bodyfat <- -36.6803
  bodyfat <- bodyfat + 1.0156 * Abdomen
  bodyfat <- bodyfat - 0.1395 * Weight
  bodyfat <- bodyfat - 1.6907 * Wrist
  bodyfat <- bodyfat + 0.6207 * Forearm
  
  return(round(bodyfat, digits=1))
}