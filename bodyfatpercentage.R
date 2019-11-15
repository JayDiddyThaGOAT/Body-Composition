bodyfatpercentage <- function(Model) {
  # your code goes here
  return(round(Model$fitted.values, digits=0))
}