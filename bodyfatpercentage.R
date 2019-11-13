bodyfatpercentage <- function(Age, Chest, Abdomen, Thigh){
  # your code goes here
  
  #Link for Equations: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2891061/#FD1
  
  S <- (Chest + Abdomen + Thigh) / 10
  
  #a <- 1.10938
  #b <- 0.0008267
  #c <- 0.0000016
  #d <- 0.000257
  #BD <- round(a - b * S + c * S^2 - d * Age, digits=4)
  
  a <- 0.109648
  b <- 0.0021745
  k <- 0.747
  d <- 0.0002516
  BD <- round(exp(a - b * S^k - d * Age), digits=4)
  
  return(BD)
}