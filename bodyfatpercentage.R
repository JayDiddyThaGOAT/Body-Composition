bodyfatpercentage <- function(Neck, Chest, Abdomen, Hip, Thigh, Forearm, Wrist){
  return(20.211-0.602*Neck - 0.215* Chest + 1.017 * Abdomen - -0.417 * Hip + 0.203* Thigh + 0.274 * Forearm +0.568 * Wrist) 
}