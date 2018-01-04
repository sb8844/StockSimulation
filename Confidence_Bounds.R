
Confidence_Bounds = function(X,T,InitialP) {
  mean <- mean(X)
  sigma <- var(X)
  part1 <- ((mean) - (((1/2)*sigma)*T)) 
  part2 <- ((1.96)*(sqrt(sigma))*(sqrt(T)))
  LBound <- (InitialP) * exp((part1 - part2))
  UBound <- (InitialP) * exp((part1 + part2))
  confVec <- c(LBound,UBound)
  return(confVec)
}