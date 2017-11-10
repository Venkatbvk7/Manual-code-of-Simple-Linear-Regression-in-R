simple.linear.reg <- function(x, y,col) {

  # Length of x to get size of sample. Assuming x and y have the same sample size.
  n <- length(x)
  
  #Calculate Error Statistics
  sxx <- sum(x^2) - sum(x)^2 / n
  syy <- sum(y^2) - sum(y)^2 / n
  sxy <- sum(x * y) - (sum(x) * sum(y)) / n
  
  # Coefficients beta0 and beta1
  b1 <- sxy / sxx
  b0 <- mean(y) - b1 * mean(x)
  
  # Calculate Fitted
  fitted <- x * b1 + b0
  
  # Calculate Residuals
  resd <- y - fitted
  RegVsFitted<-data.frame(fitted,resd)
  
  #Plot Residuals Vs Fitted
   o<-ggplot(data=RegVsFitted, aes(x=fitted, y=resd)) + geom_point(colour="blue")+geom_hline(yintercept=0,color=col)+labs(title = "Residuals Vs Fitted",x="Fitted Values",y="Residual Values")
  
  return(o)
}

simple.linear.reg(LifeCycleSavings$sr,LifeCycleSavings$ddpi,"Red")
