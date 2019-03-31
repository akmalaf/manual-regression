#data initializing
#a
x1 <- runif(20, min = 25, max = 30)
x2 <- runif(20, min = 40, max = 50)
#b
e <- rnorm(20)
#c
y <- 2+1.5*x1+4*x2+e
#d
datatest <- cbind(y,x1,x2)

#function
manualregression <- function(regdata, alpha = 0.05){
  #dataprep
  Y <- as.matrix(regdata[,1])
  n <- length(Y)
  m <- dim(regdata[,-1])[2]
  onesvect <- rep(1,n)
  X <- cbind(onesvect,regdata[,-1])
  onesmatr <- matrix(rep(1,n*n),n,n)
  #works
  #coefficients
  B <- solve(t(X)%*%X)%*%t(X)%*%Y
  rownames(B) <- c('b0','b1','b2')
  colnames(B) <- 'Coefficients'
  #anova table
  #sources
  sources <- c('regression','error','total')
  #df
  df <- c(m,n-m-1,n-1)
  #ss
  ssr <- as.numeric(t(B)%*%t(X)%*%Y-(1/n)*t(Y)%*%onesmatr%*%Y)
  sse <- as.numeric(t(Y)%*%Y-t(B)%*%t(X)%*%Y)
  ssto <- ssr + sse
  ss <- c(ssr,sse,ssto)
  #ms
  msr <- ssr/df[1]
  mse <- sse/df[2]
  ms <- c(msr,mse,NA)
  #f
  f <- c(msr/mse, NA, NA)
  pvalue <- c(1-pf(msr/mse, m, n-m-1), NA, NA)
  anovatable <- data.frame(sources,df,ss,ms,f,pvalue)
  #hypothesis testing
  h0 <- 'There is no relationship between Y and X variables'
  h1 <- 'There is a relationship between Y and at least one of the X variables'
  tablef <- qf(1-alpha,m,n-m-1)
  if(tablef<msr/mse){
    hypothesis <- h1
  } else{
    hypothesis <- h0
  }
  #regression line
  if(hypothesis==h1){
    regline <- sprintf('The regression line, therefore, is Y = %s + %sX1 + %sX2.', B[1], B[2], B[3])
  } else{
    regline <- 'Since there is no relationship between Y and any of the X variables, the regression line produced is not significant.'
  }
  results <- list(B, anovatable, hypothesis, regline)
  return(results)
}

#testing
manualregression(datatest)
