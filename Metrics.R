#' mean calculation
#'
#' @param x
#' A numeric vector whose mean is to be calculated
#' @return
#' numeric value;mean calculated
#' @export
#'
#' @examples
#' x <- c(5,6,7,8,9,10)
#' mean <- calculatemean(x)
calculatemean <- function(x){
  if(!is.vector(x)){
    stop("x should be a vector")
  }
  if(any(is.na(x))){
    warning("Data contains NA, coercing  NA to Zero")
    x[which(is.na(x))] <- 0
  }
  if(length(x)<1){
    stop("x contains no element")
  }
  if(class(x) != "numeric"){
    stop("x should be a numeric vector")
  }
  sum <- sum(x) #Summation
  nos <- length(x) #number of observations
  mean <- sum/nos
  return(mean)
}

#' For Calculating Median
#'
#' @param x
#' A numeric vector whose median is to be calculated
#' @return
#'  numeric value;mean calculated
#' @export
#'
#' @examples
#' x <- c(5,6,7,8,9,10)
#' median <- calculatemean(x)

calculatemedian <- function(x){
  if(!is.vector(x)){
    stop("x should be a vector")
  }
  if(any(is.na(x))){
    warning("x contains NA, coercing  NA to Zero")
    x[which(is.na(x))] <- 0
  }
  if(length(x)<1){
    stop("x contains no element")
  }
  if(class(x) != "numeric"){
    stop("x should be a numeric vector")
  }
  x <- sort(x)
  if(length(x)%%2!=0){
    median <- x[(length(x)+1)/2]
  }else{
    b <- x[length(x)/2]
    b <- c(b,x[(length(x)/2)+1])
    median <- calculatemean(b)
  }
  return(median)
}

#' For calculating Variance
#'
#' @param x
#' A numeric vector whose median is to be calculated
#' @return
#' numeric value;variance calculated
#' @export
#'
#' @examples
#' x <- c(5,6,7,8,9,10)
#' var <- calculatevariance(x)
calculatevariance <- function(x){
  if(!is.vector(x)){
    stop("x should be a vector")
  }
  if(any(is.na(x))){
    warning("x contains NA, coercing  NA to Zero")
    x[which(is.na(x))] <- 0
  }
  if(length(x)<1){
    stop("x contains no element")
  }
  if(class(x) != "numeric"){
    stop("x should be a numeric vector")
  }
  #x <- MarketingData$Sales
  result <- NULL
  mean <- calculatemean(x) #Calculating the mean.
  for(i in 1:length(x)){
    #i <- 2
    res <- (x[i]-mean)^2
    if(is.null(result)){
      result <- res
    }else{
      result <- result + res
    }
  }
  variance <- result/(length(x)-1)
  return(variance)
}

#' For Calculating Standard Deviation
#'
#' @param x
#' A numeric vector whose standard deviation needs to be calculated
#' @return
#' numeric value;variance calculated
#' @export
#'
#' @examples
#' x <- c(5,6,7,8,9,10)
#' sd <- calculatesd(x)
calculatesd <- function(x){
  if(!is.vector(x)){
    stop("x should be a vector")
  }
  if(any(is.na(x))){
    warning("x contains NA, coercing  NA to Zero")
    x[which(is.na(x))] <- 0
  }
  if(length(x)<1){
    stop("x contains no element")
  }
  if(class(x) != "numeric"){
    stop("x should be a numeric vector")
  }
  variance <- calculatevariance(x)
  stddev <- variance^0.5
  return(stddev)
}

#' For Calculating Correlation
#'
#' @param x
#' A numeric vector
#' @param y
#' A numeric vector
#' @return
#' correlation number
#' @export
#'
#' @examples
#' data(mtcars)
#' x <- mtcars$mpg
#' y <- mtcars$cyl
#' corr <- correlation(x,y)
correlation <- function(x,y){
  if(!is.vector(x) |!is.vector(y)){
    stop("Input data should be a vector")
  }
  if(any(is.na(x)) | any(is.na(y))){
    warning("Inut data contains NA, coercing  NA to Zero")
    x[which(is.na(x))] <- 0
  }
  if(length(x)<1 | length(y)<1){
    stop("Input  Data contains no element")
  }
  if(class(x) != "numeric" | class(y) != "numeric"){
    stop("Input Data should be a numeric vector")
  }
  xmean <- calculatemean(x)
  ymean <- calculatemean(y)
  result <- NULL
  if(length(x)==length(y)){
    for(i in 1:length(x)){
      res <- (x[i]-xmean)*(y[i]-ymean)
      if(is.null(result)){
        result <- res
      }else{
        result <- result + res
      }
    }
  }
  xcalc <- calculatevariance(x)*(length(x)-1)
  ycalc <- calculatevariance(y)*(length(y)-1)
  correlation <- result/(xcalc*ycalc)^0.5
  return(correlation)
}

#' For Getting Correlation table
#' It gives a correlation table
#' @param Data
#' A data frame for which the correlation needs to be calculated
#' @return
#' A data frame which gives the correlation for two variables
#' @export
#'
#' @examples
#' data(mtcars)
#' coratab <- Correlationtable(mtcars)
Correlationtable <- function(Data){
  if(any(is.na(Data))){
    stop("Data is having NA")
  }
  if(class(Data)!="data.frame"){
    stop("Data is having a data type other than data frame")
  }
  if(any(apply(X = Data,MARGIN = 2,FUN = is.numeric)) != T){
    stop(" Data should have only numeric vectors")
  }
  rowname <- colnames(Data)
  colname <- colnames(Data)
  cortable <- data.frame(matrix(data = NA,nrow = ncol(Data),ncol = ncol(Data),byrow = T,dimnames = list(rowname,colname)))
  for(i in 1:length(Data)){
    for(j in 1:length(Data)){
      cortable[i,j] <- correlation(Data[,i],Data[,j])
    }
  }
  return(cortable)
}

#' For Calculating MAPE(Mean Absolute Percentage Error)
#'
#' @param Actual
#' A vector of Dependent variable.
#' @param Predicted
#' A vector of fitted values obtained from using any technique
#'
#' @return
#' It returns a numeric value using mape calculation
#' @export
#'
#' @examples
#' data(mtcars)
#' pred <- lm(mpg~cyl,data = mtcars)
#' predicted <- predict(pred)
#' mape <- MAPE(mtcars$mpg,predicted)
MAPE <- function(Actual,Predicted){
  if(!is.vector(Actual) |!is.vector(Predicted)){
    stop("Input data should be a vector")
  }
  if(any(is.na(Actual)) | any(is.na(Predicted))){
    warning("Inut data contains NA")
  }
  if(length(Actual)<1 | length(Predicted)<1){
    stop("Input  Data contains no element")
  }
  if(class(Actual) != "numeric" | class(Predicted) != "numeric"){
    stop("Input Data should be a numeric vector")
  }
  mape <- ((sum(abs((Actual - Predicted)/Actual)))/(length(Predicted)))*100
  return(mape)
}

#' For Calculating (MSE/RMSE) Mean Squared Error
#'
#' @param Actual
#' A vector of Dependent variable.
#' @param Predicted
#' A vector of fitted values obtained from using any technique
#' @param Root
#' If TRUE it will return a vector containing both MSE and RMSE
#' @return
#' It will return mean square error and root mean squared error.
#' @export
#'
#' @examples
#' data(mtcars)
#' pred <- lm(mpg~cyl,data = mtcars)
#' predicted <- predict(pred)
#' MSE <- MSE(mtcars$mpg,predicted)
MSE <- function(Actual,Predicted,Root = NULL){
  if(!is.vector(Actual) |!is.vector(Predicted)){
    stop("Input data should be a vector")
  }
  if(any(is.na(Actual)) | any(is.na(Predicted))){
    warning("Inut data contains NA")
  }
  if(length(Actual)<1 | length(Predicted)<1){
    stop("Input  Data contains no element")
  }
  if(class(Actual) != "numeric" | class(Predicted) != "numeric"){
    stop("Input Data should be a numeric vector")
  }
  mse <- sum((Actual-Predicted)^2)/length(Predicted)
  if(Root == TRUE){
    mse <- c(mse,sqrt(mse))
    names(mse) <- c("mse","rmse")
  }
  return(mse)
}

#' For Calculating RSquared
#'
#' @param Actual
#' A vector of Dependent variable.
#' @param Predicted
#' A vector of fitted values obtained from using any technique
#' @return
#' It will calculate RSquared value
#' @export
#'
#' @examples
#' data(mtcars)
#' pred <- lm(mpg~cyl,data = mtcars)
#' predicted <- predict(pred)
#' R2 <- Rsquared(mtcars$mpg,predicted)
Rsquared <-function(Actual,Predicted){
  if(!is.vector(Actual) |!is.vector(Predicted)){
    stop("Input data should be a vector")
  }
  if(any(is.na(Actual)) | any(is.na(Predicted))){
    warning("Inut data contains NA")
  }
  if(length(Actual)<1 | length(Predicted)<1){
    stop("Input  Data contains no element")
  }
  if(class(Actual) != "numeric" | class(Predicted) != "numeric"){
    stop("Input Data should be a numeric vector")
  }
  SSres <- sum((Actual - Predicted)^2)
  SStot <- sum((Actual - mean(Actual))^2)
  return(1-(SSres/SStot))
}

#' For Calulating Adjusted RSqaured
#'
#' @param n
#' Number of observations
#' @param k
#' Number of Independent Variables
#' @param R2
#' Calculated Value of Rsquared
#' @return
#' It will return the Adjusted R Squared value
#' @export
#'
#' @examples
#' data(mtcars)
#' pred <- lm(mpg~cyl,data = mtcars)
#' predicted <- predict(pred)
#' R2 <- Rsquared(mtcars$mpg,predicted)
#' AR2 <- AdjRSquared(n = 32,k = 1,R2 = 0.721068)
AdjRSquared <- function(n,k,R2){
  if((!is.vector(n)) | (!is.numeric(k)) | (!is.numeric(R2))){
    stop("Input data should be a vector")
  }
  if(is.na(n) | is.na(k) | is.na(R2)){
    warning("Inut data contains NA")
  }
  if(length(n)<1 | length(k)<1 | length(R2)<1){
    stop("Input  Data contains no element")
  }
  if(class(n) != "numeric" | class(k) != "numeric"| class(R2) != "numeric"){
    stop("Input Data should be a numeric vector")
  }
    return(1 - (((1-R2) * (n-1))/(n-k-1)))
}
