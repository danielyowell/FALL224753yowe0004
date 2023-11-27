#' mymaxlik
#'
#' @param lfun log likelihood
#' @param x variable
#' @param param parameter to be measured
#' @param ... the rest
#'
#' @importFrom graphics points
#' @importFrom graphics axis
#'
#' @return MLE using grid approximation
#' @export
mymaxlik = function(lfun,x,param,...){
# various log likelihoods defined
## logbin=function(x,param) log(dbinom(x,prob=param,size=10))
## logpoiss=function(x,param) log(dpois(x,lambda=param))
## logexp=function(x,param) log(dexp(x,rate=param))

  # how many param values are there?
  np = length(param)

  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" ")) to understand
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  z = outer(x,param,lfun) # A

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  # which gives the index for the value of y == max.
  y = apply(z,2,sum)

  plot(param,y,col="Blue",type="l",lwd=2,...)
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i = max(which(y == max(y))) # B
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope should change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(
    i = i,
    parami = param[i],
    yi = y[i],
    slope = slope
  )
  )
}
