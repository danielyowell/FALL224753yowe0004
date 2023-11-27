#' myboot_Ass4
#'
#' @param iter a
#' @param x b
#' @param fun c
#' @param alpha d
#' @param ... e
#'
#' @return f
#' @export
#'
#' @examples
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,...){  #Notice where the ... is repeated in the code

  n = length(x)   # sample size

  # create n*iter samples with replacement
  y = sample(x,n*iter,replace=TRUE)

  rs.mat = matrix(y,nr=n,nc=iter,byrow=TRUE)

  xstat = apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it

  ci = quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval

  # A histogram follows

  # The object para will contain the parameters used to make the histogram

  para = hist(xstat,freq=FALSE,las=1,

            main = paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            col = rainbow(length(unique(x))),
            ...
            )

  # mat will be a matrix that contains the data, this is done so that I can use apply()

  mat = matrix(x,nr=length(x),nc=1,byrow=TRUE)

  # pte is the point estimate

  # This uses whatever fun is

  pte = apply(mat,2,fun)

  abline(v=pte,lwd=3,col="Black")# Vertical line

  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci

  # interval made with bootstrap technique
  text(ci[1],0.07,paste("(",round(ci[1],2),sep=""),col="Red",cex=2)
  text(ci[2],0.07,paste(round(ci[2],2),")",sep=""),col="Red",cex=2)

  t_sam = t.test(sam, mu=20, conf.level = 1-alpha)
    lower = as.numeric(t_sam$conf.int[1])
    upper = as.numeric(t_sam$conf.int[2])
  text(ci[1],0.14,paste("(",round(lower,2),sep=""),col="Blue",cex=2)
  text(ci[2],0.14,paste(round(upper,2),")",sep=""),col="Blue",cex=2)


  # plot the point estimate 1/2 way up the density

  text(pte,max(para$density)/2,round(pte,2),cex=2)

  return(list(
    fun = fun,
    x = x,
    t = t_sam$statistic,
    ci = ci,
    cit = t_sam$conf.int
    ))# Some output to use if necessary
}




