#' @title Bootstrap Function 2
#'
#' @description
#' Performs a bootstrap simulation to calculate confidence intervals for a use-defined statistic, that is fun, applied to a dataset. This is the better looking function.
#'
#'
#' @param iter The number of iterations
#' @param x The data vector provided
#' @param fun The function to be applied to each bootstrap
#' @param alpha The significance level for the confidence interval
#' @param cx The size of the text label on the plots
#' @param ... Optional additional parameters
#'
#' @return An invisible list of ci, fun, and x along with a histogram plot
#' @export
#'
#' @examples
#'  \dontrun{myboot2(x = sam, fun = "mean", alpha = 0.05, xlab = "mean", col = "palegoldenrod")}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
