#' CI for mean of a sample
#'
#' @param d sample
#' @param alpha level of confidence
#'
#' @return CI of mean
#' @export
#'
#' @examples
mycltu=function(d, alpha){
  n = length(d)
  samplemean = mean(d)
  samplesd = sd(d)

  # CI = 0.95
  alpha = 0.05
  t <- qt(1-alpha/2, n-1 ) # t multiplier
  samplemean - (t * samplesd / sqrt(n))
  samplemean + (t * samplesd / sqrt(n))
}
