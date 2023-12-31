#' CI for mean of a sample
#'
#' @param d sample
#' @param alpha level of confidence
#'
#' @importFrom stats qt
#' @importFrom stats sd
#'
#' @return CI of mean
#' @export
#'
myci=function(d, alpha){
  n = length(d)
  samplemean = mean(d)
  samplesd = sd(d)

  # CI = 0.95
  alpha = 0.05
  t <- qt(1-alpha/2, n-1 ) # t multiplier
  lower = samplemean - (t * samplesd / sqrt(n))
  upper = samplemean + (t * samplesd / sqrt(n))
  list(
      lower = lower,
      upper = upper
     )

}
