#' ntickets
#'
#' @importFrom stats qbinom
#' @importFrom stats pbinom
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats approx
#' @importFrom graphics lines
#' @importFrom graphics abline
#'
#' @param N Number of seats in flight
#' @param gamma Probability a plane will be truly overbooked: shows > seats
#' @param p Probability of a show
#'
#' @return list and Objective plots (binom and norm)
#' @export
#'
#' @examples
#' ntickets(200, 0.02, 0.95)
ntickets <- function(N,gamma,p) {
  ###
  ### PART 1 - NAMED LIST
  ###

  # first set the range of n
  n = seq(N, N+20, by = 1)

  # find binomial distribution
  qb = qbinom(1-gamma, n, p)

  # calculate q, mean, and sd for normal approx.
  q = 1 - p
  m = n * p
  s = sqrt( n * p *  q)
  # normal approximation
  qn = qnorm(p, m, s)

  # create and print list
  l <- list(nd=qb,
            nc=round(qn, 1),
            N=N,
            p=p,
            gamma=gamma
  )
  print(l)

  ###
  ### PART 2 - BINOM PLOT
  ###

  # OBJECTIVE FUNCTION FOR BINOMIAL DISTRIBUTION
  obj <- function(x){
    1 - gamma - pbinom(N, x, p) # pbinom(successes, sample size, probability)
  }
  Objective = obj(n)
  # find n value where Objective function is 0
  idx <- which.min( abs( Objective ) )
  idx

  n_final = N - 1 + idx

  # create objective function plot
  plot(n, Objective,
       col = "blue", pch = 19,
       main = paste("Objective vs. n to find optimal tickets sold\nn = ",
                    n_final,", gamma = ",gamma,", N = ",N,", discrete", sep = ""
       )
  )
  lines(n, Objective, col = "black")

  # add red lines at the desired n point
  abline(h = Objective[idx], col = "red", lwd = 2)
  abline(v = n_final, col = "red", lwd = 2)

  ###
  ### PART 3 - NORM PLOT
  ###

  # redefine n to include more divisions
  div = 2
  n = seq(N, N+20, by = 1/div)
  # also adjust q, mean, and sd to match
  q = 1 - p
  m = n * p
  s = sqrt( n * p *  q)

  # OBJECTIVE FUNCTION FOR NORMAL APPROXIMATION
  obj2 <- function(X, Y, Z){
    return(1 - gamma - pnorm(X, Y, Z)) # pnorm(desired number, mean, sd)
  }
  Objective = obj2(N, m, s)
  # find n value where Objective function is 0
  # (since this is continuous, it will be easier to
  # calculate the intersection via approx and extract y,
  # as calling "optimize" will snap to integer x values)
  x_intercept <- approx(Objective, n, xout = 0)$y
  # y-intercept (aka y = 0)
  y_intercept <- 0

  # create objective function plot
  plot(n, Objective,
       col = "black", pch = 19, las=2, type = "n",
       main = paste("Objective vs. n to find optimal tickets sold\nn = ",
                    x_intercept,", gamma = ",gamma,", N = ",N,", continuous", sep = ""
       )
  )
  lines(n, Objective, col = "black")

  # add red lines at the desired n point
  abline(h = y_intercept, col = "red", lwd = 2)
  abline(v = x_intercept, col = "red", lwd = 2)
}
