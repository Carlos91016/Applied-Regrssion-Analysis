#' @title Confidence interval for p
#'
#' @param x frequency category
#' @param n total frequency
#' @param alpha error rate
#'
#' @return list containing ci
#' @export
#'
#' @examples mycip(10,20,0.05)
mycip = function(x,n,alpha){
  wilson = c()
  normal = c()
  z = qnorm(1-alpha/2, mean = 0, sd = 1)
  p = x/n

  wilson_p_high = 1/(1+z^2/n)*(p+z^2/(2*n))+z/(1+z^2/n)*sqrt(p*(1-p)/n+z^2/(4*n^2))
  wilson_p_low = 1/(1+z^2/n)*(p+z^2/(2*n))-z/(1+z^2/n)*sqrt(p*(1-p)/n+z^2/(4*n^2))
  wilson = c(wilson_p_low, wilson_p_high)

  normal_p_high = p + z*sqrt(p*(1-p)/n)
  normal_p_low = p - z*sqrt(p*(1-p)/n)
  normal = c(normal_p_low, normal_p_high)

  result = list(normal, wilson)
  return(result)
}
