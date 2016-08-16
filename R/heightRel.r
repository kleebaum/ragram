#' Relative height
#' 
#' Calculates the relative height \eqn{h} for a given disparity \eqn{p} and (incidence) angles.
#' \deqn{h \approx \frac{p}{\cot  \theta_{i_2} - \cot \theta_{i_1}}}
#'  
#' @param p disparity height.
#' @param theta1 angle.
#' @param theta2 angle.
#' @export
#' @seealso \code{\link{disparityMapExpected}}
#' @examples
#' plot((c(35:45)/360*2*pi), disparity(10, (c(35:45)/360*2*pi), 45/360*2*pi), type='l')
heightRel <- function(p, theta1, theta2) {
    p/(1/tan(theta2) - 1/tan(theta1))
}