#' Disparity
#' 
#' Calculates the disparity \eqn{p} for given (incidence) angles.
#' \deqn{p \approx h  \cdot  (\cot  \theta_{i_2} - \cot \theta_{i_1})}
#'  
#' @param h realtive height.
#' @param theta1 angle.
#' @param theta2 angle.
#' @export
#' @seealso \code{\link{disparityMapExpected}}
#' @examples
#' plot((c(35:45)/360*2*pi), disparity(10, (c(35:45)/360*2*pi), 45/360*2*pi), type='l')
disparity <- function(h, theta1, theta2) {
    h*(1/tan(theta2) - 1/tan(theta1))
}