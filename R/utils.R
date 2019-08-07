
#' create a board by matrix
#'
#' @param n , the length size of the board
#' @param p  the ratio of the balck borad
#'
#' @return a new borad
#' @export
#'
#' @examples generate_board_mat(5,0.25)
#' generate_board_mat(n = 8, p = 0.75)
#' generate_board_mat(n = 8, p = 0)
generate_board_mat <- function(n =5,p=0.25 ){
  assert_that(n > 0)
  assert_that(n%%1==0)
  assert_that(p >= 0 )
  assert_that(p <= 1)
  balck_num = floor(p*n^2) /n
  x <- sample(c(0,1), replace=TRUE, size=n*n, prob = c(p,1-p))
  mat <- matrix(x, nrow = n)
  mat
}
