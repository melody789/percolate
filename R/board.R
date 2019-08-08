#' make a board object
#'
#' @param mat  the board mastrix has default value n=5, p=0.25
#' @param n  number of trails
#' @param p  probality
#'
#' @return  an object that is borad
#' @export
#'
#' @examples board(5, 0.25)
board <- function(mat = NULL, n =5 , p=0.25){
  if (!is.null(mat)) {
    is_valid(mat)
    object <- mat
    attr(object, "n") <- ncol(mat)
    attr(object, "p") <- sum(mat == 0)/(nrow(mat)^2)
  }
  else {
    mat2 <- generate_board_mat(n,p)
    object <- mat2
    attr(object, "n") <- n
    attr(object, "p") <- p
  }
  class(object) <- c("matrix","board")
  object
}
