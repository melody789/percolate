
#' create a board by matrix
#'
#' @param n , the length size of the board
#' @param p  the ratio of the balck borad
#'
#' @return a new borad
#' @export
#' @import assertthat
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


#' check if it is a valid board
#'
#' @param mat, a matrix
#'
#' @return true or error
#' @export
#' @import assertthat
#' @examples is_valid(c(matrix(c(1,2,3,4), nrow = 2)))
#' is_valid(generate_board_mat())
#' is_valid(generate_board_mat(n=1))
is_valid <-function(mat){
  assert_that(nrow(mat)==ncol(mat))
  assert_that(all(mat %in% c(0, 1, 2)))
  assert_that(all(mat%%1==0) == TRUE)
  assert_that(is.matrix(mat))
  return (TRUE)
}



#' change a string to 0 or 1
#'
#' @param s a string
#'
#' @return a vector of 0 or 1
#' @export
#'
#' @examples change_1(". * . . *")
change_1 <- function(s) {
  res <- vector()
  for (i in 1:nchar(s)) {
    if (substr(s, i, i) == "*") {
      res <- c(res, 0)
    }
    else if (substr(s, i, i) == ".") {
      res <- c(res, 1)
    }
  }
  res
}

#' change a vector of strings containing * and .
#'
#' @param v a vector of strings of * and .
#' @param d size of the matrix
#'
#' @return board that contains only 1 and 0
#' @export
#'
#' @examples change_vec <- function(c("."), 1)
change_vec <- function(v, d) {
  res <- vector()
  for (i in 1:length(v)) {
    vec <- change_1(v[i])
    res <- c(res, vec)
  }
  mat <- matrix(res, d, d, byrow = TRUE)
  board(mat)
}

#' check if a string that only contains * and . and space
#'
#' @param s string that contains only * and . and space
#'
#' @return boolean TRUE it only contains * and . and space
#' @export
#'
#' @examples check_contain("*.***")
check_contain <- function(s) {
  for (i in 1:nchar(s)) {
    if (substr(s, i, i) != "*" && substr(s, i, i) != "." && substr(s, i, i) != " ") {
      return (FALSE)
    }
  }
  return (TRUE)
}

#' Read a file and translate back to baord
#'
#' @param x file link
#'
#' @return list of transformed board objs
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")

read_boards <- function(x) {
  lines <- readLines(x)
  lines <- lines[lines != ""]
  lines <- trimws(lines)

  b_lst <- vector(mode = "list")
  res <- FALSE
  for (i in 1:length(lines)) {
    if (lines[i] == "----" && i != length(lines)) {
      if (!grepl("\\D", lines[i+1]) && as.numeric(lines[i+1]) > 0) {
        dim <- as.numeric(lines[i+1])
        for (j in ((i+2):(i+1+dim))) {
          if ((i+2+dim > length(lines)) | (lines[i+2+dim] != "----") |
              (nchar(lines[j]) != (2*dim-1)) |
              (check_contain(lines[j]) == FALSE)) {
            res <- TRUE
          }
        }
        if (res == FALSE) {
          vec <- lines[(i+2):(i+1+dim)]
          lst <- change_vec(vec, dim)
          b_lst <- c(b_lst, list(lst))
        }
        else {
          b_lst <- c(b_lst, list(NA))
        }
      }
      else {
        b_lst <- c(b_lst, list(NA))
      }
    }
  }
  assert_that((sum(lines == "----") - 1) == length(b_lst),
              msg = "file is not properly formatted")
  b_lst
}
