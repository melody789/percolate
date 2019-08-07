context("test board fn")

test_that("equivalent matrix", {
  board1 <- board()
  expect_equivalent(board1, board(board1))
  expect_equivalent(unclass(board1), unclass(board(board1)))
})

test_that("board() works", {
  mat1 <- matrix(0, 4, 4)
  mat2 <- board(mat1)
  expect_equivalent(mat1, unclass(mat2) & attr(mat2, "n") == 4 & attr(mat2, "p") == 1)
})


test_that("check incorrect matrix", {
  # not matrix
  expect_error(board(mat = c(0,1)))
  # not square
  expect_error(board(mat = matrix(nrow = 4, ncol = 3)))
  # not values in 0,1,2
  mat2 <- matrix(1:16, nrow = 4, ncol = 4)
  expect_error(board(mat = mat2))
})
