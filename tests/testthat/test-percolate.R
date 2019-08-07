context("percoalte baord fn")

#test 1
test_that("percolate() that all open works", {
  my_board_0 <- board(matrix(1, 10, 10))
  res <- percolate(my_board_0)
  expect_true(res$result_board == board(matrix(2, 10, 10)) && res$result == TRUE)
})

#test 2
test_that("percolate() that all close works ", {
  my_board_1 <- board(matrix(0, 10, 10))
  res <- percolate.board(my_board_1)
  expect_true(res$result_board == board(matrix(0, 10, 10)) && res$result == FALSE)
})


#test 3
test_that("percolate() that top close works ", {
  my_board_top <- generate_board_mat(10, 0.25)
  my_board_top[1,] <-0
  res <- percolate(board(my_board_top))
  expect_true(res$result_board == my_board_top && res$result == FALSE)
})

#test 4
test_that("percolate() that top close works ", {
  my_board_bottom <- board(matrix(1, 10, 10))
  my_board_bottom[10,] <-0
  res <- percolate(board(my_board_bottom))
  my_board_bottom[-10,] <-2
  expect_true(res$result_board == my_board_bottom && res$result == TRUE)
})

