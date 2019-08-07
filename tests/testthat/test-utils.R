context("test board fn")
#1
test_that("generate_board_mat() works with default value", {
  res <- generate_board_mat()
  expect_true(all(res %in% c(0, 1)))
})

#2
test_that("generate_board_mat() works, not using default value", {
  res <- generate_board_mat(n=3, p=0.3)
  expect_true(all(res %in% c(0, 1)))
})

#3
test_that("generate_board_mat() works when p=0", {
  res <- generate_board_mat(5, 0)
  # bool <- check_mat(res, 5, 0)
  expect_true(all(res %in% c(1)))
})

#4
test_that("generate_board_mat() works when p=1", {
  res <- generate_board_mat(5, 1)
  # bool <- check_mat(res, 5, 0)
  expect_true(all(res %in% c(0)))
})

#5
test_that("generate_board_mat() errors for invalid n", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asdf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})



#1
test_that("is_valid() errors for non integer", {
  expect_error(is_valid(c(matrix(c(1,2,3,4.2), nrow = 2))))
})

#2
test_that("is_valid() errors for not a square matrix", {
  expect_error(is_valid(matrix(c(1,2,0,0), nrow = 3)))
})
#3
test_that("is_valid() errors for only has 0,1,2", {
  expect_error(is_valid(matrix(0, 1, 3)))
})
