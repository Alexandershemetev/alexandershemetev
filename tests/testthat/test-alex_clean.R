a=2
    b=4
    c=8
alex_clean(except = "b")
test_that("multiplication works", {
  expect_equal(b, 4)
})
