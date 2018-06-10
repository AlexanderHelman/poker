library(poker)

context("deal_hand")

test_that("size of a hand", {
  expect_equal(length(deal_hand()), 5)
})
