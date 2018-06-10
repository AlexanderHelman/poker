library(poker)

context("what hand")

test_that("sanity check", {
  expect_equal(what_hand(c("As", "Ah", "5d", "4c", "8h")), "pair")
  expect_equal(what_hand(c("Qs", "Qh", "Js", "Jh", "5d")), "two pair")
  expect_equal(what_hand(c("3h", "8h", "9h", "Kh", "Ah")), "flush")
  expect_equal(what_hand(c("Ah", "2h", "3h", "4h", "5h")), "straight flush")
  expect_equal(what_hand(c("Ah", "Ac", "As", "Kc", "Kh")), "full house")
})


