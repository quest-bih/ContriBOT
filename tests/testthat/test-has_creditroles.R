# Test: Edge case tests for 'conception' and 'design' as credit roles
library(testthat)

keyword_list <- .create_keyword_list()

test_that("Edge case tests for 'conception' and 'suggestions' as credit roles", {

  # Test 1: "Conception" shouldnt be part of a credit role
  ### TODO: please use this syntax for other test cases and add more cases,
  # a couple of each of the classifiers, ideally
  expect_false(
    stringr::str_detect(" conception zxx, lxc, zxn", keyword_list$credit_roles)
    )  # "conception" should match a credit role

 # Test 2: "Conception" with other roles
  sentence2 <- "ag, hm, jf, jg, em. sbe, sbr made substantial contributions to the conception and methodology"
  has_credit_role2 <- stringr::str_detect(sentence2, keyword_list$credit_roles)  # Direct use of str_detect
  expect_true(has_credit_role2)

  # Test 3: "Suggestions"
  sentence3 <- "cc and hc helped with suggestions."
  has_credit_role3 <- stringr::str_detect(sentence3, keyword_list$credit_roles)  # Direct use of str_detect
  expect_false(has_credit_role3)
})
