# Test: Edge case tests for 'conception' and 'design' as credit roles
library(testthat)

keyword_list <- .create_keyword_list()

test_that("Edge case tests for 'conception' and 'suggestions' as credit roles", {

  # Test 1: "Conception" shouldnt be part of a credit role
  sentence1 <- " conception zxx, lxc, zxn"
  has_credit_role1 <- stringr::str_detect(sentence1, keyword_list$credit_roles) 
  expect_false(has_credit_role1)  # "conception" should match a credit role


  # Test 2: "Conception" with other roles
  sentence2 <- "ag, hm, jf, jg, em. sbe, sbr made substantial contributions to the conception and methodology"
  has_credit_role2 <- stringr::str_detect(sentence2, keyword_list$credit_roles)  
  expect_true(has_credit_role2)
  
  # Test 3: "Suggestions"
  sentence3 <- "cc and hc helped with suggestions."
  has_credit_role3 <- stringr::str_detect(sentence3, keyword_list$credit_roles)  
  expect_false(has_credit_role3)
})