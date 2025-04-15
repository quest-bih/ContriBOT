# Test: Edge case tests for credit roles such as 'conception' and 'conceived' 
library(testthat)

keyword_list <- .create_keyword_list()

test_that("Edge case tests for 'conception' and 'conceived' as credit roles", {
  
  # Test 1: "Conception" should not be matched alone
  expect_false(
    stringr::str_detect("conception zxx, lxc, zxn", keyword_list$credit_roles)
  )
  
  # Test 2: "Conception" in a valid credit role 
  expect_true(
    stringr::str_detect("ag, hm, jf, jg, em. sbe, sbr made substantial contributions to the conception and methodology", keyword_list$credit_roles)
  )
  
    # Test 3: "conceived"
  expect_false(
    stringr::str_detect("mr conceived the study and took the ,main responsibility ", keyword_list$credit_roles)
  )
  
  expect_true(
    stringr::str_detect("Krishane Patel: conceptualization, writing original draft preparation, writing review & editing.", keyword_list$credit_roles)
  )
  
  # Test 5: Suggestion
  expect_false(
    stringr::str_detect("cc and hc helped with suggestions.", keyword_list$credit_roles)
  )
  
})
