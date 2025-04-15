# Test: Edge case tests for Credit Authorship
library(testthat)

keyword_list <- .create_keyword_list()

test_that("Edge case tests for 'guarantor' and 'having full access/takes responsibility' as Authorship", {
  
  # Test 1: "guarantor" 
  sentence1 <- "fz was acting as guarantor"
  has_authorship1 <- stringr::str_detect(sentence1, keyword_list$responsibility) 
  expect_true(has_authorship1)  
  
  
  # Test 2: "having full access and takes responsilbity" with other roles
  sentence2 <- "dr kim had full access to all of the data in the study and takes responsibility for the integrity of the data and the accuracy of the data analysis"
  has_authorship2 <- stringr::str_detect(sentence2, keyword_list$responsibility)  
  expect_true(has_authorship2)
  
  # Test 3: "having full access and takes responsilbity" with other roles
  sentence3 <- "Drs Willis and Hystad had full access to all of the data in the study and takes responsibility for the integrity of the data and the accuracy of the data analysis "
  has_authorship3 <- stringr::str_detect(sentence3, keyword_list$responsibility)  
  expect_true(has_authorship3)
})
