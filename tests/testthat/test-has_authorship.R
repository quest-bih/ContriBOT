# Test: Edge case tests for Credit Authorship

keyword_list <- .create_keyword_list()

test_that("Edge case tests for 'guarantor' and 'having full access/takes responsibility' as authorship indicators", {
  
  # Test 1: "guarantor"
  expect_true(
    stringr::str_detect("fz was acting as guarantor", keyword_list$responsibility  )
  )
  
  # Test 2: "full access and takes responsibility" with specific person
  expect_true(
    stringr::str_detect("dr kim had full access to all of the data in the study and takes responsibility for the integrity of the data and the accuracy of the data analysis", 
      keyword_list$responsibility
    )
  )
  
  # Test 3: "full access and takes responsibility" with different role
  expect_true(
    stringr::str_detect(
      "Drs Willis and Hystad had full access to all of the data in the study and takes responsibility for the integrity of the data and the accuracy of the data analysis", 
      keyword_list$responsibility
    )
  )
  
})
