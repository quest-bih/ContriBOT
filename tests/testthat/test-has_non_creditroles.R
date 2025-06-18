# Test: Edge case tests for non credit roles such as 'design'
keyword_list <- .create_keyword_list()

test_that("Edge case tests for non credit roles", {
  
  # Test 1: "Design" as part of non-credit role
  expect_true(
    stringr::str_detect(
      "j.a., o.g., a.t., e.a.c.b., and m.m.v.; design,", keyword_list$non_credit_roles)
  )
  
  # Test 2: "data collection" as part of non-credit role
  expect_true(
    stringr::str_detect("conceptualization: mr, ssv, alc, gmh ; data curation: mr, alc; formal analysis: mr; funding acquisition: mr, jz, gmh; investigation/data collection: mr, alc, jz, ghm; methodology: mr, ssv, ghm; project administration: mmr; supervision: gmh; validation: mr, ssv; visualization: mr; writing – original draft: mr, ssv, gmh; writing – review and editing: mr, ssv, sjs, alc, jz, gmh.", 
      keyword_list$non_credit_roles
    )
  )
  
  # Test 3: "study design" as part of non-credit role
  expect_true(
    stringr::str_detect(
      "br, mm, ba, md, cl, and tr contributed to study design", keyword_list$non_credit_roles))
  
  # Test 1: "Design" in brackets as non non credit roles
  expect_false(
    stringr::str_detect(
      "j.a., o.g., a.t., e.a.c.b., and m.m.v.; (design of software),", keyword_list$non_credit_roles)
  )
  
  
})

