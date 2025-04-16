library(testthat)

keyword_list <- .create_keyword_list()

test_that("Edge case tests for IS_CRT", {
  
  # Test 1: Check that credit_roles <= 3 AND n3_credit_roles > 2
  expect_false(
    stringr::str_count("Conceptualization, N.B.V.; investigation, A.I.L.", keyword_list$credit_roles) <= 3 &
      stringr::str_count("Conceptualization, N.B.V.; investigation, A.I.L.", keyword_list$n3_credit_roles) > 2
    
  )
  
  # Test 2: Check that credit_roles <= 3 AND n3_credit_roles > 2
  expect_true(
    stringr::str_count("krishane patel: conceptualization, writing original draft preparation, writing review & editing",
                       keyword_list$credit_roles) <= 3 &
      stringr::str_count("krishane patel: conceptualization, writing original draft preparation, writing review & editing",
                         keyword_list$n3_credit_roles) > 2
  )
  
  # Test 3: strict with n3_credit roles
  expect_false(
    stringr::str_count("conceptualization,  investigation,  data analysis, ; writing,  funding acquisition,",
                       keyword_list$credit_roles) <= 3 &
      stringr::str_count("conceptualization,  investigation,  data analysis, ; writing,  funding acquisition",
                         keyword_list$n3_credit_roles) > 2
  )
  
  # Test 4: strict with n3_credit roles
  expect_false(
    stringr::str_count("conceptualization and writing, f.p., s.s., and s. liebscher; investigation and analyses, f.p., c.d., r.d., x.y., z.a.q., a.o., c.t., m.m., and a.t.; facs, mass spectrometry, and analyses, s.m., s. lagache, a.c.u., and m.h.; reagents, r.a.m.b., w.m.c.v.r.-m., and b.z.; supervision, s.s. and s. liebscher.",
                       keyword_list$credit_roles) <= 3 &
      stringr::str_count("conceptualization and writing, f.p., s.s., and s. liebscher; investigation and analyses, f.p., c.d., r.d., x.y., z.a.q., a.o., c.t., m.m., and a.t.; facs, mass spectrometry, and analyses, s.m., s. lagache, a.c.u., and m.h.; reagents, r.a.m.b., w.m.c.v.r.-m., and b.z.; supervision, s.s. and s. liebscher.",
                         keyword_list$n3_credit_roles) > 2
  )
})
