context("testing the main function of the package - get background")
        

test_that("get_background_produces adequate results",{
  with_mock(
    'gisserver::get_background' = function() TRUE,
    expect_true(get_background())
  )
})

