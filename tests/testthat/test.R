test_that("Result is a number",{
  expect_true(is.numeric(gra_nk(strategia_nk)))
})

test_that("An invalid number of parameters",{
  expect_error(strategia_nk())
  expect_error(strategia_nk(10))
  expect_error(gra_nk())
  expect_error(gra_nk(10))
  expect_error(badaj_gre_nk())
  expect_error(badaj_gre_nk(gra_nk))
  expect_error(badaj_gre_nk(strategia_nk))
})