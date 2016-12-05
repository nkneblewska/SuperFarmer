test_that("Testowanie strategii: dane poczatkowe", {
  herd <- c(0, 0, 0, 0, 0, 0, 0)
  names(herd) <-
  c("krolik",
    "owca",
    "swinia",
    "krowa",
    "kon",
    "maly_pies",
    "duzy_pies")
  herd <-strategia_nk(herd)
  expect_length(herd, 7)
  expect_gte(herd["krolik"], 0)
  expect_gte(herd["owca"], 0)
  expect_gte(herd["swinia"], 0)
  expect_gte(herd["krowa"], 0)
  expect_gte(herd["kon"], 0)
  expect_gte(herd["maly_pies"], 0)
  expect_gte(herd["duzy_pies"], 0)
  expect_lte(herd["krolik"], 60)
  expect_lte(herd["owca"], 24)
  expect_lte(herd["swinia"], 20)
  expect_lte(herd["krowa"], 12)
  expect_lte(herd["kon"], 6)
  expect_lte(herd["maly_pies"], 4)
  expect_lte(herd["duzy_pies"], 2)
})

test_that("Testowanie strategii: wymiana malego psa na kroliki", {
  herd <- c(0, 0, 0, 0, 0, 1, 0)
  names(herd) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  herd <-strategia_nk(herd)
  expect_length(herd, 7)
  expect_gte(herd["krolik"], 0)
  expect_gte(herd["owca"], 0)
  expect_gte(herd["swinia"], 0)
  expect_gte(herd["krowa"], 0)
  expect_gte(herd["kon"], 0)
  expect_gte(herd["maly_pies"], 0)
  expect_gte(herd["duzy_pies"], 0)
  expect_lte(herd["krolik"], 60)
  expect_lte(herd["owca"], 24)
  expect_lte(herd["swinia"], 20)
  expect_lte(herd["krowa"], 12)
  expect_lte(herd["kon"], 6)
  expect_lte(herd["maly_pies"], 4)
  expect_lte(herd["duzy_pies"], 2)
  expect_equivalent(herd["krolik"], 6)
  expect_equivalent(herd["maly_pies"], 0)
})

test_that("Testowanie strategii: wymiana krolikow na malego psa", {
  herd <- c(11, 0, 0, 0, 0, 0, 0)
  names(herd) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  herd <-strategia_nk(herd)
  expect_length(herd, 7)
  expect_gte(herd["krolik"], 0)
  expect_gte(herd["owca"], 0)
  expect_gte(herd["swinia"], 0)
  expect_gte(herd["krowa"], 0)
  expect_gte(herd["kon"], 0)
  expect_gte(herd["maly_pies"], 0)
  expect_gte(herd["duzy_pies"], 0)
  expect_lte(herd["krolik"], 60)
  expect_lte(herd["owca"], 24)
  expect_lte(herd["swinia"], 20)
  expect_lte(herd["krowa"], 12)
  expect_lte(herd["kon"], 6)
  expect_lte(herd["maly_pies"], 4)
  expect_lte(herd["duzy_pies"], 2)
  expect_equivalent(herd["krolik"], 5)
  expect_equivalent(herd["maly_pies"], 1)
})

test_that("Testowanie strategii: wymiana krolikow na drugiego malego psa", {
  herd <- c(17, 0, 0, 0, 0, 1, 0)
  names(herd) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  herd <-strategia_nk(herd)
  expect_length(herd, 7)
  expect_gte(herd["krolik"], 0)
  expect_gte(herd["owca"], 0)
  expect_gte(herd["swinia"], 0)
  expect_gte(herd["krowa"], 0)
  expect_gte(herd["kon"], 0)
  expect_gte(herd["maly_pies"], 0)
  expect_gte(herd["duzy_pies"], 0)
  expect_lte(herd["krolik"], 60)
  expect_lte(herd["owca"], 24)
  expect_lte(herd["swinia"], 20)
  expect_lte(herd["krowa"], 12)
  expect_lte(herd["kon"], 6)
  expect_lte(herd["maly_pies"], 4)
  expect_lte(herd["duzy_pies"], 2)
  expect_equivalent(herd["krolik"], 11)
  expect_equivalent(herd["maly_pies"], 2)
})

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