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

test_that("Testowanie strategii: wymiana owcy na kroliki", {
  herd <- c(0, 2, 0, 0, 0, 0, 0)
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
  expect_equivalent(herd["owca"], 1)
})

test_that("Testowanie strategii: wymiana nadwyzki na konia", {
  herd <- c(60, 7, 1, 1, 0, 1, 1)
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
  expect_equivalent(herd["kon"], 1)
  expect_equivalent(herd["owca"], 1)
  expect_equivalent(herd["krolik"], 24)
})

test_that("Testowanie strategii: wymiana nadwyzki na krowe", {
  herd <- c(60, 1, 1, 0, 1, 1, 1)
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
  expect_equivalent(herd["krowa"], 1)
  expect_equivalent(herd["krolik"], 24)
})

test_that("Testowanie strategii: wymiana nadwyzki na swinie", {
  herd <- c(36, 1, 0, 1, 1, 1, 1)
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
  expect_equivalent(herd["swinia"], 1)
  expect_equivalent(herd["krolik"], 24)
})

test_that("Testowanie strategii: wymiana nadwyzki na owce", {
  herd <- c(30, 0, 1, 1, 1, 1, 1)
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
  expect_equivalent(herd["owca"], 1)
  expect_equivalent(herd["krolik"], 24)
})

test_that("Testowanie gry: wynik jest poprawny",{
  ret1 <- gra_nk(strategia_nk)
  ret2 <- gra_nk(ruch_gracz_anty_yolo)
  expect_true(is.numeric(ret1))
  expect_true(is.numeric(ret2))
  expect_gte(ret1, 0)
  expect_gte(ret2, 0)
})

# Zbyt dlugi czas wykonania na Unit Test
# test_that("Testowanie badaj gre: wynik jest poprawny",{
#   ret <- badaj_gre_nk(gra_nk, strategia_nk)
#   expect_length(ret, 6)
#   expect_lte(ret["Mean"], 50)
# })

test_that("Testowanie strategii: bledne parametry",{
  expect_error(strategia_nk())
  expect_error(strategia_nk(10))
})

test_that("Testowanie gry: bledne parametry",{
  expect_error(gra_nk())
  expect_error(gra_nk(10))
})

test_that("Testowanie badaj gre: bledne parametry",{
  expect_error(badaj_gre_nk())
  expect_error(badaj_gre_nk(gra_nk))
  expect_error(badaj_gre_nk(strategia_nk))
})


test_that("Testowanie kup_zwierze: czy nie kupuje jeœli nie staæ",{
  example1 <- c(4, 0, 0, 0, 0, 0, 1)
  expect_equal(kup_zwierze_yolo(4, example1), example1)
  example2 <- c(0, 1, 0, 0, 0, 0, 1)
  expect_equal(kup_zwierze_yolo(7, example2), example2)

})