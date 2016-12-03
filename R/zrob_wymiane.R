zrob_wymiane <- function(stado8) {
  #parametry strategii - ile zwierzat zostawiamy, przy ilu krolikach kupujemy 1 i 2 malego psa
  Rozrodcze <- c(24, 1, 1, 1, 1, 4, 2)
  pies1 <- 11
  pies2 <- 17

  Nadwyzka <- ifelse( stado8 > Rozrodcze , stado8 - Rozrodcze , 0)

  Wartosc_nadwyzki <- Nadwyzka * Cena1

  Suma_nadwyzki <- sum(Wartosc_nadwyzki)

  nazwy_zwierzat <- c("Krolik", "Owca", "Swinia", "Krowa", "Kon", "MalyPies", "DuzyPies")
  names(Nadwyzka) <- nazwy_zwierzat
  names(Wartosc_nadwyzki) <- nazwy_zwierzat

  Wymiana(stado8, Suma_nadwyzki, Wartosc_nadwyzki, Nadwyzka, pies1, pies2)
}
