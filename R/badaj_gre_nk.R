#' @title Powtórzenie gry 10000 razy
#'
#' @description 
#' Funkcja badaj_gre() powtarza grę zadaną jako parametr przy zadanej strategii
#' 10000 razy. Funkcja wypisuje podstawowe statystyki wektora wyników przekazywanych przez grę.
#'
#' @param gra Funkcja gry, która jest powtarzana.
#' @param strategia Funckja implementująca strategię, którą przyjmuje gracz podczas gry.
#' 
#' @export

badaj_gre_nk <- function(gra, strategia) {
  counter <- numeric(10000)
  for (j in 1:10000) {
    counter[j] <- gra(strategia)
  }
  # Wyniki, Przyklady otrzymanych wynikow:
  head(counter)
  # Podstawowe statystyki:
  summary(counter)
}