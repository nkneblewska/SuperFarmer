#' @title Powtorzenie gry 10000 razy
#'
#' @description 
#' Funkcja badaj_gre() powtarza gre zadana jako parametr przy zadanej strategii
#' 10000 razy. Funkcja wypisuje podstawowe statystyki wektora wynikow przekazywanych przez gre.
#'
#' @param gra Funkcja gry, ktora jest powtarzana.
#' @param strategia Funckja implementujaca strategie, ktora przyjmuje gracz podczas gry.
#' 
#' @export

badaj_gre_nk <- function(gra, strategia) {
  counter <- numeric(10000)
  for (j in 1:10000) {
    counter[j] <- gra(strategia)
  }
  # Wyniki, Przyklady otrzymanych wynikow:
  # head(counter)
  # Podstawowe statystyki:
  summary(counter)
}