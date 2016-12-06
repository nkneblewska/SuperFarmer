#' @title Rzut jedn¹ kostk¹
#'
#' @description 
#' Funkcja losuj¹ca z rozk³adu jednostajnego element wektora zadanego jako parametr
#' Tym parametrem jest wektor_czerwonej_kostki lub wektor_zielonej_kostki zdefiniowany w zbiorze danych kostki
#'
#' @param wektor_wartosci_kostki 
#' 
#' 
#' @export
#' 
rzut_jedna_kostka <- function(wektor_wartosci_kostki) {
  return (sample(wektor_wartosci_kostki, 1))
}
