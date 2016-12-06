#' @title Funkcja sprawdz_czy_koniec
#'
#' @description 
#' Funkcja sprawdzaj¹ca czy stan stada gracza spe³nia warunki zakoñczenia gry

#'
#' @param x wektor bêd¹cy stanem stada gracza
#' 
#' @export

sprawdz_czy_koniec <- function(x) {
  for (i in x[1:5]) {
    if (i < 1) {
      return (FALSE)
    }
  }
  return (TRUE)
}
