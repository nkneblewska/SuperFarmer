#' @title Funkcja sprawdz_czy_koniec
#'
#' @description 
#' Funkcja sprawdzaj�ca czy stan stada gracza spe�nia warunki zako�czenia gry

#'
#' @param x wektor b�d�cy stanem stada gracza
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
