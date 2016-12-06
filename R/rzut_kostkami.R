#' @title Funkcja rzucaj¹ca kostkami gry Super Farmer
#'
#' 
#' @export

rzut_kostkami <- function() {
  zielona <-
    rzut_jedna_kostka(wektor_zielonej_kostki)
  czerwona <-
    rzut_jedna_kostka(wektor_czerownej_kostki)

  return (c('zielona' = zielona,'czerwona' =  czerwona))
}
