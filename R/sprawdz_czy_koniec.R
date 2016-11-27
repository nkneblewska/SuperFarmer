sprawdz_czy_koniec <- function(x) {
  for (i in x[1:5]) {
    if (i < 1) {
      return (FALSE)
    }
  }
  return (TRUE)
}
