badaj_gre_nk <- function(gra, strategia) {
  counter <- numeric(10000)
  for (j in 1:10000) {
    gra(strategia)
    counter[j] <- counter[j] + 1
  }
  # Wyniki, Przyklady otrzymanych wynikow:
  head(counter)
  # Podstawowe statystyki:
  summary(counter)
}