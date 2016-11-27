update_stanu_atak_wilka <- function(stan_gracza, stan_stada) {
  for (i in 1:4) {
    wynik_zamienienia <- zamien_zwierzaki(stan_gracza, stan_stada, names(stan_gracza)[i],
                                          (-1)*stan_gracza[names(stan_stada)[i]])
  }
  return (wynik_zamienienia)
}
