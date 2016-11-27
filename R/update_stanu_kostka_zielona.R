update_stanu_kostka_zielona <- function(stan_gracza, stan_stada, wartosc_kostki) {
  if (wartosc_kostki == 'W') {
    return (update_stanu_wilk(stan_gracza, stan_stada))
  } else {
    return (update_stanu_zwierze_lagodne(stan_gracza, stan_stada, wartosc_kostki))
  }
}
