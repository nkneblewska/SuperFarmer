update_stanu_kostka_czerowna <- function(stan_gracza, stan_stada, wartosc_kostki) {
  if (wartosc_kostki == 'F') {
    return (update_stanu_lis(stan_gracza, stan_stada))
  } else {
    return (update_stanu_zwierze_lagodne(stan_gracza, stan_stada, wartosc_kostki))
  }
}
