update_stanu_wilk <- function(stan_gracza, stan_stada) {
  if (stan_gracza['BD'] > 0) {
    return (zamien_zwierzaki(stan_gracza, stan_stada, zwierzak = 'BD', -1))
  } else {
    return (update_stanu_atak_wilka(stan_gracza, stan_stada))
  }
}
