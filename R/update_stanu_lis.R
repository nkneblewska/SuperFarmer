update_stanu_lis <- function(stan_gracza, stan_stada) {
  if (stan_gracza['SD'] > 0) {
    return (zamien_zwierzaki(stan_gracza, stan_stada, zwierzak = 'SD', -1))
  } else {
    return (update_stanu_atak_lisa(stan_gracza, stan_stada))
  }
}
