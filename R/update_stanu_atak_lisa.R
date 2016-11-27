update_stanu_atak_lisa <- function(stan_gracza, stan_stada) {
  return (zamien_zwierzaki(stan_gracza, stan_stada, names(stan_gracza)[1],
                           (-1)*stan_gracza[names(stan_gracza)[1]]))
}
