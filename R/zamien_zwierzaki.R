zamien_zwierzaki <- function(stan_gracza, stan_stada, zwierzak, ile) {
  #Je?li ile jest ujemne, to zwierze wraca od gracza do stada g??wnego
  wartosc_zmiany <- min(stan_stada[zwierzak], ile)
  #   print("zamien zwierzaki, wartosc_zmiany:")
  #   print(wartosc_zmiany)
  stan_gracza[zwierzak] <- stan_gracza[zwierzak] + wartosc_zmiany
  stan_stada[zwierzak] <- stan_stada[zwierzak] - wartosc_zmiany
  return (cbind(stan_gracza, stan_stada))
}
