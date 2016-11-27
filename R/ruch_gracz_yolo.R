ruch_gracz_yolo <- function(wektor_gracza, wektor_stada) {
  #znajdujemy najstarsze zwierze, ktorego nie mamy i staramy sie je kupic
  i <- 5
  wartosc_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  while(i >1) {
    if(wektor_gracza[i] == 0 && sum(wektor_gracza[1:(i - 1)]*wartosc_zwierzat[1:(i - 1)]) >= wartosc_zwierzat[i])
    {
      zakup <- kup_zwierze_yolo(i, wektor_gracza, wektor_stada)
      return(zakup)
    }
    i <- i-1
  }
  return(cbind(wektor_gracza, wektor_stada))
}

czy_gracz_ma_oba_psy <- function (stan_gracza) {
  return (stan_gracza['SD']*stan_gracza['BD'] > 0)
}
