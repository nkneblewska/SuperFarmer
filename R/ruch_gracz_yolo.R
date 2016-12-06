#' @title Strategia YOLO
#'
#' @description 
#' Funkcja wykonuj¹ca ruch strategi¹ YOLO, która w ka¿dym ruchu stara siê kupiæ najdro¿sze brakuj¹ce w stadzie zwierze.
#' Zakup psów jest ignorowany
#' @param wektor_gracza Wektor bêd¹cy stanem stada gracza
#' @return Wektor bêd¹cy stanem stada gracza po wykonaniu ruchu
#' @export

ruch_gracz_yolo <- function(wektor_gracza) {
  #znajdujemy najstarsze zwierze, ktorego nie mamy i staramy sie je kupic
  i <- 5
  wartosc_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  while(i >1) {
    if(wektor_gracza[i] == 0 && sum(wektor_gracza[1:(i - 1)]*wartosc_zwierzat[1:(i - 1)]) >= wartosc_zwierzat[i])
    {
      zakup <- kup_zwierze_yolo(i, wektor_gracza)
      return(zakup)
    }
    i <- i-1
  }
  return(wektor_gracza)
}

czy_gracz_ma_oba_psy <- function (stan_gracza) {
  return (stan_gracza['SD']*stan_gracza['BD'] > 0)
}
