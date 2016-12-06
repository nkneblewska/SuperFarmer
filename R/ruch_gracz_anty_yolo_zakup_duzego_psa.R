#' @title Zakup du¿ego psa
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj¹ca du¿ego psa obronnego, jeœli nas staæ
#'
#' @param stan_gracza Wektor bêd¹cy stanem stada gracza
#' 
#' @export

ruch_gracz_anty_yolo_zakup_duzego_psa <- function(stan_gracza) {
  return (zakup_zwierzecia(stan_gracza, 7))
}
