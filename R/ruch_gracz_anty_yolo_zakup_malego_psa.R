#' @title Zakup ma�ego psa
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj�ca ma�ego psa obronnego, je�li nas sta�
#'
#' @param stan_gracza Wektor b�d�cy stanem stada gracza
#' 
#' @export

ruch_gracz_anty_yolo_zakup_malego_psa <- function(stan_gracza) {
  return (zakup_zwierzecia(stan_gracza, 6))
}
