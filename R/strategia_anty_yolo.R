#' @title Ruch strategi� ANTY-YOLO
#'
#' @description 
#' Funkcja ruch_gracza_anty_yolo dla zadanego wektora gracza zwraca nowy wektor gracza po wykonaniu 
#' ruchu strategi� ANTY-YOLO, kt�ra skupia si� w pierwszej kolejno�ci na zakupie obu ps�w obronnych.
#' Je�li ju� to osi�gnie, grana jest strategia YOLO.
#' 
#' @param stan_gracza Wektor b�d�cy stanem stada gracza
#' 
#' @export

strategia_anty_yolo <- function (stan_gracza) {
  if (stan_gracza["maly_pies"] == 1 && stan_gracza["duzy_pies"] == 1) {
    return (strategia_yolo(stan_gracza))
  } else {
    return (ruch_gracz_anty_yolo_zakup_psow(stan_gracza))
  }
}
