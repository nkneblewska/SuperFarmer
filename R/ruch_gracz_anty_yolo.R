#' @title Ruch strategi¹ ANTY-YOLO
#'
#' @description 
#' Funkcja ruch_gracza_anty_yolo dla zadanego wektora gracza zwraca nowy wektor gracza po wykonaniu 
#' ruchu strategi¹ ANTY-YOLO, która skupia siê w pierwszej kolejnoœci na zakupie obu psów obronnych.
#' Jeœli ju¿ to osi¹gnie, grana jest strategia YOLO.
#' 
#' @param stan_gracza Wektor bêd¹cy stanem stada gracza
#' 
#' @export

ruch_gracz_anty_yolo <- function (stan_gracza) {
  if (stan_gracza["maly_pies"] == 1 && stan_gracza["duzy_pies"] == 1) {
    return (ruch_gracz_yolo(stan_gracza))
  } else {
    return (ruch_gracz_anty_yolo_zakup_psow(stan_gracza))
  }
}
