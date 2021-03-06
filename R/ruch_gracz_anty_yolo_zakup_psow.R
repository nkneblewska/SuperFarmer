#' @title Zakup brakuj�cych ps�w obronnych
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj�ca brakuj�cego obronnego, je�li nas sta�
#'
#' @param stan_gracza Wektor b�d�cy stanem stada gracza
#' 
#' @export

ruch_gracz_anty_yolo_zakup_psow <- function (stan_gracza) {
  if (stan_gracza['maly_pies'] == 0) {
    return (ruch_gracz_anty_yolo_zakup_malego_psa(stan_gracza))
  } else { #skoro ma�y pies jest w stadzie gracza, a jeste�my w tej funkcji
    #to w stadzie gracza nie ma du�ego psa
    return (ruch_gracz_anty_yolo_zakup_duzego_psa(stan_gracza))
  }
}
