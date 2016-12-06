#' @title Zakup brakuj¹cych psów obronnych
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj¹ca brakuj¹cego obronnego, jeœli nas staæ
#'
#' @param stan_gracza Wektor bêd¹cy stanem stada gracza
#' 
#' @export

ruch_gracz_anty_yolo_zakup_psow <- function (stan_gracza) {
  if (stan_gracza['maly_pies'] == 0) {
    return (ruch_gracz_anty_yolo_zakup_malego_psa(stan_gracza))
  } else { #skoro ma³y pies jest w stadzie gracza, a jesteœmy w tej funkcji
    #to w stadzie gracza nie ma du¿ego psa
    return (ruch_gracz_anty_yolo_zakup_duzego_psa(stan_gracza))
  }
}
