#' @title Zakup zwierz�cia
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj�ca zadane w parametrze zwierze, je�li nas sta�
#'
#' @param stan_gracza Wektor b�d�cy stanem stada gracza
#' @return Wektor gracza po udanej lub nie pr�bie zakupu
#' @export

zakup_zwierzecia <- function(stan_gracza, zwierzak) {
  wartosci_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  if (sum(stan_gracza[1:5]*wartosci_zwierzat[1:5]) >= wartosci_zwierzat[zwierzak]) {
    return (kup_zwierze_yolo(zwierzak, stan_gracza))
  } else {
    return (stan_gracza)
  }
}
