#' @title Zakup zwierzêcia
#'
#' @description 
#' Funkcja pomocnicza strategii ANTY-YOLO, kupuj¹ca zadane w parametrze zwierze, jeêli nas staæ
#'
#' @param stan_gracza Wektor bêd¹cy stanem stada gracza
#' @return Wektor gracza po udanej lub nie próbie zakupu
#' @export

zakup_zwierzecia <- function(stan_gracza, zwierzak) {
  wartosci_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  if (sum(stan_gracza[1:5]*wartosci_zwierzat[1:5]) >= wartosci_zwierzat[zwierzak]) {
    return (kup_zwierze_yolo(zwierzak, stan_gracza))
  } else {
    return (stan_gracza)
  }
}
