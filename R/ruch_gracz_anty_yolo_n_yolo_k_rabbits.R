ruch_gracz_anty_yolo_n_yolo_k_rabbits <- function (w_gracza, w_stada, n, k) {
  wartosc_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  aktualna_liczba_krolikow <- sum(w_gracza[1:5]*wartosc_zwierzat[1:5])
  if (aktualna_liczba_krolikow < k) {
    return (ruch_gracz_yolo(w_gracza, w_stada))
  } else {
    return (ruch_gracz_anty_yolo_n_yolo(w_gracza, w_stada, n))
  }
}
