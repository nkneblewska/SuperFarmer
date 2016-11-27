ruch_gracz_anty_yolo_n_yolo <- function (w_gracza, w_stada, n) {
  if (n > 0) {
    return (ruch_gracz_anty_yolo(w_gracza, w_stada))
  } else {
    return (ruch_gracz_yolo(w_gracza, w_stada))
  }
}
