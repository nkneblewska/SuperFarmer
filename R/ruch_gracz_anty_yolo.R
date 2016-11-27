ruch_gracz_anty_yolo <- function (stan_gracza, stan_stada) {
  if (czy_gracz_ma_oba_psy(stan_gracza)) {
    return (ruch_gracz_yolo(stan_gracza, stan_stada))
  } else {
    return (ruch_gracz_anty_yolo_zakup_psow(stan_gracza, stan_stada))
  }
}
