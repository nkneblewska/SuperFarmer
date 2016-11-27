ruch_gracz_anty_yolo_zakup_psow <- function (stan_gracza, stan_stada) {
  if (stan_gracza['SD'] == 0) {
    return (ruch_gracz_anty_yolo_zakup_malego_psa(stan_gracza, stan_stada))
  } else { #skoro ma?y pies jest w stadzie gracza, a jeste?my w tej funkcji
    #to w stadzie gracza nie ma du?ego psa
    return (ruch_gracz_anty_yolo_zakup_duzego_psa(stan_gracza, stan_stada))
  }
}
