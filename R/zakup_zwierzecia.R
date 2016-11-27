zakup_zwierzecia <- function(stan_gracza, stan_stada, zwierzak) {
  wartosci_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)
  if (sum(stan_gracza[1:5]*wartosci_zwierzat[1:5]) >= wartosci_zwierzat[zwierzak]) {
    return (kup_zwierze_yolo(zwierzak, stan_gracza, stan_stada))
  } else {
    return (cbind(stan_gracza, stan_stada))
  }
}
