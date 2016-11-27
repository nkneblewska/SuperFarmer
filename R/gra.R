gra <- function(debug = FALSE) {

  wektor_gracza <- c(R = 0, S = 0, P = 0, C = 0, H = 0, SD = 0, BD = 0)
  wektor_stada <- c(R = 60, S = 24, P = 20, C = 12, H = 6, SD = 4, BD = 2)
  kostki <- c(zielona = 'R', czerwona = 'R')
  koniec  = FALSE
  ile <- 0
  n <- 5
  k <- 40

  while (!koniec) {
    up <- ruch_gracz_yolo(wektor_gracza, wektor_stada);
    wektor_gracza <- up[,1]
    wektor_stada <- up[,2]
    koniec <- sprawdz_czy_koniec(wektor_gracza)
    kostki <- rzut_kostkami()
    up <- update_stanu(wektor_gracza, wektor_stada, kostki)
    wektor_gracza <- up[,1]
    wektor_stada <- up[,2]
    koniec <- sprawdz_czy_koniec(wektor_gracza)
    ile <- ile + 1
  }
  return (ile)
}
