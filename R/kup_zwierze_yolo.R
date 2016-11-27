kup_zwierze_yolo <- function(jakie_zwierze, wektor_gracza, wektor_stada){
  wartosc_zwierzat <- c(R = 1, S = 6, P = 12, C = 36, H = 72, SD = 6, BD = 36)

  wektor_wymiany <- c(R = 0, S = 0,  P = 0, C = 0, H = 0, SD = 0, BD = 0)

  suma = 0
  zwierze <- 1
  ile_zwierzat <- 1
  i <- 1
  koniec <- FALSE

  while(i < jakie_zwierze && koniec == FALSE) {
    if(wektor_gracza[i] != 0){
      for(j in 1:wektor_gracza[i]) {

        if(suma >= wartosc_zwierzat[jakie_zwierze]) {
          koniec <- TRUE
          break
        }
        suma = suma + wartosc_zwierzat[i]
        zwierze <- i
        ile_zwierzat <- j
      }
    }
    i <- i+1
  }

  #tworzymy wektor do wymiany
  for(i in 1:zwierze) {
    wektor_wymiany[i] <- wektor_gracza[i]
  }
  wektor_wymiany[zwierze] <- ile_zwierzat

  #oddajem namiarowe kroliki
  wektor_wymiany[1] <- wektor_wymiany[1] - (suma - wartosc_zwierzat[jakie_zwierze])

  wektor_gracza <- wektor_gracza - wektor_wymiany
  wektor_stada <- wektor_stada + wektor_wymiany

  wektor_gracza[jakie_zwierze] = wektor_gracza[jakie_zwierze] + 1
  wektor_stada[jakie_zwierze] = wektor_stada[jakie_zwierze] - 1


  return(cbind(wektor_gracza,wektor_stada))
}
