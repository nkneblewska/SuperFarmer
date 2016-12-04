#' @title Strategia gry w SuperFarmera
#'
#' @description 
#' Funkcja strategia dokonuje wymian zwierząt na podstawie obecnego stanu stada.
#' 
#' @param herd Obecny stan stada gracza.
#' 
#' @return stan stada po wymianach
#' 
#' @export

strategia_nk <- function(herd) {
  # Ceny zwierzat
  price <- c(1, 6, 12, 36, 72, 6, 36)
  names(price) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  DoExchange(herd, price)
}


DoExchange <- function(herd, price) {
  # Parametry strategii: ile zwierzat zostawiamy,
  # przy ilu krolikach decydujemy sie na kupno jednego lub dwoch malych psow
  animalsToExchange <- c(24, 1, 1, 1, 1, 4, 2)
  firstDogPrice <- 11
  secondDogPrice <- 17
  animalsOverflow <-
    ifelse(herd > animalsToExchange, herd - animalsToExchange, 0)
  animalsOverflowPrice <- animalsOverflow * price
  overflowPriceSum <- sum(animalsOverflowPrice)
  names <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  names(animalsOverflow) <- names
  names(animalsOverflowPrice) <- names
  Exchange(
    herd,
    overflowPriceSum,
    animalsOverflowPrice,
    animalsOverflow,
    firstDogPrice,
    secondDogPrice,
    price
  )
}

Exchange <-
  function(herd,
           overflowPriceSum,
           animalsOverflowPrice,
           animalsOverflow,
           firstDogPrice,
           secondDogPrice,
           price) {
    if (all(herd[c("krolik", "owca", "swinia", "krowa")] == c(0, 0, 0, 0)) &&
        herd["maly_pies"] > 0) {
      herd <- ExchangeSmallDogToRabbits(herd)
    } else if (herd["krolik"] >= firstDogPrice &&
               herd["maly_pies"] < 1) {
      herd <- ExchangeRabbitsToSmallDog(herd)
    } else if (CanBuyNewAnimals(herd, overflowPriceSum, price)) {
      herd <-
        ExchangeAnimals(herd,
                        overflowPriceSum,
                        animalsOverflowPrice,
                        animalsOverflow,
                        price)
    } else if (overflowPriceSum >= price["krowa"] &&
               herd["krowa"] < 2) {
      herd <-
        Buy("krowa",
            herd,
            animalsOverflowPrice,
            animalsOverflow,
            price)
    } else if (herd["krolik"] >= secondDogPrice &&
               herd["maly_pies"] < 2) {
      herd <- ExchangeRabbitsToSmallDog(herd)
    }
    return(herd)
  }

ExchangeSmallDogToRabbits <- function(herd) {
  herd["maly_pies"] <- herd["maly_pies"] - 1
  herd["krolik"] <- herd["krolik"] + 6
  return(herd)
}

ExchangeRabbitsToSmallDog <- function(herd) {
  herd["krolik"] <- herd["krolik"] - 6
  herd["maly_pies"] <- herd["maly_pies"] + 1
  return(herd)
}

CanBuyNewAnimals <- function(herd, overflowPriceSum, price) {
  ret <- FALSE
  for (animal in c("krolik", "owca", "swinia", "krowa", "kon")) {
    if (overflowPriceSum > price[animal] && herd[animal] == 0) {
      ret <- TRUE
    }
  }
  return(ret)
}

ExchangeAnimals <-
  function(herd,
           overflowPriceSum,
           animalsOverflowPrice,
           animalsOverflow,
           price) {
    if (herd["krolik"] == 0 && herd["owca"] > 0) {
      herd <- ExchangeSheepToRabbits(herd)
    } else {
      for (animal in c("kon", "krowa", "swinia", "owca")) {
        if (overflowPriceSum >= price[animal] && herd[animal] == 0) {
          herd <-
            Buy(animal,
                herd,
                animalsOverflowPrice,
                animalsOverflow,
                price)
          break()
        }
      }
    }
    return(herd)
  }

ExchangeSheepToRabbits <- function(herd) {
  herd["owca"] <- herd["owca"] - 1
  herd["krolik"] <- herd["krolik"] + 6
  return(herd)
}

Buy <- function(animalToBuy,
                herd,
                animalsOverflowPrice,
                animalsOverflow,
                price) {
  priceToPay <- price[animalToBuy]
  for (animal in c("kon", "krowa", "swinia", "owca", "krolik")) {
    if (priceToPay != 0) {
      if (animalsOverflowPrice[animal] >= priceToPay) {
        if (priceToPay < price[animal]) {
          herd[animal] <- herd[animal] - 1
          herd[animalToBuy] <-
            herd[animalToBuy] + (price[animal] / price[animalToBuy])
          priceToPay <- 0
        } else {
          herd[animal] <- herd[animal] - priceToPay / price[animal]
          herd[animalToBuy] <- herd[animalToBuy] + 1
          priceToPay <- 0
        }
      } else {
        herd[animal] <- herd[animal] - animalsOverflow[animal]
        priceToPay <- priceToPay - animalsOverflowPrice[animal]
      }
    }
  }
  return(herd)
}
