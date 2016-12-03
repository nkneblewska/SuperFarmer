# Ceny zwierząt
Price <- c(1, 6, 12, 36, 72, 6, 36)
names(Price) <-
  c("Krolik",
    "Owca",
    "Swinia",
    "Krowa",
    "Kon",
    "MalyPies",
    "DuzyPies")

################### Wymiany ###################

DoExchange <- function(herd) {
  # Parametry strategii: ile zwierząt zostawiamy,
  # przy ilu królikach decydujemy się na kupno jednego lub dwóch małych psów
  animalsToExchange <- c(24, 1, 1, 1, 1, 4, 2)
  firstDogPrice <- 11
  secondDogPrice <- 17
  animalsOverflow <- ifelse(herd > animalsToExchange, herd - animalsToExchange, 0)
  animalsOverflowPrice <- animalsOverflow * Price
  overflowPriceSum <- sum(animalsOverflowPrice)
  names <-
    c("Krolik",
      "Owca",
      "Swinia",
      "Krowa",
      "Kon",
      "MalyPies",
      "DuzyPies")
  names(animalsOverflow) <- names
  names(animalsOverflowPrice) <- names
  Exchange(herd,
          overflowPriceSum,
          animalsOverflowPrice,
          animalsOverflow,
          firstDogPrice,
          secondDogPrice)
}

Exchange <-
  function(herd,
           overflowPriceSum,
           animalsOverflowPrice,
           animalsOverflow,
           firstDogPrice,
           secondDogPrice) {
    if (all(herd[c("Krolik", "Owca", "Swinia", "Krowa")] == c(0, 0, 0, 0)) &&
        herd["MalyPies"] > 0) {
      herd <- ExchangeSmallDogToRabbits(herd)
    } else if (herd["Krolik"] >= firstDogPrice && herd["MalyPies"] < 1) {
      herd <- ExchangeRabbitsToSmallDog(herd)
    } else if (CanBuyNewAnimals(herd, overflowPriceSum)) {
      herd <-
        ExchangeAnimals(herd, overflowPriceSum, animalsOverflowPrice, animalsOverflow)
    } else if (overflowPriceSum >= Price["Krowa"] &&
               herd["Krowa"] < 2) {
      herd <- Buy("Krowa", herd, animalsOverflowPrice, animalsOverflow)
    } else if (herd["Krolik"] >= secondDogPrice && herd["MalyPies"] < 2) {
      herd <- ExchangeRabbitsToSmallDog(herd)
    }
    return(herd)
  }

ExchangeSmallDogToRabbits <- function(herd) {
  herd["MalyPies"] <- herd["MalyPies"] - 1
  herd["Krolik"] <- herd["Krolik"] + 6
  return(herd)
}

ExchangeRabbitsToSmallDog <- function(herd) {
  herd["Krolik"] <- herd["Krolik"] - 6
  herd["MalyPies"] <- herd["MalyPies"] + 1
  return(herd)
}

CanBuyNewAnimals <- function(herd, overflowPriceSum) {
  ret <- FALSE
  for (animal in c("Krolik", "Owca", "Swinia", "Krowa", "Kon")) {
    if ((overflowPriceSum > Price[animal]) && (herd[animal] == 0)) {
      ret <- TRUE
    }
  }
  return(ret)
}

ExchangeAnimals <-
  function(herd,
           overflowPriceSum,
           animalsOverflowPrice,
           animalsOverflow) {
    if (herd["Krolik"] == 0 && herd["Owca"] > 0) {
      herd <- ExchangeSheepToRabbits(herd)
    } else {
      for (animal in c("Kon", "Krowa", "Swinia", "Owca")) {
        if (overflowPriceSum >= Price[animal] && herd[animal] == 0) {
          herd <- Buy(animal, herd, animalsOverflowPrice, animalsOverflow)
          break()
        }
      }
    }
    return(herd)
  }

ExchangeSheepToRabbits <- function(herd) {
  herd["Owca"] <- herd["Owca"] - 1
  herd["Krolik"] <- herd["Krolik"] + 6
  return(herd)
}

Buy <- function(animalToBuy,
                herd,
                animalsOverflowPrice,
                animalsOverflow) {
  priceToPay <- Price[animalToBuy]
  for (animal in c("Kon", "Krowa", "Swinia", "Owca", "Krolik")) {
    if (priceToPay != 0) {
      if (animalsOverflowPrice[animal] >= priceToPay) {
        if (priceToPay < Price[animal]) {
          herd[animal] <- herd[animal] - 1
          herd[animalToBuy] <-
            herd[animalToBuy] + (Price[animal] / Price[animalToBuy])
          priceToPay <- 0
        } else {
          herd[animal] <- herd[animal] - priceToPay / Price[animal]
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

################### Rozgrywka ###################

RollDice <- function(herd) {
  greenDiceResult <- RollGreenDice()
  redDiceResult <- RollRedDice()
  if (greenDiceResult == redDiceResult) {
    herd <- UpdateHerd(greenDiceResult, 2, herd)
  } else {
    herd <- UpdateHerd(greenDiceResult, 1, herd)
    herd <- UpdateHerd(redDiceResult, 1, herd)
  }
  return(herd)
}

# Rzuty kostką
RollGreenDice <- function() {
  sample(
    c(
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Owca",
      "Owca",
      "Owca",
      "Swinia",
      "Krowa",
      "Wilk"
    ),
    1
  )
}

RollRedDice <- function() {
  sample(
    c(
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Krolik",
      "Owca",
      "Owca",
      "Swinia",
      "Swinia",
      "Kon",
      "Lis"
    ),
    1
  )
}

# Zaktualizowanie ilości zwierząt w stadzie
UpdateHerd <- function(animal, quantity, herd) {
  # Ograniczenia na liczbę zwierząt
  limits <- c(60, 24, 20, 12, 6, 4, 2)
  names(limits) <-
    c("Krolik",
      "Owca",
      "Swinia",
      "Krowa",
      "Kon",
      "MalyPies",
      "DuzyPies")
  if (animal == "Wilk") {
    herd <- EatAnimalsByWolf(herd)
  } else if (animal == "Lis") {
    herd <- EatAnimalsByFox(herd)
  } else {
    herd[animal] <-
      min(floor((herd[animal] + quantity) / 2) + herd[animal], limits[animal])
  }
  return(herd)
}

EatAnimalsByWolf <- function(herd) {
  if (herd["DuzyPies"] > 0) {
    herd["DuzyPies"] <- herd["DuzyPies"] - 1
  } else {
    for (animal in c("Krolik", "Owca", "Swinia", "Krowa")) {
      herd[animal] <- 0
    }
  }
  return(herd)
}

EatAnimalsByFox <- function(herd) {
  if (herd["MalyPies"] > 0) {
    herd["MalyPies"] <- herd["MalyPies"] - 1
  } else {
    herd["Krolik"] <- 0
  }
  return(herd)
}

# Zakończenie gry
EndGame <- function(herd) {
  all(herd >= c(1, 1, 1, 1, 1, 0, 0))
}

################### Gra ###################

# Licznik
Counter <- numeric(10000)

for (j in 1 : 10000) {
  # Stan początkowy gry
  herd <- c(0, 0, 0, 0, 0, 0, 0)
  names(herd) <-
    c("Krolik",
      "Owca",
      "Swinia",
      "Krowa",
      "Kon",
      "MalyPies",
      "DuzyPies")
  # Gra
  while (!EndGame(herd)) {
    herd <- DoExchange(herd)
    if (EndGame(herd)) {
      break
    }
    herd <- RollDice(herd)
    Counter[j] <- Counter[j] + 1
  }
}