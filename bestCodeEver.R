# Strategia

# 1. Przy ponad 10 krolikach kupujemy malego psa za kroliki.
# 2. Przy wymianach zawsze zostawiamy w stadzie odpowiednio: do 24 krolikow, 1 owcy, 1 swini, 1 krowy, 1 konia.
# 3. Wymieniamy nadwyzkowe zwierzeta na najdrozsze zwierze, ktorego brakuje.
# 4. Dokupujemy drugiego malego psa przy ponad 16 krolikach, jesli nie przeszkodzi to w powyzszych wymianach.

################### Wymiany ###################

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

# Rzuty kostka
RollGreenDice <- function() {
  sample(
    c(
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "owca",
      "owca",
      "owca",
      "swinia",
      "krowa",
      "wilk"
    ),
    1
  )
}

RollRedDice <- function() {
  sample(
    c(
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "krolik",
      "owca",
      "owca",
      "swinia",
      "swinia",
      "kon",
      "lis"
    ),
    1
  )
}

# Zaktualizowanie ilosci zwierzat w stadzie
UpdateHerd <- function(animal, quantity, herd) {
  # Ograniczenia na liczbe zwierzat
  limits <- c(60, 24, 20, 12, 6, 4, 2)
  names(limits) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  if (animal == "wilk") {
    herd <- EatAnimalsByWolf(herd)
  } else if (animal == "lis") {
    herd <- EatAnimalsByFox(herd)
  } else {
    herd[animal] <-
      min(floor((herd[animal] + quantity) / 2) + herd[animal], limits[animal])
  }
  return(herd)
}

EatAnimalsByWolf <- function(herd) {
  if (herd["duzy_pies"] > 0) {
    herd["duzy_pies"] <- herd["duzy_pies"] - 1
  } else {
    for (animal in c("krolik", "owca", "swinia", "krowa")) {
      herd[animal] <- 0
    }
  }
  return(herd)
}

EatAnimalsByFox <- function(herd) {
  if (herd["maly_pies"] > 0) {
    herd["maly_pies"] <- herd["maly_pies"] - 1
  } else {
    herd["krolik"] <- 0
  }
  return(herd)
}

# Zakonczenie gry
EndGame <- function(herd) {
  all(herd >= c(1, 1, 1, 1, 1, 0, 0))
}

################### Gra ###################

# Licznik
Counter <- numeric(10000)

for (j in 1:10000) {
  # Stan poczatkowy gry
  herd <- c(0, 0, 0, 0, 0, 0, 0)
  names(herd) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
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
  # Gra
  while (!EndGame(herd)) {
    herd <- DoExchange(herd, price)
    if (EndGame(herd)) {
      break
    }
    herd <- RollDice(herd)
    Counter[j] <- Counter[j] + 1
  }
}

# Wyniki, Przyklady otrzymanych wynikow:
head(Counter)

# Podstawowe statystyki:
summary(Counter)

# Wykresy (1/2)
library(ggplot2)
ggplot() + aes(Counter) + geom_histogram(aes(fill = ..count..), bins = 100) + labs(title =
                                                                                     "Histogram liczby rzutow") + labs(x = "Liczba rzutow", y = "Liczba wystapien") + scale_fill_gradient("Count", low = "black", high = "steelblue3")

# Wykresy (2/2)
library(ggplot2)
ggplot() + aes(Counter) + geom_density(fill = "steelblue3")  + labs(title =
                                                                      "Rozklad liczby rzutow") + labs(x = "Liczba rzutow", y = "Prawdopodobienstwo")