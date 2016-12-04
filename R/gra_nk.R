#' @title Wykonanie raz gry w SuperFarmera
#'
#' @description
#' Funkcja gra przeprowadza gre w Super Farmera jeden raz przy zadanej
#' jako parametr strategii.  Zlicza ilosc rzutow kostkami potrzebnych,
#' aby wygrac. Jest jeden gracz.
#'
#' @param strategia Funckja implementujaca strategie, która przyjmuje gracz podczas gry.
#'
#' @return Liczba rzutow kostkami wykonanych do wygranej.
#'
#' @export

gra_nk <- function(strategia) {
  herd <- c(0, 0, 0, 0, 0, 0, 0)
  names(herd) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  counter <- 0
  # Gra
  while (!EndGame(herd)) {
    herd <- strategia(herd)
    if (EndGame(herd)) {
      break
    }
    herd <- RollDice(herd)
    counter <- counter + 1
  }
  return(counter)
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