update_stanu_rozne_wartosci_na_kostkach <-
  function(stan_gracza, stan_stada, wartosci_kostki) {
    wynik_pierwszego_updatu <-
      update_stanu_kostka_zielona(stan_gracza, stan_stada, wartosci_kostki['zielona'])
    return (update_stanu_kostka_czerowna(wynik_pierwszego_updatu[,1],
                                         wynik_pierwszego_updatu[,2],
                                         wartosci_kostki['czerwona']))
}
