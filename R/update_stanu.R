update_stanu <- function(stan_gracza, stan_stada, wartosci_kostki) {
  if (wartosci_kostki['zielona'] == wartosci_kostki['czerwona']) {
    return (update_stanu_te_same_wartosci_na_kostkach(stan_gracza, stan_stada,
                                                      wartosci_kostki['zielona']))
  } else {
    return (update_stanu_rozne_wartosci_na_kostkach(stan_gracza, stan_stada,
                                                    wartosci_kostki))
  }
}
