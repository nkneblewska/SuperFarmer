update_stanu_zwierze_lagodne <-
  function(stan_gracza, stan_stada, wartosc_kostki, ile_zwierzakow = 1) {
    liczba_naleznych_zwierzakow <- as.integer((stan_gracza[wartosc_kostki] +
                                                 ile_zwierzakow)/2)
    #print("update stanu, liczba naleznych zwierzakow")
    #print(liczba_naleznych_zwierzakow)
    return (zamien_zwierzaki(stan_gracza, stan_stada, wartosc_kostki,
                             liczba_naleznych_zwierzakow))
}
