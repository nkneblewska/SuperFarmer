Wymiana <- function(Stado2, Suma_nadwyzki, Wartosc_nadwyzki, Nadwyzka, pies1, pies2){
  if (all(Stado2[c("Krolik", "Owca", "Swinia", "Krowa")] == c(0, 0, 0, 0)) && Stado2["MalyPies"] > 0) {
    Stado2 <- MPies_na_Kroliki(Stado2)
  } else if ( Stado["Krolik"] >= pies1 && Stado["MalyPies"] < 1 ) {
    Stado2 <- Kroliki_na_MPsa(Stado2)
  } else if ( wystarczy_na_nowe_zwierze(Stado2,Suma_nadwyzki) ) {
    Stado2 <- uzupelnij_zwierzeta(Stado2,Suma_nadwyzki,Wartosc_nadwyzki,Nadwyzka)
  } else if ( Suma_nadwyzki >= Cena1["Krowa"] && Stado2["Krowa"] < 2 ) {
    Stado2 <- Kup("Krowa",Stado2,Wartosc_nadwyzki,Nadwyzka)
  } else if ( Stado["Krolik"] >= pies2 && Stado["MalyPies"] < 2 ){
    Stado2 <- Kroliki_na_MPsa(Stado2)
  }
  return(Stado2)
}
