##### FUNZIONE FILTRAGGIO GOAL PER SQUADRA #####
squadra = function(nome){
  goals=numeric(38)
  conceded=numeric(38)
  countg=0
  countc=0
  for (i in 1:380) {
    if(season_1819_csv$AwayTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=season_1819_csv$FTAG[i]
      conceded[countc]=season_1819_csv$FTHG[i]
      
    }
    if(season_1819_csv$HomeTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=season_1819_csv$FTHG[i]
      conceded[countc]=season_1819_csv$FTAG[i]
    }
  }
  return(list(Fatti=goals,Concessi=conceded))
}

##### BIVARIATE POISSON DENSITY FUNCTION #####

bivpois=function(lambda1, lambda2, lambda3, x, y){
  valore=0
  for (k in 1:min(x,y)) {
    valore=valore + (choose(x,k)*choose(y,k)*factorial(k)*(lambda3/(lambda1*lambda2))^k)
  }
  density1=exp(-(lambda1+lambda2+lambda3))*(lambda1^x/factorial(x))*(lambda2^y/factorial(y))*valore
  return(density1)
}

##### GENERATE RANDOM VALUES FROM BIVARIATE POISSON #####

generate=function(n, lambda1, lambda2, lambda3){
  match_result=numeric(n)
  frama= sample_n(tbl = risultati_frame, weight = bivpois(lambda1,lambda2,lambda3, x=risultati_frame[,1], y =risultati_frame[,2]), size = 500, replace = T)
  for (i in 1:n) {
    match_result[i]=paste(frama[i,1], '-' , frama[i,2])
  }
  return(list(Match_Result=match_result , Matrice = frama))
}

##### GOAL FATTI E GOAL SUBITI DA UNA DETERMINATA SQUADRA #####

squadra_stats <- function(nome, dati) {
  # Filtra partite giocate dalla squadra come casa o trasferta
  partite_casa <- dati[dati$HomeTeam == nome, ]
  partite_trasferta <- dati[dati$AwayTeam == nome, ]
  
  # Calcola goal fatti e subiti
  goals_fatti <- sum(partite_casa$FTHG, partite_trasferta$FTAG, na.rm = TRUE)
  goals_concessi <- sum(partite_casa$FTAG, partite_trasferta$FTHG, na.rm = TRUE)
  
  # Restituisci i risultati
  return(list(
    Squadra = nome,
    Fatti = goals_fatti,
    Concessi = goals_concessi
  ))
}
