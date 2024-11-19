# FUNCTIONS USED

# Function to plot the total goals distribution
plot_total_goals_distribution <- function(data) {
  barplot(
    rbind(dpois(0:6, 1.340834), c(prop.table(table(c(data$FTHG, data$FTAG))))),
    col = c('turquoise4', 'gold'),
    ylim = c(0, 0.4),
    xlab = "Goals",
    main = "Total goals distribution",
    beside = TRUE,
    border = FALSE,
    ylab = "Frequencies"
  )
  legend("topright", fill = c('turquoise4', 'gold'), box.col = "white",
         legend = expression("Theoretical distribution from a Poisson with " ~ theta == 1.34, "Real data"))
}

# Function to plot Poisson distributions
plot_poisson_distribution <- function(lambdas) {
  par(mfrow = c(2, 2))
  set.seed(123)
  for (lambda in lambdas) {
    barplot(
      prop.table(table(rpois(380, lambda))),
      main = paste0("Poisson distribution for ", expression(theta), "=", lambda),
      col = 'orange',
      border = FALSE
    )
  }
  par(mfrow = c(1, 1))
}

# Function to plot home and away goals
plot_home_away_goals <- function(data) {
  par(mfrow = c(1, 2))
  set.seed(1234)
  
  # Home goals scored
  barplot(
    rbind(prop.table(table(data$FTHG)), dpois(0:6, 1.484)),
    col = c('#f79256', 'lightblue'),
    ylim = c(0, 0.4),
    main = 'Home goals scored',
    ylab = "Frequencies",
    xlab = expression(x[i]),
    beside = TRUE,
    border = FALSE
  )
  legend("topright", fill = c('#f79256', 'lightblue'), box.col = "white",
         legend = expression("Real data", "Poisson with " ~ theta == 1.484))
  
  # Away goals scored
  barplot(
    rbind(prop.table(table(data$FTAG)), dpois(0:6, 1.197)),
    col = c('#f79256', 'lightblue'),
    ylim = c(0, 0.4),
    main = 'Away goals scored',
    ylab = "Frequencies",
    xlab = expression(y[i]),
    beside = TRUE,
    border = FALSE
  )
  legend("topright", fill = c('#f79256', 'lightblue'), box.col = "white",
         legend = expression("Real data", "Poisson with " ~ theta == 1.197))
  
  par(mfrow = c(1, 1))
}

# Function to filter goals by team
squadra <- function(nome, data) {
  goals <- numeric(38)
  conceded <- numeric(38)
  countg <- 0
  countc <- 0
  for (i in 1:380) {
    if (data$AwayTeam[i] == nome) {
      countg <- countg + 1
      countc <- countc + 1
      goals[countg] <- data$FTAG[i]
      conceded[countc] <- data$FTHG[i]
    }
    if (data$HomeTeam[i] == nome) {
      countg <- countg + 1
      countc <- countc + 1
      goals[countg] <- data$FTHG[i]
      conceded[countc] <- data$FTAG[i]
    }
  }
  return(list(Goals = goals, Conceded = conceded))
}

# Bivariate Poisson density function
bivpois <- function(lambda1, lambda2, lambda3, x, y) {
  value <- 0
  for (k in 1:min(x, y)) {
    value <- value + (choose(x, k) * choose(y, k) * factorial(k) * (lambda3 / (lambda1 * lambda2))^k)
  }
  density1 <- exp(-(lambda1 + lambda2 + lambda3)) * (lambda1^x / factorial(x)) * (lambda2^y / factorial(y)) * value
  return(density1)
}

# Generate random values from bivariate Poisson
rbivpois <- function(n, lambda1, lambda2, lambda3) {
  match_result <- numeric(n)
  frama <- sample_n(
    tbl = risultati_frame,
    weight = bivpois(lambda1, lambda2, lambda3, x = risultati_frame[, 1], y = risultati_frame[, 2]),
    size = 500,
    replace = TRUE
  )
  for (i in 1:n) {
    match_result[i] <- paste(frama[i, 1], '-', frama[i, 2])
  }
  return(list(Match_Result = match_result, Matrix = frama))
}

# Goals scored and conceded by a specific team
squadra_stats <- function(nome, dati) {
  # Filter matches played by the team as home or away
  home_matches <- dati[dati$HomeTeam == nome, ]
  away_matches <- dati[dati$AwayTeam == nome, ]
  
  # Calculate goals scored and conceded
  goals_scored <- sum(home_matches$FTHG, away_matches$FTAG, na.rm = TRUE)
  goals_conceded <- sum(home_matches$FTAG, away_matches$FTHG, na.rm = TRUE)
  
  # Return results
  return(list(
    Team = nome,
    Scored = goals_scored,
    Conceded = goals_conceded
  ))
}

# Function for computing ALL goals (scored and conceded)
compute_goal <- function(nome_squadra) {
  dati_squadra <- squadra(nome_squadra, data)
  list(Scored = dati_squadra$Goals, Conceded = dati_squadra$Conceded)
}

compute_estimated_goal <- function(nome_squadra) {
  dati_squadra <- squadra(nome_squadra, dati_mod_1)
  list(Scored = dati_squadra$Goals, Conceded = dati_squadra$Conceded)
}

# Function to update team rankings based on match results
update_classification <- function(data) {
  # Initialize the classification table with 0 points for each team
  classifica_real <- setNames(rep(0, length(unique(c(data$HomeTeam, data$AwayTeam)))), 
                              unique(c(data$HomeTeam, data$AwayTeam)))
  
  # Loop through the matches and update the points in the classification table
  for (i in 1:nrow(data)) {
    home_t <- data$HomeTeam[i]
    away_t <- data$AwayTeam[i]
    
    # Compare the scores and update points accordingly
    home_score <- round(data$FTHG[i])
    away_score <- round(data$FTAG[i])
    
    if (home_score > away_score) {
      classifica_real[home_t] <- classifica_real[home_t] + 3
    } else if (home_score < away_score) {
      classifica_real[away_t] <- classifica_real[away_t] + 3
    } else {
      classifica_real[home_t] <- classifica_real[home_t] + 1
      classifica_real[away_t] <- classifica_real[away_t] + 1
    }
  }
  # Create a matrix to store the classification points in the correct order
  clas_mod_real <- matrix(classifica_real[order(names(classifica_real))], nrow = 1)
  colnames(clas_mod_real) <- names(classifica_real)
  return(clas_mod_real)
}


# Function to update team rankings based on match results
update_classification_estim <- function(data,theta1, theta2) {
  # Initialize the classification table with 0 points for each team
  classifica_real <- setNames(rep(0, length(unique(c(data$HomeTeam, data$AwayTeam)))), 
                              unique(c(data$HomeTeam, data$AwayTeam)))
  
  # Loop through the matches and update the points in the classification table
  for (i in 1:nrow(data)) {
    home_t <- data$HomeTeam[i]
    away_t <- data$AwayTeam[i]
    
    # Compare the scores and update points accordingly
    home_score <- round(theta1[i])
    away_score <- round(theta2[i])
    
    if (home_score > away_score) {
      classifica_real[home_t] <- classifica_real[home_t] + 3
    } else if (home_score < away_score) {
      classifica_real[away_t] <- classifica_real[away_t] + 3
    } else {
      classifica_real[home_t] <- classifica_real[home_t] + 1
      classifica_real[away_t] <- classifica_real[away_t] + 1
    }
  }
  # Create a matrix to store the classification points in the correct order
  clas_mod_real <- matrix(classifica_real[order(names(classifica_real))], nrow = 1)
  colnames(clas_mod_real) <- names(classifica_real)
  return(clas_mod_real)
}


# Function to update classification based on Bivariate Poisson results
update_classification_biv <- function(data, predictions) {
  # Initialize classification table
  classification <- setNames(rep(0, length(unique(c(data$HomeTeam, data$AwayTeam)))), 
                             unique(c(data$HomeTeam, data$AwayTeam)))
  
  # Loop through matches and update points
  for (i in 1:nrow(data)) {
    home_team <- data$HomeTeam[i]
    away_team <- data$AwayTeam[i]
    
    home_score <- round(predictions$median$ynew[i, 1])
    away_score <- round(predictions$median$ynew[i, 2])
    
    if (home_score > away_score) {
      classification[home_team] <- classification[home_team] + 3
    } else if (home_score < away_score) {
      classification[away_team] <- classification[away_team] + 3
    } else {
      classification[home_team] <- classification[home_team] + 1
      classification[away_team] <- classification[away_team] + 1
    }
  }
  
  return(classification)
}

# Function to plot team performance
plot_team_performance <- function(score_data, concede_data, team_name) {
  par(mfrow = c(1, 1))
  
  score_table <- table(paste(unlist(score_data[[team_name]]), "-", unlist(concede_data[[team_name]])))
  plot(score_table, ylab = "Number of matches with a final score", 
       main = team_name, las = 2)
  points(2, 5, col = 'red', type = 'h', lwd = 3)
  box()
}

# Function to compare Bivariate Poisson predictions with real results
compare_predictions <- function(biv_classification, real_classification, team_names) {
  plot(1:20, biv_classification, type = 'h', ylim = c(0, 90), xaxt = "n", 
       xlab = "", ylab = "Points", main = "Bivariate vs Real Points")
  points((1:20) + 0.2, real_classification, type = 'h', col = 'red')
  axis(1, at = 1:20, labels = team_names, las = 2)
  legend("topright", lwd = 2, col = c('black', 'red'), legend = c("Bivariate", "Real"))
}


