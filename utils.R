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
squadra <- function(nome,data) {
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
    if (season_1819_csv$HomeTeam[i] == nome) {
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


