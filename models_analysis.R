##### Packages #####
source('utils.R')
library(forecast)
library(ggplot2)
library(dplyr)
library(R2jags)
library(tidyverse)
library(ggridges)
library(viridis)

##### Explanatory Data Analysis #####

# Import the data
dati <- read.csv('season-1819_csv.csv')
data <- dati[3:6]

# Plot home and away goals
plot_home_away_goals(data)

# Plot the total goals distribution
plot_total_goals_distribution(data)

# Retrieve the names of the teams
teams <- names(table(data$HomeTeam))

# Use lapply to calculate goals scored and conceded for each team
results <- lapply(teams, compute_goal)

# Extract goals scored and conceded into separate lists
scored <- lapply(results, `[[`, "Scored")
conceded <- lapply(results, `[[`, "Conceded")

# Assign names to the lists
names(scored) <- teams
names(conceded) <- teams


# Cumulative goals scored
plot(cumsum(scored[[1]]),type ='l', col=i, lwd=4, ylim = c(-5,80), ylab = "Cumulative goals Scored", xlab = "Matchday")
for (i in 2:20) {
  points(cumsum(scored[[i]]),type ='l', col=i, lwd=4, add = T)
}


# Away wins
plot(table(dati$AwayTeam[dati$FTR=="A"]), las =2, main = "Away wins")
box()


# Difference between distribution of home and away goals

# Determine the common levels
all_levels <- sort(unique(c(dati$FTHG, dati$FTAG)))

# Create tables with consistent levels
fthg_table <- prop.table(table(factor(dati$FTHG, levels = all_levels)))
ftag_table <- prop.table(table(factor(dati$FTAG, levels = all_levels)))

# Create the barplot without warnings
barplot(rbind(fthg_table, ftag_table), beside = TRUE, prob = TRUE, 
        ylim = c(0, 0.4), col = c('yellow', 'blue'), border = FALSE)
box()
legend("topright", c("Home goals", "Away goals"), fill = c("yellow","blue"))


# Distribution of results
plot(sort(prop.table(table(paste(dati$FTHG, "-", dati$FTAG)))), las =2, ylab = 'Relative frequencies', main = "Distribution of results")
box()


# Insert a variable related to averga number of fans in the stadium for each team
# Define rank mapping based on teams
rank_mapping <- list(
  rank_3 = c("Inter", "Milan", "Juventus", "Roma", "Lazio", "Fiorentina"),
  rank_2 = c("Napoli", "Genoa", "Torino", "Bologna", "Udinese", "Sampdoria"),
  rank_1 = c("Atalanta", "Parma", "Cagliari", "Spal", "Chievo", "Frosinone", "Sassuolo", "Empoli")
)

# Initialize the 'rank' column
data$rank <- NA

# Assign ranks based on the mapping
for (rank in names(rank_mapping)) {
  data$rank[data$HomeTeam %in% rank_mapping[[rank]]] <- as.numeric(sub("rank_", "", rank))
}


##### Jags Double Poisson #####

# Parameters inizialization

# Creazione del vettore per att e def
att = rep(0.01, 20)  # un vettore con 20 valori (uno per ogni squadra)
def = rep(0.01, 20)  # un altro vettore con 20 valori (uno per ogni squadra)

# Assegna i nomi alle squadre (se necessario)
squadre <- names(table(data$HomeTeam))
names(att) = squadre
names(def) = squadre

# Inizializza la lista degli iniziali per il modello JAGS
inits2 <- list(att.star = att, def.star = def)

# Parameters to monitor
params2=c("att","def","theta","ynew","home")

dat = list("HomeTeam"=as.numeric(as.factor(data$HomeTeam)),"AwayTeam"=as.numeric(as.factor(data$AwayTeam)), "rank"=data$rank, "FTHG" = data$FTHG, "FTAG"=data$FTAG)

model_foot_1.jags=jags(data=dat,inits=list(inits2),
                       parameters.to.save=params2,
                       model.file="double_poisson_jags.txt",
                       DIC=TRUE,n.chains=1,n.iter=10000,n.burnin=1000,n.thin=1)


plot(1:20, model_foot_1.jags$BUGSoutput$mean$att, type = 'h', xaxt = "n",xlab = "",ylab = "Attack parameter")
abline(h=0)
axis(1,names(table(dati$HomeTeam)), at=1:20, las =2 )

dati_mod_1 = data.frame(cbind(dati$HomeTeam,dati$AwayTeam,round(model_foot_1.jags$BUGSoutput$median$ynew)))
colnames(dati_mod_1) = c("HomeTeam","AwayTeam","FTHG","FTAG")

results_estimated <- lapply(teams, compute_estimated_goal)

# Extract goals scored and conceded into separate lists
scored_estimated <- lapply(results_estimated, `[[`, "Scored")
conceded_estimated <- lapply(results_estimated, `[[`, "Conceded")

# Assign names to the lists
names(scored_estimated) <- teams
names(conceded_estimated) <- teams

plot(cumsum(scored$Atalanta), type = 'l', lwd=4, col='darkblue', ylab ="Goals", xlab = "", axes = F)
points(cumsum(scored$Parma),type ='l', col='yellow', lwd=4)
points(cumsum(scored$Fiorentina),type ='l', col='purple', lwd=4)
legend("topleft", c("Atalanta", "Parma", "Fiorentina"),lwd=4, col=c('darkblue','yellow','purple'), box.col = "white")

points(cumsum(scored_estimated$Atalanta),type ='l', col='darkblue', lwd=4, lty = 2)
points(cumsum(scored_estimated$Parma),type ='l', col='yellow', lwd=4, lty = 2)
points(cumsum(scored_estimated$Fiorentina),type ='l', col='purple', lwd=4, lty = 2)
axis(1, at =1:38, labels = 1:38, las = 2)
axis(2, at=seq(0,80, by =5), labels = seq(0,80, by =5), las =2)
box()


# Estimated effect jags
plot(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect")
text(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.8)
abline(h=0)
abline(v=0)
abline(a=0,b=0.5, lty =2)

# Check means
plot(model_foot_1.jags$BUGSoutput$sims.array[,,1], type = 'l')
abline(h=model_foot_1.jags$BUGSoutput$mean$att[1], col ='red',lwd=3)


# Initialize the classification table with 0 points for each team
classifica_doub_pois <- setNames(rep(0, 20), names(table(dati$HomeTeam)))

# Get the model predictions once for efficiency
theta_home <- model_foot_1.jags$BUGSoutput$mean$theta[,1]
theta_away <- model_foot_1.jags$BUGSoutput$mean$theta[,2]

# Loop through the matches and update the points in the classification table
for (i in 1:380) {
  home_t <- dati$HomeTeam[i]
  away_t <- dati$AwayTeam[i]
  
  # Compare the predicted scores and update points accordingly
  home_score <- round(theta_home[i])
  away_score <- round(theta_away[i])
  
  if (home_score > away_score) {
    classifica_doub_pois[home_t] <- classifica_doub_pois[home_t] + 3
  } else if (home_score < away_score) {
    classifica_doub_pois[away_t] <- classifica_doub_pois[away_t] + 3
  } else {
    classifica_doub_pois[home_t] <- classifica_doub_pois[home_t] + 1
    classifica_doub_pois[away_t] <- classifica_doub_pois[away_t] + 1
  }
}

# Create a matrix to store the classification points in the correct order
clas_mod_1 <- matrix(classifica_doub_pois[order(names(classifica_doub_pois))], nrow = 1)
colnames(clas_mod_1) <- names(classifica_doub_pois)

# Plot the sorted classification table
plot(sort(clas_mod_1), type = 'h', xaxt = "n", xlab = "Teams", ylab = "Points", main = "League Classification")
axis(1, labels = teams[order(clas_mod_1)], las = 2, at = 1:20)



##### Classifica Real ########

clas_mod_real = update_classification(dati)

# Plot the sorted classification table
plot(sort(clas_mod_real), type = 'h', xaxt = "n", xlab = "Teams", ylab = "Points", main = "League Classification")
axis(1, labels = colnames(clas_mod_1)[order(clas_mod_real)], las = 2, at = 1:20)


##### Jags Bivariate Poisson #####

# Creazione del vettore per att e def
att = rep(0.01, 20)  # un vettore con 20 valori (uno per ogni squadra)
def = rep(0.01, 20)  # un altro vettore con 20 valori (uno per ogni squadra)

# Assegna i nomi alle squadre (se necessario)
squadre <- names(table(data$HomeTeam))
names(att) = squadre
names(def) = squadre

# Inizializza la lista degli iniziali per il modello JAGS
inits2 <- list(att.star = att, def.star = def)

# Parameters to monitor
params2=c("att","def","theta","ynew","home")

dat = list("HomeTeam"=as.numeric(as.factor(data$HomeTeam)),"AwayTeam"=as.numeric(as.factor(data$AwayTeam)), "rank"=data$rank, "FTHG" = data$FTHG, "FTAG"=data$FTAG)

model_foot_2.jags=jags(data=dat,inits=list(inits2),
                       parameters.to.save=params2,
                       model.file="bivariate_poisson_jags.txt",
                       DIC=TRUE,n.chains=1,n.iter=10000,n.burnin=1000,n.thin=1)

dati_mod_2 = data.frame(cbind(dati$HomeTeam,dati$AwayTeam,round(model_foot_2.jags$BUGSoutput$mean$ynew)))
colnames(dati_mod_2) = c("HomeTeam","AwayTeam","FTHG","FTAG")


# Main workflow
# Update the Bivariate Poisson classification
classifica_BIVPm <- update_classification_biv(dati, model_foot_2.jags$BUGSoutput)

# Plot team performance (example for Juventus and Parma)
plot_team_performance(scored, conceded, "Juventus")
plot_team_performance(scored, conceded, "Parma")

# Compare the Bivariate classification with real results
compare_predictions(classifica_BIVPm, classifica_real, names(table(dati$HomeTeam)))

# Convergence
plot(1:9000,cumsum(model_foot_2.jags$BUGSoutput$sims.list$theta[,2,3])/(1:9000), type = 'l')


par(mfrow=c(2,2))
plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,1])/(1:9000), ylab = "", xlab = "Atalanta Attack", pch=16, col = c('black','blue'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,1])/(1:9000), ylab = "", xlab = "Atalanta Defense", pch=16, col = c('black','blue'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,7])/(1:9000), ylab = "", xlab = "Frosinone Attack", pch=16, col = c('gold','gold'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,7])/(1:9000), ylab = "", xlab = "Frosinone Defense", pch=16, col = c('gold','gold'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,15])/(1:9000), ylab = "", xlab = "Roma Attack", pch=16, col = c('gold','red2'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,15])/(1:9000), ylab = "", xlab = "Roma Defense", pch=16, col = c('gold','red2'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,15])/(1:9000), ylab = "", xlab = "Fiorentina Attack", pch=16, col = "purple3")

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,15])/(1:9000), ylab = "", xlab = "Fiorentina Defense", pch=16, col = "purple3")


##### Plot distribution of effects #####

plotfigo=matrix(NA, nrow = 9000*20,ncol=2)
dim(model_foot_1.jags$BUGSoutput$sims.array)

beta = c('beta1')
for (i in 0:19) {
  plotfigo[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
  plotfigo[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}

plotfigo1=matrix(NA, nrow = 9000*20,ncol=2)

for (i in 0:19) {
  plotfigo1[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
  plotfigo1[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}



par(mfrow=c(1,1))
p = ggplot(data.frame(plotfigo[1:90000,]), aes(x=as.numeric(plotfigo[1:90000,1]),y=plotfigo[1:90000,2],fill =after_stat(x))) + geom_density_ridges_gradient(scale=2) + xlab("Attack effect") + ylab("") + scale_fill_viridis(option='B')
p

q = ggplot(data.frame(plotfigo[90001:180000,]), aes(x=as.numeric(plotfigo[90001:180000,1]),y=plotfigo[90001:180000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Attack effect") + ylab("") + scale_fill_viridis(option='B')
q <- q + guides(fill=guide_legend(title="Teams"))
q


plotfigo=matrix(NA, nrow = 9000*20,ncol=2)
dim(model_foot_1.jags$BUGSoutput$sims.array)

beta = c('beta1')
for (i in 0:19) {
  plotfigo[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
  plotfigo[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}

plotfigo1=matrix(NA, nrow = 9000*20,ncol=2)

for (i in 0:19) {
  plotfigo1[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
  plotfigo1[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}

r = ggplot(data.frame(plotfigo1[1:90000,]), aes(x=as.numeric(plotfigo1[1:90000,1]),y=plotfigo1[1:90000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Defense effect") + ylab("") + scale_fill_viridis(option='B')

r

s = ggplot(data.frame(plotfigo1[90001:180000,]), aes(x=as.numeric(plotfigo1[90001:180000,1]),y=plotfigo1[90001:180000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Defense effect") + ylab("") + scale_fill_viridis(option='B')
s

##### Zero Inflated Poisson model #####

# Creazione del vettore per att e def
att = rep(0.01, 20)  # un vettore con 20 valori (uno per ogni squadra)
def = rep(0.01, 20)  # un altro vettore con 20 valori (uno per ogni squadra)

# Assegna i nomi alle squadre (se necessario)
squadre <- names(table(data$HomeTeam))
names(att) = squadre
names(def) = squadre

# Inizializza la lista degli iniziali per il modello JAGS
inits2 <- list(att.star = att, def.star = def)

params2_3=c("att","def","theta","home","beta_const","ynew","pi")

dat_2 = list("HomeTeam"=as.numeric(as.factor(dat$HomeTeam)),"AwayTeam"=as.numeric(as.factor(dat$AwayTeam)), "rank"=dati$rank, "FTHG" = dati$FTHG, "FTAG"=dati$FTAG)

model_foot_3.jags=jags(data=dat_2,inits=list(inits2),
                       parameters.to.save=params2_3,
                       model.file="zero_inflated_poisson_jags.txt",
                       DIC=TRUE,n.chains=1,n.iter=10000,n.burnin=1000,n.thin=1)


##### Comparison between models #####


plot(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect", xlim = c(-0.5,0.5), ylim =c(-.5,.5))
text(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='darkorange')

points(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect")

text(model_foot_2.jags$BUGSoutput$mean$att,-model_foot_2.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='purple')

text(model_foot_3.jags$BUGSoutput$mean$att,-model_foot_3.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='darkgreen')

legend("bottomright",legend = c("Double Poisson","Bivariate Poisson","Zero Inflated Bivariate Poisson"), col=c('darkorange','purple','darkgreen'), lwd =2, box.col = "white")

abline(h=0)
abline(v=0)
abline(a=0,b=.5,lty=2)
box()


