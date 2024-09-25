library(forecast)
library(ggplot2)
library(dplyr)


#### GGPLOT LINE GRAPH OF DESCRIPTIVE ANALYSIS #####
# autoplot of a ts object
autoplot(forecast(score$Frosinone[1:30]))
ggAcf(score$Inter)

ggplot(data = season_1819_csv, aes(x=X135_2018$round)) +
  geom_line(aes(y = score$Frosinone, colour = "TempMax")) +
  geom_line(aes(y = score$Inter, colour = "TempMedia")) +
  geom_line(aes(y = score$Atalanta, colour = "TempMin")) +
  scale_colour_manual("", 
                      breaks = c("TempMax", "TempMedia", "TempMin"),
                      values = c("red", "green", "blue")) +
  xlab(" ") +
  scale_y_continuous("Temperatura (C)", limits = c(-10,40)) + 
  labs(title="TITULO")


######### VARIABLES #####
# 1) DIV: XXXXXXXX
# 2) DATE: DATE OF THE MATCH
# 3) HOME TEAM
# 4) AWAY TEAM
# 5)

##### FUNZIONE GENERAZIONE GOAL PER SQUADRA ####
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


scored=numeric(380)
subito=c()
for (i in 1:20) {
  scored=append(scored, values=squadra(names(table(season_1819_csv$HomeTeam))[i])$Fatti)
  subito=append(subito, values=squadra(names(table(season_1819_csv$HomeTeam))[i])$Concessi)
}
scored=scored[-(1:380)]

score=list(Atalanta=scored[1:38], Bologna=scored[39:76], Cagliari=scored[77:114],Chievo=scored[115:152], Empoli=scored[153:190], Fiorentina=scored[191:228], Frosinone=scored[229:266], Genoa=scored[267:304], Inter=scored[305:342], Juventus=scored[343:380], Lazio=scored[381:418], Milan=scored[419:456], Napoli=scored[457:494], Parma=scored[495:532], Roma=scored[533:570], Sampdoria=scored[571:608], Sassuolo=scored[609:646], Spal=scored[647:684], Torino=scored[685:722], Udinese=scored[723:760])

subire=list(Atalanta=subito[1:38], Bologna=subito[39:76], Cagliari=subito[77:114],Chievo=subito[115:152], Empoli=subito[153:190], Fiorentina=subito[191:228], Frosinone=subito[229:266], Genoa=subito[267:304], Inter=subito[305:342], Juventus=subito[343:380], Lazio=subito[381:418], Milan=subito[419:456], Napoli=subito[457:494], Parma=subito[495:532], Roma=subito[533:570], Sampdoria=subito[571:608], Sassuolo=subito[609:646], Spal=subito[647:684], Torino=subito[685:722], Udinese=subito[723:760])

risultati=matrix(NA, nrow=31, ncol = 2)
risultati_frame=data.frame(risultati)
risultati_frame[35,1]=4
colnames(risultati_frame)=c("Home goals", "Away goals")


##### DESCRIPTIVE GRAPHIC #####

plot(cumsum(score$Atalanta), type = 'l', lwd=4)

for (i in 1:20) {
  plot(cumsum(score[[i]]),type ='l', col=i, lwd=4)
}
pairs(lapply(score, cumsum))

par(mfrow=c(1,1))

score[2]

plot(cumsum(score$Atalanta), type = 'l', lwd=4, col='darkblue', ylab ="Goals", xlab = "", axes = F)
points(cumsum(score$Parma),type ='l', col='yellow', lwd=4)
points(cumsum(score$Fiorentina),type ='l', col='purple', lwd=4)
legend("topleft", c("Atalanta", "Parma", "Fiorentina"),lwd=4, col=c('darkblue','yellow','purple'), box.col = "white")

points(cumsum(score2$Atalanta),type ='l', col='darkblue', lwd=4, lty = 2)
points(cumsum(score2$Parma),type ='l', col='yellow', lwd=4, lty = 2)
points(cumsum(score2$Fiorentina),type ='l', col='purple', lwd=4, lty = 2)
axis(1, at =1:38, labels = 1:38, las = 2)
axis(2, at=seq(0,80, by =5), labels = seq(0,80, by =5), las =2)
box()

for (i in 2:20) {
  points(1:38,squadra(names(table(season_1819_csv$HomeTeam))[i])$Fatti, col=i, type = 'l')
}
length(score$Bologna)
cbind(1:38, score$Bologna)

names(table(season_1819_csv$HomeTeam))

squadra('Inter')

names(table(season_1819_csv$HomeTeam))

plot(table(season_1819_csv$AwayTeam[season_1819_csv$FTR=="A"]), las =2)
box()

#GOALS
barplot(rbind(prop.table(table(season_1819_csv$FTHG)),prop.table(table(season_1819_csv$FTAG))), beside = T, prob = T, ylim=c(0, 0.4), col=c('yellow', 'blue'), border = F)
box()
legend("topright", c("Home goals", "Away goals"), fill = c("yellow","blue"))



prop.table(table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG)))

plot(table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG)), las=2)
plot(sort(prop.table(table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG)))), las =2)
box()

substr(names(table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG)))[1], 1, 3)
names(table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG)))



##### DENSITY OF BIVARIATE POISSON ####

bivpois=function(lambda1, lambda2, lambda3, x, y){
  valore=0
  for (k in 1:min(x,y)) {
    valore=valore + (choose(x,k)*choose(y,k)*factorial(k)*(lambda3/(lambda1*lambda2))^k)
  }
  density1=exp(-(lambda1+lambda2+lambda3))*(lambda1^x/factorial(x))*(lambda2^y/factorial(y))*valore
  return(density1)
}   

# TIDYVERSE
sample_n(tbl = risultati_frame, prob = bivpois(1,2,3, x=risultati[,1], y=risultati[,2]), size = 500, replace = T)

sum(bivpois(1,2,3, x=risultati[,1], y=risultati[,2])/sum(bivpois(1,2,3, x=risultati[,1], y=risultati[,2])))



# ### GENERATE SAMPLE OF RESULTS ####

# probabilities
sum(bivpois(1,2,0.4, x=risultati_frame[,1], y=risultati_frame[,2]))

generate=function(n, lambda1, lambda2, lambda3){
  match_result=numeric(n)
  frama= sample_n(tbl = risultati_frame, weight = bivpois(lambda1,lambda2,lambda3, x=risultati_frame[,1], y =risultati_frame[,2]), size = 500, replace = T)
  for (i in 1:n) {
    match_result[i]=paste(frama[i,1], '-' , frama[i,2])
  }
  return(list(Match_Result=match_result , Matrice = frama))
}

generate

plot(sort(table(generate(500, 1.2,0.4,0.5)$Match_Result)), type='h', las=2)
box()

table(paste(season_1819_csv$FTHG, "-", season_1819_csv$FTAG))

str(sample_n(tbl = risultati_frame, prob = bivpois(1,2,3, x=risultati[,1], y=risultati[,2]), size = 500, replace = T))


season_1819_csv$rank=NA
season_1819_csv$rank[season_1819_csv$HomeTeam=="Inter" | season_1819_csv$HomeTeam=="Milan" | season_1819_csv$HomeTeam=="Juventus" | season_1819_csv$HomeTeam=="Roma" | season_1819_csv$HomeTeam=="Lazio" | season_1819_csv$HomeTeam=="Fiorentina"]=3 #average spectators in home stadium in entire season > 30000

season_1819_csv$rank[season_1819_csv$HomeTeam=="Napoli" | season_1819_csv$HomeTeam=="Genoa" | season_1819_csv$HomeTeam=="Torino" | season_1819_csv$HomeTeam=="Bologna" | season_1819_csv$HomeTeam=="Udinese" | season_1819_csv$HomeTeam=="Sampdoria"]=2 #average spectators in home stadiumd in entire season > 20000 and < 30000

season_1819_csv$rank[season_1819_csv$HomeTeam=="Atalanta" | season_1819_csv$HomeTeam=="Parma" | season_1819_csv$HomeTeam=="Cagliari" | season_1819_csv$HomeTeam=="Spal" | season_1819_csv$HomeTeam=="Chievo" | season_1819_csv$HomeTeam=="Frosinone" | season_1819_csv$HomeTeam=="Sassuolo" | season_1819_csv$HomeTeam=="Empoli"]=1

season_1819_csv$rank


prod(dpois(score$Atalanta, lambda = 2))

#### MCMC JAGS FOR DATA ATT E DEF ####

mean(score$Atalanta)
mean(subire$Atalanta)

mean(score$Frosinone)
mean(subire$Frosinone)

mean(score$Parma)
mean(subire$Parma)


#### POISSON MODELS ####

poisson_l1=glm(log(season_1819_csv$HomeTeam) ~ home + att_h + def_a, family = poisson(link = "log"))

poisson_l2=glm(log(season_1819_csv$AwayTeam) ~ att_a + def_h, family = poisson(link = "log"))

poisson_l3=glm(log(season_1819_csv$HomeTeam) ~ att_h + def_a, family = poisson(link = "log"))

dati=season_1819_csv[,c(3,4,5,6,62)]
colnames(dati)= c("HomeTeam","AwayTeam","FTHG","FTAG","rank")


###### JAGS DOUBLE POISSON MODEL (WITHOUT THETA3) ######
cat("model {
  # LIKELIHOOD AND RANDOM EFFECT MODEL FOR THE SCORING PROPENSITY
  for (g in 1:380) {
    # Observed number of goals scored by each team
    FTHG[g] ~ dpois(theta[g,1])
    FTAG[g] ~ dpois(theta[g,2])
    # Predictive distribution for the number of goals scored
    ynew[g,1] ~ dpois(theta[g,1])
    ynew[g,2] ~ dpois(theta[g,2])
    # Average Scoring intensities (accounting for mixing components)
    log(theta[g,1]) <- home + att[HomeTeam[g]] + def[AwayTeam[g]]
    log(theta[g,2]) <- att[AwayTeam[g]] + def[HomeTeam[g]]
  }
  # 1. BASIC MODEL FOR THE HYPERPARAMETERS
  # prior on the home effect
  home ~ dnorm(0,0.0001)
  # ‘‘sum-to-zero’’ CONDITION
  for (t in 1:20){
    att.star[t] ~ dnorm(mu.att,tau.att)
    def.star[t] ~ dnorm(mu.def,tau.def)
    att[t] <- att.star[t] - mean(att.star[])
    def[t] <- def.star[t] - mean(def.star[])
  }
  # priors on the random effects
  mu.att ~ dnorm(0,0.0001)
  mu.def ~ dnorm(0,0.0001)
  tau.att ~ dgamma(.01,.01)
  tau.def ~ dgamma(.01,.01)
}", file="project_model_1_jags.txt",fill=TRUE)

inits2=(list(att.star=att,def.star=def))
#,theta=matrix(rep(0.001,760),nrow = 380, ncol=2))
inits2
att=rep(0.01,20)
colnames(att) = names(table(dati$HomeTeam))
att=data.frame(att)

def=rep(0.01,20)
colnames(def) = names(table(dati$HomeTeam))
def=data.frame(def)

model_foot_1.jags$BUGSoutput$DIC

# Parameters to monitor

params2=c("att","def","theta","ynew","home")

dat = list("HomeTeam"=as.numeric(as.factor(dat$HomeTeam)),"AwayTeam"=as.numeric(as.factor(dat$AwayTeam)), "rank"=dati$rank, "FTHG" = dati$FTHG, "FTAG"=dati$FTAG)
print(model_foot_1.jags)
library(R2jags)
model_foot_1.jags=jags(data=SERIEA_20223,inits=list(inits2),
                             parameters.to.save=params2,
                             model.file="project_model_1_jags.txt",
                             DIC=TRUE,n.chains=1,n.iter=10000,n.burnin=1000,n.thin=1)
print(model_foot_1.jags)
print(model_foot_2.jags)
print(model_foot_3.jags)




plot(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect", xlim = c(-0.5,0.5), ylim =c(-.5,.5))
text(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='darkorange')

points(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect")

text(model_foot_2.jags$BUGSoutput$mean$att,-model_foot_2.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='purple')

text(model_foot_3.jags$BUGSoutput$mean$att,-model_foot_3.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5, col ='darkgreen')

legend("bottomright",legend = c("Double Poisson","Bivariate Poisson","Zero Inflated Bivariate Poisson"), col=c('darkorange','purple','darkgreen'), lwd =2, box.col = "white")
box()

abline(h=0)
abline(v=0)
abline(a=0,b=.5,lty=2)


#### ANALYSIS DOUBLE POISSON (WITHOUT THETA3) ######
model_foot_1.jags$BUGSoutput$mean$theta
sum(model_foot_1.jags$BUGSoutput$mean$att)
sum(model_foot_1.jags$BUGSoutput$mean$def)
model_foot_1.jags$BUGSoutput$mean$

model_foot_1.jags$BUGSoutput$mean$ynew
model_foot_1.jags$BUGSoutput$mean$theta

plot(1:20, model_foot_1.jags$BUGSoutput$mean$att, type = 'h', xaxt = "n",xlab = "",ylab = "Attack parameter")
abline(h=0)
axis(1,names(table(dati$HomeTeam)), at=1:20, las =2 )

dati_mod_1 = data.frame(cbind(dati$HomeTeam,dati$AwayTeam,round(model_foot_1.jags$BUGSoutput$median$ynew)))
colnames(dati_mod_1) = c("HomeTeam","AwayTeam","FTHG","FTAG")

squadra_1 = function(nome){
  goals=numeric(38)
  conceded=numeric(38)
  countg=0
  countc=0
  for (i in 1:380) {
    if(dati_mod_1$AwayTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=dati_mod_1$FTAG[i]
      conceded[countc]=dati_mod_1$FTHG[i]
      
    }
    if(dati_mod_2$HomeTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=dati_mod_1$FTHG[i]
      conceded[countc]=dati_mod_1$FTAG[i]
    }
  }
  return(list(Fatti=goals,Concessi=conceded))
}


scored_mod1=numeric(380)
subito_mod1=c()
for (i in 1:20) {
  scored_mod1=append(scored_mod1, values=squadra_1(names(table(dati_mod_1$HomeTeam))[i])$Fatti)
  subito_mod1=append(subito_mod1, values=squadra_1(names(table(dati_mod_1$HomeTeam))[i])$Concessi)
}
scored_1=scored_mod1[-(1:380)]

score1=list(Atalanta=as.numeric(scored_1[1:38]), Bologna=as.numeric(scored_1[39:76]), Cagliari=as.numeric(scored_1[77:114]),Chievo=as.numeric(scored_1[115:152]), Empoli=as.numeric(scored_1[153:190]), Fiorentina=as.numeric(scored_1[191:228]), Frosinone=as.numeric(scored_1[229:266]), Genoa=as.numeric(scored_1[267:304]), Inter=as.numeric(scored_1[305:342]), Juventus=as.numeric(scored_1[343:380]), Lazio=as.numeric(scored_1[381:418]), Milan=as.numeric(scored_1[419:456]), Napoli=as.numeric(scored_1[457:494]), Parma=as.numeric(scored_1[495:532]), Roma=as.numeric(scored_1[533:570]), Sampdoria=as.numeric(scored_1[571:608]), Sassuolo=as.numeric(scored_1[609:646]), Spal=as.numeric(scored_1[647:684]), Torino=as.numeric(scored_1[685:722]), Udinese=as.numeric(scored_1[723:760]))


plot(1:20, model_foot_1.jags$BUGSoutput$mean$def, type = 'h', xaxt = "n",xlab = "",ylab = "Defense parameter")
abline(h=0)
axis(1,names(table(dati$HomeTeam)), at=1:20, las =2 )


plot(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, pch = 1, cex=0.0002, xlab = "Attack effect", ylab = "- defense effect")
text(model_foot_1.jags$BUGSoutput$mean$att,-model_foot_1.jags$BUGSoutput$mean$def, labels=names(table(dati$HomeTeam)), cex = 0.5)
abline(h=0)
abline(v=0)
abline(a=0,b=0.5, lty =2)

plot(model_foot_1.jags$BUGSoutput$sims.array[,,1], type = 'l')
abline(h=model_foot_1.jags$BUGSoutput$mean$att[1], col ='red',lwd=3)

table(model_foot_1.jags$BUGSoutput$mean$theta<0.5)

hist(model_foot_1.jags$BUGSoutput$mean$theta)

round(model_foot_1.jags$BUGSoutput$mean$theta)
#### Table of first model #######
classifica=list("Atalanta"=0,"Bologna"=0,"Cagliari" =0,"Chievo"=0,"Empoli"=0,"Fiorentina"=0, "Frosinone"=0,"Genoa"=0,"Inter"=0,"Juventus"=0,"Lazio"=0,"Milan"=0,"Napoli"=0,"Parma"=0,"Roma"=0,       "Sampdoria"=0, "Sassuolo"=0,"Spal"=0,"Torino"=0,"Udinese"=0)

for (i in 1:380) {
  home_t = season_1819_csv$HomeTeam[i]
  away_t = season_1819_csv$AwayTeam[i]
  if(round(model_foot_1.jags$BUGSoutput$mean$theta[i,1])>round(model_foot_1.jags$BUGSoutput$mean$theta[i,2])){
    classifica[[home_t]]=classifica[[home_t]]+3
  }
  if(round(model_foot_1.jags$BUGSoutput$mean$theta[i,1])<round(model_foot_1.jags$BUGSoutput$mean$theta[i,2])){
    classifica[[away_t]]=classifica[[away_t]]+3
  }
  if(round(model_foot_1.jags$BUGSoutput$mean$theta[i,1])==round(model_foot_1.jags$BUGSoutput$mean$theta[i,2])){
    classifica[[away_t]]=classifica[[away_t]]+1
    classifica[[home_t]]=classifica[[home_t]]+1
  }
}

clas_mod_1 = matrix(NA,1,20)
colnames(clas_mod_1)=names(table(dati$HomeTeam))

for (k in 1:20) {
  ui=names(table(dati$HomeTeam))[k]
  clas_mod_1[1,k]=classifica[[ui]]
}
clas
plot(sort(clas_mod_1), type = 'h',xaxt="n",xlab = "")
axis(1,labels = names(table(dati$HomeTeam))[order(clas_mod_1)], las=2,at=1:20)




#### Real Table of championship ####

classifica_real=list("Atalanta"=0,"Bologna"=0,"Cagliari" =0,"Chievo"=0,"Empoli"=0,"Fiorentina"=0, "Frosinone"=0,"Genoa"=0,"Inter"=0,"Juventus"=0,"Lazio"=0,"Milan"=0,"Napoli"=0,"Parma"=0,"Roma"=0,       "Sampdoria"=0, "Sassuolo"=0,"Spal"=0,"Torino"=0,"Udinese"=0)

for (i in 1:380) {
  home_t = season_1819_csv$HomeTeam[i]
  away_t = season_1819_csv$AwayTeam[i]
  if(season_1819_csv$FTHG[i]>season_1819_csv$FTAG[i]){
    classifica_real[[home_t]]=classifica_real[[home_t]]+3
  }
  if(season_1819_csv$FTHG[i]<season_1819_csv$FTAG[i]){
    classifica_real[[away_t]]=classifica_real[[away_t]]+3
  }
  if(season_1819_csv$FTHG[i]==season_1819_csv$FTAG[i]){
    classifica_real[[away_t]]=classifica_real[[away_t]]+1
    classifica_real[[home_t]]=classifica_real[[home_t]]+1
  }
}

clas_real_1 = matrix(NA,1,20)
colnames(clas_real_1)=names(table(dati$HomeTeam))

for (k in 1:20) {
  ui=names(table(dati$HomeTeam))[k]
  clas_real_1[1,k]=classifica_real[[ui]]
}
clas_real_1


plot(sort(clas_mod_1), type = 'h',xaxt="n",xlab = "",lwd=2)
axis(1,labels = names(table(dati$HomeTeam))[order(clas_mod_1)], las=2,at=1:20)
points(seq(1.2,20.2,length=20),(clas_real_1)[order(clas_mod_1)], type = 'h',xaxt="n",xlab = "", col='red',lwd=2)
barplot(table(paste0(round(model_foot_1.jags$BUGSoutput$mean$theta[,1]),"-",round(model_foot_1.jags$BUGSoutput$mean$theta[,2]))))
barplot(table(paste0(season_1819_csv$FTHG,"-",season_1819_csv$FTAG)),las=2)


#### MODEL WITH three thetas and one rank ####

cat("model {
  # LIKELIHOOD AND RANDOM EFFECT MODEL FOR THE SCORING PROPENSITY
  for (g in 1:380) {
    # Observed number of goals scored by each team
    FTHG[g] ~ dpois(theta[g,1])
    FTAG[g] ~ dpois(theta[g,2])
    # Predictive distribution for the number of goals scored
    ynew[g,1] ~ dpois(theta[g,1] + theta[g,3])
    ynew[g,2] ~ dpois(theta[g,2] + theta[g,3])
    # Average Scoring intensities (accounting for mixing components)
    log(theta[g,1]) <- home + att[HomeTeam[g]] + def[AwayTeam[g]] 
    log(theta[g,2]) <- att[AwayTeam[g]] + def[HomeTeam[g]] - rank[HomeTeam[g]]*0.1
    log(theta[g,3]) <- beta_const[g]
    
  }
  # 1. BASIC MODEL FOR THE HYPERPARAMETERS
  # prior on the home effect
  home ~ dnorm(0,0.0001)
  for(g in 1:380){
    beta_const[g] ~ dunif(-7,0) 
  }
  # ‘‘sum-to-zero’’ CONDITION
  for (t in 1:20){
    att.star[t] ~ dnorm(mu.att,tau.att)
    def.star[t] ~ dnorm(mu.def,tau.def)
    att[t] <- att.star[t] - mean(att.star[])
    def[t] <- def.star[t] - mean(def.star[])
  }
  # priors on the random effects
  
  mu.att ~ dnorm(0,0.0001)
  mu.def ~ dnorm(0,0.0001)
  tau.att ~ dgamma(.01,.01)
  tau.def ~ dgamma(.01,.01)
}", file="project_model_2_jags.txt",fill=TRUE)

#+ beta_h[HomeTeam[g]]*lambda1 + beta_a[AwayTeam[g]]*lambda2

inits2_2=(list(att.star=att,def.star=def))
#,theta=matrix(rep(0.001,760),nrow = 380, ncol=2))
inits2
att=rep(0.01,20)
colnames(att) = names(table(dati$HomeTeam))
att=data.frame(att)

def=rep(0.01,20)
colnames(def) = names(table(dati$HomeTeam))
def=data.frame(def)

as.mcmc.list(as.mcmc(model_foot_1.jags))

class(mcmc.football)

dati_mod_2 = data.frame(cbind(dati$HomeTeam,dati$AwayTeam,round(model_foot_2.jags$BUGSoutput$mean$ynew)))
colnames(dati_mod_2) = c("HomeTeam","AwayTeam","FTHG","FTAG")
##### FUNZIONE GENERAZIONE GOAL PER SQUADRA MODELLO 2 ####
squadra_2 = function(nome){
  goals=numeric(38)
  conceded=numeric(38)
  countg=0
  countc=0
  for (i in 1:380) {
    if(dati_mod_2$AwayTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=dati_mod_2$FTAG[i]
      conceded[countc]=dati_mod_2$FTHG[i]
      
    }
    if(dati_mod_2$HomeTeam[i] == nome){
      countg=countg+1
      countc=countc+1
      goals[countg]=dati_mod_2$FTHG[i]
      conceded[countc]=dati_mod_2$FTAG[i]
    }
  }
  return(list(Fatti=goals,Concessi=conceded))
}


##### CLASSIFICA BIVARIATE POISSON #######


classifica_BIVPm=list("Atalanta"=0,"Bologna"=0,"Cagliari" =0,"Chievo"=0,"Empoli"=0,"Fiorentina"=0, "Frosinone"=0,"Genoa"=0,"Inter"=0,"Juventus"=0,"Lazio"=0,"Milan"=0,"Napoli"=0,"Parma"=0,"Roma"=0,"Sampdoria"=0, "Sassuolo"=0,"Spal"=0,"Torino"=0,"Udinese"=0)

table(model_foot_2.jags$BUGSoutput$sims.list$ynew[,13,1])
table(model_foot_2.jags$BUGSoutput$sims.list$ynew[,13,2])

median(model_foot_2.jags$BUGSoutput$sims.list$ynew[,13,2])

table(model_foot_2.jags$BUGSoutput$sims.list$ynew[,14,1])
table(model_foot_2.jags$BUGSoutput$sims.list$ynew[,14,2])

unlist(score$Juventus)
unlist(subire$Juventus)


par(mfrow=c(1,2))
plot(table(paste(unlist(score$Juventus), "-", unlist(subire$Juventus))),ylab = "Number of matches with a final score", main ="Juventus", las =2)
points(2,5, col ='red', type='h', lwd = 3)
box()
plot(table(paste(unlist(score$Parma), "-", unlist(subire$Parma))), las =2, ylab = "Number of matches with a final score", main ="Parma")
points(6,5, col ='red', type='h', lwd = 3)
box()

for (i in 1:380) {
  home_t = season_1819_csv$HomeTeam[i]
  away_t = season_1819_csv$AwayTeam[i]
  if(round(model_foot_2.jags$BUGSoutput$median$ynew[i,1])>round(model_foot_2.jags$BUGSoutput$median$ynew[i,2])){
    classifica_BIVPm[[home_t]]=classifica_BIVPm[[home_t]]+3
  }
  if(round(model_foot_2.jags$BUGSoutput$median$ynew[i,1])<round(model_foot_2.jags$BUGSoutput$median$ynew[i,2])){
    classifica_BIVPm[[away_t]]=classifica_BIVPm[[away_t]]+3
  }
  if(round(model_foot_2.jags$BUGSoutput$median$ynew[i,1])==round(model_foot_2.jags$BUGSoutput$median$ynew[i,2])){
    classifica_BIVPm[[away_t]]=classifica_BIVPm[[away_t]]+1
    classifica_BIVPm[[home_t]]=classifica_BIVPm[[home_t]]+1
  }
}


plot(1:20,classifica_BIVPm, type='h', ylim = c(0,90), xaxt="n", xlab ="", ylab="Points")
points((1:20)+0.2,classifica_real, type = 'h', col ='red')
axis(1, at=1:20, labels = names(table(dati$HomeTeam)), las =2)
legend("topright", lwd =2, col =c('black','red'), legend = c("Bivariate","Real"))


classifica_BIVPm

model_foot_2.jags$BUGSoutput$median$ynew

##### GOALS SCORED #####

scored_mod2=numeric(380)
subito_mod2=c()
for (i in 1:20) {
  scored_mod2=append(scored_mod2, values=squadra_2(names(table(dati_mod_2$HomeTeam))[i])$Fatti)
  subito_mod2=append(subito_mod2, values=squadra_2(names(table(dati_mod_2$HomeTeam))[i])$Concessi)
}
scored_2=scored_mod2[-(1:380)]

score2=list(Atalanta=as.numeric(scored_2[1:38]), Bologna=as.numeric(scored_2[39:76]), Cagliari=as.numeric(scored_2[77:114]),Chievo=as.numeric(scored_2[115:152]), Empoli=as.numeric(scored_2[153:190]), Fiorentina=as.numeric(scored_2[191:228]), Frosinone=as.numeric(scored_2[229:266]), Genoa=as.numeric(scored_2[267:304]), Inter=as.numeric(scored_2[305:342]), Juventus=as.numeric(scored_2[343:380]), Lazio=as.numeric(scored_2[381:418]), Milan=as.numeric(scored_2[419:456]), Napoli=as.numeric(scored_2[457:494]), Parma=as.numeric(scored_2[495:532]), Roma=as.numeric(scored_2[533:570]), Sampdoria=as.numeric(scored_2[571:608]), Sassuolo=as.numeric(scored_2[609:646]), Spal=as.numeric(scored_2[647:684]), Torino=as.numeric(scored_2[685:722]), Udinese=as.numeric(scored_2[723:760]))

subire2=list(Atalanta=subito_mod2[1:38], Bologna=subito_mod2[39:76], Cagliari=subito_mod2[77:114],Chievo=subito_mod2[115:152], Empoli=subito_mod2[153:190], Fiorentina=subito_mod2[191:228], Frosinone=subito_mod2[229:266], Genoa=subito_mod2[267:304], Inter=subito_mod2[305:342], Juventus=subito_mod2[343:380], Lazio=subito_mod2[381:418], Milan=subito_mod2[419:456], Napoli=subito_mod2[457:494], Parma=subito_mod2[495:532], Roma=subito_mod2[533:570], Sampdoria=subito_mod2[571:608], Sassuolo=subito_mod2[609:646], Spal=subito_mod2[647:684], Torino=subito_mod2[685:722], Udinese=subito_mod2[723:760])

barplot(rbind(unlist(lapply(score, sum)),unlist(lapply(score2, sum))), beside = T, las =2, ylim=c(0,80), col = c('darkorange','purple2'))

legend("topright", box.col = "white", fill = c('darkorange','purple2'), legend = c("Real","Bivariate Poisson"))

table(score$Frosinone)
table(score$Chievo)
table(score$Cagliari)
table(score$Udinese)
lapply(score, table)

unlist(lapply(score3, sum))-unlist(lapply(score, sum))
box()
#######
# Parameters to monitor

params2_2=c("att","def","theta","home","beta_const","ynew")
parms = c("beta","g")


dat_2 = list("HomeTeam"=as.numeric(as.factor(dat$HomeTeam)),"AwayTeam"=as.numeric(as.factor(dat$AwayTeam)), "rank"=dati$rank, "FTHG" = dati$FTHG, "FTAG"=dati$FTAG)

library(R2jags)
model_foot_2.jags=jags(data=dat_2,inits=list(inits2_2),
                       parameters.to.save=params2_2,
                       model.file="project_model_2_jags.txt",
                       DIC=TRUE,n.chains=1,n.iter=10000,n.burnin=1000,n.thin=1)


model_foot_2.jags$BUGSoutput$last.values


round(model_foot_2.jags$BUGSoutput$mean$ynew)
round(model_foot_2.jags$BUGSoutput$mean$theta[3])
hist(model_foot_2.jags$BUGSoutput$mean$theta[,3])
barplot(table(c(round(model_foot_2.jags$BUGSoutput$mean$ynew[,1]),round(model_foot_2.jags$BUGSoutput$mean$ynew[,2]))))

model_foot_2.jags$BUGSoutput$DIC


plot(1:9000,cumsum(model_foot_2.jags$BUGSoutput$sims.list$theta[,2,3])/(1:9000), type = 'l')
model_foot_2.jags$BUGSoutput$DIC
model_foot_1.jags$BUGSoutput$DIC
##### PLOTS ######
BV00$beta3
BV00$lambda3
library(R2jags)
print(model_foot_1.jags)


par(mfrow=c(2,2))
plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,1])/(1:9000), ylab = "", xlab = "Atalanta Attack", pch=16, col = c('black','blue'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,1])/(1:9000), ylab = "", xlab = "Atalanta Defense", pch=16, col = c('black','blue'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,7])/(1:9000), ylab = "", xlab = "Frosinone Attack", pch=16, col = c('gold','gold'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,7])/(1:9000), ylab = "", xlab = "Frosinone Defense", pch=16, col = c('gold','gold'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,15])/(1:9000), ylab = "", xlab = "Roma Attack", pch=16, col = c('gold','red2'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,15])/(1:9000), ylab = "", xlab = "Roma Defense", pch=16, col = c('gold','red2'))

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$att[,15])/(1:9000), ylab = "", xlab = "Fiorentina Attack", pch=16, col = "purple3")

plot(1:9000,cumsum(model_foot_1.jags$BUGSoutput$sims.list$def[,15])/(1:9000), ylab = "", xlab = "Fiorentina Defense", pch=16, col = "purple3")

par(mfrow=c(1,1))
library(mcmc)
library(coda)
library(help=coda)
library(R2jags)
class()
coda.samples(model_foot_1.jags)
class(model_foot_1.jags)
mcmc.football <- as.mcmc(model_foot_1.jags)
mcmc.football <-as.array((model_foot_1.jags))
class(mcmc.football)
str(mcmc.football)
(mcmc.football[1])

autocorr.plot(model_foot_1.jags,auto.layout=FALSE)
effectiveSize(model_foot_1.jags)

# another useful function for effective sample size
# can be found in the "LaplacesDemon" package
# LaplacesDemon::ESS

library(ggmcmc)
LaplacesDemon::ESS(model_foot_1.jags$BUGSoutput$[,,1])
S <- ggs((mcmc.football))
# look at a collection of diagnostics graphics in a single PDF file 
??ggmcmc
ggmcmc(S)
S

#####PLOTFIGO#####

library(ggplot2)
library(ggridges)
library(viridis)
par(mfrow=c(1,1))
p = ggplot(data.frame(plotfigo[1:90000,]), aes(x=as.numeric(plotfigo[1:90000,1]),y=plotfigo[1:90000,2],fill =after_stat(x))) + geom_density_ridges_gradient(scale=2) + xlab("Attack effect") + ylab("") + scale_fill_viridis(option='B')
p

q = ggplot(data.frame(plotfigo[90001:180000,]), aes(x=as.numeric(plotfigo[90001:180000,1]),y=plotfigo[90001:180000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Attack effect") + ylab("") + scale_fill_viridis(option='B')
q <- q + guides(fill=guide_legend(title="Teams"))
q
print(model_foot_1.jags)
length(plotfigo1[,1])
round(plotfigo1[,1],2)
plotfigo=matrix(NA, nrow = 9000*20,ncol=2)
dim(model_foot_1.jags$BUGSoutput$sims.array)

beta = c('beta1')
beta[i+1]
for (i in 0:19) {
    plotfigo[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
    plotfigo[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}

plotfigo1=matrix(NA, nrow = 9000*20,ncol=2)

for (i in 0:19) {
  plotfigo1[(1+(9000*i)):(9000+9000*i),1] = model_foot_1.jags$BUGSoutput$sims.array[,,20+i+1]
  plotfigo1[(1+(9000*i)):(9000+9000*i),2] = names(table(dati$HomeTeam))[i+1]
}

r = ggplot(data.frame(plotfigo1[1:90000,]), aes(x=as.numeric(plotfigo1[1:90000,1]),y=plotfigo1[1:90000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Defense effect") + ylab("") + scale_fill_viridis(option='B') + the

r

s = ggplot(data.frame(plotfigo1[90001:180000,]), aes(x=as.numeric(plotfigo1[90001:180000,1]),y=plotfigo1[90001:180000,2],fill =after_stat(x))) + geom_density_ridges_gradient() + xlab("Defense effect") + ylab("") + scale_fill_viridis(option='B')
s


as.numeric(plotfigo1[,1])
table(plotfigo1[,2])

1:9000
9001:18000

plotfigo[1:12,1] = rnorm(12)
plotfigo[1:12,1]


k+k*i

k+k*i
9000+(20*9000)
model_foot_1.jags$BUGSoutput$sims.array[,,1][1]
sa = data.frame(model_foot_1.jags$BUGSoutput$sims.list$att)
colnames(sa) = names(table(dati$HomeTeam))[1]
sa
data.frame()
model_foot_1.jags$BUGSoutput$sims.array[1,1,1:40]

###################### BV TZOUFRAS ######

att_home = data.frame(t(model_foot_1.jags$BUGSoutput$mean$att))
colnames(att_home) = names(table(dati$HomeTeam))
dati$HomeEffectAtt = t(att_home[dati$HomeTeam])

def_home = data.frame(t(model_foot_1.jags$BUGSoutput$mean$def))
colnames(def_home) = names(table(dati$HomeTeam))
dati$HomeEffectDef = t(def_home[dati$HomeTeam])

att_away = data.frame(t(model_foot_1.jags$BUGSoutput$mean$att))
colnames(att_away) = names(table(dati$HomeTeam))
dati$AwayEffectAtt = t(att_away[dati$AwayTeam])

def_away = data.frame(t(model_foot_1.jags$BUGSoutput$mean$def))
colnames(def_away) = names(table(dati$AwayTeam))
dati$AwayEffectDef = t(def_away[dati$AwayTeam])

BV_null = lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, distribution = "geometric", data=data.frame(dati))

BV_null$beta3
exp(-1.86381)

BV_null$lambda3
round(BV_null$fitted.values)

lambda1=0
lambda2=0
BV00=lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, l3=~ lambda1 + lambda2, distribution = "geometric", data=dati)

BV00$lambda3

lambda1=1
lambda2=0
BV10=lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, l3=~ lambda1 + lambda2, distribution = "geometric", data=dati)

BV11$lambda3

lambda1=0
lambda2=1
BV01=lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, l3=~ lambda1 + lambda2, distribution = "geometric", data=dati)


lambda1=1
lambda2=1
BV11=lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, l3=~ lambda1 + lambda2, distribution = "geometric", data=dati)
BV11$fitted.values
table(floor(BV11$fitted.values[,1]))

lm.dibp(l1=FTHG ~ HomeEffectAtt + AwayEffectDef,l2= FTAG ~ HomeEffectDef + AwayEffectAtt, l3=~ lambda1 + lambda2, jmax = 1, data=dati)

round(BV00$fitted.values)

######### CLASSIFICA POISSON BIVARIATO ######

classifica_BIVP10=list("Atalanta"=0,"Bologna"=0,"Cagliari" =0,"Chievo"=0,"Empoli"=0,"Fiorentina"=0, "Frosinone"=0,"Genoa"=0,"Inter"=0,"Juventus"=0,"Lazio"=0,"Milan"=0,"Napoli"=0,"Parma"=0,"Roma"=0,"Sampdoria"=0, "Sassuolo"=0,"Spal"=0,"Torino"=0,"Udinese"=0)

for (i in 1:380) {
  home_t = season_1819_csv$HomeTeam[i]
  away_t = season_1819_csv$AwayTeam[i]
  if(round(BV10$fitted.values[i,1])>round(BV00$fitted.values[i,2])){
    classifica_BIVP10[[home_t]]=classifica_BIVP10[[home_t]]+3
  }
  if(round(BV10$fitted.values[i,1])<round(BV10$fitted.values[i,2])){
    classifica_BIVP10[[away_t]]=classifica_BIVP10[[away_t]]+3
  }
  if(round(BV10$fitted.values[i,1])==round(BV10$fitted.values[i,2])){
    classifica_BIVP10[[away_t]]=classifica_BIVP10[[away_t]]+1
    classifica_BIVP10[[home_t]]=classifica_BIVP10[[home_t]]+1
  }
}

clas_BIVP_10 = matrix(NA,1,20)
colnames(clas_BIVP_10)=names(table(dati$HomeTeam))

for (k in 1:20) {
  ui=names(table(dati$HomeTeam))[k]
  clas_BIVP_10[1,k]=classifica_BIVP10[[ui]]
}
clas_BIVP_10


barplot(rbind(clas_mod_1,clas_real_1,clas_BIVP_10), beside = T, xlab = names(dati$HomeTeam), las =2, ylim =c(0,100), col =c("turquoise4","orange","red"),yaxt="n")
axis(2, at=seq(10,100,10),las=1)
box()
legend("topright",legend = c("Double Poisson model","Real data", "Bivariate 1-0"), fill = c("turquoise4","orange","red"))

sum((clas_mod_1-clas_real_1)^2)
sum((clas_BIVP_10-clas_real_1)^2) 

cor(unlist(lapply(score,sum)),t(clas_real_1))
cor(unlist(lapply(subire,sum)),t(clas_real_1))
clas_real_1
positions = c(3,10,15,20,18,16,19,17,4,1,8,5,2,14,6,9,11,13,7,12)
clas_real_1
unlist(lapply(score,sum))


plot(c(att_away),c(def_away))
density(c(att_away) + c(def_home))

length(model_foot_2.jags$BUGSoutput$sims.list$att[,10])
plot(8000:9000, (cumsum(model_foot_2.jags$BUGSoutput$sims.list$att[,10])/(1:9000))[8000:9000], type = 'l')
plot()
