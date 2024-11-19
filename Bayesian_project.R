risultati=matrix(NA, nrow=31, ncol = 2)
risultati_frame=data.frame(risultati)
risultati_frame[35,1]=4
colnames(risultati_frame)=c("Home goals", "Away goals")

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




prod(dpois(score$Atalanta, lambda = 2))

#### MCMC JAGS FOR DATA ATT E DEF ####




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




plot(1:20, model_foot_1.jags$BUGSoutput$mean$def, type = 'h', xaxt = "n",xlab = "",ylab = "Defense parameter")
abline(h=0)
axis(1,names(table(dati$HomeTeam)), at=1:20, las =2 )






table(model_foot_1.jags$BUGSoutput$mean$theta<0.5)

hist(model_foot_1.jags$BUGSoutput$mean$theta)

round(model_foot_1.jags$BUGSoutput$mean$theta)




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


##### PLOTS ######
BV00$beta3
BV00$lambda3
library(R2jags)
print(model_foot_1.jags)




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
