# demonstration of checking and visualisation in model building

library(rstan)
library(bayesplot)
redcol<-"#b62525"
bluecol<-"#26469c"
greencol<-"#84ac1b"
purplecol<-"#b44ccd"
semitrans<-function(x, alpha="30") { paste0(x,alpha) }
load("trains.RData")
plot(trains$caltime, trains$london_se, 
     ylim=c(0,15), yaxs='i', pch=19, cex=0.9, col="#00000030", lwd=0,
     ylab="% trains delayed", xlab="Time", sub="(Data from orr.gov.uk)")
plot(trains$caltime, trains$london_se, 
     ylim=c(0,15), yaxs='i', type='l',
     ylab="% trains delayed", xlab="Time", sub="(Data from orr.gov.uk)")
trains$year <- trains$period-2000

# model with stupid linear regression
model1 <- '
data {
  int N;
  real delay[N];
  real year[N];
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta1*year[i]);
  }
}
model {
  beta0 ~ uniform(1,5);
  beta1 ~ uniform(-10,10);
  sigma ~ uniform(0.2,3);
  for(i in 1:N) {
    delay[i] ~ normal(pred[i], sigma);
  }
}
generated quantities {
  real postpred[N];
  for(i in 1:N) {
    postpred[i] = normal_rng(beta0 + beta1*year[i], sigma);
  }
}
'
model1_prior <- '
data {
  int N;
  real year[N];
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta1*year[i]);
  }
}
model {
  beta0 ~ uniform(1,5);
  beta1 ~ uniform(-10,10);
  sigma ~ uniform(0.2,3);
}
generated quantities {
  real priorpred[N];
  for(i in 1:N) {
    priorpred[i] = normal_rng(beta0 + beta1*year[i], sigma);
  }
}
'

# Fit the linear regression model
stanfit1 <- stan(model_code=model1,
                 data=list(N = dim(trains)[1],
                           delay = trains$london_se,
                           year = trains$year),
                   warmup=1000,
                   iter=2000,
                   chains=3,
                 cores=3)
print(stanfit1)
#traceplot(stanfit1, inc_warmup=TRUE)
chains1 <- rstan::extract(stanfit1, permuted=TRUE)
#ppc_dens_overlay(trains$london_se, chains1$pred)

plot(trains$caltime,trains$london_se,
     col=semitrans(bluecol),cex=0.9,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Mean predictions",xlab="Time",ylab="% delays")
lines(trains$caltime, apply(chains1$pred,2,mean),col=redcol,lwd=1.6)

plot(rep(trains$caltime,each=nrow(chains1$pred)), 
     as.vector(chains1$pred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)

plot(rep(trains$caltime,each=nrow(chains1$postpred)), 
     as.vector(chains1$postpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-5,15),yaxs='i',
     main="Posterior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)

stanfit1_prior <- stan(model_code=model1_prior,
                       data=list(N = dim(trains)[1],
                                 year = trains$year),
                       warmup=1000,
                       iter=2000,
                       chains=3,
                       cores=3)
chains1_prior <- rstan::extract(stanfit1_prior, permuted=TRUE)
plot(rep(trains$caltime,each=nrow(chains1_prior$priorpred)), 
     as.vector(chains1_prior$priorpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-25,35),yaxs='i',
     main="Prior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)





###########################
# model constraining phase and wavelength of sine wave within year
trains$t <- sin(trains$fourweek*2*pi/13 - 4)
model2 <- '
data {
  int N;
  real delay[N];
  real t[N];
  real year[N];
}
parameters {
  real beta0;
  real beta_year;
  real beta_wave;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta_year*year[i]) + (beta_wave*t[i]);
  }
}
model {
  beta0 ~ uniform(1,5);
  beta_year ~ uniform(-0.3,0.3);
  beta_wave ~ uniform(0,2);
  sigma ~ uniform(0.2,3);
  for(i in 1:N) {
    delay[i] ~ normal(beta0 + (beta_year*year[i]) + (beta_wave*t[i]), sigma);
  }
}
generated quantities {
  real postpred[N];
  for(i in 1:N) {
    postpred[i] = normal_rng(beta0 + (beta_year*year[i]) + (beta_wave*t[i]), sigma);
  }
}
'
stanfit2 <- stan(model_code=model2,
                  data=list(N = dim(trains)[1],
                           delay = trains$london_se,
                           t = trains$t,
                           year = trains$year),
                  warmup=1000,
                  iter=2000,
                  chains=3,
                  cores=3,
                  init=list(list(beta0=2.1,
                                 beta_year=0.1,
                                 beta_wave=0.5,
                                 sigma=1.1),
                            list(beta0=2.2,
                                 beta_year=0.2,
                                 beta_wave=0.4,
                                 sigma=1.2),
                            list(beta0=2.3,
                                 beta_year=0.3,
                                 beta_wave=0.3,
                                 sigma=1.3)))
chains2 <- rstan::extract(stanfit2, permuted=TRUE)

plot(trains$caltime,trains$london_se,
     col=semitrans(bluecol),cex=0.9,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Mean predictions",xlab="Time",ylab="% delays")
lines(trains$caltime, apply(chains2$pred,2,mean),col=redcol,lwd=1.6)

plot(rep(trains$caltime,each=nrow(chains2$pred)), 
     as.vector(chains2$pred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)

plot(rep(trains$caltime,each=nrow(chains2$postpred)), 
     as.vector(chains2$postpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-5,15),yaxs='i',
     main="Posterior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)





model2_prior <- '
data {
  int N;
  real t[N];
  real year[N];
}
parameters {
  real beta0;
  real beta_year;
  real beta_wave;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta_year*year[i]) + (beta_wave*t[i]);
  }
}
model {
  beta0 ~ uniform(1,5);
  beta_year ~ uniform(-0.3,0.3);
  beta_wave ~ uniform(0,2);
  sigma ~ uniform(0.2,3);
}
generated quantities {
  real priorpred[N];
  for(i in 1:N) {
    priorpred[i] = normal_rng(beta0 + (beta_year*year[i]) + (beta_wave*t[i]), sigma);
  }
}
'

stanfit2_prior <- stan(model_code=model2_prior,
                       data=list(N = dim(trains)[1],
                                 t = trains$t,
                                 year = trains$year),
                       warmup=1000,
                       iter=2000,
                       chains=3,
                       cores=3,
                       init=list(list(beta0=2.1,
                                      beta_year=0.1,
                                      beta_wave=0.5,
                                      sigma=1.1),
                                 list(beta0=2.2,
                                      beta_year=0.2,
                                      beta_wave=0.4,
                                      sigma=1.2),
                                 list(beta0=2.3,
                                      beta_year=0.3,
                                      beta_wave=0.3,
                                      sigma=1.3)))
chains2_prior <- rstan::extract(stanfit2_prior, permuted=TRUE)
plot(rep(trains$caltime,each=nrow(chains2_prior$priorpred)), 
     as.vector(chains2_prior$priorpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-25,35),yaxs='i',
     main="Prior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)









###########################
# model with an unconstrained wave
model3 <- '
data {
  int N;
  real delay[N];
  real year[N];
}
parameters {
  real beta0;
  real beta_year;
  real amplitude;
  real phase;
  real wavelength;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta_year*year[i]) + (amplitude*sin((year[i]+phase)*wavelength/6.2832));
  }
}
model {
  beta0 ~ uniform(1,5);
  beta_year ~ uniform(-0.3,0.3);
  amplitude ~ uniform(0.01,10);
  phase ~ uniform(0,20);
  wavelength ~ uniform(0.01,100);
  sigma ~ uniform(0.2,3);
  for(i in 1:N) {
    delay[i] ~ normal(beta0 + (beta_year*year[i]) + (amplitude*sin((year[i]+phase)*wavelength/6.2832)), sigma);
  }
}
generated quantities {
  real postpred[N];
  for(i in 1:N) {
    postpred[i] = normal_rng(beta0 + (beta_year*year[i]) + (amplitude*sin((year[i]+phase)*wavelength/6.2832)), sigma);
  }
}
'

stanfit3 <- stan(model_code=model3,
                 data=list(N = dim(trains)[1],
                           delay = trains$london_se,
                           year = trains$year),
                 warmup=1000,
                 iter=2000,
                 chains=3,
                 cores=3,
                 init=list(list(beta0=2.1,
                                beta_year=0.1,
                                amplitude=0.8,
                                phase=0.4,
                                wavelength=1,
                                sigma=1.1),
                           list(beta0=2.2,
                                beta_year=0.2,
                                amplitude=1.5,
                                phase=5,
                                wavelength=10,
                                sigma=1.2),
                           list(beta0=2.3,
                                beta_year=0.3,
                                amplitude=1,
                                phase=0.2,
                                wavelength=0.3,
                                sigma=1.3)))
chains3 <- rstan::extract(stanfit3, permuted=TRUE)
chains3chain <- rstan::extract(stanfit3, permuted=FALSE)

plot(trains$caltime,trains$london_se,
     col=semitrans(bluecol),cex=0.9,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Mean predictions",xlab="Time",ylab="% delays")
lines(trains$caltime, apply(chains3$pred,2,mean),col=redcol,lwd=1.6)
# chain-specific:
plot(trains$caltime,trains$london_se,
     col=semitrans(bluecol),cex=0.9,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Mean predictions per chain",xlab="Time",ylab="% delays")
lines(trains$caltime, apply(chains3chain[,1,7:266],2,mean),col=redcol,lwd=1.6)
lines(trains$caltime, apply(chains3chain[,2,7:266],2,mean),col=greencol,lwd=1.6)
lines(trains$caltime, apply(chains3chain[,3,7:266],2,mean),col=purplecol,lwd=1.6)


plot(rep(trains$caltime,each=nrow(chains3$pred)), 
     as.vector(chains3$pred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(0,15),yaxs='i',
     main="Predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)

plot(rep(trains$caltime,each=nrow(chains3$postpred)), 
     as.vector(chains3$postpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-5,15),yaxs='i',
     main="Posterior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)


# prior predictions:
model3_prior <- '
data {
  int N;
  real delay[N];
  real year[N];
}
parameters {
  real beta0;
  real beta_year;
  real amplitude;
  real phase;
  real wavelength;
  real<lower=0> sigma;
}
transformed parameters {
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta_year*year[i]) + (amplitude*sin((year[i]+phase)*wavelength/6.2832));
  }
}
model {
  beta0 ~ uniform(1,5);
  beta_year ~ uniform(-0.3,0.3);
  amplitude ~ uniform(0.01,10);
  phase ~ uniform(0,20);
  wavelength ~ uniform(0.01,100);
  sigma ~ uniform(0.2,3);
}
generated quantities {
  real priorpred[N];
  for(i in 1:N) {
    priorpred[i] = normal_rng(beta0 + (beta_year*year[i]) + (amplitude*sin((year[i]+phase)*wavelength/6.2832)), sigma);
  }
}
'
stanfit3_prior <- stan(model_code=model3_prior,
                 data=list(N = dim(trains)[1],
                           delay = trains$london_se,
                           year = trains$year),
                 warmup=1000,
                 iter=2000,
                 chains=3,
                 cores=3,
                 init=list(list(beta0=2.1,
                                beta_year=0.1,
                                amplitude=0.8,
                                phase=0.4,
                                wavelength=1,
                                sigma=1.1),
                           list(beta0=2.2,
                                beta_year=0.2,
                                amplitude=1.5,
                                phase=5,
                                wavelength=10,
                                sigma=1.2),
                           list(beta0=2.3,
                                beta_year=0.3,
                                amplitude=1,
                                phase=0.2,
                                wavelength=0.3,
                                sigma=1.3)))
chains3_prior <- rstan::extract(stanfit3_prior, permuted=TRUE)
chains3chain_prior <- rstan::extract(stanfit3_prior, permuted=FALSE)

plot(rep(trains$caltime,each=nrow(chains3_prior$priorpred)), 
     as.vector(chains3_prior$priorpred),
     col=semitrans(redcol),cex=0.5,pch=19,lwd=0,
     ylim=c(-45,45),yaxs='i',
     main="Prior predictions",xlab="Time",ylab="% delays")
points(trains$caltime,trains$london_se,col=bluecol,cex=0.9)
abline(a=0,b=0)
