## Note! I no longer use Github. This repo may be out of date.

All my ex-Github repos are now stored and maintained at [my personal website](http://www.robertgrantstats.co.uk/code.html).

Why did I leave Github? Because I consider the compulsory imposition of 2-factor authentication to be inappropriate for people writing software, including cryptography, which can attract severe punishments in certain jurisdictions. We all know that the organisations that hold the 2nd factors (mobile telephony providers, tech companies) are compromised, willingly or otherwise, in their relationships with security agencies, benign or otherwise.

Why not just close it down? Because you might use it programmatically, via http or API, and I don't want to hurt you (by breaking your code) while trying to help you (by raising issues of privacy and confidentiality).



# bayes-checking-viz
Examples of using visualisation to help build Bayesian models by MCMC

----

Accompanying various talks, this shows some ways in which visualisation can reveal problems in our statistical models which might otherwise go unnoticed and cause us problems.

We'll look at a dataset of 20 years of train delay data for London and the Southeast of England. This appears in a few places in my (ahem) [book](https://github.com/robertgrant/dataviz-book). The % of delayed or cancelled journeys has to be reported by railway companies on a four-week cycle, so there are 13 such reporting periods per year. Suppose that we are interested in modelling this outcome ( the variable is called london_se) as a function of time (the variable is called time, centered at the beginning of 2000, so that any y-intercept term in a regression model will refer to that point in time), specifically to investigate seasonal cycles in railway performance. We do not use time series models but rather some pretty silly regression models, in order to illustrate simple points.

All code is in [demo.R](demo.R) and the data are in [trains.csv](trains.csv). I use `rstan` here and draw the plots with base R (i.e. `graphics::plot()` and friends). These are the sort of visualisations that an analyst would make as they refine and check the model; they could , of course, be made prettier, and if they were to be presented to a non-technical audience, some explanation would be needed.

* Model 1 is a linear regression by time.
* Model 2 adds a sinusoidal function of time of year, so there is a linear slow trend and an annual cycle. We fix the phase (worst in the autumn) and the wavelength (annual) and let Stan tell us the amplitude only.
* Model 3 is the same but we let Stan tell us all three parameters of the wave: the amplitude, phase and wavelength.

I think these models mostly have sensible priors, which is to say, the sort of priors that I might use before finding problems in the data. They are typically uniform and weakly informative, meaning that I try to exclude parameter values I just don't believe can happen. At 260 observations, I think there are enough data to overcome sensitivity to priors, but not so many that we can get complacent.




## Four useful outputs

As you fit your model, there are four useful outputs to look at:
*  predictions: at each iteration of the MCMC algorithm (where the proposed parameter values are accepted), generate the predicted outcome for each observation. In the case of the conditionally Gaussian (normal) model for the train delays, we are asking for the mean: the linear predictor. For binomial or other exponential family models, we should convert the linear predictor to something meaningful like risk or rate. For non-exponential family likelihood functions, you're on your own, but you can always generate the expectation of the residual distribution given the input data.
* mean predictions: just average these for each observation.
* posterior predictions: at each iteration of the MCMC algorithm, and for each observation, generate the prediction (as above) plus/minus a draw from the residual distribution. In other words, a draw from a _rng function that provides a simulation of a new observation with the same values in the input variables, given the current parameter values and the likelihood and prior.
* prior predictions: draw parameter values from the priors, combine them with the observed data in the predictor, and generate new draws from the likelihood _rng. This model should not have the outcome/target variable in the model block because you want every draw from the priors to be retained and used, not to be filtered on the basis of a Metropolis-Hastings match with the data.





## Detailed implementation in Stan

Model 1 has three parameters: beta0 (the intercept), beta1 (the slope), and sigma (the residual standard deviation). In the Stan code ("model1" in demo.R), we set priors:

```
beta0 ~ uniform(1,5);
beta1 ~ uniform(-10,10);
sigma ~ uniform(0.2,3);
```

Then, a predicted delay % is derived from these:

```
  real pred[N];
  for(i in 1:N) {
    pred[i] = beta0 + (beta1*year[i]);
  }
```

I have calculated pred inside the transformed parameters block so that we can access it again, and also so that it will be returned in the MCMC chains, allowing us to do other tricks with it. Importantly, if there are N points at which we want to calcualte pred (N could be the number of observations, but you could also thin it down, interpolate, whatever), and K iterations of the MCMC chain are retained after warmup, we will have a K by N matrix of values. We can average each of the columns to give us the mean prediction for each prediction point, and I will use this as the mean prediction. Means are not the only option, of course, and you could argue for medians and the like. I haven't given much thought to finding modes in this context (the mean prediction is just a rough guide to a central line through the predictions) but I suppose that could be done outside Stan using the chains (that is probably quicker than re-running Stan to optimise over the many dimensions implied by this prediction exercise).
If model checking is not your goal, then you could certainly streamline this code, and you'll find plenty of examples of that inside the Stan manual.

Now, we can use pred and sigma to calculate the log-likelihood for each of the observations:

```
for(i in 1:N) {
  delay[i] ~ normal(pred[i], sigma);
}
```

This allows Stan to accept or reject iterations of the algorithm such that we get the joint posterior distribution out. In the generated quantities block, I use the betas and sigma (which, remember, now come from their joint posterior) to draw from the posterior predictive distribution:
```
real postpred[N];
for(i in 1:N) {
  postpred[i] = normal_rng(beta0 + beta1*year[i], sigma);
}
```

This shows us not just the predicted value, but also the scatter around it. We have to use a Stan _rng function to do this, and they have to run inside the generated quantities block. postpred will also be returned along with the MCMC chains.

The remaining output, the prior predictions, need to be obtained from a different batch of Stan code. That's because you want betas, sigmas and predictions drawn from the prior, without being filtered through the likelihood. If you take a look at that code ("model1_prior" in demo.R), you'll see it's basically the same as "model1", except that there's no likelihood code in the model block. So, the only distributions that are used to retain or reject steps from the algorithm are the priors. Beta and sigma come from the prior, and the predictions follow from that.
When you run any of these prior models, you'll see warnings about the great mojority of post-warmup transitions (iterations) being divergent. This is quite expected if there is no likelihood, and you shouldn't worry about it.







## How to interpret and respond

How might these four outputs be used?
* the **mean predictions** show you what the model reckons will be the curve of best fit (to whatever statistic you used to summarise the predictions; you could get quartiles or whatever if that would help). You can see straight away that Models 1 (Figure 2a) and 2 (Figure 4a) really fail to account for the slow up-and-down trend in railway delays. Model 2 has a small but noticeable amplitude to the annual cycle.
* the **predictions** show you the uncertainty in the curve of best fit; this is the closest thing to seeing the posterior for those parameters (Figure 2b) If there is more or less uncertainty than you would have expected, this might warrant investigation. It's not unusual, especially in polynomials, to see an explosive expansion of uncertainty at the ends of the data. That's warning you not to trust out-of-sample predictions with those sorts of model. On the other hand, do you really believe the slow upward trend of Models 1 & 2? If you set the Stan random-number generator seed to 98, you can see an example of chains that haven't converged. Hopefully you would spot this from the R-hat statistic, but if not, there's no doubt from the predictions plot that things are not right, even though the mean predictions plot looks sensible enough. (Figures 3a and 3b)
*  the **posterior predictions** show what values new data might take, given the uncertainty in the parameter values. If there are observed data outside the range of posterior predictions, then your model needs to be made more accommodating. In the case of the train delay data, the transient periods of extremely poor performance lie outside the posterior predictions (Figure 2c). I suggest this is the fault of the intercept being set at the year 2000, combined with fairly tight priors on the beta0 and beta1 parameters; more on that in the next point.
* the **prior predictions** show what those innocuous-looking priors really imply in terms of the data. Often, we specify them one at a time, univariate, and we are not in a position to imagine how they might combine to produce different predictions. So, get the computer to do it for you! Yes, you have to run a different version of the model, but because you're not evaluating likelihoods for all observations and using the autodiff algorithm to get the next iteration, even very sloooow Stan models (let alone BUGS/JAGS!) can generate these quickly. We'd hope to see the prior predictions encompassing all the data and leaving plenty of room either side. In the train delay data, there's a surprise (Figure 2d). Because we centered the time on the year 2000, we find the regression lines radiating out from there. One extreme observation (caused by nationwide speed restrictions after the Paddington rail crash) actually lies outside the prior predictions: bad news from apparently sensible priors. At the extremes of the time range, the priors allow for extremely wide ranges of predictions. Taken together, this suggests to me that the model should be reparameterised and the priors revised, perhaps by having the intercept at the beginning in 1997, a wider prior for intercept and a narrower one for slope.

Taking these same approaches to modes 2 and 3, we can see model 2's enforced phase and wavelength in the prior predictions, and then repeated all through the others (Figures 4a-d). That should make you think that maybe you've been too zealous in imposing structure on the data. Yet, when we switch to model 3, we find that we've been too lax; allowing Stan to find anything that looks like a wave, it has latched onto different shapes in the three chains (Figure 5b), even though the predictions, posterior predictions and prior predictions look OK at a glance, unless you realise that it shouldn't have any mechanism to follow the long-term undulations, just a seasonal pattern within each year.
