# average wt by day
# univariate weight--using just sample mean as a predictor. SD then represents "how much isn't explained by the mean"
# concept of residuals
# net kilocalries versus net change inweight
# expected slope (x=dWt) should be 3.6. Concept ofa. slope!
# predict weights & take diff between observed & predicted
# circle back to residuals!
# add in sodium/fiber
# recalculate predicted now---and recalculate residuals
# ratio of residuals leads to concept of R squared

# histogram of weight -- NOT NORMAL!
# histogram of residuals -- should be normal!
# unexplained variation: calories burned + water intake
# weight has to be a function of how much goes in versus goes out. So focus on calories
# SLOPE of weight loss should be function of caloric DIFFERENCE
# but there are still residuals? Why?
# food not only stuff in (water!) and CO2 from breathe not only way out (defecation/urinartion)
# no direct measure of both...BUT!
# proxies in the form of sodium + fiber intake
# 
# flat (horizontal) line over time to see if time plays a role in predicting weight.
# Note uneven residuals! Samples ABOVE the mean mostly early, samples BELOW the mean mostly later
# that pattern suggests some of the "missing" information could come from time! If weight is changing over time, one mean is a bad predictor. So how about many means?

# boxplot(wt ~ week) to look to see if wt + week together are a better fit. Calculate residuals.

# abandon week and just use day.

# that does better! color code points by am/pm and...another pattern in the residuals!
# let's explain that...incorporate am/pm into the model
# predict individual points & plot predicted points alongside observed points
# the predicted points...look like two lines!

setwd("~/Dropbox/Professional/paleomitchelljs.github.io/data")
wt <- read.csv("wtdatr.csv")
wt <- wt[!is.na(wt[,2]),]
wt <- wt[-2,]
wt$swt <- wt$wt - wt$wt[1] # get chance in weight from a "standard" weight taken before start
wt$dwt <- c(0, diff(wt$wt)) # difference from one measure to the next
#wt <- wt[-1:11,]
wt$day <- wt$day - 9
model <- bf( swt ~ day + time )
wtfit <- brm(model, data=wt)

plot(wt$day, wt$swt, type="b")
mu <- tapply(wt$swt, wt$time, mean)
abline(h = mu[1], col="red")
abline(h = mu[2], col="red")
Means <- tapply(wt$swt, wt$day, mean)
points(1:length(Means), Means, pch=16, col="red")

cwt <- wt[which(wt$time == "pm"),]
CalIn <- tapply(cwt$calories_in, cwt$day, max, na.rm=T)
CalOut <- tapply(cwt$calories_out, cwt$day, max, na.rm=T)
CalDiff <- (CalOut - CalIn ) / 1e3
WtMu <- tapply(wt$wt, wt$day, mean)
dWt <- diff(WtMu)
dWt <- dWt[-which(names(dWt) == "19")]
plot(CalDiff[names(dWt)], dWt)
abline(0, -1/3.5, col='red', lwd=2)
abline(lm(dWt ~ 0 + CalDiff[names(dWt)]), col='blue', lwd=2, lty=2)

# Tuesday, Mar 26 predictions
	# Mar 27 predict: 282.9 / 291.3
	# Mar 27 actual: 284.4 / 293.2
	# Mar 27 error: 1.5 / 1.9
	# March 31st am prediction: 281.2
	# April 2nd am prediction: 280.3
# Thursday Mar 28 prediction
	# March 31st am/pm: 
	
	
	

## TEACHING POINT:
# If our question is: "are Texas and West lineage frogs different" the answer based on the raw data is "yes", and the answer based on the raw data will always be yes for any comparison of any two groups ever. The normal statistical question is "are the Texas and West lineage frogs **clearly** different?" 
# That's the tricky part! In order to be clearly different, we have to have an expectation--an idea of how different two things *should* be. 

# Consider: if I tell I have a raw dataset that shows a difference in size of 10 lbs between two groups, and ask you are they different, the answer is "yes, they differ by 10 lbs on average". If I ask you if they're clearly different, you're in a pickle. If my two groups that differ by 10 lbs are two groups of elephants, then no, 10 lbs would not be a clear difference between any two groups of elephants. If I tell you the two groups are mice then....hoo boy, yes, 10 lbs would be a VERY clear difference between two groups of mice! The difference there isn't in the data, but in our expectation (=model). We expect elephants to be so large that a person looking at them could not reliably tell which group of elephants weighed a mere 10lbs more than the other. For mice, on the other hand, we expect mice to be so small that an average person could CLEARLY tell a group of mice 10lbs heavier than another group apart!


### TRAINING VS TESTING
 
### AUTO CORRELATION
# Big issue is that OUR MODEL IS HUGELY WRONG!
# Think about it like this: is your weight on Monday *independent* of your weight on Sunday? No! Your weight on Monday is (Your Weight on Sunday) + (How much you lost/gained)

code <- "
data	{
	int<lower=1> N;							// total number of measurements
	int<lower=0> day[N];					// 
	int<lower=0, upper=1> time[N];
	real wt[N];								// 

}
parameters	{
	real mu_t;								// effect of time
	real b_day;								// time effect
	real<lower=0> sigma_t;					// variance in trait mean by taxa
	real<lower=0> sigma_b;					// variance in trait mean by taxa
	real<lower=0> sigma_w;					// variance in trait mean by taxa
}
model	{
	sigma_w ~ student_t(3, 0, 5);			// 
	sigma_b ~ student_t(7, 0, 2);			// 
	sigma_t ~ student_t(7, 0, 5);			// 

	mu_t ~ normal(0, sigma_t);				// 
	b_day ~ normal(-0.1, sigma_b);			// 
	
	for (i in 2:N)	{
		wt[i] ~ normal(b_day * day[i] + wt[i-1] + mu_t * time[i], sigma_w);
	}
}

"
write(code, "wt.stan")

wtdat <- list(N = nrow(wt), wt=wt$wt, day=c(0, wt$day[2:nrow(wt)] - 11), time=setNames(c(0,1), c("am","pm"))[wt$time])

Fit <- stan("wt.stan",
			model_name="rcustom",
			data=wtdat,
			cores=4,
			iter = 5e3,
			control = list(max_treedepth=15, adapt_delta = 0.95)
			)



### AR models
#armodel <- bf(dwt ~ day + time)
#arfit <- brm(armodel, data=wt)
#plot(wt$day, wt$dwt, type="b")

#armodel <- bf(wt ~ ar(p=1) + time)
#arfit <- brm(armodel, data=wt)

armamodel <- bf(wt ~ arma(p=1, q=1) + time)
armafit <- brm(armamodel, data=wt)


### simulate data!

# let's simulate data.
# we'll have a vector of weights, and our weight each day will be the previous day's weight plus a calorie term plus some unobserved variance
weigh <- c()
weigh[1] <- 290
ndays <- 1e3
dailyVar <- 8
lossRate <- 0.2
for (i in 2:ndays) {
	weigh[i] <- rnorm(1, weigh[i-1] - lossRate, dailyVar)
}

