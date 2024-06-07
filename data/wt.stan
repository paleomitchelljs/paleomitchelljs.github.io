
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


