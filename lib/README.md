The `helpers.R` script holds a couple of useful functions for this project:

  * `simulate_crimes` simulates a set of crime report counts based on population 
  and fixed latent crime rate
  * `mean_crimes` creates a set of towns (crime rate and population pairs) for
  input in `simulate_crimes`. This is based on mean crime rate throughout the
  years in the ssb data.
  * `stats` takes as input a set of true crime rates, populations, and simulated crime 
  report counts. Outputs empirical bayes point estimate, 95% credibility interval,
  and a 95% confidence interval for the MLE estimate counts/population.
  * `total_crime` helper function for `mean_crimes`
