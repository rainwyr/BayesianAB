# Bayesian A/B Testing

Comparison of Bayesian A/B Testing to Frequentist A/B Testing on:

* Effect of peeking
* Effect of sample size
* Effect of sample imbalance
* Effect of prior parameter selection

# Parameter Space


* nreps: 500
* days (of experiment): 30
* Parameter Selection: [Directional, Confident, Wrong, (Neutral)]
* sample_size_per_day: [50, 500, 5000, 50000]
* threshold (of caring): [0.1, 0.01, 0.001, 0.0001, 0.00001]


# Summary

| Case ID | Application      | Distribution          | Effect  |
| ------- | ---------------- | --------------------- | ------- |
| 1       | Payer Conversion | Bernoulli             | False   |
| 2       | Payer Conversion | Bernoulli             | True    |
| 3       | Total Moves      | Poisson               | False   |
| 4       | Total Moves      | Poisson               | True    |
| 5       | Revenue          | Bernoulli-Exponential | False   |
| 6       | Revenue          | Bernoulli-Exponential | True    |


| Case ID | Peeking | Sample Size | Sample Imbalance | Parameter Selection |
| ------- | ------- | ----------- | ---------------- | ------------------- |
| 1       | X%      | Sensitive   | Sensitive        | Insensitive         |
| 2       | X%      | Sensitive   | Sensitive        | Insensitive         |
| 3       | X%      | Sensitive   | Sensitive        | Insensitive         |
| 4       | X%      | Sensitive   | Sensitive        | Insensitive         |
| 5       | X%      | Sensitive   | Sensitive        | Insensitive         |
| 6       | X%      | Sensitive   | Sensitive        | Insensitive         |


# Conclusions

* Bayesian A/B testing can be used to speed up experimentation not by shortening the duration but by requiring much less samples to achieve a comparable performance as Frequentist A/B testing framework.
* Threshold of caring differs from problem to problem but could leverage some simulation for best choices.
