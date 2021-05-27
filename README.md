# Bayesian A/B Testing

Comparison of Bayesian A/B Testing to Frequentist A/B Testing on:

* Effect of peeking
* Effect of sample size
* Effect of prior parameter selection

# Parameter Space

* nreps: [500]
* days (of experiment): [30]
* Parameter Selection: [Directional, Confident, Wrong, (Neutral)]
* sample_size_per_day: [50, 500, 5000, 50000]
* threshold
  * expected loss: [0.1, 0.01, 0.001, 0.0001, 0.00001]
  * P(A>B): [0.95]


# Summary

| Case ID | Application      | Distribution          | Effect  |
| ------- | ---------------- | --------------------- | ------- |
| 1       | Payer Conversion | Bernoulli             | False   |
| 2       | Payer Conversion | Bernoulli             | True    |
| 3       | Total Moves      | Poisson               | False   |
| 4       | Total Moves      | Poisson               | True    |
| 5       | Revenue          | Bernoulli-Exponential | False   |
| 6       | Revenue          | Bernoulli-Exponential | True    |


| Case ID | Peeking         | Sample Size | Parameter Selection |
| ------- | --------------- | ----------- | ------------------- |
| 1       | Not Immune      | Sensitive   | Insensitive         |
| 2       | Not Immune      | Sensitive   | Insensitive         |
| 3       | Not Immune      | Sensitive   | Insensitive         |
| 4       | Not Immune      | Sensitive   | Insensitive         |
| 5       | Not Immune      | Sensitive   | Insensitive         |
| 6       | Not Immune      | Sensitive   | Insensitive         |


# Conclusions

* Bayesian A/B Testing is not immune to peeking, with slightly less risk.
* Bayesian A/B Testing has very similar performance to Frequentist when we make decision based on P(A>B) > 95%, with slightly more power.
* Bayesian A/B Testing result is barely affected by the prior selection with the sample size we have.
