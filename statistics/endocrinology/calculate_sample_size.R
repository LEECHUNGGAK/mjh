# install.packages(c("pwr", "WebPower"))

library(pwr)
library(WebPower)

calculate_d <- function(m1, m2, s1, s2) {
    d <- (m2 - m1) / sqrt((s1^2 + s2^2) / 2)
    return(d)
}


# One Mean T-test ---------------------------------------------------------
pwr.t.test(d = 0.7 / 0.47,
           sig.level = 0.05,
           power = 0.8,
           type = "one.sample",
           alternative = "two.sided")


# Two Means T-test --------------------------------------------------------
# Use difference and standard deviation
pwr.t.test(d = 1/1.3,
           sig.level = 0.05,
           power = 0.8,
           type = "two.sample",
           alternative = "two.sided")

# Use calculated_d function
pwr.t.test(d = calculate_d(m1 = 8.08, m2 = 7.15, s1 = 2.8, s2 = 1.91),
           sig.level = 0.05,
           power = 0.8,
           type = "two.sample",
           alternative = "two.sided")


# Paired T-test -----------------------------------------------------------
pwr.t.test(d = calculate_d(m1 = 8.08, m2 = 7.49, s1 = 2.8, s2 = 2.57),
           sig.level = 0.05,
           power = 0.8,
           type = "paired",
           alternative = "two.sided")
