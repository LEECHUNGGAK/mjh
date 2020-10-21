# Set Environment ---------------------------------------------------------
library(rms)
library(survival)


# Worked example ----------------------------------------------------------
n <- 1000 # sample size
set.seed(88) # set seed for replication
age <- rnorm(n, 65, 11)
lac <- round(abs(rnorm(n, 3, 1)), 1)
sex <- factor(sample(1:2, n, prob = c(0.6, 0.4), TRUE),
              labels = c("male", "female"))
shock <- factor(sample(1:4, n, prob = c(0.3, 0.3, 0.25, 0.15), TRUE),
               labels = c("no", "mild", "moderate", "severe"))
z <- 0.2 * age + 3 * lac * as.numeric(sex) + 5 * as.numeric(shock) - rnorm(n, 36, 15) # linear combination with a bias
y <- ifelse(runif(n) <= plogis(z), 1, 0)

Y <- ifelse(y == 0, 0, sample(1:3, length(y), TRUE))
data <- data.frame(age = age, lac = lac, sex = sex, shock = shock, y = y, Y = Y)
var.labels = c(age = "Age in Years",
               lac = "lactate",
               sex = "Sex of the participant",
               shock = "shock",
               y = "outcome",
               Y = "ordinal")

label(data) = lapply(names(var.labels),
                     function(x) label(data[, x]) = var.labels[x])


# Nomogram for Binary Outcome ---------------------------------------------
ddist <- datadist(data) # define the distribution summaries for predictor variable
options(datadist = "ddist")
mod.bi <- lrm(y ~ shock + lac * sex + age, data) # fit Logistic Regression Model
nom.bi <- nomogram(mod.bi,
                   lp = TRUE, # add linear predictor
                   lp.at = seq(-3, 4, by = 0.5), # set the range of linear predictor
                   fun = function(x) 1 / (1 + exp(-x)),
                   fun.at = c(0.001, 0.01, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99, 0.999),
                   funlabel = "Risk of Death",
                   conf.int = c(0.1, 0.7), # control the range of CI
                   abbrev = TRUE, # abbreviate levels of categorical factors
                   minlength = 1) # minimum abbreviation length

plot(nom.bi,
     lplabel = "Linear Predictor", # rename the linear predictor axis
     fun.side = c(3, 3, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 3), # position tick marks below axis(1) or above the axis(3)
     col.conf = c("red", "green"), # CI color
     conf.space = c(0.1, 0.5), # CI vertical space
     label.every = 3, # adjust the frequency of label
     col.grid = gray(c(0.8, 0.95)), # grid color
     which = "shock")
legend.nomabbrev(nom.bi, which = "shock", x= 0.5, y = 0.5)


# Nomogram for Ordinal Outcome Variable -----------------------------------
mod.ord <- lrm(Y ~ age + rcs(lac, 4) * sex)

fun2 <- function(x) plogis(x - mod.ord$coefficients[1] + mod.ord$coefficients[2])
fun3 <- function(x) plogis(x - mod.ord$coefficients[1] + mod.ord$coefficients[3])

f <- Newlabels(mod.ord, c(age = "Age in Years"))
nom.ord <- nomogram(f, fun = list("Prob Y >= 1" = plogis,
                                  "Prob Y >= 2" = fun2,
                                  "Prob Y >= 3" = fun3),
                    lp = FALSE,
                    fun.at = c(0.01, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99))
plot(nom.ord, lmgp = 0.2, cex.axis = 0.6)


# Nomogram for Survival Data ----------------------------------------------
lung$sex <- factor(lung$sex, labels = c("male", "female"))

mod.sur <- psm(Surv(time, status) ~ ph.ecog + sex + age, lung, dist = "weibull")

med <- Quantile(mod.sur)
surv <- Survival(mod.sur)

ddist <- datadist(lung)
nom.surl <- nomogram(mod.sur,
                     fun = list(function(x) med(lp = x, q = 0.5),
                                function(x) med(lp = x, q = 0.25)),
                     funlabel = c("Median Survival Time",
                                  "1Q Survival Time"),
                     lp = FALSE)
plot(nom.surl,
     fun.side = list(c(rep(1, 7), 3, 1, 3, 1, 3), rep(1, 7)),
     col.grid = c("red", "green"))
nom.sur2 <- nomogram(mod.sur, fun = list(function(x) surv(200, x),
                                         function(x) surv(400, x)),
                     funlabel = c("200-Day Survival Probability",
                                  "400-Day Survival Probability"),
                     lp = FALSE)
plot(nom.sur2,
     fun.side = list(c(rep(c(1, 3), 5), 1, 1, 1, 1),
                     c(1, 1, 1, rep(c(3, 1), 6))),
     xfrac = 0.7,
     col.grid = c("red", "green"))

# Nomogram for Semiparametric Survival Models -----------------------------
mod.cox <- cph(Surv(time, status) ~ ph.ecog + sex + age,
               lung,
               surv = TRUE)
surv.cox <- Survival(mod.cox)
nom.cox <- nomogram(mod.cox,
                    fun = list(function(x) surv.cox(200, x),
                               function(x) surv.cox(400, x)),
                    funlabel = c("200-Day Sur.Prob.",
                                 "400-Day Sur.Prob."),
                    lp = FALSE)
plot(nom.cox,
     fun.side = list(c(rep(c(1, 3), 5), 1, 1, 1, 1),
                     c(1, 1, 1, rep(c(3, 1), 6))))
