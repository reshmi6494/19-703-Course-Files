## Final project code
setwd("/Users/reshmighosh/Downloads/untitledfolder/courses/19703/FinalP/FinalProject")

## Set up the working directory:
library(foreign)
library(plm)
library(mgcv)
library(car)
library(arm)
library(lmtest)

## Reading the stata file (dataset) 
Stock <- read.dta("/Users/reshmighosh/Downloads/untitledfolder/Courses/19703/FinalP/pstar.dta")

## Defining the panel data using the firm and years column
Stockp <- pdata.frame(Stock, index = c("cusip", "year"))

## Dependent variable
Stockp$valuesh <- (Stockp$pstar - Stockp$pricef)/Stockp$pricef

## Transforming earnings to the standardized version by dividing with assets
Stockp$divfa <- Stockp$divf/Stockp$netcap
Stockp$earnsha <- Stockp$earnsh/Stockp$netcap

################################################################################

################################################################################

## Initial logarithmic transformations of original variables
Stockp$logrnda <- log(Stockp$rnda)
Stockp$logadva <- log(Stockp$adva)
Stockp$loginva <- log(Stockp$inva)
Stockp$logdebta <- log(Stockp$debta)
Stockp$logpricef <- log(Stockp$pricef)
Stockp$logvala <- log(Stockp$vala)
Stockp$logcfa <- log(Stockp$cfa)
Stockp$logdivfa <- log(Stockp$divfa)
Stockp$logearnsha <- log(Stockp$earnsha)

################################################################################

################################################################################

## Adjusting variables to take to deal with lnfs and NaNs
Stockp$rndaone <- Stockp$rnda + 1
Stockp$advaone <- Stockp$adva + 1
Stockp$invaone <- Stockp$inva + 1
Stockp$debtaone <- Stockp$debta + 1
Stockp$valueshone <- Stockp$valuesh + 1
Stockp$divfaone <- Stockp$divfa + 1
Stockp$earnshasix <- Stockp$earnsha + 6

################################################################################

################################################################################

## Creating lagged dependent variable which will act as a regressor
Stockp$valueshlag <- lag(Stockp$valueshone, 1)
Stockp$earnshalag <- lag(Stockp$earnshasix, 1)

################################################################################

################################################################################

## New logarithmic transformations of ajusted variables

Stockp$logrnda.new <- log(Stockp$rndaone)
Stockp$logadva.new <- log(Stockp$advaone)
Stockp$loginva.new <- log(Stockp$invaone)
Stockp$logdebta.new <- log(Stockp$debtaone)
Stockp$logdivfa.new <- log(Stockp$divfaone)
Stockp$logearnsha.new <- log(Stockp$earnshasix)
Stockp$logvaluesh.new <- log(Stockp$valueshone)


################################################################################

################################################################################

## Dividing the data in 20/60/20 
sample <- unique(Stockp$cusip)
cases <- sample(rep(1:5, length.out = nrow(Stockp)))
first20 <- Stockp[cases == 1, ] # first 20% of the data
middle60 <- Stockp[cases %in% c(2:4), ] # middle 60% of the data
last20 <- Stockp[cases == 5, ] # last 20% of the data
first80 <- Stockp[cases %in% c(1:4), ]
last80 <- Stockp[cases %in% c(2:5), ]

################################################################################

################################################################################

## Use first 20% of the data to check IV distributions

## Plot of R&D - Assets ratio histogram
png(filename = "Hist.png", width = 9000, height = 9000, res = 900)
par(cex = 1.3, mfrow = c(3, 3), cex.lab = 1.5, cex.axis = 1.5)
hist(first20$rnda,
     breaks = "Scott",
     xlab = "RND to asset ratio ",
     main = "Histogram",
     xlim = c(-1, 8))
rug(first20$rnda, col = rgb(1, 0, 0, 0.3))

## Plot of Advertising - Assets ratio histogram
hist(first20$adva,
     breaks = "Scott",
     xlab = "Advertising to asset ratio ",
     main = "Histogram",
     xlim = c(-1, 4))
rug(first20$adva, col = rgb(1, 0, 0, 0.3))

## Plot of Investment - Assets ratio histogram
hist(first20$inva,
     breaks = "Scott",
     xlab = "Investment to asset ratio ",
     main = "Histogram",
     xlim = c(-0.5, 1.5))
rug(first20$inva, col = rgb(1, 0, 0, 0.3))

## Plot of Debts - Assets ratio histogram
hist(first20$debta,
     breaks = "Scott",
     xlab = "Debts to asset ratio ",
     main = "Histogram",
     xlim = c(-0.5, 9))
rug(first20$debta, col = rgb(1, 0, 0, 0.3))

## Plot of Price of Stock Histogram but this is not a regressor but is used to calculate shortfall
hist(first20$pricef,
     breaks = "Scott",
     xlab = "Prices ",
     main = "Histogram",
     xlim = c(-2, 3000))
rug(first20$pricef, col = rgb(1, 0, 0, 0.3))

## Plot of Dividends - Assets ratio histogram
hist(first20$divfa,
     breaks = "Scott",
     xlab = "Dividends-Assets Ratio ",
     main = "Histogram",
     xlim = c(0, 0.4))
rug(first20$divfa, col = rgb(1, 0, 0, 0.3))

## Plot of Earnings- Assets Ratio
hist(first20$earnsha,
     breaks = "Scott",
     xlab = "Earnings-Assets ratio ",
     main = "Histogram",
     xlim = c(-3, 3))
rug(first20$earnsha, col = rgb(1, 0, 0, 0.3))

## Plot of Valuation - Assets ratio histogram
hist(first20$vala,
     breaks = "Scott",
     xlab = "Valuation of stocks ",
     main = "Histogram",
     xlim = c(-2, 400))
rug(first20$vala, col = rgb(1, 0, 0, 0.3))

## Plot of Value shortfall histogram
hist(first20$valuesh,
     breaks = "Scott",
     xlab = "Value Shortfall ",
     main = "Histogram",
     xlim = c(-2, 400))
rug(first20$valuesh, col = rgb(1, 0, 0, 0.3))
dev.off()

################################################################################

################################################################################

## Plotting Log-transformed variables

png(filename = "logRND.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logrnda,
     breaks = "Scott",
     xlab = "Logarithm of RND to asset ratio ",
     main = "Histogram",
     xlim = c(-10, 5))
rug(first20$logrnda, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logADVA.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logadva,
     breaks = "Scott",
     xlab = "Logarithm of Advertising to asset ratio ",
     main = "Histogram",
     xlim = c(-11, 3))
rug(first20$logadva, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logINVA.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$loginva,
     breaks = "Scott",
     xlab = "Logarithm of Investment to asset ratio ",
     main = "Histogram",
     xlim = c(-9, 2))
rug(first20$loginva, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logDebta.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logdebta,
     breaks = "Scott",
     xlab = "Logarithm of Debts to asset ratio ",
     main = "Histogram",
     xlim = c(-10, 4))
rug(first20$logdebta, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logprices.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logpricef,
     breaks = "Scott",
     xlab = "Logarithm of Prices ",
     main = "Histogram",
     xlim = c(-5, 10))
rug(first20$logpricef, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logvala.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logpricef,
     breaks = "Scott",
     xlab = "Logarithm of Valuation ",
     main = "Histogram",
     xlim = c(-8, 7))
rug(first20$logvala, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logdivfa.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logdivfa,
     breaks = "Scott",
     xlab = "Logarithm of Dividends-Assets Ratio ",
     main = "Histogram",
     xlim = c(-15, 2))
rug(first20$logdivfa, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logdearnsha.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logearnsha,
     breaks = "Scott",
     xlab = "Logarithm of Earnings-Assets Ratio ",
     main = "Histogram",
     xlim = c(-15, 5))
rug(first20$logearnsha, col = rgb(1, 0, 0, 0.3))
dev.off()

png(filename = "logvala.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$logpricef,
     breaks = "Scott",
     xlab = "Logarithm of Valuation ",
     main = "Histogram",
     xlim = c(-8, 7))
rug(first20$logvala, col = rgb(1, 0, 0, 0.3))
dev.off()

################################################################################

################################################################################

## Dependent variable transformation check

## Unconditional regressors
mod <- lm(logvaluesh.new ~ 1, data = first20 )
bc.mod <- boxCox(mod, family = "yjPower")
bc.mod$x[bc.mod$y == max(bc.mod$y)] #1.232

## LSDV with dummies
lsdv1 <- lm(valueshone ~ valueshlag + rndaone + advaone + debtaone + invaone + earnshalag + divfaone + factor(cusip) + factor(year), data = first20)
bc1 <- boxCox(lsdv1)
bc1$x[bc1$y == max(bc1$y)] #0.2222


lsdv2 <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new+ logadva.new + logdebta.new + loginva.new + log(earnshalag) +
              logdivfa.new + factor(cusip) + factor(year), data = first20)
bc2 <- boxCox(lsdv2, family = "yjPower")
bc2$x[bc2$y == max(bc2$y)] #1.1111

## Dependent variable transformation using lambda = 1.232
first20$lambda.1232 <- (first20$valueshone^(1.232)-1)/(1.232)
png(filename = "1.232valuesh.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$lambda.1232,
     breaks = "Scott",
     xlab = "Adjusted Value of Shortfall with lambda = 1.232 ",
     main = "Histogram",
     xlim = c(0, 180))
rug(first20$lambda.1232, col = rgb(1, 0, 0, 0.3))
dev.off()

## Dependent variable transformation using lambda = 0.22
first20$lambda.22 <- (first20$valueshone^(0.22)-1)/(0.22)
png(filename = "0.22valuesh.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$lambda.22,
     breaks = "Scott",
     xlab = "Adjusted Value of Shortfall with lambda = 0.22 ",
     main = "Histogram",
     xlim = c(-10, 10))
rug(first20$lambda.22, col = rgb(1, 0, 0, 0.3))
dev.off()

## Dependent variable transformation using lambda = 1.11
first20$lambda1.11 <- (first20$valueshone^(1.11)-1)/(1.11)
png(filename = "1.11valuesh.png", width = 3000, height = 3000, res = 300)
par(cex = 1.5)
hist(first20$lambda1.11,
     breaks = "Scott",
     xlab = "Adjusted Value of Shortfall with lambda = 1.11 ",
     main = "Histogram",
     xlim = c(0, 150))
rug(first20$lambda1.11, col = rgb(1, 0, 0, 0.3))
dev.off()

########## Analysis of only adjusted earnnings without any lag   ############

## Unconditional regressors
mod.nl <- lm(logvaluesh.new ~ 1, data = first20 )
bc.mod.nl <- boxCox(mod.nl, family = "yjPower")
bc.mod.nl$x[bc.mod.nl$y == max(bc.mod.nl$y)] #1.232

## LSDV with dummies
lsdv3 <- lm(valueshone ~ valueshlag + rndaone + advaone + debtaone + invaone + earnshasix + divfaone + factor(cusip) + factor(year), data = first20)
bc.nl1 <- boxCox(lsdv3)
bc.nl1$x[bc.nl1$y == max(bc.nl1$y)] #0.2222


lsdv4 <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new+ logadva.new + logdebta.new + loginva.new + log(earnshasix) +
              logdivfa.new + factor(cusip) + factor(year), data = first20)
bc.nl2 <- boxCox(lsdv4, family = "yjPower")
bc.nl2$x[bc.nl2$y == max(bc.nl2$y)] #1.1111
################################################################################

################################################################################

## Independent Variable transformation check with lagged earnings-assets ratio
library(mgcv)

## GAM withuntrasnformed DV

gam1 <- gam(valuesh ~ s(rnda) + s(adva) + s(inva) + s(debta) + s(divfa) + s(earnshalag) + s(valueshlag) + 
              factor(cusip) + factor(year), data = first20)

# GAM with log transformed DV 
gam2 <- gam(logvaluesh.new ~ s(rnda) + s(adva) + s(inva) + s(debta) + s(divfa) + s(earnshalag) + s(valueshlag) +
              factor(cusip) + factor(year), data = first20)

## Plotting the gam distributions

png(filename = "gam1.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(4,2), cex.lab = 1.5, cex.axis = 1.5)
plot(gam1, residuals = TRUE, shade = TRUE)
dev.off()

png(filename = "gam2.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(4,2), cex.lab = 1.5, cex.axis = 1.5)
plot(gam2, residuals = TRUE, shade = TRUE)
dev.off()

################################################################################
## Independent Variable transformation check without any lag on earnings-assets ratio

## GAM withuntrasnformed DV

gam.nl1 <- gam(valuesh ~ s(rnda) + s(adva) + s(inva) + s(debta) + s(divfa) + s(earnsha) + s(valueshlag) + 
                 factor(cusip) + factor(year), data = first20)

# GAM with log transformed DV 
gam.nl2 <- gam(logvaluesh.new ~ s(rndaone) + s(advaone) + s(invaone) + s(debtaone) + s(divfaone) + s(earnshasix) + s(valueshlag) +
                 factor(cusip) + factor(year), data = first20)


## Plotting the gam distributions

png(filename = "gam.nl1.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(4,2), cex.lab = 1.5, cex.axis = 1.5)
plot(gam.nl1, residuals = TRUE, shade = TRUE)
dev.off()

png(filename = "gam.nl2.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(4,2), cex.lab = 1.5, cex.axis = 1.5)
plot(gam.nl2, residuals = TRUE, shade = TRUE)
dev.off()
################################################################################

################################################################################
### Analysis with lagged earnings-assets ratio variable

## Component + Residual  complete pooling
cp <- lm(logvaluesh.new ~ valueshlag + logrnda.new + logadva.new + logdebta.new + loginva.new + earnshalag + logdivfa.new, data = first20)

png(filename = "stockcp1.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp),
     rstudent(cp),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
#ylim = c(-6, 6),
#xlim = c(4, 6)
lines(lowess(fitted(cp), rstudent(cp)), col = "green", lwd = 2)
qqPlot(cp, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

# Residual plot for complete pooling with firms but time dummies

cp.t <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + logdebta.new + loginva.new + log(earnshalag) + logdivfa.new + factor(year), data = first20)

png(filename = "stockcp2.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp.t),
     rstudent(cp.t),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
#ylim = c(-6, 6),
#xlim = c(4, 6)
lines(lowess(fitted(cp.t), rstudent(cp.t)), col = "green", lwd = 2)
qqPlot(cp.t, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

## Complete pooling with time and firm dummies
cp.ts <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + logdebta.new + loginva.new + log(earnshalag) +
              logdivfa.new + factor(year) + factor(cusip), data = first20)

png(filename = "stockcp3.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp.ts),
     rstudent(cp.ts),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
lines(lowess(fitted(cp.ts), rstudent(cp.ts)), col = "green", lwd = 2)
qqPlot(cp.ts, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

################################################################################

################################################################################

# Analysis without lag transformation on earnings-assets ratio
# complete pooling 
cp.nl <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + logdebta.new + loginva.new
            + log(earnshasix) + logdivfa.new, data = first20)

png(filename = "stockcpnl1.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp.nl),
     rstudent(cp.nl),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
lines(lowess(fitted(cp.nl), rstudent(cp.nl)), col = "green", lwd = 2)
qqPlot(cp.nl, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

# Residual plot for complete pooling with firms but time dummies

cp.nl.t <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + logdebta.new
              + loginva.new + log(earnshasix) + logdivfa.new + factor(year), data = first20)

png(filename = "stockcpnl2.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp.nl.t),
     rstudent(cp.nl.t),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
lines(lowess(fitted(cp.nl.t), rstudent(cp.nl.t)), col = "green", lwd = 2)
qqPlot(cp.nl.t, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

## Complete pooling with time and firm dummies
cp.nl.ts <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + logdebta.new
               + loginva.new + log(earnshasix) + logdivfa.new + factor(year)
               + factor(cusip), data = first20)

png(filename = "stockcpnl3.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 5, 2, 1), mfrow = c(2, 1), cex.lab = 1.5, cex.axis = 1.5)
plot(fitted(cp.nl.ts),
     rstudent(cp.nl.ts),
     xlab = "Fitted Values",
     ylab = "Jackknife Residuals",
     pch = 19,
     col = rgb(0, 0, 0, 0.3))
lines(lowess(fitted(cp.nl.ts), rstudent(cp.nl.ts)), col = "green", lwd = 2)
qqPlot(cp.nl.ts, ylab = "Jackknife Residuals",
       pch = 19,
       col = rgb(0, 0, 0, 0.3))
dev.off()

################################################################################

################################################################################

## Influence plot for time dummies

png(filename = "influenceplot-time.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 6, 2, 1))
influenceIndexPlot(cp.nl.t, id.n = 3)
dev.off()

## Influence plot for time and state dummies

png(filename = "influenceplot.png", width = 3000, height = 3000, res = 300)
par(cex = 1.3, mar = c(5, 6, 2, 1))
influenceIndexPlot(cp.nl.ts, id.n = 3)
dev.off()


################################################################################

################################################################################

## Partial pooling version of the within model
## With partially pooled varying intercepts by year and firm
library(arm)
lmer1 <- lmer(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
              + logdebta.new + logdivfa.new + log(earnshasix) + (1|year) + (1| cusip), data = first20)
summary(lmer1)

################################################################################

################################################################################

## Within model using within transformation on years and states
within1 <- plm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
               + logdebta.new + logdivfa.new + log(earnshasix),
               model = "within", effect = "twoways", data = first20)

################################################################################

################################################################################

# Forecasting for year 1991

# Adjusting middle 60% of the data after receving an error about missing factor levels

## Adjusting
remove <- which(middle60$cusip == "170021")
middle60.new <- middle60[-remove, ]
remove <- which(middle60.new$cusip == "350755")
middle60.new <- middle60.new[-remove, ]
remove <- which(middle60.new$cusip == "523251")
middle60.new <- middle60.new[-remove, ]
remove <- which(middle60.new$cusip == "740459")
middle60.new <- middle60.new[-remove, ]
remove <- which(middle60.new$cusip == "86024C")
middle60.new <- middle60.new[-remove, ]

# Model forecasting by dropping 1991

# Training data drop year 1991

training1 <- middle60.new[!middle60.new$year == 91, ]
OLS <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
          + logdebta.new + logdivfa.new + log(earnshasix), data = training1)
within.2 <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
               + logdebta.new + logdivfa.new + log(earnshasix) +
                 factor(cusip) + factor(year), data = training1)

# Test data include year 1991 but changing the year to 1990 in test data as
# intercept dummy from 1990 will be the best estimnate for intercept dummy for 1992

test1 <-middle60.new[middle60.new$year == 91, ]
test1$year <- 90

# Predictions on test data, calculate the residuals  and rMSE

# Predictions for 1991

OLS.pred <- predict(OLS, newdata = test1)
within2.pred <- predict(within.2, newdata = test1)

# The residuals

OLS.resid <- test1$logvaluesh.new - OLS.pred
within2.resid <- test1$logvaluesh.new - within2.pred

## rMSE
rMSE.OLS <- mean(OLS.resid^2)
rMSE.within2 <- mean(within2.resid^2)

################################################################################

################################################################################

## Analysing errors

## For the vcovHC function to work we need to use a plm object 
within3 <- plm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
               + logdebta.new + logdivfa.new + log(earnshasix), model = "within", effect = "twoways", data = last80)

# Homoskedastic standard errors
homs <- coef(summary(within3))[ ,2]

library(lmtest)

# Heteroskedasticity-robust standard errors
hets <- coeftest(within3, vcov = function(x) vcovHC(x, method = "arellano",
                                                    type = "HC0",
                                                    cluster = "group"))
hets <- hets[, 2]
# Serial-correlation-robust standard errors
serial <- coeftest(within3, vcov = function(x) vcovHC(x,
                                                      method = "arellano",
                                                      type = "HC0",
                                                      cluster = "group"))
serial <- serial[, 2]
hets/homs
serial/homs

# It's also important to compare to the multiwayvcov package
# including the df correction
lsdv.l <- lm(logvaluesh.new ~ log(valueshlag) + logrnda.new + logadva.new + loginva.new
             + logdebta.new + logdivfa.new + log(earnshasix) + factor(year) + factor(cusip),
             data = last80)
library(multiwayvcov)
vcovCL <- cluster.vcov(lsdv.l,
                       last80$state,
                       df_correction = TRUE)

coeftest(lsdv.l, vcovCL)

