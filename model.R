library(GGally)
library(naniar)
library(readxl)
library(ggplot2)
library(car)
library(caret)
library(leaps)
library(multcomp)
library(knitr)
# read in original data
copd <- read_excel("copd_data.xlsx")
head(copd)

# remove unwanted variables and store regression dataset
copd <- copd[, !(names(copd) %in% c('sid', 'visit_year', 'visit_date'))]
head(copd)

# choose the variables of interest
# remove the unreasonable values -1 from the interested variables
copd_re <- copd %>% replace_with_na(replace = list(Duration_Smoking = -1, hr = -1,                      
                     FEV1 = -1,functional_residual_capacity = -1, pct_emphysema = -1 ))

dat <- na.omit(copd_re)
nrow(copd)
nrow(dat)

# display the first few lines of cleaned dataset and dataset summary table
head(dat)
summary(dat)

# convert the categorical variable
dat$gender<- factor(dat$gender)
class(dat$gender)
levels(dat$gender)

# use ggpairs to show the correlation and distribution of all interested variables
options(repr.plot.width=14, repr.plot.height=10)
plot_frame <- data.frame("pct_emphysema" = dat$pct_emphysema,
                         "FEV1" = dat$FEV1,
                         "smok_duration" = dat$Duration_Smoking,
                         "pct_gastrapping" = dat$pct_gastrapping,
                         "func_resid_cap" = dat$functional_residual_capacity,
                         "visit_age" = dat$visit_age,
                         "tot_lung_cap" = dat$total_lung_capacity,
                         "bmi" = dat$bmi, 
                         "gender" = dat$gender)
ggpairs(plot_frame,mapping = ggplot2 ::aes(color=gender, alpha=100, binwidth = 30))+ 
  theme(text = element_text(size = 15))

# histogram of pct_emphysema
hist(dat$pct_emphysema, main = "Histogram of Pct_Emphysema", 
     xlab = "pct_emphysema", breaks = 30,freq = FALSE)
lines(density(dat$pct_emphysema), col = "blue")
# box-cox for transformation of response variable
fit0 <- lm(pct_emphysema~Duration_Smoking, data=dat)
bc = boxCox(fit0)
pow <- bc$x[which.max(bc$y)]
pow 

# histogram of pct_gastrapping and functional_residual_capacity
hist(dat$pct_gastrapping, main = "Histogram of Pct_Gastrapping",
     xlab = "Pct_gastrapping", breaks = 30, freq = FALSE)
lines(density(dat$pct_gastrapping), col = "blue")
hist(dat$functional_residual_capacity, main = "Functional Residual Capacity", 
     xlab = "functianl_resiaudl_capacity", breaks = 30, freq = FALSE)
lines(density(dat$functional_residual_capacity), col = "blue")

# display the histogram of transformed variables
hist(log(dat$pct_emphysema), main = "Log_Pct_Emphysema", 
     xlab = "log_pct_emphysema", breaks = 30, freq = FALSE)
lines(density(log(dat$pct_emphysema)), col = "blue")
hist(log(dat$pct_gastrapping), main = "Log_Pct_Gastrapping", 
     xlab = "log_pct_gas", breaks = 30, freq = FALSE)
lines(density(log(dat$pct_gastrapping)), col = "blue")
hist(log(dat$functional_residual_capacity), main = "Log_Func_Resid_Cap",
     xlab = "log_func_resid_cap", breaks = 30, freq = FALSE)
lines(density(log(dat$functional_residual_capacity)), col = "blue")

# relationship between response variable and predictors
# continuous variable: FEV1, Duration_Smoking, pct_gastrapping, functional_residual_capacity, total_lung_capacity, visit_age, bmi

# FEV1
par(mfrow = c(2,2))
hist(dat$FEV1, breaks = 30, freq = FALSE)
lines(density(dat$FEV1), col="blue")
plot(dat$FEV1, log(dat$pct_emphysema), xlab = "FEV1",
     ylab = "log-pct_emphysema")
lines(lowess(dat$FEV1,log(dat$pct_emphysema)),col = "blue")

# Duration_Smoking
par(mfrow = c(2,2))
hist(dat$Duration_Smoking, breaks = 30, freq = FALSE)
lines(density(dat$Duration_Smoking), col="blue")
plot(dat$Duration_Smoking, log(dat$pct_emphysema), 
     xlab = "Duration_Smoking", ylab = "log-pct_emphysema")
lines(lowess(dat$Duration_Smoking,log(dat$pct_emphysema)),col = "blue")

# pct_gastrapping
par(mfrow = c(2,2))
hist(log(dat$pct_gastrapping), breaks = 30, freq = FALSE)
lines(density(log(dat$pct_gastrapping)), col="blue")
plot(log(dat$pct_gastrapping), log(dat$pct_emphysema), 
     xlab = "log-pct_gastrapping", ylab = "log-pct_emphysema")
lines(lowess(log(dat$pct_gastrapping),log(dat$pct_emphysema)),col = "blue")

# functional_residual_capaciy
par(mfrow = c(2,2))
hist(log(dat$functional_residual_capacity), breaks = 30, freq = FALSE)
lines(density(log(dat$functional_residual_capacity)), col="blue")
plot(log(dat$functional_residual_capacity), log(dat$pct_emphysema), 
     xlab = "log-func_resid_cap", ylab = "log-pct_emphysema")
lines(lowess(log(dat$functional_residual_capacity),log(dat$pct_emphysema)),col = "blue")

# total_lung_capacity
par(mfrow = c(2,2))
hist(dat$total_lung_capacity, breaks = 30, freq = FALSE)
lines(density(dat$total_lung_capacity), col="blue")
plot(dat$total_lung_capacity, log(dat$pct_emphysema), xlab = "total_lung_capacity",
     ylab = "log-pct_emphysema")
lines(lowess(dat$total_lung_capacity,log(dat$pct_emphysema)),col = "blue")

# visit_age
par(mfrow = c(2,2))
hist(dat$visit_age, breaks = 30, freq = FALSE)
lines(density(dat$visit_age), col="blue")
plot(dat$visit_age, log(dat$pct_emphysema), xlab = "visit_age",
     ylab = "log-pct_emphysema")
lines(lowess(dat$visit_age,log(dat$pct_emphysema)),col = "blue")

# bmi
par(mfrow = c(2,2))
hist(dat$bmi, breaks = 30, freq = FALSE)
lines(density(dat$bmi), col="blue")
plot(dat$bmi, log(dat$pct_emphysema), xlab = "bmi", ylab = "log-pct_emphysema")
lines(lowess(dat$bmi,log(dat$pct_emphysema)),col = "blue")

# categorical variable gender 
plot(dat$gender, log(dat$pct_emphysema), xlab = "gender", ylab = "log-pct_emphysema")

# use ggpairs to show the correlation and distribution of all interested variables
options(repr.plot.width=14, repr.plot.height=10)
plot_frame <- data.frame("log_emphysema" = log(dat$pct_emphysema),
                         "FEV1" = dat$FEV1,
                         "smok_duration" = dat$Duration_Smoking,
                         "log_gastrapping" = log(dat$pct_gastrapping),
                         "log_func_resid" = log(dat$functional_residual_capacity),
                         "visit_age" = dat$visit_age,
                         "tot_lung_cap" = dat$total_lung_capacity,
                         "bmi" = dat$bmi, 
                         "gender" = dat$gender)
ggpairs(plot_frame,mapping = ggplot2 ::aes(color=gender, alpha=100, binwidth = 30))+ 
  theme(text = element_text(size = 15))

# variable transformation 
dat$pct_emphysema <- log(dat$pct_emphysema)
dat$pct_gastrapping <- log(dat$pct_gastrapping)
dat$functional_residual_capacity <- log(dat$functional_residual_capacity)

#best subset selection
preds <- with(dat, cbind(FEV1,
                         Duration_Smoking,
                         pct_gastrapping,
                         functional_residual_capacity,
                         total_lung_capacity,
                         visit_age,
                         bmi,
                         gender))
model <- regsubsets(preds, y = dat$pct_emphysema,
                    nbest = 30,    # save the best # for each number of variables
                    nvmax = 20,    # maximum number of variables allowed in the model
                    really.big=T)  # for larger datasets       

plot(model, scale = "bic", main = "Variable inclusion plot by BIC")
plot(model, scale = "Cp", main = "Variable inclusion plot by Cp")

plot_best <- function(number_variables, crit, func) {
  
  # plot criteria vs number of variables
  plot(number_variables, model_summ[[crit]], 
       xlab = "number of variables", ylab = crit,
       cex.lab = 2)
  
  # plot line for visualization
  lines(unique(number_variables), tapply(model_summ[[crit]], number_variables, match.fun(func)))
}

model_summ <- summary(model)  # store the summary of the best subsets

# plot criteria vs number of variables
number_variables <- apply(model_summ$which, 1, sum)
plot_best(number_variables, 'rss', 'min')
plot_best(number_variables, 'rsq', 'max')
plot_best(number_variables, 'adjr2', 'max')
plot_best(number_variables, 'cp', 'min')
plot_best(number_variables, 'bic', 'min')
criteria <- data.frame(p = number_variables,
                       rss = model_summ$rss, 
                       rsq = model_summ$rsq, 
                       adjr2 = model_summ$adjr2, 
                       cp = model_summ$cp,
                       bic = model_summ$bic)

# shows all criteria and included variables ordered by bic
criteria <- cbind(criteria, model_summ$outmat)
print(head(criteria[order(criteria$bic),]))

plot(model, scale = "bic")
plot(model, scale = "Cp")

#step wise selection#
fit <- lm(pct_emphysema~FEV1+
            bmi+
            Duration_Smoking+
            pct_gastrapping +
            total_lung_capacity+
            visit_age+
            functional_residual_capacity+
            gender, data=dat)
stepAIC(fit, direction = "both")

# model A
fit1 <- lm(pct_emphysema~FEV1+
             bmi+
             pct_gastrapping +
             total_lung_capacity+
             visit_age+
             functional_residual_capacity+
             gender, data=dat)

summary(fit1)
plot(fit1, which=c(1,2,3,4,5))

# model B
fit2 <- lm(pct_emphysema~FEV1+
             Duration_Smoking+
             bmi+
             pct_gastrapping +
             total_lung_capacity+
             visit_age+
             functional_residual_capacity+
             gender, data=dat)

summary(fit2)
plot(fit2, which=c(1,2,3,4,5))
# anova test for Duration_Smoking
fit_full <- lm(pct_emphysema~FEV1+
                 Duration_Smoking+
                 bmi+
                 pct_gastrapping +
                 total_lung_capacity+
                 visit_age+
                 functional_residual_capacity+
                 gender, data=dat)

fit_reduced <- lm(pct_emphysema~FEV1+
                    bmi+
                    pct_gastrapping +
                    total_lung_capacity+
                    visit_age+
                    functional_residual_capacity+
                    gender, data=dat)
anova(fit_reduced, fit_full)
# final model diagnostics and predicting performance
fit <- lm(pct_emphysema~FEV1+
            bmi+
            pct_gastrapping +
            total_lung_capacity+
            visit_age+
            functional_residual_capacity+
            gender, data=dat)

summary(fit)

plot(fit, which=c(1,2,5))

# VIF for each predictor to check multicollinearity
print(vif(fit))
print(paste("max VIF:", max(vif(fit))))
print(paste("mean VIF:", mean(vif(fit))))

# creating and printing the inferences of the model

betahat <- formatC(signif(fit$coeff,digits=6), digits=3, format="f", flag="#")
SE <- formatC(signif(summary(fit)$coeff[,2],digits=6), digits=3,
              format="f", flag="#")
print(betahat)
print(SE)

ci <- formatC(signif(confint(fit),digits=6), digits=3,
              format="f", flag="#")
print(ci)

# Bonferroni corrected ci
alpha <- 0.05
g <- 6
ci <- confint(fit, parm = c("FEV1", "bmi", "pct_gastrapping", "total_lung_capacity", "visit_age", 
                            "functional_residual_capacity"), level = 1 - alpha / g)
print(ci)

