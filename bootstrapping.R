### validation
install.packages("rsample")
library(rsample)

mc_cv(mtcars, times = 2)
mc_cv(mtcars, prop = .5, times = 2)

library(purrr)
iris2 <- iris[1:130, ]

set.seed(13)
resample1 <- mc_cv(iris2, times = 3, prop = .5)
map_dbl(resample1$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })

set.seed(13)
resample2 <- mc_cv(iris2, strata = "Species", times = 3, prop = .5)
map_dbl(resample2$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })

set.seed(13)
resample3 <- mc_cv(iris2, strata = "Sepal.Length", breaks = 6, times = 3, prop = .5)
map_dbl(resample3$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })

library(boot)

set.seed(231241)
mean_df <- function(dataset, i) mean(dataset[i, "mpg"])
res <- boot(mtcars, mean_df, 999)
res

bootstraps(mtcars, times = 2, apparent = TRUE)

library(purrr)
iris2 <- iris[1:130, ]
range(iris2$Species)
head(iris)
set.seed(13)
resample1 <- bootstraps(iris2, times = 3)
map_dbl(resample1$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })

set.seed(13)
resample2 <- bootstraps(iris2, strata = "Species", times = 3)
map_dbl(resample2$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })

set.seed(13)
resample3 <- bootstraps(iris2, strata = "Sepal.Length", breaks = 6, times = 3)
map_dbl(resample3$splits,
        function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        })


## sas models
# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

## predictive models
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results, index=1) # intercept
plot(results, index=2) # wt
plot(results, index=3) # disp

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp
