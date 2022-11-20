## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(LM)
library(knitr)
library(bench)

## ----SLM example--------------------------------------------------------------
##Load dataset
library(datasets)
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg

fit = SLM(x,y)
fit

## ----SLM acc------------------------------------------------------------------
b <- summary(lm(y~x))
lmfit <- c(b$coefficients[1,1], b$coefficients[2,])
tab <- cbind(unlist(fit), lmfit)
colnames(tab) <- c("Outputs from LM", "Outputs from BASE")
kable(tab, caption = "Simple Linear Regression comparison: LM vs BASE")

## ----SLM Eff, fig.height = 3, fig.width = 10, fig.align = "center"------------
LM.SLM <- unlist(SLM(x, y))
Base.lm <-  c(b$coefficients[1,1], b$coefficients[2,])
all.equal(LM.SLM,Base.lm, check.names = F)

p <- mark(unname(LM.SLM),unname(Base.lm))
plot(p)



## ----ML example---------------------------------------------------------------
##Load dataset
x <- as.matrix(mtcars[,c(6,7,2)])
y <- as.vector(mtcars$mpg)
fit = MultiLinearRegression(x,y,int = T)
fit

## ----ML acc-------------------------------------------------------------------
b <- summary(lm(y~x))
lmfit <- c(b$coefficients[,1])
tab <- cbind(unlist(fit$beta.est), lmfit)
colnames(tab) <- c("estimates from LM", "estimates from BASE")
kable(tab, caption = "Multiple Linear Regression comparison: LM vs BASE")

paste0("The fitted value from both LM and Base model are the same: ", all.equal(unname(as.vector(fit$fitted)), unname(as.vector(lm(y~x)$fitted))))

## ----ML Eff, fig.height = 3, fig.width = 10, fig.align = "center"-------------
set.seed(123)
x <- matrix(rnorm(1:1000), 100, 10)
set.seed(321)
y <- rnorm(1:100)
p <- mark(MultiLinearRegression(x,y,int = T), lm(y~x), check = F)
plot(p)


## ----cov ex-------------------------------------------------------------------
##Accuracy Evaluation of OnePassCov()
set.seed(123)
x <- rnorm(1:1000)
set.seed(321)
y <- rnorm(1:1000)
op = OnePassCov(x, y)
op

## ----cov acc------------------------------------------------------------------
##Accuracy Evaluation of OnePassCov()
paste0("The covariance from both LM and Base are the same: ", all.equal(op, cov(x,y)))


## ----cov eff, fig.height = 3, fig.width = 10, fig.align = "center"------------
p = mark(OnePassCov(x,y), cov(x,y))
plot(p)

## ----ridge ex-----------------------------------------------------------------
set.seed(123)
x <- matrix(rnorm(1:500), 100, 5)
set.seed(321)
y <- rnorm(1:100)
beta = RidgeRegression(x, y, lambda = 0.1, int = TRUE)
beta

## ----ridge acc----------------------------------------------------------------
##Accuracy Evaluation of RidgeREgression()
library(MASS)
beta.ml <-  round(as.numeric(coefficients(lm.ridge(y~x, lambda = 0.1))),4)
mat <- cbind(beta,beta.ml)
colnames(mat) <- c("Estimates from LM", "Estimate from MASS")
kable(mat, caption = "Ridge Regression estimation comparison: LM vs MASS")

## ----ridge eff, fig.height = 3, fig.width = 10, fig.align = "center"----------
p <- mark(RidgeRegression(x, y, lambda = 0.1, int = TRUE), lm.ridge(y~x, lambda = 0.1), check = F)
plot(p)

