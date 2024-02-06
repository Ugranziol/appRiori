library(appRiori)
library(hypr)
testthat::context("hyper contrast in R")
tol=.001
# test
data<-datasets::ToothGrowth
data$dose<-factor(data$dose)
levels(data$dose)<-c("low","medium","high")
h <- hypr( ~1 * low - medium,~1/2 * low + 1/2 * medium - high, levels = c("low", "medium", "high"))
contrasts(data$dose,how.many=2)<-cmat(h)
cc1<-contrasts(data$dose)

testthat::test_that("cmat works", {
    testthat::expect_contains(attr(cc1,"fracs"),"1/2")
    testthat::expect_equal(dimnames(cc1)[[1]][1],"low")
})

model1<-lm(len~supp*dose,data=data)
summary(model1)

res1<-contrasts_summary(model1)

testthat::test_that("oneway works", {
  testthat::expect_contains(rownames(res1$dose),"~low - medium")
  testthat::expect_equal(res1$dose[1,1],-9.47,tol)
})

h <- hypr( ~ OJ - VC)
contrasts(data$supp)<-cmat(h)
model2<-lm(len~supp*dose,data=data)
res2<-contrasts_summary(model2)
cc2<-contrasts(data$supp)

testthat::test_that("cmat works 2", {
  testthat::expect_contains(attr(cc2,"fracs"),"1/2")
  testthat::expect_equal(dimnames(cc2)[[1]][1],"OJ")
})

testthat::test_that("twoway works", {
  testthat::expect_contains(rownames(res2$supp),"~OJ - VC")
  testthat::expect_equal(res2$supp[1,1],3.7,tol)
})


h <- hypr(~1/2 * low + 1/2 * medium - high, levels = c("low", "medium", "high"))
contrasts(data$dose,how.many=2)<-cmat(h)
cc3<-contrasts(data$dose)

model3<-lm(len~supp*dose,data=data)
contrasts_summary(model3)

library(lme4)
data("sleepstudy")
sleepstudy$cDays<-cut(sleepstudy$Days,3)
levels(sleepstudy$cDays)<-c("low","medium","high")

h <- hypr(~1/2 * low + 1/2 * medium - high, levels=c("low","medium","high"))
contrasts(sleepstudy$cDays,how.many=2)<-cmat(h)
contrasts(sleepstudy$cDays)
mod<-lme4::lmer(Reaction ~ cDays + (cDays | Subject), sleepstudy)
contrasts_summary(mod)

set.seed(101)
dd <- expand.grid(f1 = factor(1:3),
                  f2 = LETTERS[1:2], g=1:9, rep=1:15,
                  KEEP.OUT.ATTRS=FALSE)
summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)

require("MASS")## and use its glm.nb() - as indeed we have zero random effect:
## Not run:
levels(dd$f1)<-c("low","medium","high")

h <- hypr(~1/2 * low + 1/2 * medium - high, levels=c("low","medium","high"))
contrasts(dd$f1,how.many=2)<-cmat(h)
class(contrasts(dd$f1))
m.glm <- glm.nb(y ~ f1*f2, data=dd, trace=TRUE)
contrasts_summary(m.glm)
summary(m.glm)
m.nb <- glmer.nb(y ~ f1*f2 + (1|g), data=dd, verbose=TRUE)
terms(m.nb)
contrasts_summary(m.nb)
class(m.nb)
