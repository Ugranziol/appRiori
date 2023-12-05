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
summary(model3)

contrasts_summary(model3)
