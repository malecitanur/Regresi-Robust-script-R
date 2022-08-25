#input data
pidana=read.csv(file.choose(),header = TRUE)
pidana
summary(pidana)

#mkt
ols = lm(y ~ x1+x2, data = pidana)
summary(ols)

##asumsi klasik
#uji normalitas
shapiro.test(ols$residuals)
#uji homoskedastisitas
library(lmtest)
bptest(ols, studentize=FALSE, data=pidana)
#uji autokorelasi
library(lmtest)
dwtest(ols)
#multikolinieritas
library(car)
vif(ols)

#deteksi outlier
library(olsrr)
ols_plot_cooksd_bar(ols)

##reg robust
#estimasi m
library(MASS)
Mhuber = rlm(y~x1+x2, data = pidana, method = "M", maxit=40)
print(Mhuber)
summary(Mhuber)
library(sfsmisc)
f.robftest(Mhuber, var = "x1")  
f.robftest(Mhuber, var = "x2")
#estimasi s
library(robustbase)
estimasi.S = lmrob(y~x1+x2, data = pidana, method = "S")
summary(estimasi.S)
#estimasi mm
library(robustbase)
estimasi.MM = lmrob(y~x1+x2, data = pidana, method = "MM")
summary(estimasi.MM)