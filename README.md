# resulteR
ResulteR is a set of functions that generate strings of text for the use in markdown documents. 

## Installing
```
install.packages("devtools")
library(devtools)
install_github("eduardohet/resulteR")
library(resulteR)
```

## Testing
```
res <- t.test(extra ~ group, data = sleep)
t.text(res)
# "t = -1.861; df = 17.8; P = 0.079"
t.text(res, dec=",")
# "t = -1,861; df = 17,8; P = 0,079"

utils::data(anorexia, package = "MASS")
anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
lm.text(anorex.1)
# "R^2^ = 0.11; F ~1,~ ~70~ = 8.695; P = 0.004"

x <- rnorm(50, mean=10, sd=20)
mean.sd.text(x)
# "7.6 &plusmn; 18.7"
mean.sd.text(x, digits=c(3, 3))
"7.625 &plusmn; 18.699"
```
