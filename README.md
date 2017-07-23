# resulteR

ResulteR is a set of functions that generate strings of text for the use in markdown documents. It aims to help you build top level reproducible documents.

## Installing

```
install.packages("devtools")
library(devtools)
install_github("eduardohet/resulteR")
library(resulteR)
```

## Try it!

```
res <- t.test(extra ~ group, data = sleep)
t.text(res)
"t = -1.861; df = 17.8; P = 0.079"
t.text(res, dec=",")
"t = -1,861; df = 17,8; P = 0,079"

utils::data(anorexia, package = "MASS")
anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
lm.text(anorex.1)
"R^2^ = 0.11; F ~1,~ ~70~ = 8.695; P = 0.004"

plot(Postwt ~ Prewt, data = anorexia)
plot.lm.text(anorex.1, h=0.15, v=0.8, pos=4)

x <- rnorm(50, mean=10, sd=20)
mean.sd.text(x)
"7.6 &plusmn; 18.7"
mean.sd.text(x, digits=c(3, 3))
"7.625 &plusmn; 18.699"

x <- c("apples", "oranges", "grapes")
tidy.list(x)
tidy.list(c(2.3, 5.4, 3), dec=",")
```