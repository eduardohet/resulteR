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
# t-tests
res <- t.test(extra ~ group, data = sleep)
tText(res)
"t = -1.861; df = 17.8; P = 0.079"
tText(res, dec=",")
"t = -1,861; df = 17,8; P = 0,079"

# Regressions
utils::data(anorexia, package = "MASS")
anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
lmText(anorex.1)
"R^2^ = 0.11; F ~1,~ ~70~ = 8.695; P = 0.004"

plot(Postwt ~ Prewt, data = anorexia)
plotlmText(anorex.1, h=0.15, v=0.8, pos=4)

# Correlations
res <- cor.test(~Postwt + Prewt, data=anorexia)
corText(res)
res <- cor.test(~Postwt + Prewt, data=anorexia, method="spearman")
corText(res)
res <- cor.test(~Postwt + Prewt, data=anorexia, method="kendall")
corText(res)

# Descriptive statistics
x <- rnorm(50, mean=10, sd=20)
meansdText(x)
"7.6 &plusmn; 18.7"
meansdText(x, digits=c(3, 3))
"7.625 &plusmn; 18.699"

# Neat lists 
# Hint: useful to grab names of significant parameters from a model result
x <- c("apples", "oranges", "grapes")
tidyList(x)
tidyList(c(2.3, 5.4, 3), dec=",", last="&")
```