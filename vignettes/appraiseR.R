## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=FALSE, message = FALSE, warning = FALSE, 
                      dev = "png", dpi = 600, fig.height = 4.5, fig.width = 7,
                      fig.path = "./", fig.align = "center", out.width = "90%")
library(appraiseR)
library(leaflet)

## ----help, echo=TRUE, eval=FALSE-----------------------------------------
#  help(centro_2015)

## ----outliers, message=TRUE----------------------------------------------
outlier_analysis(x = trindade, criterion = "30_percent")
outlier_analysis(x = trindade, criterion = "2_sd")
outlier_analysis(x = trindade, criterion = "chauvenet")

## ----best_fit------------------------------------------------------------
library(appraiseR)
data <- as.data.frame(centro_2015@data)
best_fit <- bestfit(valor~., data)

## ----print---------------------------------------------------------------
print(best_fit)

## ----print2, eval = FALSE------------------------------------------------
#  print(best_fit, n = 20)

## ----summary-------------------------------------------------------------
s <- summary(best_fit, fit = 1)
s

## ----outlier-------------------------------------------------------------
car::outlierTest(s$fit)

## ----summary2------------------------------------------------------------
best_fit <- update(best_fit, subset = -31)
summary(best_fit)

## ----plotdf, fig.cap = "plotdf output."----------------------------------
plotdf(valor~., data)

## ----plotmod, fig.cap = "plotmod output."--------------------------------
pl <- plotmod(best_fit, interval = "confidence", level = 0.80)
pl

## ----plotmod2, fig.cap = "plotmod output with local argument specified"----
plotmod(best_fit, interval = "confidence", level = 0.80, 
        local = list(area_total = 205, quartos = 3, suites = 1, garagens = 2, 
                     dist_b_mar = 250, padrao = "medio"))

## ----map, fig.cap = "gen_map output."------------------------------------
gen_map(centro_2015)

## ----predict-------------------------------------------------------------
p <- predict(best_fit, interval = "confidence", level = 0.80)
p

