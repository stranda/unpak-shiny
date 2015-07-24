library(shiny)
library(xtable)
library(unpakR)
options(bitmapType='cairo')
type <- getOption("bitmapType")
phenotypes.to.exclude <- c("FruitLength1","FruitLength2","FruitLength3","FruitLength4",
                           "FruitLength5","FruitLength6","FruitLength7","FruitLength8",
                           "germinants.31d","germinants.41d","germinants.7d","germinants.14d",
                           "seeds.sown","basalfruit.length","midfruit.length","upperfruit.length")

