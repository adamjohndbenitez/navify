# ---------------------------------------
# 4.1. Self-organizing maps: Function som
# ---------------------------------------

library("kohonen")
data("wines")
wines.sc <- scale(wines)
set.seed(7)
wine.som <- som(data = wines.sc, grid = somgrid(5, 4, "hexagonal"))
plot(wine.som, main = "Wine data")