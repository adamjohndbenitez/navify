# ---------------------------------------
# 4.1. Self-organizing maps: Function som
# ---------------------------------------

library("kohonen")

#' data frame 177 rows & 13 cols.
data("wines")

#' centers and/or scales the cols of matrix.
wines.sc <- scale(wines)

#' Random Number Generation
#' uses a single integer argument to
#' set as many seeds as required. - returns NULL?
set.seed(seed = 7)

#' Self-organising maps for mapping high-dimensional
#' spectra or patterns to 2D; Euclidean distance is used.
wine.som <- som(data = wines.sc, grid = somgrid(xdim = 6, ydim = 6, topo = "rectangular"))
plot(wine.som, main = "Wine data")
# map(wine.som) # -> returns (unit.classif, dists, whatmap,weights,scale.distances)
# summary(wine.som) # -> returns (unit.classif, dists, whatmap,weights,scale.distances)

# -------------------------------------------
# 4.1. Self-organizing maps: Function predict
# -------------------------------------------
library("kohonen")

data("wines")

set.seed(seed = 7)

training <- sample(nrow(wines), 120)

Xtraining <- scale(wines[training, ])

Xtest <- scale(wines[-training, ], center = attr(Xtraining, "scaled:center"), scale = attr(Xtraining, "scaled:scale"))

som.wines <- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))

som.prediction <- predict(som.wines, newdata = Xtest, trainX = Xtraining, trainY = factor(wine.classes[training]))

# table(wine.classes[-training], som.prediction$prediction)
