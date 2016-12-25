# -----------------------------------------
# 4.2. Supervised mapping: The xyf function
# -----------------------------------------

library("kohonen")
data("nir")
attach(nir)
set.seed(13)
# nir.xyf <- xyf(data = spectra, Y = composition[,2], xweight = 0.5, grid = somgrid(6, 6, "hexagonal"))
nir.xyf2 <- xyf(data = spectra, Y = classvec2classmat(temperature), xweight = .2, grid = somgrid(6, 6, "hexagonal"))
par(mfrow = c(1, 2))
plot(nir.xyf, type = "counts", main = "NIR data: counts")
plot(nir.xyf, type = "quality", main = "NIR data: mapping quality")
