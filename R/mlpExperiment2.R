library(datasets)

names(infert)

library(neuralnet)

nn <- neuralnet(
 case~age+parity+induced+spontaneous,
 data=infert, hidden=2, err.fct="ce",
 linear.output=FALSE)

names(nn)

nn$result.matrix

out <- cbind(nn$covariate,nn$net.result[[1]])

dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))

head(out)

head(nn$generalized.weights[[1]])

plot(nn)
