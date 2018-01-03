require(vcd)
require(MASS)

j1 <- as.numeric(na.omit(d$jitter1))
fit1 <- fitdistr(j1,"exponential")

j2 <- as.numeric(na.omit(d$jitter2))
fit2 <- fitdistr(j2,"exponential")
