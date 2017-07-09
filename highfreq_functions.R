head(PriceByRegularSession$GMP)

x <- PriceByRegularSession$GMP


x <- x[!is.na(x)]


plot(x)

ob <- rAccumulation(x, period = 1,  align.by="minutes", align.period = 60, 
                    plotit = T, cts = F, makeReturns = T)

ob1 <- rAVGCov(x, cor = T, period = 1, align.by = "minutes", 
               align.period = 60, cts = F, makeReturns = T)

rCov <- rCov(x, cor = F, align.by="minutes", align.period = 60, makeReturns = T)

rCumSum(x, period = 1, align.by="minutes", align.period = 60, 
        plotit = T, type = "l", cts = F, makeReturns = T)

rKurt(x, align.by = "minutes", align.period = 60, makeReturns = T)

rMarginal(x, period = 5, align.by = "minutes", align.period = 60,
          plotit = T, cts = F, makeReturns = T)

rMPV(x, m = 2, p = 2, align.by = "minutes", align.period = 60, makeReturns = T)

rQPVar(x, align.by = "minutes", align.period = 60, makeReturns = T)

rQuar(x, align.by = "minutes", align.period = 60, makeReturns = T)

rSkew(x, align.by = "minutes", align.period = 60, makeReturns = T)

rSV(x, align.by = "minutes", align.period = 60, makeReturns = T)

d <- rZero(x, period = 10, align.by = "minutes", align.period = 60,
       cts = F, makeReturns = T)

MarketOpenRegularSession  <- "10:00:00"
MarketCloseRegularSession <- "18:40:00"

init = list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.007,
            sigma_k = 0.06, phi = 0.194, rho = 0.986, mu = c(1.87,-0.42),
            delta_c = c(0.25, -0.05, -0.2, 0.13, 0.02), 
            delta_s = c(-1.2, 0.11, 0.26, -0.03, 0.08) )

sv <- spotvol(x, method = "detper", on = "minutes", k = 5,
        marketopen = "7:00:00", 
        marketclose = "15:40:00",
        tz = "GMT")

require(FKF)

vol2 <- spotvol(x, method = "stochper", init = init)

data(sample_real5minprices)
data(sample_returns_5min)

h1   <- bw.nrd0((1:nrow(sample_returns_5min))*(5*60))
vol3 <- spotvol(sample_returns_5min, method = "kernel", h = h1)
vol4 <- spotvol(sample_returns_5min, method = "kernel", est = "quarticity")
vol5 <- spotvol(sample_returns_5min, method = "kernel", est = "cv")
plot(vol3, length = 2880)
lines(as.numeric(t(vol4$spot))[1:2880], col = "red")
lines(as.numeric(t(vol5$spot))[1:2880], col = "blue")
legend("topright", c("h = simple estimate", "h = quarticity corrected",
                     "h = crossvalidated"), col = c("black", "red", "blue"), lty = 1)

vol7 <- spotvol(sample_returns_5min, method = "garch", model = "sGARCH")
vol8 <- spotvol(sample_returns_5min, method = "garch", model = "eGARCH")
plot(as.numeric(t(vol7$spot)), type = "l")
lines(as.numeric(t(vol8$spot)), col = "red")
legend("topleft", c("GARCH", "eGARCH"), col = c("black", "red"), lty = 1)

data(sbux.xts)
data(lltc.xts)
par(mfrow=c(2,1))
rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,
                ylab="LLTC",xlab="SBUX",numbers=FALSE)

rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,
                ylab="LLTC",xlab="SBUX",numbers=TRUE)


head(sbux.xts)


