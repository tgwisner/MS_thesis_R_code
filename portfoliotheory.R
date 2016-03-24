library(stockPortfolio)

# Funds (change to indices?)
# vbmfx - US bond
# vtiax - int'l stock
# vtsmx - US stock

returns <- getReturns(c("VBMFX", "VTIAX", "VTSMX"), freq = "day", get = "overlapOnly")
model <- stockModel(returns, freq="day")
mvp <- optimalPort(model, Rf=0)

portPossCurve(model, riskRange = 6, add=FALSE, xlim = c(.0005, .012), main = "Efficient Frontier and MVP")
points(mvp, addNames=TRUE)
portCloud(model, riskRange = 10, N=10000, subSamp=2000, add=TRUE, pch=".", col="black")
abline(a=0, b=.1250448)
