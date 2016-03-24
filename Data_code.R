# Read in sector data

xlb <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLB.csv")
xle <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLE.csv")
xlf <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLF.csv")
xli <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLI.csv")
xlk <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLK.csv")
xlp <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLP.csv")
xlu <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLU.csv")
xlv <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLV.csv")
xly <- read.csv("~/Desktop/School/Thesis_materials/Data/Sector_indices/XLY.csv")

sectors <- data.frame(xlb[,1], xlb[,7], xle[,7], xlf[,7], xli[,7], xlk[,7], xlp[,7], xlu[,7], xlv[,7], xly[,7])
names(sectors) = c("Date", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
sectors <- sectors[order(sectors$Date),]
rownames(sectors) <- 1:4158


# Read in int'l data

asx <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/ASX200AUD.csv")
asx <- asx[,c(1,7)]
names(asx) = c("Date", "ASX")
tsc <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/TSCCompCAD.csv")
tsc <- tsc[,c(1,7)]
names(tsc) = c("Date", "TSC")
int_indices <- merge(asx, tsc, by = "Date", all = TRUE)
cac <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/CAC40EUR.csv")
cac <- cac[,c(1,7)]
names(cac) = c("Date", "CAC")
int_indices <- merge(int_indices, cac, by = "Date", all = TRUE)
dax <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/DAXEUR.csv")
dax <- dax[,c(1,7)]
names(dax) = c("Date", "DAX")
int_indices <- merge(int_indices, dax, by = "Date", all = TRUE)
stoxx <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/STOXX50EUR.csv")
stoxx <- stoxx[,c(1,7)]
names(stoxx) = c("Date", "STOXX50")
int_indices <- merge(int_indices, stoxx, by = "Date", all = TRUE)
ftse <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/FTSE100GBP.csv")
ftse <- ftse[,c(1,7)]
names(ftse) = c("Date", "FTSE")
int_indices <- merge(int_indices, ftse, by = "Date", all = TRUE)
hangseng <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/HangSengHKD.csv")
hangseng <- hangseng[,c(1,7)]
names(hangseng) = c("Date", "HangSeng")
int_indices <- merge(int_indices, hangseng, by = "Date", all = TRUE)
bovespa <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/IBOVESPABRL.csv")
bovespa <- bovespa[,c(1,7)]
names(bovespa) = c("Date", "BOVESPA")
int_indices <- merge(int_indices, bovespa, by = "Date", all = TRUE)
ipc <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/IPCMXN.csv")
ipc <- ipc[,c(1,7)]
names(ipc) = c("Date", "IPC")
int_indices <- merge(int_indices, ipc, by = "Date", all = TRUE)
nikk <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/Nikkei225.csv")
nikk <- nikk[,c(1,7)]
names(nikk) = c("Date", "Nikkei")
int_indices <- merge(int_indices, nikk, by = "Date", all = TRUE)
russ <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/Russell2000.csv")
russ <- russ[,c(1,7)]
names(russ) = c("Date", "Russ2K")
int_indices <- merge(int_indices, russ, by = "Date", all = TRUE)
sp500 <- read.csv("~/Desktop/School/Thesis_materials/Data/International_indices/S&P500.csv")
sp500 <- sp500[,c(1,7)]
names(sp500) = c("Date", "SP500")
int_indices <- merge(int_indices, sp500, by = "Date", all = TRUE)
int_indices <- int_indices[219:5709,]
rownames(int_indices) <- 1:5491
int_indices <- int_indices[719:5491,]
rownames(int_indices) <- 1:4773
library(lubridate)
int_indices$Date <- ymd(int_indices$Date)

# Read in FX data

fxrates <- read.csv("~/Desktop/School/Thesis_materials/Data/fxrates.csv")
fxrates <- fxrates[,c(2, 4:11)]
names(fxrates) <- c("Date", "CAD", "EUR", "GBP", "JPY", "AUD", "BRL", "HKD", "MXN")

temp <- mdy(fxrates[,1])
foo <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}
fxrates[,1] <- foo(temp)
fxrates <- fxrates[order(fxrates$Date),]
fxrates <- fxrates[731:5470,]
rownames(fxrates) <- 1:4740
fxrates <- fxrates[1:4720,]



# Convert to USD-denominated and clean up

temp <- int_indices[!is.na(int_indices$SP500),]
combined <- merge(temp, fxrates, by = "Date", all.x = TRUE)
sum_na <- function(x) sum(is.na(x))
apply(combined, 2, sum_na)

for (i in 14:21){
  for(j in 1:4648){
    if(is.na(combined[j,i])){
      combined[j,i] = (combined[j-1, i] + combined[j+1,i])/2
    }
  }
}

for (i in 14:21){
  combined[2072, i] = (2*combined[2071, i] + combined[2074, i])/3
  combined[2073, i] = (combined[2071, i] + 2*combined[2074, i])/3
}

int_indices_usd <- data.frame(combined$Date, combined$ASX*combined$AUD, combined$TSC*combined$CAD, combined$CAC*combined$EUR, combined$DAX*combined$EUR, combined$STOXX50*combined$EUR, combined$FTSE*combined$GBP, combined$HangSeng*combined$HKD, combined$BOVESPA*combined$BRL, combined$IPC*combined$MXN, combined$Nikkei*combined$JPY, combined$Russ2K, combined$SP500)
names(int_indices_usd) <- c("Date", "ASX", "TSC", "CAC", "DAX", "STOXX50", "FTSE", "HangSeng", "BOVESPA", "IPC", "Nikkei", "Russ2K", "SP500")

int_indices_usd <- int_indices_usd[1:4625,]
apply(int_indices_usd, 2, sum_na)



