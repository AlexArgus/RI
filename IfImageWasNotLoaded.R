Position         =  0
#Accumulator     =  0
AccumulatorLong  =  0
AccumulatorShort =  0
CommisionSummator = 0
NumberOfDeals    =  0
StopLoss         =  200
TakeProfit       =  400

Delay = 12.0 # Coefficient of multiplications
Shift = 1.0 # Delay in reaction (sec)
comissions = 1.0 + 1.0  # comissions per round (1 contract) #

c = 1

TechAcL = 0
ProfitOfPositionLong = 0
MaxProfitOfPositionLong = 0
MinProfitOfPositionLong = 0

TechAcS = 0
ProfitOfPositionShort = 0
MaxProfitOfPositionShort = 0
MinProfitOfPositionShort = 0


################# Getting indicative USD Rate from MICEX ftp server ##################

ms          <- paste0('moment_start=', dateOfTrade)

me          <- paste0('moment_end='  , dateOfTrade)

pathUSDRate <- 'http://moex.com/export/derivatives/currency-rate.aspx?language=ru&currency=USD/RUB'

pathUSDRate <- paste0(pathUSDRate,'&', ms,'&', me)

doc         <- xmlParse(pathUSDRate)

rates       <- xmlSApply(getNodeSet(doc, "//rate"), xmlAttrs)

moments     <- rates[1, 1]

value       <- as.numeric(rates[2, 1])


#####################################
SmallInterruptBegin             <- CreateTime('13:59:55')
SmallInterruptEnd               <- CreateTime('14:03:05')
CloseAllPositionTime_2          <- CreateTime('18:25:00')
SessionInterruptBegin           <- CreateTime('18:45:00')
SessionInterruptEnd             <- CreateTime('19:05:00')
StopTradeTime                   <- CreateTime('21:30:00')
NotradeTime                     <- CreateTime('23:30:00')
CloseAllPositionTime            <- CreateTime('23:40:00')
#####################################


Y_60 <-
        aggregatePrice(PriceByRegularSession, 
                       on = "secs",
                       k = k_60, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)

Y_20 <-
        aggregatePrice(PriceByRegularSession, 
                       on = "secs",
                       k = k_20, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)

Price <- 
        aggregatePrice(PriceByRegularSession, 
                       on = "secs",
                       k = 1, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)
        

#######################################

#layout(matrix(c(2,2,1,1), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1, 1))

# setkey(TradeDay, MOMENT)
# 
# TD <- TradeDay[DEAL_ID != ""]
# 
# # setnames(TD, c('VOLUME'), c('SIZE'))
# 
# setnames(TD, c('SIZE'), c('VOLUME') )

############# Roll Volume ############

# Vol <- xts(TD[ , .(PRICE,VOLUME)], order.by = TD[ , MOMENT])
# 
# #plot(Vol$PRICE)
# 
# ep <- endpoints(Vol, on = 'secs', k = 1)
# 
# aggVol <- period.sum(Vol$VOLUME, ep)
# 
# indexV <- round.POSIXt(index(Vol[ep]), units = "sec")
# 
# AggVol <- xts(aggVol,  order.by = indexV)
# 
# window <- 3600L # 1 Hour
# 
# RollVolume <- roll_sum(coredata(AggVol), n = window, by = 1L, fill = numeric(0),
#                        partial = FALSE, align = "right", na.rm = FALSE)
# 
# RollVolume <- xts( RollVolume , order.by = indexV[window:length(AggVol)] )
# 
# indexTZ(RollVolume) <- "Europe/Moscow"
# 
# plot(RollVolume)
# 
# RollVolume <- as.data.table(RollVolume)
# 
# RollVolume[, plot(tail(index(AggVol), n = length(index(AggVol)) - window + 1),
#                   type = 'l', col = 'darkblue', ylab = "RollVolume", xlab = "")]

#######################################

DayOfWeek <- unique(.indexwday(Price))

Startpoint <- CreateTime(MarketOpenRegularSession)
Endpoint   <- CreateTime(MarketCloseRegularSession)


list[M60,B60] <- CreateMSB(Y_60)
list[M20,B20] <- CreateMSB(Y_20)

#######################################


Price <- Price[paste0(Startpoint,'/')]

PriceMinutes <- to.minutes(Price)
PriceMinutes$Price.Open <- NULL
names(PriceMinutes) <- c("High", "Low", "Close")

ATR <- ATR(PriceMinutes, 120, maType = EMA)
ATR <- ATR$atr
ATR[is.na(ATR)] <- 100

Price15Minutes <- to.minutes15(Price)
names(Price15Minutes) <- c("Open", "High", "Low", "Close")
V <- volatility(Price15Minutes)


M60Lag1 <- lag(M60,1) 
M20Lag1 <- lag(M20,1) 
B20Lag1 <- lag(B20,1)


CommonTable <- merge(Price, M60,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, M60Lag1,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, B60,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, M20,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, M20Lag1,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, B20,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, B20Lag1,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)
CommonTable <- merge(CommonTable, ATR,
                     all = TRUE,
                     fill = NA,
                     suffixes = NULL,
                     join = "outer",
                     retside = TRUE,
                     retclass = "xts",
                     tzone = TimeZone)



CommonTable <- na.locf(CommonTable, na.rm = T, fromLast = F)

ind_CT <- index(CommonTable)

Price_CT    <- CommonTable[,'Price']

# M_60_CT     <- as.xts(as.numeric(CommonTable[,'M60']),     order.by = ind_CT)
# M_60_lag_CT <- as.xts(as.numeric(CommonTable[,'M60Lag1']), order.by = ind_CT)
# B_60_CT     <- as.xts(as.numeric(CommonTable[,'B60']),     order.by = ind_CT)
# M_20_CT     <- as.xts(as.numeric(CommonTable[,'M20']),     order.by = ind_CT)
# M_20_lag_CT <- as.xts(as.numeric(CommonTable[,'M20Lag1']), order.by = ind_CT)
# B_20_CT     <- as.xts(as.numeric(CommonTable[,'B20']),     order.by = ind_CT)
# B_20_lag_CT <- as.xts(as.numeric(CommonTable[,'B20Lag1']), order.by = ind_CT)

TimeCondition_Open <- 
        (ind_CT < NotradeTime)  & 
        !(ind_CT > SmallInterruptBegin    & ind_CT < SmallInterruptEnd) &
        !(ind_CT > CloseAllPositionTime_2 & ind_CT < SessionInterruptEnd) 

TimeCondition_Open <- as.xts(TimeCondition_Open, order.by = ind_CT)


TimeCondition_Close <- !(ind_CT > SmallInterruptBegin & ind_CT < SmallInterruptEnd)

TimeCondition_Close <- as.xts(TimeCondition_Close, order.by = ind_CT)

cond_1 <- (ind_CT > CloseAllPositionTime_2 & ind_CT < SessionInterruptBegin)
cond_2 <- (ind_CT > CloseAllPositionTime)

cond <- as.xts((cond_1 | cond_2), order.by = ind_CT)

ATRCons <- as.numeric(Price[1])*65/80000

OpenLongCond_T <- 
        
        (CommonTable[,'M60'] > 0) &
        (CommonTable[,'B60'] > 0) &
        (CommonTable[,'B20'] > 0) &
        (CommonTable[,'B20Lag1'] < 0) &
        TimeCondition_Open &
        CommonTable[,"atr"] > ATRCons


FirstCloseLongCondition  <- 
        
        (CommonTable[,'M60'] > CommonTable[,'M60Lag1']) & 
        (CommonTable[,'M20'] < CommonTable[,'M20Lag1']) &
        TimeCondition_Close

LongCond <- OpenLongCond_T & !(FirstCloseLongCondition)

OpenShortCond_T <- 
        
        (CommonTable[,'M60'] < 0) &
        (CommonTable[,'B60'] < 0) &
        (CommonTable[,'B20'] < 0) & 
        (CommonTable[,'B20Lag1'] > 0) & 
        TimeCondition_Open &
        CommonTable[,"atr"] > ATRCons


FirstCloseShortCondition <- 
        
        (CommonTable[,'M60'] < CommonTable[,'M60Lag1']) & 
        (CommonTable[,'M20'] > CommonTable[,'M20Lag1']) &
        TimeCondition_Close

ShortCond <- OpenShortCond_T & !(FirstCloseShortCondition)


Manifold <- LongCond | ShortCond | FirstCloseLongCondition | FirstCloseShortCondition


cat('Finish commontable')


#

plotM     <- ggplot(data=M60, aes(x=index(M60), y=M60)) + 
             geom_line( color = 'green') +
             geom_line( data=M20, color = 'darkblue',
                        aes(x=index(M20), y=M20) ) 

plotPrice <- ggplot(data = CommonTable[,'Price'],
                    aes(x=index(CommonTable[,'Price']),
                        y = CommonTable[,'Price'] ) ) + 
        
             geom_line(color = 'darkorange') +
             xlab("") + ylab("Price")

plotATR   <- ggplot(data=ATR, aes(x=index(ATR), y=ATR)) + 
                    geom_line(color = 'darkmagenta') +
                    geom_hline(yintercept = ATRCons, 
                               color = 'darkorange', linetype = 2) +
             xlab("") + ylab("ATR")

plotVolatility   <- ggplot(data=V, aes(x=index(V), y=V)) + 
                    geom_line(color = 'darkblue') +
                    xlab("") + ylab("Volatility")

