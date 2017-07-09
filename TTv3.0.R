source('C:/Users/Alexandr/Desktop/TimeSeriesAnalysis/FinData/Scripts/Real/RI_Project/forts_code_W3.0.R')

Position         =  0
#Accumulator     =  0
AccumulatorLong  =  0
AccumulatorShort =  0
CommisionSummator = 0
NumberOfDeals    =  0
StopLoss         =  10000

Delay = 12.0 # Coefficient of multiplications
Shift = 1.0 # Delay in reaction (sec)
comissions = 1.0 + 1.0  # comissions per round (1 contract) #


c=1

TechAcL = 0
MaxProfitOfPositionLong = 0
MinProfitOfPositionLong = 0

TechAcS = 0
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
NotradeTime                     <- CreateTime('23:30:00')
CloseAllPositionTime            <- CreateTime('23:40:00')
#####################################


Y_60 <-
        aggregatePrice(PriceByRegularSession, 
                       on="secs",
                       k=k_60, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)

Y_20 <-
        aggregatePrice(PriceByRegularSession, 
                       on="secs",
                       k=k_20, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)

system.time(

Price <- 
        aggregatePrice(PriceByRegularSession, 
                       on="secs",
                       k=1, 
                       marketopen  = MarketOpenRegularSession,
                       marketclose = MarketCloseRegularSession, 
                       tz = TimeZone)

, gcFirst = TRUE)

#######################################

#layout(matrix(c(2,2,1,1), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1, 1))

setkey(TradeDay, MOMENT)

TD <- TradeDay[DEAL_ID!=""]

############# Roll Volume ############

# Vol <- as.xts(TD[ , as.integer(VOLUME)], order.by = TD[ , MOMENT_2])
# 
# Volume <- 
#         aggregatePrice(Vol, 
#                        on="secs",
#                        k=1, 
#                        marketopen  = MarketOpenRegularSession,
#                        marketclose = MarketCloseRegularSession, 
#                        tz = TimeZone)
# 
# window <- 3600L
# 
# RollVolume <- roll_sum(coredata(Volume), n = window, by = 1L, fill = numeric(0),
#                        partial = FALSE, align = "right", na.rm = FALSE)
# 
# RollVolume <- as.data.table(RollVolume)
# 
# RollVolume[, plot(tail(index(Volume), n = length(index(Volume)) - window + 1),
#                   V1, type='l', col='darkblue', ylab = "RollVolume", xlab = "")]

#######################################

DayOfWeek <- unique(.indexwday(Price))

Startpoint <- CreateTime(MarketOpenRegularSession)
Endpoint   <- CreateTime(MarketCloseRegularSession)



list[M60,B60] <- CreateMSB(Y_60)
list[M20,B20] <- CreateMSB(Y_20)

#######################################


Price <- Price[paste0(Startpoint,'/')]

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



CommonTable <- na.locf(CommonTable, na.rm = T, fromLast = F)

ind_CT <- index(CommonTable)

Price_CT    <- as.xts(as.numeric(CommonTable[,'Price']),   order.by = ind_CT)
M_60_CT     <- as.xts(as.numeric(CommonTable[,'M60']),     order.by = ind_CT)
M_60_lag_CT <- as.xts(as.numeric(CommonTable[,'M60Lag1']), order.by = ind_CT)
B_60_CT     <- as.xts(as.numeric(CommonTable[,'B60']),     order.by = ind_CT)
M_20_CT     <- as.xts(as.numeric(CommonTable[,'M20']),     order.by = ind_CT)
M_20_lag_CT <- as.xts(as.numeric(CommonTable[,'M20Lag1']), order.by = ind_CT)
B_20_CT     <- as.xts(as.numeric(CommonTable[,'B20']),     order.by = ind_CT)
B_20_lag_CT <- as.xts(as.numeric(CommonTable[,'B20Lag1']), order.by = ind_CT)

cat('Finish commontable')


#

plotM     <- ggplot(data=M60, aes(x=index(M60), y=M60)) + 
             geom_line(color = 'green') +
             geom_line( data=M20, color = 'darkblue',
                   aes(x=index(M20), y=M20) ) 

plotPrice <- ggplot(data=Price_CT, aes(x=index(Price_CT), y=Price_CT)) + 
             geom_line(color = 'darkorange')


################################################# 

plots <- list()
plots[[1]] <- plotPrice
plots[[2]] <- plotM


multiplot(plotlist = plots, cols = 1)

#################################################

#ind_CT <- ind_CT[ind_CT > SmallInterruptEnd]

#y <- 1

for ( y in seq_along(ind_CT) ) {
        
        x <- ind_CT[y]
        
        P <- as.numeric( last( Price_CT[ paste0('/', x) ] ) ) 
        
        system.time(
        
        OpenLongCond_T <-
                
                (M_60_CT[x]>0) &
                (B_60_CT[x]>0) &
                (B_20_CT[x]>0) &
                (B_20_lag_CT[x]<0) &
                (x < NotradeTime)  & 
                !(x > SmallInterruptBegin & x < SmallInterruptEnd) &
                !(x > CloseAllPositionTime_2 & x < SessionInterruptEnd) 
        
        , gcFirst = TRUE)
                
        FirstCloseLongCondition  <- 
                
                (M_60_CT[x]> M_60_lag_CT[x]) & 
                (M_20_CT[x]< M_20_lag_CT[x]) &
                !(x > SmallInterruptBegin & x < SmallInterruptEnd) 
        
        
        if( OpenLongCond_T & (!FirstCloseLongCondition) & Position == 0 ) {
                
                RP <- as.numeric(RealPriceTrading('OpenLongPriceWithDelay_', 'min', zB) )
                
                cat('New Deal ##################################################', '\n')
                
                if(!is.na(RP)){
                        
                        cat('Price: ', P,
                            '   OpenLongPriceWithDelay:  ', RP,
                            'Difference', (P - RP), '\n')
                        
                        Difference <- Difference + P - RP
                        cat('TotalDifference', Difference, '\n')
                        Position = Position + c
                        CommisionSummator = CommisionSummator + c*comissions
                        NumberOfDeals = NumberOfDeals + 1
                        AccumulatorLong = AccumulatorLong - RP 
                        BeginTimeOfdealsLong <- append(BeginTimeOfdealsLong, x)
                        TechAcL = - RP  
                        
                        cat('LongTime:  ', paste0(x,' '))
                        cat('LongPrice: ', paste0(round(P, digits = 0)),'\n')
                        
                        plotPrice <- plotPrice + 
                                     geom_point(data = Price_CT, 
                                     aes(x=x, y=P), pch=2, color='blue')
                        
                        
                }else{cat('Time is', as.character(x), '\n')}
                
        }
        
        
        ProfitOfPositionLong <-  TechAcL + P 
        
        if (Position > 0) {
                
                ProfitOfPositionLong <-  TechAcL + P 
                
                if ( ProfitOfPositionLong > MaxProfitOfPositionLong) {
                        MaxProfitOfPositionLong = ProfitOfPositionLong} 
                
                if ( ProfitOfPositionLong < MinProfitOfPositionLong) {
                        MinProfitOfPositionLong = ProfitOfPositionLong} 
                
        } 
        
        
        if ( (FirstCloseLongCondition | ProfitOfPositionLong < -StopLoss) & Position == 1) {
                
                RP <- as.numeric(RealPriceTrading('CloseLongPriceWithDelay_', 'max', zS))
                
                cat('Close Deal ##################################################', '\n')
                
                if(is.na(RP)){ RP <- ClosePriceTrading(zB) }
                
                cat('Price: ', P, '   CloseLongPriceWithDelay:  ', RP,
                    'Difference', (RP - P), '\n')
                Difference <- Difference + RP - P
                cat('TotalDifference', Difference, '\n')
                Position=Position - 1
                CommisionSummator = CommisionSummator + c*comissions
                NumberOfDeals = NumberOfDeals + 1
                AccumulatorLong = AccumulatorLong + RP
                Deals <- append(Deals, (TechAcL + RP))
                DealsLong <- append(DealsLong, (TechAcL + RP))
                TimeOfdeals <- append(TimeOfdeals, x)
                TimeOfdealsLong <- append(TimeOfdealsLong, x)
                
                cat('CloseLongTime:  ', paste0(x,' '))
                cat('CloseLongPrice: ', paste0(round(P, digits = 0)),'\n')
                
                cat('TechLong:', round((RP - TechAcL), digits = 2) ,'  ')
                
                MaxDeals <- append(MaxDeals, MaxProfitOfPositionLong)
                MinDeals <- append(MinDeals, MinProfitOfPositionLong)
                cat('Max', round(MaxProfitOfPositionLong, digits = 2), '  ')
                cat('Min', round(MinProfitOfPositionLong, digits = 2), '\n')
                
                MaxProfitOfPositionLong = 0
                MinProfitOfPositionLong = 0 
                TechAcL = 0; 
                
                
                cat('AccumulatorLong: ', paste0(round(AccumulatorLong, digits = 1),'\n'))
                plotPrice <- plotPrice + 
                        geom_point(data = Price_CT, aes(x=x, y=P), pch=6, color='red')
                #points(x, P, pch=6, col='red')
                
        }  
        
        
        
        OpenShortCond_T <- 
                
                (M_60_CT[x]<0) &
                (B_60_CT[x]<0) &
                #(!is.na(B_20_lag_CT[x])) & 
                (B_20_CT[x]<0) & 
                (B_20_lag_CT[x]>0) & 
                (Position==0) & (x < NotradeTime) &
                #(x > SmallInterruptEnd) &
                !(x > SmallInterruptBegin & x < SmallInterruptEnd) &
                !(x > CloseAllPositionTime_2 & x < SessionInterruptEnd) 
                #( DayOfWeek %in% c(1,3,5) )
        
        FirstCloseShortCondition <- 
                
                #( !is.na(M_60_lag_CT[x]) &
                (M_60_CT[x] < M_60_lag_CT[x]) & 
                #( !is.na(M_20_lag_CT[x])) &
                (M_20_CT[x] > M_20_lag_CT[x]) &
                !(x > SmallInterruptBegin & x < SmallInterruptEnd)   
        
        
        
        if ( OpenShortCond_T & (!FirstCloseShortCondition) ){#& 
                #(!SecondCloseShortCondition) & (!ThirdCloseShortCondition) ) {
                
                RP <- as.numeric(RealPriceTrading('OpenShortPriceWithDelay_', 'max', zS) )
                
                cat('Next Deal ##################################################', '\n')
                
                if(!is.na(RP)){
                        
                        cat('Price: ', P, '   OpenShortPriceWithDelay:  ', RP,
                            'Difference', (RP - P), '\n')
                        
                        Difference <- Difference + RP - P
                        
                        cat('TotalDifference', Difference, '\n')
                        Position=Position - c
                        CommisionSummator = CommisionSummator + c*comissions
                        NumberOfDeals = NumberOfDeals + 1
                        AccumulatorShort = AccumulatorShort + RP  
                        BeginTimeOfdealsShort <- append(BeginTimeOfdealsShort, x)
                        TechAcS = RP  
                        cat('TechShort:', (TechAcS - RP) ,'  '); rm(RP)
                        cat('Short: ', paste0(x,' '))
                        cat('Price of Deal: ', paste0(round(P, digits = 0)),'\n')
                        plotPrice <- plotPrice + 
                                geom_point(data = Price_CT, aes(x=x, y=P), pch=6, color='magenta')
                        #points(x, P, pch=6, col='magenta')
                        
                        
                }else{cat('Time is', as.character(x), '\n')}
                
        }
        
        ProfitOfPositionShort <-  TechAcS - P  
        
        if (ProfitOfPositionShort != 0 & Position < 0 ) {
                
                if ( ProfitOfPositionShort > MaxProfitOfPositionShort) {
                        MaxProfitOfPositionShort = ProfitOfPositionShort} 
                
                if ( ProfitOfPositionShort < MinProfitOfPositionShort) {
                        MinProfitOfPositionShort = ProfitOfPositionShort} 
                
        }
        
        
        
        if ( (FirstCloseShortCondition | ProfitOfPositionShort < -StopLoss) & Position == -1) {
                
                RP <- as.numeric(RealPriceTrading('CloseShortPriceWithDelay_', 'min', zB))
                
                cat('Close Deal ##################################################', '\n')
                
                if(is.na(RP)){ RP <-  ClosePriceTrading(zS) }
                
                cat('Price: ', P, '   CloseShortPriceWithDelay:  ', RP,
                    'Difference', (P - RP), '\n')
                
                Difference <- Difference + P - RP
                
                cat('TotalDifference', Difference, '\n')
                
                #cat( 'Position before deal: ', paste0(Position,'\n') )
                Position=Position + 1
                CommisionSummator = CommisionSummator + c*comissions
                NumberOfDeals = NumberOfDeals + 1
                AccumulatorShort = AccumulatorShort - RP  
                Deals <- append(Deals, (TechAcS - RP))
                DealsShort <- append(DealsShort, (TechAcS - RP))
                TimeOfdeals <- append(TimeOfdeals,x)
                TimeOfdealsShort <- append(TimeOfdealsShort, x)
                
                cat('TechShort:', round((TechAcS - RP), digits = 2) ,'  ')
                TechAcS = 0; rm(RP)
                ##
                MaxDeals <- append(MaxDeals, MaxProfitOfPositionShort)
                MinDeals <- append(MinDeals, MinProfitOfPositionShort)
                cat('Max', round(MaxProfitOfPositionShort, digits = 2), '  ')
                cat('Min', round(MinProfitOfPositionShort, digits = 2), '\n')
                
                MaxProfitOfPositionShort = 0
                MinProfitOfPositionShort = 0 
                
                cat('Close: ', paste0(x,' '))
                cat('Price of Deal: ', paste0(round(P, digits = 0),'  ') )
                
                cat('AccumulatorShort: ', paste0(round(AccumulatorShort, digits = 1),'\n'))
                plotPrice <- plotPrice + 
                        geom_point(data = Price_CT, aes(x=x, y=P), pch=2, color='green')
                #points(x, P, pch=2, col='green')
                
        }
        
        
        
        
        
        cond_1 <- (x > CloseAllPositionTime_2 & x < SessionInterruptBegin)
        cond_2 <- (x > CloseAllPositionTime)
        
        FothCloseCondition <- ( (cond_1 | cond_2) & Position!=0 )
        
        if (FothCloseCondition) {
                
                if (Position == -1) {
                        
                        RP <- ClosePriceTrading(zS)
                        
                        cat('Close Deal ##################################################', '\n')
                        
                        cat('Price: ', P, '   CloseShortPriceWithDelay:  ', RP,
                            'Difference', (P - RP), '\n')
                        
                        Difference <- Difference + P - RP
                        
                        cat('TotalDifference', Difference, '\n')
                        
                        cat('Close: ', paste0(x,'\n'))
                        NumberOfDeals = NumberOfDeals + 1
                        AccumulatorShort = AccumulatorShort - RP 
                        Deals <- append(Deals, (TechAcS - RP))
                        DealsShort <- append(DealsShort, (TechAcS - RP))
                        TimeOfdeals <- append(TimeOfdeals, x)
                        TimeOfdealsShort <- append(TimeOfdealsShort, x)
                        
                        cat('TechShort:', (TechAcS - RP) ,'  ')
                        TechAcS = 0; rm(RP)
                        ##
                        MaxDeals <- append(MaxDeals, MaxProfitOfPositionShort)
                        MinDeals <- append(MinDeals, MinProfitOfPositionShort)
                        cat('Max',  round(MaxProfitOfPositionShort, digits = 2), '  ')
                        cat('Min',  round(MinProfitOfPositionShort, digits = 2), '\n')
                        
                        MaxProfitOfPositionShort = 0
                        MinProfitOfPositionShort = 0 
                        ##
                        Position = Position + 1
                        CommisionSummator = CommisionSummator + c*comissions
                        #cat('FthCShortP_Closing: ', paste0(x,'\n'))
                        #cat('Position: ', paste0(Position,'\n'))
                        cat('AccumulatorShort: ', paste0(round(AccumulatorShort, digits = 1),'\n'))
                        plotPrice <- plotPrice + 
                                geom_point(data = Price_CT, aes(x=x, y=P), pch=2, color='green')
                        #points(x, P, pch=2, col='green')
                        
                }
                
                else                {
                        
                        RP <- ClosePriceTrading(zB)
                        
                        cat('Close Deal ##################################################', '\n')
                        
                        cat('Price: ', P, '   CloseLongPriceWithDelay:  ', RP,
                            'Difference', (RP - P), '\n')
                        
                        Difference <- Difference + RP - P
                        
                        cat('TotalDifference', Difference, '\n')
                        
                        cat('Close: ', paste0(x,'\n'))
                        NumberOfDeals = NumberOfDeals + 1
                        AccumulatorLong = AccumulatorLong + RP 
                        DealsLong <- append(DealsLong, (TechAcL + RP))
                        TimeOfdeals <- append(TimeOfdeals, x)
                        TimeOfdealsLong <- append(TimeOfdealsLong, x)
                        
                        cat('TechLong:', (TechAcL + RP) ,'  ')
                        TechAcL= 0; rm(RP)
                        ##
                        MaxDeals <- append(MaxDeals, MaxProfitOfPositionLong)
                        MinDeals <- append(MinDeals, MinProfitOfPositionLong)
                        cat('Max', round(MaxProfitOfPositionLong, digits = 2), '  ')
                        cat('Min', round(MinProfitOfPositionLong, digits = 2), '\n')
                        MaxProfitOfPositionLong = 0
                        MinProfitOfPositionLong = 0 
                        Position = Position - 1
                        CommisionSummator = CommisionSummator + c*comissions
                        cat('AccumulatorLong: ', paste0(round(AccumulatorLong, digits = 1),'\n'))
                        plotPrice <- plotPrice + 
                                geom_point(data = Price_CT, aes(x=x, y=P), pch=6, color='red')
                        #points(x, P, pch=6, col='red')
                        
                }
                
        }
        
}


GlobalSummator                <- GlobalSummator + (AccumulatorLong + AccumulatorShort)*value*0.02
GlobalSummatorLong            <- GlobalSummatorLong  + AccumulatorLong
GlobalSummatorShort           <- GlobalSummatorShort + AccumulatorShort
GlobalCommisionSummator       <- GlobalCommisionSummator + CommisionSummator

cat('Indicative Rate is'       , value, '\n')
cat('GlobalSummator: '         , round(GlobalSummator,digits = 2), '\n')
cat('GlobalSummatorLong: '     , round(GlobalSummatorLong,digits = 2), '\n')
cat('GlobalSummatorShort: '    , round(GlobalSummatorShort,digits = 2), '\n')
cat('Total Difference: '       , round(Difference, digits = 2), '\n')
cat('Mean  Of MaxDifference: ' , round(mean(!is.na(MaxDifference)), digits = 2), '\n')
cat('GlobalCommisionSummator: ', round(GlobalCommisionSummator,digits = 2), '\n')

cat('********************************************','\n')

cat('NumberOfDeals: '          , NumberOfDeals,'\n')
cat('AccumulatorLong'          , round(AccumulatorLong,  digits = 0), '\n')
cat('AccumulatorShort'         , round(AccumulatorShort, digits = 0), '\n')

cat('********************************************','\n')

GraphOfGlobalSummator          <- append(GraphOfGlobalSummator,          GlobalSummator)
GraphOfGlobalSummatorLong      <- append(GraphOfGlobalSummatorLong,      GlobalSummatorLong)
GraphOfGlobalSummatorShort     <- append(GraphOfGlobalSummatorShort,     GlobalSummatorShort)
GraphOfNumberOfDeals           <- append(GraphOfNumberOfDeals,           NumberOfDeals)
GraphOfGlobalCommisionSummator <- append(GraphOfGlobalCommisionSummator, GlobalCommisionSummator)

Date                           <- append(Date, as.POSIXct(strptime(dateOfTrade,format = '%Y-%m-%d')))

layout( matrix(c(1,1,1,1)) )

ylim <- range(cbind(GraphOfGlobalSummator,
                    GraphOfGlobalSummatorLong,
                    GraphOfGlobalSummatorShort,
                    GraphOfGlobalCommisionSummator))

plot(Date,   GraphOfGlobalSummator, ylim = ylim, xlab = "")
points(Date, GraphOfGlobalSummatorLong,  col = 'blue')
points(Date, GraphOfGlobalSummatorShort, col = 'red')
points(Date, GraphOfGlobalCommisionSummator, col = 'magenta')



