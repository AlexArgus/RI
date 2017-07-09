


options(digits.secs=5)

Sys.setlocale("LC_ALL", 'English_United States.1252')

# source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

####### much useful function #########

list <- structure(NA, class = "result")

"[<-.result" <- function(x,...,value) {
        args <- as.list(match.call())
        args <- args[-c(1:2,length(args))]
        length(value) <- length(args)
        for (i in seq(along = args)) {
                a <- args[[i]]
                if (!missing(a)) eval.parent(substitute(a <- v,list(a = a,v = value[[i]])))
        }
        x
}


source(paste0(WorkingDirectory,'/','Scripts/HFT/multiplot.R'))

list[q, p, m, n]      <- c(3, 9, 12, 26)
list[k_60, k_20, k_6] <- c(113, 37, 13)


#################

coef <- function(l){
        
        c1  <- 2/(l + 1)
        c2  <- (l - 1)/(l + 1)
        
        return( list('c1' = c1, 'c2' = c2) )
}

list[c1n, c2n] <- coef(n)
list[c1m, c2m] <- coef(m)
list[c1p, c2p] <- coef(p)
list[c1q, c2q] <- coef(q)

rm(coef)

#################  Function for creating Special time points #########################

CreateTime <- function(Time){
        
        CT <- paste0(dateOfTrade,' ',Time)
        CT <- strptime(CT, format = '%Y-%m-%d %H:%M:%OS', tz = TimeZone)
        CT <- as.POSIXct(CT)
        return(CT)
}

#################  Constants and Initial Values ######################################

MarketOpenRegularSession  <- "10:00:00"
MarketCloseRegularSession <- "23:50:00"

GlobalSummator                   = 0
GlobalSummatorLong               = 0
GlobalSummatorShort              = 0
GlobalCommisionSummator          = 0
Difference                       = 0

GraphOfGlobalSummator          <- c()
GraphOfGlobalSummatorLong      <- c()
GraphOfGlobalSummatorShort     <- c()
GraphOfNumberOfDeals           <- c()
GraphOfGlobalCommisionSummator <- c()

Deals                          <- c()
DealsLong                      <- c()
DealsShort                     <- c()
MaxDeals                       <- c()
MinDeals                       <- c()
BeginTimeOfdealsLong           <- c()
BeginTimeOfdealsShort          <- c()
TimeOfdeals                    <- c()
TimeOfdealsLong                <- c()
TimeOfdealsShort               <- c()
Date                           <- c()

# ActivRegularSession            <- c()
# A_RegularSession               <- c()

MaxDifference                  <- c()


TimeZone <- 'Europe/Moscow'
Sys.setenv(TZ = TimeZone)

format  <- '%F %H:%M:%OS'
format2 <- '%y%m%d%H:%M:%OS'

FuturesCode <- 'RI'

########################################################

CreateMSB <- function(H){
        
        P <- as.numeric( last( Price[ paste0('/', Startpoint) ] ) ) 
        
        freq <- strsplit(deparse(substitute(H)), '_')[[1]][2]
        
        EMASlow <- P      #scalar
        EMAFast <- P      #scalar
        M       <- 0      #scalar
        #MLag1   <- 0      #scalar
        S       <- 0      #scalar
        B       <- 0      #scalar
        
        M_massive <- c()   
        B_massive <- c() 
        
        count <- index( H[paste0(Startpoint,'/', Endpoint)] )
        
        l=0
        
        for ( x in as.character(count)) {
                
                l = l + 1
                
                P <- as.numeric( last(  H[ paste0('/', x) ] ) ) 
                EMAFast <- c1m*P + c2m*EMAFast 
                EMASlow <- c1n*P + c2n*EMASlow
                M       <- c1q*(EMAFast - EMASlow) + c2q*M
                S       <- c1p*(EMAFast - EMASlow) + c2p*S
                B       <- c1q*(M - S) + c2q*B
                M_massive <- append(M_massive, M)
                B_massive <- append(B_massive, B)
                
                #MLag1   <- M     #scalar
                
                # if(l%%100 == 0){cat(as.character(x), '\n')
                #         cat(paste0('M', freq,'  '), round(M,digits = 3),
                #             paste0('S', freq,'  '), round(S,digits = 3),
                #             paste0('B', freq,'  '), round(B,digits = 3),'\n' )}
                
        }
        
        M_massive <- as.xts(M_massive, order.by = count)
        B_massive <- as.xts(B_massive, order.by = count)
        
        return( list(M_massive, B_massive) )
        
}


################# Horse work Functions #######################################


RealPriceTrading <- function(String, FUN, RealData) {
        
        Min_or_Max <- .Primitive(FUN)
        
        ### We take Shift sec period to check bid/ask level ###
        
        tbs_1 <- paste0(as.character(x), '/', as.character( x + Shift ) )
        
        ### After that we check 5*Delay sec duration period was or not Deals by Price one tick lower/higher ###
        
        tbs_5 <- paste0(as.character(x + Shift), '/', as.character(x + Shift + Delay * 5 ) )
        
        ### First of all we get the current level of bid or ask ###
        
        massive_1 <- RealData[tbs_1]$PRICE
        
        if (length(massive_1) > 0) {
                
                PriceWithDelay_1  <- Min_or_Max(massive_1)
                
        } else {
                
                PriceWithDelay_1 <- NA
                
        }
        
        ### Next we check were or not deal with level lower/upper than levels bid/ask ( see above) ###
        
        massive_5 <- RealData[RealData$DEAL_ID != ""][tbs_5]$PRICE
        
        
        if (length(massive_5) > 0) {
                
                PriceWithDelay_5 <-  Min_or_Max(massive_5)
                
        } else {
                
                
                PriceWithDelay_5 <- NA;
        }
        
        cat('**************************************************************************', '\n')
        
        cat('Range of PriceWithDelay_1 is    ')
        
        cat(range(massive_1), '\n')
        
        cat('Range of PriceWithDelay_5 is    ')
        
        cat(range(massive_5), '\n')
        
        cat('**************************************************************************', '\n')
        
        cat( paste0(String, Delay * 1,':   '),  PriceWithDelay_1, '\n')
        
        cat( paste0(String, Delay * 5,':   '),  PriceWithDelay_5, '\n')
        
        
        if ( !is.na(PriceWithDelay_1) ) {
                
                PriceWithDelay <- get0('PriceWithDelay_1')
                
                Vol_1 <- RealData[RealData$PRICE == PriceWithDelay][tbs_1]$VOLUME
                
                Vol_1 <- last(Vol_1)[[1]]
                
                Vol_5 <- RealData[(RealData$PRICE <= PriceWithDelay ) & RealData$DEAL_ID != "" ][tbs_5]$VOLUME
                
                Vol_5 <- sum( Vol_5 )
                
                cat('VolumeWithDelay_1  ', Vol_1, '\n')
                
                cat('VolumeWithDelay_5  ', Vol_5, '\n')
                
        } else {
                
                ZeroPoint <- RealData[ last( which( index( RealData ) <= x ) ) ]
                
                PriceWithDelay  <- ZeroPoint$PRICE[[1]]
                
                cat('We get the Price from previous tick...', PriceWithDelay, '\n')
                
                Vol_1 <- ZeroPoint$VOLUME[[1]]
                
                Vol_5 <- RealData[(RealData$PRICE <= PriceWithDelay) & RealData$DEAL_ID != ""][tbs_5]$VOLUME
                
                if (length(Vol_5) > 0) {Vol_5 <- sum(Vol_5)} else {Vol_5 <- 0}
                
                cat('We get the Volume from previous tick...', 'Vol_1  ', Vol_1,' Vol_5  ', Vol_5, '\n')
                
        }
        
        ###
        
        if ( !is.na(PriceWithDelay_1) & !is.na(PriceWithDelay_5) ) {
                
                if ( FUN == 'min') { condition <- (PriceWithDelay_5 <= PriceWithDelay - 0 ) & ( Vol_1 < Vol_5 )
                
                #MaxDifference <- append(MaxDifference, as.numeric(PriceWithDelay) - as.numeric(PriceWithDelay_5) )
                cat('Max Difference is ', PriceWithDelay - PriceWithDelay_5, '\n')
                
                
                } else{              condition <- (PriceWithDelay_5 >= PriceWithDelay + 0 ) & ( Vol_1 < Vol_5 )
                
                #MaxDifference <- append(MaxDifference, as.numeric(PriceWithDelay_5) - as.numeric(PriceWithDelay) )
                cat('Max Difference is ', PriceWithDelay_5 - PriceWithDelay, '\n')
                
                }
                
                
        } else { condition <- FALSE }
        
        ###
        
        if ( condition ) { return( PriceWithDelay ) # - FUNFUN(-0, 0))
                
                
        }  else {
                
                PriceWithDelay <- NA;
                
                cat('There is no Deal. Price go out ((( ...', '\n')
                
        }
        
        ##############################
        
        return(PriceWithDelay)
        
}

#################################################################################################################

ClosePriceTrading <- function(RealData) {
        
        y <- x + Shift + Delay*5
        
        PriceWithDelay  <- last(RealData[ paste0('/', y) ]$PRICE)[[1]]
        
        cat('We get the Price from previous tick...', PriceWithDelay, '\n')
        
        return(PriceWithDelay)
}


OpenLong <- function() {
        
        RP <- RealPriceTrading('OpenLongPriceWithDelay_', 'min', zB) 
        
        cat('New Deal ##################################################', '\n')
        
        if(!is.na(RP)){
                
                cat('Price: ', P, '   OpenLongPriceWithDelay:  ', RP, 'Difference ', (P - RP), '\n')
                
                Difference %+=% (P - RP); cat('TotalDifference', Difference, '\n')
                Position %+=% c
                CommisionSummator %+=% c*comissions
                NumberOfDeals %+=% 1
                AccumulatorLong %-=% RP 
                BeginTimeOfdealsLong <<- append(BeginTimeOfdealsLong, x)
                TechAcL <<- - RP  
                
                cat('LongTime:  ', paste0(x,' '))
                cat('LongPrice: ', paste0(round(RP, digits = 0)),'\n')
                
                # plotPrice <<- plotPrice + geom_point( aes(x=x, y=P), pch=2, color='blue')
                # 
                # plotPrice 
        
        
        
        }else{cat('Time is', as.character(x), '\n')}

}


OpenShort <- function() {
        
        RP <- RealPriceTrading('OpenShortPriceWithDelay_', 'max', zS) 
        
        cat('Next Deal ##################################################', '\n')
        
        if(!is.na(RP)){
                
                cat('Price: ', P, '   OpenShortPriceWithDelay:  ', RP,
                    'Difference', (RP - P), '\n')
                
                Difference %+=% (RP - P)
                
                cat('TotalDifference', Difference, '\n')
                Position %-=% c
                CommisionSummator %+=% c*comissions
                NumberOfDeals %+=% 1
                AccumulatorShort %+=% RP  
                BeginTimeOfdealsShort <<- append(BeginTimeOfdealsShort, x)
                TechAcS <<- RP  
                cat('TechShort:', (TechAcS - RP) ,'  ')
                cat('Short: ', paste0(x,' '))
                cat('Price of Deal: ', paste0(round(RP, digits = 0)),'\n')
                
                # plotPrice <<- plotPrice + geom_point( aes(x=x, y=P), pch=6, color='magenta' )
                # 
                # plotPrice
                
                
        }else{cat('Time is', as.character(x), '\n')}
        
}

CloseLong <- function() {
        
        RP <- as.numeric(RealPriceTrading('CloseLongPriceWithDelay_', 'max', zS))
        
        cat('Close Deal ##################################################', '\n')
        
        if(is.na(RP)){ RP <- ClosePriceTrading(zB) }
        
        cat('Price: ', P, '   CloseLongPriceWithDelay:  ', RP,
            'Difference', (RP - P), '\n')
        Difference %+=% (RP - P)
        cat('TotalDifference', Difference, '\n')
        Position %-=% 1
        CommisionSummator %+=% c*comissions
        NumberOfDeals %+=% 1
        AccumulatorLong %+=% RP
        Deals           <<- append(Deals, (TechAcL + RP))
        DealsLong       <<- append(DealsLong, (TechAcL + RP))
        TimeOfdeals     <<- append(TimeOfdeals, x)
        TimeOfdealsLong <<- append(TimeOfdealsLong, x)
        
        cat('CloseLongTime:  ', paste0(x,' '))
        cat('CloseLongPrice: ', paste0(round(RP, digits = 0)),'\n')
        
        cat('TechLong:', round((TechAcL + RP), digits = 2) ,'  ')
        
        MaxDeals <<- append(MaxDeals, MaxProfitOfPositionLong)
        MinDeals <<- append(MinDeals, MinProfitOfPositionLong)
        cat('Max', round(MaxProfitOfPositionLong, digits = 2), '  ')
        cat('Min', round(MinProfitOfPositionLong, digits = 2), '\n')
        
        MaxProfitOfPositionLong <<- 0
        MinProfitOfPositionLong <<- 0 
        TechAcL <<- 0; 
        
        
        cat('AccumulatorLong: ', round(AccumulatorLong, digits = 1), '\n')
        
        # plotPrice <<- plotPrice + geom_point(aes(x=x, y=P), pch=6, color='red')
        # 
        # plotPrice
        
}

CloseShort <- function() {
        
        RP <- as.numeric(RealPriceTrading('CloseShortPriceWithDelay_', 'min', zB))
        
        cat('Close Deal ##################################################', '\n')
        
        if(is.na(RP)){ RP <-  ClosePriceTrading(zS) }
        
        cat('Price: ', P, '   CloseShortPriceWithDelay:  ', RP,
            'Difference', (P - RP), '\n')
        
        Difference %+=% (P - RP)
        
        cat('TotalDifference', Difference, '\n')
        
        #cat( 'Position before deal: ', paste0(Position,'\n') )
        Position %+=% 1
        CommisionSummator %+=% c*comissions
        NumberOfDeals    %+=% 1
        AccumulatorShort %-=% RP ; cat('AccumulatorShort   ', AccumulatorShort, '\n','\n')
        Deals <<- append(Deals, (TechAcS - RP))
        DealsShort <<- append(DealsShort, (TechAcS - RP))
        TimeOfdeals <<- append(TimeOfdeals,x)
        TimeOfdealsShort <<- append(TimeOfdealsShort, x)
        
        cat('TechShort:', round((TechAcS - RP), digits = 2) ,'  ')
        TechAcS <<- 0
        ##
        MaxDeals <<- append(MaxDeals, MaxProfitOfPositionShort)
        MinDeals <<- append(MinDeals, MinProfitOfPositionShort)
        cat('Max', round(MaxProfitOfPositionShort, digits = 2), '  ')
        cat('Min', round(MinProfitOfPositionShort, digits = 2), '\n')
        
        MaxProfitOfPositionShort <<- 0
        MinProfitOfPositionShort <<- 0 
        
        cat('Close: ', paste0(x,' '))
        cat('Price of Deal: ', paste0(round(RP, digits = 0),'  ') )
        
        cat('AccumulatorShort: ', paste0(round(AccumulatorShort, digits = 1),'\n'))
        
        # plotPrice <<- plotPrice + geom_point(aes(x=x, y=P), pch=2, color='green')
        # 
        # plotPrice   
        
}

########################################################################

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <<- (e1 + e2) ) )
`%-=%` = function(e1,e2) eval.parent(substitute(e1 <<- (e1 - e2) ) )

#######################################################################



GetDDStat <- function(DD){
        
        for (i in mt){ cat(as.character(i),
                           round(  mean( DD[i] ),  digits = 2),
                           round(length( DD[i] ),  digits = 2),
                           round(   sum( DD[i] ),  digits = 2), '\n') 
        }
        
        cat('Monday'    , sum(DD[.indexwday(DD)==1]), '\n')
        cat('Tuesday'   , sum(DD[.indexwday(DD)==2]), '\n')
        cat('Wednesday' , sum(DD[.indexwday(DD)==3]), '\n')
        cat('Thursday'  , sum(DD[.indexwday(DD)==4]), '\n')
        cat('Friday'    , sum(DD[.indexwday(DD)==5]), '\n')
        
}

cppFunction('NumericVector EMACpp(NumericVector ys, NumericVector alpha) {

            int n = ys.size();
            NumericVector out(n);
            out[0] = ys[0];
            
            for(int i = 1; i < n; ++i) {
            
            double al = alpha[i - 1];
            double mu = exp(-al);
            double vu = (1 - mu)/al;
            
            out[i] = mu*out[i-1] + (vu - mu)*ys[i-1] + (1 - vu)*ys[i];
            
            }
            return out;
            
            }')












