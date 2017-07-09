

Imagename <- paste0(getwd(), '/', date, '/', FuturesCode, '_MOEXDATA_', date, '.RDate')

mark = FALSE

if ( mark ) { load(Imagename)
}else{source( paste0(ScripthPath,   '/forts_code_W3.0.R') )
      source( paste0(ScripthPath,'/IfImageWasNotLoaded.R') ) 
      save.image(Imagename)}

#################################################

gc()

################################################# 

plots <- list()
plots[[1]] <- plotPrice
plots[[2]] <- plotM
plots[[3]] <- plotATR
plots[[4]] <- plotVolatility


multiplot(plotlist = plots, cols = 1)

#################################################

plot.new()

plots <- list()
plots[[1]] <- plotPrice

multiplot(plotlist = plots, cols = 1)

#################################################



for ( y in which(Manifold == T)[1:1000] ) {
        
        x <- index(Manifold)[y]
        
        P <- as.numeric( last( CommonTable[paste0('/', x),'Price'] ) ) 
        
############################################################################
        
        if( LongCond[x] & Position == 0 ) { OpenLong() } 
        
############################################################################        

        if (Position > 0) {

                ProfitOfPositionLong <- TechAcL + P

                if ( ProfitOfPositionLong > MaxProfitOfPositionLong) {
                        MaxProfitOfPositionLong = ProfitOfPositionLong}

                if ( ProfitOfPositionLong < MinProfitOfPositionLong) {
                        MinProfitOfPositionLong = ProfitOfPositionLong}

        }
        
############################################################################
        

        if ( (FirstCloseLongCondition[x] | 
              ProfitOfPositionLong < -StopLoss | 
              ProfitOfPositionLong > TakeProfit) & 
              Position == 1) {

                CloseLong()
        }
        
        
############################################################################
        
        if ( Position == 0 & ShortCond[x]) { OpenShort() }
        
############################################################################

        if (Position < 0 ) {

                ProfitOfPositionShort <-  TechAcS - P

                if ( ProfitOfPositionShort > MaxProfitOfPositionShort) {
                        MaxProfitOfPositionShort = ProfitOfPositionShort}

                if ( ProfitOfPositionShort < MinProfitOfPositionShort) {
                        MinProfitOfPositionShort = ProfitOfPositionShort}

        }
        
############################################################################        
        
        if ( (FirstCloseShortCondition[x] | 
              ProfitOfPositionShort < -StopLoss | 
              ProfitOfPositionLong > TakeProfit) & 
              Position == -1) {

                CloseShort()
        }
        

############################################################################        

        if (cond[x] & Position != 0) {

                if (Position == -1) { CloseShort()

                } else              { CloseLong() }
        }
        
        if ( (y%%300) == 0) cat( 'Time is  ', paste0(x, '  ', '\n') )
        
}




GlobalSummator                %+=% (AccumulatorLong + AccumulatorShort)*value*0.02
GlobalSummatorLong            %+=% AccumulatorLong
GlobalSummatorShort           %+=% AccumulatorShort
GlobalCommisionSummator       %+=% CommisionSummator

cat('Indicative Rate is'       , value,                                          '\n')
cat('GlobalSummator: '         , round(GlobalSummator,              digits = 2), '\n')
cat('GlobalSummatorLong: '     , round(GlobalSummatorLong,          digits = 2), '\n')
cat('GlobalSummatorShort: '    , round(GlobalSummatorShort,         digits = 2), '\n')
cat('Total Difference: '       , round(Difference,                  digits = 2), '\n')
cat('Mean  Of MaxDifference: ' , round(mean(!is.na(MaxDifference)), digits = 2), '\n')
cat('GlobalCommisionSummator: ', round(GlobalCommisionSummator,     digits = 2), '\n')

cat('********************************************',                              '\n')

cat('NumberOfDeals: '          , NumberOfDeals,                                  '\n')
cat('AccumulatorLong'          , round(AccumulatorLong,             digits = 0), '\n')
cat('AccumulatorShort'         , round(AccumulatorShort,            digits = 0), '\n')

cat('********************************************','\n')

GraphOfGlobalSummator          <- append(GraphOfGlobalSummator,          GlobalSummator         )
GraphOfGlobalSummatorLong      <- append(GraphOfGlobalSummatorLong,      GlobalSummatorLong     )
GraphOfGlobalSummatorShort     <- append(GraphOfGlobalSummatorShort,     GlobalSummatorShort    )
GraphOfNumberOfDeals           <- append(GraphOfNumberOfDeals,           NumberOfDeals          )
GraphOfGlobalCommisionSummator <- append(GraphOfGlobalCommisionSummator, GlobalCommisionSummator)

Date                           <- append(Date, as.POSIXct(strptime(dateOfTrade,format = '%Y-%m-%d')))

layout( matrix( c(1,1,1,1) ) )

ylim <- range(cbind(
                    GraphOfGlobalSummator,
                    GraphOfGlobalSummatorLong,
                    GraphOfGlobalSummatorShort,
                    GraphOfGlobalCommisionSummator
                    )
              )

plot(Date,   GraphOfGlobalSummator, ylim = ylim, xlab = "")
points(Date, GraphOfGlobalSummatorLong,  col = 'blue')
points(Date, GraphOfGlobalSummatorShort, col = 'red')
points(Date, GraphOfGlobalCommisionSummator, col = 'magenta')



