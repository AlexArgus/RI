

cat('Analyse Date:  ', date, '\n', '\n')

filenamesMOEX <- paste0(getwd(), '/', date, '/', FuturesCode, '_MOEXDATA_', date, '.rds')

cat('Getting TradeDay data ...','\n')

TradeDay <- readRDS(filenamesMOEX)

TradeDay[ , DEAL_ID := as.numeric(DEAL_ID) ]

cat('Got it.','\n')

#dateOfTrade <- strsplit(as.character(TradeDay[1, MOMENT]),' ')[[1]][1]

dateOfTrade <- TradeDay[1, format(MOMENT, "%Y-%m-%d")]

zB <- TradeDay[TYPE=='B']; zB[, TYPE := NULL]
zB <- zB[MOMENT >= paste(dateOfTrade,'10:00:01')]
zB[MOMENT > paste(dateOfTrade,'18:45') & MOMENT < paste(dateOfTrade,'19:00'), 'PRICE' := NA]

zS <- TradeDay[TYPE=='S']; zS[, TYPE := NULL]
zS <- zS[MOMENT >= paste(dateOfTrade,'10:00:01')]
zS[MOMENT > paste(dateOfTrade,'18:45') & MOMENT < paste(dateOfTrade,'19:00'), 'PRICE' := NA]

#rm(TradeDay)

#readline("Press <return> to continue") 

Bid <- zB[ ,  max(as.numeric(PRICE)), by = MOMENT ]
setnames(Bid, 'V1', 'WP')

Ask <- zS[ ,  min(as.numeric(PRICE)), by = MOMENT ]
setnames(Ask, 'V1', 'WP')

cat('Create zB, zS xts objects...','\n')

colorder <- c('MOMENT', 'DEAL_ID', 'PRICE', 'VOLUME')

setcolorder(zB, colorder )
setcolorder(zS, colorder )

zB <- as.xts(zB)
zS <- as.xts(zS)

setkey(Bid, MOMENT)
setkey(Ask, MOMENT)

Bid <- as.xts(Bid)
Ask <- as.xts(Ask)

BA <- merge.xts(Bid, Ask, all = T)

rm(Bid,Ask)

BA <- na.locf.default(BA, fromLast = TRUE)

names(BA) <- c('BID','ASK')

BA$GMP <- sqrt(as.numeric(BA$BID)*as.numeric(BA$ASK))

PriceByRegularSession <- BA$GMP

rm(BA)

cat('Got GMP Price','\n')

gc()

#readline("Press <return> to continue") 




















