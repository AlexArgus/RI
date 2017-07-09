TradeDay[1]

Start <- TradeDay[1324, MOMENT]


View(TradeDay[MOMENT > Start & MOMENT < Start + 1])

colorder <- c('MOMENT', 'DEAL_ID','TYPE','PRICE', 'VOLUME')

setcolorder(TradeDay, colorder)

TradeDay_xts <- as.xts(TradeDay)

f <- zB[(index(zB) > Start & index(zB) < Start + 1)]

TradeDay[TYPE=='B' & !is.na(DEAL_ID) & (MOMENT > Start & MOMENT < Start + 1), points(PRICE, type = 's', col = 'darkred')]

z = 1


while (z < 1000){
        
        index <- index(f)[z]
        
        cat('PRICE ', coredata(f[index]$PRICE)[1,][[1]], 'VOLUME ', coredata(f[index]$VOLUME)[1,][[1]], '\n')
        
        readline("")
        
        z = z+1
        
}
