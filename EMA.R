


EMAFUN <- function(tay, z, n, E) {
        
        if (n == 1) {s <- z[n,][[1]]
        }else{ 
                
        alpha <- index(z[n, ]) - index(z[n - 1,]) 
        alpha <- alpha[[1]]/tay
        mu <- exp(-alpha)
        vu <- (1 - mu)/alpha
        
        s <- mu*E[n - 1] +
            (vu - mu)*z[n - 1,][[1]] +
            (1 - vu)*z[n,][[1]]
        
        }
        
        return(s)
}

diBA <- diff(index(BA))

BA$Alpha <- diff(index(BA))

Price <- BA$GMP[1,][[1]]

EMA17 <- c()
EMA17 <- append(EMA17, Price)

EMA45 <- c()
EMA45 <- append(EMA45, Price)

EMA180 <- c()
EMA180 <- append(EMA180, Price)

numbers  <- length(BA$GMP)

for (n in 2:numbers)  {
        
        v <- EMAFUN(tay = 113*9, BA$GMP,n, EMA17)
        EMA17 <- append( EMA17, v )
        
        v <- EMAFUN(tay = 113*12, BA$GMP,n, EMA45)
        EMA45 <- append( EMA45, v )
        
        v <- EMAFUN(tay = 113*26, BA$GMP,n, EMA180)
        EMA180 <- append( EMA180, v )
}



EMA17  <- xts( EMA17,  order.by = index(BA$GMP[1:numbers]) )
EMA45  <- as.xts( EMA45,  order.by = index(BA$GMP[1:numbers]) )
EMA180 <- as.xts( EMA180, order.by = index(BA$GMP[1:numbers]) )
 
plot( BA$GMP[1:numbers], type = 'p' )

points(EMA17,  type = 'l', col = 'darkred')
points(EMA45,  type = 'l', col = 'darkgreen')
points(EMA180, type = 'l', col = 'darkorange')




#######################################################################################



range <-  MOMENT > "2015-10-02 12:00:00 MSK" &  MOMENT < "2015-10-02 19:00:00 MSK"

zB2 <- zB[MOMENT > "2015-10-02 12:00:00 MSK" &  MOMENT < "2015-10-02 13:00:00 MSK", ]

zB2[ , plot(x = MOMENT, y = PRICE) ]


palitra <- rainbow(n = 10, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)

mV <- zB2[ , ][!is.na(DEAL_ID), max(VOLUME)]

setkey(zB2, MOMENT)

zB3 <- zB2[!is.na(DEAL_ID),  .(sum(VOLUME) ), keyby = .(MOMENT, PRICE)]

setnames( zB3, c('V1'), c('VOLUME') )

zB3[ ,
             points(x = MOMENT, 
                    y = PRICE, 
                    col = 'blue',
                    cex = 10*VOLUME/mV )]

################################################################################

zS2 <- zS[MOMENT > "2015-10-01 12:00:00 MSK" &  MOMENT < "2015-10-01 13:00:00 MSK", ]

zS2[ , points(x = MOMENT, y = PRICE) ]

mV <- zS2[ , ][!is.na(DEAL_ID), max(VOLUME)]

setkey(zS2, MOMENT)

zS3 <- zS2[!is.na(DEAL_ID),  .(sum(VOLUME) ), keyby = .(MOMENT, PRICE)]

setnames( zS3, c('V1'), c('VOLUME') )

zS3[ ,
     points(x = MOMENT, 
            y = PRICE, 
            col = palitra[ floor(10*VOLUME/mV) + 1],
            cex = 10*VOLUME/mV )]

zS3[ ,
     points(x = MOMENT, 
            y = PRICE, 
            col = 'red',
            cex = 10*VOLUME/mV )]

