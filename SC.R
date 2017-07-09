

packages <- c('data.table','XML','DBI','RSQLite','sqldf','bit64','tcltk')  

######### Load to Env pakages that were't load ##################################

load_packages <- packages[!(packages %in% gsub( "package:","", search() )[-1])]

lapply(load_packages, require, character.only = TRUE)




setwd('C:/Users/Alexandr/Desktop/TimeSeriesAnalysis/FinData/Data/')

# source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

dbpath <- 'C:/Users/Alexandr/Desktop/TimeSeriesAnalysis/Data/MOEX/RI'

dbname = paste0(dbpath,'/','ri_db')

FuturesCode <- 'RI'

options(digits.secs=3)
TimeZone <- 'Europe/Moscow'
format <- '%F %H:%M:%OS'
format2 <-'%y%m%d%H:%M:%OS'

### Read the data from MICEX ###

root_path <- 'http://ftp.micex.com/pub/info/stats/history/F/2015/'

links <- getHTMLLinks(root_path, xpQuery = "//a/@href" )

ftp <- grepl("FT15(09|10)+", links)   

date_links <- links[ftp]; rm(ftp,links)

#date_links <- date_links[-1] ### delite it after

for (x in date_links[-1]) {
        
        date_file <- gsub('/pub/info/stats/history/F/2015/', '', x)        
        
        date <- gsub('FT|.zip','', date_file)
        
        cat('date:  ', date, '\n')
        
        DateStorageLocation <- paste0(getwd(), '/', date)
        
        filenamesMOEX <- paste0(DateStorageLocation, '/', FuturesCode, '_MOEXDATA_', date, '.rds')
        
        if(!file.exists(filenamesMOEX)){
        
                date <- as.integer(paste0('20',date))
                DateBegin <- as.integer64(date*1e+9)
                
                date <- as.integer(date) + 1L
                DateEnd   <- as.integer64(date*1e+9)
                
                con <- dbConnect(RSQLite::SQLite(), dbname = dbname)
                
                st <- paste0('select * from RI_1510 WHERE moment BETWEEN ','\'',
                             DateBegin, '\'', ' AND ', '\'', DateEnd, '\'')
                
                TradeDay <- sqldf(st, dbname = dbname)
                
                dbDisconnect(con)
                
                TradeDay <- as.data.table(TradeDay)
                
                TradeDay[ , MOMENT := as.integer64(MOMENT)][ 
                          , MOMENT := as.character(MOMENT)][ 
                          , MOMENT := paste0(substr(MOMENT, start = 1, stop = 14), '.', 
                                             substr(MOMENT, start = 15, stop = 17))][
                          , MOMENT := as.POSIXct(strptime(MOMENT, 
                                                          format = '%Y%m%d%H%M%OS',
                                                          tz = "Europe/Moscow")) ]  
                
                
                
                code <- TradeDay[ , unique(X.SYMBOL)];  V=0
                
                for (c in code) {
                        
                        assign('Activ', TradeDay[X.SYMBOL == c] )
                        CV <- Activ[DEAL_ID != 'NA' , sum(as.numeric(VOLUME))] 
                        if (!is.na(CV) &  CV>V){V=CV;CodeWithMaxVolume=c}
                        cat("c is  ", c, "CV is  ", CV, '\n')
                        
                }
                
                print(CodeWithMaxVolume)
                
                ####################
                
                code <- CodeWithMaxVolume
                
                TradeDay <- TradeDay[X.SYMBOL == code]
                
                TradeDay <- TradeDay[, .(TYPE, DEAL_ID, PRICE, VOLUME, MOMENT)]
                
                ####
                
                
                if( !dir.exists( DateStorageLocation ) ){
                        
                        dir.create(DateStorageLocation)
                }
                
                
                saveRDS(TradeDay, file = filenamesMOEX)
                
                #readline("Press <return> to continue")
        
        }
        
}







