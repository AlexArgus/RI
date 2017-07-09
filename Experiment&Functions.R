
# Packages that project depends on.

packages <- c('data.table', 'highfrequency', 'xts', 'ggplot2', 'XML',
              'httr', 'grid', 'gridExtra', 'RDS', 'labeling', 'Rcpp',
              'RcppRoll', 'TTR', 'broom', 'scales' )

########## We install the packages that hadn't been installed. #################################

notYetInstalledPackages <- packages[!(packages %in% installed.packages())]

lapply(notYetInstalledPackages, install.packages) 

########## Update all the packages that we need in. ############################################

# lapply(packages, update.packages)

######### Load to R_GlobalEnv pakages that had not been loaded #################################

notYetLoadedPackages <- packages[!(packages %in% gsub( "package:","", search() )[-1])]

lapply(notYetLoadedPackages, require, character.only = TRUE)

################################################################################################

rm( list = ls() ) # clear workspace

plot.new()     # clear the graph window

WorkingDirectory <- 'C:/Users/Alexandr/Desktop/TimeSeriesAnalysis/FinData'

ScripthPath <- paste0(WorkingDirectory, '/Scripts/Real/RI_Project/')

########## Load functions in the Global Environment ############################################

source(paste0(ScripthPath , '/Functions.R'))

################################################################################################

setwd(paste0(WorkingDirectory,'/','Data'))

### Read the data from MICEX ftp server ###

Year <- '2015'

micex_path <- 'http://ftp.micex.com'

pub_folder <- paste0('/pub/info/stats/history/F/', Year, '/')

root_path <- paste0(micex_path, pub_folder)

rm(micex_path)

########### Get all links to MOEX daily data ###################################################

links <- getHTMLLinks(root_path, xpQuery = "//a/@href" )

exr <- paste0('FT', substring(Year, 3) )

exr <- paste0(exr, '(10)+') # Month of data

ftp <- grepl(exr, links)

date_links <- links[ftp]; rm(ftp,links,exr)

########### Main circle of package. Simulate trading day #######################################

for (z in date_links[5]) {  
        
        date_file <- gsub(pub_folder, '', z)        
        
        date <- gsub('FT|.zip','', date_file)
        
        source(paste0(WorkingDirectory,'/','Scripts/Real/RI_Project/TTv4.0.R'))
        
}

################################################################################################

m <- c()

for (x in 10:23) { for ( y in c('00','30') ){ m <- append( m, paste0('T', x, ':', y) ) } }

mt <- c()

for (z in (seq_along(m)-1) ) { mt <- append(mt, paste0(m[z],'/', m[z+1]) )  }

################################################################################################

DDLong <- as.xts(DealsLong, order.by = BeginTimeOfdealsLong)

indexTZ(DDLong) <- TimeZone

GetDDStat(DDLong)

#apply.daily(DDLong,sum)


DDShort <- as.xts(DealsShort, order.by = BeginTimeOfdealsShort)

indexTZ(DDShort) <- TimeZone

GetDDStat(DDShort)

#apply.daily(DDShort,sum)






