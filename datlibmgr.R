#Fresh R
#INGREDIENTS (FUN and LIBRARIES YOU'LL NEED)
library(ggrepel)
library(ggpmisc)
library(PerformanceAnalytics)
library(chron)
library(broom)
library(data.table) #YOU'LL NEED THE BUILT IN DATA.TABLE PACKAGE FOR FREAD
library(ggplot2)
library(lme4)
library(magrittr)
library(metafor)
library(nlme)
library(readr) #read_csv is from here
library(reshape) #YOULL NEED THIS FOR CAST
library(splitstackshape)
library(weights)
library(readxl)    
library(devtools)
library(leaflet)
library(xts)
library(rnoaa)
library(maptools)
library(rgdal)
library(dismo)
library(rgl)
library(leaps)
library(plotly)
library(rsm)
library(tidyr)
library(lubridate)
library(robfilter)
library(XLConnect)
library(EMSaov)
library(car)
library(boot)
library(ggsignif)
library(lsmeans)
library(multcompView)
library(tibble)
library(purrr)
library(car)
library(proto)
library(gridGraphics)
library(phia)
library(ggstance)
library(lmerTest)
library(multcomp)
library(egg)
library(pracma)
library(plyr);library(dplyr) #YOULL NEED THIS FOR JOIN
library(Hmisc)
library(datapasta)
library(stringi)
library(xfun)   
library(rlang)  
library(digest) 
library(mime) 
library(htmltools) 
library(rmarkdown)
library(ggpattern)
library(Rcpp)
library(sf) #errors as of 9/27 with "install.packages(sf)
#remotes::install_github("coolbutuseless/ggpattern") #ggpattern had some problems, at one point this fixed it
library(NCmisc)
library(RODBC)
library(stringr)
library(agricolaeplotr)
library(qpdf)
library(rvest) #web scraper
library(Rcrawler) #web scraper
library(xml2) #needed by rvest i think
library(RCurl)  #more web scraping stuff
library(httr) #needed for logins to websites and website sessions at least
library(gsheet) #read in google sheets
library(rlang)
library(googlesheets4) #needed for reading googlesheets (maybe better than gsheet)
library(googledrive) #needed for getting file, folder etc info from google drive
library(gmailr) #read and write gmails. needs some python to google api link I dont have time for as of 5/2/2023 so it doesn't do anything yet
library(urltools) #for getting domain from url among other things
library(googleVis) #get tables in html format
library(docxtractr)
library(tesseract) #for ocr    
library(viridis)           #colors for ggplot
#install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_1.1.0.tar.gz", repos = NULL, type="source")
library(dbscan) #for FPCALSpackage
library(deldir)#for FPCALSpackage
library(geometry)#for FPCALSpackage
library(shiny)#for FPCALSpackage
library(shinyFiles)#for FPCALSpackage
library(shinyjs)#for FPCALSpackage
library(terra)#for FPCALSpackage
library(RCSF)#for FPCALSpackage
library(randomForest)#for FPCALSpackage
#library(FPCALSpackage)#for FPCALSpackage not avail for this version of r on home computer... fix this eventually
library(RSelenium) #driving webpages
library(qdap) #gsub but works on vectors of patterns
library(foreign) #for read.dbf
library(gdata) #for left() and right()
#Delete eventually if XLconnect doesnt give problems... as of 6/21 it was not installing or loading into the library for some reason so i needed to do this:
#require(devtools)
#install_version("XLConnect", version = "1.0.2", repos = "http://cran.us.r-project.org")
#Mend delete

select <- dplyr::select #otherwise it calls MASS's select by default

wex<- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file="clipboard-65536",sep="\t",row.names=row.names,col.names=col.names,...)  } 


source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

#Get this working sometime. doesnt seem to work at least on longer paths???? not sure about shorter ones so try usuing it on shorter ones first
#src<-function(file_end){
#  source(paste0(getwd(),readClipboard()))
#}
if("you wnat to do flashcards"==T){
  
  ANSWER<-"yes"
  #loops thru and gives one column and the same row next too it one by one
  fun <- function(dataf) {
    ANSWER <- readline(cat("Are you a satisfied R user?\n\n\n "))
    if (substr(ANSWER, 1, 1) == "n")
      as.character(print((dataf[i-1,"cyl"])))
    else
      as.character(print((dataf[i,1])))
    as.character(print(rownames(dataf[i,])))
  }
  
  #works dont fuck
  for(i in c(1:4)){
    fun(dataf = mtcars)
  }
  
  
  
}


# Colorblind pallette to use with scale_fill_manual(values=cbPalette) for shape fills and scale_colour_manual(values=cbPalette) for  line and point colors
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#shell.exec that works from the working dir instead of a full path
wael<-function(pathstub=NULL){ #Wael Sawan is the CEO of Shell plc (Shell oil) as of 2023
  if(is.null(pathstub)){shell.exec(getwd())}
  else shell.exec(normalizePath(pathstub))
}

#Got narm from NCmisc https://cran.r-project.org/web/packages/NCmisc/index.html  , needed it to remove na's from a list during silvprocess.r
narm=function (X) 
{
  if (is.data.frame(X) | is.matrix(X)) {
    X <- na.exclude(X)
    attr(X, "na.action") <- NULL
    return(X)
  }
  else {
    if (is.vector(X)) {
      if (is.list(X)) {
        if (any(sapply(X, length) > 1)) {
          X <- lapply(X, narm)
          return(X)
        }
        else {
          return(X[!is.na(X)])
        }
      }
      else {
        if (is.character(X)) {
          out <- X[!is.na(X)]
          out <- out[out != "NA"]
          return(out)
        }
        else {
          return(X[!is.na(X)])
        }
      }
    }
    else {
      warning("unsupported type, X unchanged")
      return(X)
    }
  }
}

tempdir()
dir.create(tempdir())

