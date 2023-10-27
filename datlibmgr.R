
#Fresh R
  #INGREDIENTS (FUN and LIBRARIES YOU'LL NEED)
library(agricolaeplotr)
  library(sp)
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
  #library(rnoaa)
  library(maptools)
  library(rgdal)  #Bindings for the Geospatial data abstraction library
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
library(agricolaeplotr)
library(stringr)
library(RODBC) #access database thing
library(Rcrawler)# web scraper
library(rvest) #web scraper
library(xml2)# needed for rvest functions
library(devtools) #needed for TreeLS Github version of package
library(lidR) #for als and tls tree coords etc
library(sf)#Simple features for R
library(raster)
library(rgeos)
# library(arcgisbinding) #for chm stuff from ivan 6/2023; doesnt work b/c "package ‘arcgisbinding’ is not available for this version of R" but would be useuful to define parts of a las using a shp eventually. it's something esri talks about on its website esri.com somewhere as of 6/2023
library(ggpubr) #for chm stuff from ivan 6/2023
library(whitebox) #for chm stuff from ivan 6/2023
library(future) #for chm stuff from ivan 6/2023
library(writexl) #for chm stuff from ivan 6/2023
library(terra) #for chm stuff from ivan 6/2023
library(stars) #for chm stuff from ivan 6/2023
library(spatialEco) #for chm stuff from ivan 6/2023
library(exactextractr) #for chm stuff from ivan 6/2023
library(googledrive)
library(googleVis)
library(foreign)

select <- dplyr::select #otherwise it calls MASS's select by default
rename <- dplyr::rename #otherwise it calls plyr's rename by default and it doesn't have the same syntax 

wex<- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file="clipboard-32768",sep="\t",row.names=row.names,col.names=col.names,...)  } 


source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}


src<-function(fileend){
  source(paste0(getwd(),fileend))
}

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

