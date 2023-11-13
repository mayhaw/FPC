source("Q:/My Drive/Library/datlibmgr.R")
#rwdb gold------
#just define the connection to the db from R; doesn't create an object in the global env that has actual rwdb data in it
dbpath=normalizePath(paste0(gsub("\\\\Documents","",Sys.getenv("HOME")),("\\Dropbox (FPC)\\FPC Team Folder\\Static RWDB\\RWDB static 20221017.mdb")))
conn<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dbpath))
#This definitely works on FRANCE pc (laptop) 11/13/23; idk why it wont on the desktop cnr grad one

#not sure what this one is for
#conn<-odbcDriverConnect(  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")#20221017
#20221017.mdb, seanb
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")
#20221017.mdb, sabloszi
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")
#20221017.mdb, seanb?
#conn<-odbcConnect("rwdb2022")   #new error 3/16/22- went to this b/c the driver path was "unknown" somehow
#20210423.mdb, sabloszi
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb")#
#11156 in 2021, 11336 in 2022
#11156 -11336 =180
#see tables thats in it
tblnams<-sqlTables(conn, tableType = "TABLE")$TABLE_NAME
#create indiv tables as dfs
nut<-sqlFetch(conn, "dbo_NUT_SOIL")
dam<-sqlFetch(conn, "dbo_DAMAGE")
splloc<-sqlFetch(conn,"dbo_SAMPLING_LOCATION")
tree<-sqlFetch(conn,"dbo_TREE_GROWTH")
stdy<-sqlFetch(conn,"dbo_STUDY_INFO")
trts<-sqlFetch(conn,"dbo_TREATMENTS")
sqlFetch(conn,"dbo_ACTIVITY_WORKPLAN")%>%wex()
depths<-sqlFetch(conn,"dbo_DEPTH_CODES")
sqlFetch(conn,"dbo_COMPANY_OPERATIONS")%>%
  subset(OPERATION_ID!=140)%>%
  subset(.,grepl("cock",OPERATION_NAME,ignore.case=T))
grep
wex(sqlFetch(conn,"dbo_DOMINANT_HEIGHT_1"))
wex(sqlFetch(conn,"dbo_APPLIED_TREATMENTS"))
(sqlFetch(conn,"dbo_SAMPLING_LOCATION"))

#get all the names of every column in every table 
framnams1<-sapply(tblnams[1:31],function(x){names(sqlFetch(conn,x))})
framnams2<-sapply(tblnams[32:61],function(x){names(sqlFetch(conn,x))}) #its in two parts bc it takes for ever on my computer at least
#put whatever you want to search for in the grepl() function
findnames<-function(x){grepl("(?i)REP", x)}
#this shows you which tables have a column with that search in it
lapply(framnams2,FUN = function(x){sum(findnames(x))})

#nothing for spa
#TPA
franmams$dbo_DOMINANT_HEIGHT_1 #TPA_SUM, only one
#PER
franmams$dbo_STUDY_INFO #OPERATION_ID, ONLY ONE
franmams$dbo_MICRO_TISSUE #COLLECTIONS PER YEAR, ONLY ONE
franmams$dbo_MACRO_TISSUE #COLLECTIONS PER YEAR, ONLY ONE
franmams$dbo_COMPANY_OPERATIONS #OPERATION_ID OPERATION NAME, ONLY 2
#DENS
#ONLY BULK DENSITY IN DBOPHYSSOIL



grepl("(?i)regimen|promocion", c("Regimen","regimen","reximen"))


#rw18s
rw18old%>%pull(DATE_COLLECT)%>%unique()
rw18old<- 
  nut%>%
  subset(.,STDY%in%c(184401)&DEPTH==2)%>%
  pull(c(PLOT))%>%
  expand.grid(5:9,.)%>%
  mutate(.,STDY=184401,SAMPLING_LOCATION=3)%>%
  select(STDY,Var2,Var1,SAMPLING_LOCATION)

select(c(STDY,PLOT,DEPTH,DATE_COLLECT,SAMPLING_LOCATION))
#d
#get yst, sample #, and plots from new db
nut%>%
  subset(.,substr(STDY,1,2)=="18"&YST>=0)%>%
  select(c(STDY,YST))%>%
  melt(.,id.vars="STDY")%>%
  cast(.,STDY~value)%>%
  print()
head()
wex()

#md
rw18vt<-nut%>%
  subset(.,substr(STDY,1,2)=="18"&YST=?????|grepl("ty_soil",Comments))


rw18old2<-rw18old%>%select(c(STDY,YST,PLOT,DEPTH,DATE_COLLECT))
rw18new2<-rw18new%>%select(c(STDY,YST,PLOT,DEPTH,`SAMPLE_#`))

merge(rw18old2,rw18new2,by=c("STDY","YST","PLOT","DEPTH"))%>%
  mutate(.,PLOT=as.integer(substr(PLOT,2,4)))%>%
  select(c(STDY,PLOT))%>%unique()->fuckdb
wex()


pull(Comments)%>%unique()
select(c(STDY,PLOT,DATE_COLLECT,`SAMPLE_#`,DEPTH))%>%
  pull(DATE_COLLECT)%>%unique()
unique()%>%
  wex()
#rw19s
tree%>%
  subset(.,STDY==195501&YST==12)%>%select(c(PLOT))%>%arrange(PLOT)%>%unique()

stdy%>%subset(.,STDY==195501)%>%print()#names()->nms

nut%>%subset(.,STDY==185302&YST>6)%>%wex()
print()#names()->nms


nms[grepl("THIN",nms)]

#rw18s
nut%>%
  subset(.,STDY%in%c(994003,994004))%>%
  select(c(STDY,PLOT,DATE_COLLECT,`SAMPLE_#`,DEPTH))%>%
  unique()%>%
  wex()



dim()
pull(STDY)%>%
  substr(.,1,2)=="18"


dim()
unique()

subset(.,STDY==994002|STDY==9180601|STDY==9181201|STDY==9201302)%>%
  group_by(YST)%>%
  summarise(range=max(`DATE_COLLECT`))

20385-20432

wex()
dim()
subset(.,year(DATE_COLLECT)==2013|(STDY==201302&year(DATE_COLLECT)!=2218)&DEPTH==8&PLOT%in%c(1520,1521,2520,2521,3520,3521))%>%
  subset(.,(STDY>(181200)&DEPTH<7)|STDY%in%c(180601,201302))%>%
  select(YST,STDY,DATE_COLLECT,PLOT,`SAMPLE_#`,DEPTH)%>%
  str()
wex()
head()%>%
  
  pull(DATE_COLLECT)%>%
  names()
subset(.,STDY==201302&YST>8)%>%
  wex()
unique()%>%
  arrange(STDY,YST)
gdata::left(3)
wex(stdy)
#6	3 reps, MCP, hi and lo silviculture, 500 tpa
#pick up----
#just got va and v, mkae sure they make sense then go ahead and make figures for one or both and do anova
#setwd
#setwd("C:/Users/seanb/")
#setwd("C:/Users/sabloszi")



#readin----

trd<-read_excel(paste0(getwd(),"/Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC/RegionWide Trials/RW18/185201ECM.xlsx"),
                sheet="185201",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                trim_ws = T )

names(trd)<-tolower(names(trd))
names(trd)<-gsub(" ","_",names(trd))

trd<-trd%>%
  as.data.frame()%>%
  select(c(plot,dbh,new_dbh,ht,new_ht,htlc,new_htlc,tree_no))

#Need to get plot sizes
plots <- odbcConnectAccess2007("./Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb")
trd<-sqlFetch(plots, "dbo_PLOT_INFO")%>%
  subset(.,STDY==185201)%>%
  select(c(PLOT,SIZE))%>%
  merge(trd,.,by.y="PLOT",by.x="plot")

if("you want dummy size data just to get graphs templated"==T){
  trd<-mutate(trd,SIZE=1) # just needed this for the pc at home since i dont have ms access on it so it cant read from the rwdb
}

trd<-merge(trd,read.csv("./Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC/RegionWide Trials/RW18/185201ECM_trts.csv",header=T),by="plot")%>%
  select(-c(Study,TempID,Pan,Waters.,Arc.Vial.,Ziploc.,Notes))%>%
  mutate(.,block=substr(plot,1,1),new_ht=as.numeric(new_ht))%>%
  subset(.,complete.cases(new_ht))%>%
  melt(.,id.vars=c("plot","tree_no","block","trt","SIZE"))%>%
  mutate(.,variable =gsub("new_","2022_",variable))%>%
  mutate(.,variable=ifelse(nchar(variable)<=4,paste0("2016_",variable),variable))%>%
  cSplit(.,"variable","_")%>%
  rename(.,year="variable_1",var="variable_2")%>%
  mutate(.,year=as.factor(year),value=as.numeric(value))%>%
  mutate(.,year=factor(year,levels=c(2016,2022)))%>%  
  mutate(.,block=factor(block,levels=c(1,2)))%>%  
  cast(.,formula=plot+tree_no+block+trt+year+SIZE~var)%>%
  # mutate(.,dbh=dbh*2.51)%>%#convert inches to cm
  #  mutate(.,ht=ht*12*2.51/100)%>%#convert feet to m
  #  mutate(.,htlc=htlc*12*2.51/100)%>%#convert feet to m
  mutate(.,trt=revalue(as.factor(trt), c(
    #  "0N+0P"="0N+0P",   "360N+36P"="436N+44P",    "720N+72P2yr" = "871N+87P, 2 yr","720N+72P4yr" = "871N+87P, 4 yr"  )))%>%#1 lb/ac =  1.121 kg/ha
    "0N+0P"="0N+0P",   "360N+36P"="360N+36P",    "720N+72P2yr" = "720N+72P, 2 yr","720N+72P4yr" = "720N+72P, 4 yr"  )))%>%# just change years not units
  mutate(.,rat=  sub(pattern = "(^\\d+\\N).*", replacement = "\\1", x = trt))%>%
  mutate(.,rat=  as.factor(sub(pattern="N",replacement = "",rat)))%>%
  mutate(.,frq= as.factor(
    ifelse(str_detect(trt,"4 yr"),4,2)))%>%
  #NOTE: Plots 2206 and 2218 were thinned with the other plots but 1206 and 1218 were left unthinned (ISR185201-2016-version.pdf)
  #michael farmer should use this for  volume for tree growth in varrate
  mutate(.,v=ifelse(plot==1206, #this gives you volume per tree
                    (0.21949+(0.00238 * dbh * dbh * ht)), #ttrue, unthined, Units will be vol in ft3/tree, dbh in inches, ht in ft.  
                    (0.25663+(0.00239 * dbh * dbh * ht)) #false, thinned 
                    
  ))

#example for one tree
# (10*10*50*0.002)+0.25

#aggregate the data to get means by plot----
trd2<-
  trd%>%
  merge(.,dplyr::summarize(group_by(.,year,plot), 
                           v_m=mean(v,na.rm=T), #mean volume per tree, useful on its own
                           v_s=sum(v,na.rm=T), #sum volume per plot so you can get on aerial basis later with the plot size as a denominator #note there is waaaay higher vol per plot and vol per area for 1206 because it never got thinned but this is correct. this is true for both 2016 and 2022
                           ht_m=mean(ht,na.rm=T), #mean ht
                           dbh_m=mean(dbh,na.rm=T)), #mean dhb
        by=c("year","plot"),all.y=T)%>%
  mutate(.,vm2=cumsum(!duplicated(interaction(year,plot))))%>%  
  subset(.,!duplicated(vm2))%>%
  mutate(.,va=v_s/(SIZE*2.471))%>% #get volume per acre but convert ha size in RWDB to ac bc 2.471 ac in a ha
  #  mutate(.,vamha=(v_s/(SIZE*2.471))*0.028317*2.471)%>% #convert to cubic meters per ha to compare with figures in tims 2015 paper in forests journal
  select(-c(vm2,v,v_s,dbh,ht,htlc,tree_no) )#(get rid of aggregation indicator vm2, and v, which is leftover individual tree data), and v_s, which is per plot volume which lacks context, and dbh, ht, and htlc which are leftover individual rtree data


#get growth (difference over 6 years) data-----
trd2<-melt(trd2,measure.vars = c("v_m","ht_m","dbh_m","va"))%>%
  cast(.,plot+block+trt+SIZE+rat+frq+variable~year)%>%
  rename(.,"x2016"="2016","x2022"="2022")%>%
  mutate(.,g=(x2022-x2016)/6)%>% #growth for whatever parameter is differecn between years divided by six years
  as.data.frame()%>%
  melt(.,measure.vars=c("x2016","x2022","g"),variable_name="year")%>% #
  cast(.,plot+block+trt+SIZE+rat+frq+year~variable)


tpp<- trd%>%as.data.frame%>%#tpp trees per plot
  melt(.,id.vars=c("year","plot"))%>%
  subset(.,variable=="tree_no")%>%
  cast(.,year~plot)%>%
  mutate(.,year=paste0("x",year))%>%
  melt.data.frame(.,id.vars = "year",variable_name = "plot")%>%
  rename(.,trees="value")
#you get an aggregation warning, but that is exactly what you want so ignore

#1 boxplots with whiskerys-----
if("you just want one plot at a time and you want boxes"==T){
  #this is pretty much there, just make more and pretty it up, chage the colors etc
  cbbPalette <- c( "#B9975B","#115740") #need for colors
  trd%>%
    melt(.,id.vars=c("plot","tree_no","block","trt" ))%>%
    mutate(.,variable =gsub("new_","2022_",variable))%>%
    mutate(.,variable=ifelse(nchar(variable)<=4,paste0("2016_",variable),variable))%>%
    cSplit(.,"variable","_")%>%
    rename(.,year="variable_1",var="variable_2")%>%
    mutate(.,year=as.factor(year),value=as.numeric(value))%>%
    subset(.,var=="ht")%>%
    mutate(.,year=factor(year,levels=c(2016,2022)))%>%  
    mutate(.,block=factor(block,levels=c(1,2)))%>%  
    ggplot(., aes(x = trt, y = value, color = year)) +
    geom_boxplot(outlier.size = 0,position=position_dodge(width=1),size=1.5) +
    geom_point(alpha=0.6,position = position_jitterdodge(dodge.width = 1),
               size=3,aes(shape=factor(block),group=year))+
    scale_colour_manual(values=cbbPalette)+
    theme(      legend.background = element_rect(fill="white"),
                legend.position=c(1,1), legend.justification=c(1.4,3.4))+
    labs(shape="Block",color="Year")+ #legend label
    theme_bw()
}
#one plot at a time but with standard error se 's------
#pick up getting the mean and st err bors to work and put in ggplot below


if("you just want one plot at a time"==T){
  #this is pretty much there, just make more and pretty it up, chage the colors etc
  cbbPalette <- c( "#B9975B","#115740") #need for colors
  trd2%>%
    mutate(.,block=factor(block,levels=c(1,2)))%>%  
    dplyr::group_by(year,trt)%>%
    dplyr::summarize(., mean=mean(va,na.rm=T),se=
                       sd(va,na.rm=T)/sqrt(n()))%>%#std.error or se
    as.data.frame()%>% #this is where you would merge in letters if you get means seps
    #Then the chart of this:
    mutate(.,year=factor(year,levels=c(2016,2022)),mean=as.numeric(mean))%>%  
    #print()
    ggplot(., aes(x = trt, y = mean, color = year)) +
    #geom_point(color="black",position=position_dodge(0.9),stat="identity")+
    geom_point(position=position_dodge(width=0.9))+
    geom_errorbar(position=position_dodge(0.9),aes(ymax=mean+se,ymin=mean-se,width=0.1))+#ymin=mean+se,
    scale_colour_manual(values=cbbPalette)+
    theme(      legend.background = element_rect(fill="white"),
                legend.position=c(1,1), legend.justification=c(1.4,3.4))+
    labs(shape="Block",color="Year")+ #legend label
    theme_bw()
}  





#2 all data------
soil_vars<-function(svariable,otherthings){
  trd%>%
    #Now the actual plot part:
    ggplot(., aes(x=trt, y={{svariable}},color = year)) + #dbh, ht, htlc
    geom_boxplot(outlier.size = 0,position=position_dodge(width=1),size=1.5) +
    geom_point(alpha=0.6,position = position_jitterdodge(dodge.width = 1),
               size=3,aes(shape=factor(block),group=year))+
    scale_colour_manual(values=cbbPalette)+
    theme(      legend.background = element_rect(fill="white"),
                legend.position=c(1,1), legend.justification=c(1.4,3.4))+
    labs(shape="Block",color="Year")+ #legend label
    xlab("Treatment")+
    theme_bw()+
    theme_poster()+
    ylab(ifelse(otherthings=="dbh",expression(paste("Tree diameter (DBH, in)")),
                ifelse(otherthings=="ht",expression(paste("Tree height (ft)")),
                       ifelse(otherthings=="htlc",expression(paste("Height to live crown (ft)")),"you fucked up")
                )))
}

#Now actually run this function on each piece of data. I couldnt figure out how to get the same vector as both a vector name and a character string that would trigger stuff so thats why theres the same word repeated twice
dbh			    <-	soil_vars(svariable =  		dbh	, otherthings = "dbh")
ht			    <-	soil_vars(svariable =  		ht	, otherthings = "ht")
htlc			    <-	soil_vars(svariable =  		htlc	, otherthings = "htlc")

ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_years_dbh.png"	,device="png",     width = 15, height = 8,plot=	dbh ,units="in"	)
ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_years_ht.png"	,device="png",     width = 15, height = 8,plot=	ht ,units="in"	)
ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_years_htlc.png"	,device="png",     width = 15, height = 8,plot=	htlc ,units="in"	)


#2.1 all variables but just means instead of raw points------
soil_vars<-function(svariable,otherthings){
  trd%>%
    #Now the actual plot part:
    ggplot(., aes(x=trt, y={{svariable}},color = year)) + #dbh, ht, htlc
    geom_boxplot(outlier.size = 0,position=position_dodge(width=1),size=1.5) +
    geom_point(alpha=0.6,position = position_jitterdodge(dodge.width = 1),
               size=3,aes(shape=factor(block),group=year))+
    scale_colour_manual(values=cbbPalette)+
    theme(      legend.background = element_rect(fill="white"),
                legend.position=c(1,1), legend.justification=c(1.4,3.4))+
    labs(shape="Block",color="Year")+ #legend label
    xlab("Treatment")+
    theme_bw()+
    theme_poster()+
    ylab(ifelse(otherthings=="dbh",expression(paste("Tree diameter (DBH, in)")),
                ifelse(otherthings=="ht",expression(paste("Tree height (ft)")),
                       ifelse(otherthings=="htlc",expression(paste("Height to live crown (ft)")),"you fucked up")
                )))
}

#Now actually run this function on each piece of data. I couldnt figure out how to get the same vector as both a vector name and a character string that would trigger stuff so thats why theres the same word repeated twice
dbh			    <-	soil_vars(svariable =  		dbh	, otherthings = "dbh")
ht			    <-	soil_vars(svariable =  		ht	, otherthings = "ht")
htlc			    <-	soil_vars(svariable =  		htlc	, otherthings = "htlc")

ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_dbh.png"	,device="png",     width = 15, height = 8,plot=	dbh ,units="in"	)
ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_ht.png"	,device="png",     width = 15, height = 8,plot=	ht ,units="in"	)
ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_htlc.png"	,device="png",     width = 15, height = 8,plot=	htlc ,units="in"	)



#2.2 just growth measurements (means of those by trt)-----
#givemn makes mns for the bars themselves
give.mn <- function(x){ 
  return(data.frame(y = .8*mean(x), label = round(mean(x),1)))#change to 0 or 1 for ht, dbh vs volume
}

soil_vars<-function(svariable,otherthings){
  trd2%>%
    rename(.,Year="year")%>%
    subset(.,Year=="g")%>%
    #as.data.frame()%>% #this is where you would merge in letters if you get means seps
    ggplot(., aes(x = trt,  y={{svariable}},fill = Year)) + #need for colors- wolfpack red for the one time (growth) figures 
    stat_summary(position=position_dodge(width=0.9),fun.data=mean_se, geom="errorbar",  # #mean_se is built in to give se 
                 width=.2, color="black",size=2) + # 
    stat_summary(fun=mean, geom="bar", pch=21, width = 0.6, size=2,position=position_dodge(width=0.9),aes(fill=Year))+#aes(fill="#CC0000")
    stat_summary(fun.data = give.mn, geom = "text",color="white",size=10)+
    scale_fill_manual(values = c("#CC0000","#CC0001","#CC0002"),guide = "none")+
    xlab("")+
    theme_bw()+
    theme_poster()+
    theme(      legend.background = element_rect(fill="white"),
                #legend.position=c(1,1), legend.justification=c(1.4,3.4),
                legend.position = "right",
                axis.title.y= element_text(size=18),
                axis.text.x = element_text(angle = 20, hjust = 1))+
    ylab(ifelse(otherthings=="va",expression(paste("Volume growth (",ft^3, " " , ac^-1, yr^-1,")")),  
                ifelse(otherthings=="ht_m",expression(paste("Height growth (",ft, " " , yr^-1,")")),  #tims 2015 forests paper uses "height growth" in table six
                       ifelse(otherthings=="dbh_m",expression(paste("Diameter growth (in ", yr^-1, ")")),"you fucked up")
                )))
}
#v_m mean vol per tree in each plot and va plot scale volume/ac are the ones we want


va<-soil_vars(svariable =  		va	, otherthings = "va")
ht_mg<-soil_vars(svariable =  		ht_m 	, otherthings = "ht_m")
dbh_mg<-soil_vars(svariable =  		dbh_m, otherthings = "dbh_m")
if("youre wanting to save, pardner"==T){
  ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_growth_va.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	va ,units="in"	)
  ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_growth_ht.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	ht_mg ,units="in"	)
  ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_growth_dbh.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	dbh_mg ,units="in"	)
}

#2.4 just yearly measurements, and just means of those-----


#need -  
#vol/ac/yr.  
# 2 panels 
# growth in dbh (inches/yr) 
#growth in and ht (ft/yr) from 2016 to 2022. 
give.mn <- function(x){
  return(data.frame(y = .8*mean(x), label = round(mean(x),0)))
}
soil_vars<-function(svariable,otherthings){
  trd2%>%
    rename(.,Year="year")%>%
    subset(.,Year=="x2022")%>%
    #as.data.frame()%>% #this is where you would merge in letters if you get means seps
    ggplot(., aes(x = trt,  y={{svariable}})) +
    stat_summary(position=position_dodge(width=0.9),fun.data=mean_se, geom="errorbar",  # #mean_se is built in to give se 
                 width=.2, color="black",size=2) + # 
    stat_summary(fun=mean, geom="bar", pch=21, size=7,width = 0.6,fill="#284265")+##284265 is mcc blue gray 
    stat_summary(fun.data = give.mn, geom = "text",color="white",size=10)+
    xlab("")+
    theme_bw()+
    theme_poster()+
    theme(      legend.background = element_rect(fill="white"),
                #legend.position=c(1,1), legend.justification=c(1.4,3.4),
                legend.position = "right",
                axis.title.y= element_text(size=18),
                axis.text.x = element_text(angle = 20, hjust = 1))+
    ylab(ifelse(otherthings=="ht_m",expression(paste("2022 height (ft)")),
                ifelse(otherthings=="dbh_m",expression(paste("2022 diameter (in)")),
                       ifelse(otherthings=="htlc_m",expression(paste("htlc (ft)")),"you fucked up")
                )))
}
#pick up seeing if these look ok then save them
ht_m<-soil_vars(svariable =  		ht_m, otherthings = "ht_m")
dbh_m<-soil_vars(svariable =  		dbh_m	, otherthings = "dbh_m")
if("you want to save these"==T){
  ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_dbh.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	dbh_m,units="in"	)
  ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_ht.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	ht_m,units="in"	)
}



#2.3 just tpa trees per acre-----



tpa<-  trd2%>%
  merge(.,tpp,by=c("year","plot"))%>%
  select(c(year,plot,trees,SIZE,trt,block))%>%
  subset(.,year=="x2022")%>%
  mutate(.,tpa=trees/(SIZE*2.471))%>%
  rename(.,Block="block")%>%
  # print()
  #as.data.frame()%>% #this is where you would merge in letters if you get means seps
  ggplot(., aes(x = trt,  y=tpa, fill = Block)) +
  #stat_summary(position=position_dodge(width=0.9),fun.data=mean_se, geom="errorbar",  # #mean_se is built in to give se 
  #width=.2, color="black",size=2) + # 
  #stat_summary(fun=mean, geom="bar", pch=21, size=7,aes(fill=factor(Year)),position=position_dodge(width=0.9))+
  geom_bar(position=position_dodge(width=0.9),stat="identity",)+
  #geom_text(stat="identity",size=10,label=({{svariable}}))+
  geom_text(aes(x=trt,y=((0.8*tpa)),label=round(tpa,0)), 
            position=position_dodge(.9), size=10,color="white")+
  
  scale_fill_manual(values= c("#093e81","#18173b","#B9975B"))+ ##093e81 random color from MCC website that matches official dark blue
  labs(color="Block")+ #legend label
  xlab("")+
  theme_bw()+
  theme_poster()+
  theme(      legend.background = element_rect(fill="white"),
              #legend.position=c(1,1), legend.justification=c(1.4,3.4),
              legend.position = "right",
              axis.title.y= element_text(size=18),
              axis.text.x = element_text(angle = 20, hjust = 1))+
  ylab(expression(paste("2022 trees ",ac^-1)))

ggsave(filename=	"G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder/RegionWide Trials/RW18/185201 Wadeville NC/rw18_5201_share/Chandler_Poster-185201_2022_tpa.png"	,device="png",     width = 8, height = 8/1.618033988749894,plot=	tpa,units="in"	)





#3 anova-----
trd2%>%
  lmer((v_m)~(frq+rat)*year+(1|block/frq:rat),    #don't fuck with this 
       data=.,                        #this gives 3 ddf for frq and rat (freq:rat-1), 4 for year (4*((blocks-1)*(year-1))
       na.action = na.exclude)->trd_all_trts

#Second do an anova (IMPORTANT: not saved anywhere, just run it to see in the console)
anova(trd_all_trts)%>%
  tidy(.)%>%
  mutate(.,sig=symnum(p.value, corr = FALSE, na = FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","."," ")),
         fvsigfig=signif(statistic,2))#%>%wex() #this is whats in the pub
#Takehomes: clt:labtr is significant at 0.05 * 

##151.         Means sep for "lbcextr" soil data-----

#Note this is all pristine, ie. exactly what is in the 
#C:\Users\seanb\Documents\words\My Career\NCSU\Studies\LabileC\Presentable\MBC Plots\2020Redo_soil_extrns\LabC_2020Redo_soil_extrns_all_vars_means_sep_letters.pptx
#... powerpoint. Ie there is no any letter on that powerpoint that didnt come from the following outputs

#1. Grvwat 
#samp:year #p=2.18E-08 ***  maineff_for_trt_w_sampbyyear # all samps in model- extremely significant
lbcextr2%>% 
  lmer(grvwat~trt+samp*year+(1|block), data=., #
       na.action = na.exclude,
       control = lmerControl(optimizer ="Nelder_Mead"))%>% #model fails to converge w/o this
  lsmeans(.,pairwise~samp*year,adjust="none")%>%
  cld(.,
      by="year",
      alpha=0.05,
      Letters=letters,      ### Use lower-case letters for .group
      adjust="none",       ### Tukey-adjusted or bonferroni comparisons show all a's (i.e. no means difference detected) #"G:\My Drive\Library\Graphpad_multiple_comparisons.pdf" shows how to do these without adjustments/what to call them etc
      rev=T)
#takehomes: Theres only the significant samp:year effect on grvwat (also samp and year but there is this interaction so i am ignoring those)




#999 delete-----
#pick up 8/29
tim<-read.csv("C:/Users/sabloszi/Downloads/20220825 Sample Numbers for samples sent and to be sent to Waters.csv",header = T)
sea<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header = T)
#merge these so that you have the sea df as is but it has sample # on it for all
#"varrate" throws of plot and stdy str on read.csv b/c names are fucked up combo of numbers and letters. Fix this:
sea<-sea%>%
  mutate(.,PLOT=as.integer(PLOT))%>%
  mutate(.,STDY=as.integer(STDY))

#now merge all the important columns from each
new<-merge(sea,      tim[tim$DEPTH=="0-15",c("STDY","PLOT","SAMPLING_LOCATION","DATE_COLLECTED","SAMPLE_.","yst")],
           by=c("STDY","PLOT","SAMPLING_LOCATION","DATE_COLLECTED"),all.x=T)%>%
  mutate(.,SAMPLE_N=coalesce(SAMPLE_..x,SAMPLE_..y))

#now just check that the plots from the original carried over into this new 
#Also, the selection of the new df below is more convenient b/c it doesnt have the sample numbers from both me and tim
new[,c(names(sea)[names(sea)!="SAMPLE_."],"SAMPLE_N","yst")]%>%arrange(DATE_COLLECTED,PLOT)%>%wex
wex(sea%>%arrange(DATE_COLLECTED,PLOT))

new[,c(names(sea)[names(sea)!="SAMPLE_."],"SAMPLE_N","yst")]%>%arrange(DATE_COLLECTED,PLOT)%>%
  subset(.,!is.na(STDY))%>%#don't want var rate yet, no sample numbers as of8/29/22
  subset(.,!(PLOT==1624&STDY==185201))%>%#don't want this one plot brody sampled for soil in Mt Gilead and we never went back for tree measurements
  subset(.,!(SAMPLING_LOCATION=="INTER BED"&(STDY==282401|STDY==284201)))%>%#don't want interbed from Jacobs samples from june 2022 in these two studies
  wex()

read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header = T)%>%
  mutate(.,PLOT=as.integer(PLOT))%>%
  mutate(.,STDY=as.integer(STDY))%>%
  arrange(DATE_COLLECTED,PLOT)%>%
  subset(.,!is.na(STDY))%>%#don't want var rate yet, no sample numbers as of8/29/22
  subset(.,!(PLOT==1624&STDY==185201))%>%#don't want this one plot brody sampled for soil in Mt Gilead and we never went back for tree measurements
  subset(.,!(SAMPLING_LOCATION=="INTER BED"&(STDY==282401|STDY==284201)))%>%#don't want interbed from Jacobs samples from june 2022 in these two studies
  select(.,-c(PLOT_FIELD_LABEL,NeedsCollectedForFunga,pkged4funga))%>% #these don't mean anything to funga but are important for FPC
  mutate(.,shipment=1001)%>%#sampling dates for shipment 1001 all fall between  2/24/2021 8/2/2022
  rename(.,"SAMPLE_NUMBER"=SAMPLE_.)%>%
  arrange(.,STDY,PLOT)%>%
  write.csv(file = "C:/Users/sabloszi/Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Shipping/Funga/1001_funga_20220831_digital.csv")
if("you want a paper version that goes in the box"==T){ #.... then move this up.
  select(.,c(STDY,PLOT,DATE_COLLECTED,SAMPLE_NUMBER))%>%
    write.csv(file = "C:/Users/sabloszi/Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Shipping/Funga/1001_funga_20220831_paper.csv")
}
#or to get the study sample ranges etc move this up:
group_by(.,STDY)%>%
  summarise(min=min(SAMPLE_N),max=max(SAMPLE_N),numbsss=length(SAMPLE_N))



#FOR713----
f713<-read.csv("C:/Users/sabloszi/Desktop/RW18_2013soils_example.csv",header = T)



#note: this comes from https://rstudio-pubs-static.s3.amazonaws.com/68452_ed50146f1ca24b568d4cfbd6db3b58d6.html
zn_soil  <- seq(min(f713$zn_soil), max(f713$zn_soil), length.out = 35);
mn_soil  <- seq(min(f713$mn_soil), max(f713$mn_soil), length.out = 35);

p_soil_ppm  <- seq(min(f713$p_soil_ppm), max(f713$p_soil_ppm), length.out = 35);

#name the model so it can be used easily in the next step
zmodel<-    lm(data=f713,mn_soil~zn_soil*p_soil_ppm) 
## Interpolate surface
mnlnr  <- outer(zn_soil,p_soil_ppm,function(zn_soil,p_soil_ppm){predict(zmodel, data.frame(zn_soil=zn_soil, p_soil_ppm=p_soil_ppm))});
#look at the resulting surface- does it make sense?
persp3d(zn_soil,p_soil_ppm,mnlnr, theta = 30, phi = 20,
        col = "lightblue", shade = 0.8, ticktype = "detailed")

#zn_soil-clay
#apparently this is base graphics stuff, not rsm::contour or raster::contour
windows(100, 100, pointsize = 12) #opens a separate window with the size you want
par(mar=c(4,6,4,3))
graphics::contour(zmodel, p_soil_ppm ~ zn_soil, image = TRUE,labcex=2.8,cex.lab=2.8,cex.axis=2.8, xlabs=c("Soil P (ppm)","Soil Zn (mg kg^-1)"))   
#all possible combos of Funga docket soils-----

#Here is a list of samples that still need sending to Funga as of 10/25/2022 ish:
#191919191919
outer(c(30,130),c(1000,2000,3000,4000),FUN="+")%>%melt()%>%
  mutate(.,X1="194202")%>%
  rbind(.,
        #2 reps, 300 tpa, 500 tpa, fert and no fert -x000, x030, x100, x130 where x=  1 and 3
        (outer(c(0,30,100,130),c(1000,3000),FUN="+")%>%melt()%>% #need to see if the blocks are right here
           mutate(.,X1="195501")))%>%
  rbind(.,
        (outer(c(520,521),c(1000,2000,3000,4000),FUN="+")%>%melt()%>%
           mutate(.,X1="201301")))%>%
  rbind(.,
        (outer(c(520,521),c(1000,2000,3000),FUN="+")%>%melt()%>%
           mutate(.,X1="201302")))%>%
  rbind(.,
        (outer(c(0,151,200),c(1000,2000),FUN="+")%>%melt()%>%
           mutate(.,X1="284202")))%>%
  rbind(.,
        (outer(c(7, 9, 12, 15, 17, 18),c(0),FUN="+")%>%melt()%>%
           mutate(.,X1="991302")))%>%
  
  #9999999999999999  
  rbind(.,
        (outer(c(400,403),c(1000,2000,3000),FUN="+")%>%melt()%>%
           mutate(.,X1="994001")))%>%
  rbind(.,
        (outer(c(400,403),c(1000,2000,3000),FUN="+")%>%melt()%>%
           mutate(.,X1="994002")))%>%
  rbind(.,
        (outer(c(400,405),c(1000,2000,3000),FUN="+")%>%melt()%>%
           mutate(.,X1="994003")))%>%
  rbind(.,
        (outer(c(400,405),c(1000,2000,3000),FUN="+")%>%melt()%>%
           mutate(.,X1="994004")))%>%
  #9898989898989898  
  rbind(.,
        (outer(c(2, 10, 11, 38, 39,13, 14, 15, 23, 25,  6, 18, 20, 32, 35, 48),c(0),FUN="+")%>%melt()%>%
           mutate(.,X1="985503")))%>%
  #181818181818
  
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="180601"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="181201"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="181502"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="181503"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="183102"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="184401"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="184501"))%>%
  rbind(.,
        outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
          mutate(.,X1="184801"))%>%
  #18s but they were sampled already:
  #    rbind(.,
  #outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
  #  mutate(.,X1="185301"))%>%
  #  rbind(.,
  #outer(c(0,218,424),c(1000,2000),FUN="+")%>%melt()%>%
  #  mutate(.,X1="185302"))%>%
  
  #mutate(.,value=as.integer(substr(value,2,4)))%>%
  # select(c(X1,value))%>%unique()%>%mutate(.,X1=as.integer(X1))->fuckcre
  select(-X2)%>%
  rename(STDY="X1",PLOT="value")%>%
  merge(.,
        (read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header = T)%>%    mutate(.,DATE_COLLECTED=as.Date.character(DATE_COLLECTED,format="%m/%d/%Y"))),
        #      read.csv("C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header = T),
        
        by=c("STDY","PLOT"),all=T)%>%
  subset(.,!STDY%in%c(193101,
                      185201,
                      193501,
                      193901,
                      194001,
                      281303,
                      282401,
                      284201,
                      185301,
                      185302)
  )%>%
  arrange(STDY,PLOT)%>%
  merge(.,
        read.table(text = 
                     "
STDY  notes
180601	bag
181201	bag
181502	bag
181503	box
183102	box
184401	box
184501	bag
184801	box
",header=T),by="STDY",all=T)%>%
  mutate(.,notes=coalesce(notes.x,notes.y))%>%
  select(-c(notes.x,notes.y))%>%
  #then get the dates for the last few
  merge(.,
        
        read.table(text="
sampler	DATE_COLLECTED	STDY
JacobH	2022-10-14	194202
Sean	2022-10-21	195501
Kyle	2022-10-20	201301
Sean	2022-10-07	201302
JacobH	2022-10-11	284202
Sean	2022-10-21	991302
Sean	2022-10-21	994001
Brody	2022-10-21	994002
",header=T)%>%
          mutate(.,DATE_COLLECTED=as.Date.character(DATE_COLLECTED)),
        by=c("STDY"),all=T)%>%
  mutate(.,DATE_COLLECTED=coalesce(DATE_COLLECTED.x,DATE_COLLECTED.y))%>%
  select(-c(DATE_COLLECTED.x,DATE_COLLECTED.y))->nut18dock #%>%

#now merge with the dates for the arcive 18s your actually gonna send:
merge(nut18dock,(read.csv(
  "G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Shipping/Funga/1008_funga_202210XX_digital_rw18XXXX-archived-only_delete.csv",header=T)%>%
    mutate(.,DATE_COLLECTED=as.Date.character(DATE_COLLECTED,format="%m/%d/%Y"))%>%
    select(-c(notes))),
  by=c("STDY","PLOT"),all=T)%>%
  mutate(.,DATE_COLLECTED=coalesce(DATE_COLLECTED.x,DATE_COLLECTED.y))%>%
  select(-c(DATE_COLLECTED.x,DATE_COLLECTED.y))%>%
  wex()
select(c(DATE_COLLECTED))%>%
  unique()
head()

dim()
head()%>%
  dim()
dim()



fuckcre%>%arrange(X1,value)%>%head
fuckdb%>%
  subset(.,PLOT%in%c(0,218,424))%>%
  arrange(STDY,PLOT)%>%
  cast(.,STDY~.)%>%pull(STDY)->db

fuckcre%>%
  subset(.,value%in%c(0,218,424))%>%
  arrange(X1,value)%>%
  cast(.,X1~.)%>%pull(X1)->cre

db[!db%in%cre]
cre[!cre%in%db]

nut%>%
  subset(.,substr(STDY,1,2)=="18"&year(DATE_COLLECT)==2013|grepl("ty_soil",Comments))%>%
  pull(STDY)%>%unique()


nut%>%
  subset(.,substr(STDY,1,2)=="18"&year(DATE_COLLECT)==2013|grepl("ty_soil",Comments))%>%
  pull(YST)%>%unique()

cast


wex()

(outer(c(400,405),c(1000, 2000,3000),FUN="+"))%>%
  melt()%>%
  pull(value)%>%
  outer(.,(10000*c(994001)),FUN="+")%>%
  melt()%>%
  mutate(.,X1=substr(value,1,6))%>%
  mutate(.,X2=substr(value,7,10))%>%
  select(-value)%>%
  wex()
head()
cspli
print()
mutate(.,X1="284202"))


cre[cre%in%db]

#needs to go to : 
#C:\Users\sabloszi\Dropbox (FPC)\FPC Team Folder\RegionWide Trials\Special Studies\Funga fungal microbiome\Funga FPC Site Summary 6-22-2022_Plotnumbers.csv

#delete trash: samples on bench as of10/14/222 ----
read.table(text="
           994005
994005
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
994004
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
185201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184201
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
184202
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
180101
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
183901
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
182201
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
185302
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
182401
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
181502
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
184501
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
181201
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
183102
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
181503
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801
184801")->bench
read.table(text="
           994001
994002
994003
994004
180601
181201
181502
181503
183102
184401
184501
184801
185301
185302
")->need
need<-need%>%
  pull(V1)%>%
  unique()%>%
  sort()

need[need%in%bench]

need%>%str()

bench<-bench%>%
  pull(V1)%>%
  unique()%>%
  sort()
#read in shit-----
read.table(text="
           1110
2110
4110
3110
2010
4010
3010
1010
1120
2120
4020 
4120
3020
2020
1020
3120
1130
4130
2030
2130
3130
1030
3030
1100
1190
3090
2100
2000
4190
4100
4090 
4000
3190
2090
3100
2190
3000
1090 
1000
")%>%
  duplicated()
str()


#combine pdfs-----
qpdf::pdf_combine(input = 
                    c("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/281303_DataSheets_20230105.pdf",
                      "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/281303_DataSheets_202301052.pdf"  ),
                  output = "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/281303_DataSheets_202301053.pdf")


qpdf::pdf_combine(input = c("B:/20221215_ROSS_CLARK_CIR.pdf",
                            "B:/20221215_ROSS_CLARK_CIR2.pdf"
),
output = "G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/Official/Pcard/Receipts/20221215_uspsdothan_doesoilsshipping.pdf")





#have need funga----
read.table(text=
             "have	need
185301	185201
185301	193101
185301	193501
185301	193901
185301	194001
185301	194202
185301	195501
185301	201301
185301	201302
185301	281303
185301	282201
185301	282401
185302	284201
185302	284202
185302	991302
185302	994001
185302	994002
185302	994003
185302	994004
185302	985503
185302	180601
185302	181201
185302	181502
185302	181503
varrate	183102
varrate	184401
varrate	184501
varrate	184801
varrate	185301
varrate	185302
varrate	varrate
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
varrate	1
185201	1
185201	1
185201	1
185201	1
185201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
282401	1
282401	1
284201	1
284201	1
284201	1
284201	1
284201	1
284201	1
185201	1
185201	1
185201	1
185201	1
193101	1
193101	1
193101	1
193101	1
193101	1
193101	1
193101	1
193101	1
193501	1
193501	1
193501	1
193501	1
193501	1
193901	1
193501	1
193901	1
193501	1
193901	1
193501	1
193901	1
193901	1
193901	1
193901	1
193901	1
194001	1
194001	1
194001	1
194001	1
194001	1
194001	1
194001	1
194001	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
281303	1
994002	1
994002	1
994002	1
994002	1
994002	1
994002	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
985503	1
991302	1
991302	1
991302	1
991302	1
991302	1
991302	1
194202	1
194202	1
194202	1
194202	1
194202	1
194202	1
194202	1
194202	1
195501	1
195501	1
195501	1
195501	1
195501	1
195501	1
195501	1
195501	1
201301	1
201301	1
201301	1
201301	1
201301	1
201301	1
201301	1
201301	1
284202	1
284202	1
284202	1
284202	1
284202	1
284202	1
201302	1
201302	1
201302	1
201302	1
201302	1
201302	1
180601	1
180601	1
180601	1
180601	1
180601	1
180601	1
181201	1
181201	1
181201	1
181201	1
181201	1
181201	1
181502	1
181502	1
181502	1
181502	1
181502	1
181502	1
181503	1
181503	1
181503	1
181503	1
181503	1
181503	1
183102	1
183102	1
183102	1
183102	1
183102	1
183102	1
184401	1
184401	1
184401	1
184401	1
184401	1
184401	1
184501	1
184501	1
184501	1
184501	1
184501	1
184501	1
184801	1
184801	1
184801	1
184801	1
184801	1
184801	1
185301	1
185301	1
185301	1
185301	1
185301	1
185301	1
185302	1
185302	1
185302	1
185302	1
185302	1
185302	1
",header=T)->havned
need<-havned$need
have<-havned$have
need[!need%in%have]%>%unique()


#shauna's organization of boxes-----
shauna<-read.csv("C:/Users/sabloszi/Desktop/delete2.csv",header = T)%>%
  subset(.,grepl("18",study))%>%
  subset(.,grepl("subedi",Box))%>%
  pull(study)%>%unique()

#which of the boxes in the lab do we still need to package for funga? (not 06,12, or 45 bc those are pretty much done)
fuckcre[fuckcre$X1%in%shauna,]%>%subset(.,!X1%in%c(180601,181201,184501))
#note 1502,4401,5301,and 5302 are not called for even though we have them
#so 181503 183102 184801 are all game b/c theyere in the lab and on the funga list
#so now which plots from those do we need lavbels for/to ship to funga in reality?
lbtrs<-fuckcre[fuckcre$X1%in%shauna,]%>%subset(.,!X1%in%c(180601,181201,184501))

#So which of the samples in the labfrom an 18  do i want to send to colin still?

outer(lbtrs$value[1:3],c(1000,2000),FUN = "+")%>%
  melt(.)%>%pull(value)%>%
  outer(unique(lbtrs$X1)*10000,., FUN="+")%>%
  melt()%>%
  mutate(.,PLOT=as.integer(substr(value,1,6)),STDY=as.integer(substr(value,7,10)))%>%
  select(c(PLOT,STDY))%>%  
  mutate(full=paste0(PLOT,",",STDY))%>%
  select(full)%>%
  merge(.,fuckdb,by=c("STDY","PLOT")) #this should be the 2021 db
#ok but there's nothing here, so i need to see if these are in the db for sample numbers. 
#what are the most recent samplings in 181503 183102 184801?
#rw18s

#what ysts etc do these in subedi boxes have in the db?
nut%>%
  subset(.,substr(STDY,1,2)=="18"&YST!=0&YST!=8&STDY%in%c(180101,180601, 181503 ,183102 ,184401,184801,185201))%>%
  select(c(DATE_COLLECT,STDY,YST,`SAMPLE_#`,DEPTH))%>%
  wex()
select(c(STDY,YST,DATE_COLLECT))%>%
  select()
#just y st 0

#shaunas boxes: which can alysse pull o0ut?----
outer(lbtrs$value[1:3],c(1000,2000),FUN = "+")%>%
  melt(.)%>%pull(value)%>%
  outer(unique(lbtrs$X1)*10000,., FUN="+")%>%
  melt()%>%
  mutate(.,STDY=as.integer(substr(value,1,6)),PLOT=as.integer(substr(value,7,10)))%>%
  select(c(PLOT,STDY))%>%  
  mutate(Plot..e.g..1234.=paste0(substr(STDY,3,6),"-",PLOT,"-1"))%>%
  #print()
  merge(.,read.csv("C:/Users/sabloszi/Desktop/delete2.csv",header = T),by=
          "Plot..e.g..1234.",all.x=T)%>%
  wex()
wex()


read.table(text="
  1000	181503  k
  1218	181503  k
  1424	181503  k 
  2000	181503  k
  2218	181503  k
  2424	181503  k
  1000	183102  k
  1218	183102  k
  1424	183102  k
  2000	183102  k
  2218	183102  k
  2424	183102  k
  1000	184801  k
  1218	184801  k
  1424	184801  k
  2000	184801  k
  2218	184801  k
  2424	184801  k
  ")%>%
  rename(PLOT="V1",STDY="V2")->subs



nut%>%  
  subset(.,substr(STDY,1,2)=="18"&STDY%in%(c(181503,183102,184801))&YST!=0)%>%
  merge(.,subs,by=c("STDY","PLOT"),all.x=T,all=F,all.y=F)%>%
  dim()
wex()
pull(DATE_COLLECT)%>%unique()
dim()


subset(.,year(DATE_COLLECT)==2013|(STDY==201302&year(DATE_COLLECT)!=2218)&DEPTH==8&PLOT%in%c(1520,1521,2520,2521,3520,3521))%>%
  subset(.,(STDY>(181200)&DEPTH<7)|STDY%in%c(180601,201302))%>%
  
  
  #rw18 merging----
#20221017.mdb, seanb
#connnew<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")#20221017
#20210423.mdb, seanb
#connold<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb")
#20221017.mdb, sabloszi
#connnew<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")#20210423
#20210423.mdb, sabloszi
#connold<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb")#
#11156 in 2021, 11336 in 2022
#11156 -11336 =180
#see tables thats in it
sqlTables(conn, tableType = "TABLE")$TABLE_NAME
#create indiv tables as dfs
nutnew<-sqlFetch(connnew, "dbo_NUT_SOIL")
nutold<-sqlFetch(connold, "dbo_NUT_SOIL")

tree<-sqlFetch(conn,"dbo_TREE_GROWTH")
stdy<-sqlFetch(conn,"dbo_STUDY_INFO")
sqlFetch(conn,"dbo_ACTIVITY_WORKPLAN")%>%wex()
sqlFetch(conn,"dbo_DEPTH_CODES")%>%subset(RW==18)
sqlFetch(conn,"dbo_COMPANY_OPERATIONS")%>%
  subset(OPERATION_ID==40)

#get stdy, yst, sample number and plot
nutnews<-  
  nutnew%>%
  subset(.,substr(STDY,1,2)=="18"&YST>=9)%>%
  subset(.,DEPTH==5)%>%
  #select(c(STDY,DEPTH))%>%  #so all 18s >=yst 9 all techinically are at depth of "5" except
  #select(c(STDY,YST))%>% #just shows the most recent yst <10 is 8 and its 2006
  #melt(.,id.vars="STDY")%>%
  #cast(.,STDY~value)%>%
  subset(!grepl("53",STDY))%>%
  select(c(STDY,YST,PLOT,`SAMPLE_#`))#%>%
print()
head()
wex()

nutolds<-  
  nutold%>%
  subset(.,substr(STDY,1,2)=="18"&YST>=9)%>%
  subset(.,DEPTH==5)%>%
  #      select(c(STDY,DATE_COLLECT))%>%
  #select(c(STDY,DEPTH))%>%  #so all 18s >=yst 9 all techinically are at depth of "5" except
  #select(c(STDY,YST))%>% #just shows the most recent yst <10 is 8 and its 2006
  #melt(.,id.vars="STDY")%>%
  #cast(.,STDY~value)%>%
  subset(!grepl("53",STDY))%>%
  select(c(STDY,YST,PLOT,DATE_COLLECT))
print()
head()

#now merge the 18s in the lab with the dock(et) info
nut18ids<-merge(nutnews,nutolds,by=c("STDY","YST","PLOT"),all=T)%>%
  subset(STDY%in%c(181502,181503,183102,184801))#%>%
wex()
head()
head(nut18dock)
merge(nut18ids,nut18dock,by=c("STDY","PLOT"),all.y=F)%>%
  wex()

#is tims 10/27 plot numbers funga same as my 10/26 plot numbers funga?----------
tim<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/sample numbers for funga study Funga FPC Site Summary 6-22-2022_Plotnumbers-3.2.csv",header = T)
sea<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers-3.csv",header = T)
names(tim)
tim%>%select(-(c(SAMPLE_.,notes.1,DATE_COLLECTED)))%>%subset(STDY!=184501)%>%
  all.equal(.,(sea%>%select(-c(SAMPLE_.,DATE_COLLECTED))%>%subset(STDY!=184501)))
#merge varrate bag names with plot info----
pltinf<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Maps for plot installation/plot_ids_varrate.csv",header=T)
baginf<-read.csv("C:/Users/sabloszi/Desktop/delete123123.csv",header=T)

str(pltinf$Name)
str(baginf$Name)
#whats in pltinf and not in bags?
pltinf$Name[!pltinf$Name%in%baginf$Name]
#fuck yea its just that Rate-516_4-smit-113 that there were two of to begin with
#whats on the bags and not in plot list?
baginf$Name[!baginf$Name%in%pltinf$Name]

baginf%>%
  select(-nratetxt)%>%
  merge(.,pltinf,by="Name")%>%
  select(c(tdesc,stand,Herbicide,N_lbac))%>%
  melt(.,id.vars=c("stand","Herbicide","N_lbac"))%>%
  cast(.,Herbicide~N_lbac~stand)%>%
  capture.output(file=NULL)%>%writeClipboard()


read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Maps for plot installation/plot_ids_varrate.csv",header=T)%>%
  mutate(.,fert_rx=ifelse(fert_rx=="control","random",fert_rx))%>%
  #subset(.,fert_rx!="neighbor")%>%
  #names()
  #pull(ResrcID)%>%unique()%>%length  
  select(c(SUB_ID,fert_rx,Herb,N_lbac))%>%
  melt(.,id.vars=c("fert_rx","Herb","N_lbac"))%>%
  cast(.,Herb~N_lbac~fert_rx)%>%
  print()
dim()
head()  
capture.output(file=NULL)%>%writeClipboard()
dim()


write.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Maps for plot installation/plot_ids_varrate.csv")

wex()  
names()
dim()
subset(.,Name.x==Name.y)
head()
names()


#merge sample numbers for funga 180601 18xxxxx .... with sample nubmers for funga study .... 3.2-----
f1806<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/sample numbers for FUnga samples 180601 181201 184501 201301 201302-1.csv",header=T)%>%
  subset(.,DEPTH=="0-15")
f3.2<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/sample numbers for funga study Funga FPC Site Summary 6-22-2022_Plotnumbers-3.2.csv",header=T)

fnew<-merge(f1806,f3.2,by=c("STDY","PLOT","SAMPLE_."),all.x=F,all.y=T)%>%
  mutate(.,SAMPLING_LOCATION=coalesce(SAMPLING_LOCATION.x,SAMPLING_LOCATION.y))%>%
  mutate(.,DEPTH=coalesce(DEPTH.x,DEPTH.y))%>%
  mutate(.,YST=coalesce(YST.x,YST.y))%>%
  mutate(.,DATE_COLLECT=coalesce(DATE_COLLECT.x,DATE_COLLECT.y))%>%
  select(-c(SAMPLING_LOCATION.x,SAMPLING_LOCATION.y,DEPTH.x,DEPTH.y,YST.x,YST.y,DATE_COLLECT.x,DATE_COLLECT.y))

fplotnu<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header=T)%>%
  subset(.,STDY!="varrate")



fplotnu<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Shipping/Funga/1001_funga_20220831_digital.csv",header=T)%>%
  subset(.,STDY!="varrate")

#not the plot number studies that are in new
fplotnu$STDY[fplotnu$STDY%in%fnew$STDY]%>%unique()
fnew$STDY[!fnew$STDY%in%fplotnu$STDY]%>%unique()

#not the plot number names that are in new
names(fplotnu)[!names(fplotnu)%in%names(fnew)]%>%unique()

fnew%>%names()
fnew$STDY[!fnew$STDY%in%fplotnu$STDY]%>%unique()

rbind.fill(fnew,fplotnu)%>%
  merge(.,rlc,by="STDY")%>%
  write.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers2.csv")
dim()

wex()

unique(f)
head()
select(-c(X))

#now merge with the excel.xsxs
rlc<-
  read.csv("C:/Users/sabloszi/Desktop/delete_Funga FPC Site Summary 9-28-2022_rlc.csv",header = T)%>%
  subset(STATE%in%state.abb)



sea[!sea%in%finames]
finames[!finames%in%sea]
finames


mr<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header=T)
ff<-read.csv("C:/Users/sabloszi/Desktop/delete.csv",header=T)

ff%>%head()

merge(mr,ff,by=c("STDY","PLOT"),all.y=F,all.x=T)%>%wex()

#merge funga sample numbers-----
sids<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/Funga fungal microbiome/sample numbers for Funga forest floor samples delete_need_forest-floor_sample-numbers.csv",header=T)
big<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga FPC Site Summary 6-22-2022_Plotnumbers.csv",header=T)
str(sids)
str(big)
tail(big)
big$t

big%>%
  subset(STDY=="varrate")%>%
  rename(PLOT2="tdesc")%>%
  rename(tdesc="PLOT")%>%
  rename(PLOT="PLOT2")->bigv

merge(sids,bigv,by=c("STDY","PLOT"),all.x=T,all.y=F)%>%
  select(c(STDY,PLOT,FFSamp.,tdesc,sampleid))%>%
  subset(STDY=="varrate")%>%
  wex()
dim()

big[big$STDY=="varrate",]
#merge for the last four 18s in white bags brody wanted labesl fro while i was in tx:---------

nutnew2<-nutnew%>%
  dplyr::select(c(STDY,PLOT,DATE_COLLECT,DEPTH,`SAMPLE_#`,YST))
nut2<-nut%>%
  dplyr::select(c(STDY,PLOT,DATE_COLLECT,DEPTH,`SAMPLE_#`,YST))

require(dplyr)

merge(nut2,nutnew2,by=c("STDY","PLOT","YST","DEPTH"))%>%
  subset(.,STDY%in%c("185201","182401","185302","184202")
  )%>%
  subset(.,YST>=10)%>%
  dplyr::mutate(.,DATE_COLLECT=coalesce(DATE_COLLECT.x,DATE_COLLECT.y))%>%
  dplyr::mutate(.,`SAMPLE_#`=coalesce(`SAMPLE_#.x`,`SAMPLE_#.y`))%>%
  dplyr::select(-c(`SAMPLE_#.x`,`SAMPLE_#.y`,DATE_COLLECT.x,DATE_COLLECT.y))%>%
  merge(.,
        (      sqlFetch(conn,"dbo_DEPTH_CODES")%>%subset(RW==18)
        ),by="DEPTH"
  )%>%
  arrange(`SAMPLE_#`)%>%
  select(-c(STATUS,RW))%>%
  wex()
head()
head()
dim()%>%
  subset(.,)
requi


nut%>%subset(.,STDY==185302&YST>10)%>%
  merge(.,
        (      sqlFetch(conn,"dbo_DEPTH_CODES")%>%subset(RW==99&STATUS=="A")
        ),by="DEPTH"
  )%>%
  arrange(`SAMPLE_#`)%>%
  select(c(DEPTH_DESC,DEPTH,STDY,PLOT,`SAMPLE_#`))%>%
  wex()
head()
dim()



sqlFetch(conn,"dbo_DEPTH_CODES")%>%print()
print("x")
#read in archive cup sample numbers scanned from a paper sheet-----
read.csv("C:/Users/sabloszi/Desktop/delete.csv")%>%
  mutate(.,char=apply(.,1,FUN = function(x){nchar(x)}))%>%
  subset(.,char==5)%>%
  wex()
print()
head()
nchar("SDASD")

appl
nchar()
dim()
head()
22966 23132

#get range of samples in each box-----

read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/NCSU Sample Archive/bigsampletable.csv",header = T)%>%
  group_by(archive_box_code)%>%
  summarise(min=min(`SAMPLE_.`),max=max(`SAMPLE_.`),n=length(`SAMPLE_.`))%>%
  mutate(.,rng=paste0(min,"-",max))%>%
  print()
wex()
str()
head()
dim()

#find last sample numbers assigned-----
nut%>%arrange(`SAMPLE_#`)%>%
  subset(.,!is.na(`SAMPLE_#`))%>%
  tail()%>%
  wex()

nut%>%pull(`SAMPLE_#`)%>%max(na.rm=T)
#merging lob, cot, etc dfs-----
lobsol<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/DOE/LOB_PotentialSites_wsoilData/LOB_PotentialSites_wsoilData.csv",header=T)
cotcon<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/DOE/Specific_sites_notes2.csv",header=T)
lobcon<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/DOE/LOB Potential Sites2.csv",header=T)
trip<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/DOE/LOB_PotentialSites_wsoilData/LOB_PotentialSites_wsoilData_tripsubset2.csv",header=T)

merge(trip,lobsol,by="siteid",all=T)%>%
  merge(.,cotcon,by="siteid",all=T)%>%
  merge(.,lobcon,by="siteid",all=T)%>%
  mutate(.,Site=coalesce(Site.x,Site.y))%>%
  mutate(.,Soil=coalesce(Soil.x,Soil.y))%>%
  mutate(.,Site=coalesce(Rank.x,Rank.y))%>%
  mutate(.,Notes=coalesce(Notes.x,Notes.y))%>%
  mutate(.,Contact=coalesce(Contact.x,Contact.y))%>%
  select(-c(Site.x,Site.y,Rank.x,Rank.y,Soil.x,Soil.y,Contact.x,Contact.y,Notes.y,Notes.x))%>%
  wex()
dim()
wex()
select(=)
names()




#all possible combos of rw28 stuff-----
outer(c(1000,2000,3000,4000),c(0,  361,  360,  481,  480,  721,  720,  108),FUN = "+")%>%
  melt()%>%
  pull(value)%>%outer(.,c("N","S","E","W"),FUN = "paste0")%>%
  wex()



#google sheets read inn=-----
#STUDY	PLOT	Tree	Ht,  ft.in	HTLC,ft.in	dbh,mm	RCD,mm	Mortality	Damage code	 dam	row end?	Notes	date																	
#fuck<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1foGV8kUzgDiTsx0coouDvG0TWm7eks7DYMZt0Gx-e_4/edit#gid=1283512166')
link_to_Gsheets <- "https://docs.google.com/spreadsheets/d/1foGV8kUzgDiTsx0coouDvG0TWm7eks7DYMZt0Gx-e_4/edit#gid=1283512166"

datadbh282201<-read_sheet(link_to_Gsheets, sheet="282201")
datadbh281303<-read_sheet(link_to_Gsheets, sheet="281303")

#see trees per plot
#datadbh281303%>%
datadbh282201%>%
  as.data.table()%>%
  dplyr::select(c(PLOT,dbh))%>% #select is comgin from somewhere else if not explicit
  melt(.,id.vars="PLOT")%>%
  cast(PLOT~variable)%>%
  print

is.num
datadbh <-  link_to_Gsheets %>%
  sheet_names() %>% #get names
  set_names()  #give names
#%>%  map_df(read_sheet, ss = link_to_Gsheets, .id = "Cut") #if you want to combine sheets (usually you wouldnt unless they are essentially the same ; this is like rbindrows more or less)

#ocr------
#
#library(pdftools) #just need this if converting from pdf to png or other image formats;
#note it has problems with Jpeg version library being too high for some JPegs so PNG seems more foolproof
ocr("G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Lab Documents/Safety/Training/Mentoring/Protocols_NCSU_mentor_compact.png")%>%writeClipboard() 

#find newest files----
df<-file.info(list.files("C:/Users/sabloszi/Desktop/deleteasd", full.names = T))

files=function(path){file.info(list.files(path, full.names = T))}
newest=function(x){rownames(x)[which.max(x$mtime)]}

newest(files(
  newest(files(
    newest(files(newest(files(path = "C:/Users/sabloszi/Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC"))))
  ))
))




#2023 cruising data varrateNC calibration---- 
#(link from pickupcalks vertex section somwhoe)

#first lets see how raw data looks to anser:
#1. How does actual (tape measure) height of object affect the vertex measured height?
#2. How does distance from the object affect the vertex measured height?
#3. How does parallax affect the vertex measured height? (like for bent trees etc)
#4. how does incorrect (too close, too far) calibration affect the vertex measured height?
#5. To some degree, how does variability in all kinds of things i didnt account for (like A] difficulty of pinpointing the thing i'm sighting too b/c of tree foliage in the way; B] minor elevation differences; and C] no idea) affect the vertex measured height?

#Notes: 29.52, 32.8, and 34.8 ft represent the farthest forward from, closest to, and farthest behind the actual 32.8 ft on the tape measure on the ground (ie the vertex won't let you calibrate if you are closer than an actual 29.52 ft from the transponder and you have 32.8 set as the true distance; which is actually a nice feature)
#Also some other calibration data is in G:\My Drive\PICK UP-CALCULATIONS.xlsx

read.csv("C:/Users/sabloszi/Dropbox (FPC)/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Lab Documents/SOPs_Methods/vertex_iv_me_calib-data.csv",header=T)%>%
  subset(dt=20230328)%>% #for this modeling example stuff just use the data i collected this date in the biltmore jordan courtyard. more info in the pickup calculations.xlsx above
  mutate(.,ftcalib=as.factor(ftcalib))%>%
  mutate(.,floor=as.factor(floor))%>%
  #head()%>%
  ggplot(data=.)+
  geom_hline(    yintercept=c(65.6,50.9,36))+
  geom_line(aes(x=fthoriz,y=ftvert,color=floor,linetype=ftcalib))
#so what would make the trees taller last year?
#1. If the trees were actually taller and we calibrated correctly (the colors of lines represents this, more or less- it is an actual difference in heights to floors)
#2. If we were calibrating acurately and standing too far away (but this seems unlikely, I would guess if anything we were probably closer than the heigh away from the trunk)
#3. Most likely, I had the tape measure too slack when I measured out 32.8 feet. This easily gives you between 1-5 feet of extra depending on how far away you are and how tall the tree was.
#4. If the transponder height was wrong last year in the vertex itself. In reality, it's probably 1-2 inches below dbh since it's in the way of the dtape if it's at exactly 4.5. FWIW, the old vertex (the one we used last year and maybe the first measurement this year) was at 3.5 when I checked so maybe the first day's measurements were 1 foot off because of this (maybe for which day it was since I didn't keep track of which vertex was used when)

moddat<-read.csv("Q:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Lab Documents/SOPs_Methods/vertex_iv_me_calib-data.csv",header=T)%>%
  subset(dt=20230328)%>%
  mutate(.,tree=rep(1:10,9))%>%
  subset(.,ftcalib!=34.8)%>%
  mutate(.,ftcalib=factor(ftcalib,levels=c("29.52","32.8"),labels=c("c","f")))%>%
  mutate(.,tree=(floor*10+tree))%>%
  select(-c(floor,fthoriz,item,dt))%>%
  melt(.,id.vars=c("ftcalib","tree"))%>%
  select(-variable)%>%
  cast(.,tree~ftcalib)

fit=  lm(data=moddat,f~c)#model far as a function of close since we want to find out the estimated farther (ie acurate 32.8ft calibration height i/o less than 32.8ft calib height) based on last years close data

ggplot(data=moddat)+
  geom_point(aes(x=c,y=f))+ #looks linear by the points
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2])#so yea looks nice and linear to the eye, worth modeling as a line
#ylim(0,10)+xlim(0,10) yes that positive intercept really does mean at 0,0 the close trees are shorter. But the slope <1 makes up for that

summary(fit)#99%r2 without even knowing anyuthing about how far from the tree we were or the actual calib distance; I think as long as we can estimate where they started growing from last year correctly we'll be good


#merge arc attr tables with 2022 cruising data varrateNC-----
#think dropbox as wd is outdated but just leaving this here as a reminder theres still duplicate data there.
#setwd("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data")
setwd("Q:/Shared drives/FER - FPC Team/RegionWide Trials/RW29 Variable Rate x Herb/NC/Tabular_Data")


v2023plots<-read_excel(paste0(getwd(),"/cruising-data-VarRate-20230405-stand127only-plots.xls"),
                       sheet="0",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                       #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                       trim_ws = T )
v2023trees<-read_excel(paste0(getwd(),"/cruising-data-VarRate-20230405-stand127only.xls"),
                       sheet="1",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                       #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                       trim_ws = T )

v2022trees<-read.csv(file="cruising-data-VarRate-20220412-25.csv",header=T)%>%
  mutate(tre=as.integer(tre))
#
v2023treesp<-read.csv(file="cruising-data-VarRate-20230405.csv",header=T)%>%
  mutate(tre=as.integer(tre))


#subset so just NC trees
v2023plots<-subset(v2023plots,Other_stand==127&usmaxht!=99)
#just get columns needed for ht2022 and ht 2023 comparison
v2023plots<-select(v2023plots,c(globalid,Other_stand,plot))
v2023trees<-select(v2023trees,c(parentglobalid,treeno,dbh,lbttlht,htlc,tree_height_2022))
#now just get 2023 NC data
v2023trees<-merge(v2023plots,v2023trees,by.x="globalid",by.y="parentglobalid")
#plot example 5620 v2023trees
v2023trees<-v2023trees%>%
  mutate(.,rt=as.integer(substr(plot,2,4)))%>%
  mutate(.,plot=as.integer(substr(plot,1,1)))%>%
  rename(.,site="Other_stand")%>%
  rename(.,tree_height_2023="lbttlht")%>%
  rename(.,tre="treeno")%>%
  rename(.,dbhin_2023="dbh")%>%
  rename(.,htlcft_2023="htlc")%>%
  select(c(site,rt,plot,tre,tree_height_2023,tree_height_2022,dbhin_2023,htlcft_2023))%>%
  mutate(.,tre=as.integer(tre))%>%
  merge(.,v2022trees,by=c("site","rt","plot","tre"))%>%
  mutate(.,Htft=as.numeric(Htft))%>%
  rename(.,Height_tip_2022="Htft")%>% #rename so it's easier to interpret
  rename(.,Height_node_2023="tree_height_2022")

#need this for figure below; relating node height in 2023 to real/precise but inaccurate tree heigh in 2022
#note "fit" for transponder data is elsewhere this doc
fit2=  v2023trees%>%
  subset(.,Height_node_2023>30)%>%#one must've been a typo b/c it says 27.6 ft tall and there weren't actually any trees under 30 in these three plots as Sean remembers it and htlc is above 30 for both years.
  lm(data=.,Height_node_2023~Height_tip_2022)#model far as a function of close since we want to find out the estimated farther (ie acurate 32.8ft calibration height i/o less than 32.8ft calib height) based on last years close data
summary(fit2)


#what does the relationship between node2023 and tip2022 measurements look like
v2023trees%>%
  subset(.,Height_node_2023>30)%>%#one must've been a typo b/c it says 27.6 ft tall and there weren't actually any trees under 30 in these three plots as Sean remembers it and htlc is above 30 for both years.
  #wex()
  ggplot(.,)+
  geom_point(aes(x=Height_tip_2022,y=Height_node_2023,color=PLOT))+
  geom_text(aes(x=20,y=60),data=data.frame(x=NULL,y=NULL),label="Green is 1:1; Blue is tree model; Dash is bldg model")+ 
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2],color="Blue")+
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],linetype="dashed")+#jordan hall model where the transponder height was known as 4.5ft ';  
  geom_abline(intercept = 0, slope = 1,color="green")+
  ylim(0,80)+xlim(0,80) #yes that positive intercept really does mean at 0,0 the 2022 tip trees are shorter. But the slope <1 makes up for that



#so in english:
#Measuring accurately in 2023 is the same as 
#... inaccurately in 2022 IF we make the 2023 height 82% of the 
#... 2022 height but then also add then add 2.5 feet to that
#... Or, correct= .82 * incorrect + 2.53

#... This is compared to the test of the same day, looking at
#... the balconies on the side of jordan with an off vs correct
#... calibration telling us the that accurately calibrated 
#... measurements are the same as:
#... innacurately calibrated IF we make the accurately calibrated
#... 92% of the inaccurately calibrated but then also add 0.36' (4-5")
#... to all the inaccruate heights

#comparing the two models, rmse for the ideal condition jordan hall
#.. where i knew where the railing was every time,
#... there is a rmse of ~8" vs about 18" for my in-the-pines model
#... thsi is the RSE in the summary(fit) and is the square root of 
#... the mean square error

#getting TreeLS to work----
#install.packages('devtools', dependencies = TRUE)
#library(devtools)
devtools::install_github('tiagodc/TreeLS')

#2222
##add a new path to where packages are stored for R

myPaths <- .libPaths()   # get the paths

myPaths <- c("C:/additional_r_packages/", myPaths)  # switch the directories - the first one will have things written to it!

.libPaths(myPaths)  # reassign them


devtools::install_github('tiagodc/TreeLS') #install TLS package - will be loaded in additional directory

install.packages("C:/additional_r_packages/TreeLS-master.tar.gz", repos = NULL, type="source")
#aybe need a linux shell and 
#https://stackoverflow.com/questions/48799228/compilation-failed-for-package-from-github
#and or 
#https://stackoverflow.com/questions/23135703/package-install-error-compilation-failed?rq=3
#or https://brew.sh/
#or https://www.geeksforgeeks.org/introduction-linux-shell-shell-scripting/

library('TreeLS', lib.loc="D:/additional_r_packages")
#lidR first use----
system.file()


#pick up later- 127's ..27 or ...31 las would be good to see about that one plot
library(lidR)
#get the file path of the las file
LASfile <- system.file("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las/220517_135817_Classified.las", package="lidR")
#read in the las file
las1 <- readLAS("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las/220517_135817_Classified.las", select = "xy",
                filter = "-keep_xy 2296207.395 902375.75 2296352.57375 902531.41325   -drop_z_below 255 
  -drop_z_above 269")

#https://www.earthdatascience.org/courses/use-data-open-source-python/data-stories/what-is-lidar-data/lidar-points-to-pixels-raster/
#locate trees witin the las file
loct<-locate_trees(las = las1,
                   algorithm = lmf(ws = 5),uniqueness = "incremental")
separated_coord<-data.frame(long = unlist(map(loct$geometry,1)),
                            lat = unlist(map(loct$geometry,2)),
                            z=1)

separated_coord<- st_drop_geometry(separated_coord)

write.csv(file="G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/wholestandtrees.csv",separated_coord)

#st_write(separated_coord, 
#        "G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/wholestandtrees.shp")



#chm------
#CHM
#most of this is based off ivans code from 6/20/23 sent to sean. this is sean's mods
#lots of sean's notes in here like "ivan had X" b/c his las crs was in m, but mines' in ft
### ready classfied cluod, normalized, CHM

require(raster)
require(rgeos)
options(stringsAsFactors = FALSE)
# arc.check_product()#?

#read in the las FOLDER you want ot work with (can't have other files in there i dont htinkg, at least i tried and it failed with non las files)
##replace paths with the path to the folder were data is stored
#stand 55 all
#ctg <- readLAScatalog("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las")
#stand 113 all
ctg <- readLAScatalog("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 2 - smit 113/las_lidar")


#temp smaller extent for NE plots in 55 (might need to edit what exists in which folders to make this work again):
#ctg<-clip_rectangle(ctg, xleft = 2296061, ybottom=902133.9,xright= 2298115,ytop= 902765.6)
#write it back to the folder
#writeLAS(ctg,"C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las2delete/n17.las")


print(ctg)
list(ctg)
plot(ctg)
ctg
las_check(ctg)

#library(arcgisbinding) # needed for this read_sf shp stuff I think but wouldnt install arcgisbinrding as of 6/2023
##shp to cut point cloud. do you need to? if not go to ground classification
shp=read_sf("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/whereplotsaredeletepoly.shp")
shp=read_sf("C:/Users/sabloszi/AppData/Local/Temp/ArcGISProTemp20528/Untitled/516_3_113.shp")
plot(shp)


crs(ctg) #get or set coord ref syst
crs(shp)

#what does this do? idk
plan(multisession) #"sequentially or parallel" for some kind of processing
opt_chunk_size(ctg) <- 750 #ivan had 250 for his data in meters
opt_chunk_buffer(ctg) <- 15 #ivan had 5
opt_output_files(ctg) <- paste0(tempdir(), "/_{ID}")

#not sure how this works, ivan gave it to me but IDK what shp_buffer is since I don't see it elsewhere SB 6/21/23
ctg <- clip_roi(ctg, shp)


plot(ctg)
print(ctg)


### best ground classification"progressive morphological filter" Zhang et al. (2003)
opt_chunk_size(ctg) <-750 #ivan had 250
opt_chunk_buffer(ctg) <- 15 #ivan had 5
ws <- seq(9, 36, 9) #window size #ivan had 3, 12, 3)
th <- seq(0.3, 4.5, length.out = length(ws)) #threshold size or sequence of threshold heights #ivan had 0.1, 1.5, 
opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")
ctg <- classify_ground(ctg, algorithm = pmf(ws = ws, th = th))


#don't usually need noise?
opt_chunk_size(ctg) <-750 #ivan had 250
opt_chunk_buffer(ctg) <- 30 #ivan had 10
opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")
ctg<- classify_noise(ctg, sor(8,1)) #ivan had 8,1

#normalize heights
opt_chunk_size(ctg) <-1750 #ivan had 250 , wont work if there are no (or too few??) points in a chunk
opt_chunk_buffer(ctg) <- 30 #ivan had 10
opt_output_files(ctg) <- paste0(tempdir(), "/{ID}__norm")
#trial1_PMF2_norm<- normalize_height(trial1_PMF2, tin(), na.rm = TRUE)
ctg_norm<- normalize_height(ctg, tin(), na.rm = TRUE)


#classify niise again? idk but ivan didnt have this step here
opt_chunk_size(ctg_norm) <-750 #ivan had 250
opt_chunk_buffer(ctg_norm) <- 30 #ivan had 10
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{ID}")
ctg_norm<- classify_noise(ctg_norm, sor(8,1)) #ivan had 8,1

opt_filter(ctg_norm) = "-drop_z_below 0 -drop_z_above 140"
#las file similar step would be: ctg_norm <- filter_poi(ctg_norm, Classification != LASNOISE)  ......... but not really bc this is removing noice hereas the line above this is just saying to remove z below and aobe some value
#and ifyou had a las file you could also reduce the extent with:
#ctg_norm<-clip_rectangle(ctg_norm, xleft = 2290061, ybottom=899500,xright= 2230000,ytop= 902800)
#... then write it back to the folder
#writeLAS(ctg_norm,"C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las2delete/n17noiseless.las")
#here's how the opt_output_files is supposed to look if your actually leaving stuff in the orig folder:
#opt_output_files(ctg) <- "folder/where/to/store/outputs/dtm_{ORIGINALFILENAME}"

#this saves ctg_norm as a tif file and  "rasterize_canopy.vrt" somehow, not sure tho 
opt_output_files(ctg_norm) <- "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/las3outputdelete/n17"


opt_chunk_size(ctg_norm) <-1750
opt_chunk_buffer(ctg_norm) <- 30
opt_output_files(ctg_norm) <- paste0(tempdir(), "/_{ID}", overwrite=F)
CHM <- grid_canopy(ctg_norm, res=1.5, p2r(0.65)) #in feet per pixesl #these numbers worked for me as of 6/22/23 sean
plot(CHM)
f <- file.path("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/ALS/Stand 1 - smit 55/17tif2delete.tif")
writeRaster(CHM, f, overwrite=TRUE,datatype="INT1U")#INT1U saves as gri if you dont put the extension at the end of the path, and i think even if you put "filetype=tif" or whatever in the args

#chm used in locate_trees-----
f2 <- function(x) { x * 0.07 + 3 }

#ttops <- locate_trees(ctg_norm, lmf(f, hmin=3)) #if you dont want to use the chm

ker <- matrix(1,3,3) #search kernel

chm2 <- terra::focal(CHM, w = ker, fun = mean, na.rm = TRUE) #generate new smoothed raster layer

ttops <- lidR::locate_trees(chm2, lmf(f2))
plot(ttops)
separated_coord<-data.frame(long = unlist(map(ttops$geometry,1)),
                            lat = unlist(map(ttops$geometry,2)),
                            z=1)

separated_coord<- st_drop_geometry(separated_coord)
#add z to the future csv data
separated_coord<-s2<-separated_coord
separated_coord<- separated_coord%>%cbind(.,zr=ttops$Z)

write.csv(file="G:/My Drive/Studies/FPC/SharedFolderSean/FPC Team Folder2/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Scans_TLS_ALS/ALS_TLS_Manulife_VarRatePlots_Centerville/wholestandtreessub.csv",separated_coord)


#delete---

x=c(1,2,3)
x
x=1,2
df<-data.frame(x,y=c(2,3,4))


df%>%
  mutate(.,z=2+2,
         3+3)




#convert state plane to latlong----
data<-separated_coord%>%
  select(c(long,lat))%>%
  mutate(.,long=long-1,lat=lat+2.6)

coordinates(data) <- ~ long+lat
proj4string(data) <- CRS("+init=epsg:6543")
latlong = data.frame(spTransform(data, CRS("+init=epsg:4326")))
setnames(latlong,c("long","lat"))
latlong

write.csv(file="C:/Temp/wholestandtrees2.csv",latlong)
#
#lidar stuff------
install.packages("FPCALSpackage")
library(FPCALSpackage)
fpc.lidar.app()  
#dates-----
#CAN DELETE THIS SECTION IF NEED BE
set.seed(1)
as.Date(sample.int(365,24), origin=as.Date("1970-01-01"))%>%sort()

#Merging: make bigcombotable: just merge the main meeting tbl with year by year===-----
#ok what if we just make massive out of execls?

yby<-read_excel("Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres.xlsx",
                sheet="Events-past-yearbyear-pres",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                trim_ws = T )
main<-read_excel("Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-maintable.xlsx",
                 sheet="Events-past-maintable",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                 #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                 trim_ws = T )

wex(main)  

massive<- merge(main,yby,
                by="linkparent",all=T,
                suffixes=c(".main",".yby"))



wex(massive)

#pick up polishing this off; this should have al the meeting info plus the links to old files. you can at least get the new google links working for some of the things. 

massive2<-merge(massive,googledmerge,by.x="linkbase.yby",by.y="name",all.x=T,all.y=T)%>%
  mutate(., glink=ifelse(  !is.na(id),
                           paste0("https://drive.google.com/open?id=",id),
                           ifelse(grepl("youtu\\.be",link),link,
                                  NA)))%>%
  arrange(indmain,indyby)%>%
  subset(!is.na(linkparent))

massive2%>%  
  subset(indyby!=285)%>%
  mutate(FPC.Meeting2=
           ifelse(grepl("Annual",FPC.Meeting,ignore.case=T),"Annual",
                  ifelse(grepl("WG|orking",FPC.Meeting,ignore.case=T),"Working Group",
                         ifelse(grepl("contact",FPC.Meeting,ignore.case=T),"Contact",
                                "Other"))))%>%
  mutate(Format=
           ifelse(grepl("\\.pdf|-pdf",link,ignore.case=F),"PDF",
                  ifelse(grepl("youtu\\.be",link,ignore.case=F),"Recording",NA)))%>%
  mutate(.,Presentation.sp=
           ifelse(duplicated(Presentation),target.yby,Presentation))%>%
  select(
    c(FPC.Meeting2,
      Year,
      Location,
      Presentation,
      Presentation.sp,
      Speaker,
      glink,
      Date.main,
      Date.yby,
      Time,
      indmain,
      indyby,
      FPC.Meeting,
      target.yby
    ))%>%wex()

dim()

subset(.,grepl("LiDAR - Bart A",target.yby))%>%
  print()

#pick up seeing if you can get a "if (contains forestproductivitycoop.net), replace with NA" kind of function going for the "link" column (this is where the actual files were linked on yby pages)

write.csv(.,file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-bigcombo.csv")
view()




#Downloads/extractions: Untar Rachel's wp dwnlds and compare it with my mlfd dwnlds-----
#Update 11/23 i think i already moved the tar ones to the google drive but if not this is a good way to start seeing which ones arent in the googledrove
#this is how I extracted the stuff rachel downloaded and put into dropbox
untar("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/download-managers-backup.tar",
      exdir="C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar",verbose = T)

wex(tls)

tarf<-"HALLK-EucGY-FPC-2015-annual-meeting.pdf"
tabf<-"hallk-eucgy-fpc-2015-annual-meeting-pdf" #tabf
"\\.pdf"
"\\.ppt$"

tolower(gsub("\\.pdf$","-pdf",tarf))==tabf#works

tlsb<-tolower(gsub("\\.pdf$","-pdf",basename(tls2)))

#no weirdos
tls2<-(tls[-c(957,1030,1529,1578,1600,1611,1619,2595,2603,2607,2613,2624,2738,2741,2749,2758,2760,2762,2745:2907)])

lkb<-pdf_links%>%pull(linkbase)
tlsb%in%lkb%>%
  sum()# so they do match a ton of them. lets make a way to get the ones we need into the folder where tey should be

#As of 11/2023 you can see what got downloded into either the dropbox folder rachel downloaded,...
#set wd to the main folder rachel downloaded from wp
setwd("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar")
#Or the gdrive mlfd one Sean downloaded:
setwd("Q:/Shared drives/FER - FPC Team/Website/Htmltables/wordpressdownloads/mlfd-forestproductivitycoop2")


#final .net formatting to merge googled links with meeting info, don't delete-----

#So at one point i was merging like this: (tbl was an old version of the .net years' page tables)
#merge(tbl,googledmerge,by.x="linkbase",by.y="name",all.x=T,all.y=F,sort=T)%>%
#googledmerge has the name as it appeared in the .net link so this works
#so, if I put files in the google drive with titles that match whats in the yby (equibalend to tbl above), then i can do this same merge
#therefore, at one point I the next step was get all the files from the [I assume tar] dropbox out of their hierarchy
#... and then put them into the googledrive folder

#So once everything's in the google drive folder, you can merge the meeting table info with the links.
#Therefore you will need this in a little bit as the think to merge onto:
pdf_links<-"Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres.csv"%>%
  read.csv(encoding="UTF-8")

#General architecture of if you wanted to move all to the base folder (wd folder)
  file.rename(list.files,basename(PUTlist.filesBACKHERE))

# Replace the special characters with their corresponding replacements
fixed_strings <- str_replace_all(strings, c(
  '├▒'= "n" ,
  "├í"="a",
  '├⌐'="e",
  '├│'="o"
))

#make named char vector of effed spanish characters
spanc<-c('├▒'= "n" ,
          "├í"="a",
          '├⌐'="e",
          '├│'="o")

#ok test run
"13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"%>%
  gsub("\\.pdf$","-pdf",.)%>%
str_remove_all(.,spanc)%>%  
  tolower()

#ok now what happens same thing in googldmerge?
googledmerge%>%
  subset(grepl("├",name))%>%
  mutate(.,name=gsub("\\.pdf$","-pdf",name))%>%
  mutate(.,name=gsub("-pdf-pdf$","-pdf",name))%>%#some have pdf in the name already but we want them to all end up with just one "-pdf"
  mutate(.,name=str_remove_all(name,spanc))%>%  
  mutate(.,name=tolower(name))%>%
merge(.,read.csv(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres4.csv"),
      by.x="name",by.y="linkbase",all.x=T,all.y=F)%>%
  wex()

  file.rename(from=paste0("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads/13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"),
              to=paste0(getwd(),"/",.))

#works dont fuck
file.rename(from=paste0("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads/13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"),
            to=paste0(getwd(),"/",tolower(gsub("\\.pdf$","-pdf","13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"))))




#D1

# remove punctuations characters 
str_replace_all(")", "[[:punct:]]", "")

grepl("[[:punct:]]","├")
grepl("[^[\\-]]&[^[:alnum:]]","a-")
grepl("[^[:alnum:]]","a-")


str_replace_all(string, "[^[:alnum:]]", "")
grepl("^[[:alnum:]_-]+$","3a-_A") #find rows thta have anything but ª and alphanumeric and _ and -
#pick up gettin gthis subset of gogldrive files, then put their things on a table
#MD1

#11/8
#need to do the spchars and then also replace periods and then 
#so options are 
#1 rbind the few that this spanc stuff fixes onto one of the yby htings like the pres4
#2 Restore the problem spanish character lines to pres4 using meeetingstablehtmllinktargets
#3 maybe use the bigcombotable for these messed up ones in pres4?
#anyways goal is take pres4 and fix the effed 2017 and others with weird characters by stealing from OR merging with meeetingstablehtmllinktargets


#so. which csvs have broken cells?
#meetingtabletargethtml doesnt, but it is missing lots of googleinks
#pres2 and pres3 do
#bigcombotable doesnt, and has no google links
#what is the minimum we need forom pres2 or 4?
#ok first what can we even get from pres2 or 4? well allot ar effed so can we select against the noneffed?
#then we can get anything.
#can i just select against "link_target" that doesnt have (,) format?

#
#https://forestproductivitycoop.net/?page_id=13306 are the only ones we need google links in the new folder? 
#no, there's all the oens with the spancharacters,
#ones with "--pdf" at the end
# a few with perionds that got changed to "-"
####jjq.ecofis.anualmeeting.2017-pdf becomes jjq-ecofis-anualmeeting-2017-pdf
#lots that i genuinely cant find like k-carryover-184202
#some that mabe had spaces or special charcters when uplodad so they become laprw2-selecci%c2%a6n-del-dise%c2%a6o-de-plantaci%c2%a6n_huape-pdf; this one is a pptx in dropbox with both space and periods, but no pdf
######tyhis one in particular exists in 3 places: 
######1. the copy of the dropbox meetings folders as a pptx and as a pdf (so two places in one) in fpc fer team google drive, and 
######2. in mlfd2 as laprw2.-selecci┬ªn-del-dise┬ªo-de-plantaci┬ªn_huape-pdf and
######3. the copy of the dropbox meetings folders as only          a pdf                 what brody put in fpc  google drive, and 

#workspace for final websitetable stuffB=====================================================================D-----

#0. some stuff to see what's going on with pres2
read.csv(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres2.csv",header=T)%>%
  select(c(indyby,linkbase,glink.f2,glink.mt,x))%>%
  #subset(.,grepl("--pdf",linkbase))%>%# note there's none with this format in the website tables, its just a filename thing in ggoledrive
  subset(.,grepl("\\.",substr(linkbase,1,nchar(linkbase)-4)))%>% #so theres not many with periods outside the end and it doesnt matter bc the googledrive seems to have that tooo
  pull(linkbase)
#note pres2 has meeting classifcaation stuff but is missing lots of glinks

#0. some stuff to see what's going on with pres4
read.csv(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres4.csv",header=T)%>%
  select(c(indyby,linkbase,glink))%>%
  #subset(.,grepl("--pdf",linkbase))%>%# note there's none with this format in the website tables, its just a filename thing in ggoledrive
  subset(.,grepl("\\.",substr(linkbase,1,nchar(linkbase)-4)))%>% #so theres not many with periods outside the end and it doesnt matter bc the googledrive seems to have that tooo
  pull(linkbase)


#1. get almost done pres table but it has cells messed up by spanchars
read.csv(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres4.csv",header=T)%>%
  subset(is.na(link_target)|!base::startsWith(link_target,"("))->prescellseffed

#2 get almost done pres table but w/o the messed up cells
read.csv(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres4.csv",header=T)%>%
  subset(base::startsWith(link_target,"("))->prescellsOK



#3 is there anything to salvage from prescellseffed?
wex(prescellseffed)
#Yes. We need to get the linkparent and linkbase for sure, might as well get indyby
prescellseffeds<-prescellseffed%>%select(c(indyby,linkparent,linkbase))

#so now this subset needs its google links back as 
#do go ahead and get ones that are "-pdf.pdf" format switched bc those arent good
googledmerge2<-
  googledmerge%>%
  mutate(.,name=gsub("-pdf\\.pdf$","-pdf",name)  )%>%
  mutate(.,name=
      # Replace the special characters with their corresponding replacements bc the website tables dont have the special characters
    str_replace_all(name, c(
      '├▒'= "n" ,
      "├í"="a",
      '├⌐'="e",
      '├│'="o",
      '┬ª'="%c2%a6",
      "--pdf"="-pdf"
    ))           )

#merge googledmerge2 and prescelleffeds; get rid of ones w/o google links
prescellseffeds1<-
  merge(prescellseffeds,googledmerge2,by.x="linkbase",by.y="name",all.x=T)%>%
  subset(!is.na(id))

#merge googledmerge2 and prescelleffeds; keep only ones w/o glinks
prescellseffeds2<-
merge(prescellseffeds,googledmerge2,by.x="linkbase",by.y="name",all.x=T)%>%
  subset(is.na(id))
#ok theres a few that still wont merge; make a special googleid table with no periods for the few that are missing periods in the html tables
googledmerge3<-  googledmerge2%>%
  #mutate(.,nam=substr(name,1,10))%>% #get first few characters
mutate(.,name=gsub("\\.", "", name))

#get ones that still dont have googleids and fix them
prescellseffeds2<-prescellseffeds2%>%
select(-id)%>%
    #mutate(.,linkba=substr(linkbase,1,10))%>%
  merge(.,googledmerge3,by.x="linkbase",by.y="name",all.x=T)

#put them back togeteher and write over prescelleffeds
prescellseffeds<-rbind(prescellseffeds1,prescellseffeds2)

#note need to eventually put these back somewhere : put this these need need to be moved need to move
#overall 30k foot view of links that need fixing:
#                            
#dm (domain)              total 
#docs.google.com               1 #subset(dm=="docs.google.com") #keep this, it still works
#drive.google.com            530 #these are golden
#forestproductivitycoop.net   21 #these need fixing if possible but not sure where the files are; also maybe some are mp4s?
#mymediasite.online.ncsu.edu   2 #subset(dm=="mymediasite.online.ncsu.edu") #can get rid of these, they are obsolete and there are only 2
#www.dropbox.com              19 #subset(dm=="www.dropbox.com") #can get rid of these, they are deleted files that i think i messed up; only 26 anyways
#youtu.be                     25 #these should all be fine, haven't checked all but probably ok

#And then specific detailed view of links that need fixing for a handful of them:
  # Rubilar_RW23-CM-Intensidad-y-Frecuencia-8-a https://forestproductivitycoop.net/?page_id=13306" fnas
  #rw7b-respuesta-a-la-preparaci%c2%a6n-de-suelo-y-cm_venado-pdf
  #laprw2-selecci%c2%a6n-del-dise%c2%a6o-de-plantaci%c2%a6n_huape-pdf
#Presentation
#197 Update on strategic planning results from 2020 & Estrat\xe9gia para a Am\xe9rica Latina 2021 - CookUpdate on strategic planning results from 2020 & Estrat\xe9gia para a Am\xe9rica Latina 2021 - Carter
#Speaker  Date       Time indyby
#197 Cook, Carter, Rubilar, Campoe 4-May 9:30-10:00    190
#target
#197 Update on strategic planning results from 2020 & Estrat\xe9gia para a Am\xe9rica Latina 2021 - Cook
#..... and then any vieos in this folder need to be linked
#https://drive.google.com/drive/folders/1YQjWMTqHHYNQlzuoQ0-dm5lmGSOaaWce?usp=drive_link

#also indyby 156 and 710 don;t have meetnig-level (bigcombo) info for some reason so add that.



#also need to merge glinks onto ones that werent effed cells
#first fine files in google mlfd that have weird chars

#works dont fuck but delete any time after glinks added to htmltables
data.frame(n=(1:3),
JJ=c("assorted","sfas├▒","asfasf"))%>%
subset(unname(apply(sapply(JJ,str_detect,c('├▒',"├í",'├⌐','├│','┬ª',"--pdf")),2,any)))

#so fine files in google mlfd that have weird chars
googledmerge4<-googledmerge%>%
  subset(unname(apply(sapply(name,str_detect,c('├▒',"├í",'├⌐','├│','┬ª',"--pdf")),2,any)))%>%
  mutate(.,name=
           # Replace the special characters with their corresponding replacements bc the website tables dont have the special characters
           str_replace_all(name, c(
             '├▒'= "n" ,
             "├í"="a",
             '├⌐'="e",
             '├│'="o",
             '┬ª'="%c2%a6",
             "--pdf"="-pdf"
           ))           )

#now get spechail char ok cells table
prescellsOK%>%
  merge(googledmerge4,.,by.x="name",by.y="linkbase",all.x=T,all.y=F)%>%
  head()
#now get no special char ok cells table
prescellsOKchars%>%pull(indyby)

#"03-rw18-field-presentation-contact-meeting-2015--pdf" in gdrive needs to match with "03-rw18-field-presentation-contact-meeting-2015-pdf"
prescellsOK%>%
  subset(.,grepl("Nestor",linkbase))


prescellsOK%>%
  view()
  


#ok a bunch that still dont have glinks are in  eetings folders. can i move them?
from<-"Q:/Shared drives/FER - FPC Team/Meetings/Strategic/2020/Murals/Day 4 Final Topics.pdf"
to<-"Q:/Shared drives/FER - FPC Team/Website/Htmltables/wordpressdownloads/mlfd-forestproductivitycoop2/Day 4 Final Topics.pdf"
file.copy(from=from,to=to)
#file.rename(from=to,to=from)

#great so that works, lets try to do it on a batch:
#1.make the main table:
pdlks<-rbind.fill(prescellsOK,prescellseffeds)
#there are blanks, make them NA's to make subsetting stuff easier
pdlks[pdlks==""] <- NA



#now get ones that have no link still and should have one
pdlks1<-#part of final that had the "--pdf" thing
pdlks%>%
  subset(.,is.na(id))%>% #ones that dont have glinks now
    subset(is.na(link_target)|link_target!="(, )")%>% #these never had anylinks
subset(!grepl("youtu",link))%>% #dont want videios
  subset(!is.na(linkbase))%>%
    mutate(.,linkbase=
      # Replace the special characters with their corresponding replacements bc the file names dont have specail chars
    str_replace_all(linkbase, c('%20'= " ")))%>%
  subset(.,grepl("pdf",linkbase))%>%
  #subset(grepl("2016-euc-wg-contact-meeting-field-trip-pctures",linkbase))%>%
  merge(.,googledmerge2,by.x="linkbase",by.y="name")%>%
  mutate(.,id=coalesce(id.y,id.x))%>%
  select(-c(id.x,id.y))
#make pdkls even better
pdlks44<-
pdlks%>%
  subset(.,!indyby%in%pdlks1$indyby)%>%
      rbind(.,pdlks1)%>% #... and addd the fixed rows back so no need to coalesce over all 700+ rows
  tail()#then get rid of rows you fixed in pdkls1 
#anythign still missing from pdlks?
pdlks44%>%
  wana
#yea somehow just rbinded some rows that only have indby, linkbase, glink, and linkparent
#ok right that was on purpose bc the cells were effed; need to get them from bigcombo table
#what distingueshes these?
#easy, they have NA link_target; so read in bigcombo and rbind a subset
bigcombo<-read.csv("Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-bigcombo.csv")
#now subset pdlks44 so no NA linktarg:
pdlks45<-
  pdlks44%>%
  subset(.,!is.na(link_target))%>%
select(-X)  
#and get the one with the na linktarg so you can merge with bigcombo, at least a small subset of it that is
pdlks46<-
  pdlks44%>%
  subset(.,is.na(link_target))%>%
  select(-X)  
#ok just need to merge the ones with stuff
pdlks46<-pdlks46[,!apply(pdlks46,2,function(x){all(is.na(x))})]
#then get bigcombo just for those in pdlks45 with no linktarge
bigcombosm<-bigcombo[!bigcombo$indyby%in%pdlks45$indyby,]%>%
  rename("Date"=Date.yby,"target"=target.yby)%>%
  select(names(.)[names(.)%in%names(pdlks45)])%>%
  select(-glink)
bigcombosmlks<-merge(bigcombosm,pdlks46,by="indyby")

#if i rbind and fill will that be done?
filenamsp<-
rbind.fill(pdlks45,bigcombosmlks)%>%
select(-c(glink,link_target))%>% #glink is superseded by id; all info in the original link_ is in column link, and then everything that was in _target is either in Presentation or target
arrange(indyby)%>%
  subset(.,is.na(id)&!is.na(linkbase))%>%
  subset(!grepl("youtu",link))%>%
  subset(!grepl("mp4",link))%>%
pull(linkbase)%>%
    str_replace_all(.,
                  c("%20"=" ",
                    "\\?dl=0"="")
  )
#no, theres still these that are missing glinks. filenamps was the problem linkbases that still dont have ids

#So i moved a few files with some code i can now forget i think.
#that doesnt add anything to the 
#but i want to see if i can salvege/match some more of the ones in linkbase 
#so first lets create another step data-
pdlks47<-
  rbind.fill(pdlks45,bigcombosmlks)

#now lets see what we can rbind or merge to that or to a subset of that
pdlks47<- pdlks47%>%
  select(-c(glink,link_target))%>% #glink is superseded by id; all info in the original link_ is in column link, and then everything that was in _target is either in Presentation or target
  arrange(indyby)

#so i think part of the answer is the gmertge4 bc i never used it to put things on a main thing  
pdlks49<-googledmerge4%>%
  mutate(.,name=tolower(
           str_replace_all(name,
                           c("\\.pdf"="-pdf",
                             "-pdf-pdf"="-pdf",
                             "\\."=""
                             )
           )
  ))%>%
merge(.,pdlks48,by.x="name",by.y="linkbase",all.x=T,all.y=F)%>%
subset(!is.na(indyby))%>%
  rename("id"=id.x)%>%
  select(-id.y)%>%
  rename("linkbase"=name)

#now put 49 on 47; 50 is almost golden
pdlks50<-pdlks47[!pdlks47$indyby%in%pdlks49$indyby,]%>%
rbind(.,pdlks49)%>%
  arrange(as.numeric(indyby))

#anything else easy? prob not, caution about getting into more
pdlks48<- pdlks50%>%
  subset(.,is.na(id)&!is.na(linkbase))%>%
  subset(!grepl("youtu",link))%>%
  subset(!grepl("mp4",link))

#ok maybe one more bro
googledmerge5<-
  googledmerge%>%
  mutate(linkbase2=str_replace_all(name,"[^[:alnum:]]",""))%>%
  mutate(linkbase2=tolower(str_replace_all(linkbase2,"pdf","")))#%>%
  pull(linkbase2)%>%sort()%>%wex()
  
  #ok last one i swear
pdlks481<-pdlks48%>%
  mutate(linkbase=gsub("%20","",linkbase))%>%
  mutate(linkbase=gsub(fixed =T, ".pdf?dl=0","",linkbase))%>%
#[1] "Day%202%20Veg%20Management.pdf?dl=0" 
  mutate(linkbase2=str_replace_all(linkbase,"[^[:alnum:]]",""))%>%
mutate(linkbase2=tolower(str_replace_all(linkbase2,"pdf","")))%>%
  merge(.,googledmerge5,by="linkbase2",all.y=F)%>%
  rename("id"=id.y)%>%
  select(-c(id.x,linkbase2,name))
#yes 14 more rows!
#why the fuck didnt i start with that  alnum shit?
  
#nayways rbind this to the subset inverse of pgdlks50
pdlks51<-pdlks50[!pdlks50$indyby%in%pdlks481$indyby,]%>%
  rbind(.,pdlks481)%>%
  arrange(as.numeric(indyby))
#is this ok?
wana(pdlks51)
#yea that;s gold
#coalse the youtube etc links and the gids
pdlks52<-pdlks51%>%
  mutate(.,dm=domain(link),ind=1)%>%
  #subset(dm=="www.dropbox.com") #can get rid of these, they are deleted files that i think i messed up; only 26 anyways
  #subset(dm=="mymediasite.online.ncsu.edu") #can get rid of these, they are obsolete and there are only 2
  #subset(dm=="docs.google.com") #keep this, it still works
  mutate(., linknew=#first make glinks out of any that have g ids
           ifelse(!is.na(id),paste0("https://drive.google.com/open?id=",id),NA))%>%
  mutate(.,linknew=coalesce(linknew,link))%>%
  mutate(.,dm=domain(linknew),ind=1)
#now get the table that has all the meeting level info, not just the yearbyyear/presentation level info
bigcombo<-read.csv("Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-bigcombo.csv")

#so some last formatting thins and putt with the meeting info
  bigcombo%>%
  rename("Date"=Date.yby,"target"=target.yby)%>%
  select(indyby,names(.)[!names(.)%in%names(pdlks52)])%>%
    select(-glink)%>%
merge(.,pdlks52,by="indyby",all=T)%>%
    arrange(as.numeric(indyby))%>%
    select(-c(ind,id,dm))%>%
    write.csv("Q:/Shared drives/FER - FPC Team/Website/Htmltables/meetingtablehtmllinkstargets.csv",row.names = F)
#now get an second version that is like the goglesheet  meetingtablehtmllinkstargets.gsheet, "Q:\Shared drives\FER - FPC Team\Website\Htmltables\meetingtablehtmllinkstargets.gsheet", https://docs.google.com/spreadsheets/d/1Z1PGyYdzn5W2_jbypl1uwMoyb4XZ3oLKgGs94oZPmUM?usp=drive_fs 
twt<-  read.csv("Q:/Shared drives/FER - FPC Team/Website/Htmltables/meetingtablehtmllinkstargets.csv",header = T)

#Ok now read it back in to select only the columns that go in the googlesheet; 
twtf<-twt%>% #twtf is "that writeable table final" as in final for googlesheets
  mutate(Presentation2=Presentation)%>%#make a placeholder copy bc the googlesheet has a linked version of the same column
  mutate(Speaker.list="")%>% #another plaeholder for eventual list??
  mutate("Speaker.old"=Speaker)%>% #these need to be tidied up but leaving them in as old speaker bc its the only place that info exists now
  select(c(FPC.Meeting2,Year,Location,Presentation,Presentation2,Speaker,linknew,Date.main,Date,Time,indmain,indyby,FPC.Meeting,Presentation.concat,target,Speaker.list,Speaker.old))
  #note this just leaves out  "linkbase"      "linkparent"   "link"          "Working.Group"; dont need these for the website
  #see here are the columns in the gsheet as of 11/12/23:
  
#so now lets make a connection to the main gsheet the .org meetings table links to and write to there:
#meetingtablehtmllinkstargets is #https://docs.google.com/spreadsheets/d/1Z1PGyYdzn5W2_jbypl1uwMoyb4XZ3oLKgGs94oZPmUM?usp=drive_fs which is "Q:\Shared drives\FER - FPC Team\Website\Htmltables\meetingtablehtmllinkstargets.gsheet"
mtb<- ("https://docs.google.com/spreadsheets/d/1Z1PGyYdzn5W2_jbypl1uwMoyb4XZ3oLKgGs94oZPmUM/edit#gid=0")
sheet_write(twtf, mtb, sheet = "meetingtablehtmllinkstargets")
#If successful this should tell you this in the Console:
#"✔ Writing to meetingtablehtmllinkstargets."
#"✔ Writing to sheet meetingtablehtmllinkstargets."

  
#Wbesite table Final steps are some QC========------
#make a random subset of the meetings pres that has two links per meeting (for meetings with links)
setwd("Q:/Shared drives/FER - FPC Team/Website/Htmltables/")
set.seed(6660)
subs<-
  twt%>%
  subset(!linknew=="")%>%#just want ones with linkes
split(.,.$indmain)%>%
lapply(.,function(x)
{slice_sample(x,n=2)}
  )
#then write it
rbindlist(subs,idcol = "indmain")%>%
  write.csv("meetingtablehtmllinkstargets_randomsubsetdelete.csv")

#github-----
setwd("Q:/My Drive/Studies/FPC/Scripts")
#load(".RData")
wael()

