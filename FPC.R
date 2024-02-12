#source("Q:/My Drive/Library/datlibmgr.R")
#rw185201:read in----
#setwd("C:/Users/seanb/")
#setwd("C:/Users/sabloszi")
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

#rw185201:aggregate the data to get means by plot----
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


#rw185201:get growth (difference over 6 years) data-----
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

#rw185201:1 boxplots with whiskerys-----
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
#rw185201:one plot at a time but with standard error se 's------
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





#rw185201:2 all data------
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


#rw185201:2.1 all variables but just means instead of raw points------
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



#rw185201:2.2 just growth measurements (means of those by trt)-----
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

#rw185201:2.4 just yearly measurements, and just means of those-----


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



#rw185201:2.3 just tpa trees per acre-----



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





#rw185201:3 anova-----
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

#rw185201:#151.         Means sep for "lbcextr" soil data-----

#note this is unfinished and is just copied and pasted from another similar designed experiment from my agronomic soil science postdoc
#... and eventually could be modified for any blocked data

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
#ecm to survey123------
#you can recreate this if you go to an email Mon, Feb 5, 3:52PM from talbaugh@vt.edu "Ca trials at Hofmann" and get all those excel attachments:
setwd("C:/Users/sabloszi/Downloads")

list(
ca994001=read_excel(paste0(getwd(),"/994001 ECM(1).xlsx"),#nevermind all the "(1)"'s on these file names thats just bc i made an exact copy of the original at some point
                sheet="994001", 
                trim_ws = T ),
ca994002=read_excel(paste0(getwd(),"/994002 ECM(1).xlsx"),
           sheet="994002", 
           trim_ws = T ),
ca994003=read_excel(paste0(getwd(),"/994003 ECM(1).xlsx"),
           sheet="994003", 
           trim_ws = T ),
ca994004=read_excel(paste0(getwd(),"/994004 ECM(1).xlsx"),
           sheet="994004", 
           trim_ws = T ))%>%
  rbindlist()%>%
  select(c(STUDY,PLOT,TREE_No,
           HT,#HT_L is what it becomes
           HTLC,#HTLC_L
           DBH,#	DBH_L,
           MORT,
           DAM))%>%
wex() #i put this on the following media csv I got from treegrodat: C:\Users\sabloszi\ArcGIS\My Survey Designs\TreeGroDat3\media\TreeGroDat_LASTYEAR.csv , below all the 28s data




  
994004
#F
#google sheets read inn=-----
#fuck<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1foGV8kUzgDiTsx0coouDvG0TWm7eks7DYMZt0Gx-e_4/edit#gid=1283512166')
link_to_Gsheets <- "https://docs.google.com/spreadsheets/d/1foGV8kUzgDiTsx0coouDvG0TWm7eks7DYMZt0Gx-e_4/edit#gid=1283512166"

#if you dont do the coltypes it fucks up and gets lists from some columns which is hard to deal with
datadbh311301<-read_sheet(link_to_Gsheets, sheet="311301",col_types = "iiinnncccc--")

#see trees per plot
#datadbh281303%>%
datadbh311301%>%
  as.data.frame()%>%
  dplyr::select(c(PLOT,MORT))%>% #select is comgin from somewhere else if not explicit
  melt(id.vars=c("PLOT"))%>%
  cast(PLOT~variable)%>%
    print()

datadbh <-  link_to_Gsheets %>%
  sheet_names() %>% #get names
  set_names()  #give names
#%>%  map_df(read_sheet, ss = link_to_Gsheets, .id = "Cut") #if you want to combine sheets (usually you wouldnt unless they are essentially the same ; this is like rbindrows more or less)

#read in the fieldmaps data, and format it so it will rbind nicely
datadbh3113012<-read.csv(  "C:/Users/angelcruz13/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW31_Funga/311301 BLSF/Data/311301_20240117_htlc-ht-dbh-dam-mort.csv")%>%
  rename_with(.fn = function(x){gsub("L0TREE_points.","",x)})%>%
  select(-OBJECTID)%>%
  rename_with(.fn = function(x){gsub("TREEview_AddSpatialJoin.","",x)})%>%
  select(Id,TREE_No,NEW_MORT,NEW_DAM,NEW_HT,	NEW_HTLC,	NEW_DBH,New_Comments)%>%
  rename_with(.fn = function(x){gsub("NEW_","",x,ignore.case=T)})%>%
rename(PLOT="Id")%>%
  mutate(.,STUDY=311301)%>%
  mutate(.,date=20240117)

#read in RE row end data
res<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW31_Funga/311301 BLSF/Data/311301_20240117_htlc-ht-dbh-dam-mort_RE.csv",header=T)


#then rbind that with the data from the lab
 list(datadbh3113012,(datadbh311301))%>%
rbindlist(fill=T)%>%
    subset(PLOT%in%1:16)%>% #not sure how but one tree from testing at Sean's house ended up with the BLSF data; delete it and subset against it by picking ones with plot names only
  mutate_at(vars(MORT), ~replace(., .=="", "A"))%>% #there are blanks for the mort and they need to say A bc we were skipping entering A on t he paper and app to save time
  mutate_at(vars(DAM), ~replace(., .==""&MORT!="Y", "A"))%>% #make the dam's A for blank dam's only if it's not a dead tree
   merge(.,res,by=c("PLOT","TREE_No"),all=T)%>%
  mutate(Comments=(na_if(Comments,"")))%>%
  mutate(New_Comments=(na_if(New_Comments,"")))%>%
   mutate(.,Comments=coalesce(Comments,New_Comments))%>%
  write.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW31_Funga/311301 BLSF/Data/311301_202401XX_htlc-ht-dbh-dam-mort.csv")


  

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


#heatmap------
tree%>% #tree raw straight up from  rwdb
  subset(STDY%in%c(180601,184501,184401,181502,281303))%>% 
  select(STDY,PLOT,TREE_No)%>%
  mutate(.,PT=paste0(PLOT,TREE_No))%>%
  group_by(STDY)%>%
  reframe(a=length(unique(PT)))->dat
#split by study b/c its too big with all the studies
a2<-
dat%>%
  split(.,.$STDY)%>%
  lapply(.,function(x){
    x<-as.data.frame(x)
  mutate(x,sp=paste0(STDY,PLOT))})
#make the figure
ggplot(a2[[4]] , aes(x=as.factor(YST), y=sp, fill = a)) + 
    geom_tile(color = "white", size = 0.1) + 
    scale_x_discrete(expand=c(0,0)) + 
    scale_y_discrete(expand=c(0,0)) + 
    scale_fill_viridis(name="# of trs", option = "plasma") + 
    coord_equal() + 
    labs(x="Call hour", y=NULL, title=sprintf("trees")) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  
  ## A tibble: 6 × 3
  #STDY   YST     a
  #<int> <dbl> <int>
  #  1 184401     0  1502
  #2 184401     0  1517
  #3 185201     0  2553
  #4 185201     0  2570
  #5 185201    10 22593
  #6 185201    10 22652
#so the bigsampletable insinuates pretty clearly that 4 samples, ranging 23231-23282, were sent to Funga and archived; i assume fall 2022;
#... but there is no yst info or DATE_COLLECT on these so where can i look to see what ive already had someone package?
#1. Is that in a funga doc? ("C:\Users\sabloszi\Dropbox (FPC)\FPC Team Folder\RegionWide Trials\Special Studies\Funga fungal microbiome\Funga FPC Site Summary 6-22-2022_Plotnumbers.csv")
#.... Yes, it is; 4401 was sampled 7/25/2015 for the ones we sent to funga, and they came from boxes, so i assume we took some boxes out and left the rest where
#... they were in the correugated box. So i am pretty confident that the numbers tim gave us ended up actually going to 2015, not 2013 samples like I 
#... had asked him for, incorrectly. tds2023-11-13
#2. So, now what? 
#2.1 Let tim KNow that the samples that went to funga are two years newer than I said; also make sure he can still use/reassign these smaple numbers to the 2015 date and whether missing 2013 soil is bad
#2.2 See whether the 185201 numbers are OK; does tim think these are 2015 or 2013? The tds2023-11-13 email from him sais 20130525
#2.2.1 Can i tell what those numbers are in the rwdb? 
# Yes, because it says yst 10 on both the rwdb and the tds2023-11-13 email xlsx
#2.2.2 Therefore I also need to tell him these are 2015 soils and ask whether that changes the next steps
stdy%>%
  subset(STDY%in%c(185201 ,184401))%>%view()
#ok so yst=0 is 2003 for 185201 and 2000 for 4401
  #pick up here after tim replies. you should move this to a archive section or doc or something. 

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
#I
#J
#K
#L
#M
#N
#O
#pdfs combine or split-----

setwd(
  "Q:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/Official/HR/Contracts"
  )

qpdf::pdf_combine(input = 
                    c(
                      "Access Agreement NC-FPC_2023_v1 -1.pdf"
                      ,
                      "PotlatchDeltic Corporation - North Carolina State University - 24011929333041 - 570103657335"
                    )
                  ,
                  output = "Independent-Contractor-Agreement_Rantizo_20242.pdf",)


qpdf::pdf_combine(input = 
                    c(
"Independent-Contractor-Agreement_Rantizo_2024.pdf"
                      ,
"Independent-Contractor-Agreement_Rantizo_2024_Scope.pdf"
                      )
  ,
                  output = "Independent-Contractor-Agreement_Rantizo_2024.pdf",)


qpdf::pdf_combine(input = c("B:/20221215_ROSS_CLARK_CIR.pdf",
                            "B:/20221215_ROSS_CLARK_CIR2.pdf"
),
output = "G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/Official/Pcard/Receipts/20221215_uspsdothan_doesoilsshipping.pdf")
qpdf::pdf_combine(input = c(
"Q:\\My Drive\\Studies\\FPC\\SharedFolderSean\\Bloszies (1)\\NCSU\\Official\\Pcard\\Receipts\\20231127_industrialmarkingpensCom_markers_584115_1.pdf",
"Q:\\My Drive\\Studies\\FPC\\SharedFolderSean\\Bloszies (1)\\NCSU\\Official\\Pcard\\Receipts\\20231127_industrialmarkingpensCom_markers_584115_2.pdf"),
output="Q:\\My Drive\\Studies\\FPC\\SharedFolderSean\\Bloszies (1)\\NCSU\\Official\\Pcard\\Receipts\\20231127_industrialmarkingpensCom_markers_584115.pdf")
#i want to split a pdf into individual pages
setwd("Q:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/Official/Pcard/Receipts")
pdf_split("PDFcopyofreceiptsinfolder.pdf", output = NULL, password = "")

#fnas crtrlf afjatynfnwss pick up here 11/20 : see which pdfs of research summaries elijah hasnt done by going to https://drive.google.com/drive/folders/1hIYQAQkAil0VSnBHG5aA6WahP4kp3qoE?usp=drive_link aka the fer-fpc team > website folder

#plot list of all plots-----
list("281502"=c(1502,1503),
     "284501"=c(1600,1999))%>%
    bind_rows()%>%as.data.frame()%>%
    melt()

list(

     "284501"=c(2721,2360,2480,2361,2108,2481,1108,1721,1481,1000,1360,1361,1720,2720,1480,2000),
     "281502"=c(1480,2721,2361,2108,2720,2480,1108,1481,1360,1720,2481,1361,1721,2000,1000,2360),
     "284401"=c(1000,1481,2360,2720,1480,1720,2481,1108,1361,2108,2000,2480,1721,2721,2361,1360)
     )%>%
  bind_rows()%>%as.data.frame()%>%
  melt()%>%
  arrange(variable,value)%>%
  rename(STDY=variable,PLOT=value)%>%
mutate(.,PLOTgood=gsub("481","722",PLOT))%>%
mutate(.,PLOTgood=gsub("721","961",PLOT))%>%
mutate(.,PLOTgood=gsub("108","960",PLOT))%>%
  #  mutate(unite(.,STDY_PLOT))%>%
#  select(STDY_PLOT)%>%
#  expand_grid(1:40)%>%
as.data.frame()%>%
  subset(.,STDY=="284501")%>%
wex()



svgs%>%apply(.,1,function(x){as.character(x)})%>%as.list()->svgsl

dir.create("Q:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Inventories/Trees/ids")
setwd(  "Q:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Inventories/Trees/ids")

mkqr<-function(x){
  qr_code(x,ecl="Q")%>%
    generate_svg(., filename = paste0(x,".svg"), show = F)}

#gold leave it be and get it into other stuff
lapply(svgsl,mkqr)
write.csv(svgs,"STDY_PLOT_TreeNoID.csv")
  

#qr codes-----

read.table(text="
PLOT
2412
2412
2000
1412
2624
2212
1624
1424
2206
1206
1212
1218
1211
2211
1000
2418
2218
1418
",header=T)%>%
  mutate(TREE_No=c(1,2,1:16),STDY=311301)%>%
  select(STDY,PLOT,TREE_No)%>%
  mutate(.,fpcid=unite(.,fpcid))%>%
pull(fpcid)%>%
  head(3)->svgs


qr_code("311301_2215_14",ecl = "Q")%>%
  plot()

#read in text w read.table-----
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


#ALPHA STARTS HERE*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@
#rwdb gold------
#just define the connection to the db from R; doesn't create an object in the global env that has actual rwdb data in it
#FRANCE, angelcruz13
dbpath=normalizePath(paste0(gsub("\\\\Documents","",Sys.getenv("HOME")),("\\Dropbox (FPC)\\FPC Team Folder\\Static RWDB\\RWDB static 20221017.mdb")))
#on france it looks like 
#"C:\\Users\\sabloszi\\Dropbox (FPC)\\FPC Team Folder\\Static RWDB\\RWDB static 20221017.mdb" in R as a result of above
#"C:\Users\sabloszi\Dropbox (FPC)\FPC Team Folder\Static RWDB\RWDB static 20221017.mdb" and this in the explorer path
#GRADS, shi-bloszies
dbpath=normalizePath(paste0(gsub("\\/Documents","",Sys.getenv("HOME")),("\\Dropbox (FPC)\\FPC Team Folder\\Static RWDB\\RWDB static 20221017.mdb")))
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
mort<-sqlFetch(conn, "dbo_MORTALITY")
splloc<-sqlFetch(conn,"dbo_SAMPLING_LOCATION")
tree<-sqlFetch(conn,"dbo_TREE_GROWTH")
stdy<-sqlFetch(conn,"dbo_STUDY_INFO")
trts<-sqlFetch(conn,"dbo_TREATMENTS")
sqlFetch(conn,"dbo_ACTIVITY_WORKPLAN")%>%wex()
depths<-sqlFetch(conn,"dbo_DEPTH_CODES")
sqlFetch(conn,"dbo_COMPANY_OPERATIONS")%>%
  subset(OPERATION_ID!=140)%>%
  subset(.,grepl("cock",OPERATION_NAME,ignore.case=T))
wex(sqlFetch(conn,"dbo_DOMINANT_HEIGHT_1"))
wex(sqlFetch(conn,"dbo_APPLIED_TREATMENTS"))
(sqlFetch(conn,"dbo_SAMPLING_LOCATION"))

#d
#So 11/2023 trying to figure out which 184401 and 185201 soils are already in the rwdb, physical archive, and bigsample table or any other locations
#1st, what sample nu,bers for 184401  and 185201 are there in the rwdb?
tree%>%
  #subset(STDY%in%c(184401))%>% #185201
  subset(substr(STDY,1,2)==28)%>% #185201
  select(STDY,YST,PLOT)%>% # `SAMPLE_#`,
pull(STDY)%>%unique()%>%c(.,c(282201))wex()
  #pull(PLOT)%>%
  c(.,rw0645,rw22)%>%
  unique()%>%
wex()

#delete
#280606
rw0645<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/Workplan/RW28 Workplan 20200522_trt_codes_only.csv",header=T)%>%
  pull(raw28)
read.table(text="
2412
2424
1412
2000
2624
2212
1624
1424
2206
1206
1212
1218
1211
2211
1000
2418
2218
1418
",header=F)%>%pull(V1)->rw22
#menddelete



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

#ALPHA ENDS HERE*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@*@*@*@*@*@*@**@@
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
set.seed(2)
as.Date(sample.int(365,4), origin=as.Date("1970-01-01"))%>%sort()
#Sean on annual leave

#file search indexing------

#First we have to specify where we want to look and what the nnicknames for those places are
nmchfls<-c(#named charecter vec of base filepaths
  googlemfpc="Q:/My Drive/Studies/FPC", #as of 11/2023 takes less than 10sec below
  googles_="Q:/Shared drives/FER - FPC",#as of 11/2023 takes less than 10sec below
  googlest="Q:/Shared drives/FER - FPC Team",#as of 11/2023 takes like 70sec (seventy) below
  dropa="C:/Users/sabloszi/Dropbox (FPC)"#as of 11/2023 takes less than 10sec below
)

#Now get the actual lists
#This is a function to go into all the places  you have in nmchfls and get the lists of files in each of those dirs
iu<-sapply(nmchfls, #index unnamed
  function(basep){
    restnmd=names(basep)
    itmp<-list.files(basep,recursive = T,include.dirs = T) #itmp: index temporary
    names(itmp)<-restnmd
    return(itmp)
  }
)

#Something to do eventually: get non-fpc my drive files index'd; also this shows the alternative dir_ls which is equivalent to list.files except it makes pretty colored paths and is a named character which could be useful; also maybe faster and has more argument options
#google my drive non fpc
#googlem<-dir_ls("Q:/My Drive/Studies/FPC",all=T,type="any",recurse = T)



##this is a function to put the list of path stuff in in a prettier, easier to navigate format.
mindex<-mapply(function(x,y){list(list(basep=x,restnmd=y))}, #y is suppoed to be the big thing, x is supposed to be the nmc
       nmchfls, # the named character vector of root paths
       iu       ) #the big thing of the all files paths within those root path directories

#actual search-----
#gold
#Define the search
sirch="right"
sirch2="of"
sirch3="entry"
#do the search
system.time(
u7s<-mindex%>% #.1 seconds
  lapply(., function(x) #https://stackoverflow.com/questions/14052612/extract-value-of-the-same-field-from-multiple-list-object-in-r
    x[["restnmd"]]
  )%>%
    lapply(., function(ch) subset(ch,grepl(sirch,ch,ignore.case=T)&
#                                    grepl(sirch2,ch,ignore.case=T)&
                                  grepl(sirch3,ch,ignore.case=T)))
)

nmchfls[names(nmchfls)==names(u7s)]

sresults<-mapply(function(x,y){
  paste0(x,"/",y)
},
nmchfls,
u7s   )
sresults%>%unlist%>%subset(grepl("pdf",ignore.case = T,.))


#end of runthrough
#crtrlf afjatynfnwss delete this line after fixing elijahs pdfs

#write to json stuff-----
rtw<-function(indvListOfFls){
df.name<-deparse(substitute(indvListOfFls))

inside<-indvListOfFls[103:105]%>%
  unname()%>%
  as.character()%>%
  gsub("C:/Users/sabloszi/Dropbox (FPC)/","",.,fixed = T)
    
df.name<-list(indvListOfFls<-list("C:/Users/sabloszi/Dropbox (FPC)/"=inside))

return(df.name)
}
rtw(dropa)

#dropar<-
  gsub("C:/Users/sabloszi/Dropbox (FPC)/","",as.character(dropa[103:104]),fixed=T)
  dropar<-gsub("C:/Users/sabloszi/Dropbox (FPC)/","",dropa,fixed = T)

#orig ctrl
    cat(toJSON(list(dropa=dropa)),file=
        "C:/delete/dropa.json")
#dropar is remove the base part
    cat(toJSON(list(dropar=dropar)),file=
          "C:/delete/dropar.json")
    #so removing the base of the path can save 1mb
#dropar is unname 
cat(toJSON(list(dropau=unname(dropa))),file=
      "C:/delete/dropau.json")
#so unnaming can save 1/2 of the total mb so this is a big deal, have to do this; also saves a lot in r's env ram or whatever. interesting that r memory.summary seem to be like 130%ish of the json version of th same lists so json is great i guess?
#dropar is both rem and unnn
cat(toJSON(list(dropau=unname(dropar))),file=
      "C:/delete/dropaur.json")

dropar<-unname(dropar)
memory.summary()
str(dropa)


#can i functify just the making a list of the base and the rest?



#this also works great, but idk how to name it up a level programatically 
#... based on whatever i want to call the thing that is made (like 
#-... say i want to name it like
#... deleat=maybe(base="C:/delete"))

maybe(base="C:/delete")

maybe<-function(base){
rest<-list.files(path=base,recursive=T)

test<-
  list(pathparts=list(
    base=base,  #this is just a string that could come from when  you make the object
    rest=rest)) #this is the object and what apply woudl do on

return(test)
}


#this is golden dont fuck but use
#onechunk dir
dropan<-
  list(pathparts=list(
    base="C:/Users/sabloszi/Dropbox (FPC)/",  #this is just a string that could come from when  you make the object
    rest=dropar[103:104])) #this is the object and what apply woudl do on
#anotheronechunk dir
dropan2<-
  list(pathparts=list(
    base="C:/Users/sabloszi/Dropbox (FPC)/",
    rest=dropar[105:106]))
#this is the thing that would go to json
ml<-list(dropan=dropan,dropan2=dropan2)

  

  
  myname="dropa",
    list(pathparts=list(
    base="C:/Users/sabloszi/Dropbox (FPC)/",
    rest=dropar[105:106]))
)%>%
  str()



#works dont fuck
list(  
  list(myname="dropa",pathparts=list(
base="C:/Users/sabloszi/Dropbox (FPC)/",
          rest=dropar[103:104])),
  list(myname="dropa",pathparts=list(
    base="C:/Users/sabloszi/Dropbox (FPC)/",
    rest=dropar[105:106]))
)%>%
  str()
  str(data())
     
     googlemfpc[1]%>%
  str()

#pick up seeing how to name just the list itself outside the function



  as="fic/t"
list(fu=list(a=as))

  #alt+ shift+ f
#wirjs dont fuck
#write to jason
cat(toJSON(list(lffs=lffs,mtcars=mtcars)),file=
      "C:/delete/LS1.json")
#get it back
LS1<-fromJSON(file="C:/delete/LS1.json"
              )

#trash
#
manna<-dir_ls(c(
  googst="Q:/Shared drives/FER - FPC Team/FPC Team Meetings",
  googmfpc="Q:/My Drive/Library",
  dropt="C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/FPC Business",
  cndrp="C:/Users/sabloszi/Documents/ArcGIS"
),all=T,type="any",recurse = T)


#everything bw parentheses----------
library(stringr)

read.table(text ="
xz
Welcome and FPC Annual Meeting Overview (Campoe, Rubilar, Cook and Carter) 
RW23: Intensity and duration of competing vegetation control in Colombia (Rubilar) 
Calcium application response in Eucalyptus grandis (Cook) 
Coffee-break 
Carbon and water balance in eucalyptus genotypes under contrasting water availability regimes - Eucarbhydro (Valverde) 
Soil carbon stocks over 40 years in Eucalyptus plantations (Cook and Campoe) 
FPC Soil Map Update (Cook) 
New RW trials & Ongoing Research Focus in LA (Campoe and Rubilar) 
Lunch 
RW7: Long Term Effects of Soil Preparation, Weed Control and Fertilization on Pine Productivity – Klabin Brazil (Campoe) 
RW7: Long Term Effects of Soil Preparation, Weed Control and Fertilization on Radiata Pine Productivity – CMPC Chile (Bozo and Rubilar) 
LA RW2: GxExS Genotype and plantation stocking in in Pinus taeda (Rubilar) 
Coffee-break 
RW20: Silviculture x Genotype x Environment study (Albaugh) 
RW23: Intensity and duration of competing vegetation control in Pinus taeda – Klabin and NGB-FIA (Campoe) 
Final discussion 
Departure from the hotel 
Visit to RW7: Long Term Effects of Soil Preparation, Weed Control and Fertilization on Pine Productivity – Klabin 
Visit to RW23: Intensity and duration of competing vegetation control in Pinus taeda – Klabin 
Lunch 
Remote Sensing Tools Training Session 
Strategic planning session and discussions 
Lunch 
Shuttle back to Curitiba-PR Airtport 
",sep="$",header=T)%>%
  pull(xz)->pary
  #head(1)%>%
  str_extract_all(., "\\([^()]+\\)")%>%
  substring(., 2, nchar(.)-1)->par

pary%>%
  gsub("\\(.*?\\)","",.)%>%
  wex()
gsub("\\)","",gsub("\\(","",gsub(par,"",pary)))
      )
  
  
  
  print()
head()
# Get the parenthesis and what is inside
k <- str_extract_all(j, "\\([^()]+\\)")[[1]]
# Remove parenthesis
k <- substring(k, 2, nchar(k)-1)

#github-----
#not much happens in the R script, just need to set the wd then go to the terminal
setwd("Q:/My Drive/Studies/FPC/Scripts")
#not sure what this load thing was for, i think its for returning to a previous workspace image or whatever
#load(".RData2")
#if you wan to look at the wd:
wael()
#the simplest terminal procedures are : (full procedures are on Protocols_NCSU_FPC.gdoc)
#1. git add FPC.R
#2. git commit -m"added all the semi-finished survey123 json stuff"
#3. git push origin main

#then that's it, it pops up in the github. 

#GET treedata  from json survey123-----
LS1<-fromJSON(file="C:/Users/sabloszi/Desktop/delete/FUCK.json"
)
str(LS1)
#get an example of a couple trees treeno and ht just to show it's in there
for(i in 1:2){
print(LS1$TreeGroDatUnpub$Plots[[1]]$TREE[[i]][c("HT","countnum")])}


#ok but can we get that in a table?
#1st lets see what one tree looks like:
LS1$TreeGroDatUnpub$Plots[[1]]$TREE[[1]]%>%
  str()
#not only is it a lit but there is stuff nested within the GPS_location

#Next, can we do this unlist thing on the highest level, the one we need from this?
#1st make a toy of the TREE level
#So this does work for 2 trees
tree1<-
  LS1$TreeGroDatUnpub$Plots[[1]]$TREE[[1]]%>%
    Unlist(depth = 0)%>%
    as.list()%>%
    data.frame()

tree2<-
  LS1$TreeGroDatUnpub$Plots[[1]]$TREE[[2]]%>%
  Unlist(depth = 0)%>%
  as.list()%>%
  data.frame()

(rbind.fill(tree1,tree2))

#how do we get it to go on n trees withoutnaming each?
#So this does work for  one plots trees
lapply(LS1$TreeGroDatUnpub$Plots[[1]]$TREE[],function(x){
  Unlist(x,depth = 0)%>%
  as.list()%>%
  data.frame()
  }
       )%>%
  rbind.fill()%>%
  wex()
#ok great this works, dont fuck

prac[[1]]<-c("red","blue")
prac[[2]]<-c("green","yellow")

LS1$TreeGroDatUnpub$Plots[[1]]

#How bout for more plots? 
lplts<-lapply(1:1,function(n){

inside<-list()

plotz<-  
  lapply(LS1$TreeGroDatUnpub$Plots[[n]]$TREE[],function(x){
  Unlist(x,depth = 0)%>%
    as.list()%>%
    data.frame()
}
)%>%
  rbind.fill()
  
inside[[n]]<-plotz  
  })

  

  
#next, if i kept doing this plot by lot it might take forever, can i get the json out of the squlite?
library(dplyr) #I think I got DBI with dplyr
library(dbplyr) #for some database connection functions
#main database from sean's phone at the end of 281303, needs elements 2 through 14
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "C:/Users/sabloszi/ArcGIS/My Surveys/Databases/a084e65cd9403d813e8ec667ca329a63.sqlite")
#1 plot database from brody's phone at the beginning of 281303, just needs one survey but not sure the id # for the "  pull(data)%>% .[11:13]" part
con <- DBI::dbConnect(RSQLite::SQLite(), dbname ="C:/Users/sabloszi/ArcGIS/My Surveys/Databases/20231207_rbhall_survey123_export.sqlite")
#2 plot (plus one tree in another plot) database from sean's phone at the beginning of 281303
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "C:/Users/sabloszi/Desktop/dontdeleteArcgis/a084e65cd9403d813e8ec667ca329a64.sqlite")
#not sure what this is for but i think it is in case you want to make a db connection from scratch and put stuff into it then save it back out?
#con <- dbConnect(RSQLite::SQLite(), ":memory:")


dbListTables(con)
dataz<-dbGetQuery(con,"select data from Surveys limit 999")%>%
  pull(data)%>%
  .[11:13]
names(dataz)<-paste0("d",c(2:14))

#ok so lets do a test to get the plot thing for one of these, say number__
LS2<-dataz[13]%>%
  fromJSON()

#what do lists look like?
prac<-list("red","blue")
#how do i know if a level has more than one item?

#can i make a list of the json'ed datas?
listy<-lapply(c(1:3), function(x){

inside=
    dataz[x]%>%
    fromJSON()

list(return(inside))
})

#yes that worked.

#you can take a peak at the data with something like this:
listy[[1]]$TreeGroDat$Plots[[1]]$TREE[[1]][c("HT","countnum")]

#can i get the tree things as a list by calling the same stuff each survey?
#yes, here's how:
#(This is the all-the-plots stuff)
trlv<-lapply(c(1:3),function(inside){
headz<-
lapply(listy[[inside]]$TreeGroDat$Plots[[1]]$TREE,function(x){ #the survey name has to be changed depending on what you called it when you set up the survey. TreeGroDat and TreeGroDatUnpub are two main ones

    Unlist(x,depth = 0)%>%
    as.list()%>%
    data.frame()#%>%
    #select(c(HT,countnum))
})%>%
  rbind.fill()
  
list(return(headz))  

})

#put all trlv together and wex
trlvbs<-rbindlist(trlv,idcol = "huh",fill=T)
#make brodys plot obvious if you're doing that one's con
trlvbs$huh<-82
#heres all the phone data:
phdat<- list(trlvb,trlvbb)%>%rbindlist(.,fill=T)%>% #the different trlvbb, trlvbs, trlvb etc are from different sqlites in the beginning of this section and now im just birnding them togetoher
  mutate(huh=huh+1) #get it back to the integer of the survey in the latest 281303 sean phone sqlite

#ok can i get the plots level stuff?
#here's the model for one when you want to peak at whats inside:
listy[[9]]$TreeGroDat$Plots[[1]]$Comments
#now get each element you want from all the surveys:
phts<-  lapply(1:3,function(plot){listy[[plot]]$TreeGroDat$Plots[[1]]$photo_plot}) #lapply(8:13,.. for the main most recent seans phone sqlite for all these; lapply(1:3,... for the earlier sean phone sqlite
plts<-  lapply(1:3,function(plot){listy[[plot]]$TreeGroDat$Plots[[1]]$PLOT  })
gbid<-  lapply(1:3,function(plot){listy[[plot]]$TreeGroDat$Plots[[1]]$globalid  })
pgbd<-  lapply(1:3,function(plot){listy[[plot]]$TreeGroDat$Plots[[1]]$parentglobalid  })
cmmt<-  lapply(1:3,function(plot){listy[[plot]]$TreeGroDat$Plots[[1]]$Comments})
#wont dataframe if theyre not the same dims and unlist gets rid of nulls
phts[sapply(phts, is.null)] <- NA
cmmt[sapply(cmmt, is.null)] <- NA

#if there's not already a csv from getting the plot level data out earlier:
data.frame(Comments=unlist(cmmt),
           photo_plot=unlist(phts),
           PLOT=unlist(plts))%>%
  unique()%>%
  #overwrites the csv, careful here
  write.csv(
    "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_PLOT-survey123phone.csv")

#but then once you have plot level data on a csv, you need to bind newer stuuff to it:
#so first read in the old plot level stuff...
pltlvOld<-read.csv(
  "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_PLOT-survey123phone.csv",header = T)%>%
  select(-X)

#then bind them together:
data.frame(Comments=unlist(cmmt),
           photo_plot=unlist(phts),
           PLOT=unlist(plts))%>%
  unique()%>%
list(pltlvOld,.)%>%rbindlist(.,fill=T)%>%  #binding
    #overwrites the csv, careful here
  write.csv(
    "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_PLOT-survey123phone.csv")


phdat%>%
  mutate(PLOT=substr((gsub("281303","",PLOTTREE)),1,4))%>%
  write.csv(
    "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_TREE-survey123phone.csv")

#Ok and if you get more after the fact and want to add it to the csv:
phdat2<-read.csv(
  "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_TREE-survey123phone.csv",header = T)
#ok are the ones you want to put together at least similar in size?
dim(phdat2) #835 by 39
dim(trlvbs) #200 by 30, hmm not sure about thes 9 missing columns hope its nothing major

trlvbs%>%
mutate(PLOT=substr((gsub("281303","",PLOTTREE)),1,4))%>%
  list(phdat2,.)%>%rbindlist(.,fill=T)%>% 
  #now overwrite the csv;  be careful with this step
  write.csv(
    "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/RW28/281303/Data/201303_20231207_htlc-ht-dbh-dam-mort_TREE-survey123phone.csv")


#put pieces back together again with the setres yst3------
setwd(  "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials")

#plot 1000 got to the cloud successfully but its htlc was integer so it didnt merge; you need to put that htlc back in a step further below
cloud<-  read_excel(paste0(getwd(),"/GIS/GIS for FPC WORKING 2021/RW28s/281303/gisdata/treeMeasure/TREETreeGroDatPub_TableToExcel_plot1000.xlsx"),
                  sheet="1",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                  #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                  trim_ws = T )%>%
  select(HTLC,PLOTTREE)%>%
  mutate(.,HTLC=(substr(HTLC, 2, nchar(HTLC))))%>%
  mutate(HTLC=as.numeric(paste0("0.",HTLC)))
  

#heres the data that was on phones plus most of the stuff in the cloud togeter
phone<-read_excel(paste0(getwd(),"/GIS/GIS for FPC WORKING 2021/RW28s/281303/gisdata/treeMeasure/TREEsurvey123phone_TableToExcel.xlsx"),
                sheet="TREEsurvey123phone",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                trim_ws = T )
#phone$
phone%>%
mutate(PLOT2=substr((gsub("281303","",PLOTTREE)),1,4))%>%
#  subset(GPS_location_y<35)%>%
#  subset(GPS_location_y>34)%>%
#  select(!contains("GPS_location_"))%>%
  subset(!is.na(PLOT2))%>%
  mutate(PLOT=replace_na(PLOT,0))%>%
subset(!is.na(MORT))%>%
  mutate(.,TREE_No=as.numeric(TREE_No))%>%
    mutate(.,TREE_No2=as.numeric(
  str_replace(PLOTTREE,PLOT2,"")
      ))%>%
  mutate(.,TREE_No2=as.numeric(
    str_replace(TREE_No2,"281303","")
    ))%>%
  mutate(.,TREE_No3=coalesce(TREE_No2,TREE_No))%>%
  mutate(.,TREE_No3=coalesce(TREE_No3,as.numeric(countnumsave)))%>%
#  subset(TREE_No!=TREE_No2)%>% note there are a few in plot 1000 that have differnt tree numbers here at 64 65 and 66
  subset(!(PLOT2%in%c(3150)&is.na(huh)))%>%
#  mutate(PLOT=replace_na(TREE_No3,0))%>%
  mutate(.,TREE_No3=if_else(huh==8,TREE_No3+64,TREE_No3,missing=TREE_No3))%>%
  mutate(.,vm2=cumsum(!duplicated(interaction(PLOT2,TREE_No3+1000))))%>%  
  #select(huh,TREE_No3,TREE_No2,TREE_No,PLOT2,PLOT,PLOTTREE,MORT,HT,CommentsTree,vm2)%>%
  subset(.,!duplicated(vm2))%>%
#  subset(PLOT2%in%c(3150,4151,4150))%>%
select(  OID,	huh,	GPS_location_horizontalAccuracy,	GPS_location_satellitesInUse_1,	GPS_location_verticalAccuracy,	GPS_location_x,	GPS_location_y,	GPS_location_z,	MORT,	PLOTTREE,	accuracy,	countnumsave,	PLOT2,	TREE_No3,	globalid,	parentglobalid,	DAM,	DBH,	HT,	HTLC1,	CommentsTree,	RE_L)%>%
  merge(.,cloud,by="PLOTTREE",all=T)%>%
  arrange(PLOT2,TREE_No3)%>%
  mutate(HTLC=coalesce(HTLC1,as.character(HTLC)))%>%
  select(-HTLC1)%>%
  write.csv(
    "C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/GIS/GIS for FPC WORKING 2021/RW28s/281303/gisdata/treeMeasure/TREE_survey123all.csv"
  )
  #pick up doing some qc then merge with the paper
    
  sub
  
  dim()



#breaking arcgis fieldmaps
  
data.frame(TREE_No=c(1,2,3),
           HT=c(2.6,1.7,1.2),
           DBH=c(55.2,49.1,71.3),
           MORT=c("a","b","c"),
           DAM=c("E","E","F"),
           Comments=c("RE",NA,NA))%>%
  mutate(pladoh=paste0(HT,DBH,MORT,DAM,Comments,sep=" - "))


paste0("a","n",sep="",collapse="   asasdasd")
#get the columns separated in one cell somehow


  print()
  
  

