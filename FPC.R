#pick up----
#just got va and v, mkae sure they make sense then go ahead and make figures for one or both and do anova
#setwd
#setwd("C:/Users/seanb/")
#setwd("C:/Users/sabloszi")

source("Q:/My Drive/Library/datlibmgr.R")


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
#rwdb gold------
#20221017.mdb, seanb


conn<-odbcConnect("rwdb2022")   #new error 3/16/22- went to this b/c the driver path was "unknown" somehow
#

#not sure what this one is for
#conn<-odbcDriverConnect(  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")#20221017
#20210423.mdb, seanb
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb
#20221017.mdb, seanb
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/seanb/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")
#20221017.mdb, sabloszi
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20221017.mdb")
#20210423.mdb, sabloszi
#conn<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Static RWDB/RWDB static 20210423.mdb")

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
sqlFetch(conn,"dbo_ACTIVITY_WORKPLAN")%>%wex()
depths<-sqlFetch(conn,"dbo_DEPTH_CODES")
sqlFetch(conn,"dbo_COMPANY_OPERATIONS")%>%
  subset(OPERATION_ID!=140)%>%
  subset(.,grepl("cock",OPERATION_NAME,ignore.case=T))
grep
wex(sqlFetch(conn,"dbo_DOMINANT_HEIGHT_1"))
wex(sqlFetch(conn,"dbo_APPLIED_TREATMENTS"))
(sqlFetch(conn,"dbo_SAMPLING_LOCATION"))

names(splloc)
#d
sqlFetch(conn,"dbo_MACRO_TISSUE")%>%pull("SAMPLE_#")%>%histogram()
#md



framnams1<-sapply(tblnams[1:31],function(x){names(sqlFetch(conn,x))})
framnams2<-sapply(tblnams[32:61],function(x){names(sqlFetch(conn,x))})


findnames<-function(x){grepl("(?i)DENS", x)}
lapply(franmams,FUN = function(x){sum(findnames(x))})%>%str()
_STUDY_INFO
_MICRO_TISSUE
_MACRO_TISSUE
_COMPANY_OPERATIONS 

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


#numbers 1-10 letters a-l:-----
outer(c(letters[1:12]),c(1:10),FUN=function(x,y){paste0(x,y)})%>%
  t()%>%
  melt()%>%
  wex()
head()
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

#merge-----

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



# move to somewhere else AND THEN delete (link from pickupcalks vertex section somwhoe)---- 

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

moddat<-read.csv("G:/My Drive/Studies/FPC/SharedFolderSean/Bloszies (1)/NCSU/CNR/FER/FPC/Lab/Lab Documents/SOPs_Methods/vertex_iv_me_calib-data.csv",header=T)%>%
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




#merge arc attr tables with 2022 cruising data varrateNC-----

setwd("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data")

v2023plots<-read_excel(paste0(getwd(),"/cruising-data-VarRate-20230405-stand127only-plots.xls"),
                       sheet="0",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                       #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                       trim_ws = T )
v2023trees<-read_excel(paste0(getwd(),"/cruising-data-VarRate-20230405-stand127only.xls"),
                       sheet="1",#skip=1,col_names =c("baeid_1","tocmgl_1","baeid_2","tocmgl_2"), 
                       #     col_types = c("numeric","skip","numeric","skip","skip","numeric","skip","numeric"),
                       trim_ws = T )
v2022trees<-read.csv(file="C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data/cruising-data-VarRate-20220412-25.csv",header=T)%>%
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

#what does the relationship between node2023 and tip2022 measurements look like
v2023trees%>%
  subset(.,Height_node_2023>30)%>%#one must've been a typo b/c it says 27.6 ft tall and there weren't actually any trees under 30 in these three plots as Sean remembers it and htlc is above 30 for both years.
  #wex()
  ggplot(.,)+
  geom_point(aes(x=Height_tip_2022,y=Height_node_2023,color=PLOT))+
  geom_text(aes(x=20,y=60),data=data.frame(x=NULL,y=NULL),label="Green is 1:1; Blue is tree model; Dash is bldg model")+
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2],color="Blue")+
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],linetype="dashed")+#jordan hall model where the transponder height was known as 4.5ft
  geom_abline(intercept = 0, slope = 1,color="green")+
  ylim(0,80)+xlim(0,80) #yes that positive intercept really does mean at 0,0 the 2022 tip trees are shorter. But the slope <1 makes up for that


fit2=  v2023trees%>%
  subset(.,Height_node_2023>30)%>%#one must've been a typo b/c it says 27.6 ft tall and there weren't actually any trees under 30 in these three plots as Sean remembers it and htlc is above 30 for both years.
  lm(data=.,Height_node_2023~Height_tip_2022)#model far as a function of close since we want to find out the estimated farther (ie acurate 32.8ft calibration height i/o less than 32.8ft calib height) based on last years close data
summary(fit2)

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

#delete eventually- this is the all meetings table-----
text='
<table id="tablepress-2" class="tablepress tablepress-id-2 dataTable no-footer">
<thead>
<tr class="row-1 odd"><th class="column-1 sorting_disabled" rowspan="1" colspan="1" style="width: 40.4px;"><strong>Year</strong></th><th class="column-2 sorting_disabled" rowspan="1" colspan="1" style="width: 69.8px;"><strong>Date</strong></th><th class="column-3 sorting_disabled" rowspan="1" colspan="1" style="width: 283.95px;"><strong>FPC Meeting</strong></th><th class="column-4 sorting_disabled" rowspan="1" colspan="1" style="width: 145.85px;"><strong>Location</strong></th></tr>
</thead>
<tbody class="row-hover">





























































<tr class="row-2 even">
	<td class="column-1">2022</td><td class="column-2">December 14</td><td class="column-3">Forest Management and Carbon Markets: Recent Developments and Key Knowledge Gaps - Justin Baker (NCSU) and Greg Latta (U of Idaho)<br>
<br>
<a href="https://www.dropbox.com/s/lk09eg17he53lnx/video1352293942.mp4?dl=0" target="_blank" rel="noopener noreferrer">Video Recording - presentations begin about minute 10</a><br>
<br>
Powerpoint slides will be available soon</td><td class="column-4">Webinar, from Virginia Tech, Blacksburg, VA</td>
</tr><tr class="row-3 odd">
	<td class="column-1">2022</td><td class="column-2">December 1, 6, 13</td><td class="column-3"><a href="https://forestproductivitycoop.net/2022-latin-american-annual-meeting/" target="_blank" rel="noopener noreferrer">2022 Latin American Annual Meeting</a></td><td class="column-4">Online</td>
</tr><tr class="row-4 even">
	<td class="column-1">2022</td><td class="column-2">November 15-17</td><td class="column-3"><a href="https://forestproductivitycoop.net/2022-contact-meeting/" target="_blank" rel="noopener noreferrer">2022 US Contact Meeting</a></td><td class="column-4">Leesville, LA</td>
</tr><tr class="row-5 odd">
	<td class="column-1">2022</td><td class="column-2">August 9-10</td><td class="column-3"><a href="https://forestproductivitycoop.net/2022-annual-meeting/" target="_blank" rel="noopener noreferrer">2022 US Annual Meeting</a></td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-6 even">
	<td class="column-1">2022</td><td class="column-2">April 29</td><td class="column-3">Optimizing pine plantation management via geospatial data science and forest soil classification - Chris Cohrs PhD. defense<br>
<a href="https://www.dropbox.com/sh/mit2y893r20k84f/AABSHTlkggybR7HcU9rR0ScVa?dl=0" target="_blank" rel="noopener noreferrer">Video Recording</a><br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2022/05/CWCohrs_Defense_29Apr2022.pdf" target="_blank" rel="noopener noreferrer">Powerpoint slides</a></td><td class="column-4">Graduate student defense/ webinar, from NCSU, Raleigh, NC</td>
</tr><tr class="row-7 odd">
	<td class="column-1">2021</td><td class="column-2">Year end summary</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2021/12/CAFS-Annual-Report-Year-2_opt.pdf" target="_blank" rel="noopener noreferrer">2021 Center for Advanced Forest Systems Progress Report</a></td><td class="column-4"></td>
</tr><tr class="row-8 even">
	<td class="column-1">2021</td><td class="column-2">October19-20</td><td class="column-3"><a href="https://forestproductivitycoop.net/2021-contact-meeting/" target="_blank" rel="noopener noreferrer">2021 US Contact Meeting</a></td><td class="column-4">Lynchburg, VA</td>
</tr><tr class="row-9 odd">
	<td class="column-1">2021</td><td class="column-2">August 10, 12, 17, and 19</td><td class="column-3"><a href="https://forestproductivitycoop.net/2021-annual-meeting/" target="_blank" rel="noopener noreferrer">2021 US Annual Meeting</a></td><td class="column-4">Lunch and Learn Online</td>
</tr><tr class="row-10 even">
	<td class="column-1">2021</td><td class="column-2">May 4-25</td><td class="column-3"><a href="https://forestproductivitycoop.net/2021-fpc-latin-american-contact-meeting/" target="_blank" rel="noopener noreferrer">2021 Latin American Contact Meeting</a></td><td class="column-4">Online</td>
</tr><tr class="row-11 odd">
	<td class="column-1">2020</td><td class="column-2">December 7-15</td><td class="column-3"><a href="https://forestproductivitycoop.net/2020-strategic-planning/" target="_blank" rel="noopener noreferrer">2020 Strategic Planning Meeting</a></td><td class="column-4">Strategic Planning</td>
</tr><tr class="row-12 even">
	<td class="column-1">2020</td><td class="column-2">August 4-September 1</td><td class="column-3"><a href="https://forestproductivitycoop.net/2020-us-annual-meeting/" target="_blank" rel="noopener noreferrer">2020 US Annual Meeting</a></td><td class="column-4">Lunch and Learn Online</td>
</tr><tr class="row-13 odd">
	<td class="column-1">2020</td><td class="column-2">May 28</td><td class="column-3">Sentinel 2 Leaf Area Estimation in the Southeastern US - Chris Cohrs<br>
<a href="https://youtu.be/0io2Wt5k1C4" target="_blank" rel="noopener noreferrer">Youtube Video Recording</a><br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2020/06/Cohrs-S2LAI-FPC-Webinar-28-May-2020.pdf" target="_blank" rel="noopener noreferrer">Powerpoint slides</a></td><td class="column-4">Webinar, from NCSU, Raleigh, NC</td>
</tr><tr class="row-14 even">
	<td class="column-1">2019</td><td class="column-2">November 20-21</td><td class="column-3"><a href="https://forestproductivitycoop.net/2019-us-pine-contact-meeting/" target="_blank" rel="noopener noreferrer">2019 US Pine Contact Meeting</a></td><td class="column-4">Mt. Gilead, NC</td>
</tr><tr class="row-15 odd">
	<td class="column-1">2019</td><td class="column-2">November 11</td><td class="column-3">An introduction to the new Landsat 7 ETM+ and Landsat 8 Leaf Area<br>
Index estimation tool: R package walkthrough - Matthew Sumanll<br>
<a href="https://www.dropbox.com/s/xw2krh3jf3t6ykx/Webinar%202_Sumnall%20Landsat%207%20and%208%20Tool.mp4?dl=0" target="_blank" rel="noopener noreferrer">Video recording</a><br>
<a href="https://www.dropbox.com/sh/8dypcwx7o6ghh2r/AAAgooJRjU0ScR8f5A7AyeEQa?dl=0" target="_blank" rel="noopener noreferrer">Powerpoint slides and R tools</a></td><td class="column-4">Webinar, from Virginia Tech, Blacksburg, VA</td>
</tr><tr class="row-16 even">
	<td class="column-1">2019</td><td class="column-2">October 21-24</td><td class="column-3"><a href="https://forestproductivitycoop.net/?page_id=13306" target="_blank" rel="noopener noreferrer">2019 Contact Meeting in Argentina</a></td><td class="column-4">Posadas, Argentina</td>
</tr><tr class="row-17 odd">
	<td class="column-1">2019</td><td class="column-2">August 6-8</td><td class="column-3"><a href="https://forestproductivitycoop.net/2019-us-annual-meeting/" target="_blank" rel="noopener noreferrer">2019 US Annual Meeting</a></td><td class="column-4">Savannah, GA</td>
</tr><tr class="row-18 even">
	<td class="column-1">2019</td><td class="column-2">May 31</td><td class="column-3"><a href="https://forestproductivitycoop.net/2019-may-webinar/" target="_blank" rel="noopener noreferrer">Recent Developments in Remote Sensing</a></td><td class="column-4">Webinar, from Virginia Tech, Blacksburg, VA</td>
</tr><tr class="row-19 odd">
	<td class="column-1">2018</td><td class="column-2">November 6-7</td><td class="column-3"><a href="https://forestproductivitycoop.net/2018-us-pine-contact-meeting/" target="_blank" rel="noopener noreferrer">2018 US Pine Contact Meeting</a></td><td class="column-4">Yulee, FL</td>
</tr><tr class="row-20 even">
	<td class="column-1">2018</td><td class="column-2">August 6-9</td><td class="column-3"><a href="http://forestproductivitycoop.net/2018-us-annual-meeting/" target="_blank" rel="noopener noreferrer">2018 US Annual Meeting</a></td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-21 odd">
	<td class="column-1">2017</td><td class="column-2">October 17-19</td><td class="column-3"><a href="http://forestproductivitycoop.net/2017-la-annual-meeting/">2017 LA Annual Meeting</a></td><td class="column-4">Concepción, Chile</td>
</tr><tr class="row-22 even">
	<td class="column-1">2017</td><td class="column-2">August 7-10</td><td class="column-3"><a href="http://forestproductivitycoop.net/2017-us-annual-meeting/" target="_blank" rel="noopener noreferrer">2017 US Annual Meeting</a></td><td class="column-4">Wilmington, NC</td>
</tr><tr class="row-23 odd">
	<td class="column-1">2017</td><td class="column-2">July 26</td><td class="column-3"><a href="http://forestproductivitycoop.net/2017-contact-meeting-chile-2/" target="_blank" rel="noopener noreferrer">2017 Contact Meeting in Chile 2</a></td><td class="column-4">Chile</td>
</tr><tr class="row-24 even">
	<td class="column-1">2017</td><td class="column-2">July 5</td><td class="column-3"><a href="http://forestproductivitycoop.net/2017-contact-meeting---chile-1/" target="_blank" rel="noopener noreferrer">2017 Contact Meeting in Chile 1</a></td><td class="column-4">Chile</td>
</tr><tr class="row-25 odd">
	<td class="column-1">2016</td><td class="column-2">October 4-6</td><td class="column-3"><a href="http://forestproductivitycoop.net/2016-annual-meeting/" target="_blank" rel="noopener noreferrer">2016 Annual Meeting</a></td><td class="column-4">Blacksburg, VA</td>
</tr><tr class="row-26 even">
	<td class="column-1">2016</td><td class="column-2">July 12-14</td><td class="column-3"><a href="http://forestproductivitycoop.net/2016-july-12-14-la-pine-and-eucalyptus-wg-meeting/" target="_blank" rel="noopener noreferrer">2016 LA Pine and Eucalyptus WG Meeting</a></td><td class="column-4">Cali, Colombia</td>
</tr><tr class="row-27 odd">
	<td class="column-1">2016</td><td class="column-2">May 24-26</td><td class="column-3"><a href="http://forestproductivitycoop.net/2016-may-24-26-us-pine-wg-meeting/" target="_blank" rel="noopener noreferrer">2016 US Pine WG Meeting</a></td><td class="column-4">Brookeland, TX</td>
</tr><tr class="row-28 even">
	<td class="column-1">2016</td><td class="column-2">March 31-April 1</td><td class="column-3"><a href="http://forestproductivitycoop.net/2016-march-31-april-1-us-eucalyptus-wg-meeting/" target="_blank" rel="noopener noreferrer">2016 US Eucalyptus WG Meeting</a></td><td class="column-4">Quincy, FL</td>
</tr><tr class="row-29 odd">
	<td class="column-1">2015</td><td class="column-2">November 20</td><td class="column-3"><a href="http://forestproductivitycoop.net/2015-november-webex-meeting/" target="_blank" rel="noopener noreferrer">2015 November WEBEX Meeting</a></td><td class="column-4">Online</td>
</tr><tr class="row-30 even">
	<td class="column-1">2015</td><td class="column-2">September 29-30</td><td class="column-3"><a href="http://forestproductivitycoop.net/2015-annual-meeting/" target="_blank" rel="noopener noreferrer">2015 Annual Meeting</a></td><td class="column-4">Chapel Hill, NC</td>
</tr><tr class="row-31 odd">
	<td class="column-1">2015</td><td class="column-2">June 22-27</td><td class="column-3"><a href="http://forestproductivitycoop.net/la-pine-and-eucalyptus-working-group-meeting-june-22-27-2015/" target="_blank" rel="noopener noreferrer">LA Pine and Eucalyptus WG Meeting</a></td><td class="column-4">Argentina and Brazil</td>
</tr><tr class="row-32 even">
	<td class="column-1">2015</td><td class="column-2">May 12-14</td><td class="column-3"><a href="http://forestproductivitycoop.net/us-pine-working-group-meeting-may-12-14-2015/" target="_blank" rel="noopener noreferrer">US Pine WG Meeting</a></td><td class="column-4">Florence, SC</td>
</tr><tr class="row-33 odd">
	<td class="column-1">2015</td><td class="column-2">March 9 - 10</td><td class="column-3"><a href="http://forestproductivitycoop.net/us-eucalyptus-wg-meeting-march-9-10-2015/" target="_blank" rel="noopener noreferrer">US Eucalyptus WG Meeting</a></td><td class="column-4">Ravenel, SC</td>
</tr><tr class="row-34 even">
	<td class="column-1">2014</td><td class="column-2">October 14 - 16</td><td class="column-3"><a href="http://forestproductivitycoop.net/annual-meeting-2014/">Annual Meeting</a></td><td class="column-4">Atlanta, GA</td>
</tr><tr class="row-35 odd">
	<td class="column-1">2014</td><td class="column-2">August 3 - 5</td><td class="column-3"><a href="http://forestproductivitycoop.net/la-eucalyptus-wg-meeting-august-2014/" target="_blank" rel="noopener noreferrer">Latin America Eucalyptus Working Group - Research Review and Strategic Planning Meeting</a></td><td class="column-4">Tres Lagoas, Mato Grosso do Sul, Brazil</td>
</tr><tr class="row-36 even">
	<td class="column-1">2014</td><td class="column-2">June 3 - 5</td><td class="column-3"><a href="http://forestproductivitycoop.net/us-pine-meetings/" target="_blank" rel="noopener noreferrer">US Pine Working Group Research Review &amp; Contact Meeting</a></td><td class="column-4">Tuscaloosa, AL</td>
</tr><tr class="row-37 odd">
	<td class="column-1">2014</td><td class="column-2">February 10 - 11</td><td class="column-3"><a href="http://forestproductivitycoop.net/us-eucalyptus-working-group-meeting-february-2014/" target="_blank" rel="noopener noreferrer">US Eucalyptus WG</a></td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-38 even">
	<td class="column-1">2014</td><td class="column-2">January 13 - 15</td><td class="column-3">US Pine WG</td><td class="column-4">New Bern, NC</td>
</tr><tr class="row-39 odd">
	<td class="column-1">2013</td><td class="column-2">October 7 - 9</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2013-october-7-9-annual-meeting-atlanta-ga/">Annual Meeting</a></td><td class="column-4">Atlanta, GA</td>
</tr><tr class="row-40 even">
	<td class="column-1">2013</td><td class="column-2">June 3 - 7</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2013-contact-meeting-in-north-america/" target="_blank" rel="noopener noreferrer">Contact Meeting</a></td><td class="column-4">Blacksburg, VA</td>
</tr><tr class="row-41 odd">
	<td class="column-1">2012</td><td class="column-2">November 15</td><td class="column-3">Eucalyptus Special Project</td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-42 even">
	<td class="column-1">2012</td><td class="column-2">October 15 - 18</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2012-annual-advisory-council-meeting/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Chapel Hill, NC</td>
</tr><tr class="row-43 odd">
	<td class="column-1">2012</td><td class="column-2">June 12 - 14</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/june-12-14-contact-meeting-pineville-la/" target="_blank" rel="noopener noreferrer">2012 Contact Meeting</a></td><td class="column-4">Pineville, LA</td>
</tr><tr class="row-44 even">
	<td class="column-1">2012</td><td class="column-2">May 2012</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2012-latin-america-contact-meeting-agenda/" target="_blank" rel="noopener noreferrer">Latin America Contact Meeting</a></td><td class="column-4">Concepcion, Chile</td>
</tr><tr class="row-45 odd">
	<td class="column-1">2011</td><td class="column-2">October 11 - 13</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2011-annual-advisory-council-meeting/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Blacksburg, VA</td>
</tr><tr class="row-46 even">
	<td class="column-1">2011</td><td class="column-2">June 8 - 9</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2011contactmeeting-pdf/?tmstv=1682057812" rel="nofollow">
	2011ContactMeeting.pdf</a></td><td class="column-4">Waycross, GA</td>
</tr><tr class="row-47 odd">
	<td class="column-1">2011</td><td class="column-2">June 6 - 7</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2011-workshop-all-pdf/?tmstv=1682057812" rel="nofollow">
	2011-Workshop-all.pdf</a></td><td class="column-4">Jacksonville, FL</td>
</tr><tr class="row-48 even">
	<td class="column-1">2011</td><td class="column-2">March 9</td><td class="column-3">RW24 Eucalyptus Cold Tolerance Project - 3rd Mtg</td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-49 odd">
	<td class="column-1">2011</td><td class="column-2">March 8</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2011-planning-meeting/" target="_blank" rel="noopener noreferrer">Planning Meeting to Discuss RW 18 and RW 19 Studies</a></td><td class="column-4">Raleigh, NC</td>
</tr><tr class="row-50 even">
	<td class="column-1">2010</td><td class="column-2">November 14 - 19</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2010_cm_a_u_documentos_campo_eng-pdf/?tmstv=1682057812" rel="nofollow">
	2010_CM_A_U_documentos_campo_eng.pdf</a></td><td class="column-4">Argentina, Uruguay</td>
</tr><tr class="row-51 odd">
	<td class="column-1">2010</td><td class="column-2">October 26 - 28</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2010-advisory-council-meeting/" target="_blank" rel="noopener noreferrer">Advisory Council Meeting</a></td><td class="column-4">New Bern, NC</td>
</tr><tr class="row-52 even">
	<td class="column-1">2009</td><td class="column-2">October 6 - 8</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2009-advisory-council-meeting/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Chapel Hill, NC</td>
</tr><tr class="row-53 odd">
	<td class="column-1">2009</td><td class="column-2">June 1 - 2</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2009contactmeeting-pdf/?tmstv=1682057812" rel="nofollow">
	2009ContactMeeting.pdf</a></td><td class="column-4"></td>
</tr><tr class="row-54 even">
	<td class="column-1">2008</td><td class="column-2">September 29 - October 1</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2008-advisory-council-meeting/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Roanoke, VA</td>
</tr><tr class="row-55 odd">
	<td class="column-1">2008</td><td class="column-2">June 2 - 4</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2008-silviculture-relationships-workshop/" target="_blank" rel="noopener noreferrer">Forest Production - Silviculture Relationships Workshop</a></td><td class="column-4"></td>
</tr><tr class="row-56 even">
	<td class="column-1">2008</td><td class="column-2">June 5 - 6</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2008contactmeeting-pdf/?tmstv=1682057812" rel="nofollow">
	2008ContactMeeting.pdf</a></td><td class="column-4"></td>
</tr><tr class="row-57 odd">
	<td class="column-1">2007</td><td class="column-2">October 15 - 17</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2007-advisory-council/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Blacksburg, VA</td>
</tr><tr class="row-58 even">
	<td class="column-1">2007</td><td class="column-2">June 6</td><td class="column-3"><a class="download-link" title="" href="https://forestproductivitycoop.net/fpcdata/2007contactmeeting-pdf/?tmstv=1682057812" rel="nofollow">
	2007ContactMeeting.pdf</a></td><td class="column-4">Tuscaloosa, AL</td>
</tr><tr class="row-59 odd">
	<td class="column-1">2006</td><td class="column-2">October 3 - 4</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2006-advisory-council/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Chapel Hill, NC</td>
</tr><tr class="row-60 even">
	<td class="column-1">2005</td><td class="column-2">October 11 - 13</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2005-advisory-council/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4">Blacksburg VA</td>
</tr><tr class="row-61 odd">
	<td class="column-1">2005</td><td class="column-2">June 6 - 8</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2005-contact-meeting/" target="_blank" rel="noopener noreferrer">Contact Meeting Workshop Presentations</a></td><td class="column-4">Jacksonville, FL</td>
</tr><tr class="row-62 even">
	<td class="column-1">2004</td><td class="column-2">September 21 - 22</td><td class="column-3"><a href="http://forestproductivitycoop.net/publications/fpc-presentations/2004-advisory-council/" target="_blank" rel="noopener noreferrer">Annual Advisory Council Meeting</a></td><td class="column-4"></td>
</tr></tbody>
</table>
'
#delete eventually- this is the 2020 minimal table-----
text= #this is the 2020 annual meeting as of 4/20/23
  '
<table id="tablepress-168" class="tablepress tablepress-id-168 dataTable no-footer">
<caption style="caption-side: bottom; text-align: left; border: medium none; background: none; margin: 0px; padding: 0px; --darkreader-inline-border-top: currentcolor; --darkreader-inline-border-right: currentcolor; --darkreader-inline-border-bottom: currentcolor; --darkreader-inline-border-left: currentcolor; --darkreader-inline-bgcolor: rgba(0, 0, 0, 0); --darkreader-inline-bgimage: none;" data-darkreader-inline-border-top="" data-darkreader-inline-border-right="" data-darkreader-inline-border-bottom="" data-darkreader-inline-border-left="" data-darkreader-inline-bgcolor="" data-darkreader-inline-bgimage=""><a href="https://forestproductivitycoop.net/wp-admin/admin.php?page=tablepress&amp;action=edit&amp;table_id=168" rel="nofollow">Edit</a></caption>
<thead>
<tr class="row-1 odd"><th class="column-1 sorting_disabled" rowspan="1" colspan="1" style="width: 67.45px;">Date</th><th class="column-2 sorting_disabled" rowspan="1" colspan="1" style="width: 54.4333px;">Time</th><th class="column-3 sorting_disabled" rowspan="1" colspan="1" style="width: 304.583px;">Presentation</th><th class="column-4 sorting_disabled" rowspan="1" colspan="1" style="width: 113.533px;">Speaker</th></tr>
</thead>
<tbody class="row-hover">
<tr class="row-2 even">
	<td class="column-1">Tuesday, August 4 -<br>
Tuesday, September 1</td><td class="column-2"></td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/06/FPC-Annual-Meeting-Online-Series-Agenda-2020_with-links.pdf" target="_blank" rel="noopener noreferrer">2020 FPC US Annual Meeting Agenda</a></td><td class="column-4"></td>
</tr><tr class="row-3 odd">
	<td class="column-1">Tuesday Aug 4</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_State-of-the-FPC-AM-2020.pdf" target="_blank" rel="noopener noreferrer">State of the FPC, overview of meeting structure, preview of upcoming presentations (pdf)</a><br>
<br>
<a href="https://youtu.be/XA60np_O_Bo" target="_blank" rel="noopener noreferrer">Presentation recording</a><br>
</td><td class="column-4">Rachel Cook</td>
</tr><tr class="row-4 even">
	<td class="column-1">Tuesday Aug 4</td><td class="column-2">1:00-1:30</td><td class="column-3">Business Meeting - Advisory Council members only</td><td class="column-4"></td>
</tr><tr class="row-5 odd">
	<td class="column-1">Thursday Aug 6</td><td class="column-2">12:00-12:30</td><td class="column-3">Background on FPC Soil Management System - Watch or review this before Thursday at 12:00 if you are unfamiliar with the FPC soil system<br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_FPC-Soils-Background_Recorded-only.pdf" target="_blank" rel="noopener noreferrer">PDF</a><br>
<a href="https://youtu.be/vOn8J0rGMBM" target="_blank" rel="noopener noreferrer">Video</a><br>
<br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_FPC-Soils-Updates-2020.pdf" target="_blank" rel="noopener noreferrer">FPC Soil management and update (pdf)</a><br>
<a href="https://youtu.be/gzr78GkHFIs" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Rachel Cook</td>
</tr></tbody>
</table>
'  
#turn a html table into a dataframe- ------
text= #this is the 2020 annual meeting as of 4/20/23
  '
<table id="tablepress-168" class="tablepress tablepress-id-168 dataTable no-footer">
<caption style="caption-side: bottom; text-align: left; border: medium none; background: none; margin: 0px; padding: 0px; --darkreader-inline-border-top: currentcolor; --darkreader-inline-border-right: currentcolor; --darkreader-inline-border-bottom: currentcolor; --darkreader-inline-border-left: currentcolor; --darkreader-inline-bgcolor: rgba(0, 0, 0, 0); --darkreader-inline-bgimage: none;" data-darkreader-inline-border-top="" data-darkreader-inline-border-right="" data-darkreader-inline-border-bottom="" data-darkreader-inline-border-left="" data-darkreader-inline-bgcolor="" data-darkreader-inline-bgimage=""><a href="https://forestproductivitycoop.net/wp-admin/admin.php?page=tablepress&amp;action=edit&amp;table_id=168" rel="nofollow">Edit</a></caption>
<thead>
<tr class="row-1 odd"><th class="column-1 sorting_disabled" rowspan="1" colspan="1" style="width: 67.45px;">Date</th><th class="column-2 sorting_disabled" rowspan="1" colspan="1" style="width: 54.4333px;">Time</th><th class="column-3 sorting_disabled" rowspan="1" colspan="1" style="width: 304.583px;">Presentation</th><th class="column-4 sorting_disabled" rowspan="1" colspan="1" style="width: 113.533px;">Speaker</th></tr>
</thead>
<tbody class="row-hover">





<tr class="row-2 even">
	<td class="column-1">Tuesday, August 4 -<br>
Tuesday, September 1</td><td class="column-2"></td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/06/FPC-Annual-Meeting-Online-Series-Agenda-2020_with-links.pdf" target="_blank" rel="noopener noreferrer">2020 FPC US Annual Meeting Agenda</a></td><td class="column-4"></td>
</tr><tr class="row-3 odd">
	<td class="column-1">Tuesday Aug 4</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_State-of-the-FPC-AM-2020.pdf" target="_blank" rel="noopener noreferrer">State of the FPC, overview of meeting structure, preview of upcoming presentations (pdf)</a><br>
<br>
<a href="https://youtu.be/XA60np_O_Bo" target="_blank" rel="noopener noreferrer">Presentation recording</a><br>
</td><td class="column-4">Rachel Cook</td>
</tr><tr class="row-4 even">
	<td class="column-1">Tuesday Aug 4</td><td class="column-2">1:00-1:30</td><td class="column-3">Business Meeting - Advisory Council members only</td><td class="column-4"></td>
</tr><tr class="row-5 odd">
	<td class="column-1">Thursday Aug 6</td><td class="column-2">12:00-12:30</td><td class="column-3">Background on FPC Soil Management System - Watch or review this before Thursday at 12:00 if you are unfamiliar with the FPC soil system<br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_FPC-Soils-Background_Recorded-only.pdf" target="_blank" rel="noopener noreferrer">PDF</a><br>
<a href="https://youtu.be/vOn8J0rGMBM" target="_blank" rel="noopener noreferrer">Video</a><br>
<br>
<a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_FPC-Soils-Updates-2020.pdf" target="_blank" rel="noopener noreferrer">FPC Soil management and update (pdf)</a><br>
<a href="https://youtu.be/gzr78GkHFIs" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Rachel Cook</td>
</tr><tr class="row-6 even">
	<td class="column-1">Thursday Aug 6</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Cook_Calcium-Update-FPC-AM-2020.pdf" target="_blank" rel="noopener noreferrer">Calcium study update  (pdf)</a><br>
<a href="https://youtu.be/RA7Ec3p25vs" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Rachel Cook</td>
</tr><tr class="row-7 odd">
	<td class="column-1">Thursday Aug 6</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-8 even">
	<td class="column-1">Tuesday Aug 11</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Sumnall_FPC_AM_RStools_2020.pdf" target="_blank" rel="noopener noreferrer">LiDAR and Landsat tools overview and update (pdf)</a><br>
<a href="https://youtu.be/4mXrpELpOR0" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Matthew Sumnall</td>
</tr><tr class="row-9 odd">
	<td class="column-1">Tuesday Aug 11</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Sumnall_FPC_AM_UAV_and_TLS_projects_2020.pdf" target="_blank" rel="noopener noreferrer">High resolution metric estimation - UAV and terrestrial laser scanning research (pdf)</a><br>
<a href="https://youtu.be/m4i30kOugro" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Matthew Sumnall</td>
</tr><tr class="row-10 even">
	<td class="column-1">Tuesday Aug 11</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-11 odd">
	<td class="column-1">Thursday Aug 13</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Albaugh-RW20-Aug-13-2020-AM.pdf" target="_blank" rel="noopener noreferrer">Contribution of biomass partitioning in explaining loblolly pine growth differences in the Southeast US and Brazil (pdf)</a><br>
<a href="https://youtu.be/4RuUHA9DWVM" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Tim Albaugh</td>
</tr><tr class="row-12 even">
	<td class="column-1">Thursday Aug 13</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Trlica-RW20-biomass-economics-Aug13-2020.pdf" target="_blank" rel="noopener noreferrer">Effect of genetics, planting density, and silviculture on financial returns from short rotation loblolly pine plantations for bioenergy (pdf)</a><br>
<a href="https://youtu.be/HwqYMAX-3T8" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Andrew Trlica</td>
</tr><tr class="row-13 odd">
	<td class="column-1">Thursday Aug 13</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-14 even">
	<td class="column-1">Tuesday Aug 18</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Carter-Site-Prep-and-15N.pdf" target="_blank" rel="noopener noreferrer">Effects of site preparation timing on loblolly pine and midrotation 15N fertilization  (pdf)</a><br>
<a href="https://youtu.be/gos3Qneacho" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">David Carter</td>
</tr><tr class="row-15 odd">
	<td class="column-1">Tuesday Aug 18</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Carter_Genotypic-Mixing.pdf" target="_blank" rel="noopener noreferrer">Complimentarity increases production in genetic mixtures of loblolly pine (pdf)</a> <br>
<a href="https://youtu.be/cg-pabtXocI" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">David Carter</td>
</tr><tr class="row-16 even">
	<td class="column-1">Tuesday Aug 18</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-17 odd">
	<td class="column-1">Thursday Aug 20</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Albaugh-RW19-and-LobTHIN-Aug-20-2020-AM.pdf" target="_blank" rel="noopener noreferrer">Using RW19 data to validate a new thinning model (pdf)</a><br>
<a href="https://youtu.be/De4JXDuLmCo" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Tim Albaugh</td>
</tr><tr class="row-18 even">
	<td class="column-1">Thursday Aug 20</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Albaugh-RW28-Aug-20.pdf" target="_blank" rel="noopener noreferrer">RW28 P carryover workplan changes and installation update (pdf)</a><br>
<a href="https://youtu.be/pLa7deMXYTQ" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Tim Albaugh</td>
</tr><tr class="row-19 odd">
	<td class="column-1">Thursday Aug 20</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-20 even">
	<td class="column-1">Tuesday Aug 25</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Rubilar-RW23-WC-Intensity-and-Duration-8-years-E-grandis-2020.pdf" target="_blank" rel="noopener noreferrer">RW23 Intensity and duration of weed control in Eucalyptus grandis - 8 year response  (pdf)</a><br>
<a href="https://youtu.be/QvZCPw8qwJQ" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Rafael Rubilar</td>
</tr><tr class="row-21 odd">
	<td class="column-1">Tuesday Aug 25</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Campoe_RW20-BR_C-Balance_2020.pdf" target="_blank" rel="noopener noreferrer">Implications of C balance on the productivity of varietals (pdf)</a><br>
<a href="https://youtu.be/0qzcQFvL9gQ" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Otávio Campoe</td>
</tr><tr class="row-22 even">
	<td class="column-1">Tuesday Aug 25</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-23 odd">
	<td class="column-1">Thursday Aug 27</td><td class="column-2">12:00-12:30</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Campoe-RW07-BR-Pinus-Early-growth_2020.pdf" target="_blank" rel="noopener noreferrer">Effects of site preparation, weed control, and fertilization on pine plantations in southern Brazil (pdf)</a><br>
<a href="https://youtu.be/dJpHTeVunAU" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Otávio Campoe</td>
</tr><tr class="row-24 even">
	<td class="column-1">Thursday Aug 27</td><td class="column-2">12:30-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/08/Rubilar-FPC-AM-2020-RW7-Long-term-effects-of-site-prep-wc-and-fert_final.pdf" rel="noopener noreferrer">Long-term effects of site preparation, weed control, and fertilization on radiata pine (pdf)</a><br>
<a href="https://youtu.be/dDLSw8pHNls" target="_blank" rel="noopener noreferrer">Video</a></td><td class="column-4">Rafael Rubilar</td>
</tr><tr class="row-25 odd">
	<td class="column-1">Thursday Aug 27</td><td class="column-2">1:00-1:30</td><td class="column-3">Open discussion</td><td class="column-4"></td>
</tr><tr class="row-26 even">
	<td class="column-1">Tuesday Sep 1</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/09/Hackman_FPC-2020-RW28-Soil-P.pdf" rel="noopener noreferrer">RW28 Biotic And Abiotic Contributions To Phosphorus Bioavailability For Loblolly Pine (pdf)</a></td><td class="column-4">Jacob Hackman</td>
</tr><tr class="row-27 odd">
	<td class="column-1">Tuesday Sep 1</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/09/Cohrs-Sentinel2-FPC-AM-2020.pdf" rel="noopener noreferrer">Sentinel-2 LAI Web App, Soil Modeling, and Precision Forestry (pdf)</a><br>
<a href="https://youtu.be/9DLk5y2VfQM" target="_blank" rel="noopener noreferrer">Sentinel 2 Data download walkthrough and directory navigation video</a><br>
<br>
</td><td class="column-4">Chris Cohrs</td>
</tr><tr class="row-28 even">
	<td class="column-1">Tuesday Sep 1</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/09/Caisley-Ca-Project-update-FPC-2020-LabpHWork_V4.pdf" rel="noopener noreferrer">Calcium study:  Increasing the pH of organic soil (pdf)</a></td><td class="column-4">Lena Caisley</td>
</tr><tr class="row-29 odd">
	<td class="column-1">Tuesday Sep 1</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://forestproductivitycoop.net/wp-content/uploads/2020/09/Byers-Chem-Site-Prep-study-FPC-2020-AM-Presentation-9-1.pdf" rel="noopener noreferrer">Chemical site prep study (pdf)</a></td><td class="column-4">Alexander Byers</td>
</tr><tr class="row-30 even">
	<td class="column-1">Tuesday Sep 1</td><td class="column-2">12:00-1:00</td><td class="column-3"><a href="https://youtu.be/Ymue23pt_H0" rel="noopener noreferrer">Video, all speakers combined</a></td><td class="column-4">Hackman, Cohrs, Caisley and Byers</td>
</tr></tbody>
</table>
'  
#replicates the table as a df
my_df <- as.data.frame(read_html(text) %>% html_table(fill=TRUE))

(my_df)%>%wex()


#delete<-

# headt<-
#get the text from the links
text%>%
  read_html()%>%
  html_elements("a")%>%
  html_text()%>%
  wex()
head()

#get the urls form the links
text%>%
  read_html()%>%
  html_elements("a")%>%
  html_attr("href")%>%
  wex()

#can i get the html url and the columns stuff?
#1st look for just the columns stuff
text%>%
  read_html()%>%
  html_elements("tr")%>%
  head()
#this gives the rows as individual html things
text%>%
  read_html()%>%
  html_elements("td")%>%
  head()
#this gives the columns by row as individual html things

html_text()%>%
  
  #see what rcrawler does-----
LinkExtractor(url="https://forestproductivitycoop.net/2020-us-annual-meeting/",
              ExternalLInks=T)
#seems powerful but too long to learn. no obvious way to get link text

#see if rvests html_text helps with out mapdf:----
#pretty sure it doesnt because the htlm nodes isnt working
library(rvest)

url<-"https://forestproductivitycoop.net/2020-us-annual-meeting/"

#read the page
page<-read_html(url)

#get hyperlink nodes
#the 'a' tag under a 'h3' tag under 'div' tag of class 'summary' 
#under a 'div' tag of class 'question-summary'
nodes<-html_nodes(page, "div.question-summary div.column-3 h3 a")

#Get text
question<-html_text(nodes)
#get link
link<-paste0("https://stackoverflow.com", html_attr(nodes, "href"))

answer<-data.frame(question, link)
head(answer)

#see if rvest works with map_df:-----
library(tidyverse)
library(rvest)

nodes <- read_html('https://stackoverflow.com/questions/tagged/r?tab=votes&page=1&pagesize=50')%>%html_nodes("[class=question-hyperlink]")

page2020<- read_html("https://forestproductivitycoop.net/2020-us-annual-meeting/")
#  html_nodes(trash, "table[border='1']")
html_nodes(page2020, "table[id='tablepress-168']")


html_elements(page2020, xpath=".//div[@data-testid=]")
html_nodes("[class=column-3]")

df <- map_df(nodes,~{
  questions = .x %>% html_text()
  links =  paste0('https://stackoverflow.com',.x %>% html_attr("href") )
  tibble(questions, links)
})



#trash-----
read_html(
  '
 <table border="1" cellspacing="0" cellpadding="5">
   <tbody><tr><td>
   <table border="0" cellpadding="2" cellspacing="0">
   <tbody><tr>
   <td bgcolor="#ff9999"><strong><font size="+1">CASEID</font></strong></td>
   </tr></tbody>
   </table>
   <tr><td>[tbody]
 </table>
')->trash

html_nodes(trash, "a")

#summarise slight success----
#replicates the table as a df
my_df <- as.data.frame(read_html(text) %>% html_table(fill=TRUE))
head(my_df) 
#gets all the text associated with links in a list, not confined to 
#... what isinside cells
headt<-text%>%
  read_html()%>%
  html_elements("a")%>%
  html_text()

#this is how i get the links again
text%>%
  read_html()%>%
  html_elements("a")%>%
  html_attr(., "href")%>%
  wex()

#im sure I could get the speaker etc somehow


list.fol
#just get pdfs------
# Set the working directory to the directory containing the folders to copy
setwd("G:/Shared drives/FER - FPC Team/Meetings")

# Create a list of all folders in the source directory
folders <- list.dirs()

# Create a new directory to copy the PDF files to
dir.create("G:/Shared drives/FER - FPC/Meetings")

# Loop through each folder and copy PDF files to the new directory
for (folder in folders) {
  # Create a list of PDF files in the current folder
  pdf_files <- list.files(path = folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # If there are PDF files, copy them to the new directory
  if (length(pdf_files) > 0) {
    file.copy(from = pdf_files, to = "G:/Shared drives/FER - FPC/Meetings", overwrite = TRUE)
  }
}
#just get pdfs but maintain hierarchy-----
# Set the working directory to the directory containing the folders to copy
setwd("G:/Shared drives/FER - FPC Team/Meetings")

# Create a new directory to copy the PDF files to
dir.create("G:/Shared drives/FER - FPC/Meetings", showWarnings = FALSE)

# Loop through each subdirectory in the Meetings directory
for (folder in list.dirs(full.names = TRUE)) {
  # Create a new directory in the target directory that matches the source directory
  target_folder <- file.path("G:/Shared drives/FER - FPC/Meetings", folder)
  dir.create(target_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Create a list of PDF files in the current folder
  
  pdf_files <- list.files(path = folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # If there are PDF files, copy them to the new directory
  if (length(pdf_files) > 0) {
    file.copy(from = pdf_files, to = target_folder, overwrite = TRUE)
  }
}



#same as above but for thing that also have a pptx:~~~~~~~~~~~~~

# Set the working directory to the directory containing the folders to copy
setwd("G:/Shared drives/FER - FPC Team/Meetings")

# Create a new directory to copy the PDF files to
dir.create("G:/Shared drives/FER - FPC/Meetings", showWarnings = FALSE)

# Loop through each subdirectory in the Meetings directory
for (folder in list.dirs(full.names = TRUE)) {
  # Create a new directory in the target directory that matches the source directory
  target_folder <- file.path("G:/Shared drives/FER - FPC/Meetings", folder)
  dir.create(target_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Create a list of PDF files in the current folder
  
  pdf_files <- list.files(path = folder, pattern = "\\.ppt$", full.names = TRUE)%>%
    str_replace(string=.,pattern = "\\.ppt$","\\.pdf")
  
  
  #  pdf_files <- list.files(path = folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # If there are PDF files, copy them to the new directory
  if (length(pdf_files) > 0) {
    file.copy(from = pdf_files, to = target_folder, overwrite = TRUE)
  }
}


#In the code above, we first set the working directory to the directory containing the source folders. We then use the list.dirs function to create a list of all folders in the source directory.

#Next, we create a new directory to copy the PDF files to using the dir.create function.

#We then loop through each folder in the folders list, and use the list.files function to create a list of PDF files in each folder. The pattern argument is set to "\\.pdf$" to only list files with a ".pdf" extension. The full.names argument is set to TRUE to return the full path to each file.

#If there are any PDF files in the current folder, we use the file.copy function to copy them to the new directory.

#Note that the overwrite argument in file.copy is set to TRUE to overwrite any existing files in the destination directory with the same name.


#delete, timing stuff, delete this----

start_time <- Sys.time()
sleep_for_a_minute()
end_time <- Sys.time()
end_time - start_time

#
all_files <- list.files() # Get all the files in your working directory
pptx_files <- all_files[ which(stringr::str_detect(all_files, "\\.pptx"))] # filter for only those files with .pptx extenstion

# Loop through all the pptx files converting them to pdf using the function. Replace the .pptx extension with a .pdf
start_time <- Sys.time()
for(i in seq_along(pptx_files)) {
  convert_to_pdf(path = pptx_files[i], pdf_file = paste0(str_remove(pptx_files[i], "\\.pptx"), ".pdf"))
} #this is in 
end_time <- Sys.time()
end_time - start_time




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
#html table trtash----
#Media Library File Download
#trash
library(XML)
library(httr)

pgsession
url="https://forestproductivitycoop.net/2020-us-annual-meeting/"
#pick up modifying this  to use on a members page and see if it writes now
read_html(url)%>%write_html(file="G:/Shared drives/FER - FPC Team/Website/Htmltables/2020writehtml-loggedin.html")


r2 <- s %>% GET("https://forestproductivitycoop.net/2020-us-annual-meeting/")

# Extract the HTML from the response
html <- content(r, as = "text")



content(r,as = "text")%>%head()

read_html("https://forestproductivitycoop.net/2020-us-annual-meeting/")%>%write_html(file="G:/Shared drives/FER - FPC Team/Website/Htmltables/2020writehtml-loggedin.html")

tbl <- url %>% read_html() %>% html_node("table") #%>% html_table(fill = TRUE,header=F)
url %>% read_html() %>% html_node("article div") 
resXML <- htmlParse(url, as = "text")

htmlParse(url, isURL = T)

url <- paste0("http://www.speech.cs.cmu.edu/cgi-bin/cmudict?in=",word,"&stress=-s")
h <- handle(url)
res <- GET(handle = h)

GET(handle(url))
htmlpar

library(XML)
library(RCurl)
#url <- ("https://angel.co/companies?locations[]=1647-India")
httr::set_config(httr::config(ssl_verifypeer=0L))
devtools::install_github('r-lib/httr')
htmlContent <- getURL(url)
htmlTree <- htmlTreeParse(htmlContent)
#md


#html DONT DELETE login (start)-----
#get full list of webpages read in:
mp<-read.csv("Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-maintable.csv")%>% #main page table, only the events with tables on the page when you click on them as opposed ot just a pdf or video etc
  subset(.,site==TRUE)%>%
  select(link,linkbase,what)





# Create a web session with the desired login address
s <- session("https://forestproductivitycoop.net/wp-login.php")

# Fill in login form
pgform <- html_form(s)[[1]]
filled_form <- html_form_set(pgform, log = "sabloszi@ncsu.edu", pwd = "didn'tthinkid6ehere")


#mp2<-mp#safe big one is mp2
#mpp<-mp2%>%
# subset(.,what=="Title (pdf)")#had to do chunks bc unknown ones kep messing up and apparently it doensnt do them in order
#
#mpp2<-mpp[,]

#mpl<-lapply(split(mpp2,mpp2$link), function(x){

r2 <- 
  session_submit(s, filled_form)%>%
  session_jump_to(., "https://forestproductivitycoop.net/2015-annual-meeting/")

#8/28/23 pick up getting the last full one, the one with two presetnation columns then delete this line
#session_jump_to(., x$link) 

#use this to test what it got:
#r2%>%
# read_html()%>%write_html(file="G:/Shared drives/FER - FPC Team/Website/Htmltables/2020writehtml-loggedin.html")


#html DONT DELETE actual page stuff----


# read the HTML table from the URL
#url <- "https://mayhaw.github.io/delete/"
url<-r2 #from the 2020 or whatever actual page

#need this for later to get the text columns
tbl <- url %>% read_html() %>% html_node("table") %>% html_table(fill = TRUE,header=T)

#need this for the immediately next steps to get the code. it is a bodge that I don't really understand because 
#... it doesn't actually matter what column you pick in the .$Presentation%>% part but it seems like it does have to reproduce the list element the same number of times as the same number of rows in the dataframe for everything else to work so picking presentation works fine
html_pres <- 
  url %>% read_html() %>% html_node("table") %>% html_table(fill = TRUE,header=T)%>% 
  .$`Presentation` %>%
  map(~read_html(url))

#get the links from the html_pres thing
links<-html_elements(html_pres[[1]],"tr")[-1]%>% #the -1 part in  ...tr")[-1]%  is a bodge because I'm manually getting rid of the first row of the table becauses really it's a header but idk how else to do this for now  since there's no header option for this stuff since html_elements isn't meant for thinking of this as a table
  map(~html_nodes(., "a") %>% html_attr("href"))

#get the english text that the link is presented as to the website viewer from the html_pres thing
targets<-  html_elements(html_pres[[1]],"tr")[-1]%>%  #the -1 part in  ...tr")[-1]%  is a bodge because I'm manually getting rid of the first row of the table becauses really it's a header but idk how else to do this for now since there's no header option for this stuff since html_elements isn't meant for thinking of this as a table
  map(~html_nodes(., "a") %>% html_text())

# combine the links and targets into a single column
link_targets <- mapply(function(links, targets) {
  paste0("(", links, ", ", targets, ")")
}, links, targets, SIMPLIFY = FALSE)

#get the original html_table() character data together with the link html_text() ("targets") and html_attr("href) ("link") info
tbl<-data.frame(tbl)%>%
  mutate(link_target = link_targets) %>%  
  unnest(link_target) %>%
  separate(link_target, into = c("link", "target"), sep = ", ", remove = FALSE)%>%
  mutate(link=  substr(link,2,nchar(link)))%>%
  mutate(target=  substr(target,1,nchar(target)-1))



tbl<-tbl%>%
  mutate(linkbase=basename(link))
wex(tbl)

#})

#https://docs.google.com/spreadsheets/d/1mTh9tjHqDijWx0OsoPXTXF_RH0croFYTip0vMmAQeVU/edit?usp=sharing
#got the button on the title menue but it doesnt work.


#google drive file info stuff----

#usually need to login in fresh r-: (also usually need to check the boxt that says allow tidyverse api to see files in drive):
#PICK SEND TO BROWSER
drive_auth(
  email = gargle::gargle_oauth_email(),
  #path = NULL, shouldn't need this unless it stops remembering sabloszi, in which case Idk what to write here instead of NULL
  scopes = "https://www.googleapis.com/auth/drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
#i used this to get the file list in a google file folder INCLUDING the links etc
#https://drive.google.com/drive/u/0/folders/...[your id here]
#xme <- drive_get(id="1YQacwq8m4320xQgfJ1RNgmZMYFXmKtRI")
#xme <- drive_ls(as_id="1FpjYxZkAfqKEs0jB-oybreMETapXjuuW")
#xme <- drive_ls(drive_get(id="1YQacwq8m4320xQgfJ1RNgmZMYFXmKtRI"))
xme <- drive_ls(drive_get(id="1kJ5Wau23QKu1Wuw0l1R8fL_lRKV42-fO"))#usually you want this one which is "Q:\Shared drives\FER - FPC Team\Website\Htmltables\wordpressdownloads\mlfd-forestproductivitycoop"

#1kJ5Wau23QKu1Wuw0l1R8fL_lRKV42-fO is the unsplit folder, downloaded 9/?/23
#1FpjYxZkAfqKEs0jB-oybreMETapXjuuW is the unsplit  folder, downloaded 4/25/23
#1YQacwq8m4320xQgfJ1RNgmZMYFXmKtRI is split by ext folder, downloaded ~4/21?/23

#drive.google.com/open?id={file-id} should open any id if you have the id
#for example drive.google.com/open?id=1kJ5Wau23QKu1Wuw0l1R8fL_lRKV42-fO
#so pick back up seeing if you can do a merge of a real limited number of files

#see names of the google tibble dribble thing
#xme%>%names()
googledmerge<-xme%>%dplyr::select(c(id,name))
#  unnest_wider(col=drive_resource)#maybe this would tell me whats in that colkumn (its a list i thnk0)

#as is, this is a way to get recents through R. It seems toalso be a flexible search if you know how to use it
#drive_find(n_max = 30) 

#merge googledmerge with tbl----
#this finally works; has links for the same pdf in both wordpress and in google drive;
# can now repeat for all meetings but first:
#1. tweak so youtube and dropbox show up in same column as glink current contents
#figure out if dropbox stuff ends up with a link for other years
#2. keep sorting of the tbl as it was so you dont have to fool with times and dates columns
#3. fix column names
#4. next enginner a way to do at least two different meetings' tbls; 
#... should stil be able to use just one googledmerge

#need custom function temporarily to get the start of where the linktext is inside the presentation box in the wordpress
find_start_position <- function(string, substring) {
  result <- str_locate(string, substring)
  if (is.na(result[1])) {
    return(NA)
  } else {
    return(result[1])
  }
}




tbl$rowsortid  <- 1:nrow(tbl)#create tbl column to sort by later

#gold<-
merge(tbl,googledmerge,by.x="linkbase",by.y="name",all.x=T,all.y=F,sort=T)%>%
  mutate(., glink=paste0("https://drive.google.com/open?id=",id))%>%
  #GOLLLLLLLLLLLLLLLLLLLLLLD!
  
  #ok now clean up table:
  
  # mutate(.,dirname=domain(link))%>% #get host for later step
  #  mutate(.,ext=extension(link))%>%
  #  select(c(rowsortid,dirname,ext,Presentation,link,target))%>%
  mutate(start = map2_int(Presentation, target, find_start_position))%>%
  #  select(-c(link_target,linkbase))%>% #dont need link_target because it was split into link and target columns; dont need linkbase anymore because you already used it for its one purpose: the merge to get the glinks
  rename(.,wplink="link")%>%#make it clear this was the wordpress link
  mutate(.,glink=ifelse(is.na(id),NA,glink))%>%
  mutate(., replpres=ifelse(
    #if target's in the latter half
    (start>nchar(Presentation)*0.5),
    #get rid of the target (the replacement is "")
    "", #gsub(target," ",pres)
    #, It's in the first half of pres,
    #(else)##keep the whole pres as is
    target))%>%
  mutate(.,newpres=str_replace_all(.$Presentation,fixed(.$target),.$replpres))%>%
  arrange(rowsortid)%>%
  wex()
select(c(Presentation,newpres,wplink,replpres,target))
#ok so this works as long as you can figoure out how to a) do a group by to pick the shortest one of "newpres" and fill down and B) get rid of anything like (pdf) in that column
#  mutate(.,link=ifelse(dirname%in%c("youtu.be","dropbox",NA),wplink,glink))#wp links for youtube, dropbox, and NA (no link on wp); but google links for wordpress-hosted stuff 




cleangold<-gold   #leave gold as a backup for now
#FNAS pick up here 5/23/2023 - need to see where i was going with the pres titles to clean them up



cleangold2<-cleangold%>%
  select(c(Date,Time,Presentation,Speaker,link,rowsortid,target))%>%
  arrange(rowsortid)%>%#put back in order it was in the wp website html table
  select(-rowsortid)%>%
  transform(., Presentation = 
              ifelse(nchar(link)==0,Presentation,
                     paste('<a href = ', shQuote(link), '>', target, '</a>')
              ))%>%
  mutate(.,Date=gsub("\n","",Date))%>%
  mutate(.,Meeting_Type="Annual")%>%
  mutate(.,Year="2020")%>%
  select(c(Meeting_Type,Year,Date,Time,	Presentation,	Speaker))%>%
  gvisTable(., options = list(allowHTML = TRUE))
#see it
plot(cleangold2)

#turn it into flat text instead of google stuff
htmlstring <- paste(c(cleangold2$html$header,paste(cleangold2$html$chart,collapse = ""),cleangold2$html$caption,cleangold2$html$footer),collapse = "\n")

#save it



write_html(read_html(htmlstring),file="G:/Shared drives/FER - FPC Team/Website/Htmltables/2020writehtml-glinks.html")  


#1. Getting rid of target text in a string with regex (for pdf and video in pres titles): Test-----

dfl<-df%>%
  mutate(.,fuck4=nchar(target))%>%
  mutate(.,ty=c("pdf","vd"))%>%
  mutate(.,w=c("R...(pdf)","R...ff"))

dfl2<-dfl%>%
  mutate(.,presrepl=ifelse(start>0.5*(nchar(Presentation))," ",target))%>%
  #string 
  #pat
  #repl
  mutate(.,newpres=str_replace_all(.$Presentation,
                                   fixed(target),
                                   presrepl))
#ok this is good because I did get the thing i want in "newpres" in the shortest
#... version. Great if theres only two. which is most of them
dfl2%>%
  group_by(.,Presentation)%>% #meed to add year etc for big table)
  summarise(ncharm=min(nchar(newpres)))%>%
  merge(.,dfl2,by="Presentation")%>%#meed to add year etc for big table)
  mutate(.,newpres=ifelse(ncharm==nchar(newpres),newpres,NA))%>%
  select(c(Presentation,newpres))





#if(high start,replace presentation-target with "")
#if(low start,replace presentation-target with target)


















#string pat repl
mutate(.,preserepl=ifelse(start>0.5*nchar(Presentation),target,""))%>%
  mutate(.,newpres=str_replace_all(.$Presentation,
                                   fixed(preserepl),
                                   target))

#2. Getting rid of target text in a string with regex (for pdf and video in pres titles): Real-----------
golddf<-merge(tbl,googledmerge,by.x="linkbase",by.y="name",all.x=T,all.y=F,sort=T)%>%
  mutate(., glink=paste0("https://drive.google.com/open?id=",id))%>%
  #GOLLLLLLLLLLLLLLLLLLLLLLD!
  
  #ok now clean up table:
  
  # mutate(.,dirname=domain(link))%>% #get host for later step
  #  mutate(.,ext=extension(link))%>%
  #  select(c(rowsortid,dirname,ext,Presentation,link,target))%>%
  mutate(start = map2_int(Presentation, target, find_start_position))%>%
  #  select(-c(link_target,linkbase))%>% #dont need link_target because it was split into link and target columns; dont need linkbase anymore because you already used it for its one purpose: the merge to get the glinks
  rename(.,wplink="link")%>%#make it clear this was the wordpress link
  mutate(.,glink=ifelse(is.na(id),NA,glink))



golddfl2<-golddf%>%
  mutate(.,presrepl=ifelse(start>0.5*(nchar(Presentation))," ",target))%>%
  #string 
  #pat
  #repl
  mutate(.,newpres=str_replace_all(.$Presentation,
                                   fixed(target),
                                   presrepl))
#ok this is good because I did get the thing i want in "newpres" in the shortest
#... version. Great if theres only two. which is most of them
golddfl2%>%
  group_by(.,Presentation)%>% #meed to add year etc for big table)
  summarise(ncharm=min(nchar(newpres)))%>%
  merge(.,golddfl2,by="Presentation")%>%#meed to add year etc for big table)
  mutate(.,newpres=ifelse(ncharm==nchar(newpres),newpres,NA))%>%
  #select(c(Presentation,newpres))%>%
  wex()





#3. Getting rid of target text in a string with regex (for pdf and video in pres titles): List (delete this eventually)----
#this was just to see if I could turn a df into a list. Was goig to submit to the oracle for advice. Or at least see how a list vs df looks and play with unlist(
#or something to figure out on my own, since really the presentaiton tables are more like lists in that there are
#... multiple and different numbers of pdfs and videos per talk/time slot/agenda item
df<-data.frame(a=(c("video","pdf","pdf1","pdf2","video","pdf")),
               remove=c("recording","presentation","pdf1","pdf2","video","pdf"),
               b=c("abcrecording","abcpresentation","defpdf1pdf2","defpdf1pdf2","defvideo","ghipdf"))

dfrun<-paste(unique(df$remove), collapse = "|")

df$c<-gsub(dfrun,"",df$b)


list("t1"=data.frame(b="oct1",c=c("t1video","t1pdf")),
     "t2"=data.frame(b="oct1",c=c("t2pdf1","t2pdf2","t2video")),
     "t3"=data.frame(b="oct1",c=c("t1pdf")))

#4. Getting rid of target text in a string with regex (for pdf and video in pres titles): whichmax trash (delete this eventually, probably sooner than later)-----
df <- data.frame(Presentation = c("Rachel talks about stuff(pdf)", "Rachel talks about stuff(pdf)"),
                 target = c("(pdf)", "Rachel t"))%>%
  mutate(start = map2_int(Presentation, target, find_start_position))

#not that important/germance but worth some sor points
#pull<-df%>% 
#  pull(target)
#which(pull==max(nchar(pull)))
#drunk you found out somethign interesting if you want to pist a q to sor
# end not that important 

dfl<-df%>%
  mutate(.,fuck4=nchar(target))%>%
  mutate(.,ty=c("pdf","vd"))%>%
  mutate(.,w=c("R...(pdf)","R...ff"))
dfl%>%
  #string pat repl
  mutate(.,preserepl=ifelse(start>0.5*nchar(Presentation),target,""))%>%
  mutate(.,newpres=str_replace_all(.$Presentation,
                                   fixed(preserepl),
                                   target))
print()
#iftarget's at the end,
#... new pres= oldpres
#...else newp= old pres but get rid of presentation pdf thing
#realization: need to get rid of "pdf" on lines with no indication that there's
#... anything special about "pdf" so what do i di??????
#just get rid of anything with a linK thats not at the beginning....
#... and only keep presentation's linked text if it's at the beginning.
#... else get rid of linked text in presentation (target0)





str_repl_






mutate(.,preserepl=ifelse(start>0.5*nchar(Presentation),target," "))%>%
  mutate(.,newpres=str_replace_all(.$Presentation,fixed(.$presrepl),
                                   ("fuck")))%>%
  print()
group_by(.,Presentation)%>%
  summarise(.,fucklong=min(fuck4))%>%
  merge(.,dfl,by="Presentation")


subset(which(nchar=max(nchar(.))))
#5. Getting rid of target text in a string with regex (for pdf and video in pres titles): almost certainly trash (delete this eventually, probably sooner than later)----
# Load the stringr package
library(stringr)

# Create a sample data frame
df <- data.frame(pres = c("Rachel talks about stuff(pdf)", "Rachel talks about stuff(pdf)"),
                 target = c("(pdf)", "Rachel t"))

# Define a function to find the starting position of a string in a column of character data
find_start_position <- function(string, substring) {
  result <- str_locate(string, substring)
  if (is.na(result[1])) {
    return(NA)
  } else {
    return(result[1])
  }
}


replace_target_string <- function(pres, target, start) {
  if (start < 0.5*nchar(pres)) {
    result <- gsub(target, " ", pres)
  } else {
    result <- pres
  }
  return(result)
}

df %>%
  mutate(start = map2_int(pres, target, find_start_position))%>%
  mutate(.,pres=ifelse(start<0.5,
                       replace_target_string(pres, target,start),
                       pres)
         
         
         df%>%
           mutate(.,start=gsub("f","",target))
         
         
         
         
         
         # Apply the function to the data frame using map2
         result <- map2(df$pres, df$target, find_start_position))

# Print the result
print(result)

#6. Getting rid of target text in a string with regex (for pdf and video in pres titles): mostly trash (delete this eventually, probably sooner than later)----

# Define a function to find the starting position of a string in a column of character data
find_start_position <- function(string, substring) {
  result <- str_locate(string, substring)
  if (is.na(result[1])) {
    return(NA)
  } else {
    return(result[1])
  }
}

# Define a function to replace the "target" string with a space character in the "pres" string
# only if the "start" value is less than 3
replace_target_string <- function(pres, target, start) {
  if (start < 3) {
    result <- gsub(target, " ", pres)
  } else {
    result <- pres
  }
  return(result)
}
# Create a sample data frame
df <- data.frame(pres = c("Rachel talks about stuff(pdf)", "Rachel talks about stuff(pdf)"),
                 target = c("(pdf)", "Rachel t"))



df<-df %>%
  mutate(start = map2_int(pres, target, find_start_position))

dfrun<-paste(unique(df[df$start>0.5*nchar(df$pres),"target"]), collapse = "|")
df%>%
  mutate(., replpres=ifelse(
    #if target's in the latter half
    (start>nchar(pres)*0.5),
    gsub(dfrun,"",.$pres,fixed=T),
    pres))%>%
  wex()

gsu

#" "))%>%
mutate(.,newpres=str_replace_all(.$pres,fixed(.$target),.$replpres))%>%
  
  str_repl

vector <- c("SPN.subset(RELN(s)geneset1", "Myeloid.svz.geneset1")
sub("\\(\\)", ").caudate.", vector)

gsub("log\\(", "", string)

#string pat repl
df%>%
  )))))))

df%>%
  mutate(.,presw=gsub(target," ",pres))



# Print the result
print(dfwant)
#read a gmail----

#rmarkdown----



##libreoffice stuff to convert pptx or docx or doc to pdf-----
library(dplyr)
library(purrr)
library(docxtractr)

# You have to show the way to the LibreOffice before
set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")

#0.1 set wd
setwd("G:/Shared drives/FER - FPC Team")
library(dplyr)
library(purrr)
library(docxtractr)

# You have to show the way to the LibreOffice before
set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")

# 1) List of pptx documents
pptxs <- list.files("Meetings/",recursive = T,
                    pattern = "?.pptx",
                    full.names = T)

# 2) Custom function
pptx2pdf <- function(path){
  
  # Let's extract the name of the file
  name <- str_remove(path, "Meetings/") %>% 
    str_remove(".pptx")
  
  convert_to_pdf(path,
                 pdf_file = paste0("Meetings/",
                                   name,
                                   ".pdf"))
  
}

# 3) Convert
pptxs %>%
  map(~pptx2pdf(.x))


#delete-----
read.csv("C:/Users/sabloszi/Desktop/delete.csv")%>%
  select(-STUDY)%>%
  melt(id.vars="PLOT")%>%
  cast(PLOT~variable)

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




#maybe delete? idk what this was it was just op[en in an unsaved r----
srpt23<-vrt23%>%
  mutate(.,srpt=paste0(site,rt,plot,tre))%>%
  pull(srpt)

srpti<-vrt_Interlude%>%
  mutate(.,srpt=paste0(site,rt,plot,tre))%>%
  pull(srpt)


srpt23[!(srpti%in%srpt23)]

c(srpti,srpt23)%>%
  .[duplicated(.)]

#plots 127-0-2, 127-620-5, and 127-620-3 all exist in both interlude and vrt23

srpti[ !srpti %in% srpt23]
srpt23[ !srpt23 %in% srpti]

srpt23[grepl("127",srpt23)]
srpt23[substr(srpt23,1,3)==127]
srpti[substr(srpti,1,3)==127]


options(rstudio.help.showDataPreview = FALSE) in your ~/.Rprofile.


R.home()

file.edit(file.path("~", ".Rprofile"))
file.edit(".Rprofile")
#color viewing , prob delete this later-----

"#002e52"
"#161f29"
"#29506a"
"#26487e"
"#0e0f3b"
"#1f305e" #flexfit royal rbg 39,60,118
ggplot(data=data.frame(x=c(100,200),y=c(1,1),color=c("#002e52","#1f305e")),aes(x=x,y=y),color=color)+
  geom_point(size=300,aes(color=color))+
  scale_color_identity(guide = 'legend')+
  theme(legend.position = "none")


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
#get unique trts in varrate_-----
read.csv("C:/Users/sabloszi/Desktop/delete.csv",header = T)%>%
  select(c(ResrcID,Herb,fert_rx,fert_FINAL,GID))%>%
  subset(ResrcID=="SMIT-RS-00127")%>%
  cast(.,Herb+fert_rx+fert_FINAL~ResrcID)
head()
names()
select()
head()

#lidar stuff------
install.packages("FPCALSpackage")
library(FPCALSpackage)
fpc.lidar.app()  
#dates-----
set.seed(1)
as.Date(sample.int(365,24), origin=as.Date("1970-01-01"))%>%sort()

#download randos 8/29-----
download.file(url="http://forestproductivitycoop.net/fpcdata/fox-rw19-annual-meeting-2015-pdf.pdf/",
              destfile="C:/Users/sabloszi/Desktop/delete/fox-rw19-annual-meeting-2015-pdf.pdf")
shell.exec("http://forestproductivitycoop.net/fpcdata/fox-rw19-annual-meeting-2015-pdf/")


download.file(url="https://forestproductivitycoop.net/wp-content/uploads/2019/10/Cook_FPC-Intro-CM-2019_Spanish.pdf",
              destfile="C:/Users/sabloszi/Desktop/delete/Cook_FPC-Intro-CM-2019_Spanish.pdf")


s <- session("https://recipes.had.co.nz/")

session_submit(s, filled_form)%>%
  session_jump_to(., "https://forestproductivitycoop.net/wp-content/uploads/2019/10/Cook_FPC-Intro-CM-2019_Spanish.pdf")%>%
  read_html%>%
  write_html(file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Cook_FPC-Intro-CM-2019_Spanish.html")

session_jump_to("orcshid.gif") %>%
  session_jump_to("/") %>%
  session_history()

download_files("https://forestproductivitycoop.net/wp-content/uploads/2019/10/Cook_FPC-Intro-CM-2019_Spanish.pdf")  

#v8 stuff-----
#pdf_links<-
session_submit(s, filled_form)%>% 
  session_jump_to("https://forestproductivitycoop.net/publications/fpc-presentations/2013-october-7-9-annual-meeting-atlanta-ga/")%>%
  session_follow_link("Summary of Latin America Pine Working Group Projects & Regionwide Studies (pdf)")%>%#Research
  html_nodes("a[href*='-pdf']")
html_attr("href")


#doanload pdfs chatgpt for loop-----
#ok so this owkrs with .pdf files.
pdf_links<-c("http://forestproductivitycoop.net/fpcdata/agenda-latinamerican-pine-and-eucalyptus-contact-meeting-2016-pdf/",
             "http://forestproductivitycoop.net/fpcdata/allen-fpc-challenges-success-opportunities-pdf/"
)
session_submit(s, filled_form)%>% 
  session_jump_to("https://forestproductivitycoop.net/publications/fpc-presentations/2013-october-7-9-annual-meeting-atlanta-ga/")%>%
  session_follow_link("Research")%>%#Summary of Latin America Pine Working Group Projects & Regionwide Studies (pdf)
  read_html()%>%
  str()
write_html(file="C:/Users/sabloszi/Desktop/delete/success.html")
str()

html_node(".pdf")%>%
  write_disk("C:/Users/sabloszi/Desktop/delete/success.pdf")



#cant use GET on the -pdf ones b/c its not in session  
GET("http://forestproductivitycoop.net/fpcdata/allen-fpc-challenges-success-opportunities-pdf/")%>%
  content()
write_html(file="C:/Users/sabloszi/Desktop/delete/success.html")



#orig  good for loop-----


GET("https://forestproductivitycoop.net/wp-content/uploads/2019/10/Cook_FPC-Intro-CM-2019_Spanish.pdf",
    write_disk("C:/Users/sabloszi/Desktop/delete/Cook_FPC-Intro-CM-2019_Spanish.pdf",overwrite=T))
#c("https://forestproductivitycoop.net/wp-content/uploads/2019/10/Cook_FPC-Intro-CM-2019_Spanish.pdf")
#pdf_links<-

pdf_links<-"Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres.csv"%>%
  read.csv(encoding="UTF-8")
pull(link)
#subet for .dpfs
pdf_links<-pdf_links[grepl("\\.pdf", pdf_links)]

pdf_links<-pdf_links[!grepl("dropbox", pdf_links)]

wex(pdf_links)

for (pdf_link in pdf_links) {
  pdf_response <- GET(pdf_link, write_disk(paste0("C:/Users/sabloszi/Desktop/delete/wpdownloads/", basename(pdf_link)),overwrite=T))
  if (http_status(pdf_response)$category != "Success") {
    cat("Failed to download:", pdf_link, "\n")
  }
}
#just merge the main table with year by year===-----
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
#sr------
t |> 
  group_by()

mutate(sstr_segment = str_split(V1, "-", n = 2))|>
  
  print()
#gd-----
library(qdap)
library(magrittr)
t<-  read.table(text="
V1,V2
  Video of all presentations and discussionPart 1Part 2,Part 1
  Video of all presentations and discussionPart 1Part 2,Part 2
  Background - PDFVideo Soil management and update (pdf)Video,PDF
  Background - PDFVideo Soil management and update (pdf)Video,Video
  Background - PDFVideo Soil management and update (pdf)Video,Soil management and update (pdf)
  Background - PDFVideo Soil management and update (pdf)Video,Video
",
                header=T,sep = ",")

#oai----
t %>%
  mutate(V2 = str_extract(V1, V2),
         V1 = str_replace(V1, V2, "")) %>%
  select(V1, V2)

#practice lag grepl thing-----
library(qdap)
library(magrittr)
t<-  read.table(text="
V1,V2
Video of all presentations and discussionPart 1,Part 1
Video of all presentations and discussionPart 1,Part 2
Background - PDF,PDF
Background - Video,Video
Background - Soil management and update (pdf),Soil management and update (pdf)
Background - Video,Video
",
                header=T,sep = ",")
dat <- structure(list(V1 = c("  Video of Animals-ElephantRhino", "  Video of Animals-ElephantRhino", "  Audio at loud volume-SirensHornsCrickets", "  Audio at loud volume-SirensHornsCrickets", "  Audio at loud volume-SirensHornsCrickets"), V2 = c("Elephant", "Rhino", "Sirens", "Horns", "Crickets")), class = "data.frame", row.names = c(NA, -5L))
dat$ans<-mapply(sub, paste0("\\b[A-Za-z]*(", dat$V2, ")[A-Za-z]*\\b"), "\\1", dat$V1)
str()
# \\b[A-Za-z]*(Elephant)[A-Za-z]*\\b    \\b[A-Za-z]*(Rhino)[A-Za-z]*\\b   \\b[A-Za-z]*(Sirens)[A-Za-z]*\\b    \\b[A-Za-z]*(Horns)[A-Za-z]*\\b 
#      "  Video of Animals-Elephant"         "  Video of Animals-Rhino"    "  Audio at loud volume-Sirens"     "  Audio at loud volume-Horns" 
# \\b[A-Za-z]*(Crickets)[A-Za-z]*\\b 
#  "  Audio at loud volume-Crickets" 



'Video of Animals-ElephantCrickets,Elephant'
'Audio at loud volume-ElephantCrickets,Crickets'
'Audio at loud volume-ElephantCrickets,Elephant'

t%>%  
  split(.,.$V1)%>%  
  lapply(.,function(x){(unique(x$V2))})%>%
  lapply(.,function(y){mgsub(pattern=y[[1]],replacement="",names(y))})

read.table(text="
V1,V2
  Video of Animals-Elephant,Elephant
  Video of Animals-Rhino,Rhino
  Audio at loud volume-Sirens,Sirens
  Audio at loud volume-Horns,Horns
  Audio at loud volume-Crickets,Crickets
",header=T,sep = ",")

#unzp-----
unzip("Q:/Shared drives/FER - FPC Team/Website/Htmltables/wordpressdownloads/mlfd-forestproductivitycoop3.zip")
#seleniooooooooooooooooom-----
library(RSelenium)
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$navigate("https://forestproductivitycoop.net/publications/fpc-presentations/2004-advisory-council/")
docker-machine ip  

#downloads------
#' Download multiple files, in parallel
#'
#' For each file, a list of URLs can be given, and they are tried one
#' by one.
#'
#' If a `<filename>.etag` file exists, then it is used to check if the
#' download is needed, with a HTTP HEAD request.
#'
#' @param downloads Named list. The names are the paths for the target
#'   files. Each list element is a character vector of URLs to try.
#'
#' @keywords internal
#' @importFrom curl new_pool curl_fetch_multi multi_run new_handle
#'   handle_setopt parse_headers

download_files <- function(downloads) {
  
  result <- vector(mode = "list", length(downloads))
  etags <- get_etags_for_downloads(downloads)
  
  make_callbacks <- function(which) {
    force(which)
    last_try <- 0
    ## This is the etag for HEAD, and NULL for GET
    expected_etag <- NULL
    last_verb <- "GET"
    
    callbacks <- list(
      done = function(resp) {
        if (is_success(resp)) {
          result[[which]] <<- downloads[[which]][[last_try]]
          target <- get_target(downloads, which, last_try)
          if (last_verb == "GET") {
            writeBin(resp$content, target)
            write_etag_for_path(target, get_etag_from_response(resp))
          } else {
            Sys.setFileTime(target, Sys.time())
          }
        } else {
          try_next()
        }
      },
      fail = function(err = "no urls specified") {
        result[[which]] <<- make_download_error(err)
        try_next()
      }
    )
    
    try_next <- function() {
      if (last_try == length(downloads[[which]]) && last_verb == "GET") {
        return()
      }
      if (last_verb == "GET") last_try <<- last_try + 1
      
      h <- new_handle()
      url <- downloads[[which]][[last_try]]
      if (last_verb == "GET" && !is.na(etag <- etags[[which]][[last_try]])) {
        last_verb <<- "HEAD"
        expected_etag <<- etag
        handle_setopt(h, customrequest = "HEAD", nobody = TRUE)
      } else {
        last_verb <<- "GET"
        expected_etag <<- NULL
      }
      
      curl_fetch_multi(url, done = callbacks$done, fail = callbacks$fail,
                       pool = pool)
    }
    
    is_success <- function(resp) {
      if (resp$status_code != 200) return(FALSE)
      if (is.null(expected_etag)) return (TRUE)
      etag_new <- get_etag_from_response(resp)
      identical(etag_new, expected_etag)
    }
    
    shedule_next_http <- function(try) {
      h <- new_handle()
      if (!is.na(etags[[which]][[try]])) {
        expected_etag <<- etags[[which]][[try]]
        handle_setopt(h, customrequest = "HEAD", nobody = TRUE)
        
      } else {
        expected_etag <<- NULL
      }
    }
    
    callbacks
  }
  
  pool <- new_pool()
  for (d in seq_along(downloads)) make_callbacks(d)$fail()
  
  multi_run(pool = pool)
  structure(result, names = names(downloads))
}

get_etags_for_downloads <- function(downloads) {
  etags <- vector(mode = "list", length(downloads))
  targets <- get_targets_for_downloads(downloads)
  for (i in seq_along(downloads)) {
    e <- vapply(targets[[i]], get_etag_for_path, character(1))
    etags[[i]] <- rep_len(e, length(downloads[[i]]))
  }
  etags
}

get_target <- function(downloads, which, try) {
  if (is.null(names(downloads[[which]]))) {
    names(downloads)[which]
  } else {
    names(downloads[[which]])[try]
  }
}

get_targets_for_downloads <- function(downloads) {
  lapply(seq_along(downloads), function(i)  {
    if (is.null(names(downloads[[i]]))) {
      names(downloads)[i]
    } else {
      names(downloads[[i]])
    }
  })
}

get_etag_from_response <- function(resp) {
  line <- grep("^etag:", ignore.case = TRUE, parse_headers(resp$headers),
               value = TRUE)
  sub("^etag:[ ]*", "", line, ignore.case = TRUE)
}

get_etag_file <- function(path) {
  file.path(dirname(path), "_cache", paste0(basename(path), ".etag"))
}

get_etag_for_path <- function(path) {
  ## there is a warning if the file does not exist
  tryCatch(
    suppressWarnings(readLines(get_etag_file(path))[[1]]),
    error = function(e) NA_character_
  )
}

write_etag_for_path <- function(path, etag) {
  etag_file <- get_etag_file(path)
  dir.create(dirname(etag_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(etag, etag_file)
}

make_download_error <- function(msg) {
  structure(
    list(message = msg, call = NULL),
    class = c("download_error", "error", "condition")
  )
}  

#selenium-----
library(RSelenium)
selServ <- selenium(verbose = FALSE)
selServ$process
selServ$log(iedrver = "latest")
RSelenium::checkForServer()
rsDriver(Name = "firefox")

remDr <- remoteDriver$new()
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
remDr$open()

remote
#untar-----
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

#merge(tbl,googledmerge,by.x="linkbase",by.y="name",all.x=T,all.y=F,sort=T)%>%
#googledmerge has the name as it appeared in the .net link so this works
#so, if I put files in the google drive with titles that match whats in the yby (equibalend to tbl above), then i can do this same merge
#therefore, first step is get all the files from the dropbox out of their hierarchy


#tarf is
pdf_links<-"Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres.csv"%>%
  read.csv(encoding="UTF-8")
#you will need this in a little bit as the think to merge onto

setwd("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads")
list.files("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads",recursive = T)%>%
  file.rename(.,basename(.))

setwd("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads/2016/12")

"13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"%>%
  gsub("\\.pdf$","-pdf",.)%>%
  tolower()%>%
  file.rename(from=paste0("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads/13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"),
              to=paste0(getwd(),"/",.))

#works dont fuck
file.rename(from=paste0("C:/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/Publications/Forestproductivitycoop.net docs/untar/dlm_uploads/13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"),
            to=paste0(getwd(),"/",tolower(gsub("\\.pdf$","-pdf","13-Nestor-Ria├▒o-Fisiologia-Egrandis.pdf"))))



filerename2=function(from){ 
  file.rename(from=from,
              to=paste0(getwd(),"/",tolower(gsub("\\.pdf$","-pdf",basename(from)))))
}
setwd("C:/Users/sabloszi")

filerename2((paste0(getwd(),"/",list.files(getwd()))))

googledmerge2<-googledmerge%>%
  mutate(.,name=gsub("-pdf\\.pdf$","-pdf",name)  )

f2<-merge(pdf_links,googledmerge2,by.x="linkbase",by.y="name",all.x=T,all.y=F,sort=T)%>%
  mutate(., glink=paste0("https://drive.google.com/open?id=",id))#%>%


f2%>%write.csv(.,file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres4.csv")




drive
#source of older pdfs:
#C:\Users\sabloszi\Dropbox (FPC)\FPC Team Folder\Publications\Forestproductivitycoop.net docs\untar\dlm_uploads
#pick up merging the yby (pdf_links) with the googledmerge and then get that into that main meetingshtmltargest.gsheet thing. 

#

goo<-read.csv("C:/Users/sabloszi/Desktop/meetingtablehtmllinkstargets.csv",header = T)%>%
  #names()%>%
  select(c(glink,indyby))%>%
  merge(f2,.,by="indyby",all=T,suffixes=c(".f2",".mt"))

wex(goo)
names(f2)
vecs <- list(
  c(NA, NA, 3, 22, 5),
  c(1, 2, NA, 33, 5)
  
)
coalesce(!!!vecs)

pdf_links%>%
  subset(.,grepl("google",glink.mt))%>%
  dim()

mutate(.,x=dplyr::coalesce(glink.mt,glink.f2))%>%
  write.csv(.,file="Q:/Shared drives/FER - FPC Team/Website/Htmltables/Events-past-yearbyear-pres2.csv")

#keep only certain columns in a list works dont mess with this delete-----
#auto
lis=list(mtcars,chickwts)
#get list of names by df, this is the mgt columns
nms2kp<-list(mtcars=c("mpg","cyl"),chickwts=c("weight"))
#add more names to keep, this is like the merge columns
nms2kpj<-list(mtcars=c("disp","drat"),chickwts=c())
nms2kp<-lapply(names(nms2kp),function(i){
  nms2kp[[i]]<-c(nms2kp[[i]],nms2kpj[[i]])
})


#make the list names a separate thing for dbfl
names(lis)=c("mtcars","chickwts")
names(nms2kp)=c("mtcars","chickwts")
dbfllsnms<-names(lis)
lis<-lapply(names(lis),function(i){
  lis[[i]]<-select(lis[[i]],nms2kp[[i]])
})
names(lis)<-dbfllsnms


#gcrap-----
  

  wex()
  subset(.,is!=YEAR_EST_2)%>%
  
#Act_Date is great, it's always a date or NA. don't need Method (which is herb method) or Activity (alwayus Release)
#FERT_YEAR is great, it's always a date or NA. it's never a year when FERT_METH is not na and vice versa
#THIN_YEAR is great, it's always a date or NA. it's never a year when THIN_TYPe is not na and vice versa

  
  
    dim()
  Method Act_Date
Year_Est
YEAR_EST_2

  wex()


wael("./Workspace_Sean/GIS/Files/Company_G")

#randos-----------
dbfcf<-list.files(path ="./Workspace_Sean/viewingdelete/")
#make a list (takes like 5 sec on home pc, 1 mississippi sec on workpc)
dbfcf<-dbfcf[-12]
dbflf<-lapply(dbfcf,function(x){read.csv(paste0(getwd(),"/Workspace_Sean/viewingdelete/",x),colClasses = "num2")})
names(dbflf)<-dbfcf%>%gsub("\\.csv","",.)

lapply(dbflf,dim)

set.seed(6660)
subs<-read.csv("silv.csv")%>%
  mutate(.,co=substr(compID,1,3))%>%
split(.,.$co)%>%
lapply(.,function(x)
{slice_sample(x,n=5)}
  )

rbindlist(subs,idcol = "co")%>%
  wex()
#github-----
setwd("Q:/My Drive/Studies/FPC/Scripts")