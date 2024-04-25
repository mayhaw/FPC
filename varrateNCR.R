#Introduction/readme-----
#Section 1.1-1.n is more or less raw data steps; no subsettting is done to remove dead or shrinking trees etc; but a correction based on a miscalibrated vertex in 2022 is applied
#Section 1.1-1.n ... and this section is also meant to merge to GIS layers so has already been qc'ed as much as possible for typos from paper sheets etc
#Section 2.1.2.n is for doing stats and making figures etc
#1.1. read in data------------------------------------------------------------------------------------------------------------------------------------
setwd("Q:/Shared drives/FER - FPC Team/RegionWide Trials/RW29 Variable Rate x Herb/NC/Tabular_Data")
#then get mgt and plot info read in
mgt<-read.csv("management-data-VarRate-2022-groundplotsonly.csv",header=T)%>%
  select(STDY,PLOT,fert_FINAL,Herb,fert_rx,Shape_Area)


#then get the meat of the data read in_-------  
vrtlf<-
  read.csv("cruising-data-VarRate.csv",header = T)%>%
  merge(mgt,.,by=c("STDY","PLOT"))%>%
  mutate(.,HT=as.numeric(HT))%>%
  mutate(.,HTLC=as.numeric(HTLC))%>%
  mutate(.,DBH=as.numeric(DBH))%>%
  select(c(STDY,PLOT,YST,TREE_No,DBH,HTLC,HT,Comments,DAM,MORT,Shape_Area,fert_FINAL,Herb,fert_rx))%>%#DBH,HTLC,DAM,MORT,Comments))%>%
  mutate(.,CrLe=HT-HTLC)%>%
  mutate(ba=(pi*((DBH/2)^2))/144)%>% # one tree basal areal also note its converteing from in to feet^2 since all futher use needs to be in ft2
  mutate(ba=ba/((Shape_Area/10000)*2.471))%>% #get ba in sqft per plot acres (same as vol also convert from sq m Shape_Area to ac)
  mutate(.,vol=ifelse(STDY==291207, #this gives you volume per tree
                      (0.21949+(0.00238 * DBH * DBH * HT)), #true, unthined, Units will be vol in ft3/tree, dbh in inches, ht in ft.  
                      (0.25663+(0.00239 * DBH * DBH * HT))))%>% #false, thinned 
  mutate(.,vola=vol/((Shape_Area/10000)*2.471))%>% #get volume per acre but convert sq meters to ha and then ha size in to ac bc 10000 sq m in a ha and 2.471 ac in a ha. the Shape_area is just the area of the trees plus a 2m buffer around that
  arrange(STDY,PLOT,TREE_No,YST,fert_FINAL)%>%
  group_by(STDY,PLOT,TREE_No)%>% 
  mutate(.,DBHg=DBH-lag(DBH))%>%
  mutate(.,HTg=HT-lag(HT))%>%
  mutate(.,CrLeg=CrLe-lag(CrLe))%>%
  mutate(.,volg=vol-lag(vol))
                      
vrtlfg<-
  vrtlf%>%
subset(.,YST==1)%>%
  group_by(STDY,PLOT,Herb,fert_FINAL,fert_rx)%>% #only really need to group_by STDY and PLOT but this is a lazy way to keep the trts
  summarise(
  #1st mean measurements
    CrLeg=mean(CrLeg,na.rm=T),
    DBHg=mean(DBHg,na.rm=T),
    HTg=mean(HTg,na.rm=T),
    bam=mean(ba,na.rm=T),
    volgm=mean(volg,na.rm=T),
  #2nd sum measurements
    bas=sum(ba,na.rm=T),
    volgs=sum(volg,na.rm=T))%>%#
  mutate(.,fert_FINAL=as.factor(fert_FINAL))%>%
  mutate(.,STDY=as.factor(STDY))%>%
  mutate(.,nh=interaction(fert_FINAL,Herb))

#get means per trt combo for graphs
herbmns<-
  vrtlfg%>%
  group_by(fert_FINAL,Herb)%>%
  summarise(CrLeg=mean(CrLeg,na.rm=T),
            DBHg=mean(DBHg,na.rm=T),
            volgm=mean(volgm,na.rm=T),
            bam=mean(bam,na.rm=T),
            volgs=mean(volgs,na.rm=T),
            bas=mean(bas,na.rm=T),
            HTg=mean(HTg,na.rm=T))

#graphs 
vrtlfg<-
  vrtlfg%>%
    mutate(.,fert_FINAL=factor(fert_FINAL,levels=c("0","100","200","250","300"),labels=c("0","100","200","250","300")))%>%
      arrange(fert_FINAL,STDY,PLOT)
    head()

  ggplot(vrtlfg,aes(as.factor(fert_FINAL), HTg, group=nh)) +
  geom_line(data = herbmns, aes(group=Herb,linetype=Herb), size=1,
            position = position_dodge(width=0.15)) +
  geom_point(aes(color=Herb,group=nh,shape=STDY),
             position = position_dodge(width=0.15),size=4)
  #geom_point(data = summh, aes(group=MAJ_ARE), colour="blue",size=3, 
  #          position = position_dodge(width=0.15)) +
  ylab(expression(paste("Basal area growth (",ft^2, " " , tree^-1, yr^-1,")")))+  
  xlab(expression(paste("Fertilizer (lbs. N ",ac^-1,")")))+  
  
  theme_bw() + 
  theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
        axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
        legend.text=element_text(size=16))+
  labs(shape="Plot mn.: Stand",color="Plot mn.: Weed ctrl.",linetype="Overall N x H means")+
  # scale_color_viridis(discrete = TRUE,option="D",guide="none") +
  scale_colour_manual(values=cbPalette)+
  guides(colour = guide_legend(override.aes = list(shape = 9),order=1),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 3))+
  theme(axis.text.x = element_text(size = 14))

  
  
  
  

#1.3.2. now export it-----
if("you want to overwrite the main rw29 clean data for GIS etc"==T){
write_xlsx(
  x=list(cruising=vrtl),
  path = "cruising-data-VarRate.xlsx",#this is the final merged 2022/2023/20___ tree data
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)
  }
#2.1. calc'e growth, remove any negative growth whatsoever for now, and do volume calcs----

names(vrt)<-gsub("_","",names(vrt)) #added underscore further up for other reasons
vrt<-    
  vrt%>%
rename("Htftwrong22in22"=Htftwrong22in22, "Htlcftwrong22in22"=Htlcftwrong22in22)%>%
  mutate(.,Htft=Htft23-Htft22)%>%
  mutate(.,Htlcft=Htlcft23-Htlcft22)%>%
  mutate(.,Dbhin=Dbhin23-Dbhin22)%>%
  mutate(.,CrownLength23=Htft23-Htlcft23)%>%
  mutate(.,CrownLength22=Htft22-Htlcft22)%>%
  mutate(.,CrownLength=CrownLength23-CrownLength22)%>%
  mutate(.,CrownLength=ifelse(CrownLength<0,NA,CrownLength), #wow ~328 negs here
         Dbhin=ifelse(Dbhin<0,NA,Dbhin), #only 34 negs here
         Htlcft=ifelse(Htlcft<0,NA,Htlcft), #~289 negs here
         Htft=ifelse(Htft<0,NA,Htft))%>% #~129 negs here
  mutate(.,v22=ifelse(site==139, #this gives you volume per tree
                  (0.21949+(0.00238 * Dbhin22 * Dbhin22 * Htft22)), #true, unthined, Units will be vol in ft3/tree, dbh in inches, ht in ft.  
                  (0.25663+(0.00239 * Dbhin22 * Dbhin22 * Htft22)) #false, thinned 
  ))%>%
  mutate(.,v23=ifelse(site==139, #this gives you volume per tree
                    (0.21949+(0.00238 * Dbhin23 * Dbhin23 * Htft23)), #true, unthined, Units will be vol in ft3/tree, dbh in inches, ht in ft.  
                    (0.25663+(0.00239 * Dbhin23 * Dbhin23 * Htft23)) #false, thinned 
  ))%>%
  mutate(.,site_rate=paste0(site,"_",rt))%>%
#  mutate(.,site_herb=paste0(site,"_",herb))%>%
  mutate(v=v23-v22)


#2.2. basal area math ----
vrt <-
  vrt%>%
  #mutate(basalarea22w=.005454*Dbhin22^2)%>% # same thing but less clear as below
  #mutate(basalarea23w=.005454*Dbhin23^2)%>% #  same thing but less clear as below
  mutate(basalarea22=(pi*((Dbhin22/2)^2))/144)%>% # also note its converteing to feet^2 since all futher use needs to be in ft2
  mutate(basalarea23=(pi*((Dbhin23/2)^2))/144)%>%# also note its converteing to feet^2 since all futher use needs to be in ft2
  mutate(basalarea=basalarea23-basalarea22) #basal area increment per tree in ft^2



#2.3. GET RID OF DAMAGED (SHRINKING) TREES from vrt-----
  #so get rid of physical damaged trees and dead trees
vrt<-  vrt%>%
  subset(.,!(d23%in%c("D","M","O","P","U")|d22%in%c("D","M","O","P","U")))%>% #get rid of any trees that would actually shrink - there are ~20 that had dieback, bent over etc
  subset(.,(m23=="A"&m22=="A")) #only want trees that were alive both years (no deads in 2022 but ~7 died by 2023)



#2.4. Add in lai data (you can run through 1.1 to here at least)=====
#Needed to fully show lai data-
#options(max.print=1000000)

#old version of lai final, delete later probably
  #vrt_lai_final <- list(vrt_2020_DUnder, vrt_2020_SRCLAI, vrt_2021_DUnder, vrt_2021_SRCLAI,vrt_2022_DUnder, vrt_2022_SRCLAI, vrt_2023_DUnder, vrt_2023_SRCLAI)
  #Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

#code for getting lai information labled and combined
vrt_2020_DUnder <- vrt_2020_DUnder %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2020_DUnder', 'vrt_2020_DUnder'))
vrt_2020_SRCLAI <- vrt_2020_SRCLAI %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2020_SRCLAI', 'vrt_2020_SRCLAI'))
vrt_2021_DUnder <- vrt_2021_DUnder %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2021_DUnder', 'vrt_2021_DUnder'))
vrt_2021_SRCLAI <- vrt_2021_SRCLAI %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2021_SRCLAI', 'vrt_2021_SRCLAI'))
vrt_2022_DUnder <- vrt_2022_DUnder %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2022_DUnder', 'vrt_2022_DUnder'))
vrt_2022_SRCLAI <- vrt_2022_SRCLAI %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2022_SRCLAI', 'vrt_2022_SRCLAI'))
vrt_2023_DUnder <- vrt_2023_DUnder %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2023_DUnder', 'vrt_2023_DUnder'))
vrt_2023_SRCLAI <- vrt_2023_SRCLAI %>%
  mutate(LAI_YEAR_TYPE= if_else(.$OBJECTID > 27, 'vrt_2023_SRCLAI', 'vrt_2023_SRCLAI'))

vrt_2020_DUnder <- vrt_2020_DUnder %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2020', '2020'))
vrt_2020_SRCLAI <- vrt_2020_SRCLAI %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2020', '2020'))
vrt_2021_DUnder <- vrt_2021_DUnder %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2021', '2021'))
vrt_2021_SRCLAI <- vrt_2021_SRCLAI %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2021', '2021'))
vrt_2022_DUnder <- vrt_2022_DUnder %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2022', '2022'))
vrt_2022_SRCLAI <- vrt_2022_SRCLAI %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2022', '2022'))
vrt_2023_DUnder <- vrt_2023_DUnder %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2023', '2023'))
vrt_2023_SRCLAI <- vrt_2023_SRCLAI %>%
  mutate(Year= if_else(.$OBJECTID > 27, '2023', '2023'))


vrt_lai1<-merge(vrt_2020_DUnder,vrt_2020_SRCLAI,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai2<-merge(vrt_lai1,vrt_2021_DUnder,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai3<-merge(vrt_lai2,vrt_2021_SRCLAI,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai4<-merge(vrt_lai3,vrt_2022_DUnder,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai5<-merge(vrt_lai4,vrt_2022_SRCLAI,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai6<-merge(vrt_lai5,vrt_2023_DUnder,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))

vrt_lai_final<-
  merge(vrt_lai6,vrt_2023_SRCLAI,all=T,by=c("Name","OBJECTID","ZONE_CODE","COUNT","AREA","MEAN","LAI_YEAR_TYPE","Year"))%>%
    mutate(.,LAI_YEAR_TYPE=gsub("[^SRCDU]","",LAI_YEAR_TYPE ))
#temporary lai csv saved to same folder as this R file
#vrt_lai_final%>%
#  merge(.,vrt_grdplt[,c("Name","MAJ_ARE","fert_rx","N_lbac")],by="Name",all.x=T)%>%
#  write.csv(.,file="Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/R/varrateNCR_LAI_delete.csv")

 

#and keep just the lai collumns for vrt_grdplts itself
vrt_lai_final<-vrt_lai_final%>%
  select(c(Name,MEAN,LAI_YEAR_TYPE,Year))%>%
  cast(.,Name~LAI_YEAR_TYPE+Year,value="MEAN")

#I don't know what this is, but it is not working.
#vrt_lai_final<-vrt_lai_final%>%
  #select(-c(OBJECTID,ZONE_CODE,COUNT,AREA))%>% #git rid of stuff from GIS thats not helpful or AREA which is elsewhere in vrt
  #mutate(.,LAI_YEAR_TYPE=gsub("vrt_20","y",LAI_YEAR_TYPE))%>%
  #cast(Name~LAI_YEAR_TYPE,value="MEAN")

#Sean moved this line to the plot scale data set since it's not lai by tree: vrt<-merge(vrt,vrt_lai_final,all=T,by=c("Name"))

#2.4.1. Make minimal data set to send QCto RWDB etc----
#3.1. merging plot numbers to main.RW29_NCVarRate_MasterPlotPlan ----
library(rgdal)# for st_read and ogrListLayers
library(readxl)
setwd("Q:/Shared drives/FER - FPC Team/RegionWide Trials/RW29 Variable Rate x Herb")
#see what's in the main newish (as of 10/2023) gpkg
ogrListLayers(paste0(getwd(),"/NC/GIS/RW29_NCVarRate_allLayers.gpkg"))
#Get the main measurement plot layer
meas<-st_read(paste0(getwd(),"/NC/GIS/RW29_NCVarRate_allLayers.gpkg"),
              layer = "RW29_NCVarRate_IntMeasPlots"        )%>%
  as_tibble()
trt<-st_read(paste0(getwd(),"/NC/GIS/RW29_NCVarRate_allLayers.gpkg"),
             layer = "RW29_NCVarRate_MasterPlotPlan"        )%>%
  unique()%>%
  as_tibble()

meas%>%
  select(SUB_ID)
#these are the columns of old factor level names and what new level names they correspond to by factor
level_match<-read_excel(paste0(getwd(),"/RW29_VarRate_Trtcodes_5-24-2023.xlsx"),
                        sheet="current treatment levels",
                        trim_ws = T )
pln<-read_excel(paste0(getwd(),"/RW29_VarRate_Trtcodes_5-24-2023.xlsx"),
                sheet="planned treatment codes",
                trim_ws = T )%>%head(20)
#this simple function was a way to rename the levels of variable really explicityl 
cbmgtnms<- function(these,df){#combine management with similar ones "1/0 is it a date?" column
  #df<-df[df$company==dfcompany,]
  reduce2(df$old, df$new,  .init = these, str_replace)
}

#allcodes<-
trt%>%
  select(c(ResrcID,GID,Herb,fert_FINAL,fert_rx))%>%
  as.data.table()%>%
  melt.data.table(id.vars=c("ResrcID","GID"))%>%
  mutate(.,value=cbmgtnms(these=value,df=level_match))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  unique()%>% #there are two kinds of duplicates of GIDs- by grid squares that are split by roads etc and b/c some grid squares are stacked one on top of another
  head()#pick up here figuring out what to do about herbs that go across squares as y and n
cast(.,ResrcID+GID~variable)%>%
  merge(.,pln,by.x=c("Herb" ,"fert_FINAL",   "fert_rx"),
        by.y=c("Herbicide (Y/N)","Application Rate", "Application method"),all=T)%>%
  select(c(ResrcID, GID,'Treatment code',Herb, fert_FINAL,   fert_rx       ))%>%
  subset(!is.na(`Treatment code`))%>%# these are neighbors so dont get their own PLOT number in the end
  arrange(ResrcID,GID)%>% #decided to preserve the alplanumeric order the ResrcID and then GIDs were already in...
  group_by(ResrcID,`Treatment code`)%>% #... and then to have the numbers count up within a RescrID X Treatment code combo
  mutate(REP=10:(n()+9))%>%#make rep numbers that start over in each STDY
  ungroup()%>% #grouping again by different thing next so ungrou
  group_by(ResrcID) %>% mutate(STDY = 291200+cur_group_id())%>% #tim said use 12 for manulife
  mutate(.,PLOT=(100*REP)+as.integer(`Treatment code`))%>% #make plot numbers
  arrange(ResrcID,`Treatment code`,PLOT)

#Q:\Shared drives\FER - FPC Team

pathz="RW29_VarRate_Trtcodes_5-24-2023.xlsx"
pathz%>%
  excel_sheets()%>%
  set_names()%>%
  purrr::map(read_xlsx,path=pathz)%>%
  c(.,list(allcodes=allcodes))%>%#be careful bc this allcodes=allcodes is set to overwrite whatever is in the sheet named allcodes
  write_xlsx(
    .,
    path = "RW29_VarRate_Trtcodes_5-24-2023.xlsx",
    col_names = TRUE,
    format_headers = TRUE,
    use_zip64 = FALSE
  )


#3.2. QC on RW29_NCVarRate_allLayers.gpkg-----  
#in RW29_NCVarRate_MasterPlotPlan there are lots of duplicates, like you click on a polygon and there are four identical one on top of the other with all the same columns ; the only column that differentieates them is fid. fid doesnt show up with st read as is so you can use unique to get rid of the dups
#Note there are at least some trees in 139's control N ground plot that are in a neighbor plot that we'll have to manually asign the PLOT to if I use the fert_rx to assign levels since those arent' in the "planned treatment codes" table
#Ok so if we go with the allcodes join, can we just assign the trees plot numbers based on the allcodes PLOT theyre in?
#113 (124): yes this ones easy, only a few trees span two SUBIDs and then theyre both exact same mgt just two reps and none are in neighbors
#55       : yes this ones easy, no         trees span two SUBIDs and                                               and none are in neighbors
#139      : the most effed bc a bunch of trees are in a neighbor that doesnt get a PLOT,; furthermore most of the trees are in a neighbor so .... cant do plot center or majority area or anything. also there are trees that are spanning an LAIbased and a random verson of the 100lb/ac herb trt
#127      :  there are trees that are spanning an LAIbased and a random verson of the 300lb/ac herb trt. there's another two sets of ground plots where they span two nextdoor instances of the same herbXmethodXfert treatment
#ok so see what these problems mean in terms of what the groundplot outlines got assigned for their SUBID; if they make sense go ahead and join the plot to the groundplot and then the groundplot to the trees in them and fuck off


#todo/fix things lines:------
#1.1 volume growht from 2022 to 20223 boxplots by the frt rate
#1.2 height growht from 2022 to 20223 boxplots by the frt rate
#1.3 dbh  growht from 2022 to 20223 boxplots by the frt rate
#2. need to get damage and mortalty codes of scanned pdfs for 2023 these plots and put them in the csv vrt23 is based off of :
  #plot site  rt
  #3  127 620
  #5  127 620
  #2  127 000


#----------------------------------------------------------------------------------------------------------
#3. check that this 2022 data is correct: (i don't see trees between 18 and 23 in this plot so i think tree 23 below probably goes to another plot)
#plot	site	rt	rep	tre	Htft	Dbhin	Htlcft	d	m	Notes	PLOT	percentage covered	date
#	55	516	5	23	69.5	4.75	39.2	A	a	RE	Rate-516_1-smit-55	25%	4/25/2022
#-----------------------------------------------------------------------------------------------------------



#4. See if for the plots where we measured height twice in 2022, do the two height measurements correlate? (lo priority. you can find them bc on the paper sheets/csv raw data there are duplicates of a few stand 55 plots)




#--------------------------------------------------------------------------------------
#5. fix this: there are two tree 5s in the 22 data this plot:
#plot	site	rt	rep	tre	Htft	Dbhin	Htlcft	d	m	Notes	PLOT	percentage covered	date
#	55	516	5	5	82.1	77	N/A	A	A	RE	Rate-516_1-smit-55	85%	4/12/2022
#	55	516	5	5	73.3	80.9	N/A 	A	A	RE	Rate-516_1-smit-55	85%	4/12/2022
#--------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------
#6. Fix this: there are two tree 18s in the 22 data this plot
#plot	site	rt	rep	tre	Htft	Dbhin	Htlcft	d	m	Notes	PLOT	percentage.covered	date
     #	55	0	   6	18	72.12146	NA	41.98142	N/A	n/a 	RE	Control_1-smit-55	N/A	4/25/2022
#	55	0	6	18	58.31066	NA	31.25774	N/A	n/a 		Control_1-smit-55	N/A	4/25/2022
#--------------------------------------------------------------------------------------------

#7. Figure out the problem from ctrlf+aa80939sa8na93t2

#8. come back to the figure and neaten it up, make a version for box plots

#9. figure out whats up with the >10ft growht and <-5ft growth
#"G:/Shared drives/FER - FPC Team/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data/cruising-data-VarRate-outliers.csv"
#plot, site, rt rep, tre ,  notes
#1, 55, rt 0, rep 6, tre 7,  >11 ft htft growth but cant find antying wrong #example line
#1, 55, rt 0, rep 6, tre 7, htft22 is obviously wrong but can't find anything typo-wise in the scanned copies etc


#htft22 =65
#htft23 =59
#dbh22= 8
#dbh23= 8.5
#htlct22 = 34
#htlct23= 34


#htft22 =22
#htft23 =59 
#dbh22= 8
#dbh23= 8.5
#htlct22 = 34
#htlct23= 34

#10 work with eli to get the herb for each plot; also get the lai/random fert assignment; maybe the parent material?  
#G:/Shared drives/FER - FPC Team/Variable Rate/NC/NC_Stands_HerbNoHerb_merged_wCLAIwFertPrescripFINALv2_Remove00121


#11. vrt%>%subset(is.na(PLOT)) at the step before merging vrt with the vrt_plots as of 7/14/23 there are 10 new trees in 2023 i think that 
#... for some reason dont get plot names and need to merge those right so they get plot names i guess


#packages ---------

#Only install these if your computer dosnt have them
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("tidyr")
#nstall.packages("Hmisc")
#install.packages("readxl")
#rm(list=ls())

library("magrittr")
library("ggplot2")
library("dplyr")
library("tidyr") #for fill() function
library("Hmisc")
library("readxl")

wex<- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file="clipboard-32768",sep="\t",row.names=row.names,col.names=col.names,...)  } 


#section for the plot scale based data: WIDE-----

pn=function(x){sum(x<0,na.rm=T)/length(x)}#needed to get proportion negative growth metrics by plot
vrt_grdplt<-
  vrt%>%
  merge(.,dplyr::summarize(group_by(.,Name), 
                           #2022
                           v22_m=mean(v22,na.rm=T), #mean volume per tree, useful on its own
                           v22_s=sum(v22,na.rm=T), #sum volume per plot so you can get on aerial basis later with the plot size as a denominator #note there is waaaay higher vol per plot and vol per area for 1206 because it never got thinned but this is correct. this is true for both 2016 and 2022
                           Htft22_m=mean(Htft22,na.rm=T), #mean ht
                           Dbhin22_m=mean(Dbhin22,na.rm=T), #mean dhb
                           basalarea22_m=mean(basalarea22,na.rm=T),#mean basalarea
                           basalarea22_s=sum(basalarea22,na.rm=T),#sum basalarea
                           CrownLength22_m=mean(CrownLength22,na.rm=T),#mean crownlength
                           #2023     
                           v23_m=mean(v23,na.rm=T), #mean volume per tree, useful on its own
                           v23_s=sum(v23,na.rm=T), #sum volume per plot so you can get on aerial basis later with the plot size as a denominator #note there is waaaay higher vol per plot and vol per area for 1206 because it never got thinned but this is correct. this is true for both 2016 and 2022
                           Htft23_m=mean(Htft23,na.rm=T), #mean ht
                           Dbhin23_m=mean(Dbhin23,na.rm=T), #mean dhb
                           basalarea23_m=mean(basalarea23,na.rm=T),#mean basalarea
                           basalarea23_s=sum(basalarea23,na.rm=T),#sum basalarea
                           CrownLength23_m=mean(CrownLength23,na.rm=T),#mean crownlength
                           #growth 
                           v_m=mean(v,na.rm=T), #mean volume per tree, useful on its own
                           v_s=sum(v,na.rm=T), #sum volume per plot so you can get on aerial basis later with the plot size as a denominator #note there is waaaay higher vol per plot and vol per area for 1206 because it never got thinned but this is correct. this is true for both 2016 and 2022
                           Htft_m=mean(Htft,na.rm=T), #mean ht
                           Htftprn=pn(Htft),
                           Htlcftprn=pn(Htlcft),
                           Dbhinprn=pn(Dbhin),
                           Dbhin_m=mean(Dbhin,na.rm=T), #mean dhb
                           basalarea_m=mean(basalarea,na.rm=T),#mean basalarea
                           basalarea_s=sum(basalarea,na.rm=T),#sum basalarea
                           CrownLength_m=mean(CrownLength,na.rm=T)),#mean crownlength
        
        by=c("Name"),all.y=T)%>%
  mutate(.,vm2=cumsum(!duplicated(Name)))%>%  
  #%>%histogram()
  subset(.,!duplicated(vm2))%>%
  mutate(.,va22=v22_s/((Shape_Area/10000)*2.471))%>% #get volume per acre but convert sq meters to ha and then ha size in to ac bc 10000 sq m in a ha and 2.471 ac in a ha
  mutate(.,va23=v23_s/((Shape_Area/10000)*2.471))%>% #get volume per acre but convert sq meters to ha and then ha size in to ac bc 10000 sq m in a ha and 2.471 ac in a ha
  mutate(.,va=v_s/((Shape_Area/10000)*2.471))%>% #get volume per acre but convert sq meters to ha and then ha size in to ac bc 10000 sq m in a ha and 2.471 ac in a ha
  mutate(.,ba22=basalarea22_s/((Shape_Area/10000)*2.471))%>% #get ba
  mutate(.,ba23=basalarea23_s/((Shape_Area/10000)*2.471))%>% #get ba
  mutate(.,ba=basalarea_s/((Shape_Area/10000)*2.471))%>% #get ba
  select(c(Name,site,MAJ_ARE,N_lbac,fert_rx,v22_m,v23_m,v_m,Htft22_m,Htft23_m,Htft_m,Dbhin22_m,Dbhin23_m,Dbhin_m,basalarea22_m,basalarea23_m,basalarea_m,CrownLength22_m,CrownLength23_m,CrownLength_m,
           va22,va23,va,ba22,ba23,ba,Htftprn,Htlcftprn,Dbhinprn) )#keep only per plot data


#add in lai data
vrt_grdplt<-vrt_grdplt%>%
  merge(.,vrt_lai_final,by="Name")
#section for the plot scale based data: LONG-----
vrt_grdpltl<-vrt_grdplt%>%   
  #you could add in change in lai 
  #mutate(.,chainge2021202=dunder20221-2022)
  melt.data.frame(id.vars=c("Name","site","MAJ_ARE","N_lbac","fert_rx"))%>%
  mutate(.,year=(gsub("[^0-9]+","",variable)))%>%
  mutate(.,variable=(gsub("[0-9]+","",variable)))%>%
  #wex()
  #mutate(.,year=ifelse(year%in%c(22,23),paste0("y",year),"growth"))%>%
    subset(.,grepl("DU",variable)|grepl("SR",variable))
#look at negative percentages--------------------  
#note you need to leave them back in the data for this to work

if((4>5)==T){#doesn't work 11/3/23
vrt_grdplt%>%
  ggplot()+
geom_vline(x=22)
}

if((3>4)==T){#d, just putting this here for now for qc of data, delete eventally
  #see how % negative growth relates to tree size
  
  vrt_grdplt%>%
    subset(.,grepl("Htft",variable))%>%
    cast(Name+site+MAJ_ARE+N_lbac+fert_rx~variable,value="growth")%>%
    
    ggplot(aes(x=Htft_m,y=Htftprn,color=site))+
    geom_point()+
    geom_label_repel(aes(label = Name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50')
  
  #see 23-22 correlations for all kinds of things as plot avergs  
  vrt_grdplt%>%
    subset(.,!(grepl("prn",variable)))%>%
    ggplot(aes(x=y22,y=y23))+
    geom_point()+
    geom_label_repel(aes(label = Name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') +
    facet_wrap("variable",scales = "free")
  
  #md
}

#main raw data qc for difference 22 to 23 dependent variables----------------------------------------------------------------------------------------------------

tree_vars(tvariable=Dbhin) #y axis

tree_vars<-function(tvariable){
  vrt%>%
    subset(.,abs(Htft)<=10)%>%
    ggplot(., aes(x=rt, y={{tvariable}})) +
    geom_point(alpha=0.4, position=position_jitter(height=0, width=0.2),size=9,aes(shape=as.factor(MAJ_ARE))) +
    #geom_boxplot(aes(color=(site)))+
        scale_colour_gradient(low = "yellow",high = "blue")+
    facet_grid(.~site)
  #stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar",  #For more on where the 68.3% comes from, here's some language form a random intro level stats class notes i ffound on line from a professor in the UK (doesnt need a link because the definition of 1 sd at 68% of the confidence interval is very standard for this particular distribution): he Gaussian distribution is symmetric about its mean value, so it follows that given the fraction of area within ?1?? is 68.3%, then the fraction of area outside of this interval is 31.7%. This is equally distributed above and below the 1?? interval. 
  #width=0.03, colour="red", alpha=0.7) +
  #stat_summary(fun=mean, geom="point", fill="blue", pch=21, size=4)
}

#choices
#Dbhin 22 or 23
#Htft 22 or 23
#Htlcft 22 or 23
tree_vars(tvariable=vrt_Specific_data_22)%>% #y axis
  ggsave(filename=	paste0("Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/figures/",
                         "vrt_Specific_data_23",".png")	,device="png",     width = 10, height = 6,plot=	. ,units="in"	)


vrt_Specific_data_23<-merge(vrt_plots,vrt23, by = c("plot","site","rt"), suffixes=c("s","23"))

#more raw data qc for correlatios 22 to 23 dependent variables--------------------------------------------------------------------------------------
#lets see how the two years data line up with eachother; ie, were tall trees in 2022 still tall in 2023 ? etc
vrt%>%
  select(Htft22,Htft23,Htft,Htlcft22,Htlcft23,Htlcft,Dbhin22,Dbhin23,Dbhin)%>%
PerformanceAnalytics::chart.Correlation(., histogram=TRUE, pch=30)

#ok now lets look at just the 2022-23 ht data to qc it:
vrt%>%# vrt$d23
  mutate(.,d23=substr(d23,1,1))%>%
  #subset(.,d23!="A"&d23!="I"&d23!="R")%>%
  subset(.,d23=="A"&Htft<0)%>%
  wex()
  #pull(d23)%>%unique()
  ggplot(.,aes(x=Htft22,y=Htft23,color=d23))+
  geom_abline(slope=1,intercept = 0)+
  geom_point(size=3)+
  scale_colour_manual(values=cbPalette)

vrt%>%pull(d23)%>%unique()
vrt%>%subset(site==55&rt==516)%>%wex()

#more raw data qc for wierdo growth -----
#Are there typos or transcription or copying erros from the raw paper sheets?
#read in michaels outlier csv:
read.csv("cruising-data-VarRate-outliers.csv",header = T)
#no not any more; there were a small handful as of 7/2023 but these have all fixed at least for any that were easy to catch because they made growth way too high or negative growth

#more raw data stuff for plot scale vrt_grdplts: fertXherb all at once-----

fud<-
  vrt_grdplt%>%
    mutate(.,N_lbac=as.factor(N_lbac))%>%
  melt.data.frame(.,id.cols=c("Name","Site","MAJ_ARE","N_lbac","fert_rx"))%>%
  subset(.,!grepl("prn",variable))%>%
  split(.,.$variable)%>%
  lapply(.,function(x){
    ggplot(x, aes(x=MAJ_ARE, y=value)) +
      geom_boxplot(aes(fill=N_lbac)) +
      geom_point(position=position_jitterdodge(jitter.width = .1),aes(group=N_lbac,shape=site))
    }
)

lapply(names(fud), 	function(x){ggsave(filename=paste0("Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/figures/grdplt/",x,".png"),
                                             width = 6, height = 4,plot=fud[[x]])})

#


  ggplot(.,aes(x=MAJ_ARE, y=v22_m,color=N_lbac))+
  geom_boxplot()+
  geom_point(aes(y=interaction(MAJ_ARE,N_lbac)))
#more raw data stuff for plot scale vrt_grdplts: all main effects, just areal growth-----
  setwd("Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/figures/grdplt/Maineffects/")
#fert
lm(ba~N_lbac,data=vrt_grdplt)%>%
    anova()
  a<-
      vrt_grdplt%>%
    mutate(.,N_lbac=as.factor(N_lbac))%>%
    ggplot(., aes(x=N_lbac, y=ba)) +
        geom_boxplot(aes(fill=N_lbac))+
        geom_point(position=position_jitterdodge(jitter.width = .4),size=5,color="orange",aes(group=N_lbac,shape=site,fill=as.factor(Htftprn)))+
        scale_x_discrete("Fertilizer N, lb ac^-1") +
    scale_y_continuous("Basal area growth, ft^2 ac^-1 yr^-1") +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
          axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
          legend.text=element_text(size=16))+
    labs(shape="Site")+
      annotate("text", x=1,y=7, label = "p = 0.0904")+
    scale_fill_viridis(discrete = TRUE,option="C",guide="none") 
  
  ggsave(a,filename="fertXsite_va.png",width=8,height=6)
#herb
#  vrt_grdplt$
  vrt_grdplt%>%
    lmer(Htft_m~N_lbac*MAJ_ARE+(1|site/MAJ_ARE), data=.,na.action = na.exclude)%>%
  anova()
  
    a<-
    vrt_grdplt%>%
    mutate(.,MAJ_ARE=as.factor(MAJ_ARE))%>%
    ggplot(., aes(x=MAJ_ARE, y=ba)) +
    geom_boxplot(aes(fill=MAJ_ARE))+
    geom_point(position=position_jitterdodge(jitter.width = .4),size=5,color="orange",aes(group=MAJ_ARE,shape=site,fill=as.factor(Htftprn)))+
    scale_x_discrete("") +
    scale_y_continuous("Basal area growth, ft^2 ac^-1 yr^-1") +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
          axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
          legend.text=element_text(size=16))+
    labs(shape="Site")+
      scale_fill_viridis(discrete = TRUE,option="C",guide="none") 
  
  ggsave(a,filename="herbXsite_ba.png",width=8,height=6)

#nova  for fert, ba
  lm(ba~N_lbac,data=vrt_grdplt)%>%
    anova()
#p = 0.0904
  
#  Then if they have an effect and you want to look at means separation
(lm(value~sec*vlv*pmp+jar,data=syr[syr$variable=="n2o",]))%>%
  emmeans(.,list(pairwise~sec:pmp),adjust="tukey")%>%
  CLD(.,
      alpha=0.05,
      Letters=letters,
      by="sec",
      adjust=F)  
        
#more raw data stuff for plot scale vrt_grdplts: all main effects, just areal growth but int-----
  
  
  
  #interaciton plots
  vrt_grdplt<-vrt_grdplt%>%
    mutate(sh=interaction(site,MAJ_ARE))%>%
    mutate(sb=interaction(site,fert_rx))
  #within site, interaction between herb and nlbac
  interaction.plot(x.factor = vrt_grdplt$N_lbac,trace.factor = vrt_grdplt$sh,response = vrt_grdplt$ba,type = "b")
  #within site, it almost looks like N helps ba
  interaction.plot(x.factor = vrt_grdplt$N_lbac,trace.factor = vrt_grdplt$site,response = vrt_grdplt$ba)
  #within site, it almost looks like N helps dbh
  interaction.plot(x.factor = vrt_grdplt$N_lbac,trace.factor = vrt_grdplt$site,response = vrt_grdplt$Dbhin_m)
  #herbicide helps only in stand 55, hurts in 113 and 127:
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$ba)
  #more fertilizer hurts basal area but idk why there arent more randoms above 200
  interaction.plot(x.factor = vrt_grdplt$N_lbac,trace.factor = vrt_grdplt$fert_rx,response = vrt_grdplt$ba)
  #Dbh mean growth herb effect inconsistent across stands 113 55
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$Dbhin_m) 
  #basalarea (non areal) mean growth herb effect inconsistent across stands 113 55
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$basalarea_m)
  #crowtnlenth mean growth herb effect inconsistent across stands 113
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$CrownLength_m)
  #v_m mean growth herb effect inconsistent across stands 113 55
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$v_m)
  #htft mean growth herb effect inconsistent across stands  113
  interaction.plot(x.factor = vrt_grdplt$MAJ_ARE,trace.factor = vrt_grdplt$site,response = vrt_grdplt$Htft_m)
#for the next two not sure why 127 wasnt showing the 0 but i split them up to get all the levels
  #within site, no clear effect of n
  with(vrt_grdplt%>%subset(.,site%in%c(127)),
  interaction.plot(x.factor = N_lbac,trace.factor = site,response = Dbhin_m))
  #within site, no clear effect of n for 139.
  with(vrt_grdplt%>%subset(.,site%in%c(139,55,113)),
       interaction.plot(x.factor = N_lbac,trace.factor = site,response = Dbhin_m)  )
  
  setwd("Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/figures/grdplt/interactions/")
#herb ggplot interaciton plot
  summh <- ddply(vrt_grdplt, .(N_lbac, sh), summarise, v_m = mean(v_m))%>%
    cSplit(.,"sh",sep=".",drop=F)

summh <- ddply(vrt_grdplt, .(N_lbac, MAJ_ARE), summarise, v_m = mean(v_m))%>%
    cSplit(.,"sh",sep=".",drop=F)
  
  
#fertilizer by site by herb int plot  
a<-ggplot(vrt_grdplt, aes(as.factor(N_lbac), v_m, group=MAJ_ARE)) +
    geom_point(aes(shape=site,color=MAJ_ARE,group=sh),
               position = position_dodge(width=0.15),size=3) +
    geom_point(data = summh, aes(group=sh,shape=as.factor(sh_1)), colour="blue",size=3, 
               position = position_dodge(width=0.15)) +
    geom_line(data = summh, aes(group=sh,linetype=sh_2), size=1,
              position = position_dodge(width=0.15)) +
    scale_x_discrete("Fertilizer N, lb/ac") +
    scale_y_continuous("Volume growth, ft^3 yr^-1 tree^-1") +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
          axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
          legend.text=element_text(size=16))+
    labs(shape="Site",color="Herbicide plots",linetype="Herbicide mean")
ggsave(a,filename="fertXsiteXherb_v_m.png",width=8,height=6)

#fertilizer by herb int plot  , v
summh <- ddply(vrt_grdplt[vrt_grdplt$site!=139,], .(N_lbac, MAJ_ARE), summarise, v_m = mean(v_m))%>%
  mutate(.,nh=interaction(N_lbac,MAJ_ARE))

vrt_grdplt<-vrt_grdplt%>%
  mutate(.,nh=interaction(N_lbac,MAJ_ARE))

a<-
ggplot(vrt_grdplt[vrt_grdplt$site!=139,], aes(as.factor(N_lbac), v_m, group=nh)) +
  geom_point(aes(shape=site,color=MAJ_ARE,group=nh),
             position = position_dodge(width=0.15),size=3) +
  geom_point(data = summh, aes(group=MAJ_ARE,shape=as.factor(MAJ_ARE)), colour="blue",size=3, 
             position = position_dodge(width=0.15)) +
  geom_line(data = summh, aes(group=MAJ_ARE,linetype=MAJ_ARE), size=1,
            position = position_dodge(width=0.15)) +
  scale_x_discrete("Fertilizer N, lb/ac") +
  scale_y_continuous("Volume growth, ft^3 yr^-1 tree^-1") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
        axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
        legend.text=element_text(size=16))+
  labs(shape="Site",color="Herbicide plots",linetype="Herbicide mean")
ggsave(a,filename="fertXherb_v_m.png",width=8,height=6)

#fertilizer by herb int plot  , bam
summh <- ddply(vrt_grdplt[vrt_grdplt$site!=139,], .(N_lbac, MAJ_ARE), summarise, basalarea_m = mean(basalarea_m))%>%
  mutate(.,nh=interaction(N_lbac,MAJ_ARE))

vrt_grdplt<-vrt_grdplt%>%
  mutate(.,nh=interaction(N_lbac,MAJ_ARE))


a<-
  ggplot(vrt_grdplt[vrt_grdplt$site!=139,], aes(as.factor(N_lbac), basalarea_m, group=nh)) +
  geom_line(data = summh, aes(group=MAJ_ARE,linetype=MAJ_ARE), size=1,
            position = position_dodge(width=0.15)) +
  geom_point(aes(shape=site,color=MAJ_ARE,group=nh),
             position = position_dodge(width=0.15),size=4) +
  #geom_point(data = summh, aes(group=MAJ_ARE), colour="blue",size=3, 
   #          position = position_dodge(width=0.15)) +
  ylab(expression(paste("Basal area growth (",ft^2, " " , tree^-1, yr^-1,")")))+  
  xlab(expression(paste("Fertilizer (lbs. N ",ac^-1,")")))+  
  
  theme_bw() + 
  theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
        axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
        legend.text=element_text(size=16))+
  labs(shape="Plot mn.: Stand",color="Plot mn.: Weed ctrl.",linetype="Overall N x H means")+
 # scale_color_viridis(discrete = TRUE,option="D",guide="none") +
  scale_colour_manual(values=cbPalette)+
  guides(colour = guide_legend(override.aes = list(shape = 9),order=1),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 3))+
  theme(axis.text.x = element_text(size = 14))
a
  ggsave(a,filename="fertXherb_basalarea_m.png",width=8,height=6)

vrt_grdplt[vrt_grdplt$site!=139&vrt_grdplt$N_lbac!=100,]%>%
  mutate(.,N_lbac=as.factor(N_lbac))%>%
  lmer(basalarea_m~N_lbac*MAJ_ARE+(1|site/MAJ_ARE), data=.,na.action = na.exclude)%>%
  #anova()
  lsmeans(.,pairwise~N_lbac,adjust="none")%>%#pairwise~samp*year,
  cld(.,
      #by="year",
      alpha=0.05,
      Letters=letters,      ### Use lower-case letters for .group
      adjust="none",       ### Tukey-adjusted or bonferroni comparisons show all a's (i.e. no means difference detected) #"G:\My Drive\Library\Graphpad_multiple_comparisons.pdf" shows how to do these without adjustments/what to call them etc
      rev=T)

#  Pr(>F)
#  N_lbac         0.2799
#  MAJ_ARE        0.9233
# N_lbac:MAJ_ARE 0.6984


cbPalette <- c( "#D55E00", "#0072B2","#000000", "#A92224","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

 










    #boutique ggplot interaciton plot
vrt_grdplt<-vrt_grdplt%>%
    mutate(.,fert_rx=factor(fert_rx,levels=c("boutique","control","random"),labels=c("LAI-based","Control","Random")))
    
  summb <- ddply(vrt_grdplt, .(N_lbac, sb), summarise, ba = mean(ba))%>%
    cSplit(.,"sb",sep=".",drop=F)%>%
    mutate(.,sb_2=factor(sb_2,levels=c("boutique","control","random"),labels=c("LAI-based","Control","Random")))
  
  #fertilizer by site by fert_rx int plot  
a<-vrt_grdplt%>%
  ggplot(., aes(as.factor(N_lbac), ba, group=fert_rx)) +
    geom_point(aes(shape=site,color=fert_rx,group=sb),
               position = position_dodge(width=0.15),size=3) +
    geom_point(data = summb, aes(group=sb,shape=as.factor(sb_1)), colour="blue",size=3, 
               position = position_dodge(width=0.15)) +
    geom_line(data = summb, aes(group=sb,linetype=sb_2), size=1,
              position = position_dodge(width=0.15)) +
    scale_x_discrete("Fertilizer N, lb/ac") +
    scale_y_continuous("Basal area, ft^2 ac^-1 yr^-1") +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
          axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
          legend.text=element_text(size=16))+
    labs(shape="Site",color="Fert. RX plots",linetype="Fert. RX mean")
ggsave(a,filename="fertXsiteXrx_ba.png",width=8,height=6)  

  #note theres not a lot of obs for boutique to do a regression on groundplots:
  vrt_grdplt%>%select(c(site,fert_rx,N_lbac))%>%cast(site~fert_rx,fun.aggregate = function(x){length(unique(x))})
  #because only stand 113 has 2 different LAI based fertilizer rates
#site boutique control random
#55        1       1      1
#113        2       1      3
#127        1       1      2
#139        1       1      0   

  #lastly what about just a interaction bw site and n, ignoring herb? kinda nonsense after looking at the last figs but what the hell:
  summbf <- ddply(vrt_grdplt, .(N_lbac, fert_rx), summarise, ba = mean(ba))#%>%
  #more fertilizer hurts basal area but idk why there arent more randoms above 200
summbf%>%
  ggplot(.,aes(x=N_lbac,y=ba))+
  geom_line(aes(group=fert_rx,linetype=fert_rx))
    vrt_grdplt%>%
    mutate(.,N_lbac=as.factor(N_lbac))%>%
    ggplot(., aes(x=MAJ_ARE, y=ba)) +
    geom_line(aes(group=N_lbac))
        geom_boxplot(aes(fill=N_lbac)) +
        geom_point(position=position_jitterdodge(jitter.width = .1),aes(group=N_lbac,shape=site))
    
    
  
  lapply(names(fud), 	function(x){ggsave(filename=paste0("Q:/Shared drives/FER - FPC Team/Variable Rate/Workspaces/Farmer_Michael/figures/grdplt/",x,".png"),
                                         width = 6, height = 4,plot=fud[[x]])})
  
  #
  
  
  ggplot(.,aes(x=MAJ_ARE, y=v22_m,color=N_lbac))+
    geom_boxplot()+
    geom_point(aes(y=interaction(MAJ_ARE,N_lbac)))
  
  
#more raw data stuff for plot scale vrt_grdplts: LAI correlations-----
  
  cbPalette <- c( "#D55E00", "#0072B2","#000000", "#A92224","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
  
  


  a<-  
  vrt_grdplt %>% 
    ggplot(aes(x=percov22,y=DU_2022))+ #might need percov_22 (same thing just old/new names)
    geom_point(aes(color=MAJ_ARE),size=6)+
    scale_colour_manual(values=cbPalette)+
      labs(color="Weed ctrl.")+
      scale_x_continuous("Observed understory cover") +
      scale_y_continuous("Satelite understory index") +
      theme_bw() + 
      theme(axis.title.x = element_text(size = 20, hjust = 0.54, vjust = 0),
            axis.title.y = element_text(size = 20, angle = 90,  vjust = 0.25),
            legend.text=element_text(size=16),
            legend.position = c(0.8, 0.2))+
    stat_poly_line() +
    stat_poly_eq()
    
    ggsave(a,filename="sateliteobs.png",width=6,height=6)
      
#useful plots and lm() for corrections to miscalibrated 2022 data, from vrt final data---------------------------------------------------------------
    
  #Definitely useful plot of 2022 in 2022 vs 2022 in 2023  heith  - this is what Sean and Michael used to test whether the estimated 2022 heights were good 
#... to use to model real 2022 heights. During 2023 three plots were used to measure 2022 heights by shooting the vertex at the last branching point up 
#.... in the tree as the place where the apical bud wouldve been the previous year
#So it looks pretty good

  #find out why there is a 28 foot tall tree
  #temportaty fix for 28ft tall tree
vrt%>%
  subset(.,Htft22in23>=30)%>%
#  mutate(.,Htft_wrong22in22=(Htft_wrong22in22*0.8284)+2.5323)%>% #correct this before the lm if you want to double check that it is correcting 22 data in the right direction (should get close to 0 intercept and slope of 1 if doing it right)
  lm(Htft22in23~Htft_wrong22in22,data=.)%>%
  #Htft_wrong22in22~Htft22in23*1.10627+2.92324
  summary()   #ok so r2 is 91.
  
#visually show the same thing as the lm()above (that things correlate well)
    vrt%>% 
      ggplot(., mapping=aes(y=Htft22in23,x=Htft_wrong22in22,color=rt))+
      geom_abline(slope=0.8124,intercept = 3.5549,color="blue")+ #before correcting some weird tree data these were the coefficients. note this is a very minor change if you change the intercept by 1 foot since the slope matters more
      geom_abline(slope=0.8284 ,intercept = 2.5323)+
      geom_abline(slope=1,intercept = 0)+
      xlim(0,80)+
      ylim(0,80)+
    geom_point()

#mutate(.,Htft=HtftI-Htft22)%>%
#Old plots made when there was only 1 vrt--------------------------------------------------------------------
#
      
#boxplot of dbh growth by stand
vrt%>%
  subset(.,Dbhin<=3&Dbhin>=-0.5)%>%
  ggplot(., mapping=aes(y=Dbhin,x=site))+
  geom_boxplot()

#boxplot of htlc growth by stand
  vrt%>%
    ggplot(., mapping=aes(y=Htlcft,x=site))+
    geom_boxplot()
    
#Scatter of dbh, ht, colored by Fert
vrt%>%
  ggplot(., mapping=aes(y=Dbhin,x=Htft,color=rt))+
  geom_point()


#Next 9 graphs are Box plots of growth
  #Site and Htlc
  vrt%>%
    subset(.,Htlcft<=3&Htlcft>=-0.5)%>%
    ggplot(., mapping=aes(y=Htlcft,x=site))+
    geom_boxplot()
  
      #Site and Ht
      vrt%>%
        ggplot(., mapping=aes(y=Htft,x=site))+
        geom_boxplot()
      
          #Site and DBH
          vrt%>%
            ggplot(., mapping=aes(y=Dbhin,x=site))+
            geom_boxplot()
          
              #Fert rate and Htlc
              vrt%>%
                ggplot(., mapping=aes(y=Htlcft,x=rt))+
                geom_boxplot()
      
                  #Fert rate and Ht
                  vrt%>%
                    subset(. ,Htft>=0)%>%
                    #subset(. ,Htft<=2)%>%
                    ggplot(., mapping=aes(y=Htft,x=rt))+
                    geom_boxplot()
                      #Fert rate and DBH
                      vrt%>%
                        subset(. ,Dbhin>=0)%>%
                        subset(. ,Dbhin<=2)%>%
                        ggplot(., mapping=aes(y=Dbhin,x=rt))+
                        geom_boxplot()
  
                          #Damage and Htlc
                          vrt%>%
                            ggplot(., mapping=aes(y=Htlcft,x=d23))+
                            geom_boxplot()
  
                              #Damage and Ht
                              vrt%>%
                                ggplot(., mapping=aes(y=Htft,x=d23))+
                                geom_boxplot()
  
                                  #Damage and DBH
                                  vrt%>%
                                    ggplot(., mapping=aes(y=Dbhin,x=d23))+
                                    geom_boxplot()
                                      
#Crown length per site Difference from 2022-2023
vrt%>%
  ggplot(., mapping=aes(y=CrownLength,x=site))+
  geom_boxplot()
                                          
#Crown length by fertilizer rate
vrt%>%
  ggplot(., mapping=aes(y=CrownLength,x=rt))+
  geom_boxplot()
                                              

#Volume growth from 2022-2023
vrt%>%
  ggplot(., mapping=aes(y=v,x=rt))+
  geom_boxplot()
#example for one tree
# (10*10*50*0.002)+0.25



#do statistics (signal to noise ratio)----
lm(height~fertilizer, data=vrt)
#Herbicide Information-------------------------------------------------------------------------------------------------
#
vrt_Herb_Final<-cbind(vrt_Herbicide, vrt22)


  df <- data.frame(x1=x1, x2=x2, y=y)

vrt_Herb_Final<-merge(vrt_Herbicide,vrt22, by = c("plot"), suffixes=c("H","22"))
vrt_Herbicide<-vrt_Herbicide%>%
  rename("Plot"=ResrcID)


vrt_Herbicide<-read.csv("Q:/Shared drives/FER - FPC Team/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data/cruising-data-VarRate-Herbicide.csv"
                        ,header=T)
vrt_Herbicide%>%
mutate(vrt_Herbicide, select= - FID)
subset(vrt_Herbicide, select= -GID)
#ggplot(., mapping=aes(y=ResrcID,x=rt))+
#  geom_boxplot()
subset(vrt_Herbicide, select= -HarvstU)
subset(vrt_Herbicide, select= -Proprty)
subset(vrt_Herbicide, select= -ReginId)
subset(vrt_Herbicide, select= -AreaId)
subset(vrt_Herbicide, select= -OfficId)
subset(vrt_Herbicide, select= -StatePr)
subset(vrt_Herbicide, select= -ContyId)
subset(vrt_Herbicide, select= -Activty)
subset(vrt_Herbicide, select= -Actvt_1)
subset(vrt_Herbicide, select= -UnitNam)
subset(vrt_Herbicide, select= -CommonN)
subset(vrt_Herbicide, select= -SubuntN)
subset(vrt_Herbicide, select= -CommonN)
subset(vrt_Herbicide, select= -GISArea)%>%
  names()%>%paste(.,collapse="','")%>%writeClipboard()

vrt_Herbicide%>%
  select('GID','HARV_TY','Herb','ResrcID','Acres','SUBGRID','CLAI','MAJ_ARE','GRID_AR','CLAI_GR','COMPLET','StandID','fert_rx','N_lbac','MAPlbac','MAPNlbac','Urealbac','fragArea','MAPlbs','Urealbs','MAPLAB')%>%
wex()#  head()

#i don't think this is necessary
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -HARV_TY)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -Acres)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -SUBGRID)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -CLAI)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAJ_ARE)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -GRID_AR)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -CLAI_GR)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAJ_ARE)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -COMPLET)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -StandID)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -fert_rx)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -frt_rtW)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -frt_rtW)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -frt_r70)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -frt_r80)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -fert_FINAL)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -SUB_ID)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -P_lbac)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -N_lbac)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAPlbac)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAPNlbac)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -Urealbac)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -fragArea)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAPlbs)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -Urealbs)%>%
vrt_Herb_Final<- subset(vrt_Herb_Final, select= -MAPLAB)
#---------------------------------------------------------------------------------------------------
#vrt whole plot and stand 2022 merge

vrt_Specific_data_22<-merge(vrt_plots,vrt22, by = c("plot","site","rt"), suffixes=c("s","22"))

vrt_Specific_data_22%>%
  ggplot(., mapping=aes(y=tre,x=Herbicide))+
  geom_boxplot()

vrt_Specific_data_22%>%
  ggplot(.,aes(x=plot,y=Fert_level,color=factor(site),shape=Herbicide,size=rt))+
  geom_point()+
  geom_abline(slope = 1,intercept=0)
#---------------------------------------------------------------------------------------------------
#vrt whole plot and stand 2023 merge
?merge
vrt_Specific_data_23<-merge(vrt_plots,vrt23, by = c("plot","site","rt"), suffixes=c("s","23"))

vrt_Specific_data_23%>%
  ggplot(.,aes(x=Herbicide,y=tre))+
  geom


#2022 variable rate plots-------------------------------------------------------------------------------------------------
#

    #DBH and site

vrt22%>%
  ggplot(., mapping=aes(y=Dbhin,x=site))+
  geom_boxplot()
  
    #DBH and herb

vrt22%>%
  ggplot(., mapping=aes(y=Dbhin,x=herb))+
  geom_boxplot()
  
    #DBH and Fertilizer Rate

vrt22%>%
  ggplot(., mapping=aes(y=Dbhin,x=rt))+
  geom_boxplot()

    #Height and site
  
vrt22%>%
  ggplot(., mapping=aes(y=Htft,x=site))+
  geom_boxplot()

    #Height and herb
  


    #Height and Fertilizer Rate
    
vrt22%>%
  ggplot(., mapping=aes(y=Htft,x=rt))+
  geom_boxplot()

    #Htlc and site
    
vrt22%>%
  ggplot(., mapping=aes(y=Htlcft,x=site))+
  geom_boxplot()


    #Htlc and herb



    
    #Htlc and Fertilizer Rate

vrt22%>%
  ggplot(., mapping=aes(y=Htlcft,x=rt))+
  geom_boxplot()


    
    #Volume and site
    
vrt22%>%
  ggplot(., mapping=aes(y=v,x=site))+
  geom_boxplot()


    #Volume and herb



    #Volume and Fertilizer Rate

vrt22%>%
  ggplot(., mapping=aes(y=v,x=rt))+
  geom_boxplot()

#2023 variable rate plots------------------------------------------------------------------------------------------------
#

#DBH and site

vrt23%>%
  ggplot(., mapping=aes(y=Dbhin,x=site))+
  geom_boxplot()

#DBH and herb



#DBH and Fertilizer Rate

vrt23%>%
  ggplot(., mapping=aes(y=Dbhin,x=rt))+
  geom_boxplot()

#Height and site

vrt23%>%
  ggplot(., mapping=aes(y=Htft,x=site))+
  geom_boxplot()

#Height and herb



#Height and Fertilizer Rate

vrt23%>%
  ggplot(., mapping=aes(y=Htft,x=rt))+
  geom_boxplot()

#Htlc and site

vrt23%>%
  ggplot(., mapping=aes(y=Htlcft,x=site))+
  geom_boxplot()


#Htlc and herb




#Htlc and Fertilizer Rate

vrt23%>%
  ggplot(., mapping=aes(y=Htlcft,x=rt))+
  geom_boxplot()



#Volume and site

vrt23%>%
  ggplot(., mapping=aes(y=v,x=site))+
  geom_boxplot()


#Volume and herb



#Volume and Fertilizer Rate

vrt23%>%
  ggplot(., mapping=aes(y=v,x=rt))+
  geom_boxplot()
#Lai plots-------------------------------------------------------------------------------------------



#LAi data complete
vrt_lai_final%>%
  ggplot(., mapping=aes(y=LAI_YEAR_TYPE,x=MEAN))+
  geom_boxplot()

#DUnder lai data
vrt_lai_final%>%
  filter(LAI_YEAR_TYPE != "vrt_2020_SRCLAI") %>%
  filter(LAI_YEAR_TYPE != "vrt_2021_SRCLAI") %>%
  filter(LAI_YEAR_TYPE != "vrt_2022_SRCLAI") %>%
  filter(LAI_YEAR_TYPE != "vrt_2023_SRCLAI") %>%
  ggplot(., mapping=aes(y=LAI_YEAR_TYPE,x=MEAN))+
  geom_boxplot() 

#SRCLAI lai data
vrt_lai_final%>%
  filter(LAI_YEAR_TYPE != "vrt_2020_DUnder") %>%
  filter(LAI_YEAR_TYPE != "vrt_2021_DUnder") %>%
  filter(LAI_YEAR_TYPE != "vrt_2022_DUnder") %>%
  filter(LAI_YEAR_TYPE != "vrt_2023_DUnder") %>%
  ggplot(., mapping=aes(y=LAI_YEAR_TYPE,x=MEAN))+
  geom_boxplot() 

#Herbicide accounted for      

      #All lai information with and without herbicide
      vrt%>%
        ggplot(., mapping=aes(y=LAI_YEAR_TYPE,x=MEAN))+
        facet_wrap("MAJ_ARE",scales = "free")+
        geom_boxplot()

      #SRCLAI lai data with and without herbicide
      
      vrt%>%
        filter(LAI_YEAR_TYPE != "vrt_2020_DUnder") %>%
        filter(LAI_YEAR_TYPE != "vrt_2021_DUnder") %>%
        filter(LAI_YEAR_TYPE != "vrt_2022_DUnder") %>%
        filter(LAI_YEAR_TYPE != "vrt_2023_DUnder") %>%
        ggplot(., mapping=aes(y=MEAN,x=Year))+
        facet_wrap("MAJ_ARE",scales = "free")+
        geom_boxplot()
        geom_vline(xintercept = 2021)
        
        
      #DUnder lai data with and without herbicide
      vrt%>%
        filter(LAI_YEAR_TYPE != "vrt_2020_SRCLAI") %>%
        filter(LAI_YEAR_TYPE != "vrt_2021_SRCLAI") %>%
        filter(LAI_YEAR_TYPE != "vrt_2022_SRCLAI") %>%
        filter(LAI_YEAR_TYPE != "vrt_2023_SRCLAI") %>%
        ggplot(., mapping=aes(y=MEAN,x=LAI_YEAR_TYPE))+
        facet_wrap("MAJ_ARE",scales = "free")+
        geom_boxplot()+
#Difference from 2022 to 2023-------------------------------------------------------------------------------------------------
#

#DBH and site

vrt%>%
  ggplot(., mapping=aes(y=Dbhin,x=site))+
  geom_boxplot()

#DBH and herb



#DBH and Fertilizer Rate

vrt%>%
  ggplot(., mapping=aes(y=Dbhin,x=rt))+
  geom_boxplot()

#Height and site

vrt%>%
  ggplot(., mapping=aes(y=Htft,x=site))+
  geom_boxplot()

#Height and herb



#Height and Fertilizer Rate

vrt%>%
  ggplot(., mapping=aes(y=Htft,x=rt))+
  geom_boxplot()

#Htlc and site

vrt22%>%
  ggplot(., mapping=aes(y=Htlcft,x=site))+
  geom_boxplot()


#Htlc and herb




#Htlc and Fertilizer Rate

vrt%>%
  ggplot(., mapping=aes(y=Htlcft,x=rt))+
  geom_boxplot()



#Volume and site

vrt%>%
  ggplot(., mapping=aes(y=v,x=site))+
  geom_boxplot()


#Volume and herb



#Volume and Fertilizer Rate

vrt%>%
  ggplot(., mapping=aes(y=v,x=rt))+
  geom_boxplot()








#ploit nubmering-----
#https://stackoverflow.com/questions/11996135/create-a-sequential-number-counter-for-rows-within-each-group-of-a-dataframe
NC_Remove00121<-read_excel("Q:/Shared drives/FER - FPC Team/RegionWide Trials/Special Studies/Variable Rate Fertilizer Application/2022 work near Louisburg NC/Data/NC_Remove00121.xls"
                           ,sheet=1)

NC_Remove00121 <- mutate(NC_Remove00121, Combined = paste(Herb, ResrcID, fert_rx, fert_final)) 

NC_Remove00121 <- NC_Remove00121 %>%
  mutate(Combined= if_else(.$OBJECTID = NoHerb, '1000', '1000'))


names(NC_Remove00121
      )

NC_Remove00121[,"ResrcID"]

paste0("-smit-001-55","Noherb")

#
NC_Remove00121 %>%
  group_by(Combined) %>% 
  mutate(id = seq_len(n()))%>%
  wex

#works dont mess
mtcars %>% group_by(cyl) %>% mutate(id = seq_len(n()))


#New 11/2023 stufff----
#started trying to merge tree data to gis so this is in service of that
#table up the number of trees in gps vs the paper sheets for nc-----
with(vrt,
     table(!is.na(Htft22),Name, useNA = "ifany"))->fff
data.frame(plot=fff[1,]%>%names(),
           trees=fff[1,])->ffff

rownames(ffff)<-NULL

#get the tree location stuff
gpkg<-"Q:/Shared drives/FER - FPC Team/RegionWide Trials/RW29 Variable Rate x Herb/Workspaces/Bloszies_Sean/RW29_doodlingmap/ncvr_rwPltStdyNms.gpkg"
tgs<-sf::st_read(dsn =gpkg,
                 layer = "tree_locations")
sf::st_layers(gpkg)

names(tgs)<-tgs%>%names%>%gsub("tree_locations_","",.)


#now tgs turn
with(tgs,
     table(!is.na(TREE_No),Name, useNA = "ifany"))->ttt
data.frame(plot=ttt[1,]%>%names(),
           trees=ttt[1,])->tttt

rownames(tttt)<-NULL

sort(ffff$plot)==sort(tttt[nchar(tttt$plot)>=4,]%>%pull(plot))

merge(ffff,tttt,by="plot",suffixes=c(".f",".t"))%>%
  mutate(x=trees.f-trees.t)%>%
  subset(x>0)

#huh? why more plots from gps
tgs%>%subset(Name=="Control_1-shad-139")%>%wex
#oh ok i just kept trees if they were duplicates (i guess i relocated them and didnt delete the first one a coupe times in like half the plots)
as.data.frame(tgs)%>%
  .[,c("TREE_No","Name")]%>%
  #subset(Name=="Control_1-shad-139")%>%
  unique()%>%dim()
#yea there's like 25ish trees that have multiple locations
#there are only like 5 or 6 plots where i have fewer trees on the gps so that's good