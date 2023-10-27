#dbf read in and merging with xlsx and accdb etc----
#setwd("C:/Users/seanb/Dropbox (FPC)/Site index stands_anon")#personal personal computer
#setwd("C:/Users/sabloszi/Dropbox (FPC)/Site index stands_anon")#work computer
midrot=10 #set the cutoff for Release and Thin to be included as a value in the global env for use later
#get all the dbfs
dbfc<-list.files(recursive = T)%>%
  subset(.,grepl("Company_",dirname(.)))%>%#all the ones in "Company"folders
  subset(.,grepl("\\.dbf",.))%>%
  subset(!grepl("Company_G/G_20230630.dbf",.))%>% #this one is one of the two vicent used to create the final G boundariesand can be ignored
  subset(!grepl("Company_G/ReleaseHistory.dbf",.))#this one is one of the two vicent used to create the final G boundaries and can be ignored
#make a list (takes like 5 sec on home pc, 1 mississippi sec on workpc)
dbfl<-lapply(dbfc,function(x){read.dbf(x)})
names(dbfl)<-dbfc%>%str_extract(.,"[^/]+")%>%#rename them by the root of the directory (the company)
  ave(., ., FUN = function(i) paste0(i, seq_along(i))) #... but give unique names bc some (G) have multiple dbfs to merge. update this doesnt do anything as of 10/13/23 bc g has been joined into the ..CLIPtoG dbf
names(dbfl)<-gsub("1","",names(dbfl)) #just need to keep the 2 or greater

#just CG, create the df
#UPDATE 9/29 This is an older version of Company_C so ignore for now or maybe for forever
#CompanyCG<-read.csv("C:/Users/sabloszi/Dropbox (FPC)/Site index stands_anon/Company_CG/CG_all_treatments_data_to_join.csv")
#I think these are the columns wee need from H
#unique(CompanyH[,c("Activitycategoryname","Activitytypename","Activitystatusname","Activitysubcategoryname")])%>%wex()
#create the name of company H as it will appear in the list
#val<-"Company_CG"
#add to list
#dbfl[[val]]<-CompanyCG

#just TIR, create the df
CompanyTIR<-read.csv("./Company_TIR/TIR_silv_hist_to_use.csv")
#create the name of company TIR as it will appear in the list
val<-"Company_TIR"
#add to list
dbfl[[val]]<-CompanyTIR

#company A is all in the dbf

#all the below merges, i tried to merge and maintain the .dbf standid, not the one from spreadsheets
#Compny B: Merge in this excel that has most of the mgt info. The dbf just mainly has stand est and age
dbfl[["Company_B"]]<-read_excel("./Company_B/LRM_Stand_Treatment_History.xlsx", #bigger one of the two for B bc it's stand level, not operation (activity) level
                                sheet="LRM Exp 9-6-2023")%>%
  merge(dbfl[["Company_B"]],.,by.x="STAND_KEY",by.y="Stand Key",all.x=T,all.y=F)%>%
  rename(Start.Date="Start Date",End.Date="End Date")

#Compny C: Merge in two csvs that has most of the mgt info. The dbf just mainly has stand est and age
dbfl[["Company_C"]]<-
  read.csv("./Company_C/thin_history.csv",header = T)%>%
  rename(.,treatment_type="thin_type",treatment_date="thin_date")%>%
  rbind.fill(.,read.csv("./Company_C/silv_history.csv",header = T))%>%
  merge(dbfl[["Company_C"]],.,by.x="standid",by.y="stand_id",all.x=T,all.y=T)



#company D dbf is all we have

##Compny E: Merge in the Access database with the dbf
#note dbf has i_ThinYr, i_EstYr  and .accdb has Thin_Year,Estab_Year
conn<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",getwd(), "/Company_E/Company_E_FPC site index project_Aug 2023.accdb"))
#see tables thats in it
#tblnams<-sqlTables(conn, tableType = "TABLE")$TABLE_NAME
#create indiv tables as dfs
#planti<-sqlFetch(conn, "FPC planting info") #could uncomment this for genetics stuff later
silvi<-sqlFetch(conn, "FPC silvi data",as.is=T)
standl<-sqlFetch(conn, "FPC standlist",as.is=T)


dbfl[["Company_E"]]<-
  #put the access dbs two together
  merge(standl,silvi,by.x="RELATE",by.y="Relate")%>%
  #then merge in the dbf
  merge((dbfl[["Company_E"]]),.,by.x="STNDNUM",by.y="RELATE",all.x=T,all.y=T)

#company F dbf and SI_Mapping_DD.xlsx are the same, here are some notes about the mgt columns (more details in QC section too):
#FERT_A_IND is ALWAYS                               Y when there is a year in FERT_A_YEA and vice versa, we can get rid of FERT_A_IND
#HERB_A_IND is ALWAYS                               Y when there is a year in HERB_A_YEA and vice versa, we can get rid of HERB_A_IND
#THIN_A_TYP is ALWAYS '1st Thinning' or '2nd Thinning' when there is a year in HERB_A_YEA and vice versa, we can get rid of THIN_A_TYP

#company G 3 dbfs . this one is unique as of 10/5/23 in that they gave us two dbfs that had to be merged. vicent merged them 10/12 and created "./Company_G/G_20230630_UNION_ReleaseHistory_CLIPtoG/G_20230630_UNION_ReleaseHistory_CLIPtoG.dbf" which is what sean used. note   Year_Est!=YEAR_EST_2 for a small handful like 7 stands in this : 
#"STAND_ID"	"Year_Est"	"YEAR_EST_2"
#71564	2006	1993
#71564	2006	1993
#65301	1985	2007
#92548	2002	1995
#92548	2002	1995
#92548	2002	1995
#74184	1981	1998
#sean is keeping the YEAR_EST_2 ones since thats what vicents been using
#Note also there are like 12 weird relics of the join that dont have polygons but are in the table:
#STAND_KEY 4135004 and #4712001 are two examples of this. Sean gets rid of them later with the final steps tat remove stands without stand ids (since these only have their harbicide STAND_KEY and are NA for the more important STAND_ID)


#Company H:
#for h theres no dbfs, create the df from gdbs
CompanyH<-merge(sf::st_read(dsn = "Company_H/Company_H.gdb", layer = "ChemicalandFertilization"), #has fert and herb when and what
                sf::st_read(dsn = "Company_H/Company_H.gdb", layer = "Silvicultural_History"), #has thin and fert and herb when 
                by.x="ResourceLabel",by.y="Resourcelabel",all=T)  


#I think these are the columns wee need from H
#unique(CompanyH[,c("Activitycategoryname","Activitytypename","Activitystatusname","Activitysubcategoryname")])%>%wex()
#create the name of company H as it will appear in the list
val<-"Company_H"
#add to list
dbfl[[val]]<-CompanyH

#Company I:
#This is a mix of merging plus selection all at once since I is more complicated and bigger than the others
#To make sense of Company I, you need the below .gdb's plus (getwd) \Company_I\export_files_description_US.doc
#... since that .doc tells you what the columns mean (eg PLN_OR_ACT means Plannned or Actual)
#.. and then you need (getwd) \Company_I\Export_File_Codes.csv to find out what is WITHIN those columns

CompanyIh<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "tblHarvSched_Vol")%>%
  subset(PLN_OR_ACT=="A")%>% #was the harv planned or actual
  subset(HARV_TYPE%in%c("T1","T2","T3","T4","T5"))%>%# Thinning number 1-5; note i excluded "Selection", Salvage Thin, Real Estate Thin,Shelterwood/Seedtree Thin ; ignoring "selection" is based on rachel's email from oct 2023 saying she would not do HBU or Select for another company
  select(c(PROPERTY, COMPART, STAND,foreignKey,HARV_TYPE,HARV_YEAR)) #rows of  harv operations by stand

CompanyIs<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "tblSilvs")%>%
  subset(PLN_OR_ACT=="A")%>% #was the silv planned or actual
  subset(TREATMENT%in%c("FT","WR","HR"))%>%# STRM in Export_File_Codes.csv 
  subset(TRMNT_TYPE!="SP")%>%# STYP and SP in Export_File_Codes.csv means Site Prep. It only pops up in Herbaceous Release (HR) STRM's so its safe to get rid of all of them
  select(c(PCS,ForeignKey,OP_YEAR, TREATMENT,TRMNT_TYPE,APP_MTHD )) #rows of  silv operations by stand
#ok so if TREATMENT==FT, its always fertilization we want
#the ZZs are probably safe to include? I am makig the assumption that they are where they had FT (fert) AT (at plant) and don;t have a record of how it was applied; or they had HR (herbaceous rel) and dno't know what kind  
#there's no reason to exclude WR or HR based on TRMNT_TYP or APP_MTH
#we can safely ignore APP_MTH but need to go to TRMNT_TYPE and select against SP

CompanyIf<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "FieldStandData")%>% 
  select(c(PCS,ESTYEAR))%>% #rows of stands with boundaries
  as.data.frame()%>% #this one's not a table in GIS so you need to convert it to df
  select(-Shape) #s

#now get the harv together with silv
CompanyIhs<-merge(CompanyIh,CompanyIs,by.x="foreignKey",by.y="ForeignKey",all=T)
#now merge harv+silv from before with the rows we have boundaries for to make the ultimate Company I df

val<-"Company_I"
#add to list
dbfl[[val]]<-CompanyIhs%>%
  mutate(.,PCSh=paste0(PROPERTY,(1e+10+(COMPART   *1000   )+STAND)))%>%#need to recreate PCS
  mutate(.,PCS= coalesce(PCS,PCSh))%>%
  select(-c(PCSh,PROPERTY, COMPART, STAND,foreignKey))%>% #dont need these anymore since done joining by them coalescing etc
  merge(.,CompanyIf,by="PCS",all.x=F,all.y=T)

rm(CompanyIf,CompanyIh,CompanyIhs,CompanyIs)

#Company_J: both an excel and the dbf. all of the mgt we're interested in comes fomr the excel join by#OBJECTID  in excel STANDOID in dbf
dbfl[["Company_J"]]<-
  read_excel("./Company_J/FPC_SI_StandDataDetails_v20230914.xlsx", #the other sheets in the book are not complete management; they look like harvest focusted
             sheet="FPC_Report_Activities")%>%
  merge(dbfl[["Company_J"]],.,by.x="OBJECTID",by.y="STANDOID",all.x=T,all.y=T)

####CLEANING OPERATIONS
#These things fix either actual problems like typos or data that needs coalescing and are meant to happen before selecting columns;
#.. note these operations also preserve specific oddities of the companies records that our general qualifications  the "midrot" cutoff miss like Comapny J's explicit "Midrotation" differntiation

#Company B
#so for company b I had to fix two of the years on two dates in the "End Date" column b/c the start was after the end. I just fixed it by
#... moving the date back exactly 365 days since I'm assuming they got the day right and the year wrong. They seem to trust their "End Date" more bc
#... that is the one that they use to derive a separate "year" column
val<-"Company_B"
#add to list
dbfl[[val]]<-dbfl[["Company_B"]]%>%
  mutate(.,Start.Date=base::as.Date(Start.Date,format=c("%Y-%m-%d")))%>%#))%>%# #make it a date
  mutate(.,End.Date=base::as.Date(End.Date,format=c("%Y-%m-%d")))%>%#))%>%# #make it a date
  mutate(.,Start.Date=base::as.Date(ifelse(Start.Date>End.Date,(Start.Date-365),(Start.Date))))

#Company H
#Just similar data in different columns to coalesce and some other treatments we arent interested in and subsetting by one broad mgt column (eg it includes cells  Clearcut) so it (Activitytypename) can be ignored and dropped later
val<-"Company_H"
#add to list
dbfl[[val]]<-  dbfl[["Company_H"]]%>%
  mutate(ActualEndDate           =coalesce(ActualEndDate.x,ActualEndDate.y))     %>% #they have herb and fert times  in two columns for different stands but doesnt look like theres any difference 
  mutate(Activitysubcategoryname2=coalesce(ActivityType,Activitysubcategoryname))%>% #they have herb and fert apps   in two columns for different stands but doesnt look like theres any difference 
  subset(Activitysubcategoryname2%in%c("Harvest","Fertilization","Herbicide"))%>%
  subset(!Activitytypename%in%c("Clearcut","Select Cut","Salvage - Thin","Salvage - Clearcut","HBU Cut","Clearcut - Lump Sum","Shelterwood - Seeding Cut"))#Get rid of non-thin harvest stuff. Note you end up with "Activitysubcategoryname2" == Harvest representing the thins we want. every kind of operation is in this column , easier to just say what not to use

#Company J 
val<-"Company_J"
#add to list
dbfl[[val]]<-dbfl[["Company_J"]]%>% #i had to do the "midrot" qualifications in this step for J rather than later bc they explicitly call some herb mid rotation and some herb is not called that so i didnt want our def of midrot to trump theirs
  mutate(.,COMPLETION_DATE=base::as.Date(COMPLETION_DATE,format=c("%Y-%m-%d")))%>%#))%>%# #make it a date
  subset(((PRIMARY_ES)-1)<=year(COMPLETION_DATE))%>% #there are things that happened before planting we need to get rid of including aerial release which sounds wrong. maybe some of the site prep stuff is right since this cuts out like 4k rows
  subset(Act!="Misc Activity")%>% #Misc Activity is only Site prep and release that is PCT or HWC. this is the only filter needed on Act
  subset(ActType%in%c("Release", "First Thin","Fertilizer", "Second Thin"))%>% #other options are like Harvest and Pest Control
  subset((ActType=="Release"& #want herbicide that...
            ((PRIMARY_ES)+midrot)<=year(COMPLETION_DATE))| #... happened at least 10years after est or...
           (ActType=="Release"& #is herbicide that...
              (ActDtl=="Mid Rotation"))| #... they explicitly call Mid Rot or... (note this only picks up all of 1 stinkin more row, a stand that they call midrotation release at 7 years. The next earliest midrotation release is 18 years)
           ActType!="Release") #... is not herbicide
#note these steps still leave in a lot of  ActDtl=="Premerch"&ActType== "Release" so not sure if we want to keep that or not


#so now let's see what they all look like:
#lapply(dbfl,function(x){dim(x)})
#some notes: 
#COLUMNS: Companies A, F, and J all have 70 or more columns. The next smallest is D with 35. Why are they so big?
#F is easy, its just a inefficiently constructed table; its the one with columns like "HERB_B_C_2"
#A is easy, its efficiently arragnged table but they just gave us all their merchandising info like specifically how much went to CNS etc per stand
#J makes sense, it is a merge of two big tables; the escel with all the mgt is like 21 columns and the attribute table is another 50

#ROWS:
#H is huge at 63k rows but that seems to be a combination of not only detailed records but storing it in a long format where the activities are all in the same column instead of across columns
#A is tiny at 758 rows but looks right bc it has the smallest .dbf and .shp at only like roughly 3mb each vs like 30mb for Company F
#Various others will have bigger sizes in the dbfl list than the attribute table bc of merging the individual stands with multiple operations/activities on each stand. like C for example has only 4k stands but like 21k rows at the end list

#write  to look at them
if(!dir.exists("./Workspace_Sean/viewingdelete/")){dir.create("./Workspace_Sean/viewingdelete/")}
lapply(names(dbfl), function(x) 
{write.csv((dbfl[[x]]), file = paste0(getwd(),"/Workspace_Sean/viewingdelete/",x, ".csv"))})

#table with stuff i have in common w vicent from his email 9/29 join columns. keeping this format so its easy to alter in R
nms2kpj<-read.table(text="
listnm  toe join
Company_A YEAR_EST  STAND_ID
Company_B YEAR_EST  STAND_OID
Company_C estyear standid
Company_D Estab_year  StandID
Company_E i_EstYr STNDNUM
Company_F YEAR_ESTAB  WTS_STAND
Company_G YEAR_EST_2  STAND_ID
Company_H EstablishmentDate ResourceLabel
Company_I ESTYEAR PCS
Company_J PRIMARY_ES  STAND
Company_TIR NA IFMSKey
",#note Company_J has Est_Yr but it's either the same as PRIMARY_ES or NA. PRIMARY_ES is never NA
                    #Note Company_TIR doesnt have years yet 
                    #note Vicent said EstDate for H but I see EstablishmentDate so keeping that instead for now. emailed him about it 10/9/23; change back if Im missing something and he corrects me
                    
                    header=T)
#make it a list
nms2kpj<-melt(nms2kpj,id.vars=c("listnm"))%>%
  cast(.,variable~listnm)%>%
  select(-c(variable))%>%
  as.list()

#keep only certain columns in a list-----
#decided to do this as a first step b/c it's too unweuidly to edal with tables that are 70-135 columns long.
#auto
#get list of names by df
nms2kp<-list(
  Company_A= c("THIN_TYPE",	"THIN_YEAR"	,"FERT_METH",	"FERT_YEAR",	"REL_METH",	"REL_YEAR"),
  Company_B= c("Type", "End.Date"),
  Company_C= c("treatment_type",	"treatment_date",	"activity_class",	"treatment_method",	"product_name"),
  Company_D= c("Fertilized"),
  Company_E= c("i_ThinYr",	"Trmt_Type",	"Trmt_Year"),
  Company_F= c("THIN_A_YEA","FERT_A_YEA","HERB_A_YEA"), #see notes in QC section on how i picked these
  Company_G= c("Act_Date", "FERT_YEAR", "THIN_YEAR"),# Fert is never before est year so we can just use FERT_METH. Thin is pretty specific so we can ignore the year bc nothing is precommercial (all are "Commercial" or some kind of row/selection thinning)
  Company_H= c("ActualEndDate","Activitysubcategoryname2"),
  Company_I= c("HARV_TYPE","HARV_YEAR","OP_YEAR","TREATMENT","ESTYEAR"),# I did this one already at the read in step bc it is just more complicated so its not reducing the dimensions here; also note you end up with a lot of stands, like 350 that have no mgt info (ie all na in     subset(is.na(TRMNT_TYPE)&is.na(APP_MTHD)&is.na(HARV_TYPE)&is.na(TREATMENT)))
  Company_J= c("COMPLETION_DATE", "ActType"),
  Company_TIR= c("fertilization",	"mid_rot_chem",	"thinning") #vicent already made this one 
)

#put the two lists of columns (nms2kpj = id vars, nms2kp = mgt vars) to keep together
nms2kp<-lapply(names(nms2kpj),function(x){
  nms2kp[[x]]<-c(narm(nms2kpj[[x]]),nms2kp[[x]]) #also get rid of na's
})

#give it back its names
names(nms2kp)<-names(nms2kpj)

#d
#make the list names a separate thing for dbfl
#dbfllsnms<-names(lis)
#lis<-lapply(names(lis),function(i){
#  lis[[i]]<-select(lis[[i]],nms2kp[[i]])
#})
#names(lis)<-dbfllsnms
#md


#this stuff gets rid of extraneous columns as far as the fert herb thin experiment goes
dbfllsnms<-names(dbfl)
dbfl<-lapply(names(dbfl),function(i){
  dbfl[[i]]<-select(dbfl[[i]],nms2kp[[i]])
})
names(dbfl)<-dbfllsnms




#write  to look at them
if(!dir.exists("./Workspace_Sean/viewingdelete/selected/")){dir.create("./Workspace_Sean/viewingdelete/selected/")}
lapply(names(dbfl), function(x) 
{write.csv((dbfl[[x]]), file = paste0(getwd(),"/Workspace_Sean/viewingdelete/selected/",x, ".csv"))})


#steps of standardizing------

#this simple fuction gets used to change non-na to 1's
na1 <- function(x) ifelse(is.na(x),NA,1)
#this  fuction gets used later to change 0s to NAs
zerona <- function(x) ifelse(x==0,NA,x)


#this simple function is to standardize columns
lookup_rename <- function(df, column_lookup) {
  df2 <- df
  names(df2) = column_lookup$new[match(names(df), column_lookup$old)]
  df2
}




#this simple function was a way to rename the levels of variable really explicityl to lump dates with mgt
#swmgtdnms<- function(these){#switch management and date names  with more general ones after creating a "1/0 is it a date?" column
#  reduce2(c('dtthin','thinning', 'dtfert', 'fertilization', 'mid_rot_chem', 'dtchem'), c('thi','thi','fer','fer','che','che'),  .init = these, str_replace)
#}

#this simple function was a way to rename the levels of variable really explicityl to lump dates with mgt
cbmgtnms<- function(these,df,dfcompany){#combine management with similar ones "1/0 is it a date?" column
  df<-df[df$company==dfcompany,]
  reduce2(df$old, df$new,  .init = these, str_replace)
}



#these are the columns of company variable names and  and what FPC variable names they correspond to by company
variable_match<-read.csv("./Workspace_Sean/R/variable_match.csv",header = T)
#these are the columns of company level names and  and what FPC variable names they correspond to by company
level_match<-read.csv("./Workspace_Sean/R/level_match.csv",header = T,strip.white = T)

#wael("./Workspace_Sean/R/level_match.csv")



dbfl[["Company_A"]]<-dbfl[["Company_A"]]%>%
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_A",])%>%
  #mutate(.,ifelse(thinning=="Operator Select",NA,thinning))%>% #personal communication 10/9 about company J w/ rachel she said operator select could be some hardwood selection thing  in their case bc they call it "Misc Thin, Operator Select" or something v similar and in that case to ignore it as a thinning we are interstedin 
  mutate(across(c(fertilization,mid_rot_chem,thinning), na1))%>% 
  mutate(.,thi=paste0(thinning,",",dtthin))%>%
  mutate(.,fer=paste0(fertilization,",",dtfert))%>%
  mutate(.,che=paste0(mid_rot_chem,",",dtchem))%>%
  select(c(yrest,stand,thi,fer,che))%>%
  melt.data.frame(id.vars=c("yrest","stand"))%>%
  separate_wider_delim(value, delim = ",", names = c("op", "opye"))%>%
  mutate_all(.,function(x){gsub("NA",NA,x)})%>% #Nas are charcer, need to turn back to true NAs
  subset(.,!is.na(op))%>% #you get NAs Im guessing bc the melt etc when some stands dont have the other operations done on them
  select(-op)%>%#don't need 1/0 anymore
  rename(op="variable") # keep the kind of op column and renameit


dbfl[["Company_B"]]<-dbfl[["Company_B"]]%>%#Release had a Type==Combination for all of 4 rows; Sean didn't include those bc its not clear what it means. their readme is spartan
  subset(.,Type%in%level_match[level_match$company=="Company_B","old"])%>%
  mutate(.,Type=cbmgtnms(these=Type,df=level_match,dfcompany="Company_B"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  #mutate(.,seq=with(., ave(STAND_OID,Type, FUN = seq_along)))%>%
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_B",])

dbfl[["Company_C"]]<-dbfl[["Company_C"]]%>%
  select(-c(activity_class,treatment_method,product_name))%>% #after looking at the data you don't actually need these three since all info for this project is containedin treatment_type
  subset(.,treatment_type%in%level_match[level_match$company=="Company_C","old"])%>%
  mutate(.,treatment_type=cbmgtnms(these=treatment_type,df=level_match,dfcompany="Company_C"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_C",])

dbfl[["Company_D"]]<-dbfl[["Company_D"]]%>% #  Extremely spartan
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_D",])%>%
  mutate(.,opye=ifelse(op==1,"2023-01-01",0))%>% #make up a date
  mutate(.,op="fer")%>% #theres no che or thi
  subset(.,opye!=0)

dbfl[["Company_E"]]<-dbfl[["Company_E"]]%>% #  #Im assuming HWC stands for Herbaceous weed control. there's no notes saying that anywhere.
  melt.data.frame(id.vars=c("i_EstYr","STNDNUM","Trmt_Type"),measure.vars=c("i_ThinYr","Trmt_Year"))%>%
  mutate(.,variable=as.character(variable))%>%#ugh melt converted it to factor
  mutate(.,Trmt_Type=ifelse(variable=="i_ThinYr",variable,Trmt_Type))%>%
  subset(!is.na(value))%>%
  subset(.,Trmt_Type%in%level_match[level_match$company=="Company_E","old"])%>%
  mutate(.,Trmt_Type=cbmgtnms(these=Trmt_Type,df=level_match,dfcompany="Company_E"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  select(-variable)%>%
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_E",])%>%
  subset(!(op=="thi"&opye==0)) #these are true non thinned stands, thats how they record thinning vs none as a real year vs a 0 for the year


dbfl[["Company_F"]]<-dbfl[["Company_F"]]%>% 
  melt.data.frame(.,id.vars=c("YEAR_ESTAB","WTS_STAND"),variable_name="op")%>%
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_F",])%>%
  mutate(.,opye=zerona(opye))%>%
  mutate(.,op=cbmgtnms(these=op,df=level_match,dfcompany="Company_F")) #rename the stuff in the "these" column for "company" whatevr in variable_match

dbfl[["Company_G"]]<-dbfl[["Company_G"]]%>% 
  melt.data.frame(id.vars = c("STAND_ID","YEAR_EST_2"),measure.vars=c("Act_Date", "FERT_YEAR", "THIN_YEAR"),
                  variable_name = "op")%>%
  mutate(.,op=cbmgtnms(these=op,df=level_match,dfcompany="Company_G"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_G",])

dbfl[["Company_H"]]<-dbfl[["Company_H"]]%>% 
  mutate(.,Activitysubcategoryname2=cbmgtnms(these=Activitysubcategoryname2,df=level_match,dfcompany="Company_H"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_H",])%>%
  mutate(.,yrest=as.integer(year(as.Date(yrest,tz="America/New_York"))))%>% #theyre the only ones recording in ymd hms format
  mutate(.,opye=as.integer(year(as.Date(opye,tz="America/New_York")))) #theyre the only ones recording in ymd hms format

dbfl[["Company_I"]]<-dbfl[["Company_I"]]%>% 
  melt.data.frame(id.vars=c("ESTYEAR",	"PCS","HARV_TYPE","TREATMENT"),measure.vars=c("HARV_YEAR",	"OP_YEAR"),
                  variable_name="yeartype")%>%
  rename(opye="value")%>%
  melt.data.frame(id.vars=c("ESTYEAR",	"PCS","yeartype","opye"),measure.vars=c("HARV_TYPE","TREATMENT"))%>%
  select(-c(yeartype,variable))%>%#don't need variable bc it's just a column of whether an op is  (HARV_TYPE) or (TREATMENT)
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_I",])%>%
  subset(.,!(is.na(op)|is.na(opye)))%>%
  mutate(.,op=cbmgtnms(these=op,df=level_match,dfcompany="Company_I")) #rename the stuff in the "these" column for "company" whatevr in variable_match

dbfl[["Company_J"]]<-dbfl[["Company_J"]]%>% 
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_J",])%>%
  mutate(.,op=cbmgtnms(these=op,df=level_match,dfcompany="Company_J")) #rename the stuff in the "these" column for "company" whatevr in variable_match

dbfl[["Company_TIR"]]<-dbfl[["Company_TIR"]]%>% 
  lookup_rename(.,column_lookup = variable_match[variable_match$company=="Company_TIR",])%>%
  melt.data.frame(id.vars=c("stand"),variable_name = "op")%>%
  mutate(.,value=cbmgtnms(these=value,df=level_match,dfcompany="Company_TIR"))%>% #rename the stuff in the "these" column for "company" whatevr in variable_match
  subset(.,value==1)%>%
  mutate(.,yrest="2005")%>% #note this is fake and just a placeholder to make the midrot calcs work
  mutate(.,opye="2016")%>% #note this is fake and just a placeholder to make the midrot calcs work
  select(-value)


#write  to use them in the next step
if(!dir.exists("./Workspace_Sean/viewingdelete/selected/final/")){dir.create("./Workspace_Sean/viewingdelete/selected/final/")}
lapply(names(dbfl), function(x) 
{write.csv((dbfl[[x]]), file = paste0(getwd(),"/Workspace_Sean/viewingdelete/selected/final/",x, ".csv"))})

#put it all together----
#read them out
midrot=10 #set the cutoff for Release to be included as a value in the global env for use later
#get all the final csvs
#need these nxt 7 or so lines bc some csvs have leading 0's but dont want to make everything character
setClass("num2")
setAs("character", "num2",
      function(from) {
        from2 <- type.convert(from, as.is = TRUE)
        if (is.numeric(from2) && any(grepl("^0", from))) from else from2
      })
dbfcf<-list.files(path ="./Workspace_Sean/viewingdelete/selected/final/")
#make a list (takes like 5 sec on home pc, 1 mississippi sec on workpc)
dbflf<-lapply(dbfcf,function(x){read.csv(paste0(getwd(),"/Workspace_Sean/viewingdelete/selected/final/",x),colClasses = "num2")})
names(dbflf)<-dbfcf%>%gsub("\\.csv","",.)
#now i can die in piece and go to ... because i have made the uber list
valhal<-rbindlist(dbflf,idcol = "CO",use.names = T)

valhal<-valhal%>%
  mutate(.,opye=as.integer(ifelse(nchar(opye)<=4,opye,year(as.Date.character(opye)))))%>% #turn into year not date for those that report it as date (i think this is just company H)
  mutate(.,yrest=as.integer(ifelse(nchar(yrest)<=4,yrest,as.character(year(as.Date.character(yrest))))))%>% ##turn into year not date for those that report it as date (i think this is just company H)
  subset(.,op%in%c("fer","thi")|(op=="che"&((opye-yrest)>=10))) #note this automatically excludes herbicides where we dont know either the yrest or the opye bc R can't subtract from NA. You can reverse this with |is.na(yrest)|is.na(opye)). There's only 6k out of 467k che rows that this na case applies to

if("you want to write over the final silv.csv"==T){
  valhal%>%
    mutate(.,compID=paste0("CO",gsub(".*_","",CO),stand))%>%
    mutate(compID=gsub("^COTIR","TIR",compID))%>% #see email form vicent 10/19/23 9:12am to sean
    subset(.,!is.na(stand))%>%
    subset(!(CO=="Company_G"&is.na(opye)))%>% #should move this step to the companies reformatting steps at some point
    select(compID,op)%>%  
    distinct()%>%
    mutate(i=1)%>% #placeholder for casting
    cast(.,compID~op,value="i")%>%
    write.csv("./silv.csv",row.names = F)}


#QC notes ------
#Is it fine to get rid of stands that are NA? THere are just a handful
#D is easy, its just ferts that had a 0 year for some reason but no stand or anything else so just drop those by getting rid of all nas for stand in d
#G is easy, theres a few weirdos that had no area in a polygon and no STAND_ID in the original table but they did have herbicide info

#Is it fine to get rid of duplicate rows?
#Company A is fine

#Company b
#119405 in Company B is an example of one that got thinned twice in one year - both "1st Thin" so I'm assuming they stopped part way through and finished later in the year?
#122536 in Company B is an example of one that got thinned twice in one year - but I think this is a data entry error bc the rows in the actual raw data are duplicates
#end of story i think in company b you can get rid of duplicates bc theyre in the raw data

#Company C
#10222 got different rows for the surfactant, the garlon, and the escort (yes slash pine but thats not my dept)
#10222 same for fert, dap is a different row in 2004 by ground than 2014 aerial
#company c is fine

#company D
#Company D is wierd
#80031 has duplicate fertilizer apps in the raw Stand_Acre; everything else in the rows is the same ecept like the Stand_Acre column and maybe global id. idk.
#im saying itis fine

#Company E
#Company E is more intersting. It's just picking up dpulicates bc i had to do 3 total merges. Since I didnt
#... subset out treatments we weren't interested in the data that becomes "silvi" first, each one of them
#... (eg, planting, chemical site prep (CSP), and mech site prep (MSP)) gets merged to the same thinning
#.... operation. so anyways its fine

#Company F 
#it's fine because theres only two duplicates (so 4 total rows) amd its a waste of time to go after them

#Company G
#Company G made some mistakes like they have two rows that are identical with the only thing 
#.. different is they say they  put out "Herbaceous Control - Broadcast Ground" on 2016-12-31 with
#.. Polaris and then  2017-06-14  with  Surfactant . both are to 104232 so something is wrong with their records
#And then I added in some op's that truly have no opye bc they didn't occur. They end up in the list that
#.... goes into valhal bc G had three columns for each of fert che and thi so two of the three are na.
#so g yes its fine. it's also fine to remove ops without an opye / with na for opye

#Company H
#Company H is like E in that it's  picking up dpulicates bc i had to do a merge
#its fine.

#Company I
#I is fine too. It got real big bc of having different ops in different columns and doing two merges and merging on different columns each time.

#Company J
#J is fine. THey just have for example for STANDOID==124044 aka STAND==116010110 thinning that gets three rows even thow its on one stand in one 
#... summer or 2018 because it took place over three date ranges. The date ranges for this stand all
#... overlap which is funny. Like why store that info? maybe they have three invoices from the logger or something idk

#Company TIR
#TIR is fine. Vicent has duplicate rows in the raw table for whatever reason

#Is it fine to get rid of opye's that are NA?
#For G, yes. I think this was a merging thing
#for F, no. (for actual operations they had years listed as 0's and i changed them to NAs to be less nonsensical)

#QC (non-edit) Opperations----- 
#these are what sean did to determine which columns to use when they had more than 1 or two per mgt type and he had to make a judgement call on which columns actually were meaningful to us 
#. these are not meant to edit the data that is used for analysis, just to aid in column selection

#QC on Company F
Company_F<-read.csv(paste0(getwd(),"/Workspace_Sean/viewingdelete/Company_F.csv"),header = T) #ctrl+f aAWbhgbt2525
Company_F<-
  Company_F[,c("THIN_A_BEF","TOT_NUM_TH","THIN_A_AGE","THIN_A_YEA","THIN_A_TYP","THIN_A_AFT","FERT_A_IND","FERT_A_AGE","FERT_A_YEA","FERT_A_TRE","FERT_A_CHE","FERT_A_C_1","FERT_A_C_2","FERT_A_C_3","FERT_A_C_4","FERT_A_C_5","FERT_A_C_6","FERT_A_C_7","FERT_A_C_8","FERT_A_C_9","FERT_A__10","FERT_A__11","HERB_A_IND","HERB_A_AGE","HERB_A_YEA","HERB_A_TRE","HERB_A_TYP","HERB_A_MET","HERB_A_CHE","HERB_A_C_1","HERB_A_C_2","HERB_A_C_3","HERB_A_C_4","HERB_A_C_5")]
Company_F%>%
  dplyr::select(where(is.character))%>%
  apply(.,2,FUN=function(c){
    ifelse(is.na(c),NA,as.integer(1))
  }
  )%>%
  cbind(.,Company_F%>%dplyr::select(!where(is.character)))%>%
  dplyr::select(.,contains("HERB"))%>% #cycle through FERT, HERB, and TH
  rowSums(na.rm = T)->allnoth

notyp<-c(1:nrow(Company_F))[0==(Company_F$HERB_A_YEA)] #cycle through THIN_A_YEA, THIN_A_TYP,FERT_A_IND,HERB_A_IND,HERB_A_YEA and change back and forth b/w 0== and is.na() depending on the vector type
#if this is TRUE below that tells you they don't record anything not summarized by the ..._IND or ..._TYP columns and their associated years
all.equal(notyp,c(1:nrow(Company_F))[0==(allnoth)])

#for company F, herb columns:
#yes its true there arent cases where they dont give a herb year or ind and then there's data in another column

#for Company F, thinning columns:
#yes it's true, you can forget the other columns besides THIN_A_YEA and THIN_A_TYP, and then you can get rid of thin_a_yea if its meeting the thinngin year criteria
#There are a few weirdos like the THIN_A_AGE for WTS_STAND==K113-0130-0025 is 0 bc the THIN_A_AGE =YEAR_ESTAB but it's been thinned so it's gotta be wrong but it doesn't concern us (this is the only THIN_A_AGE that is wrong)
#note there are 100s of stands where THIN_A_AFT==0&THIN_A_BEF!=0; not sure what that means

#for Company F, fert columns:
#yes it's true, you can forget the other columns besides FERT_A_IND bc there's no case where there is info in another column that is not summed up by a "Y" in the FERT_A_IND

#QC on Company I
#By far the most extensive and kinda the most complicated data set bc its all in GIS AND its all relates so I had to rejoin everything
#to make sens of this, start with 
#Company_I\export_files_description_US.doc
#then go to (maybe EXPORT_STAND.csv) but definitely EXPORT_SILV.csv section within that...
#... and then youll need to do a complicated join by the Type == STRM for the tblSilvs TREATMENT column
#..                                                   or Type == STYP for the tblSilvs TRMNT_TYPE column
#..                                                   or Type == SLAM for the tblSilvs APP_MTHD column
#I Think that will do it for fert and herb
#then youll need to join the 
#PROPERTY COMPART STAND becomes PCS just as...
#WI	9702	303 becomes WI10009702303 (so paste0("WI",(1e+10+(9702*1000)+303))=="WI10009702303"),
#or paste0(PROPERTY,(1e+10+(COMPART   *1000   )+STAND))
#...and   DC8FBFE0-7F4A-4B78-9B25-4D351FADDA4E in TblSilvs has WI10009702303,
#...while DC8FBFE0-7F4A-4B78-9B25-4D351FADDA4E in tblHarvSched_Vol has (WI	9702	303) for (PROPERTY COMPART STAND)

CompanyIh<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "tblHarvSched_Vol")%>%
  select(c(PROPERTY, COMPART, STAND,foreignKey))
CompanyIs<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "tblSilvs")%>%
  select(c(PCS,ForeignKey))
CompanyIf<-sf::st_read(dsn = "Company_I/FPC_DataRequest.gdb", layer = "FieldStandData")%>% 
  select(c(PCS,ESTYEAR))

CompanyIhs<-merge(CompanyIh,CompanyIs,by.x="foreignKey",by.y="ForeignKey",all=T)

head(CompanyIhs[is.na(CompanyIhs$PCS),]) #ok so lots of harvested stands dont have silv info
#are they not matched by pcs either?
(CompanyIhs)%>%
  mutate(.,PCSh=paste0(PROPERTY,(1e+10+(COMPART   *1000   )+STAND)))%>%#need to recreate PCS
  subset(!is.na(PROPERTY))%>% # like 7k rows were in the silv and not the harv
  subset(!is.na(PCS))%>% #only like 20 rows were in the harv and not the silv
  subset(PCS==PCSh)%>%
  dim() #same dim as before subset(PCS==PCSh) so their PCS and my reconstructed PCSh are the same

#ok now lets see if the boundary table goes with these:
(CompanyIhs)%>%
  mutate(.,PCSh=paste0(PROPERTY,(1e+10+(COMPART   *1000   )+STAND)))%>%#need to recreate PCS
  mutate(.,PCS= coalesce(PCS,PCSh))%>%
  select(-PCSh)%>%
  merge(.,CompanyIf,all.x=F,all.y=T)%>%
  subset(is.na(PROPERTY))%>%
  pull(PCS)%>%unique()%>%length() #answer is 3773, which means there are 3773 stands that have no harvest info (bc theyre not in the harvest table, whcih i know bc only that table lends a column named" property" to this df)

#Also note company I has an oddly high number of stands getting fertilized 4 times or more:
read.csv("C:/Users/sabloszi/Dropbox (FPC)/Site index stands_anon/Workspace_Sean/viewingdelete/Company_I.csv",header = T)%>%
  subset(TREATMENT=="FT")%>%
  select(PCS,OP_YEAR,TREATMENT,TRMNT_TYPE)%>%
  distinct()%>%
  group_by(PCS)%>%
  reframe(hm=length(OP_YEAR))%>% #how many different years of each Fertilizer happens if you group by stand?
  select(hm,PCS)%>%
  with(.,table(PCS,hm))%>%
  colSums()/8753 # 8753 is just the number of unique stands that got fertilizer at any point.
#Long story short like 396 or 4% of their stands get fertilized in four different years. 2 stands got fertilized 6 times.


#bash----
setwd("C:/Users/sabloszi/Dropbox (FPC)/Site index stands_anon/Workspace_Sean/R")