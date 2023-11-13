#Notes/ Intro -----
#The point of this .R script was to get the computer to go to the old website (forestproductivitycoop.net),
#... scrape info out of the meetings tables (like this meeting was in 2013 in Atlanta, and these speakers spoke, 
#... and here are theyre presentations as linked pdfs) so that it could be repurposed on the new website (forestproductivity.org)
#... A lot of the work wasnt in this original scraping but instead in the merging the table info back with the files they went with, in the form
#... of the google drive (at present) location where they live in the shared FER-FPC Team drive
#... Lastly this writes the finished product (presentation info and links) out to a google sheet.
#... Plus there is a little bit of qc at points throughout.
#The "; USED" after certain sections means I used that section , or at least a lot of it
#... to make the final table meetingtabletargets.gsheet or whatever it is now
#... Ones without "; USED" were run and probably worked at one time but might just be practice
#... or were kept just in case they were useful to other projects. Sean Bloszies 11/13/2023
#... 
#Scraping: see what rcrawler does-----
LinkExtractor(url="https://forestproductivitycoop.net/2020-us-annual-meeting/",
              ExternalLInks=T)
#seems powerful but too long to learn. no obvious way to get link text

#Scraping: download randos 8/29-----
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

#Scraping: v8 stuff-----
#pdf_links<-
session_submit(s, filled_form)%>% 
  session_jump_to("https://forestproductivitycoop.net/publications/fpc-presentations/2013-october-7-9-annual-meeting-atlanta-ga/")%>%
  session_follow_link("Summary of Latin America Pine Working Group Projects & Regionwide Studies (pdf)")%>%#Research
  html_nodes("a[href*='-pdf']")
html_attr("href")


#Scraping: doanload pdfs chatgpt for loop-----
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




#Scraping-html tables: turn a html table into a dataframe- ------
text<-"Pretend_that_you_have_some_tables_html_code"#note obviously you have to get some html to put here instead of this placeholder
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
  
#Scraping-html tables: see if rvest works with map_df:-----
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




#Scraping-html tables: html DONT DELETE wp login; USED-----
#This section was the main one where I connected to the wp .net site and got html tables content from
#so in the end the columns i want are:
#  Year
#Meetingtype-annual, contact etc
#Meetingplace? sure
#Speaker
#Title, hyperlinked to pdf or video

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

#use this to test what it got:
#r2%>%
# read_html()%>%write_html(file="G:/Shared drives/FER - FPC Team/Website/Htmltables/2020writehtml-loggedin.html")

#fnas pick  up neating this up and swiping from fpc.r 11/12/2023 (you were in the middle of seeing if any of the html in the websitehtml folder is worth keeping; yes some prob)

#Scraping-html tables: html DONT DELETE actual within-page stuff; USED----


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


#Downloads/extractions: libreoffice stuff to convert pptx or docx or doc to pdf-----
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



#Downloads/extractions: just get pdfs------
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
#Downloads/extractions: just get pdfs but maintain hierarchy-----
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


#Downloads/extractions: Untar Rachel's wp dwnlds and compare it with my mlfd dwnlds; USED-----
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



#Google drive file info: get file name and id IMPORTANT DONT DELETE; USED#################################################################################----

#usually need to login in fresh r-: (also usually need to check the boxt that says allow tidyverse api to see files in drive):
#PICK SEND TO BROWSER (should be opttion "1")
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

#Big step; make googledmerge for the first time (its just the drive id and the name of the file but that's all you need)
googledmerge<-xme%>%dplyr::select(c(id,name))
#  unnest_wider(col=drive_resource)#maybe this would tell me whats in that colkumn (its a list i thnk0)

#as is, this is a way to get recents through R. It seems toalso be a flexible search if you know how to use it
#drive_find(n_max = 30) 

#Merging: First gooogle /wp merge; USED---------
#Getting rid of target text in a string with regex (for pdf and video in pres titles): ReaL;
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





#Merging: make bigcombotable: just merge the main meeting tbl with year by year; USED===-----
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





#Merging: final .net formatting to merge googled links with meeting info, don't delete; USED-----

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

#Merging: dealing w/ effed cells, spanish characters; a workspace for final websitetable stuff; USED-----

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


#QC: make a random subset of the meetings pres that has two links per meeting (for meetings with links); USED========------
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

