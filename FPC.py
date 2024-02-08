#how to pste/contat in py
" ".join(['yourself','go']) 

#region Get area of polygons from shp gpkg etc
import sys
import pip
#is geopandas installed(?)?
'geopandas' in sys.modules
#you get "False" the first time you install python , pylance, whatever it is idk
import geopandas as gpd
#run this in cmd: pip install geopandas
#python -m ensurepip --default-pip
#I gto an error in cmd on the GRADS computer with this;
#... so I went and 1st did something like turn off the python as 
#... default installer somewhere in windows settings
#... then i got a different error "'python' is not recognized as an internal or external command,
#operable program or batch file." ; so then i went and followed
#... https://stackoverflow.com/questions/7054424/python-not-recognized-as-a-command
#... which then needed me ot find where python was, so i did
import os #works,just new nothing visible in terminal
import sys#works,just new nothing visible in terminal
os.path.dirname(sys.executable) #this gives #C:\Users\sabloszi\AppData\Local\Programs\Python\Python312 ; so i put it in the environment variables on the PATH thing
#python -m pip install geopandas #this works in cmd now
#... but I got an error with the gpd.read_file() part below
#... so then I tried 
#>conda install -c conda-forge geopandas 
# #in cmd but i got the error
#>'conda' is not recognized as an internal or external command,
#>operable program or batch file.
#so then  I went to anaconda.com and got it
#but I still got the same exact error
#so I did what this says :https://stackoverflow.com/questions/44515769/conda-is-not-recognized-as-internal-or-external-command
#... and then the "conda install...geopandas" stuff worked
#So I think basically you need anaconda to install stuff like geopandas


#To set the working directory:
import os
import pandas as pd
import geopandas as gpd
path="/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/GIS/GIS for FPC WORKING 2021"
os.chdir(path)
#file="/Users/sabloszi/Dropbox (FPC)/FPC Team Folder/RegionWide Trials/Special Studies/Funga fungal microbiome/Funga_BLSF_site_updated_with_plot_numbers.gpkg"
file="RW28s/280601/180601PlotBoundaries.shp"
file="RW28s/284501/T184501PlotBoundaries_update.shp"#T184501PlotBoundaries_update
file="RW18s/181502/181502PlotBoundaries.shp"
file="RW18s/184401/184401PlotBoundaries.shp"
file="RW28s/281303/281303_Plots.shp"
file="RW28s/282201/282201/282201.gdb"
gdf = gpd.read_file(file,layer='T184501PlotBoundaries_update') #T184501PlotBoundaries_update plotboundaries2023trt #this needs to be the layer of the active file
gdf = gdf['geometry'].to_crs({'proj':'cea'}) 
df = pd.DataFrame(gdf.area)
df = (df/10000)/.4046 #if its in sq meters and you want it in ac
#df = df/43560 #if its in sq ft and you want it in ac
df.to_clipboard(index=False,header=False)
#test
450*6
#endregion
