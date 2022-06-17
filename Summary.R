library(reticulate)

wdpath <- "C:/Users/niedo/OneDrive/SL/2022/202206モンゴル調査/PreliminaryStudy/GPX"
dronepath <- "C:/Users/niedo/OneDrive/SL/2022/202206モンゴル調査/PreliminaryStudy/kml"
setwd(wdpath)

py.path <- "C:/Users/niedo/OneDrive/Documents/Pworks/Coord_UTM/"

source("C:/Users/niedo/OneDrive/Documents/Rworks/202206Mongolia/CalCoordinatesPlot.r")
reticulate::source_python(paste(py.path,"e_oneeqa2utm.py",sep=""))
