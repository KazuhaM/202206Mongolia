# 変数
ref.loc <- 9 # 基準点の位置。下参照
#     4
# 5┌─────┐3
# 6│  1  │2
# 7└─────┘9
#     8
# 分割数
div.style <- 3 # 1: 短冊状(緯線に沿って横に), 2: 短冊状（経線に沿って縦に）, 3: 格子状
div.num <- 4 # 短冊状の場合は短冊の本数、格子状の場合は縦横それぞれの行数・列数

# library
library(reticulate)



# set path
wdpath <- "C:/Users/niedo/OneDrive/SL/2022/202206MongoliaSurvey/PreliminaryStudy/GPX"
dronepath <- "C:/Users/niedo/OneDrive/SL/2022/202206MongoliaSurvey/PreliminaryStudy/kml"


py.path <- "C:/Users/niedo/OneDrive/Documents/Pworks/Coord_UTM/"

# python library
setwd(py.path)
reticulate::import("f_eqa2utm")
reticulate::import("f_utm2eqa")
pd <- reticulate::import("pandas")
tqdm <- reticulate::import("tqdm")
reticulate::import("pyproj")
setwd(wdpath)

# EQA to UTM ref point
reticulate::source_python(paste(py.path,"e_oneeqa2utm.py",sep=""))
print("complete to convert reference point from EQA to UTM")

# 基準点とその相対位置によって格子点を移動
source("C:/Users/niedo/OneDrive/Documents/Rworks/202206Mongolia/CalCoordinatesPlot.r")
print("complete to shift grid points on the basis of reference points")

# 格子点をUTMから座標に
reticulate::source_python(paste(py.path,"e_someutm2eqa.py",sep=""))
print("complete to convert grid points from UTM to EQA")

# ファイルを分割
source("C:/Users/niedo/OneDrive/Documents/Rworks/202206Mongolia/DivideFilesGPX.r")
print("complete to divede quadrat points in some files and make gpx files")

# ドローンの飛行経路を座標を推定
reticulate::source_python(paste(py.path,"e_dronecentpoints_utm2eqa.py",sep=""))
print("complete to calculate route points of drone flight")

# ドローンの飛行経路のテンプレート作成用のway point作成
# source("C:/Users/niedo/OneDrive/Documents/Rworks/202206Mongolia/QuadLoc_DroneWaypoints.r")
# reticulate::source_python(paste(py.path,"e_dronewaypoints_utm2eqa.py",sep=""))

