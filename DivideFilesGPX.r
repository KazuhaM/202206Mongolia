######################## CallCoordinatePlot.rを実行してから################


# Init --------------------------------------------------------------------
# # 分割数
# div.style <- 1 # 1: 短冊状(緯線に沿って横に), 2: 短冊状（経線に沿って縦に）, 3: 格子状
# div.num <- 1 # 短冊状の場合は短冊の本数、格子状の場合は縦横それぞれの行数・列数

# Divide files ------------------------------------------------------------
# データ読み込み
quad.coordEQA <- read.csv("quad_coordinate.csv",header = T) # e_someutm2eqa.pyでeqaに戻したファイル
grid.coordEQA <- read.csv("Grid240_ponits_coordinate.csv") # 最初にUTMに変換する前のファイル
ref.coordEQA <- read.csv("RefPoint.csv") # 緯度経度表示の基準点

# grid.coordEQAを基準点で移動する
grid.rangeEQA <- c(max(grid.coordEQA$x) - min(grid.coordEQA$x), # サイトの縦横の大きさ（緯度経度）
                max(grid.coordEQA$y) - min(grid.coordEQA$y))

# 左下を0,0にする
grid.coordEQA$x <- grid.coordEQA$x - min(grid.coordEQA)
grid.coordEQA$y <- grid.coordEQA$y - min(grid.coordEQA$y)

shft <- switch(ref.loc,
               "1" = ref.coordEQA - c(grid.rangeEQA[1]/2, grid.rangeEQA[2]/2),
               "2" = ref.coordEQA - c(grid.rangeEQA[1]/2, grid.rangeEQA[2]),
               "3" = ref.coordEQA - c(grid.rangeEQA[1], grid.rangeEQA[2]),
               "4" = ref.coordEQA - c(grid.rangeEQA[1], grid.rangeEQA[2]/2),
               "5" = ref.coordEQA - c(grid.rangeEQA[1], 0),
               "6" = ref.coordEQA - c(grid.rangeEQA[1]/2, 0),
               "7" = ref.coordEQA,
               "8" = ref.coordEQA - c(0, grid.rangeEQA[2]/2),
               "9" = ref.coordEQA - c(0, grid.rangeEQA[2]),
               stop("Please enter an integer (from 1 to 9) to reg.loc")
               
)
grid.coordEQA$x <- grid.coordEQA$x + as.numeric(shft[1])
grid.coordEQA$y <- grid.coordEQA$y + as.numeric(shft[2])

# 分割用の数値計算
min.x <- min(grid.coordEQA$x) # 最も小さい緯度　（サイトの下端）
min.y <- min(grid.coordEQA$y) # 最も小さい軽度　（サイトの左端）
div.dist.x <- grid.rangeEQA[1] / div.num # 分割した場合の間隔
div.dist.y <- grid.rangeEQA[2] / div.num # 分割した場合の間隔


# サイト分割図
plot(grid.coordEQA$y,grid.coordEQA$x,cex = 0.4, col = 1,bg=1, pch = 21,
     xlab = "longitude, E", ylab = "latitude, N")

# 分割
pb <- txtProgressBar(min=0, max=div.num, style=3)
if(div.style == 1){
  # 短冊状に横に分割する場合（緯線に並行）
  for (i_div_x in 1:div.num) {
    temp.quad.coord <- quad.coordEQA[quad.coordEQA$lat > min.x + div.dist.x * (i_div_x - 1) & quad.coordEQA$lat < min.x + div.dist.x * i_div_x,]
    rect(min.y,
         min.x + div.dist.x * (i_div_x - 1),
         max(grid.coordEQA$y),
         min.x + div.dist.x * i_div_x,
         border = 3, lwd = 1.5)
    text((min.y + max(grid.coordEQA$y))/2,
         (min.x + div.dist.x * (i_div_x - 1) + min.x + div.dist.x * i_div_x)/2,
         paste("lat",i_div_x,sep=""),
         col=3,font=2,cex=1.5)
    write.csv(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"quad_coordinate.csv",sep=""))
    write.gpx(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"quad_coordinate.gpx",sep=""))
    setTxtProgressBar(pb, i_div_x) 
  }
  
}else if(div.style == 2){
  # 短冊状に縦に分割する場合(経線に並行)
  for (i_div_y in 1:div.num) {
    temp.quad.coord <- quad.coordEQA[quad.coordEQA$lon > min.y + div.dist.y * (i_div_y - 1) & quad.coordEQA$lon < min.y + div.dist.y * i_div_y,]
    rect(min.y + div.dist.y * (i_div_y - 1),
         min.x,
         min.y + div.dist.y * i_div_y,
         max(grid.coord$x),
         border = 3, lwd = 1.5)
    text((min.y + div.dist.y * (i_div_y - 1) + min.y + div.dist.y * i_div_y)/2,
         (min.x + max(grid.coord$x))/2,
         paste("lon",i_div_y,sep=""),
         col=3,font=2,cex=1.5)
    write.csv(temp.quad.coord, file = paste(wdpath,"/lon",i_div_y,"quad_coordinate.csv",sep=""))
    write.gpx(temp.quad.coord, file = paste(wdpath,"/lon",i_div_y,"quad_coordinate.gpx",sep=""))
    setTxtProgressBar(pb, i_div_y) 
  }
  
}else if(div.style ==3){
  # 格子状に分割する場合
  for (i_div_x in 1:div.num) {
    for (i_div_y in 1:div.num) {
      temp.quad.coord <- quad.coordEQA[quad.coordEQA$lat > min.x + div.dist.x * (i_div_x - 1) & quad.coordEQA$lat < min.x + div.dist.x * i_div_x &
                                         quad.coordEQA$lon > min.y + div.dist.y * (i_div_y - 1) & quad.coordEQA$lon < min.y + div.dist.y * i_div_y,]
      rect(min.y + div.dist.y * (i_div_y - 1),
           min.x + div.dist.x * (i_div_x - 1),
           min.y + div.dist.y * i_div_y,
           min.x + div.dist.x * i_div_x,
           border = 3, lwd = 1.5)
      text((min.y + div.dist.y * (i_div_y - 1) + min.y + div.dist.y * i_div_y)/2,
           (min.x + div.dist.x * (i_div_x - 1) + min.x + div.dist.x * i_div_x)/2,
           paste("lat",i_div_x,"_lon",i_div_y,sep=""),
           col=3,font=2,cex=1.5)
      write.csv(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"_lon",i_div_y,"quad_coordinate.csv",sep=""))
      write.gpx(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"_lon",i_div_y,"quad_coordinate.gpx",sep=""))
      setTxtProgressBar(pb, i_div_x) 
    }
  }
}else {
  stop("Please define div.style as an integer from 1 to 3")
}

paste("quad:",nrow(temp.quad.coord),", cells:",nrow(temp.quad.coord)/8 ,sep="")

