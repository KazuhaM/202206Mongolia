# Parameter ---------------------------------------------------------------
# 基準点座標
ref.coord <- c(46.874073, 105.925902) # lat N, lon E
ref.loc <- 9 # 基準点の位置。下参照
#     4
# 5┌─────┐3
# 6│  1  │2
# 7└─────┘9
#     8

# 分割数
div.style <- 3 # 1: 短冊状(緯線に沿って横に), 2: 短冊状（経線に沿って縦に）, 3: 格子状
div.num <- 4 # 短冊状の場合は短冊の本数、格子状の場合は縦横それぞれの行数・列数

# Init --------------------------------------------------------------------
source("C:/Users/niedo/OneDrive/Documents/Rworks/Functions/createGPX.R")
wdpath <- "C:/Users/niedo/OneDrive/SL/2022/202206モンゴル調査/PreliminaryStudy/GPX"
setwd(wdpath)

grid.coord <- read.csv("Grid240_ponits_coordinate.csv",header = T)
grid.coord
# plot(grid.coord$x,grid.coord$y)

# サイト情報
grid.dist <- 240 # セルの一辺の長さ(m)
grid.ncol <- 33
grid.scale <- grid.dist*grid.ncol # サイトの一辺の長さ(m)
quad.num <- 8 # セルあたりのコドラート数

# ドローン設定
uav.h <- 40 # ドローンの測定高
uav.range <- c(165, 110) # ドローンの測定高100mのときの撮影範囲(m)

# 結果格納用データフレーム
quad.coord <- createEmptyDf(grid.ncol^2 * 8,
                            5,c("id","lat","lon","name","sym"))

# Functions ---------------------------------------------------------------
# あるデータフレームにおいて、x, yが特定の番号のときの行番取得
find.xy <- function(dataframe, idx, idy){
  if(is.integer(idx) & is.integer(idy)){
    stop("idx and idx need to be integer.")
  }
  result <- intersect(which(dataframe$idx == idx),
                      which(dataframe$idy == idy))
  if(length(result) > 1){
    stop("the idx and idy match some row of the detaframe. please check the dataframe")
  }
  return(result)
}

rect.cent <- function(x, y, w, h){
  if(length(h) != 1 | length(w) != 1){
    stop("height and width need to be single number")
  }
  result.x1 <- x - w/2
  result.x2 <- x + w/2
  result.y1 <- y - h/2
  result.y2 <- y + h/2
  
  rect(result.x1, result.y1, result.x2, result.y2)
}


# Cal Drone scale ---------------------------------------------------------
uav.h.range <- uav.range * uav.h / 100 


# Cal ratio of quad's coordinate in a cell --------------------------------
sp <- (grid.dist / 4 - (uav.h.range[1] + uav.h.range[2]) / 2) / 2 # ドローン画像とセルの4分割との隙間
tx <- uav.h.range[1] / 2 + sp # セルの端から、コドラートの中央までの距離
t <- sqrt(2) * tx # セルの端点から、斜辺上におけるコドラート中央までの距離
nt <- sqrt(2) * grid.dist / 2 - 2 * t # セル4分割内で隣り合うコドラート間の距離（中央同士）
quad.rate <- t / (sqrt(2) * grid.dist) # セルの斜辺に対して、セルの端点から、斜辺上におけるコドラート中央までの距離の比
quad.rate.in <- 0.5 - 2 * quad.rate + quad.rate # セルの斜辺に対して、内側のコドラートまでの距離の比


# Site shift by reference point --------------------------------------------
grid.range <- c(max(grid.coord$x) - min(grid.coord$x),
                max(grid.coord$y) - min(grid.coord$y))
# 左下を0,0にする
grid.coord$y <- grid.coord$y - 100.5112526
#     4
# 5┌─────┐3
# 6│  1  │2
# 7└─────┘9
#     8
shft <- switch(ref.loc,
               "1" = ref.coord - c(grid.range[1]/2, grid.range[2]/2),
               "2" = ref.coord - c(grid.range[1]/2, grid.range[2]),
               "3" = ref.coord - c(grid.range[1], grid.range[2]),
               "4" = ref.coord - c(grid.range[1], grid.range[2]/2),
               "5" = ref.coord - c(grid.range[1], 0),
               "6" = ref.coord - c(grid.range[1]/2, 0),
               "7" = ref.coord,
               "8" = ref.coord - c(0, grid.range[2]/2),
               "9" = ref.coord - c(0, grid.range[2]),
               stop("Please enter an integer (from 1 to 9) to reg.loc")
  
)
grid.coord$x <- grid.coord$x + shft[1]
grid.coord$y <- grid.coord$y + shft[2]


# Cal quad coordinates -----------------------------------------------------

# test
plot(grid.coord$y,grid.coord$x,cex = 0.4, col = 1,bg=1, pch = 21)


for (i_x in 1:grid.ncol - 1) { # セルNoのx方向の番号 1:grid.ncol
  for (i_y in 1:grid.ncol - 1) { # セルNoのy方向の番号 1:grid.ncol
    # セルのNo 1:grid.ncol^2
    i_cell <- i_x * grid.ncol + i_y + 1
    
    # あるセルの四方の座標を取得
    # (i_x3,i_y3)ー(i_x4,i_y4)
    # ｜                    ｜
    # (i_x1,i_y1)ー(i_x2,i_y2)
    i_x1 <- grid.coord$x[find.xy(grid.coord,i_x,i_y)]
    i_x2 <- grid.coord$x[find.xy(grid.coord,i_x+1,i_y)]
    i_x3 <- grid.coord$x[find.xy(grid.coord,i_x,i_y+1)]
    i_x4 <- grid.coord$x[find.xy(grid.coord,i_x+1,i_y+1)]
    i_y1 <- grid.coord$y[find.xy(grid.coord,i_x,i_y)]
    i_y2 <- grid.coord$y[find.xy(grid.coord,i_x+1,i_y)]
    i_y3 <- grid.coord$y[find.xy(grid.coord,i_x,i_y+1)]
    i_y4 <- grid.coord$y[find.xy(grid.coord,i_x+1,i_y+1)]
    
    # 全体のコドラートのうち、今いるセルの1番目のコドラートのNo.を計算
    i_quad <- (i_cell - 1) * 8 + 1 
    
    # 各コドラートの座標を計算
    # 外周のコドラートのx座標
    quad.coord$lat[i_quad] <- (1-quad.rate) * i_x1 + quad.rate * i_x2
    quad.coord$lat[i_quad + 1] <- (1 - quad.rate) * i_x1 + quad.rate * i_x2
    quad.coord$lat[i_quad + 2] <- quad.rate * i_x1 + (1 - quad.rate) * i_x2
    quad.coord$lat[i_quad + 3] <- quad.rate * i_x1 + (1 - quad.rate) * i_x2
    
    # 内周のコドラートのx座標
    quad.coord$lat[i_quad + 4] <- (1-quad.rate.in) * i_x1 + quad.rate.in * i_x2
    quad.coord$lat[i_quad + 5] <- (1 - quad.rate.in) * i_x1 + quad.rate.in * i_x2
    quad.coord$lat[i_quad + 6] <- quad.rate.in * i_x1 + (1 - quad.rate.in) * i_x2
    quad.coord$lat[i_quad + 7] <- quad.rate.in * i_x1 + (1 - quad.rate.in) * i_x2
    
    # 外周のコドラートのy座標
    quad.coord$lon[i_quad] <- (1-quad.rate) * i_y1 + quad.rate * i_y3
    quad.coord$lon[i_quad + 1] <- quad.rate * i_y1 + (1 - quad.rate) * i_y3
    quad.coord$lon[i_quad + 2] <- (1 - quad.rate) * i_y1 + quad.rate * i_y3
    quad.coord$lon[i_quad + 3] <- quad.rate * i_y1 + (1 - quad.rate) * i_y3
    
    # 内周のコドラートのy座標
    quad.coord$lon[i_quad + 4] <- (1-quad.rate.in) * i_y1 + quad.rate.in * i_y3
    quad.coord$lon[i_quad + 5] <- quad.rate.in * i_y1 + (1 - quad.rate.in) * i_y3
    quad.coord$lon[i_quad + 6] <- (1 - quad.rate.in) * i_y1 + quad.rate.in * i_y3
    quad.coord$lon[i_quad + 7] <- quad.rate.in * i_y1 + (1 - quad.rate.in) * i_y3
    
    # コドラートのid設定
    quad.coord$id[i_quad:(i_quad + 7)] <- i_quad:(i_quad + 7)
    # quad.coord$id[i_quad] <- i_quad 
    # quad.coord$id[i_quad + 1] <- i_quad + 1
    # quad.coord$id[i_quad + 2] <- i_quad + 2
    # quad.coord$id[i_quad + 3] <- i_quad + 3
    # quad.coord$id[i_quad + 4] <- i_quad + 4
    # quad.coord$id[i_quad + 5] <- i_quad + 5
    # quad.coord$id[i_quad + 6] <- i_quad + 6
    # quad.coord$id[i_quad + 7] <- i_quad + 7
    # 
    # コドラートの名称
    quad.coord$name[i_quad:(i_quad + 7)] <- paste("lat",i_x,"lon",i_y,"_q",1:8,sep="")
    # quad.coord$name[i_quad] <- paste(i_x,i_y,1,sep="_")
    # quad.coord$name[i_quad + 1] <- paste(i_x,i_y,2,sep="_")
    # quad.coord$name[i_quad + 2] <- paste(i_x,i_y,3,sep="_")
    # quad.coord$name[i_quad + 3] <- paste(i_x,i_y,4,sep="_")
    # quad.coord$name[i_quad + 4] <- paste(i_x,i_y,5,sep="_")
    # quad.coord$name[i_quad + 5] <- paste(i_x,i_y,6,sep="_")
    # quad.coord$name[i_quad + 6] <- paste(i_x,i_y,7,sep="_")
    # quad.coord$name[i_quad + 7] <- paste(i_x,i_y,8,sep="_")
    
    # マーカー形状
    quad.coord$sym[i_quad:(i_quad + 7)] <- "Flag, Red"
    
    # test
    # plot(cbind(c(i_x1,i_x2,i_x3,i_x4),c(i_y1,i_y2,i_y3,i_y4)))
    # points(quad.coord$x[1:8],quad.coord$y[1:8])
    # 
    # rect.cent(quad.coord$x[1:8],quad.coord$y[1:8],
    #           (max(grid.coord$x) - min(grid.coord$x)) * uav.h.range[1] / (grid.dist * grid.ncol),
    #           (max(grid.coord$y) - min(grid.coord$y)) * uav.h.range[2] / (grid.dist * grid.ncol))
    # abline(h = (i_y1 + i_y4) / 2)
    # abline(v = (i_x1 + i_x4) / 2)
    rect(i_y1,i_x1,i_y4,i_x4)
  }
}

# 図の描画
points(quad.coord$lon,quad.coord$lat, cex = 0.4, col = 2, bg=1 , pch = 21)
rect.cent(quad.coord$lon,quad.coord$lat,
          (max(grid.coord$y) - min(grid.coord$y)) * uav.h.range[2] / (grid.dist * grid.ncol),
          (max(grid.coord$x) - min(grid.coord$x)) * uav.h.range[1] / (grid.dist * grid.ncol))
points(ref.coord[2],ref.coord[1],col=3,pch=23,bg=3,cex=1.2)

# Divide files ------------------------------------------------------------
min.x <- min(grid.coord$x) # 最も小さい緯度　（サイトの下端）
min.y <- min(grid.coord$y) # 最も小さい軽度　（サイトの左端）
div.dist.x <- grid.range[1] / div.num # 分割した場合の間隔
div.dist.y <- grid.range[2] / div.num # 分割した場合の間隔
if(div.style == 1){
  # 短冊状に横に分割する場合（緯線に並行）
  for (i_div_x in 1:div.num) {
      temp.quad.coord <- quad.coord[quad.coord$lat > min.x + div.dist.x * (i_div_x - 1) & quad.coord$lat < min.x + div.dist.x * i_div_x,]
      rect(min.y,
           min.x + div.dist.x * (i_div_x - 1),
           max(grid.coord$y),
           min.x + div.dist.x * i_div_x,
           border = 3, lwd = 1.5)
      write.csv(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"quad_coordinate.csv",sep=""))
      write.gpx(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"quad_coordinate.gpx",sep=""))
  }
  
}else if(div.style == 2){
  # 短冊状に縦に分割する場合(経線に並行)
  for (i_div_y in 1:div.num) {
    temp.quad.coord <- quad.coord[quad.coord$lon > min.y + div.dist.y * (i_div_y - 1) & quad.coord$lon < min.y + div.dist.y * i_div_y,]
    rect(min.y + div.dist.y * (i_div_y - 1),
         min.x,
         min.y + div.dist.y * i_div_y,
         max(grid.coord$x),
         border = 3, lwd = 1.5)
    write.csv(temp.quad.coord, file = paste(wdpath,"/lon",i_div_y,"quad_coordinate.csv",sep=""))
    write.gpx(temp.quad.coord, file = paste(wdpath,"/lon",i_div_y,"quad_coordinate.gpx",sep=""))
  }
  
}else if(div.style ==3){
  # 格子状に分割する場合
  for (i_div_x in 1:div.num) {
    for (i_div_y in 1:div.num) {
      temp.quad.coord <- quad.coord[quad.coord$lat > min.x + div.dist.x * (i_div_x - 1) & quad.coord$lat < min.x + div.dist.x * i_div_x &
                                      quad.coord$lon > min.y + div.dist.y * (i_div_y - 1) & quad.coord$lon < min.y + div.dist.y * i_div_y,]
      rect(min.y + div.dist.y * (i_div_y - 1),
           min.x + div.dist.x * (i_div_x - 1),
           min.y + div.dist.y * i_div_y,
           min.x + div.dist.x * i_div_x,
           border = 3, lwd = 1.5)
      write.csv(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"_lon",i_div_y,"quad_coordinate.csv",sep=""))
      write.gpx(temp.quad.coord, file = paste(wdpath,"/lat",i_div_x,"_lon",i_div_y,"quad_coordinate.gpx",sep=""))
    }
  }
}else {
  stop("Please define div.style as an integer from 1 to 3")
}

paste("quad:",nrow(temp.quad.coord),", cells:",nrow(temp.quad.coord)/8 ,sep="")





