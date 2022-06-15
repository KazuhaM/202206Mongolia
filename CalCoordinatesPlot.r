
# Init --------------------------------------------------------------------
wdpath <- "C:/Users/niedo/OneDrive/SL/2022/202206モンゴル調査/Pre/GPX"
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
                            3,c("id","x","y"))

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

# Cal quad coordinates -----------------------------------------------------

# test
plot(grid.coord$x,grid.coord$y,cex = 0.4, col = 1,bg=1, pch = 21)


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
    quad.coord$x[i_quad] <- (1-quad.rate) * i_x1 + quad.rate * i_x2
    quad.coord$x[i_quad + 1] <- (1 - quad.rate) * i_x1 + quad.rate * i_x2
    quad.coord$x[i_quad + 2] <- quad.rate * i_x1 + (1 - quad.rate) * i_x2
    quad.coord$x[i_quad + 3] <- quad.rate * i_x1 + (1 - quad.rate) * i_x2
    
    # 内周のコドラートのx座標
    quad.coord$x[i_quad + 4] <- (1-quad.rate.in) * i_x1 + quad.rate.in * i_x2
    quad.coord$x[i_quad + 5] <- (1 - quad.rate.in) * i_x1 + quad.rate.in * i_x2
    quad.coord$x[i_quad + 6] <- quad.rate.in * i_x1 + (1 - quad.rate.in) * i_x2
    quad.coord$x[i_quad + 7] <- quad.rate.in * i_x1 + (1 - quad.rate.in) * i_x2
    
    # 外周のコドラートのy座標
    quad.coord$y[i_quad] <- (1-quad.rate) * i_y1 + quad.rate * i_y3
    quad.coord$y[i_quad + 1] <- quad.rate * i_y1 + (1 - quad.rate) * i_y3
    quad.coord$y[i_quad + 2] <- (1 - quad.rate) * i_y1 + quad.rate * i_y3
    quad.coord$y[i_quad + 3] <- quad.rate * i_y1 + (1 - quad.rate) * i_y3
    
    # 内周のコドラートのy座標
    quad.coord$y[i_quad + 4] <- (1-quad.rate.in) * i_y1 + quad.rate.in * i_y3
    quad.coord$y[i_quad + 5] <- quad.rate.in * i_y1 + (1 - quad.rate.in) * i_y3
    quad.coord$y[i_quad + 6] <- (1 - quad.rate.in) * i_y1 + quad.rate.in * i_y3
    quad.coord$y[i_quad + 7] <- quad.rate.in * i_y1 + (1 - quad.rate.in) * i_y3
    
    # コドラートのid設定
    quad.coord$id[i_quad] <- i_quad 
    quad.coord$id[i_quad + 1] <- i_quad + 1
    quad.coord$id[i_quad + 2] <- i_quad + 2
    quad.coord$id[i_quad + 3] <- i_quad + 3
    quad.coord$id[i_quad + 4] <- i_quad + 4
    quad.coord$id[i_quad + 5] <- i_quad + 5
    quad.coord$id[i_quad + 6] <- i_quad + 6
    quad.coord$id[i_quad + 7] <- i_quad + 7
    
    # test
    # plot(cbind(c(i_x1,i_x2,i_x3,i_x4),c(i_y1,i_y2,i_y3,i_y4)))
    # points(quad.coord$x[1:8],quad.coord$y[1:8])
    # 
    # rect.cent(quad.coord$x[1:8],quad.coord$y[1:8],
    #           (max(grid.coord$x) - min(grid.coord$x)) * uav.h.range[1] / (grid.dist * grid.ncol),
    #           (max(grid.coord$y) - min(grid.coord$y)) * uav.h.range[2] / (grid.dist * grid.ncol))
    # abline(h = (i_y1 + i_y4) / 2)
    # abline(v = (i_x1 + i_x4) / 2)
    rect(i_x1,i_y1,i_x4,i_y4)
  }
}

points(quad.coord$x,quad.coord$y, cex = 0.4, col = 2, bg=1 , pch = 21)
rect.cent(quad.coord$x,quad.coord$y,
          (max(grid.coord$x) - min(grid.coord$x)) * uav.h.range[1] / (grid.dist * grid.ncol),
          (max(grid.coord$y) - min(grid.coord$y)) * uav.h.range[2] / (grid.dist * grid.ncol))

write.csv(quad.coord, file = paste(wdpath,"/quad_coordinate.csv",sep=""))


