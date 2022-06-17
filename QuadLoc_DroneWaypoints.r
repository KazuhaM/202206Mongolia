# セル内コドラート配置図 -------------------------------------------------------------
acell <- cbind(c(grid.coord$y[find.xy(grid.coord,i_x,i_y)],
                 grid.coord$y[find.xy(grid.coord,i_x+1,i_y)],
                 grid.coord$y[find.xy(grid.coord,i_x,i_y+1)],
                 grid.coord$y[find.xy(grid.coord,i_x+1,i_y+1)]),
               c(grid.coord$x[find.xy(grid.coord,i_x,i_y)],
                 grid.coord$x[find.xy(grid.coord,i_x+1,i_y)],
                 grid.coord$x[find.xy(grid.coord,i_x,i_y+1)],
                 grid.coord$x[find.xy(grid.coord,i_x+1,i_y+1)]))

plot(acell,xlab = "longitude",ylab = "latitude",cex.lab=1.5,pch=21,bg=1
     ,xaxt = "n",yaxt = "n",)
points(quad.coord[i_quad:(i_quad + 7),"lon"],quad.coord[i_quad:(i_quad + 7),"lat"],pch=21,col=2,bg=2)
text(quad.coord[i_quad:(i_quad + 7),"lon"],quad.coord[i_quad:(i_quad + 7),"lat"],1:8,cex=1.5)
rect(acell[1,1],acell[1,2],acell[4,1],acell[4,2])
lines(acell[c(1,4),1],acell[c(1,4),2])
lines(acell[c(2,3),1],acell[c(2,3),2])
abline(h = (acell[1,2] + acell[4,2])/2)
abline(v = (acell[1,1] + acell[4,1])/2)
rect.cent(quad.coord[i_quad:(i_quad + 7),"lon"],quad.coord[i_quad:(i_quad + 7),"lat"],
          w = (max(grid.coord$y) - min(grid.coord$y)) * uav.h.range[1] / (grid.dist * grid.ncol),
          h = (max(grid.coord$x) - min(grid.coord$x)) * uav.h.range[2] / (grid.dist * grid.ncol),
          border=4, lwd = 1.5)



################ CalCoordinatesPlotを実行してから実行すること##################
# セル内ドローンway point計算用 -----------------------------------------------------
# quad.coord[i_quad:(i_quad + 7),]
cell.cent <- c((acell[1,1]+acell[4,1])/2,
               (acell[1,2]+acell[4,2])/2)
# points(cell.cent[1],cell.cent[2],col=5,bg=5,pch=22)
edge.rate <- 0.1
cell.edge <- rbind(c(edge.rate * acell[1,1] + (1 - edge.rate) * acell[4,1], edge.rate * acell[1,2] + (1 - edge.rate) * acell[4,2]),
                   c(edge.rate * acell[2,1] + (1 - edge.rate) * acell[3,1], edge.rate * acell[2,2] + (1 - edge.rate) * acell[3,2]),
                   c(edge.rate * acell[4,1] + (1 - edge.rate) * acell[1,1], edge.rate * acell[4,2] + (1 - edge.rate) * acell[1,2]),
                   c(edge.rate * acell[3,1] + (1 - edge.rate) * acell[2,1], edge.rate * acell[3,2] + (1 - edge.rate) * acell[2,2]))

cell.edge <- rbind(cell.edge,
                   c((0.5 - edge.rate) * acell[2,1] + (0.5 + edge.rate) * acell[3,1], (0.5 - edge.rate) * acell[2,2] + (0.5 + edge.rate) * acell[3,2]),
                   c((0.5 - edge.rate) * acell[4,1] + (0.5 + edge.rate) * acell[1,1], (0.5 - edge.rate) * acell[4,2] + (0.5 + edge.rate) * acell[1,2]))
colnames(cell.edge) <- c("lon","lat")
drone.waypoints <- rbind(cell.cent,
                         quad.coord[i_quad + 7, 3:2],
                         quad.coord[i_quad + 3, 3:2],
                         cell.edge[1:2,],
                         quad.coord[i_quad + 1, 3:2],
                         quad.coord[i_quad + 5, 3:2],
                         cell.edge[5:6,],
                         quad.coord[i_quad + 4, 3:2],
                         quad.coord[i_quad, 3:2],
                         cell.edge[3:4,],
                         quad.coord[i_quad + 2, 3:2],
                         quad.coord[i_quad + 6, 3:2]
                         )
row.names(drone.waypoints) <- 1:nrow(drone.waypoints)
points(drone.waypoints$lon, drone.waypoints$lat,col = 5, bg = 5, pch = 22)
drone.waypoints$type <- c("center","p8","p4","edgeNE",
                          "edgeSE","p2","p6","edgeCE",
                          "edgeCW","p5","p1","edgeSW",
                          "edgeNW","p3","p7")
write.csv(drone.waypoints, file = paste(dronepath,"/DroneWaypoints_coordinateUTM.csv",sep=""),row.names=F)
