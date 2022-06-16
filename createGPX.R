write.gpx <- function(data,filename,colnames=c("lat","lon","name","sym")){
  init.info <- '<?xml version="1.0"?><gpx version="1.1" creator="GDAL 3.4.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">'
  meta.info <- paste('<metadata><bounds minlat="',min(data$lat),'" minlon="',min(data$lon),
                     '" maxlat="',max(data$lat),'" maxlon="',max(data$lon),'"/></metadata>',sep="")
  wpt.info <- ""
  for (i_row in 1:nrow(data)) {
    wpt.info <- paste(wpt.info,'<wpt lat="',data$lat[i_row],'" lon="',data$lon[i_row],'"><name>',data$name[i_row],'</name><sym>',data$sym[i_row],'</sym></wpt>\n',sep="")
  }
  result.text <- paste(init.info,'\n',meta.info,'\n',wpt.info,'</gpx>',sep="")
  cat(result.text,file=filename)
}
