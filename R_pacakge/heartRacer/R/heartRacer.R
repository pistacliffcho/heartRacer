#' @import fit 
#' @import RgoogleMaps 
#' @import animation
#' @import ggmap
#' @import ggplot2
#' @import gganimate
NULL


#' Upload fit files from device
#' 
#' @param device_location Location of device
#' @param destination Where saved files should go
#' 
#' @details Grabs new fit files from device and saves them locally
#' 
#' @export
upload_fit <- function(device_location = '~/../../Volumes/GARMIN/GARMIN/ACTIVITY',
                       destination = "~/Documents/fitFiles"){
  current_docs = list.files(destination)
  device_docs  = list.files(device_location)

  old_dir = getwd()
  setwd(device_location)  
  for(doc in device_docs){
    this_doc = fit::read.fit(doc)
    this_timeStamp = as.character(this_doc$session$timestamp)
    if(!(this_timeStamp %in% current_docs) ){
      save_name = paste(destination, this_timeStamp, '.Rdata', sep = "/")
      saveRDS(this_doc, file = save_name)
    }
  }
  setwd(old_dir)
}


#' Load n most recent fit files
#' 
#' @param filesLocation Folder contain location of files
#' @param n How many files to load
#' 
#' Loads in \code{n} most recent fit files at \code{filesLocation}. Assumes name is
#' is timestamp (i.e. saved by upload_fit)
#' @export
load_fits <- function(filesLocation = "~/Documents/fitFiles", 
                      n = 5){
  file_names = list.files(filesLocation)
  file_numbers = as.numeric(file_names)
  file_numbers = sort(file_numbers, decreasing = T)
  file_numbers = file_numbers[1:n]
  file_names   = as.character(file_numbers)
  
  old_dir = getwd()
  setwd(filesLocation)
  ans <- list()
  for(i in 1:n){
    ans[[i]] = readRDS(file_names[i])
  }
  setwd(old_dir)
  return(ans)
}




animate2d <- function(fit){
  record <- fit$record[,c("timestamp", 
                          "position_lat", 
                          "position_long",
                          "heart_rate")]
  has_na <- rowSums(is.na(record) ) > 0
  record <- record[!has_na, ]
  time <- record$timestamp
  time <- time - min(time)
  grid <- 1:round(max(time/6) - 1 ) * 6 
  
  raw_lat <- record$position_lat
  lat = approx(x = time, y = record$position_lat, 
               xout = grid)$y
  lon = approx(x = time, y = record$position_long, 
               xout = grid)$y
  hr  = approx(x = time, y = record$heart_rate, 
               xout = grid)$y
  hr_col = rep('black', length(hr))
  hr_col[hr > 110] <- 'darkblue'
  hr_col[hr > 130] <- 'purple'
  hr_col[hr > 150] <- 'orange'
  hr_col[hr > 165] <- 'red'
  
  plot(NA, xlim = range(lon), ylim = range(lat), 
       xlab = 'Longitude', ylab = 'Latitude')
  
  for(i in seq_along(grid)){
    points(lon[i], lat[i], col = hr_col[i], pch = 16, cex = .75)
    Sys.sleep(0.1)
  }
}

hrCuts <- list(`HR <= 110` = list(0, 'darkgrey'), 
               `110 < HR <= 130` = list(110, 'lightblue'), 
               `130 < HR <= 145` = list(130, 'green'), 
               `145 < HR <= 160` = list(145, 'orange'), 
                `HR > 160` = list(160, 'red'))

guess_lgd_location <- function(x, y){
  x_rng <- range(x)
  y_rng <- range(y)
  x_mid <- mean(x_rng)
  y_mid <- mean(y_rng)
  onLeft <- x < x_mid
  onTop  <- y > y_mid
  inBottomLeft <- sum(onLeft & !onTop)
  inTopLeft    <- sum(onLeft & onTop)
  inBottomRight <- sum(!onLeft & !onTop)
  inTopRight    <- sum(!onLeft & onTop)
  minCounts <- min(c(inBottomLeft, inTopLeft, inBottomRight, inTopRight))
  if(inBottomLeft == minCounts) return("bottomleft")
  if(inTopLeft == minCounts) return('topleft')
  if(inTopRight == minCounts) return('topright')
  if(inBottomRight == minCounts) reeturn('bottomright')
}

makeHRLegend <- function(ids, location = 'topleft'){
  n_ids <- length(ids) 
  lgdText <- c(ids,  names(hrCuts) )
  cols    <- rep('black', n_ids)
  for(i in seq_along(hrCuts)) cols = c(cols, hrCuts[[i]][[2]])
  pch = c(1:n_ids, rep(NA, length(hrCuts)))
  legend(location, legend = lgdText, 
         lwd = 2, col = cols, 
         pch = pch)
}

#' Convert Heart Rates to color
#' @param hr Heart Rate
#' @export
hr2col = function(hr){
  ans = rep(hrCuts[[1]][[2]], length(hr))
  for(i in 2:length(hrCuts)){
    ans[hr > hrCuts[[i]][[1]] ] = hrCuts[[i]][[2]]
  }
  return(ans)
}



#' Animate points for group of subjects
#' 
#' @param x Longitude
#' @param y Latitude
#' @param time Time is seconds. Should be minimized to 0
#' @param hrtRt Heart Rate
#' @param id Id correpsonding to other variables
#' @param speed_up How much faster should animation run than in reality?
#' @param ptsPerSec How many points plotted per second?
#' @param ani Is this an animation?
#' @param lgdLocation Where onshould the legend be plotted?
#' @param googMap Google map object to be plotted on
#' @param ... arguments that will be ignored. Only for compatibility with \code{RgoogleMaps::plotmap}
#' @export
race_points <- function(x, y, time, hrtRt, id, 
                        speed_up = 120, ptsPerSec = 3,
                        ani = TRUE, lgdLocation = NULL, googMap = NULL,
                        ggmap = NULL,
                        ...){
  useGoog = TRUE
  use_ggmap = TRUE
  if(is.null(googMap)) useGoog = FALSE
  if(is.null(use_ggmap)) use_ggmap = FALSE
  if(is.null(lgdLocation)) lgdLocation = guess_lgd_location(x, y)
  hr <- hrtRt
  ptsPrSec = 3
  time_rng <- range(time)
  time_spn <- time_rng[2] - time_rng[1]
  grid     <- (1:(round(time_spn / speed_up * ptsPrSec))) * speed_up / ptsPrSec
  
  unq_id = unique(id)
  grd_vals = list()
  for(i in seq_along(unq_id)){
    this_id = unq_id[i]
    id_ind  = id == this_id
    this_time = time[id_ind]
    this_lon = approx(this_time, x[id_ind], grid)$y
    this_lat = approx(this_time, y[id_ind], grid)$y
    this_hr  = approx(this_time, hr[id_ind], grid)$y
    this_data <- data.frame(time  = grid, 
                            lon   = this_lon, 
                            lat   = this_lat, 
                            hrCol = hr2col(this_hr), 
                            stringsAsFactors = F)
    grd_vals[[i]] <- this_data
  }
  
  saveGIF(movie.name = "~/Documents/heartRace.gif", expr = {
    ani.options(interval = 0.1, nmax = length(grid))
    
    for(i in seq_along(grid)){
      if(!useGoog){
        plot(x, y, col = NA, 
             xlab = "Longitude", 
             ylab = 'Latitude')
      }
       else{ 
         map <- PlotOnStaticMap(googMap) 
       }
      if(!is.na(lgdLocation) & !useGoog){ 
        makeHRLegend(unq_id, lgdLocation) 
      }
      for(j in seq_along(unq_id)){
        df <- grd_vals[[j]]
        plot_list <- list(x = df$lon[1:i], 
                          y = df$lat[1:i], 
                          pch = j, 
                          col = df$hrCol[1:i], 
                          lwd = 2, 
                          cex = .75)
        if(!useGoog) do.call(colored_lines, plot_list)
        if(useGoog){
          plot_list$FUN = colored_lines
          plot_list$MyMap = googMap
          plot_list$lat = df$lat[1:i]
          plot_list$lon = df$lon[1:i]
#          plot_list$add = TRUE
          do.call(PlotOnStaticMap, plot_list)
        }
        if(i > 10){
          p_max_ind = floor(i / 10)
          p_inds <- 1:p_max_ind * 10
          plot_list <- list(x = df$lon[p_inds], 
                        y = df$lat[p_inds], 
                        pch = j, 
                        col = df$hrCol[p_inds], 
                        lwd = 2, 
                        cex = 1.5)
          if(!useGoog) do.call(points, plot_list)
          # if(useGoog){
          #   plot_list$FUN = points
          #   plot_list$MyMap = map
          #   do.call(PlotOnStaticMap, plot_list)
          # }
        }
      }
    ani.pause()
    }
  })
}

#' Awesome function for being awesome
#' @export
heartRace <- function(recordList, 
                      zoom = 14, 
                      map_type = 'terrain', 
                      nudgeLatBy = 0,
                      nudgeLonBy = 0
                      ){
  lon  = NULL
  lat  = NULL 
  hr   = NULL
  id   = NULL
  time = NULL
  
  ids = names(recordList)
  if(is.null(ids)) ids = seq_along(recordList)
  for(i in seq_along(ids)){
    this_id = as.character( ids[i] )
    this_data <- recordList[[i]][,c("timestamp", 
                                     "position_lat", 
                                     "position_long",
                                     "heart_rate")]
    drop <- rowSums(is.na(this_data)) > 0
    this_data <- this_data[!drop, ]
    nvals <- nrow(this_data)
    this_time <- this_data$timestamp
    this_time <- this_time - min(this_time)
    time <- c(time, this_time)
    this_lon =  this_data$position_long + (nudgeLonBy * (i - 1))
    lon  <- c(lon, this_lon)  
    this_lat = this_data$position_lat + (nudgeLatBy * (i - 1))
    lat  <- c(lat, this_lat) 
    hr   <- c(hr, this_data$heart_rate)
    id   <- c(id, rep(this_id, nvals))
  }
  pos <- c(lon = mean(lon),
             lat = mean(lat))
  mp <- ggmap::get_map(pos, zoom = zoom, 
                         maptype = map_type)
  gmap <- ggmap(mp, zoom = zoom)
  ans <- gg_race_points(lon, lat, time, hrtRt = hr, 
                   id, ggMap = gmap)
    return(ans)
}


colored_lines <- function(x, y, col_vec,...){
  k = length(x) - 1
  for(i in 1:k){
    lines(x[i:(i+1)], y[i:(i+1)], 
          col = col_vec[i], lwd = 3)
  }
}

hr_2_colors <- function(hr){
  color_fun = colorRampPalette( c('lightblue', 
                                  'green', 'orange', 
                                  'red'))
  refColVec <- color_fun(100)
  col_vec <- rep('grey', length(hr))
  has_color <- hr > 110
  col_number <- (hr - 110)/70 * 99 + 1
  col_number[col_number > 100] = 100
  col_number = round(col_number)
  col_vec[has_color] <- refColVec[col_number[has_color]]
  return(col_vec)
}

#' Plot a single fit file
#' @export
plotMyRun <- function(fit_file, zoom = 14, 
                      map_type = 'terrian'){
  record <- fit_file$record
  hr <- record$heart_rate
  lat <- record$position_lat
  long <- record$position_long
  center <- c(mean(lat), mean(long))
  map <- plotmap(lat = lat, 
                  lon = long,
                  zoom = zoom,
                  maptype = map_type,
                  FUN = colored_lines,
                  col_vec = hr_2_colors(hr))
  return(map)
}



#' Add a workout to map
#' 
#' @param gmap ggmap object
#' @param rcd A record from 
#' @export
add_gg_run <- function(gmap, rcd, size = 1, pch = 1){
  these_cols <- as.character(rcd$hrCol)
  gmap + geom_point(data = rcd,
                    aes(x = position_long,
                        y = position_lat),
                    col = these_cols, 
                    size = size,
                    pch = pch)
}



#' Animate points for group of subjects using ggmap
#' 
#' @param x Longitude
#' @param y Latitude
#' @param time Time is seconds. Should be minimized to 0
#' @param hrtRt Heart Rate
#' @param id Id correpsonding to other variables
#' @param speed_up How much faster should animation run than in reality?
#' @param ptsPerSec How many points plotted per second?
#' @param ggMap map to be plotted on
#' @param ... arguments that will be ignored. Only for compatibility with \code{RgoogleMaps::plotmap}
#' @export
gg_race_points <- function(x, y, time, hrtRt, id, 
                        speed_up = 120, ptsPerSec = 3,
                        ggMap = NULL,
                        ...){
  hr <- hrtRt
  ptsPrSec = 3
  time_rng <- range(time)
  time_spn <- time_rng[2] - time_rng[1]
  grid     <- (1:(round(time_spn / speed_up * ptsPrSec))) * speed_up / ptsPrSec
  
  unq_id = unique(id)
  grd_vals = list()
  for(i in seq_along(unq_id)){
    this_id = unq_id[i]
    id_ind  = id == this_id
    this_time = time[id_ind]
    this_lon = approx(this_time, x[id_ind], grid)$y
    this_lat = approx(this_time, y[id_ind], grid)$y
    this_hr  = approx(this_time, hr[id_ind], grid)$y
    this_data <- data.frame(time  = grid,
                            lon   = this_lon,
                            lat   = this_lat,
                            hrCol = hr2col(this_hr),
                            stringsAsFactors = F, 
                            id = this_id)
    grd_vals[[i]] <- this_data
  }
  
  full_data <- grd_vals[[1]]
  if(length(grd_vals) > 1){
    for(i in 2:length(grd_vals)){
      full_data <- rbind(full_data, grd_vals[[i]])
    }
  }
  hasNA <- rowSums(is.na(full_data)) > 0
  df <- full_data[!hasNA, ]
  ggMap <- ggMap + 
            geom_point(data = df, 
                        aes(x = lon, 
                            y = lat, 
                            frame = time,
                            cumulative = TRUE),
                       col = df$hrCol, 
                       pch = as.numeric(factor(df$id)) + 15,
                       size = 3) 
  return(ggMap)
  
  # 
  # saveHTML(movie.name = "~/Documents/heartRace.html", expr = {
  #   ani.options(interval = 0.1, nmax = length(grid))
  #   
  #   for(i in seq_along(grid)){
  #     this_map <- ggMap
  #     for(j in seq_along(unq_id)){
  #       df <- grd_vals[[j]][i,]
  #       this_map <- add_gg_run(this_map, df, size = 2)
  #       if(i > 10){
  #         p_max_ind = floor(i / 10)
  #         p_inds <- 1:p_max_ind * 10
  #         df <- df[p_inds,]
  #         this_map <- add_gg_run(this_map, df, size = 4, pch = j)
  #       }
  #     }
  #     this_map
  #     ani.pause()
  #   }
  # })
}
  