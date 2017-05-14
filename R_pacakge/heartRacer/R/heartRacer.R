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





hrCuts <- list(`HR <= 110` = list(0, 'darkgrey'), 
               `110 < HR <= 130` = list(110, 'lightblue'), 
               `130 < HR <= 145` = list(130, 'green'), 
               `145 < HR <= 160` = list(145, 'orange'), 
                `HR > 160` = list(160, 'red'))

getHR_cols <- function(hrCuts){
  ans <- NULL
  for(i in seq_along(hrCuts)) ans[i] = hrCuts[[i]][[2]]
  return(ans)
}


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

ls_approx <- function(x, y, eval_pts){
  span = 0.05
  if(length(x) < 100){
    span = 0.25
  }
  fit <- loess(y ~ x, span = span)
  yhats <- predict(fit, data.frame(x = eval_pts))
  ans <- data.frame(x = eval_pts, y = yhats)
  return(ans)
}


#' Animate fit files onto google maps
#' @param recordList A list of records from fit file
#' @param zoom Zoom option for google maps
#' @param map_type Type of map pulled from google maps
#' @param nudgeLatBy How much should each subject's latitude be moved by? 
#' @param nudgeLonBy How much should each subject's longitude be moved by?
#' @details 
#' Returns a ggplot object with the work out plotted on top of it. This can be animated
#' by using \code{gganimate::gganimate(hrMap)}, where \code{hrMap} is an object returned by heartRace. 
#' @export
heartRace <- function(recordList, 
                      zoom = 14, 
                      map_type = 'satellite', 
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
  these_cols <- as.character(rcd$HeartRate)
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
    this_lon = ls_approx(this_time, x[id_ind], grid)$y
    this_lat = ls_approx(this_time, y[id_ind], grid)$y
    this_hr  = ls_approx(this_time, hr[id_ind], grid)$y
    this_data <- data.frame(time  = grid,
                            lon   = this_lon,
                            lat   = this_lat,
                            HeartRate = hr2col(this_hr),
                            stringsAsFactors = T, 
                            id = this_id)
    grd_vals[[i]] <- this_data
  }
  
  # full_data <- grd_vals[[1]]
  # if(length(grd_vals) > 1){
  #   for(i in 2:length(grd_vals)){
  #     full_data <- rbind(full_data, grd_vals[[i]])
  #   }
  # }
  # hasNA <- rowSums(is.na(full_data)) > 0
  for(i in seq_along(grd_vals)){
    df <- grd_vals[[i]]
    hasNA <- rowSums(is.na(df)) > 0
    df <- df[!hasNA,]
    lon_end <- df$lon[-1]
    lat_end <- df$lat[-1]
    df <- df[-nrow(df), ]
    df$lon_end <- lon_end
    df$lat_end <- lat_end
    ggMap <- ggMap + 
              geom_segment(data = df, 
                          aes(x = lon, xend = lon_end, 
                              y = lat, yend = lat_end,
                              group = id,
                              color = HeartRate,
                              frame = time,
                              cumulative = TRUE),
                         size = 1.5) 
    pts_delay = 20
    if(nrow(df) > pts_delay){
      keep_ind <- 1:(floor(nrow(df) / pts_delay)) * pts_delay
      df_pt <- df[keep_ind, ]
      pt_type <- i + 15
      ggMap <- ggMap + geom_point(data = df_pt, 
                         aes(x = lon, y = lat,
                             frame = time, 
                             cumulative = TRUE,
                             col = HeartRate),
                         size = 3, 
                         pch = pt_type) +
        labs(x = "Longitude", y = "Latitude")
    }
  }
  
  ggMap <- ggMap + scale_color_manual(name = 'Heart Rate', 
                                      values = sort(getHR_cols(hrCuts)),
                                      breaks = getHR_cols(hrCuts), 
                                      labels = names(hrCuts)) 
  return(ggMap)
}
  