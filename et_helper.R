## get number of header lines
get_smi_header <- function(file_name, n = 100) {
  f <- file(file_name)
  header <- readLines(f, n=n)
  firstline <- seq(1:n)[!str_detect(header, "## ")][1]
  resolution <- header[str_detect(header, "## Calibration Area:\t")]
  samp_rate <- header[str_detect(header, "## Sample Rate:\t")]
  
    
  file_data <- data_frame(file_name = file_name, 
                          header_len = firstline - 1,
                          samp_rate = to.n(str_split(samp_rate, "\t")[[1]][2]),
                          width = to.n(str_split(resolution, "\t")[[1]][2]),
                          height = to.n(str_split(resolution, "\t")[[1]][3]))
  close(f)
  return(file_data)
}

## convert to number
to.n <- function(x) {
  as.numeric(as.character(x))
}

################################################################################
## IDF CONVERT
## read in data file from SMI tracker's IDF converter utility
## major issue is that new stimuli are marked as new "events" 
## need to be converted to their own column
##
## adapted from aen and dy
################################################################################

read_smi_idf <- function (info) {
  
  # ## read the header from the file to paste back into the new file
  # header <- scan(file.name, what = character(),
  #                nlines=header.rows, sep="\n", quiet=TRUE)
  
  ## DATA CLEANING 
  # read in data and get rid of header rows
  all.d <- suppressWarnings(read_tsv(info$file_name, skip=info$header_len, guess_max = 5000, col_types = cols()))
  
  ## split data into messages and data
  ## First get data:
  d <- filter(all.d, Type == "SMP") %>%
    rename(lx = `L Raw X [px]`,
           rx = `R Raw X [px]`,
           ly = `L Raw Y [px]`,
           ry = `R Raw Y [px]`, 
           t = Time) %>%
    select(t, lx, ly, rx, ry) %>%
    mutate(lx = as.numeric(lx)) # corrects the weirdness of storing events in this column
  

  ## Now get "messages" - about the stimulus that's being presented
  msgs <- filter(all.d, Type=="MSG") %>%
    select(Time, `L Raw X [px]`) %>%
    rename(t = Time, 
           stimulus = `L Raw X [px]`) %>%
    mutate(stimulus = str_replace(stimulus, "# Message: ", ""), 
           trial = 1:n())
  
  ## merge these in
  find_stim <- function(x) {
    stim_set <- msgs$stimulus[msgs$t < x]
    stim <- stim_set[length(stim_set)]
    
    if (length(stim)==0) { 
      return(NA) 
    } else {
      return(stim)
    }
  }
  
  find_trial <- function(x) {
    trial_set <- msgs$trial[msgs$t < x]
    trial <- trial_set[length(trial_set)]
    
    if (length(trial)==0) { 
      return(NA) 
    } else {
      return(trial)
    }
  }
  
  d$stimulus <- d$t %>% map_chr(find_stim)
  d$trial <- d$t %>% map_int(find_trial) 
  
  ## drop the times before the first video
  d <- d %>% 
    filter(!is.na(stimulus)) %>%
    mutate(width = info$width, 
           height = info$height,
           samp_rate = info$samp_rate, 
           file_name = info$file_name)
  
  return(d)
}

################################################################################
## PREPROCESS DATA 
## take data file with l and r, x and y, as well as stimulus, average
## eyes, do whatever preprocessing needs to be done. 
##
## note that this assumes that each stimulus is unique (none are shown twice)
################################################################################

preprocess_data <- function(d, avg_eyes=TRUE) {
  
  ## average the eyes
  if (avg_eyes) {
    # round to the nearest pixel
    d$x <- round(rowMeans(d[,c("lx","rx")], na.rm=TRUE))
    d$y <- round(rowMeans(d[,c("ly","ry")], na.rm=TRUE))
    d <- d[, !(names(d) %in% c("lx","rx","ly","ry"))]
  }
  
  ## clip off out of range numbers
  d$x[d$x <= 0 | d$x >= d$width] <- NA
  d$y[d$y <= 0 | d$y >= d$height] <- NA
  
  ## convert the time into seconds
  ## get stimulus-time
  ## round to the nearest sample  
  ## y flip (so origin is cartesian, not matrix (bottom left, instead of top left)
  d <- d %>% 
    mutate(t = round((d$t - d$t[1])/(1000000), 3), 
           dt = c(diff(t),0)) %>%
    group_by(stimulus) %>%
    mutate(t_stim = round(cumsum(dt) * samp_rate)/samp_rate, 
           y = height - y) 
  
  return (d)
}

################################################################################
## ROI CHECK
## takes a list of ROIs as x, y, w, h boxes
## returns ROI number or NAN
################################################################################

roi.check <- function (d, rois) {
  roi <- factor(NA,levels=names(rois))
  
  for (i in 1:length(rois)) {
    r <- rois[[i]]
    roi[d$x > r[1] & d$x < r[1] + r[3] &
          d$y > r[2] & d$y < r[2] + r[4]] <- names(rois)[i]
  }
  
  return(roi)
}

################################################################################
## REZERO TRIALS
## create timestamps starting from the point of disambiguation
################################################################################

rezero.trials <- function (d, onset.name = "targetOnset") {
  d %>% 
    group_by(stimulus, subid) %>%
    mutate(t.crit = t.stim - targetOnset)
}
################################################################################
## ROI IMAGE1
## takes a list of ROIs as x, y, w, h boxes
## makes a picture so you can check them
################################################################################

roi.image <- function (rois,y.max=1050,x.max=1680) {
  plot(NA,xlim=c(0,x.max),ylim=c(0,y.max),bty="n",xlab="x",ylab="y")
  
  for (i in 1:length(rois)) {
    r <- rois[[i]]
    rect(r[1], r[2], r[1] + r[3], r[2] + r[4])
    text(r[1] + r[3]/2,
         r[2] + r[4]/2,
         names(rois)[i])
  }
}