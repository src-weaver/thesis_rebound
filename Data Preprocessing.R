# Import MCAv data
# install.packages("pacman")

library(pacman)

pacman::p_load(here, tidyverse, ggplot2, gridExtra, zoo, forecast, GGally, patchwork)

add.col<-function(df, new.col) {
  n.row<-dim(df)[1]
  length(new.col)<-n.row
  cbind(df, new.col)
}

# loop per data set (FUTURE: make into a function that can take time window and two vectors of filenames)

# import datasets - continuous method

# list file names
file_names <- list()
file_names$MCAv <- list.files(here("TCD_20Hz"))
file_names$PETCO2 <- list.files(here("Vyntus_Export"))

# table of signal strength/side choice
side_strength <- read_delim("TCD_side_strength.txt", delim = "\t")
side_strength[side_strength == "LEFT"] <- "lMCAv"
side_strength[side_strength == "RIGHT"] <- "rMCAv"

for (file in 1:length(file_names$MCAv)) {
  ID <- substr(file_names$MCAv[file], 1, 6)
  visit <- substr(file_names$MCAv[file], 8, nchar(file_names$MCAv[file]) - 4)
  side <- side_strength[[visit]][which(side_strength$ID == ID)]
  
  MCAv_temp <- read_delim(paste("TCD_20Hz/", file_names$MCAv[file], sep = ""), delim = "\t", 
                          col_names = c("time", "lMCAv", "rMCAv", "HR", "comment"))
  
  rest_rows <- which(grepl("rest", MCAv_temp$comment, ignore.case = T))
  rest_times <- MCAv_temp$time[rest_rows[1]]
  print(MCAv_temp$comment[rest_rows[1]])
  
  sit_rows <- which(grepl("sit", MCAv_temp$comment, ignore.case = T))
  sit_times <- MCAv_temp$time[sit_rows]
  
  if(file == 1){
    comm_timing <- MCAv_temp[c(rest_rows[1], sit_rows), 5]
  }
  
  comm_timing[[paste(ID, visit, sep = "_")]] <- MCAv_temp$time[c(rest_rows[1], sit_rows)]
  
  for (sprint in 1:length(sit_rows)){
    if(file == 1 && sprint == 1){
      cont_bouts <- MCAv_temp[(sit_rows[sprint]-1200):(sit_rows[sprint]+2399), 1]
      cont_bouts$time <- cont_bouts$time - MCAv_temp$time[sit_rows[sprint]]
    }
    
    col_heading <- paste(ID, visit, sprint, sep = "_")
    cont_bouts[[col_heading]] <- MCAv_temp[[side]][(sit_rows[sprint]-1200):(sit_rows[sprint]+2399)]
  }
  
  # Store 4Hz and B2b data in full, to compare to overlay interpolation
  if(file == 1){
    full_cont <- MCAv_temp[, c("time", side)]
    colnames(full_cont) <- c("time", paste(ID, visit, sep = "_"))
    
  }else{
    new_col <- MCAv_temp[[side]]
    
    full_cont <- add.col(full_cont, new_col)
    
    colnames(full_cont)[ncol(full_cont)] <- paste(ID, visit, sep = "_")
  }
}

cont_bouts <- cont_bouts %>% pivot_longer(cols = starts_with("SI"), names_to = "ID_visit_sprint", values_to = "MCAv") %>%
  separate(col = 2, into = c("Study", "ID", "visit", "bout"), sep = "_") %>%
  unite("ID", Study:ID, remove = T)

cont_bouts$visit <- factor(cont_bouts$visit, levels = c("80", "120", "160", "200"), ordered = T)
cont_bouts$bout <- factor(cont_bouts$bout, levels = c("1", "2", "3", "4"), ordered = T)

cont_bouts <- cont_bouts %>% arrange(ID, visit, bout, time)

cont_bouts <- cont_bouts %>% group_by(ID, visit, bout) %>% mutate(MCAv_10sec = rollmean(MCAv, k = 200, fill = NA), 
                                                                MCAv_30sec = rollmean(MCAv, k = 600, fill = NA))

# import data - B2B_method
# list file names
file_names <- list()

file_names$MCAv <- list.files(here("TCD_B2B"))
file_names$PETCO2 <- list.files(here("BxB_export"))

for(file in 1: length(file_names$MCAv)){
  ID <- substr(file_names$MCAv[file], 1, 6)
  visit <- substr(file_names$MCAv[file], 8, nchar(file_names$MCAv[file]) - 4)
  
  mcav_b2b <- read_delim(paste("TCD_B2B/", file_names$MCAv[file], sep = ""), delim = "\t")
  mcav_b2b <- separate(mcav_b2b, TimeDate, sep = " ", into = c("date", "time"), fill = "left")
  mcav_b2b <- separate(mcav_b2b,time, sep = ":", into = c("min", "sec"), fill = "left")
  mcav_b2b[c("min", "sec")] <- sapply(mcav_b2b[c("min", "sec")], as.numeric)
  mcav_b2b$min <- mcav_b2b$min * 60
  mcav_b2b <- mcav_b2b %>% rowwise() %>% mutate(time = sum(min, sec, na.rm=T))
  b2b_raw <- mcav_b2b %>% select(time, `Mean Pressure (cm/s)`)
  
  # Needs to find timing for comments from the comm_timing tibble
  for(bout in 2:nrow(comm_timing)){
    wind_zero <- b2b_raw %>% filter(time < (comm_timing[[paste(ID, visit, sep = "_")]][bout] + 0.4) & 
                                      time >= (comm_timing[[paste(ID, visit, sep = "_")]][bout] - 0.4))
    wind_zero <- wind_zero$time[1]
    
    wind_start <- b2b_raw %>% filter(time < (comm_timing[[paste(ID, visit, sep = "_")]][bout] - 60) & 
                                       time >= (comm_timing[[paste(ID, visit, sep = "_")]][bout] - 60.8))
    wind_start <- wind_start$time[1]
    
    wind_end <- b2b_raw %>% filter(time < (comm_timing[[paste(ID, visit, sep = "_")]][bout] + 120.8) & 
                                       time >= (comm_timing[[paste(ID, visit, sep = "_")]][bout] + 120))
    wind_end <- wind_end$time[1]
    
    # Extract same window either side of bout
    b2b_orig <- b2b_raw[which(b2b_raw$time == wind_start):which(b2b_raw$time == wind_end), ]
    colnames(b2b_orig) <- c("time", paste(ID, visit, bout - 1, sep = "_"))
    
    b2b_orig$time <- b2b_orig$time - wind_zero
    
    poss_out <- tsoutliers(b2b_orig[[2]])
    
    b2b_tso <- b2b_orig[-poss_out$index, ]
    
    if(nrow(b2b_tso) == 0){
      b2b_tso <- b2b_orig
    }
    
    b2b_4Hz_orig <- spline(b2b_orig$time, b2b_orig[[2]], 
                           n = round((b2b_orig$time[length(b2b_orig$time)] - b2b_orig$time[1]) / 0.25),
                           method = "natural") 
    
    b2b_4Hz_orig <- as.data.frame(b2b_4Hz_orig)
    
    if(nrow(b2b_4Hz_orig) > 720){
      if(which.min(abs(b2b_4Hz_orig$x)) > 240){
        start_4Hz <- which.min(abs(b2b_4Hz_orig$x)) - 240
      }else{
        start_4Hz <- 1
      }
      end_4Hz <- start_4Hz + 719
    }else{
      start_4Hz = 1
      end_4Hz = 720
    }
    
    b2b_4Hz_orig <- b2b_4Hz_orig[start_4Hz:end_4Hz,]
    colnames(b2b_4Hz_orig) <- c("time", paste (ID, visit, bout - 1, sep = "_"))
    
    b2b_4Hz_tso <- spline(b2b_tso$time, b2b_tso[[2]], 
                          n = round((b2b_tso$time[length(b2b_tso$time)] - b2b_tso$time[1]) / 0.25),
                          method = "natural")
    
    b2b_4Hz_tso <- as.data.frame(b2b_4Hz_tso)
    
    if(nrow(b2b_4Hz_tso) > 720){
      if(which.min(abs(b2b_4Hz_tso$x)) > 240){
        start_4Hz <- which.min(abs(b2b_4Hz_tso$x)) - 240
      }else{
        start_4Hz <- 1
      }
      end_4Hz <- start_4Hz + 719
    }else{
      start_4Hz = 1
      end_4Hz = 720
    }
    
    b2b_4Hz_tso <- b2b_4Hz_tso[start_4Hz:end_4Hz,]
    colnames(b2b_4Hz_tso) <- c("time", paste (ID, visit, bout - 1, sep = "_"))
    
    #If it's the first file and bout data to be extracted then create output variables, otherwise append values to exciting outputs
    if(file == 1 && bout == 2){
      # list of raw output (different lengths so can't be dataframe)
      raw_bouts <- list()
      raw_bouts[paste (ID, visit, bout - 1, "b2b_orig", sep = "_")] <- list(b2b_orig)
      raw_bouts[paste (ID, visit, bout - 1, "b2b_tso", sep = "_")] <- list(b2b_tso)
      
      # dataframe for orig and tso data
      b2b_bouts_orig <- b2b_4Hz_orig
      
      b2b_bouts_tso <- b2b_4Hz_tso
    
    }else{
      raw_bouts[paste (ID, visit, bout - 1, "b2b_orig", sep = "_")] <- list(b2b_orig)
      raw_bouts[paste (ID, visit, bout - 1, "b2b_tso", sep = "_")] <- list(b2b_tso)
      
      col_heading <- paste(ID, visit, bout - 1, sep = "_")
      b2b_bouts_orig[[col_heading]] <- b2b_4Hz_orig[[col_heading]]
      b2b_bouts_tso[[col_heading]] <- b2b_4Hz_tso[[col_heading]]
    }
  }
}

b2b_bouts_orig <- b2b_bouts_orig %>% pivot_longer(cols = starts_with("SI"), names_to = "ID_visit_bout", values_to = "MCAv") %>%
  separate(col = 2, into = c("Study", "ID", "visit", "bout"), sep = "_") %>%
  unite("ID", Study:ID, remove = T)

b2b_bouts_orig$visit <- factor(b2b_bouts_orig$visit, levels = c("80", "120", "160", "200"), ordered = T)

b2b_bouts_orig$bout <- factor(b2b_bouts_orig$bout, levels = c("1", "2", "3", "4"), ordered = T)

b2b_bouts_orig <- b2b_bouts_orig %>% arrange(ID, visit, bout, time)

b2b_bouts_orig <- b2b_bouts_orig %>% group_by(ID, visit, bout) %>% mutate(MCAv_smooth = rollmean(MCAv, k = 5, fill = NA))

b2b_bouts_tso <- b2b_bouts_tso %>% pivot_longer(cols = starts_with("SI"), names_to = "ID_visit_bout", values_to = "MCAv") %>%
  separate(col = 2, into = c("Study", "ID", "visit", "bout"), sep = "_") %>%
  unite("ID", Study:ID, remove = T)

b2b_bouts_tso$visit <- factor(b2b_bouts_tso$visit, levels = c("80", "120", "160", "200"), ordered = T)

b2b_bouts_tso$bout <- factor(b2b_bouts_tso$bout, levels = c("1", "2", "3", "4"), ordered = T)

b2b_bouts_tso <- b2b_bouts_tso %>% arrange(ID, visit, bout, time)

b2b_bouts_tso <- b2b_bouts_tso %>% group_by(ID, visit, bout) %>% mutate(MCAv_smooth = rollmean(MCAv, k = 5, fill = NA))


# import PETCO2 data - B2B_method
# list file names

for(file in 1: length(file_names$PETCO2)){
  ID <- substr(file_names$PETCO2[file], 1, 6)
  visit <- substr(file_names$PETCO2[file], 8, nchar(file_names$PETCO2[file]) - 8)
  
  petco2_bxb <- read_delim(paste("BxB_export/", file_names$PETCO2[file], sep = ""), delim = "\t")
  
  petco2_bxb <- petco2_bxb[2:nrow(petco2_bxb),]
  
  petco2_bxb <- separate(petco2_bxb, Time, sep = ":", into = c("min", "sec"), fill = "left")
  petco2_bxb[c("min", "sec")] <- sapply(petco2_bxb[c("min", "sec")], as.numeric)
  petco2_bxb$min <- petco2_bxb$min * 60
  petco2_bxb <- petco2_bxb %>% rowwise() %>% mutate(time = sum(min, sec, na.rm=T))
  
  # Find rest timing and adjust to match MCAv timing
  rest_row <- which(grepl("rest", petco2_bxb$`V'CO2`, ignore.case = T))
  
  time_adjust <- petco2_bxb$time[rest_row[1]] - comm_timing[[paste(ID, visit, sep = "_")]][1]
  
  petco2_bxb$time <- petco2_bxb$time - time_adjust
  
  # Adjust petco2 from kPa to mmHg
  petco2_bxb$PETCO2 <- as.numeric(petco2_bxb$PETCO2)
  
  petco2_bxb$PETCO2 <- petco2_bxb$PETCO2 * 7.50062
  
  # Isolate desired data and remove NA rows
  process_data <- petco2_bxb %>% select(time, PETCO2)
  
  process_data <- na.omit(process_data)
  
  # Interpolate data set
  process_data <- spline(process_data$time, process_data$PETCO2, 
                         n = round((process_data$time[length(process_data$time)] - process_data$time[1]) / 0.25))
  bxb_4Hz <- as.data.frame(process_data)
  
  for(bout in 2:nrow(comm_timing)){
    wind_time <- bxb_4Hz %>% filter(x < (comm_timing[[paste(ID, visit, sep = "_")]][bout] + 0.125) & 
                                      x >= (comm_timing[[paste(ID, visit, sep = "_")]][bout] - 0.125))
    wind_time <- wind_time$x[1]
    # Extract same window either side of bout
    if(file == 1 && bout == 2){
      bxb_bouts <- bxb_4Hz[(which(bxb_4Hz$x == wind_time) - 240):(which(bxb_4Hz$x == wind_time) + 479), ]
      colnames(bxb_bouts) <- c("time", paste(ID, visit, bout - 1, sep = "_"))
      
      bxb_bouts$time <- bxb_bouts$time - wind_time
    }else{
      col_heading <- paste(ID, visit, bout - 1, sep = "_")
      bxb_bouts[[col_heading]] <- bxb_4Hz$y[(which(bxb_4Hz$x == wind_time) - 240):(which(bxb_4Hz$x == wind_time) + 479)]
    }
  }
}

bxb_bouts <- bxb_bouts %>% pivot_longer(cols = starts_with("SI"), names_to = "ID_visit_bout", values_to = "PETCO2") %>%
  separate(col = 2, into = c("Study", "ID", "visit", "bout"), sep = "_") %>%
  unite("ID", Study:ID, remove = T)

bxb_bouts$visit <- factor(bxb_bouts$visit, levels = c("80", "120", "160", "200"), ordered = T)
bxb_bouts$bout <- factor(bxb_bouts$bout, levels = c("1", "2", "3", "4"), ordered = T)
bxb_bouts <- bxb_bouts %>% arrange(ID, visit, bout, time)
bxb_bouts <- bxb_bouts %>% group_by(ID, visit, bout) %>% mutate(PETCO2_smooth = rollmean(PETCO2, k = 5, fill = NA))


# Combined bxb/b2b data

b2xb_data <- b2b_bouts_orig
colnames(b2xb_data) <- c("time", "ID", "visit", "bout", "mcav_orig", "mcav_orig_smooth")

b2xb_data$mcav_tso <- b2b_bouts_tso$MCAv
b2xb_data$mcav_tso_smooth <- b2b_bouts_tso$MCAv_smooth

b2xb_data$petco2 <- bxb_bouts$PETCO2
b2xb_data$petco2_smooth <- bxb_bouts$PETCO2_smooth

