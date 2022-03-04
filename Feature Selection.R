# Feature Selection

# These variables specify where the script looks for the first feature
earliest <- 0
latest <- 40

# unique ID, intensity and bouts
part_id <- unique(b2xb_data$ID)
visits <- unique(b2xb_data$visit)
bouts <- unique(b2xb_data$bout)

# loop through each participant, visit and bout, for each outcome measure within the given dataset
pb <- txtProgressBar(min = 0, max = length(part_id), style = 3)
for(curr_id in 1:length(part_id)){
  for(curr_visit in 1:length(visits)){
    for(curr_bout in 1: length(bouts)){
      indi_data <- b2xb_data %>% filter(ID == part_id[curr_id], visit == visits[curr_visit], bout == bouts[curr_bout])
      indi_cont <- cont_bouts %>% filter(ID == part_id[curr_id], visit == visits[curr_visit], bout == bouts[curr_bout])
      
      min_range <- indi_data %>% filter(between(time, earliest, latest))
      min_cont_range <- indi_cont %>% filter(between(time, earliest, latest))
      
      min_orig <- min_range[which.min(min_range[["mcav_orig"]]),]
      min_orig <- min_orig %>% rename(min_orig = mcav_orig, min_t_orig = time)
      min_orig <-  min_orig %>% relocate(min_t_orig, .before = min_orig)
      min_max <- min_orig[c("ID", "visit", "bout", "min_t_orig", "min_orig")]
      
      min_orig_smooth <- min_range[which.min(min_range[["mcav_orig_smooth"]]),]
      min_max <- min_max %>% add_column(min_t_orig_smooth = min_orig_smooth$time, min_orig_smooth = min_orig_smooth$mcav_orig_smooth)
      
      min_tso <- min_range[which.min(min_range[["mcav_tso"]]),]
      min_max <- min_max %>% add_column(min_t_tso = min_tso$time, min_tso = min_tso$mcav_tso)
      
      min_tso_smooth <- min_range[which.min(min_range[["mcav_tso_smooth"]]),]
      min_max <- min_max %>% add_column(min_t_tso_smooth = min_tso_smooth$time, min_tso_smooth = min_tso_smooth$mcav_tso_smooth)
      
      min_10s <- min_cont_range[which.min(min_cont_range[["MCAv_10sec"]]),]
      min_max <- min_max %>% add_column(min_t_10s = min_10s$time, min_10s = min_10s$MCAv_10sec)
      
      min_30s <- min_cont_range[which.min(min_cont_range[["MCAv_30sec"]]),]
      min_max <- min_max %>% add_column(min_t_30s = min_30s$time, min_30s = min_30s$MCAv_30sec)
      
      min_petco2 <- min_range[which.min(min_range[["petco2"]]),]
      min_max <- min_max %>% add_column(min_t_petco2 = min_petco2$time, min_petco2 = min_petco2$petco2)
      
      max_orig_range <- indi_data %>% filter(time > min_max$min_t_orig)
      max_orig <- max_orig_range[which.max(max_orig_range[["mcav_orig"]]),]
      min_max <- min_max %>% add_column(max_t_orig = max_orig$time, max_orig = max_orig$mcav_orig)
      
      max_orig_smooth_range <- indi_data %>% filter(time > min_max$min_t_orig_smooth)
      max_orig_smooth <- max_orig_smooth_range[which.max(max_orig_smooth_range[["mcav_orig_smooth"]]),]
      min_max <- min_max %>% add_column(max_t_orig_smooth = max_orig_smooth$time, max_orig_smooth = max_orig_smooth$mcav_orig_smooth)
      
      max_tso_range <- indi_data %>% filter(time > min_max$min_t_tso)
      max_tso <- max_tso_range[which.max(max_tso_range[["mcav_tso"]]),]
      min_max <- min_max %>% add_column(max_t_tso = max_tso$time, max_tso = max_tso$mcav_tso)
      
      max_tso_smooth_range <- indi_data %>% filter(time > min_max$min_t_tso_smooth)
      max_tso_smooth <- max_tso_smooth_range[which.max(max_tso_smooth_range[["mcav_tso_smooth"]]),]
      min_max <- min_max %>% add_column(max_t_tso_smooth = max_tso_smooth$time, max_tso_smooth = max_tso_smooth$mcav_tso_smooth)
      
      max_10s_range <- indi_cont %>% filter(time > min_max$min_t_10s)
      max_10s <- max_10s_range[which.max(max_10s_range[["MCAv_10sec"]]),]
      min_max <- min_max %>% add_column(max_t_10s = max_10s$time, max_10s = max_10s$MCAv_10sec)
      
      max_30s_range <- indi_cont %>% filter(time > min_max$min_t_30s)
      max_30s <- max_30s_range[which.max(max_30s_range[["MCAv_30sec"]]),]
      min_max <- min_max %>% add_column(max_t_30s = max_30s$time, max_30s = max_30s$MCAv_30sec)
      
      max_petco2_range <- indi_data %>% filter(time > min_max$min_t_petco2)
      max_petco2 <- max_petco2_range[which.max(max_petco2_range[["petco2"]]),]
      min_max <- min_max %>% add_column(max_t_petco2 = max_petco2$time, max_petco2 = max_petco2$petco2)
      
      min_comp_30s <- indi_cont[which(indi_cont$time == 15),]
      min_max <- min_max %>% add_column(min_t_comp = min_comp_30s$time, min_comp = min_comp_30s$MCAv_30sec)
      max_comp_30s <- indi_cont[which(indi_cont$time == 60),]
      min_max <- min_max %>% add_column(max_t_comp = max_comp_30s$time, max_comp = max_comp_30s$MCAv_30sec)
      
      if(curr_id == 1 && curr_visit == 1 && curr_bout == 1){
        rebound_out <- min_max
      }else{
        rebound_out <- rbind(rebound_out, min_max)
      }
      
    }
  }
  setTxtProgressBar(pb, curr_id)
}

close(pb)

prism_out <- rebound_out[,c("ID", "visit", "bout", "min_tso", "max_tso", "min_t_tso", "max_t_tso", "min_petco2", "max_petco2", "min_t_petco2", "max_t_petco2")]

prism_out <- prism_out %>% pivot_wider(names_from = ID, values_from = c(min_tso, max_tso, min_t_tso, max_t_tso, min_petco2, max_petco2, min_t_petco2, max_t_petco2))

write_csv(prism_out, "prism_format_output.csv", na = "")
