# Plotting script

# Load colour palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# New facet label names for visit variable
visit_labs <- c("80" = "80%", "120" = "120%", "160" = "160%", "200" = "200%")

# New facet label names for bout variable
bout_labs <- c("1" = "Bout 1", "2" = "Bout 2", "3" = "Bout 3", "4" = "Bout 4")

# Plot outlier removed data as geom line overlay
for(ID in unique(b2xb_data$ID)){
  indi_b2xb <- b2xb_data[which(b2xb_data$ID == ID),]
  
  tso_plot <- indi_b2xb %>% ggplot(aes(x = time, y = mcav_orig)) +
    geom_line(aes(color = cbPalette[2]), size = 0.9) +
    geom_line(aes(x = time, y = mcav_tso, color = cbPalette[1]), size = 0.9) +
    scale_color_identity(name = NULL, breaks = c(cbPalette[2], cbPalette[1]), labels = c("Original", "Outlier Removed"), guide = "legend") +
    scale_x_continuous(breaks = scales::breaks_width(30)) +
    theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
          strip.text = element_text(size = 11, face = "bold"),
          axis.title = element_text(size = 11, face = "bold"),
          axis.text = element_text(size = 10),
          text = element_text(family="Arial"),
          legend.position = "bottom") +
    xlab("Time (s)") +
    ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
    facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs), drop = F)
  
  filename <- paste(ID, "tso_comp.tiff", sep = "_")
  
  tso_plot 
  ggsave(path = "Figures", filename = filename, device = "tiff", width = 8, height = 8.4, dpi = 320)
  
}

# Plot indvidual data sets across different export processes
for(ID in unique(rebound_out$ID)){
  indi_cont <- cont_bouts[which(cont_bouts$ID == ID),]
  indi_b2xb <- b2xb_data[which(b2xb_data$ID == ID),]
  indi_rebound <- rebound_out[which(rebound_out$ID == ID),]
  
  rebound_plot <- indi_b2xb %>% ggplot(aes(x = time, y = mcav_tso)) +
    annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
             ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_line(aes(color = cbPalette[1]), size = 1.1) +
    geom_vline(data = indi_rebound, aes(xintercept = min_t_tso), color = cbPalette[1], linetype = "dashed", alpha = 1) +
    geom_vline(data = indi_rebound, aes(xintercept = max_t_tso), color = cbPalette[1], linetype = "dotted", alpha = 1, size = 0.7) +
    geom_line(data = indi_cont, aes(x = time, y = MCAv_10sec, color = cbPalette[2]), size = 1.1) +
    geom_vline(data = indi_rebound, aes(xintercept = min_t_10s), color = cbPalette[2], linetype = "dashed", alpha = 1) +
    geom_vline(data = indi_rebound, aes(xintercept = max_t_10s), color = cbPalette[2], linetype = "dotted", alpha = 1, size = 0.7) +
    geom_line(data = indi_cont, aes(x = time, y = MCAv_30sec, color = cbPalette[3]), size = 1.1) +
    geom_vline(data = indi_rebound, aes(xintercept = min_t_30s), color = cbPalette[3], linetype = "dashed", alpha = 1) +
    geom_vline(data = indi_rebound, aes(xintercept = max_t_30s), color = cbPalette[3], linetype = "dotted", alpha = 1, size = 0.7) +
    scale_color_identity(name = NULL, breaks = c(cbPalette[1], cbPalette[2], cbPalette[3]), labels = c("Beat-to-Beat", "10s Smoothing", "30s Smoothing"), guide = "legend") +
    scale_alpha_manual(guide = guide_legend(override.aes = list(linetype = 1, shape = NA))) +
    scale_x_continuous(breaks = scales::breaks_width(30)) +
    theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
          strip.text = element_text(size = 11, face = "bold"),
          axis.title = element_text(size = 11, face = "bold"),
          axis.text = element_text(size = 10),
          text = element_text(family="Arial"),
          legend.position = "bottom") +
      xlab("Time (s)") +
      ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
      facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs), drop = F)
  
  filename <- paste(ID, "individual_comp.tiff", sep = "_")
  
  rebound_plot
  ggsave(path = "Figures", filename = filename, device = "tiff", width = 8, height = 8.4, dpi = 320)
  
}

# Correlation plots
min_t_out <- rebound_out %>% select(min_t_tso, min_t_10s, min_t_30s)

colnames(min_t_out) <- c("ID", "visit", "bout", "Beat-to-Beat", "10s Smoothing", "30s Smoothing")

ggpairs(min_t_out, columns = 4:6, aes(colour = visit, alpha = 0.5),
        lower = list(continuous = wrap("smooth", se = F, alpha = 0.8))) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        legend.position = "bottom")
ggsave(path = "Figures", filename = "min_time_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

max_t_out <- rebound_out %>% select(ID, visit, bout, max_t_tso, max_t_10s, max_t_30s)

colnames(max_t_out) <- c("ID", "visit", "bout", "Beat-to-Beat", "10s Smoothing", "30s Smoothing")

ggpairs(max_t_out, columns = 4:6, aes(colour = visit, alpha = 0.5),
        lower = list(continuous = wrap("smooth", se = F, alpha = 0.8))) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        legend.position = "bottom")
ggsave(path = "Figures", filename = "max_time_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

peak_trough <- rebound_out %>% select(!starts_with(c("min_t_","max_t_")) & !ends_with(c("orig", "smooth", "petco2")))

rebound_trough <- peak_trough %>% select(starts_with("min"))
colnames(rebound_trough) <- c("ID", "visit", "bout", "Beat-to-Beat", "10s Smoothing", "30s Smoothing")

ggpairs(rebound_trough, columns = 4:6, aes(colour = visit, alpha = 0.5),
        lower = list(continuous = wrap("smooth", se = F, alpha = 0.8))) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        legend.position = "bottom")
ggsave(path = "Figures", filename = "trough_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

rebound_peak <- peak_trough %>% select(starts_with("max"))
colnames(rebound_peak) <- c("ID", "visit", "bout", "Beat-to-Beat", "10s Smoothing", "30s Smoothing")

ggpairs(rebound_peak, columns = 4:6, aes(colour = visit, alpha = 0.5),
        lower = list(continuous = wrap("smooth", se = F, alpha = 0.8))) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        legend.position = "bottom")
ggsave(path = "Figures", filename = "peak_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

comp_30 <- rebound_out %>% select(contains("30") | contains("comp"))

comp_minmax <- comp_30 %>% select(!contains("_t_"))

ggpairs(comp_minmax, columns = 4:7, aes(colour = visit, alpha = 0.5),
        lower = list(continuous = wrap("smooth", se = F, alpha = 0.8))) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        legend.position = "bottom")
ggsave(path = "Figures", filename = "comp_minmax_30s.tiff", device = "tiff", width = 7, height = 7, dpi = 320)



# Plotting for a single bout, full data, two smooths and B2B, with final full overlay

overlay_CDS <- cont_bouts[which(cont_bouts$ID == "SI_002"),]
overlay_CDS <- overlay_CDS[which(overlay_CDS$visit == "200"),]
overlay_CDS <- overlay_CDS[which(overlay_CDS$bout == "1"),]
overlay_CDS$raw_title <- "Raw Data"
overlay_CDS$ten_title <- "10-Second Smoothing"
overlay_CDS$thirty_title <- "30-Second Smoothing"
overlay_CDS$b2b_title <- "Beat-to-Beat Averaging"
overlay_CDS$combi_title <- "Preprocessing Overlay"

overlay_b2b <- b2xb_data[which(b2xb_data$ID == "SI_002"),]
overlay_b2b <- overlay_b2b[which(overlay_b2b$visit == "200"),]
overlay_b2b <- overlay_b2b[which(overlay_b2b$bout == "1"),]

# Plot each one
raw_plot <- ggplot(data = overlay_CDS, aes(x = time, y = MCAv))+
  geom_line(color = cbPalette[1], size = 0.7) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
                              panel.grid = element_blank(),
                              panel.background = element_blank(),
                              strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
                              strip.text = element_text(size = 11, face = "bold"),
                              axis.title = element_text(size = 11, face = "bold"),
                              axis.text = element_text(size = 10),
                              text = element_text(family="Arial"),
                              axis.title.x = element_blank()) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(.~raw_title)

ten_plot <- ggplot(data = overlay_CDS, aes(x = time, y = MCAv))+
  geom_line(color = cbPalette[1], size = 0.7) +
  geom_line(aes(x = time, y = MCAv_10sec), color = cbPalette[2], size = 0.8) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        axis.title.x = element_blank()) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(.~ten_title)

thirty_plot <- ggplot(data = overlay_CDS, aes(x = time, y = MCAv))+
  geom_line(color = cbPalette[1], size = 0.7) +
  geom_line(aes(x = time, y = MCAv_30sec), color = cbPalette[3], size = 0.8) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        axis.title.x = element_blank()) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(.~thirty_title)

b2b_plot <- ggplot(data = overlay_CDS, aes(x = time, y = MCAv))+
  geom_line(color = cbPalette[1], size = 0.7) +
  geom_line(data = overlay_b2b, aes(x = time, y = mcav_tso), color = cbPalette[4], size = 0.8) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial"),
        axis.title.x = element_blank()) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(.~b2b_title)

combi_plot <- ggplot(data = overlay_CDS, aes(x = time, y = MCAv))+
  geom_line(color = cbPalette[1], size = 0.7) +
  geom_line(aes(x = time, y = MCAv_10sec), color = cbPalette[2], size = 0.8) +
  geom_line(aes(x = time, y = MCAv_30sec), color = cbPalette[3], size = 0.8) +
  geom_line(data = overlay_b2b, aes(x = time, y = mcav_tso), color = cbPalette[4], size = 0.8) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 10),
        text = element_text(family="Arial")) +
  xlab("Time (s)") +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(.~combi_title)


  

# Patchwork each one
combi_examples <-  ten_plot/ 
  thirty_plot/
  b2b_plot/
  combi_plot

combi_examples

ggsave(path = "Figures", filename = "combi_preprocessing_examples.tiff", device = "tiff", width = 8, height = 11, dpi = 320)
