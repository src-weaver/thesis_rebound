# Load colour palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# average across full time course
mean_cont <- cont_bouts %>% group_by(visit, bout, time) %>%
  summarise(across(c("MCAv", "MCAv_10sec", "MCAv_30sec"), ~ mean(.x, na.rm = TRUE)))

mean_b2xb <- b2xb_data %>% group_by(visit, bout, time) %>%
  summarise(n = length(unique(b2xb_data$ID)),
            across(c("mcav_orig", "mcav_orig_smooth", "mcav_tso", "mcav_tso_smooth", "petco2"), list(mean = mean, SD = sd)))

# New facet label names for visit variable
visit_labs <- c("80" = "80%", "120" = "120%", "160" = "160%", "200" = "200%")

# New facet label names for bout variable
bout_labs <- c("1" = "Bout 1", "2" = "Bout 2", "3" = "Bout 3", "4" = "Bout 4")


rebound_plot_10 <- cont_bouts %>% ggplot(aes(x = time, y = MCAv_10sec, group = ID)) +
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(color = '#999999', alpha = 0.5) + 
  geom_line(data = mean_cont, aes(x = time, y = MCAv_10sec), size = 1.1, color = "#0072B2") + 
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

rebound_plot_10
ggsave(path = "Figures", filename = "rebound_plot_10sec.tiff", device = "tiff", width = 8, height = 8, dpi = 320)

rebound_plot_30 <- cont_bouts %>% ggplot(aes(x = time, y = MCAv_30sec, group = ID)) +
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(color = '#999999', alpha = 0.5) + 
  geom_line(data = mean_cont, aes(x = time, y = MCAv_30sec), size = 1.1, color = "#0072B2") + 
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

rebound_plot_30
ggsave(path = "Figures", filename = "rebound_plot_30sec.tiff", device = "tiff", width = 8, height = 8, dpi = 320)

rebound_plot_tso <- b2xb_data %>% ggplot(aes(x = time, y = mcav_tso, group = ID)) +
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(color = '#999999', alpha = 0.5) + 
  geom_line(data = mean_b2xb, aes(x = time, y = mcav_tso_mean), size = 1.1, color = cbPalette[2]) + 
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

rebound_plot_tso
ggsave(path = "Figures", filename = "rebound_plot_tso.tiff", device = "tiff", width = 8, height = 8, dpi = 320)

rebound_plot_petco2 <- b2xb_data %>% ggplot(aes(x = time, y = petco2, group = ID)) +
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(color = '#999999', alpha = 0.5) + 
  geom_line(data = mean_b2xb, aes(x = time, y = petco2_mean), size = 1.1, color = "#0072B2") + 
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "#CCCCCC"),
        strip.text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~P[ET]*~CO[2]*" (mmHg)"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

rebound_plot_petco2
ggsave(path = "Figures", filename = "rebound_plot_petco2.tiff", device = "tiff", width = 8, height = 8, dpi = 320)


tso_80 <- ggplot(subset(mean_b2xb, visit == 80), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = mcav_tso_mean), colour = "#E69F00")+
  geom_ribbon(aes(ymin = mcav_tso_mean - mcav_tso_SD, ymax = mcav_tso_mean + mcav_tso_SD), fill = "#E69F00", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(t = 1)) +
  xlab("Time (s)") +
  ylim(50, 150) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))#no x

tso_120 <- ggplot(subset(mean_b2xb, visit == 120), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = mcav_tso_mean), colour = "#E69F00")+
  geom_ribbon(aes(ymin = mcav_tso_mean - mcav_tso_SD, ymax = mcav_tso_mean + mcav_tso_SD), fill = "#E69F00", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(t = 1)) +
  xlab("Time (s)") +
  ylim(50, 150) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

tso_160 <- ggplot(subset(mean_b2xb, visit == 160), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = mcav_tso_mean), colour = "#E69F00")+
  geom_ribbon(aes(ymin = mcav_tso_mean - mcav_tso_SD, ymax = mcav_tso_mean + mcav_tso_SD), fill = "#E69F00", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(t = 1)) +
  xlab("Time (s)") +
  ylim(50, 150) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

tso_200 <- ggplot(subset(mean_b2xb, visit == 200), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = mcav_tso_mean), colour = "#E69F00")+
  geom_ribbon(aes(ymin = mcav_tso_mean - mcav_tso_SD, ymax = mcav_tso_mean + mcav_tso_SD), fill = "#E69F00", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(t = 1)) +
  xlab("Time (s)") +
  ylim(50, 150) +
  ylab(bquote(bold(~MCA[v]*" (cm."~s^-1*")"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

petco2_80 <- ggplot(subset(mean_b2xb, visit == 80), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = petco2_mean), colour = "#56B4E9")+
  geom_ribbon(aes(ymin = petco2_mean - petco2_SD, ymax = petco2_mean + petco2_SD), fill = "#56B4E9", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(b = 1)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~P[ET]*~CO[2]*" (mmHg)"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

petco2_120 <- ggplot(subset(mean_b2xb, visit == 120), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = petco2_mean), colour = "#56B4E9")+
  geom_ribbon(aes(ymin = petco2_mean - petco2_SD, ymax = petco2_mean + petco2_SD), fill = "#56B4E9", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11), 
        plot.margin = margin(b = 1)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~P[ET]*~CO[2]*" (mmHg)"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

petco2_160 <- ggplot(subset(mean_b2xb, visit == 160), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = petco2_mean), colour = "#56B4E9")+
  geom_ribbon(aes(ymin = petco2_mean - petco2_SD, ymax = petco2_mean + petco2_SD), fill = "#56B4E9", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11),
        plot.margin = margin(b = 1)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~P[ET]*~CO[2]*" (mmHg)"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label no x

petco2_200 <- ggplot(subset(mean_b2xb, visit == 200), aes(x = time))+
  annotate("rect", xmin = 0, xmax = 30, fill = cbPalette[4],
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(aes(y = petco2_mean), colour = "#56B4E9")+
  geom_ribbon(aes(ymin = petco2_mean - petco2_SD, ymax = petco2_mean + petco2_SD), fill = "#56B4E9", alpha = 0.4) +
  theme(panel.border = element_rect(linetype = "solid", size = 0.5, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        plot.margin = margin(b = 1)) +
  xlab("Time (s)") +
  ylab(bquote(bold(~P[ET]*~CO[2]*" (mmHg)"))) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs)) #no bout label 

patched_mcav_pet <- tso_80 /
  petco2_80 /
  tso_120 / 
  petco2_120 /
  tso_160 /
  petco2_160 /
  tso_200 / 
  petco2_200

for(i in 1:7){
  patched_mcav_pet[[i]] <- patched_mcav_pet[[i]] + theme(axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank(),
                                                 axis.title.x = element_blank())
}

patched_mcav_pet
ggsave(path = "Figures", filename = "mcav_vs_petco2.tiff", device = "tiff", width = 10, height = 12, dpi = 320)

petco2_tso_out <- rebound_out %>% select(ends_with(c("tso", "petco2")))


ggplot(data = petco2_tso_out, aes(x = min_t_tso, y = min_t_petco2)) +
  geom_point() +
  geom_smooth(method = "lm", colour = cbPalette[3]) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

ggsave(path = "Figures", filename = "min_time_tso_petco2_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

ggplot(data = petco2_tso_out, aes(x = min_tso, y = min_petco2)) +
  geom_point() +
  geom_smooth(method = "lm", colour = cbPalette[3]) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

ggsave(path = "Figures", filename = "min_mag_tso_petco2_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

ggplot(data = petco2_tso_out, aes(x = max_t_tso, y = max_t_petco2)) +
  geom_point() +
  geom_smooth(method = "lm", colour = cbPalette[3]) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

ggsave(path = "Figures", filename = "max_time_tso_petco2_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)

ggplot(data = petco2_tso_out, aes(x = max_tso, y = max_petco2)) +
  geom_point() +
  geom_smooth(method = "lm", colour = cbPalette[3]) +
  facet_grid(visit~bout, labeller = labeller(visit = visit_labs, bout = bout_labs))

ggsave(path = "Figures", filename = "max_mag_tso_petco2_corr.tiff", device = "tiff", width = 7, height = 7, dpi = 320)