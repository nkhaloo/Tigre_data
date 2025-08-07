library(tidyverse)

# Directories
fig_dir   <- "/Users/noahkhaloo/Desktop/Tigre_data/figures"
stats_dir <- "/Users/noahkhaloo/Desktop/Tigre_data/summary_stats"

# 0. Read in the CSV
df <- read_delim(
  "/Users/noahkhaloo/Desktop/Tigre_data/pilot/ejective_measurements.csv",
  delim   = "\t",
  locale  = locale(encoding = "UTF-16")
) %>%
  mutate(
    VOT       = bDur + rhDur,                     # already in seconds
    ejective  = if_else(str_detect(seg, "'$"), "y", "n"),
    fricative = if_else(str_starts(seg, "s"),   "y", "n")
  )

# Common theme
clean_theme <- theme_minimal() +
  theme(
    legend.position  = "none",
    axis.text.x      = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.line        = element_line(color = "black")
  )

# Helper to save plots
save_plot <- function(plot, filename) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot     = plot,
    width    = 6,
    height   = 4,
    dpi      = 300,
    bg       = "white"
  )
}

# 1. Fricative CoG summary & plot
fric_summary <- df %>%
  filter(fricative == "y") %>%
  group_by(seg, ejective) %>%
  summarise(
    mean_rfcog = mean(rfcog, na.rm = TRUE),
    sd_rfcog   = sd(rfcog,   na.rm = TRUE),
    se_rfcog   = sd_rfcog / sqrt(n()),
    n          = n(),
    .groups    = "drop"
  )

# Save fricative summary table
fric_summary %>%
  select(seg, mean_rfcog, sd_rfcog, se_rfcog, n) %>%
  write_csv(file.path(stats_dir, "fricative_CoG_summary.csv"))

# Fricative CoG plot (no numbers above bars)
fric_CoG_plot <- fric_summary %>%
  ggplot(aes(seg, mean_rfcog, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = mean_rfcog - se_rfcog, ymax = mean_rfcog + se_rfcog),
    position = position_dodge(0.9), width = 0.2
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(
    x     = "Segment",
    y     = "CoG (dB)",
    title = "Mean CoG (dB) for fricatives"
  ) +
  clean_theme

save_plot(fric_CoG_plot, "fric_CoG_plot.png")


# 2. Summarise stops (non‐fricatives)
stops_summary <- df %>%
  filter(fricative == "n") %>%
  group_by(seg, ejective) %>%
  summarise(
    mean_bDur     = mean(bDur,     na.rm = TRUE),
    sd_bDur       = sd(bDur,       na.rm = TRUE),
    se_bDur       = sd_bDur / sqrt(n()),
    mean_bcog     = mean(bcog,     na.rm = TRUE),
    sd_bcog       = sd(bcog,       na.rm = TRUE),
    se_bcog       = sd_bcog / sqrt(n()),
    mean_bMeanInt = mean(bMeanInt, na.rm = TRUE),
    sd_bMeanInt   = sd(bMeanInt,   na.rm = TRUE),
    se_bMeanInt   = sd_bMeanInt / sqrt(n()),
    mean_VOT      = mean(VOT,      na.rm = TRUE),
    sd_VOT        = sd(VOT,        na.rm = TRUE),
    se_VOT        = sd_VOT / sqrt(n()),
    mean_rhDur    = mean(rhDur,    na.rm = TRUE),
    sd_rhDur      = sd(rhDur,      na.rm = TRUE),
    se_rhDur      = sd_rhDur / sqrt(n()),
    n             = n(),
    .groups       = "drop"
  )

# 3. Write separate CSVs for each stop measure
stops_summary %>%
  select(seg, mean_bDur,   sd_bDur,   se_bDur,   n) %>%
  write_csv(file.path(stats_dir, "bDur_summary.csv"))

stops_summary %>%
  select(seg, mean_bcog,   sd_bcog,   se_bcog,   n) %>%
  write_csv(file.path(stats_dir, "bCoG_summary.csv"))

stops_summary %>%
  select(seg, mean_bMeanInt, sd_bMeanInt, se_bMeanInt, n) %>%
  write_csv(file.path(stats_dir, "bMeanInt_summary.csv"))

stops_summary %>%
  select(seg, mean_VOT,    sd_VOT,    se_VOT,    n) %>%
  write_csv(file.path(stats_dir, "VOT_summary.csv"))

stops_summary %>%
  select(seg, mean_rhDur,  sd_rhDur,  se_rhDur,  n) %>%
  write_csv(file.path(stats_dir, "rhDur_summary.csv"))


# 4a. bDur (s)
bDur_plot <- stops_summary %>%
  ggplot(aes(seg, mean_bDur, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bDur - se_bDur, ymax = mean_bDur + se_bDur),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(x = "Segment", y = "Release-Burst Duration (s)", title = "Mean Release-Burst Duration (s)") +
  clean_theme
save_plot(bDur_plot, "bDur_plot.png")

# 4b. bCoG (Hz)
bCoG_plot <- stops_summary %>%
  ggplot(aes(seg, mean_bcog, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bcog - se_bcog, ymax = mean_bcog + se_bcog),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(x = "Segment", y = "CoG (Hz)", title = "Mean Release-Burst CoG (Hz)") +
  clean_theme
save_plot(bCoG_plot, "bCoG_plot.png")

# 4c. bMeanInt (dB)
bMeanInt_plot <- stops_summary %>%
  ggplot(aes(seg, mean_bMeanInt, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bMeanInt - se_bMeanInt, ymax = mean_bMeanInt + se_bMeanInt),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(x = "Segment", y = "Burst Intensity (dB)", title = "Mean Burst Intensity (dB)") +
  clean_theme
save_plot(bMeanInt_plot, "bMeanInt_plot.png")

# 4d. VOT (s)
VOT_plot <- stops_summary %>%
  ggplot(aes(seg, mean_VOT, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - se_VOT, ymax = mean_VOT + se_VOT),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(x = "Segment", y = "VOT (s)", title = "Mean VOT (s)") +
  clean_theme
save_plot(VOT_plot, "VOT_plot.png")

# 4e. rhDur (s)
rhDur_plot <- stops_summary %>%
  ggplot(aes(seg, mean_rhDur, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_rhDur - se_rhDur, ymax = mean_rhDur + se_rhDur),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  labs(x = "Segment", y = "Post-release Aspiration (s)", title = "Mean Post-release Aspiration (s)") +
  clean_theme
save_plot(rhDur_plot, "rhDur_plot.png")
