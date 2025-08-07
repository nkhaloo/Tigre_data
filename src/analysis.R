library(tidyverse)

# 1. Fricative CoG summary
fric_summary <- df %>%
  filter(fricative == "y") %>%
  group_by(seg, ejective) %>%
  summarise(
    mean_rfcog = mean(rfcog,   na.rm = TRUE),
    sd_rfcog   = sd(rfcog,     na.rm = TRUE),
    se_rfcog   = sd_rfcog / sqrt(n()),
    n          = n(),
    .groups    = "drop"
  )

# 2. Fricative CoG plot
fric_CoG_plot <- fric_summary %>%
  ggplot(aes(seg, mean_rfcog, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_rfcog - se_rfcog, ymax = mean_rfcog + se_rfcog),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_rfcog + se_rfcog),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "CoG (dB)",
    title = "Mean CoG (dB) for fricatives"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

# small df for fricative CoG stats (no ejective column)
fric_CoG_stats <- fric_summary %>%
  select(seg, mean_rfcog, sd_rfcog, se_rfcog, n)

# 3. Stops summary 
stop_summary <- df %>%
  filter(fricative == "n") %>%
  group_by(seg, ejective) %>%
  summarise(
    # burst duration
    mean_bDur    = mean(bDur,   na.rm = TRUE),
    sd_bDur      = sd(bDur,     na.rm = TRUE),
    se_bDur      = sd_bDur / sqrt(n()),
    # burst CoG
    mean_bcog    = mean(bcog,   na.rm = TRUE),
    sd_bcog      = sd(bcog,     na.rm = TRUE),
    se_bcog      = sd_bcog / sqrt(n()),
    # mean intensity
    mean_bMeanInt = mean(bMeanInt, na.rm = TRUE),
    sd_bMeanInt   = sd(bMeanInt,   na.rm = TRUE),
    se_bMeanInt   = sd_bMeanInt / sqrt(n()),
    # voice‐onset time
    mean_VOT     = mean(VOT,    na.rm = TRUE),
    sd_VOT       = sd(VOT,      na.rm = TRUE),
    se_VOT       = sd_VOT / sqrt(n()),
    # release‐hold duration
    mean_rhDur   = mean(rhDur,  na.rm = TRUE),
    sd_rhDur     = sd(rhDur,    na.rm = TRUE),
    se_rhDur     = sd_rhDur / sqrt(n()),
    # count
    n            = n(),
    .groups      = "drop"
  )

# little data‐frames dropping ejective
bDur_stats     <- stop_summary %>% select(seg, mean_bDur, sd_bDur, se_bDur,     n)
bCoG_stats     <- stop_summary %>% select(seg, mean_bcog, sd_bcog, se_bcog,     n)
bMeanInt_stats <- stop_summary %>% select(seg, mean_bMeanInt, sd_bMeanInt, se_bMeanInt, n)
VOT_stats      <- stop_summary %>% select(seg, mean_VOT, sd_VOT, se_VOT,         n)
rhDur_stats    <- stop_summary %>% select(seg, mean_rhDur, sd_rhDur, se_rhDur,     n)

# plots for stops
bDur_plot <- non_fric_summary %>%
  ggplot(aes(seg, mean_bDur, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bDur - se_bDur, ymax = mean_bDur + se_bDur),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_bDur + se_bDur),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "Burst Duration (ms)",
    title = "Burst Duration (ms) for stops"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

bCoG_plot <- non_fric_summary %>%
  ggplot(aes(seg, mean_bcog, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bcog - se_bcog, ymax = mean_bcog + se_bcog),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_bcog + se_bcog),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "CoG (Hz)",
    title = "Mean CoG (Hz) for stops"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

bMeanInt_plot <- non_fric_summary %>%
  ggplot(aes(seg, mean_bMeanInt, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_bMeanInt - se_bMeanInt, ymax = mean_bMeanInt + se_bMeanInt),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_bMeanInt + se_bMeanInt),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "Mean Burst Intensity (dB)",
    title = "Mean Burst Intensity (dB) for stops"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

VOT_plot <- non_fric_summary %>%
  ggplot(aes(seg, mean_VOT, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_VOT - se_VOT, ymax = mean_VOT + se_VOT),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_VOT + se_VOT),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "VOT (ms)",
    title = "Mean VOT (ms) for stops"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

rhDur_plot <- non_fric_summary %>%
  ggplot(aes(seg, mean_rhDur, fill = ejective)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_rhDur - se_rhDur, ymax = mean_rhDur + se_rhDur),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = n, y = mean_rhDur + se_rhDur),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x     = "Segment",
    y     = "Duration of Aspiration following release (ms)",
    title = "Duration of Aspiration following release (ms)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 14)
  )

# 4. Display plots and stats
fric_CoG_plot
fric_CoG_stats

bDur_plot
bDur_stats

bCoG_plot
bCoG_stats

bMeanInt_plot
bMeanInt_stats

VOT_plot
VOT_stats

rhDur_plot
rhDur_stats
