# Q2a

NFL2022_stuffs <- read_csv('https://bcdanl.github.io/data/NFL2022_stuffs.csv')
NFL2022_stuffs <- NFL2022_stuffs[complete.cases(NFL2022_stuffs$posteam), ]

# Q2b

filtered_data <- NFL2022_stuffs %>%
  filter(wp > 0.2 & wp < 0.75, down <= 2, half_seconds_remaining > 120)

summary_pass <- filtered_data %>%
  group_by(posteam) %>%
  summarize(mean_pass = mean(pass, na.rm = TRUE))

# Q2c

library(ggplot2)

# Reordering posteam based on mean value of pass
summary_pass$posteam <- factor(summary_pass$posteam, levels = summary_pass$posteam[order(summary_pass$mean_pass)])

# Plotting
ggplot(summary_pass, aes(x = mean_pass, y = posteam)) +
  geom_point() +
  labs(title = "Mean Value of Pass for Each posteam",
       x = "Percentage of pass plays",
       y = "Team with possession") +
  theme_minimal()

# Q2d

NFL2022_epa <- read_csv('https://bcdanl.github.io/data/NFL2022_epa.csv')

NFL2022_stuffs_EPA <- NFL2022_stuffs %>%
  left_join(NFL2022_epa %>% select(play_id, passer, receiver, epa), by = "play_id") %>%
  filter(!is.na(passer))

# Q2e

weekly_trend <- NFL2022_stuffs_EPA %>%
  filter(passer %in% c("J.Allen", "P.Mahomes")) %>%
  group_by(week, passer) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))

# Plotting
ggplot(weekly_trend, aes(x = week, y = mean_epa, color = passer)) +
  geom_line() +
  labs(title = "NFL Weekly Trend of Mean EPA",
       x = "Week",
       y = "Mean EPA") +
  theme_minimal()

# Q2f

mean_epa_diff <- weekly_trend %>%
  spread(passer, mean_epa) %>%
  mutate(diff_J_Allen_P_Mahomes = `J.Allen` - `P.Mahomes`)


# Q2g


passer_summary <- NFL2022_stuffs_EPA %>%
  group_by(posteam, passer) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE),
            n_pass = n()) %>%
  filter(n_pass >= quantile(n_pass, 0.75))

top_10_passers <- passer_summary %>%
  group_by(passer) %>%
  summarize(mean_epa = mean(mean_epa, na.rm = TRUE)) %>%
  top_n(10, wt = mean_epa)




