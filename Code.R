library(tidyverse)

df <- read_csv("230 project - Form Responses 1.csv")

df <- df %>%
  rename(
    Major = `What is your major?`,
    GPA = `What is you GPA?`,
    Credits = `How many credits are you taking a semester`
  ) %>%
  mutate(
    GPA = as.numeric(GPA),
    Credits = parse_number(Credits)
  )

ggplot(df, aes(x = GPA)) +
  geom_histogram(binwidth = 0.2, fill = "orange", color = "black") +
  geom_density(aes(y = ..count.. * 0.2), color = "blue", size = 1) +
  labs(title = "Histogram of GPA", x = "GPA", y = "Frequency")

ggplot(df, aes(x = Major, y = GPA)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of GPA by Major", x = "Major", y = "GPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = Credits, y = GPA)) +
  geom_point(color = "darkorange") +
  labs(title = "Scatterplot of GPA vs Credits", x = "Credits", y = "GPA")

df %>%
  group_by(Major) %>%
  summarise(
    mean_GPA = mean(GPA, na.rm = TRUE),
    sd_GPA = sd(GPA, na.rm = TRUE),
    min_GPA = min(GPA, na.rm = TRUE),
    max_GPA = max(GPA, na.rm = TRUE)
  )
