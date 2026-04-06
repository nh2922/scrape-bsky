library(quanteda.textstats)
library(quanteda)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(modelsummary)

data_raw <- read.csv("/Users/Nicholas/Desktop/bluesky_bots/bsky_sentiment_log.csv")

data_clean <- data_raw %>%
  dplyr::filter(sample_number != 42)

data_clean <- data_clean %>%
  mutate(
    bot_group = case_when(
      str_starts(bot_handle, "c") ~ "Control",
      str_starts(bot_handle, "n") ~ "Negative",
      str_starts(bot_handle, "p") ~ "Positive"
    )
  )

data_readability <- data_clean %>%
  distinct(URI, .keep_all = TRUE)

# creating variant datasets
data_r_nourl <- data_readability %>%
  mutate(
    text = str_remove_all(text, "https?://\\S+|www\\.\\S+")
  )
data_r_nourl <- data_r_nourl %>%
  mutate(
    text = str_squish(text)
  )
data_r_nourlhash <- data_r_nourl %>%
  mutate(
    text = as.character(coalesce(text, "")),
    text = str_replace_all(text, "#", " "),
    # split CamelCase inside words:
    text = str_replace_all(text, "([a-z])([A-Z])", "\\1 \\2"),
    text = str_replace_all(text, "([A-Z]+)([A-Z][a-z])", "\\1 \\2"),
    text = str_replace_all(text, "[_\\-]", " "),
    text = str_squish(text)
  )

data_readability <- data_readability %>%
  mutate(
    # make sure we have character text, and treat missing as empty string
    text = as.character(coalesce(text, "")),
    
    # Compute each readability measure (one per new column)
    coleman_liau = textstat_readability(text, measure = "Coleman.Liau.short")$Coleman.Liau.short,
    dale_chall   = textstat_readability(text, measure = "Dale.Chall.PSK")$Dale.Chall.PSK,
    ari          = textstat_readability(text, measure = "ARI")$ARI,
    flesch_ease  = textstat_readability(text, measure = "Flesch")$Flesch
  )

data_r_nourl <- data_r_nourl %>%
  mutate(
    text = as.character(coalesce(text, "")),
    
    # Compute readability measure (one per new column)
    coleman_liau = textstat_readability(text, measure = "Coleman.Liau.short")$Coleman.Liau.short,
    dale_chall   = textstat_readability(text, measure = "Dale.Chall.PSK")$Dale.Chall.PSK,
    ari          = textstat_readability(text, measure = "ARI")$ARI,
    flesch_ease  = textstat_readability(text, measure = "Flesch")$Flesch
  )
data_r_nourlhash <- data_r_nourlhash %>%
  mutate(
    # make sure we have character text, and treat missing as empty string
    text = as.character(coalesce(text, "")),
    
    # Compute each readability measure (one per new column)
    coleman_liau = textstat_readability(text, measure = "Coleman.Liau.short")$Coleman.Liau.short,
    dale_chall   = textstat_readability(text, measure = "Dale.Chall.PSK")$Dale.Chall.PSK,
    ari          = textstat_readability(text, measure = "ARI")$ARI,
    flesch_ease  = textstat_readability(text, measure = "Flesch")$Flesch
  )

data_readability <- data_readability %>%
  mutate(
    flesch_difficulty = 100 - flesch_ease
  )
data_r_nourl <- data_r_nourl %>%
  mutate(
    flesch_difficulty = 100 - flesch_ease
  )
data_r_nourlhash <- data_r_nourlhash %>%
  mutate(
    flesch_difficulty = 100 - flesch_ease
  )

data_readability <- data_readability[
  complete.cases(
    data_readability[, c("coleman_liau", "dale_chall", "ari", "flesch_difficulty")]
  ),
]

data_r_nourl <- data_r_nourl[
  complete.cases(
    data_r_nourl[, c("coleman_liau", "ari", "dale_chall", "flesch_difficulty")]
  ),
]
data_r_nourlhash <- data_r_nourlhash[
  complete.cases(
    data_r_nourlhash[, c("coleman_liau", "ari", "dale_chall", "flesch_difficulty")]
  ),
]

avg_readability <- data_readability %>%
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>%
  summarise(
    coleman_liau = mean(coleman_liau, na.rm = TRUE),
    ari          = mean(ari, na.rm = TRUE),
    dale_chall   = mean(dale_chall, na.rm = TRUE),
    flesch_difficulty  = mean(flesch_difficulty, na.rm = TRUE),
    .groups = "drop"
  )

avg_readability_long <- avg_readability %>%
  pivot_longer(
    cols = c(coleman_liau, ari, dale_chall, flesch_difficulty),
    names_to = "metric",
    values_to = "average_score"
  ) %>%
  group_by(metric) %>%
  mutate(z_score = as.numeric(scale(average_score))) %>%
  ungroup()

avg_r_nourl <- data_r_nourl %>%
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>%
  summarise(
    coleman_liau = mean(coleman_liau, na.rm = TRUE),
    ari          = mean(ari, na.rm = TRUE),
    dale_chall   = mean(dale_chall, na.rm = TRUE),
    flesch_difficulty  = mean(flesch_difficulty, na.rm = TRUE),
    .groups = "drop"
  )

avg_r_nourl_long <- avg_r_nourl %>%
  pivot_longer(
    cols = c(coleman_liau, ari, dale_chall, flesch_difficulty),
    names_to = "metric",
    values_to = "average_score"
  ) %>%
  group_by(metric) %>%
  mutate(z_score = as.numeric(scale(average_score))) %>%
  ungroup()

avg_r_nourlhash <- data_r_nourlhash %>%
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>%
  summarise(
    coleman_liau = mean(coleman_liau, na.rm = TRUE),
    ari          = mean(ari, na.rm = TRUE),
    dale_chall   = mean(dale_chall, na.rm = TRUE),
    flesch_difficulty  = mean(flesch_difficulty, na.rm = TRUE),
    .groups = "drop"
  )

avg_r_nourlhash_long <- avg_r_nourlhash %>%
  pivot_longer(
    cols = c(coleman_liau, ari, dale_chall, flesch_difficulty),
    names_to = "metric",
    values_to = "average_score"
  ) %>%
  group_by(metric) %>%
  mutate(z_score = as.numeric(scale(average_score))) %>%
  ungroup()

# graphing the four metrics / 3 datasets

all_readability <- bind_rows(
  avg_r_nourlhash_long %>% mutate(dataset = "Clean"),
  avg_r_nourl_long %>% mutate(dataset = "NoURL"),
  avg_readability_long %>% mutate(dataset = "Raw"),
)

all_readability$dataset <- factor(
  all_readability$dataset,
  levels = c("Raw", "NoURL", "Clean")
)

plot_metric <- function(metric_name, title_name) {
  ggplot(
    all_readability %>% filter(metric == metric_name),
    aes(x = sentiment, y = z_score, fill = sentiment)
  ) +
    geom_col() +
    facet_wrap(~ dataset, nrow = 1) +
    labs(
      title = title_name,
      x = "Sentiment",
      y = "Z-score"
    ) +
    scale_y_continuous(limits = c(-1.2, 1.2)) +  
    theme_minimal(base_family = "Times New Roman") + 
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      plot.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14),  
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "none"
    )
}
coleman_plot <- plot_metric("coleman_liau", "Coleman-Liau by Sentiment")
ari_plot <- plot_metric("ari", "ARI by Sentiment")
dale_plot <- plot_metric("dale_chall", "Dale-Chall by Sentiment")
flesch_plot <- plot_metric("flesch_difficulty", "Flesch Difficulty by Sentiment")

ggsave("~/Desktop/coleman_plot.jpg", coleman_plot, width = 6, height = 4, dpi = 300)
ggsave("~/Desktop/ari_plot.jpg", ari_plot, width = 6, height = 4, dpi = 300)
ggsave("~/Desktop/dale_plot.jpg", dale_plot, width = 6, height = 4, dpi = 300)
ggsave("~/Desktop/flesch_plot.jpg", flesch_plot, width = 6, height = 4, dpi = 300)

# regression readability models - CONTROL
metrics <- c("coleman_liau", "ari", "dale_chall", "flesch_difficulty")

data_readability <- data_readability %>%
  mutate(
    sentiment = factor(sentiment, levels = c("neutral", "positive", "negative"))
  )

models_readability <- lapply(metrics, function(m) {
  formula <- as.formula(paste(m, "~ sentiment"))
  lm(formula, data = data_readability)
})

lapply(models_readability, summary)


# regression readability models - NO URL

data_r_nourl <- data_r_nourl %>%
  mutate(
    sentiment = factor(sentiment, levels = c("neutral", "positive", "negative"))
  )

models_r_nourl <- lapply(metrics, function(m) {
  formula <- as.formula(paste(m, "~ sentiment"))
  lm(formula, data = data_r_nourl)
})

lapply(models_r_nourl, summary)

# regression readability models - CLEAN

data_r_nourlhash <- data_r_nourlhash %>%
  mutate(
    sentiment = factor(sentiment, levels = c("neutral", "positive", "negative"))
  )

models_r_nourlhash <- lapply(metrics, function(m) {
  formula <- as.formula(paste(m, "~ sentiment"))
  lm(formula, data = data_r_nourlhash)
})

lapply(models_r_nourlhash, summary)

# creating tables

models_cl <- list(
  Raw = models_readability[[1]],
  NoURL = models_r_nourl[[1]],
  Clean = models_r_nourlhash[[1]]
)

modelsummary(
  models_cl,
  statistic = "({std.error})",
  stars = TRUE,
  title = "Effect of Sentiment on Coleman–Liau Readability",
)

models_cl <- list(
  Raw = models_readability[[2]],
  NoURL = models_r_nourl[[2]],
  Clean = models_r_nourlhash[[2]]
)

modelsummary(
  models_cl,
  statistic = "({std.error})",
  stars = TRUE,
  title = "Effect of Sentiment on ARI Readability",
)

models_cl <- list(
  Raw = models_readability[[3]],
  NoURL = models_r_nourl[[3]],
  Clean = models_r_nourlhash[[3]]
)

modelsummary(
  models_cl,
  statistic = "({std.error})",
  stars = TRUE,
  title = "Effect of Sentiment on Dale-Chall Readability",
)

models_cl <- list(
  Raw = models_readability[[4]],
  NoURL = models_r_nourl[[4]],
  Clean = models_r_nourlhash[[4]]
)

modelsummary(
  models_cl,
  statistic = "({std.error})",
  stars = TRUE,
  title = "Effect of Sentiment on Flesch Difficulty Readability",
)


# hashtag analysis

data_hashtags <- data_clean %>%
  mutate(
    sentiment = str_trim(tolower(sentiment)),
    hashtag_count = str_count(text, "#")
  )
hashtag_summary <- data_hashtags %>%
  group_by(sentiment) %>%
  summarise(
    avg_hashtags = mean(hashtag_count, na.rm = TRUE),
    .groups = "drop"
  )
hashtag_plot <- ggplot(hashtag_summary, aes(x = sentiment, y = avg_hashtags, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Hashtags per Post by Sentiment",
    x = "Sentiment",
    y = "Average Hashtags per Post"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none")
ggsave("~/Desktop/hashtag_plot.jpg", plot=hashtag_plot)

# Longitudinal analysis

sentiment_counts <- data_clean %>%
  group_by(bot_handle, sample_number, sentiment) %>%
  summarise(
    n_posts = n(),
    .groups = "drop"
  )

sentiment_counts <- sentiment_counts %>%
  group_by(bot_handle, sample_number) %>%
  mutate(
    total_posts = sum(n_posts),
    proportion = n_posts / total_posts
  ) %>%
  ungroup()

sentiment_counts <- sentiment_counts %>%
  mutate(
    bot_group = case_when(
      str_starts(bot_handle, "c") ~ "Control",
      str_starts(bot_handle, "n") ~ "Negative",
      str_starts(bot_handle, "p") ~ "Positive"
    )
  )


data_separated <- data_clean %>%
  group_by(bot_group, bot_handle, sample_number, sentiment) %>%
  summarise(post_count = n(), .groups = "drop")

control_data <- data_separated %>%
  filter(bot_group == "Control")
  
negative_data <- data_separated %>%
  filter(bot_group == "Negative")
  
positive_data <- data_separated %>%
  filter(bot_group == "Positive")

neutral_s_data <- data_separated %>%
  filter(sentiment == "neutral")

negative_s_data <- data_separated %>%
  filter(sentiment == "negative")

positive_s_data <- data_separated %>%
  filter(sentiment == "positive")

neutral_sent_over_time_plot <- ggplot(neutral_s_data, aes(
  x = sample_number,
  y = post_count,
  color = bot_group,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "Positive" = "blue",
      "Control"  = "green",
      "Negative" = "red"
    )
  ) +
  labs(
    title = "Neutral Posts: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Bot group"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

negative_sent_over_time_plot <- ggplot(negative_s_data, aes(
  x = sample_number,
  y = post_count,
  color = bot_group,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "Positive" = "blue",
      "Control"  = "green",
      "Negative" = "red"
    )
  ) +
  labs(
    title = "Negative Posts: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Bot group"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

positive_sent_over_time_plot <- ggplot(positive_s_data, aes(
  x = sample_number,
  y = post_count,
  color = bot_group,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "Positive" = "blue",
      "Control"  = "green",
      "Negative" = "red"
    )
  ) +
  labs(
    title = "Positive Posts: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Bot group"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

control_group_over_time_plot <- ggplot(control_data, aes(
  x = sample_number,
  y = post_count,
  color = sentiment,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "positive" = "blue",
      "neutral"  = "green",
      "negative" = "red"
    )
  ) +
  labs(
    title = "Control Bots: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Sentiment"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

negative_group_over_time_plot <- ggplot(negative_data, aes(
  x = sample_number,
  y = post_count,
  color = sentiment,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "positive" = "blue",
      "neutral"  = "green",
      "negative" = "red"
    )
  ) +
  labs(
    title = "Negative Bots: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Sentiment"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

positive_group_over_time_plot <- ggplot(positive_data, aes(
  x = sample_number,
  y = post_count,
  color = sentiment,
  group = interaction(bot_handle, sentiment)
)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "positive" = "blue",
      "neutral"  = "green",
      "negative" = "red"
    )
  ) +
  labs(
    title = "Positive Bots: Sentiment Over Time",
    x = "Sample Number",
    y = "Post Count",
    color = "Sentiment"
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


ggsave("~/Desktop/control_group_over_time_plot.jpg", plot = control_group_over_time_plot, width = 9, height = 4, dpi = 300)
ggsave("~/Desktop/negative_group_over_time_plot.jpg", plot = negative_group_over_time_plot, width = 9, height = 4, dpi = 300)
ggsave("~/Desktop/positive_group_over_time_plot.jpg", plot = positive_group_over_time_plot, width = 9, height = 4, dpi = 300)
ggsave("~/Desktop/neutral_sent_over_time_plot.jpg", plot = neutral_sent_over_time_plot, width = 9, height = 4, dpi = 300)
ggsave("~/Desktop/negative_sent_over_time_plot.jpg", plot = negative_sent_over_time_plot, width = 9, height = 4, dpi = 300)
ggsave("~/Desktop/positive_sent_over_time_plot.jpg", plot = positive_sent_over_time_plot, width = 9, height = 4, dpi = 300)



# data high confidence

data_highconf <- data_clean %>%
  filter(confidence >= 0.5)

# data top 20

data_clean_t20 <- data_clean %>%
  group_by(bot_handle, sample_number) %>%
  slice_head(n = 20) %>%
  ungroup()

# longitudinal regressions


data_clean <- data_clean %>%
  mutate(
    sentiment_numeric = case_when(
      sentiment == "negative" ~ -1,
      sentiment == "neutral" ~ 0,
      sentiment == "positive" ~ 1
    )
  )

sentiment_time <- data_clean %>%
  group_by(bot_group, bot_handle, sample_number) %>%
  summarise(
    mean_sentiment = mean(sentiment_numeric),
    .groups = "drop"
  )

model_control <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_time %>% filter(bot_group == "Control")
)

model_positive <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_time %>% filter(bot_group == "Positive")
)

model_negative <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_time %>% filter(bot_group == "Negative")
)


# highconf models


data_highconf <- data_highconf %>%
  mutate(
    sentiment_numeric = case_when(
      sentiment == "negative" ~ -1,
      sentiment == "neutral" ~ 0,
      sentiment == "positive" ~ 1
    )
  )
data_highconf <- data_highconf %>%
  mutate(
    bot_group = case_when(
      str_starts(bot_handle, "c") ~ "Control",
      str_starts(bot_handle, "n") ~ "Negative",
      str_starts(bot_handle, "p") ~ "Positive"
    )
  )
sentiment_highconf <- data_highconf %>%
  group_by(bot_group, bot_handle, sample_number) %>%
  summarise(
    mean_sentiment = mean(sentiment_numeric),
    .groups = "drop"
  )


model_hc_control <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_highconf %>% filter(bot_group == "Control")
)

model_hc_positive <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_highconf %>% filter(bot_group == "Positive")
)

model_hc_negative <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_highconf %>% filter(bot_group == "Negative")
)

# top 20

data_clean_t20 <- data_clean_t20 %>%
  mutate(
    sentiment_numeric = case_when(
      sentiment == "negative" ~ -1,
      sentiment == "neutral" ~ 0,
      sentiment == "positive" ~ 1
    )
  )
data_clean_t20 <- data_clean_t20 %>%
  mutate(
    bot_group = case_when(
      str_starts(bot_handle, "c") ~ "Control",
      str_starts(bot_handle, "n") ~ "Negative",
      str_starts(bot_handle, "p") ~ "Positive"
    )
  )
sentiment_clean_t20 <- data_clean_t20 %>%
  group_by(bot_group, bot_handle, sample_number) %>%
  summarise(
    mean_sentiment = mean(sentiment_numeric),
    .groups = "drop"
  )


model_t20_control <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_clean_t20 %>% filter(bot_group == "Control")
)

model_t20_positive <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_clean_t20 %>% filter(bot_group == "Positive")
)

model_t20_negative <- lm(
  mean_sentiment ~ sample_number,
  data = sentiment_clean_t20 %>% filter(bot_group == "Negative")
)

# representation

all_models <- list(
  "Base: Control"      = model_control,
  "Base: Positive"     = model_positive,
  "Base: Negative"     = model_negative,
  "High Conf: Control" = model_hc_control,
  "High Conf: Positive"= model_hc_positive,
  "High Conf: Negative"= model_hc_negative,
  "Top 20: Control"    = model_t20_control,
  "Top 20: Positive"   = model_t20_positive,
  "Top 20: Negative"   = model_t20_negative
)

modelsummary(
  all_models,
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Initial Sentiment",
    "sample_number" = "Sampling Period"
  ),
  gof_omit = "AIC|BIC|Log.Lik|RMSE|Adj",
  title = "Sentiment Drift Over Time by Dataset and Bot Type"
)

# distribution graph

data_sample_early <- data_clean %>%
  filter(sample_number <= 10)

data_sample_late <- data_clean %>%
  filter(sample_number >= 32)



post_counts <- data_clean %>%
  group_by(URI) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )

max_count <- max(post_counts$count)

post_counts_early <- data_sample_early %>%
  group_by(URI) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )
post_counts_late <- data_sample_late %>%
  group_by(URI) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )

early_dist <- post_counts_early %>%
  group_by(count) %>%
  summarise(prop = n() / nrow(post_counts_early), .groups = "drop") %>%
  complete(count = 1:max_count, fill = list(prop = 0))

late_dist <- post_counts_late %>%
  group_by(count) %>%
  summarise(prop = n() / nrow(post_counts_late), .groups = "drop") %>%
  complete(count = 1:max_count, fill = list(prop = 0))

distribution_plot <- ggplot(post_counts, aes(x = count)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)), fill = "Full Sample"),
    binwidth = 1,
    color = "black"
  ) +
  geom_line(
    data = early_dist,
    aes(x = count, y = prop, color = "First 10 Samples"),
    linewidth = 1
  ) +
  geom_line(
    data = late_dist,
    aes(x = count, y = prop, color = "Last 10 Samples"),
    linewidth = 1
  ) +
  scale_fill_manual(values = c("Full Sample" = "grey80")) +
  scale_color_manual(values = c("First 10 Samples" = "blue", "Last 10 Samples" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Post Appearances",
    y = "Proportion of Total Observations",
    fill = "",   # 👈 removes legend title
    color = ""
  ) +
  theme_minimal(base_family = "Times New Roman") + 
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave("~/Desktop/distribution_plot.jpg", distribution_plot, width = 8, height = 4)
 