library(readr)
library(dplyr)
library(ggplot2)

# ---- 1. Loading dataset ----
df <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

print(dim(df))
print(head(df, 5))

# ---- 2. Handling missing values ----
missing_summary <- sapply(df, function(x) sum(is.na(x)))
print(missing_summary)

# Keeping rows with both Critic_Score and Global_Sales
df_clean <- df %>%
  filter(!is.na(Critic_Score), !is.na(Global_Sales))

cat("Rows after cleaning:", nrow(df_clean), "\n")

# ---- 3. Visualisations ----

# Histogram: Global Sales
p1 <- ggplot(df_clean, aes(x = Global_Sales)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "pink") +
  labs(title="Histogram of Global Sales",
       x="Global Sales (millions)", y="Frequency")

print(p1)  # <-- SHOW IN INTERFACE
ggsave("hist_global_sales.png", p1, width=7, height=5)


# Histogram: Critic Score
p2 <- ggplot(df_clean, aes(x = Critic_Score)) +
  geom_histogram(bins = 30, fill="lightgreen", color="red") +
  labs(title="Histogram of Critic Scores",
       x="Critic Score", y="Frequency")

print(p2)  # <-- SHOW IN INTERFACE
ggsave("hist_critic_score.png", p2, width=7, height=5)


# Scatter Plot: Critic Score vs Global Sales
p3 <- ggplot(df_clean, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=TRUE, color="yellow") +
  labs(title="Critic Score vs Global Sales",
       x="Critic Score", y="Global Sales (millions)")

print(p3)  # <-- SHOW IN INTERFACE
ggsave("scatter_cs_vs_sales.png", p3, width=7, height=5)


# Log-transform Global Sales
df_clean$logGlobal <- log1p(df_clean$Global_Sales)

p4 <- ggplot(df_clean, aes(x = Critic_Score, y = logGlobal)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE, color="blue") +
  labs(title="Critic Score vs log(1 + Global Sales)",
       x="Critic Score", y="Log-transformed Global Sales")

print(p4)  # <-- SHOW IN INTERFACE
ggsave("scatter_cs_vs_logsales.png", p4, width=7, height=5)

# ---- 4. Statistical Tests ----
sink("correlation_results.txt")

cat("=== Pearson Correlation ===\n")
pearson_test <- cor.test(df_clean$Critic_Score, df_clean$Global_Sales, method = "pearson")
print(pearson_test)

cat("\n=== Spearman Correlation ===\n")
spearman_test <- cor.test(df_clean$Critic_Score, df_clean$Global_Sales, method = "spearman", exact = FALSE)
print(spearman_test)

cat("\n=== Pearson on log-transformed Sales ===\n")
pearson_log <- cor.test(df_clean$Critic_Score, df_clean$logGlobal, method = "pearson")
print(pearson_log)

sink()

# ---- 5. Output notification ----
cat("Plots saved as PNG files.\n")
cat("Correlation results saved to correlation_results.txt\n")

