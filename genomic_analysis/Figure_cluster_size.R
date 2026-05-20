rm(list=ls())
library(dplyr)
library(readr)


df <- read_csv("all_cluster.csv")


cluster_summary <- df %>%
  group_by(cluster_no) %>%
  summarise(
    size = first(size),  
    n_aTB = sum(types == 0),
    n_sTB = sum(types == 1)
  ) %>%
  mutate(
    cluster_type = case_when(
      n_aTB > 0 & n_sTB == 0 ~ "Both aTB",
      n_aTB == 0 & n_sTB > 0 ~ "Both sTB",
      n_aTB > 0 & n_sTB > 0 ~ "Mix"
    )
  )
#统计数量
plot_df <- cluster_summary %>%
  group_by(cluster_type, size) %>%
  summarise(count = n(), .groups = "drop")

#计算整体p值
tab <- table(cluster_summary$cluster_type, cluster_summary$size)

p_value <- fisher.test(tab)$p.value
p_value


pairwise_fisher_size <- function(df, type1, type2) {
  sub <- df %>% filter(cluster_type %in% c(type1, type2))
  tab <- table(sub$cluster_type, sub$size)
  fisher.test(tab)$p.value
}

p_ab <- pairwise_fisher_size(cluster_summary, "Both aTB", "Both sTB")
p_am <- pairwise_fisher_size(cluster_summary, "Both aTB", "Mix")
p_sm <- pairwise_fisher_size(cluster_summary, "Both sTB", "Mix")


plot_df <- plot_df %>%
  mutate(
    x_label = paste(cluster_type, size, sep = "_")
  )


plot_df$x_label <- factor(
  plot_df$x_label,
  levels = c(
    paste("Both aTB", c(2,3,4,8), sep = "_"),
    paste("Both sTB", c(2,3,4,8), sep = "_"),
    paste("Mix", c(2,3,4,8), sep = "_")
  )
)

library(ggplot2)

p <- ggplot(plot_df, aes(x = x_label, y = count, fill = cluster_type)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = count), vjust = -0.3, size = 4) +
  scale_x_discrete(labels = function(x) gsub(".*_", "", x)) +
  scale_fill_manual(values = c("#7d8faf", "#e88165", "#5fb396")) +
  labs(
    x = "Cluster size",
    y = "Number of clusters"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0),
    legend.position = "none"
  )

p

y_max <- max(plot_df$count)

get_sig <- function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
}

p +
  # Both aTB vs Both sTB
  annotate("segment", x = 1, xend = 4, y = y_max*1.15, yend = y_max*1.15) +
  annotate("text", x = 2.5, y = y_max*1.2,
           label = get_sig(p_ab)) +
           #label = paste0("p = ", signif(p_ab, 2))) +
  
  # Both aTB vs Mix
  annotate("segment", x = 1, xend = 8, y = y_max*1.35, yend = y_max*1.35) +
  annotate("text", x = 4.5, y = y_max*1.4,
           label = get_sig(p_ab)) +
           #label = paste0("p = ", signif(p_am, 2))) +
  
  # Both sTB vs Mix
  annotate("segment", x = 3, xend = 8, y = y_max*1.55, yend = y_max*1.55) +
  annotate("text", x = 5.5, y = y_max*1.6,
           label = get_sig(p_ab)) +
  #label = paste0("p = ", signif(p_sm, 2)))
  
  annotate("text", x = 1.5, y = max(plot_df$count)*1.1, label = "Both aTB") +
  
  annotate("text", x = 3.5, y = max(plot_df$count)*1.1, label = "Both sTB") +
  
  annotate("text", x = 6.5, y = max(plot_df$count)*1.1, label = "Mix")
           

kruskal.test(size ~ cluster_type, data = cluster_summary)
