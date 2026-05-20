
##堆叠条形图
# 1. 加载包
library(ggplot2)
library(dplyr)
library(ggsci)
library(scales)
library(patchwork)

# 2. 读取数据
df <- read.csv("secondary_cases.csv")

# 分组标签
df$types <- factor(df$types, 
                   levels = c(0, 1), 
                   labels = c("Asymptomatic\n(n = 57)", "Symptomatic\n(n = 118)"))

# 3. 封装函数（画单个panel）
plot_panel <- function(data, outcome_var, title_label) {
  
  # 分类 secondary cases
  data$SC_cat <- cut(data[[outcome_var]],
                     breaks = c(-1, 0, 1, 2, Inf),
                     labels = c("0", "1", "2", "≥3"))
  
  data$SC_cat <- factor(data$SC_cat, levels = c("0","1","2","≥3"))
  
  # 计算比例
  df_plot <- data %>%
    group_by(types, SC_cat) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(types) %>%
    mutate(prop = n / sum(n),
           label = ifelse(prop > 0.03, percent(prop, accuracy = 1), ""))
  
  # 计算 p 值
  tab <- table(data$types, data$SC_cat)
  p_val <- fisher.test(tab)$p.value
  p_text <- paste0("p = ", signif(p_val, 3))
  
  # 作图
  p <- ggplot(df_plot, aes(x = types, y = prop, fill = SC_cat)) +
    
    geom_bar(stat = "identity",
             width = 0.6,
             color = "black",
             size = 0.2,
             alpha = 0.8) +   # ⭐ 80%透明度
    
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 3.5) +
    
    scale_fill_manual(values = pal_npg('nrc')(9)[1:4]) +
    
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.08))) +
    
    labs(title = title_label,
         x = NULL,
         y = "Proportion",
         fill = "Secondary cases") +
    
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 11),
      legend.position = "none",  # 合并后统一图例
      panel.grid = element_blank()
    ) +
    
    annotate("text",
             x = 1.5,
             y = 1.05,
             label = p_text,
             size = 4)
  
  return(p)
}

# 4. 分别画三个 panel
p1 <- plot_panel(df, "SC_threshold5", "Secondary Cases (PP > 0.5)")
p2 <- plot_panel(df, "SC_threshold3", "Secondary Cases (PP > 0.3)")
p3 <- plot_panel(df, "SecondaryCases", "Secondary Cases (PP > 0)")

# 5. 合并图（共享图例）
combined_plot <- (p1 | p2 | p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# 显示
print(combined_plot)

# 6. 保存
ggsave("/Users/yll/Documents/scTB/4-结果图片/secondary_cases_3panel.pdf",
       plot = combined_plot,
       width = 14,
       height = 5)