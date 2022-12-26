library(ggplot2)
library(dplyr)
library(readr)

df = read_csv("dataset.csv")

category_order <- c("Not working and studying",
                 "Not working and not studying",
                 "Working and studying",
                 "Working and not studying")

category_factor <- factor(df$category, levels=category_order)

df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage (%)",
       fill="")

# Focusing on only one cagetory
category_colours <- list("Working and studying" = "#eeeeee",
                         "Working and not studying" = "#e0e0e0",
                         "Not working and studying" = "#bdbdbd",
                         "Not working and not studying" = "#1976d2")

df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage (%)",
       fill="")

# Rearranging the order to make easier to compare categories
category_order_v2 <- c("Working and studying",
                        "Working and not studying",
                        "Not working and studying",
                        "Not working and not studying")

category_factor_v2 <- factor(df$category, levels=category_order_v2)

df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor_v2)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage (%)",
       fill="")

# Simplifying the theme
my_colors <- list(
  title = "#616161",
  axis = "#9e9e9e",
  no_emphasis = "#757575"
)

theme_simple <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colors$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colors$axis),
          axis.ticks = element_line(colour = my_colors$axis),
          axis.text = element_text(colour = my_colors$axis),
          axis.title = element_text(colour = my_colors$axis))
}

df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor_v2)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage (%)",
       fill="") +
  theme_simple()

# Playing with axis
df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor_v2)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage",
       fill="") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), labels=c("", "25%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank())


# Getting only useful labels
df %>% 
  ggplot(aes(x=year, y=value, fill=category_factor_v2)) +
  geom_area() +
  geom_text(aes(label=value), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage",
       fill="") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), labels=c("", "25%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank())