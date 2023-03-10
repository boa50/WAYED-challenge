library(ggplot2)
library(dplyr)
library(readr)
library(ggtext)

df = read_csv("dataset.csv")

category_order <- c("Not working and studying",
                 "Neither working nor studying",
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
                         "Neither working nor studying" = "#1976d2")

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
                        "Neither working nor studying")

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
    theme(plot.title = element_markdown(hjust = 0, colour = my_colors$title),
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
first_value <- df %>% 
  filter(category == "Neither working nor studying",
         year == 2016)

last_value <- df %>% 
  filter(category == "Neither working nor studying",
         year == 2020)

y_axis_breaks <- c(23.7, 50, 75, 100)
y_axis_custom <- y_axis_breaks == 23.7

df %>% 
  ggplot(aes(x=year, y=value)) +
  geom_area(aes(fill=category_factor_v2)) +
  geom_text(data=last_value, aes(x=year, y=value), label="29.3%",
            position=position_stack(vjust=1), hjust=-0.15,
            size=4, fontface="bold", colour=my_colors$axis) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage",
       fill="") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0,0,.7), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks=y_axis_breaks,
                     labels=c("23.7%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_markdown(face = ifelse(y_axis_custom, "bold", "plain"),
                                     size = ifelse(y_axis_custom, 10.5, 9)))

# Removing external legend
legend_labels <- df %>% 
  arrange(desc(category_factor_v2)) %>% 
  filter(year == 2018)

df %>% 
  ggplot(aes(x=year, y=value)) +
  geom_area(aes(fill=category_factor_v2), show.legend=FALSE) +
  geom_text(data=legend_labels, aes(x=year, y=value, label=category),
            position=position_stack(vjust=.5)) +
  geom_text(data=last_value, aes(x=year, y=value), label="29.3%",
            position=position_stack(vjust=1), hjust=-0.15,
            size=4, fontface="bold", colour=my_colors$axis) +
  scale_fill_manual(values=category_colours) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage",
       fill="") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0,0,.7), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks=y_axis_breaks,
                     labels=c("23.7%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_markdown(face = ifelse(y_axis_custom, "bold", "plain"),
                                     size = ifelse(y_axis_custom, 10.5, 9)))


# Adjusting colors to focus on adverse values
category_colours_v2 <- list("Working and studying" = "#eeeeee",
                         "Working and not studying" = "#e0e0e0",
                         "Not working and studying" = "#c2c2c2",
                         "Neither working nor studying" = "#ff7043")

df %>% 
  ggplot(aes(x=year, y=value)) +
  geom_area(aes(fill=category_factor_v2), show.legend=FALSE) +
  geom_text(data=legend_labels, aes(x=year, y=value, label=category),
            position=position_stack(vjust=.5), size=3.5,
            colour=c("black", rep(my_colors$title, 3))) +
  geom_text(data=last_value, aes(x=year, y=value), label="29.3%",
            position=position_stack(vjust=1), hjust=-0.15,
            size=4, fontface="bold", colour="black") +
  scale_fill_manual(values=category_colours_v2) +
  labs(title=paste("Youngsters percentage between 15 and 29 years old per",
                   "\nwork relation and studies, Brazil 2016-2020"),
       x="",
       y="Youngsters percentage",
       fill="") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0,0,.7), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks=y_axis_breaks,
                     labels=c("23.7%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_markdown(face=ifelse(y_axis_custom, "bold", "plain"),
                                     size=ifelse(y_axis_custom, 10.5, 9),
                                     colour=ifelse(y_axis_custom, "black", my_colors$axis)))

# Improving textual information
df %>% 
  ggplot(aes(x=year, y=value)) +
  geom_area(aes(fill=category_factor_v2)) +
  geom_text(data=last_value, aes(x=year, y=value), label="29.3%",
            position=position_stack(vjust=1), hjust=-0.15,
            size=4, fontface="bold", colour="black") +
  scale_fill_manual(values=category_colours_v2) +
  labs(title="Youngsters are **<span style='color:#ff7043;'>losing interest</span>** in work and study",
       subtitle=paste("The number of youngsters between 15 and 29 years old that",
                      "are neither working nor studying increased \nin the last",
                      "years in Brazil"),
       y="Youngsters percentage") +
  theme_simple() +
  scale_x_discrete(expand=c(0,0,0,.7), limits=c(2016, 2020)) +
  scale_y_continuous(expand=c(0,0), 
                     breaks=y_axis_breaks,
                     labels=c("23.7%", "50%", "75%", "100%")) +
  theme(axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_markdown(face=ifelse(y_axis_custom, "bold", "plain"),
                                     size=ifelse(y_axis_custom, 10.5, 9),
                                     colour=ifelse(y_axis_custom, "black", my_colors$axis)),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.key.size=unit(0.55, "cm"),
        legend.text=element_text(colour=my_colors$axis, size=9),
        axis.title.x=element_blank(),
        axis.title.y=element_text(hjust=1.05),
        plot.subtitle=element_text(colour=my_colors$axis, margin=margin(b=15)),
        legend.margin=margin(t=0),
        legend.box.margin = margin(l=-80),
        plot.margin = margin(t=5, r=-20, b=5, l=5))
