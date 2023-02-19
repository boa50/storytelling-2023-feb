library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)

my_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  # line_complementary = "#78909c"
  line_complementary = "#76D219"
)

theme_boa <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colours$axis),
          axis.ticks = element_line(colour = my_colours$axis),
          axis.text = element_text(colour = my_colours$axis),
          axis.title = element_text(colour = my_colours$axis),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)
          )
}

save_plot <- function(plot_name = "myplot") {
  # ggsave(paste(plot_name, ".png", sep = ""), width = 3840, height = 2460, units = "px", dpi = 425, bg = "transparent")
  ggsave(paste(plot_name, ".png", sep = ""), width = 3840, height = 2904, units = "px", dpi = 425, bg = "transparent")
}

week <- c(1, 2, 3, 4, 5, 6)
tv <- c(0.29, 0.18, 0.10, 0.05, 0.03, 0.01)
phone <- c(0.16, 0.10, 0.08, 0.04, 0.03, 0.06)

df <- data.frame(week, tv, phone)

annotate_last_point <- function(y, colour = "black") {
  list(
    annotate(
      "point",
      x = 6,
      y = {{ y }},
      size = 2,
      shape = 21,
      stroke = 2,
      fill = "white",
      colour = {{ colour }}
    ),
    annotate(
      "text",
      label = {{ y }},
      x = 6,
      y = {{ y }} + 1,
      colour = {{ colour }}
    )
  )
}

plot <- df %>%
  pivot_longer(2:3, names_to = "category") %>%
  ggplot(aes(x = week, y = value * 100)) +
  geom_smooth(aes(colour = category), se = FALSE, size = 2) +
  annotate_last_point(6, my_colours$line_main) +
  annotate_last_point(1, my_colours$divergent) +
  labs(title = "Phones ads are the best",
       x = "Weeks",
       y = "New customres (per 100,000 impressions)") +
  scale_x_continuous(breaks = c(1:6),
                     expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(breaks = seq(0, 30, by = 05),
                     limits = c(0, 30),
                     expand = expansion(mult = 0)) +
  scale_color_manual(values = c(my_colours$line_main, my_colours$divergent)) +
  theme_boa() +
  theme(legend.position = "none",
        plot.margin = unit(rep(1.5, times = 4), "lines"),
        plot.title = element_text(margin = margin(b = 15)),
        axis.title.y = element_text(margin = margin(r = 7), hjust = 1),
        axis.title.x = element_text(margin = margin(t = 7), hjust = 0)
        )

plot +
  geom_richtext(
    inherit.aes = FALSE,
    data = tibble(),
    aes(
      x = 3.5,
      y = 25,
      lineheight = 1.7,
      hjust = 0,
      vjust = 1,
      text.colour = my_colours$title,
      label = "Ads on phones showed an
      <br><b><span style='color:#1976d2; padding-top: 10px;'>increase of
      <span style='font-size:30px;'>5</span> customers</span></b>
      <br>compared to those shown on TVs
      <br>after a six-week campaign"),
    fill = NA,
    label.color = NA)

save_plot()


annotate_point <- function(x, y, colour = "black") {
  list(
    annotate(
      "point",
      x = {{ x }},
      y = {{ y }},
      size = 7,
      colour = {{ colour }}
    ),
    annotate(
      "text",
      label = {{ y }},
      x = {{ x }},
      y = {{ y }},
      colour = "white"
    )
  )
}


df %>%
  pivot_longer(2:3, names_to = "category") %>%
  ggplot(aes(x = week, y = value * 100)) +
  geom_smooth(aes(colour = category), se = FALSE, size = 2) +
  annotate_point(1, 16, my_colours$main) +
  annotate_point(1, 29, my_colours$line_complementary) +
  annotate_point(6, 6, my_colours$main) +
  annotate_point(6, 1, my_colours$line_complementary) +
labs(x = "Weeks") +
  scale_x_continuous(breaks = c(1,6),
                     expand = expansion(mult = c(.02, .02))) +
  scale_y_continuous(breaks = seq(0, 30, by = 05),
                     limits = c(0, 30),
                     expand = expansion(mult = 0)) +
  scale_color_manual(values = c(my_colours$main, my_colours$line_complementary)) +
  theme_boa() +
  theme(legend.position = "none",
        plot.margin = unit(rep(1.5, times = 4), "lines"),
        axis.title.y = element_text(margin = margin(r = 7), hjust = 1),
        axis.title.x = element_text(margin = margin(t = -12), size = 11),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = my_colours$axis, size = 13),
        axis.title = element_text(colour = my_colours$axis),
        axis.title.y.left = element_blank(),
        axis.text.y = element_blank()
  )

save_plot("slide_plot")
