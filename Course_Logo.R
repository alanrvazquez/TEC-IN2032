# install.packages("hexSticker")

library(hexSticker)

library(tidyverse)
library(qcc)
library(ggQC)

# Generate sample data
set.seed(20190117)
example_df <- data_frame(values = rnorm(n=30*5, mean = 25, sd = .005),
                         subgroup = rep(1:30, 5),
                         n = rep(1:5, each = 30)) %>%
  add_row(values = rnorm(n=2*5, mean = 25 + .006, sd = .005),
          subgroup = rep(31:32, 5),
          n = rep(1:5, each = 2))

p <- example_df %>%
  ggplot(aes(x = subgroup, y = values, group = 1)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_QC(method = "xBar.rBar", auto.label = FALSE, label.digits = 4) +
  scale_x_continuous(expand =  expand_scale(mult = c(.05, .15))) + # Pad the x-axis for the labels
  ylab("") + xlab("") 
violations <- example_df %>%
  QC_Violations(value = "values", grouping = "subgroup", method = "xBar.rBar") %>%
  filter(Violation == TRUE) %>%
  select(data, Index) %>%
  unique()

p <- p + geom_point(data = violations, aes(x = Index, y = data), color = "red")
#+theme_bw()
p <- p + theme_void() + theme_transparent() + theme(legend.position="none")
p

sticker(p, package="IN2032", p_size=25, p_color = "#195e83", s_x=1, s_y=0.8, 
        s_width=1.2, s_height=0.8, h_fill="white", h_color="#195e83",
        filename="IN2032_logo.png")
?sticker
# Help
# https://imagecolorpicker.com/en
