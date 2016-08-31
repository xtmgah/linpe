## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message = FALSE----------------------------------------------------
require(dplyr)
require(ggplot2)

## ------------------------------------------------------------------------
mtcars %>% 
  tbl_df() %>%
  group_by(cyl) %>%
  summarise(n = n(), mean_mpg = mean(mpg), sd_mpg = sd(mpg))

## ------------------------------------------------------------------------
ggplot(mtcars, aes(disp, mpg)) + geom_point()

