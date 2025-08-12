library(trelliscope)
library(dplyr)
library(ggplot2)
p <- ggplot(fd_data_shiny, aes(age_years, points)) +
  geom_point(alpha = 0.6, size = 1.7, color = "red") +
  labs(
    title = NULL,
    x = "Age (years)",
    y = "IAAF points"
  ) +
  facet_panels(vars(name, event))
tdf <- as_trelliscope_df(as_panels_df(p), 
                         name = "Olympic Track and Field Athlete IAAF-points vs. Age",
                         description = "Observed IAAF points by age for each athlete-event panel")
view_trelliscope(tdf)