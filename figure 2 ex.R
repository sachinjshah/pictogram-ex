
## First follow this tutorial to install proper fonts and make them available to R: https://rud.is/rpubs/building-pictograms.html


library(waffle)
library(hrbrthemes)
library(extrafont)
library(dplyr)

## modify plot theme

mod.theme <-theme_ipsum_rc(base_family = "Arial", 
                           plot_title_margin = 0,
                           subtitle_family = "Arial",
                           subtitle_size = 10,
                           subtitle_margin = 0,
                           caption_margin = 0,
                           grid="",
                           plot_margin = margin(0, 0, 0, 0),
                           panel_spacing = grid::unit(0, "lines")) +
  theme_enhance_waffle() +
  theme(legend.position = "none")
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 1))

## ADL
tibble(
  disability = factor(
    c("One independent person-year", "One dependent person-year not attributable to stroke", "One dependent person-year attributable to stroke"),
    levels=c("One independent person-year", "One dependent person-year not attributable to stroke", "One dependent person-year attributable to stroke")
  ),
  py = c(65.9, 33.3, .8)
) -> xdf

xdf

fig2.adl <- ggplot(xdf, aes(label = disability, values = py, color = disability)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE, size = 8) +
  scale_color_manual( name = NULL, values = c(`One independent person-year` = "#157696", `One dependent person-year not attributable to stroke` = "#F0881A", `One dependent person-year attributable to stroke` = "#F03B1A")) +
  scale_label_pictogram(name = NULL, values = c(Fruit = "female", Sandwiches = "female", Pizza = "female")) +
  coord_equal() +
  labs(x=NULL, subtitle="(A) ADL independence")+
  mod.theme 

##Create separate figure for legend 
fig2.adl.legend <- ggplot(xdf, aes(label = disability, values = py, color = disability)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE, size = 8) +
  scale_color_manual( name = NULL, values = c(`One independent person-year` = "#157696", `One dependent person-year not attributable to stroke` = "#F0881A", `One dependent person-year attributable to stroke` = "#F03B1A")) +
  scale_label_pictogram(name = NULL, values = c(Fruit = "female", Sandwiches = "female", Pizza = "female")) +
  coord_equal() +
  labs(x=NULL, subtitle="(A) ADL independence")+
  mod.theme + theme(legend.position = "right") +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))

 legend <- cowplot::get_legend(fig2.adl.legend) #extract legend


## IADL
tibble(
  disability = factor(
    c("Disability free years", "Disability years independent of stroke", "Disability years attributable to stroke"),
    levels=c("Disability free years", "Disability years independent of stroke", "Disability years attributable to stroke")
  ),
  py = c(54.9, 44.5, .6)
) -> xdf

xdf

fig2.iadl <- ggplot(xdf, aes(label = disability, values = py, color = disability)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE, size = 8) +
  scale_color_manual( name = NULL, values = c(`Disability free years` = "#157696", `Disability years independent of stroke` = "#F0881A", `Disability years attributable to stroke` = "#F03B1A")) +
  scale_label_pictogram(name = NULL, values = c(Fruit = "female", Sandwiches = "female", Pizza = "female")) +
  coord_equal() +
  labs(x=NULL, subtitle="(B) IADL independence")+
  mod.theme 


## NH
tibble(
  disability = factor(
    c("Independent living years", "Nursing home years independent of stroke", "Nursing home years attributable to stroke"),
    levels=c("Independent living years", "Nursing home years independent of stroke", "Nursing home years attributable to stroke")
  ),
  py = c(93, 7, 1)
) -> xdf

xdf

fig2.nh <- ggplot(xdf, aes(label = disability, values = py, color = disability)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE, size = 8) +
  scale_color_manual( name = NULL, values = c(`Independent living years` = "#157696", `Nursing home years independent of stroke` = "#F0881A", `Nursing home years attributable to stroke` = "#F03B1A")) +
  scale_label_pictogram(name = NULL, values = c(Fruit = "female", Sandwiches = "female", Pizza = "female")) +
  coord_equal() +
  labs(x=NULL, subtitle="(C) Community dwelling")+
  mod.theme 

## stitch together
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(cowplot)

text.title <- "Figure 2: Contribution of stroke to population ADL independence, IADL independence, and independent living over 100 person-years"

text.title <- gsub('(.{1,100})(\\s|$)', '\\1\n', text.title)
t.title <- textGrob(text.title, gp=gpar(fontsize=11), x = 0.01, hjust=0)

text.caption <- "To estimate the population burden of dependence attributable to strokes, we used the regression model results to calculate the likelihood of independence for each person based on their comorbidities and accounting for their survey sampling weight over seven years (the mean follow-up time of the cohort). Next, we used the same parameters to estimate the dependent-years for the same population assuming no strokes had occurred. The difference between the two measures of dependent-years represents the dependent-years attributable to stroke over 100 person-years (red icon). The blue icons represent independent person-years over 100 person-years. The yellow icons represent dependent-years, not attributable to stroke."
text.caption <- gsub('(.{1,140})(\\s|$)', '\\1\n', text.caption)
text.caption <- paste("Legend:", "ADL – activity of daily living; IADL – instrumental activity of daily living", text.caption, sep="\n")
t.caption <- textGrob(text.caption, gp=gpar(fontsize=9), x=0.01, hjust=0)


## Save images

m <- grid.arrange(fig2.adl, fig2.iadl, fig2.nh, legend , ncol=2, top=t.title, bottom = t.caption)
ggsave("/fig2-full.png", m, width = 8, height = 11)

m1 <- grid.arrange(fig2.adl, fig2.iadl, fig2.nh, legend, ncol=2)
ggsave("/fig2-ex.png", m1, width = 8, height = 9)
