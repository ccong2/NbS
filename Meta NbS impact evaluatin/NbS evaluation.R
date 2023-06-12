library(tidyverse)
library(forestplot)
# library(ggplotify)
# library(patchwork)
# library(ggpmisc)
setwd("D:\\02_My Research\\EU 50cities_distribution\\Nature Revision")

data <- read_csv("percentage.csv")

data[data == 0] <- NA

out_data <- data |>
  pivot_longer(cols = everything() & -id) |>
  mutate(group = gsub("(.+)([123])$", "\\2", name),
         name = gsub("(.+)([123])$", "\\1", name))|>
  pivot_wider() |>
  group_by(id) |>
  mutate(col1 = id,
         col2 = lapply(1:n(), \(i) substitute(expression(val),
           list(val = paste0('mean = ',round(mean(coef, na.rm = TRUE),2),'%','\n',
                             '95%CI = [', round(min(low, na.rm = TRUE),2), '%,',
                             round(max(high, na.rm = TRUE),2), '%]' ))))) %>% 
  group_by(group)
  

out_data$col1 <- ifelse(out_data$col1 == 1, "Green\n Infrastructure (GI)", 
                        ifelse(out_data$col1 == 2, "Green buildings\n", 
                               ifelse(out_data$col1 == 3, "Street trees & \ngreen pavements\n", 
                                      ifelse(out_data$col1 == 4, "Urban green spaces\n & agriculture\n", 
                                             ifelse(out_data$col1 == 5, "Habitat preservation\n & remediation\n", 0)))))

  
out_data |>
  forestplot(mean = coef,
             lower = low,
             upper = high,
             labeltext = c(col1, col2),
             txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex = 0.75),
                              ticks = gpar(cex = 0.8),
                              xlab = gpar(cex = 0.8)),
             title = "Percentage of carbon emissions reduced through sequestration (n=12)",
             boxsize = 0.15,
             xlab = "Estimated offset of carbon emission (%)",
             #new_page = TRUE,
             legend = c("transportation", "industrial", "residential"),
             legend_args = fpLegend(
               pos =list(x=0.9, y=0.5),
               title = "section",
               r = unit(.1, "snpc")
               #gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#ED7C97", "#704D9E", "#F3E79A"))
               #line = c("darkblue", "orange"),
               #summary = c("darkblue", "red"))

data1 <- out_data

# t_ha_yr -----------------------------------------------------------------

data <- read_csv("t_ha_yr.csv")

data[data == 0] <- NA

out_data <- data |>
  pivot_longer(cols = everything() & -id) |>
  mutate(group = gsub("(.+)([123])$", "\\2", name),
         name = gsub("(.+)([123])$", "\\1", name))|>
  pivot_wider() |>
  group_by(id) |>
  mutate(col1 = id,
         col2 = lapply(1:n(), \(i) substitute(expression(val),
                                              list(val = paste0('mean = ',round(mean(coef, na.rm = TRUE),2),'\n',
                                                                '95%CI = [', round(min(low, na.rm = TRUE),2), ',',
                                                                round(max(high, na.rm = TRUE),2), ']' ))))) %>% 
  group_by(group)


out_data$col1 <- ifelse(out_data$col1 == 1, "Green\n Infrastructure (GI)\n", 
                        ifelse(out_data$col1 == 2, "Green buildings\n", 
                               ifelse(out_data$col1 == 3, "Street trees & \ngreen pavements\n", 
                                      ifelse(out_data$col1 == 4, "Urban green spaces\n & agriculture\n", 
                                             ifelse(out_data$col1 == 5, "Habitat preservation\n & remediation\n", 0)))))


out_data |>
  forestplot(mean = coef,
             lower = low,
             upper = high,
             labeltext = c(col1, col2),
             txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex = 0.75),
                              ticks = gpar(cex = 0.8),
                              xlab = gpar(cex = 0.8)),
             title = "Carbon reduction effect through direct and indirect interventions (n=17)",
             boxsize = 0.15,
             xlab = "Annual carbon reduction per unit area (t/ha/yr)",
             #xticks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5),
             #new_page = TRUE,
             legend = c("transportation", "industrial", "residential"),
             legend_args = fpLegend(
               pos =list("topright"),
               title = "section",
               r = unit(.1, "snpc")
               #gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#ED7C97", "#704D9E", "#F3E79A"))


