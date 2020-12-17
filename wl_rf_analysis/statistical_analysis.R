install.packages("tidyverse")
install.packages("xlsx")
install.packages("readxl") 
install.packages("openxlsx")
install.packages("janitor")
install.packages("tibble")
install.packages("rlist")
install.packages("plot3D")
install.packages("plotly")

library(tibble)
library(janitor)
library(ggpubr)
library(rstatix)
library(openxlsx)
library(rlist)
library(plot3D)
library(plotly)

setwd("D:/onedrive/rpi/20fall/ITWS-4350/project")
getwd()

remove_drought_days <- function(f1, f2, ...) {
  # use f1 for water levels
  # use f2 for rainfall
  
  fLen = length(f1)
  if (fLen != length(f2)) {
    stop('Error: factors lengths not equal')
  }
  
  i = 0
  j = -1
  k = -1
  l = -1
  
  for (val in f2) {
    if (val == 0) {
      if (k == -1){
        if (j == -1) {
          j = i
          k = i
        }
      }
      else {
        k = i
      }
    }
    else {
      if (k - j > 60) {
        #l = floor((k - j)/2)
        #j = j + ceiling((k - j)/4)
        l = k - j - 20
        j = j + 20
        
        while (l > 0) {
          f1 <- f1[-j]
          f2 <- f2[-j]
          l = l - 1
          fLen = fLen - l
          i = i - 1
        }
      }
      j = -1
      k = -1
    }
    i = i + 1
  }
  
  for (val in f2) {
    if (val == 0) {
      f1 <- f1[-i]
      f2 <- f2[-i]
    }
    i = i + 1
  }
  result <- list("WL" = f1, "RF" = f2)
  return(result)
}

#convertToDateTime(f_date)
water_level_data <- read.xlsx("chennai_reservoir_levels.xlsx", detectDates = TRUE)
rain_data <- read.xlsx("chennai_reservoir_rainfall.xlsx", detectDates = TRUE)
#sapply(c(rain_data["POONDI"]), typeof)

#POONDI
###########
f_POONDI_WL <- as.numeric(as.character(factor(water_level_data$POONDI)))
f_POONDI_RF <- as.numeric(as.character(factor(rain_data$POONDI)))
#hist(f_POONDI_RF, breaks = 300)

tmp <- remove_drought_days(f_POONDI_WL, f_POONDI_RF)
f_POONDI_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_POONDI_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))

model1 <- aov(f_POONDI_WL ~ f_POONDI_RF)
#shapiro_test(sample(residuals(model1), 5000))
#plot(model1)
#ggqqplot(residuals(model1))

#CHOLAVARAM
###########
f_CHOLAVARAM_WL <- as.numeric(as.character(factor(water_level_data$CHOLAVARAM)))
f_CHOLAVARAM_RF <- as.numeric(as.character(factor(rain_data$CHOLAVARAM)))
#hist(f_CHOLAVARAM_RF, breaks = 300)
tmp <- remove_drought_days(f_CHOLAVARAM_WL, f_CHOLAVARAM_RF)
f_CHOLAVARAM_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_CHOLAVARAM_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
model2 <- aov(f_CHOLAVARAM_WL ~ f_CHOLAVARAM_RF)

#REDHILLS
###########
f_REDHILLS_WL <- as.numeric(as.character(factor(water_level_data$REDHILLS)))
f_REDHILLS_RF <- as.numeric(as.character(factor(rain_data$REDHILLS)))
#hist(f_REDHILLS_RF, breaks = 300)
tmp <- remove_drought_days(f_REDHILLS_WL, f_REDHILLS_RF)
f_REDHILLS_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_REDHILLS_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
model3 <- aov(f_REDHILLS_WL ~ f_REDHILLS_RF)

#CHEMBARAMBAKKAM
###########
f_CHEMBARAMBAKKAM_WL <- as.numeric(as.character(factor(water_level_data$CHEMBARAMBAKKAM)))
f_CHEMBARAMBAKKAM_RF <- as.numeric(as.character(factor(rain_data$CHEMBARAMBAKKAM)))
#hist(f_CHEMBARAMBAKKAM_RF, breaks = 300)
tmp <- remove_drought_days(f_CHEMBARAMBAKKAM_WL, f_CHEMBARAMBAKKAM_RF)
f_CHEMBARAMBAKKAM_WL <- as.numeric(as.character(factor(unlist(tmp["WL"]))))
f_CHEMBARAMBAKKAM_RF <- as.numeric(as.character(factor(unlist(tmp["RF"]))))
model4 <- aov(f_CHEMBARAMBAKKAM_WL ~ f_CHEMBARAMBAKKAM_RF)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

#shapiro_test(residuals(model1))
#shapiro_test(residuals(model2))
#shapiro_test(residuals(model3))
#shapiro_test(residuals(model4))

hist(residuals(model1))
hist(residuals(model2))
hist(residuals(model3))
hist(residuals(model4))

ggqqplot(residuals(model1))
ggqqplot(residuals(model2))
ggqqplot(residuals(model3))
ggqqplot(residuals(model4))

re_list <- list("POONDI" = residuals(model1), "CHOLAVARAM" = residuals(model2), "REDHILLS" = residuals(model3), "CHEMBARAMBAKKAM" = residuals(model4))
boxplot(re_list, horizontal=TRUE)


tests_data <- read.xlsx("feature_results.xlsx")

# x, y, z variables
x1 <- tests_data$a
y1 <- tests_data$b
z1 <- tests_data$c
R1 <- tests_data$POONDI
R2 <- tests_data$CHOLAVARAM
R3 <- tests_data$REDHILLS
R4 <- tests_data$CHEMBARAMBAKKAM

# in this case we use VAR4 as continuous, you can put color = ~as.factor(VAR4) to have it as factors
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R1) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R2) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R3) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
plot_ly(tests_data, x = ~x1, y = ~y1, z = ~z1, color = ~R4) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'A'),
                                        yaxis = list(title = 'B'),
                                        zaxis = list(title = 'C')))
