#Plots for AZMP data
#Alannah Wudrick Thurs April 29th 

#Install packages
install.packages("car")
library(car)

#Plot for AZMP cruises, grouping spring and fall cruises and plotting methods vs depth

####SPRING####

#SetWD 
setwd('C:/Users/Wudrickal/Documents/R/INPUT/AZMP/Spring')

#List the spring files
spring.files <- list.files(pattern = '.csv')

#Loop through and combine data
Spring.output = NULL

for (i in 1:length(spring.files)){
  
  Spring.out = read.table(spring.files[i], stringsAsFactors = FALSE, header = TRUE, sep=",", na.strings = 'NaN', comment.char = '')
  
  Spring.output = rbind(Spring.output, Spring.out)
}
  
####FALL####

#SetWD 
setwd('C:/Users/Wudrickal/Documents/R/INPUT/AZMP/Fall')

#List the spring files
fall.files <- list.files(pattern = '.csv')

#Loop through and combine data
Fall.output = NULL

for (i in 1:length(fall.files)){
  
  Fall.out = read.table(fall.files[i], stringsAsFactors = FALSE, header = TRUE, sep=",", na.strings = 'NaN', comment.char = '')
  
  Fall.output = rbind(Fall.output, Fall.out)
}


###GENERATE PLOTS###

#Spring index
Spr_index_chl <- Spring.output$METHOD == 'Chl_a_Holm-Hansen_F'
Spr_index_NO2NO3 <- Spring.output$METHOD == 'NO2NO3_Tech_F'
Spr_index_O2 <- Spring.output$METHOD == 'O2_Winkler_Auto'
Spr_index_PO4 <- Spring.output$METHOD == 'PO4_Tech_F'
Spr_index_Sal <- Spring.output$METHOD == 'Salinity_Sal_PSS'
Spr_index_SiO4 <- Spring.output$METHOD == 'SiO4_Tech_F'

#Get the number of observations for each method for spring
as.character(Spr_chl_n <- sum(with(Spring.output, Spring.output$METHOD == 'Chl_a_Holm-Hansen_F')))
as.character(Spr_no2no3_n <- sum(with(Spring.output, Spring.output$METHOD == 'NO2NO3_Tech_F')))
as.character(Spr_o2_n <- sum(with(Spring.output, Spring.output$METHOD == 'O2_Winkler_Auto')))
as.character(Spr_po4_n <- sum(with(Spring.output, Spring.output$METHOD == 'PO4_Tech_F')))
as.character(Spr_sal_n <- sum(with(Spring.output, Spring.output$METHOD == 'Salinity_Sal_PSS')))
as.character(Spr_sio4_n <- sum(with(Spring.output, Spring.output$METHOD == 'SiO4_Tech_F')))


#Fall index
Fal_index_chl <- Fall.output$METHOD == 'Chl_a_Holm-Hansen_F'
Fal_index_NO2NO3 <- Fall.output$METHOD == 'NO2NO3_Tech_F'
Fal_index_O2 <- Fall.output$METHOD == 'O2_Winkler_Auto'
Fal_index_PO4 <- Fall.output$METHOD == 'PO4_Tech_F'
Fal_index_Sal <- Fall.output$METHOD == 'Salinity_Sal_PSS'
Fal_index_SiO4 <- Fall.output$METHOD == 'SiO4_Tech_F'

#Get the number of observations for each method for fall
as.character(Fal_chl_n <- sum(with(Fall.output, Fall.output$METHOD == 'Chl_a_Holm-Hansen_F')))
as.character(Fal_no2no3_n <- sum(with(Fall.output, Fall.output$METHOD == 'NO2NO3_Tech_F')))
as.character(Fal_o2_n <- sum(with(Fall.output, Fall.output$METHOD == 'O2_Winkler_Auto')))
as.character(Fal_po4_n <- sum(with(Fall.output, Fall.output$METHOD == 'PO4_Tech_F')))
as.character(Fal_sal_n <- sum(with(Fall.output, Fall.output$METHOD == 'Salinity_Sal_PSS')))
as.character(Fal_sio4_n <- sum(with(Fall.output, Fall.output$METHOD == 'SiO4_Tech_F')))



#PLOT SPRING

#Plot Chlorophyll Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_chl], Spring.output$HEADER_START_DEPTH[Spr_index_chl], 
     main = paste("Spring Chlorophyll with", Spr_chl_n, "Observations"),
     xlab = "Data value [mg/m3]", 
     ylab = "Depth (m)", 
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_chl])))

#Plot Nitrate Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_NO2NO3], Spring.output$HEADER_START_DEPTH[Spr_index_NO2NO3], 
     main = paste("Spring Nitrate with", Spr_no2no3_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)",
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_NO2NO3])))

#Plot Oxygen Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_O2], Spring.output$HEADER_START_DEPTH[Spr_index_O2], 
     main = paste("Spring Oxygen with", Spr_o2_n, "Observations"), 
     xlab = "Data value [ml/l]", 
     ylab = "Depth (m)", 
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_O2])))

#Plot Phosphate Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_PO4], Spring.output$HEADER_START_DEPTH[Spr_index_PO4], 
     main = paste("Spring Phosphate with", Spr_po4_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)", 
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_PO4])))

#Plot Silicate Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_SiO4], Spring.output$HEADER_START_DEPTH[Spr_index_SiO4], 
     main = paste("Spring Silicate with", Spr_sio4_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)", 
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_SiO4])))

#Plot Salinity Spring
scatterplot(Spring.output$DATA_VALUE[Spr_index_Sal], Spring.output$HEADER_START_DEPTH[Spr_index_Sal], 
     main = paste("Spring Salinity with", Spr_sal_n, "Observations"), 
     xlab = "Data value [PSU]", 
     ylab = "Depth (m)", 
     col = "#006600",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     xlim = c(28,37),
     ylim = rev(range(Spring.output$HEADER_START_DEPTH[Spr_index_Sal])))

#PLOT FALL

#Plot Chlorophyll Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_chl], Fall.output$HEADER_START_DEPTH[Fal_index_chl], 
     main = paste("Fall Chlorophyll with", Fal_chl_n, "Observations"), 
     xlab = "Data value [mg/m3]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     xlim = c(0,19),
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_chl])))


#Plot Nitrate Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_NO2NO3], Fall.output$HEADER_START_DEPTH[Fal_index_NO2NO3], 
     main = paste("Fall Nitrate with", Fal_no2no3_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_NO2NO3])))


#Plot Oxygen Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_O2], Fall.output$HEADER_START_DEPTH[Fal_index_O2], 
     main = paste("Fall Oxygen with", Fal_o2_n, "Observations"), 
     xlab = "Data value [ml/l]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     xlim = c(0,10),
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_O2])))


#Plot Phosphate Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_PO4], Fall.output$HEADER_START_DEPTH[Fal_index_PO4], 
     main = paste("Fall Phosphate with", Fal_po4_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     xlim = c(0,2.5),
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_PO4])))


#Plot Silicate Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_SiO4], Fall.output$HEADER_START_DEPTH[Fal_index_SiO4], 
     main = paste("Fall Silicate with", Fal_sio4_n, "Observations"), 
     xlab = "Data value [mmol/m3]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_SiO4])))


#Plot Salinity Fall
scatterplot(Fall.output$DATA_VALUE[Fal_index_Sal], Fall.output$HEADER_START_DEPTH[Fal_index_Sal], 
     main = paste("Fall Salinity with", Fal_sal_n, "Observations"), 
     xlab = "Data value [PSU]", 
     ylab = "Depth (m)", 
     col = "Blue",
     regLine = FALSE,
     smooth = FALSE,
     boxplots = 'x',
     xlim = c(28, 37),
     ylim = rev(range(Fall.output$HEADER_START_DEPTH[Fal_index_Sal])))
