#setwd("\\\\192.168.99.150/25_Laboratore/CVMB/Fermentace")




#######################
library(tidyr)
library(ggplot2)
library(readxl)
library(plotly)

filename <- "Run_201111.xlsx"

data <- read_excel(path = filename, skip = 15)
data

data$LoggingTime <- as.difftime(data$LoggingTime, format = "%H:%M:%S", units = "hours")
data$Stirrer <- as.numeric(data$Stirrer)
data$pO2 <- scan(text = data$pO2, dec = ",")
data$Feed_Pump <- scan(text = data$Feed_Pump, dec = ",")

#################################
ggplot(data)+
  geom_line(aes(x = LoggingTime,y = pO2),
            col = "blue")+
  geom_line(aes(x = LoggingTime, y = Feed_Pump / 200),
            col = "red")+
  geom_line(aes(x = LoggingTime, y = Stirrer/10),
            col = "green")+
  scale_y_continuous(name = "O2", limits = c(-10,110),
                     sec.axis = sec_axis(~.*200, name = "Feed"))+
  xlab(label = "")+
  #facet_wrap(~kultivace, scales = "free")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"))



#################################

sec_y <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "feed"
)

plot_ly(data, x = ~LoggingTime, y = ~pO2, name = "pO2",
        type = "scatter", mode = "lines") %>%
  add_trace(y = ~Feed_Pump, name = "feed",
            yaxis = "y2") %>%
  add_trace(y = ~Stirrer/10, name = "stirrer/10",
            yaxis = "y1") %>%
  layout(title = filename,
    xaxis = list(title = "Logging Time [h]"),
         yaxis = list(title = "pO<sub>2</sub> [%]; stirrer [rpm/10]"),
         yaxis2 = sec_y)

