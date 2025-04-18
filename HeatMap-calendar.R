library(ggTimeSeries)
library(readr)
All_simulations <- read_csv("All.simulations.csv")

## Considering the months from May to September ####

All_simulations<-All_simulations[(151:273),]

## Without Spray ###

Wdot.Spray <- cbind.data.frame(All_simulations$Date, All_simulations$A1)
names(Wdot.Spray) <- c("DateCol", "ValueCol")

## Strategy 1 #####
Wd.Spray1 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A1)
names(Wd.Spray1) <- c("DateCol", "ValueCol")
Wd.Spray2 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A2)
names(Wd.Spray2) <- c("DateCol", "ValueCol")
Wd.Spray3 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A3)
names(Wd.Spray3) <- c("DateCol", "ValueCol")
Wd.Spray4 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A4)
names(Wd.Spray4) <- c("DateCol", "ValueCol")
Wd.Spray5 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A5)
names(Wd.Spray5) <- c("DateCol", "ValueCol")
Wd.Spray6 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A6)
names(Wd.Spray6) <- c("DateCol", "ValueCol")
Wd.Spray7 <- cbind.data.frame(All_simulations$Date, All_simulations$A_A7)
names(Wd.Spray7) <- c("DateCol", "ValueCol")

############# Extracting Day and Month #####
library(lubridate)
Wdot.Spray$Month <-month(as.POSIXlt(Wdot.Spray$DateCol, format="%Y/%m/%d"))
Wdot.Spray$day <- weekdays(as.Date(Wdot.Spray$DateCol))
Wd.Spray1$Month <-month(as.POSIXlt(Wd.Spray1$DateCol, format="%Y/%m/%d"))
Wd.Spray1$day <- weekdays(as.Date(Wd.Spray1$DateCol))
Wd.Spray2$Month <-month(as.POSIXlt(Wd.Spray2$DateCol, format="%Y/%m/%d"))
Wd.Spray2$day <- weekdays(as.Date(Wd.Spray2$DateCol))
Wd.Spray3$Month <-month(as.POSIXlt(Wd.Spray3$DateCol, format="%Y/%m/%d"))
Wd.Spray3$day <- weekdays(as.Date(Wd.Spray3$DateCol))
Wd.Spray4$Month <-month(as.POSIXlt(Wd.Spray4$DateCol, format="%Y/%m/%d"))
Wd.Spray4$day <- weekdays(as.Date(Wd.Spray4$DateCol))
Wd.Spray5$Month <-month(as.POSIXlt(Wd.Spray5$DateCol, format="%Y/%m/%d"))
Wd.Spray5$day <- weekdays(as.Date(Wd.Spray5$DateCol))
Wd.Spray6$Month <-month(as.POSIXlt(Wd.Spray6$DateCol, format="%Y/%m/%d"))
Wd.Spray6$day <- weekdays(as.Date(Wd.Spray6$DateCol))
Wd.Spray7$Month <-month(as.POSIXlt(Wd.Spray7$DateCol, format="%Y/%m/%d"))
Wd.Spray7$day <- weekdays(as.Date(Wd.Spray7$DateCol))

ggplot(data = Wdot.Spray, aes(x = Month, y = day, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_fixed()+
  theme_classic()

ggplot(data = Wd.Spray1, aes(x = Month, y = day, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_fixed()+
  theme_classic()

Wdot.Spray$Strategy <- c("S0")
Wd.Spray1$Strategy <- c("S1")
Wd.Spray2$Strategy <- c("S2")
Wd.Spray3$Strategy <- c("S3")
Wd.Spray4$Strategy <- c("S4")
Wd.Spray5$Strategy <- c("S5")
Wd.Spray6$Strategy <- c("S6")
Wd.Spray7$Strategy <- c("S7")

Ploting.Df <- rbind.data.frame(Wdot.Spray, Wd.Spray1, Wd.Spray2, Wd.Spray3,
                               Wd.Spray4, Wd.Spray5, Wd.Spray6, Wd.Spray7)

ggplot(data = Ploting.Df, aes(x = Month, y = day, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_fixed()+
  theme_classic() + facet_wrap(~Strategy)


ggplot(data = Ploting.Df, aes(x = Month, y = day, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_equal()+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))


Ploting.Df$DoW <- factor(Ploting.Df$day, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

Ploting.Df1<-Ploting.Df[order(Ploting.Df$DoW), ]

PlotGr<-ggplot(data = Ploting.Df1, aes(x = Month, y = DoW, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_equal()+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))

ggsave("Cal.pdf", width = 20, height = 20, units = "cm")

ggplot(data = Ploting.Df1, aes(x = Month, y = DoW, fill = ValueCol)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "E", name="Mosquito \n abundance")+
  scale_y_discrete() +
  scale_x_continuous(
    breaks = seq(1, 12, length = 12),
    labels = c("Jan","Feb","Mar","Apr","May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  coord_equal()+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

ggsave("CalAbundance_New2.pdf", width = 35, height = 35, units = "cm")

ggplot(data = Ploting.Df1, aes(x = Month, y = DoW, fill = ValueCol)) +
  viridis::scale_fill_viridis(name="Mosquito abundance",
                              option = 'C',
                              direction = 1,
                              na.value = "grey93") +
  geom_tile(color = 'white', size = 0.1) +
  facet_wrap('Strategy', ncol = 4) +
  coord_equal()+
  scale_y_discrete()+
  scale_x_continuous(
    breaks = seq(1, 12, length = 12),
    labels = c("Jan","Feb","Mar","Apr","May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

################################################################################
################## Timeseries plots  ###################################
################################################################################

ggplot(data= Ploting.Df )+
  geom_line(aes(DateCol, ValueCol, colour = Strategy), linewidth=1.5)+
  scale_colour_manual(values=c(S0="#607848",
                               S1="#663399",S2="#339999",S3="#CC0033",
                               S4="#FF6600",S5="#FF9933",
                               S6= "#a84830",
                               S7= "red4"))+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))+
  labs(x = "Month", y= 'Mosquito population abundance', size = 180)+
  theme(axis.text.x = element_text(angle = 90))
  
  #theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))


ggsave("TimeSeriesPlot1_Manu1.pdf", width = 35, height = 25, units = "cm")



library(plot3D)
## rearrange data into matrix form
m1 <- matrix(
  data$Variable,
  nrow=length(unique(data$ID)))
hist3D(z = m)

m1 <- matrix(
  Ploting.Df$ValueCol,
  nrow=length(unique(Ploting.Df$Strategy)))
hist3D(z = m1)

library(ggplot2)
library(ggridges)
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_joy() + 
  theme_joy()

ggplot(Ploting.Df, aes(Month, Strategy, height = ValueCol, group = Strategy)) + 
  geom_density_ridges(stat = "identity", scale = 1)

ggplot(Ploting.Df, aes(Month, Strategy)) +   geom_density_ridges2()

ggplot(Ploting.Df, aes(x = Month, y = Strategy )) + 
geom_density_ridges(scale = 1) + facet_wrap(~Strategy)

