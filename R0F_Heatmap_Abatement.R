library(tidyverse)
library(readxl)
library(ggpubr)
library(patchwork)
library(latex2exp)
library(ochRe)
library(ggTimeSeries)
library(readr)
library(lubridate)
#############################################################################
TempData <- read_excel("Patrick_Simulation_Data.xlsx",col_types = c("date", "numeric"))

T2010_dataTmean <- TempData$Tmean[3654:4018] 
T2010_dataDate <- TempData$Date[3654:4018] 
T2010TmeanDate_Data <- cbind.data.frame(T2010_dataTmean, T2010_dataDate)
names(T2010TmeanDate_Data)<-c("Tmean", "Date")

T1 <- T2010_dataTmean


l<-length(T1)
n1<-length(T1)-1
t1<-seq(0,n1)


alphaA <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  if(T1[i]>16)
    alphaA[i] <- -0.153*T1[i]*T1[i] + 8.61*T1[i] - 97.7
}

gammaL <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{ if (T1[i]>10.4)
  gammaL[i] <- (T1[i] - 10.4)/110
}


gammaP <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  gammaP[i] <- 0.0007*T1[i]*T1[i] + 0.0392*T1[i] +0.3911 #-0.0007*T1[i]*T1[i] + 0.0392*T1[i] +0.3911
}


gammaA <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  gammaA[i] <- 0.0008*T1[i]*T1[i] - 0.0051*T1[i] +0.0319
}

gammaB <-0.4
muB<- 0.058
gammaEn <- 0.2

gammaEl <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{ if (T1[i]>10)
  gammaEl[i] <- (T1[i] - 10)/77
}

gammaem <- 0.1
sigmaA <- 0.5
betaE <- 0.051

betaL <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  betaL[i] <- 0.02 +0.0007*exp(0.1838*(T1[i]-10))   #exp(-T1[i]/2) + 0.08
}


betaP <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  betaP[i] <- 0.02 +0.0003*exp(0.2228*(T1[i]-10)) #exp(-T1[i]/2) + 0.03
}


betaA <- seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  betaA[i] <- max(0.02, 0.04417 + 0.00217*T1[i])
}

## Reference Case

A0<-seq(0, 0, length=n1+1)
B0<-seq(0, 0, length=n1+1)
C0<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  A0[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  B0[i]<- betaA[i]*(betaA[i]+gammaB)*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB)
  C0[i]<- (A0[i])/(B0[i])
  
}
plot(T1, C0)
plot(T2010_dataDate, C0, "l")

##### Abatement Strategies####

### S1 strategies ####

zetaS1<-seq(0, 0, length=n1+1)

zetaS1<-as.numeric(replace(zetaS1, 196, '0.8'))
zetaS1<-as.numeric(replace(zetaS1, 203, '0.8'))
zetaS1<-as.numeric(replace(zetaS1, 210, '0.8'))

Azeta1<-seq(0, 0, length=n1+1)
Bzeta1<-seq(0, 0, length=n1+1)
Czeta1<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta1[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta1[i]<- betaA[i]*(betaA[i]+gammaB+zetaS1[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS1[i])
  
  Czeta1[i]<- Azeta1[i]/Bzeta1[i]
  
}

#plot(T2010_dataDate, Czeta1)



### S2 strategies ####

zetaS2<-seq(0, 0, length=n1+1)

zetaS2<-as.numeric(replace(zetaS2, 201, '0.8'))
zetaS2<-as.numeric(replace(zetaS2, 208, '0.8'))
zetaS2<-as.numeric(replace(zetaS2, 215, '0.8'))
zetaS2<-as.numeric(replace(zetaS2, 222, '0.8'))
zetaS2<-as.numeric(replace(zetaS2, 229, '0.8'))

Azeta2<-seq(0, 0, length=n1+1)
Bzeta2<-seq(0, 0, length=n1+1)
Czeta2<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta2[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta2[i]<- betaA[i]*(betaA[i]+gammaB+zetaS2[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS2[i])
  
  Czeta2[i]<- Azeta2[i]/Bzeta2[i]
  
}


### S3 strategies ####

zetaS3<-seq(0, 0, length=n1+1)

zetaS3<-as.numeric(replace(zetaS3, 187, '0.8'))
zetaS3<-as.numeric(replace(zetaS3, 189, '0.8'))
zetaS3<-as.numeric(replace(zetaS3, 194, '0.8'))
zetaS3<-as.numeric(replace(zetaS3, 197, '0.8'))
zetaS3<-as.numeric(replace(zetaS3, 201, '0.8'))
zetaS3<-as.numeric(replace(zetaS3, 203, '0.8'))
Azeta3<-seq(0, 0, length=n1+1)
Bzeta3<-seq(0, 0, length=n1+1)
Czeta3<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta3[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta3[i]<- betaA[i]*(betaA[i]+gammaB+zetaS3[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS3[i])
  
  Czeta3[i]<- Azeta3[i]/Bzeta3[i]
  
}


### S4 strategies ####

zetaS4<-seq(0, 0, length=n1+1)

zetaS4<-as.numeric(replace(zetaS4, 157, '0.8'))
zetaS4<-as.numeric(replace(zetaS4, 164, '0.8'))
zetaS4<-as.numeric(replace(zetaS4, 171, '0.8'))
zetaS4<-as.numeric(replace(zetaS4, 178, '0.8'))
zetaS4<-as.numeric(replace(zetaS4, 185, '0.8'))


Azeta4<-seq(0, 0, length=n1+1)
Bzeta4<-seq(0, 0, length=n1+1)
Czeta4<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta4[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta4[i]<- betaA[i]*(betaA[i]+gammaB+zetaS4[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS4[i])
  
  Czeta4[i]<- Azeta4[i]/Bzeta4[i]
  
}

### S5 strategies ####

zetaS5<-seq(0, 0, length=n1+1)

zetaS5<-as.numeric(replace(zetaS5, 135, '0.8'))
zetaS5<-as.numeric(replace(zetaS5, 142, '0.8'))
zetaS5<-as.numeric(replace(zetaS5, 149, '0.8'))
zetaS5<-as.numeric(replace(zetaS5, 156, '0.8'))
zetaS5<-as.numeric(replace(zetaS5, 163, '0.8'))


Azeta5<-seq(0, 0, length=n1+1)
Bzeta5<-seq(0, 0, length=n1+1)
Czeta5<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta5[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta5[i]<- betaA[i]*(betaA[i]+gammaB+zetaS5[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS5[i])
  
  Czeta5[i]<- Azeta5[i]/Bzeta5[i]
  
}


### S6 strategies ####

zetaS6<-seq(0, 0, length=n1+1)

zetaS6<-as.numeric(replace(zetaS6, 157, '0.8'))
zetaS6<-as.numeric(replace(zetaS6, 158, '0.8'))
zetaS6<-as.numeric(replace(zetaS6, 159, '0.8'))



Azeta6<-seq(0, 0, length=n1+1)
Bzeta6<-seq(0, 0, length=n1+1)
Czeta6<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta6[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta6[i]<- betaA[i]*(betaA[i]+gammaB+zetaS6[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS6[i])
  
  Czeta6[i]<- Azeta6[i]/Bzeta6[i]
  
}

### S7 strategies ####

zetaS7<-seq(0, 0, length=n1+1)

zetaS7<-as.numeric(replace(zetaS7, 202, '0.8'))
zetaS7<-as.numeric(replace(zetaS7, 203, '0.8'))
zetaS7<-as.numeric(replace(zetaS7, 204, '0.8'))



Azeta7<-seq(0, 0, length=n1+1)
Bzeta7<-seq(0, 0, length=n1+1)
Czeta7<-seq(0, 0, length=n1+1)
for (i in 1:n1){
  
  Azeta7[i] <- alphaA[i]*gammaL[i]*gammaP[i]*gammaA[i]*gammaEl[i]*gammaB*gammaEn*sigmaA*exp(-gammaem)
  Bzeta7[i]<- betaA[i]*(betaA[i]+gammaB+zetaS7[i])*(betaA[i]+gammaEl[i])*(betaE+gammaL[i])*(betaL[i]+gammaP[i])*(betaP[i]+gammaA[i])*(betaP[i]+gammaA[i])*(betaA[i]+gammaEn+muB+zetaS7[i])
  
  Czeta7[i]<- Azeta7[i]/Bzeta7[i]
  
}



plot(T2010_dataDate, C0, "l")
lines(T2010_dataDate, Czeta1, "l", col="red")
lines(T2010_dataDate, Czeta2, "l", col="blue")
lines(T2010_dataDate, Czeta3, "l", col="green")
lines(T2010_dataDate, Czeta4, "l", col="purple")
lines(T2010_dataDate, Czeta5, "l", col="pink")
lines(T2010_dataDate, Czeta6, "l", col="cyan")
lines(T2010_dataDate, Czeta7, "l", col="yellow")

R0FS0 <- cbind.data.frame(T2010_dataDate, C0)
names(R0FS0) <- c("Time", "Value")

R0FS1 <- cbind.data.frame(T2010_dataDate, Czeta1)
names(R0FS1) <- c("Time", "Value")

R0FS2 <- cbind.data.frame(T2010_dataDate, Czeta2)
names(R0FS2) <- c("Time", "Value")

R0FS3 <- cbind.data.frame(T2010_dataDate, Czeta3)
names(R0FS3) <- c("Time", "Value")

R0FS4 <- cbind.data.frame(T2010_dataDate, Czeta4)
names(R0FS4) <- c("Time", "Value")

R0FS5 <- cbind.data.frame(T2010_dataDate, Czeta5)
names(R0FS5) <- c("Time", "Value")

R0FS6 <- cbind.data.frame(T2010_dataDate, Czeta6)
names(R0FS6) <- c("Time", "Value")

R0FS7 <- cbind.data.frame(T2010_dataDate, Czeta7)
names(R0FS7) <- c("Time", "Value")

R0FS0$Strategy <- c("S0")
R0FS1$Strategy <- c("S1")
R0FS2$Strategy <- c("S2")
R0FS3$Strategy <- c("S3")
R0FS4$Strategy <- c("S4")
R0FS5$Strategy <- c("S5")
R0FS6$Strategy <- c("S6")
R0FS7$Strategy <- c("S7")





R0FS0$Month <-month(as.POSIXlt(R0FS0$Time, format="%Y/%m/%d"))
R0FS0$Day <- weekdays(as.Date(R0FS0$Time))

R0FS1$Month <-month(as.POSIXlt(R0FS1$Time, format="%Y/%m/%d"))
R0FS1$Day <- weekdays(as.Date(R0FS1$Time))

R0FS2$Month <-month(as.POSIXlt(R0FS2$Time, format="%Y/%m/%d"))
R0FS2$Day <- weekdays(as.Date(R0FS2$Time))

R0FS3$Month <-month(as.POSIXlt(R0FS3$Time, format="%Y/%m/%d"))
R0FS3$Day <- weekdays(as.Date(R0FS3$Time))

R0FS4$Month <-month(as.POSIXlt(R0FS4$Time, format="%Y/%m/%d"))
R0FS4$Day <- weekdays(as.Date(R0FS4$Time))

R0FS5$Month <-month(as.POSIXlt(R0FS5$Time, format="%Y/%m/%d"))
R0FS5$Day <- weekdays(as.Date(R0FS5$Time))

R0FS6$Month <-month(as.POSIXlt(R0FS6$Time, format="%Y/%m/%d"))
R0FS6$Day <- weekdays(as.Date(R0FS6$Time))

R0FS7$Month <-month(as.POSIXlt(R0FS7$Time, format="%Y/%m/%d"))
R0FS7$Day <- weekdays(as.Date(R0FS7$Time))

R0FS0$Strategy <- c("S0")
R0FS1$Strategy <- c("S1")
R0FS2$Strategy <- c("S2")
R0FS3$Strategy <- c("S3")
R0FS4$Strategy <- c("S4")
R0FS5$Strategy <- c("S5")
R0FS6$Strategy <- c("S6")
R0FS7$Strategy <- c("S7")

Ploting.Df <- rbind.data.frame(R0FS0, R0FS1, R0FS2, R0FS3,
                               R0FS4, R0FS5, R0FS6, R0FS7)


ggplot(data = Ploting.Df, aes(x = Month, y = Day, fill = Value)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_fixed()+
  theme_classic() + facet_wrap(~Strategy)


ggplot(data = Ploting.Df, aes(x = Month, y = Day, fill = Value)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_equal()+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))


Ploting.Df$DoW <- factor(Ploting.Df$Day, levels= c("Sunday", "Monday", 
                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

Ploting.Df1<-Ploting.Df[order(Ploting.Df$DoW), ]

ggplot(data = Ploting.Df1, aes(x = Month, y = DoW, fill = Value)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "B")+
  scale_y_discrete() +
  coord_equal()+
  theme_classic2() + facet_wrap(~Strategy)+
  theme(legend.text=element_text(size=15))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  theme(axis.text = element_text(colour = "black", size = 20))+
  theme(axis.title = element_text( face="bold", size=20))

ggplot(data = Ploting.Df1, aes(x = Month, y = DoW, fill = Value)) +
  geom_tile(colour="white", width=1)+
  scale_fill_viridis_c(option = "E", name="R0F")+
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

ggsave("CalR0F_New.pdf", width = 35, height = 35, units = "cm")
