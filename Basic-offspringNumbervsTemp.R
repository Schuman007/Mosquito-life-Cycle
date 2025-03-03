library(ochRe)

## R_{0_{F}} vs Temperature #####

## A --> gammaA0*alphaA*gammaA*gammaB*gammaEl*gammaEn*gammaL*gammaP*sigmaA*exp{-gammaem}

Trap2TempData <- read_excel("Trap2TempData.xlsx",col_types = c("date", "numeric"))



T1 <- Trap2TempData$Tmean
T1_1<- Trap2TempData$Tmean+1.5

l<-length(T1)
n1<-length(T1)-1
t1<-seq(0,n1)

l<-length(T1_1)
n1<-length(T1_1)-1
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
  
  #betaA[i] <- 0.02 +0.0003*exp(0.1745*(T1[i]-10))
}

betaW0 <- 0.000

A0 <- alphaA*gammaL*gammaP*gammaA*gammaEl*gammaB*gammaEn*sigmaA*exp(-gammaem)
B0<- betaA*(betaA+gammaB)*(betaA+gammaEl)*(betaE+gammaL)*(betaL+gammaP+betaW0)*(betaP+gammaA)*(betaP+gammaA)*(betaA+gammaEn+muB)

C0<- A0/B0


betaW1 <- 0.03

A1 <- alphaA*gammaL*gammaP*gammaA*gammaEl*gammaB*gammaEn*sigmaA*exp(-gammaem)
B1<- betaA*(betaA+gammaB)*(betaA+gammaEl)*(betaE+gammaL)*(betaL+gammaP+betaW1)*(betaP+gammaA)*(betaP+gammaA)*(betaA+gammaEn+muB)

C1<- A1/B1

plot(T1, C1)

betaW2 <- 0.3

A2 <- alphaA*gammaL*gammaP*gammaA*gammaEl*gammaB*gammaEn*sigmaA*exp(-gammaem)
B2<- betaA*(betaA+gammaB)*(betaA+gammaEl)*(betaE+gammaL)*(betaL+gammaP+betaW2)*(betaP+gammaA)*(betaP+gammaA)*(betaA+gammaEn+muB)

C2<- A2/B2

plot(T1, C2)

betaW3 <- 0.9

A3 <- alphaA*gammaL*gammaP*gammaA*gammaEl*gammaB*gammaEn*sigmaA*exp(-gammaem)
B3<- betaA*(betaA+gammaB)*(betaA+gammaEl)*(betaE+gammaL)*(betaL+gammaP+betaW3)*(betaP+gammaA)*(betaP+gammaA)*(betaA+gammaEn+muB)

C3<- A3/B3


R0DataFrame_Plot <- cbind.data.frame(T1,C0, C1, C2, C3)

ggplot(data= R0DataFrame_Plot)+
  geom_point(aes(T1, C1),colour = "#78a890", size = 3)+
  geom_point(aes(T1, C2),shape = 1,size = 3,colour = "black")+
  xlim(15, 40)+
  theme_classic2()

colors <- c("C0" = "#607848", "C1" = "#a84830", "C2"= "red4", "C3" = "steelblue") 


ggplot()+
  geom_line(data= R0DataFrame_Plot,aes(x = T1, y = C0, color = "C0"), 
             linewidth=1.5)+
  geom_line(data= R0DataFrame_Plot,aes(x = T1, y = C1, color = "C1"), 
            linewidth=1.5)+
  geom_line(data= R0DataFrame_Plot,aes(x = T1, y = C2, color = "C2"), 
            linewidth=1.5)+
  geom_line(data= R0DataFrame_Plot,aes(x = T1, y = C3, color = "C3"), 
            linewidth=1.5)+
  xlim(15, 40)+
  ylim(0,50)+
  theme_classic2()+
  labs(color= "Legend")+
  scale_color_manual(values = colors)+
  theme(legend.text=element_text(size=30))+
  theme(legend.title = element_blank())+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Temperature'), y= TeX('$\\R_{0_{F}}$'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(plot.title = element_text(color="Black", size=24, face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = 'R0F_Sensitivity.pdf',plot = last_plot(),width = 25, height = 15, units = "cm") 
