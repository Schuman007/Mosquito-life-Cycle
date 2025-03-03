##############################################################################
### Different Spray Strategies according to Patrick ###
### First Trial:  Mid- July	once a week	for 3 weeks ###
### Second Trial: late July 	Once a week	for 5 weeks ###
### Third Trial: 3rd week of August 	every day	3 consecutive days ###
##############################################################################

library(tidyverse)
library(readxl)
library(ggpubr)
library(patchwork)
library(latex2exp)
library(ochRe)
#############################################################################



TempData <- read_excel("Patrick_Simulation_Data.xlsx",col_types = c("date", "numeric"))

plot(TempData$Date[730:3288], TempData$Tmean[730:3288] )

ggplot(data= TempData, aes(as.Date(Date), Tmean))+
  geom_line(color="red4", linewidth=0.5)+
  geom_point(size=1)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Year'), y= TeX(' Temperature$[^{\\circ}C]$'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="top")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ theme(
    plot.title = element_text(color="Black", size=24, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(breaks = scales::breaks_pretty(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

T1 <- TempData$Tmean

l<-length(T1)
n1<-length(T1)-1
t1<-seq(0,n1)

###  Eggs Parameters ###
########################
### Egg Birth rate ###

alphaA <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  if(T1[i]>16)
    alphaA[i] <- -0.153*T1[i]*T1[i] + 8.61*T1[i] - 97.7
}

### Egg Mortality ###

betaE <- 0.05



### Egg ---> Larvae ###

gammaL <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{ if (T1[i]>10.4)
  gammaL[i] <- (T1[i] - 10.4)/110
}

###  Larvae Parameters ###
##########################

### Larvae Mortality ###

betaL <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  betaL[i] <- 0.02 +0.0007*exp(0.1838*(T1[i]-10))   #exp(-T1[i]/2) + 0.08
}

### Larvae ---> Pupae ###

gammaP <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  gammaP[i] <- -0.0007*T1[i]*T1[i] + 0.0392*T1[i] +0.3911
}

###  Pupae Parameters ###
##########################

### Pupae Mortality ###

betaP <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  betaP[i] <- 0.02 +0.0003*exp(0.2228*(T1[i]-10)) #exp(-T1[i]/2) + 0.03
}

### Pupae ---> Adult ###

gammaA <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  gammaA[i] <- 0.0008*T1[i]*T1[i] - 0.0051*T1[i] +0.0319
}

###  Adult mosquito Parameters ###
##########################

### Adult mosquito Mortality ###

betaA <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  betaA[i] <- max(0.02, 0.04417 + 0.00217*T1[i])
  
  #betaA[i] <- 0.02 +0.0003*exp(0.1745*(T1[i]-10))
}

### Adult mosquito Mortality another functional form ###


betaA1 <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{
  betaA1[i] <- (0.0025*T1[i]^2-0.094*T1[i]+1.02570)/10
}

#######################################################

### Emerging adult mosquito --> Blood seeking adults  ###

gammaB <-0.4
muB<- 0.058
gammaEn <- 0.2


gammaEl <- seq(0, 0, length=n1+1)

for (i in 1:n1)
{ if (T1[i]>10)
  gammaEl[i] <- (T1[i] - 10)/77
}

###############################
gammaem <- 0.1
KP <-  40104 # 250000
KL<-   40104# 250000 #
sigmaA <- 0.5
deltaT <- 0.125
NMmin <- 1000
###############################

###############################

## The model Equation ##
#\begin{eqnarray}
# \frac{dE}{dt} &=& \alpha_A A - \beta_E E -\gamma_LE \\
# \frac{dL}{dt} &=& \gamma_LE -\beta_L L -\frac{\beta_L L^2}{K_L}-\gamma_PL \\
# \frac{dP}{dt} &=& \gamma_PL-\beta_P P -\gamma_A P  \\
# \frac{dA}{dt} &=&  \gamma_A P \sigma_A exp{-\gamma_{em}(1+\frac{P}{K_P})}-\beta_A A -\gamma_B A\\
# \frac{dA_B}{dt} &=& \gamma_B A - \beta_A A_B -\mu_B A_B -\gamma_{En} A_B \\
# \frac{dA_{En}}{dt} &=& \gamma_{En} A_B -\beta_A A_{En} -\gamma_{El} A_{En}\\
# \frac{dA_{El}}{dt} &=& \gamma_{El}A_{En} -\beta_A E_{El}
#\end{eqnarray}
###############################
# initialisation reference Case 

E =     seq(1000000, 1000000, length=n1+1)
L =     seq(100, 100, length=n1+1)
P =     seq(100, 100, length=n1+1)
A =     seq(0, 0, length=n1+1)
A_B =   seq(0, 0, length=n1+1)
A_En =  seq(0, 0, length=n1+1)
A_El =  seq(0, 0, length=n1+1)
NM=seq(1000000, 1000000, length=n1+1)
z<-1




# Life cycle model  
## Reference Case 

for (i in 1:n1){
  
  E[i+1]= E[i]+ deltaT*(alphaA[i]*A_El[i]-betaE*E[i]-gammaL[i]*E[i]*z)
  L[i+1]= L[i]+ deltaT*(gammaL[i]*E[i]*z-betaL[i]*L[i]-(betaL[i]*L[i]*L[i])/KL-gammaP[i]*L[i])
  P[i+1]= P[i]+ deltaT*(gammaP[i]*L[i]-betaP[i]*P[i]-gammaA[i]*P[i])
  A[i+1]= A[i]+ deltaT*(gammaA[i]*P[i]*sigmaA*exp(-gammaem*(1+P[i]/KP))-betaA[i]*A[i] -gammaB*A[i])
  A_B[i+1]= A_B[i]+ deltaT*(gammaB*A[i]-betaA[i]*A_B[i]-muB*A_B[i]-gammaEn*A_B[i])
  A_En[i+1]= A_En[i]+ deltaT*(gammaEn*A_B[i]-betaA[i]*A_En[i]-gammaEl[i]*A_En[i])
  A_El[i+1]= A_El[i]+ deltaT*(gammaEl[i]*A_En[i]-betaA[i]*A_El[i])
  NM[i+1]=E[i+1]+L[i+1]+P[i+1]+A[i+1]+A_B[i]+A_En[i]+A_El[i]
  
}

# initialisation with different death rate betaA1

E1 =     seq(1000000, 1000000, length=n1+1)
L1 =     seq(100, 100, length=n1+1)
P1 =     seq(100, 100, length=n1+1)
A1 =     seq(1, 1, length=n1+1)
A_B1 =   seq(1, 1, length=n1+1)
A_En1 =  seq(1, 1, length=n1+1)
A_El1 =  seq(5, 5, length=n1+1)
NM1=seq(1000000, 1000000, length=n1+1)

for (i in 1:n1){
  
  E1[i+1]= E1[i]+ deltaT*(alphaA[i]*A_El1[i]-betaE*E1[i]-gammaL[i]*E1[i])
  L1[i+1]= L1[i]+ deltaT*(gammaL[i]*E1[i]-betaL[i]*L1[i]-(betaL[i]*L1[i]*L1[i])/KL-gammaP[i]*L1[i])
  P1[i+1]= P1[i]+ deltaT*(gammaP[i]*L1[i]-betaP[i]*P1[i]-gammaA[i]*P1[i])
  A1[i+1]= A1[i]+ deltaT*(gammaA[i]*P1[i]*sigmaA*exp(-gammaem*(1+P1[i]/KP))-betaA1[i]*A1[i] -gammaB*A1[i])
  A_B1[i+1]= A_B1[i]+ deltaT*(gammaB*A1[i]-betaA1[i]*A_B1[i]-muB*A_B1[i]-gammaEn*A_B1[i])
  A_En1[i+1]= A_En1[i]+ deltaT*(gammaEn*A_B1[i]-betaA1[i]*A_En1[i]-gammaEl[i]*A_En1[i])
  A_El1[i+1]= A_El1[i]+ deltaT*(gammaEl[i]*A_En1[i]-betaA1[i]*A_El1[i])
  NM1[i+1]=E1[i+1]+L1[i+1]+P1[i+1]+A1[i+1]+A_B1[i]+A_En1[i]+A_El1[i]
  
}
plot(t1, A1, "l", col="red")
lines(t1, A, "l", col="blue")
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### First Spraying Schedule 
### Spray start             time	interval	               Number of times
### mid- July	             once a week	for                    3 weeks
##############################################################################
# initialisation (We are considering beta1 the U shaped function)

E_A1 =     seq(1000000, 1000000, length=n1+1)
L_A1 =     seq(100, 100, length=n1+1)
P_A1 =     seq(100, 100, length=n1+1)
A_A1 =     seq(1, 1, length=n1+1)
A_B_A1 =   seq(1, 1, length=n1+1)
A_En_A1 =  seq(1, 1, length=n1+1)
A_El_A1 =  seq(5, 5, length=n1+1)
NM_A1=seq(1000000, 1000000, length=n1+1)
zeta <- 0.6

for (i in 1:n1)
  {
  
  E_A1[i+1]= E_A1[i]+ deltaT*(alphaA[i]*A_El_A1[i]-betaE*E_A1[i]-gammaL[i]*E_A1[i])
  L_A1[i+1]= L_A1[i]+ deltaT*(gammaL[i]*E_A1[i]-betaL[i]*L_A1[i]-(betaL[i]*L_A1[i]*L_A1[i])/KL-gammaP[i]*L_A1[i])
  P_A1[i+1]= P_A1[i]+ deltaT*(gammaP[i]*L_A1[i]-betaP[i]*P_A1[i]-gammaA[i]*P_A1[i])
  A_A1[i+1]= A_A1[i]+ deltaT*(gammaA[i]*P_A1[i]*sigmaA*exp(-gammaem*(1+P_A1[i]/KP))-betaA1[i]*A_A1[i] -gammaB*A_A1[i])
  A_B_A1[i+1]= A_B_A1[i]+ deltaT*(gammaB*A_A1[i]-betaA1[i]*A_B_A1[i]-muB*A_B_A1[i]-gammaEn*A_B_A1[i])
  A_En_A1[i+1]= A_En_A1[i]+ deltaT*(gammaEn*A_B_A1[i]-betaA1[i]*A_En_A1[i]-gammaEl[i]*A_En_A1[i])
  A_El_A1[i+1]= A_El_A1[i]+ deltaT*(gammaEl[i]*A_En_A1[i]-betaA1[i]*A_El_A1[i])
  NM_A1[i+1]=E_A1[i+1]+L_A1[i+1]+P_A1[i+1]+A_A1[i+1]+A_B_A1[i]+A_En_A1[i]+A_El_A1[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3849 & i<3851) # Mid July 
  {A_A1[i+1]= A_A1[i]+ deltaT*(gammaA[i]*P_A1[i]*sigmaA*exp(-gammaem*(1+P_A1[i]/KP))-betaA1[i]*A_A1[i] -gammaB*A_A1[i]-zeta*A_A1[i]);
  A_B_A1[i+1]= A_B_A1[i]+ deltaT*(gammaB*A_A1[i]-betaA1[i]*A_B_A1[i]-muB*A_B_A1[i]-gammaEn*A_B_A1[i]-zeta*A_B_A1[i])
  print(i)
  }
  else if (i>3857 & i< 3859)
  {A_A1[i+1]= A_A1[i]+ deltaT*(gammaA[i]*P_A1[i]*sigmaA*exp(-gammaem*(1+P_A1[i]/KP))-betaA1[i]*A_A1[i] -gammaB*A_A1[i]-zeta*A_A1[i]);
  A_B_A1[i+1]= A_B_A1[i]+ deltaT*(gammaB*A_A1[i]-betaA1[i]*A_B_A1[i]-muB*A_B_A1[i]-gammaEn*A_B_A1[i]-zeta*A_B_A1[i])
  } 
  else if (i>3865 & i< 3867)
  {A_A1[i+1]= A_A1[i]+ deltaT*(gammaA[i]*P_A1[i]*sigmaA*exp(-gammaem*(1+P_A1[i]/KP))-betaA1[i]*A_A1[i] -gammaB*A_A1[i]-zeta*A_A1[i]);
  A_B_A1[i+1]= A_B_A1[i]+ deltaT*(gammaB*A_A1[i]-betaA1[i]*A_B_A1[i]-muB*A_B_A1[i]-gammaEn*A_B_A1[i]-zeta*A_B_A1[i])
  } 
  
}

plot(t1, A1, "l", col="red")
lines(t1, A, "l", col="blue")
lines(t1, A_A1, "l", col="green")

plot(t1[1:365], A1[1:365], "l", col="red")
lines(t1[1:365], A[1:365], "l", col="blue")
lines(t1[1:365], A_A1[1:365], "l", col="green")

plot(t1[3845:3867], A_B1[3845:3867], "l", col="red")
lines(t1[3845:3867], A_B_A1[3845:3867], "l", col="green")


plot(t1[3654:4018], A_B1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_B_A1[3654:4018], "l", col="green")

plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A1[3654:4018], "l", col="green")


First.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_B1[3654:4018], 
                                          A_A1[3654:4018],A_B_A1[3654:4018], 
                                          t1[3654:4018], 
                                          TempData$Date[3654:4018])
names(First.Trial_Dataframe) <- c("A1","A_B1", "A_A1", "A_B_A1","TimeFrame", "Date")

pracma::trapz(x = First.Trial_Dataframe$TimeFrame, y = First.Trial_Dataframe$A1)
pracma::trapz(x = First.Trial_Dataframe$TimeFrame, y = First.Trial_Dataframe$A_A1)



ggplot(data=First.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_A1), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggplot(data=First.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A1), colour = "#607848", linewidth=1.5)+
  geom_line(aes(y = A_A1), colour = "#a84830", linewidth=1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'First-Test_Emerging-Adultflip.png',plot = last_plot(),width = 35, height = 25, units = "cm")


ggplot(data=First.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = A_B_A1), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood Seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggplot(data=First.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour =  "#607848", linewidth =1.5)+
  geom_line(aes(y = A_B_A1), colour = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood Seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggsave(filename = 'First-Test_Blood-feeding.png',plot = last_plot(),width = 35, height = 25, units = "cm")
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Second Spraying Schedule 
### Spray start             time	interval	               Number of times
### Late- July	             once a week for                    5 weeks
##############################################################################
# initialisation (We are considering beta1 the U shaped function)



E_A2 =     seq(1000000, 1000000, length=n1+1)
L_A2 =     seq(100, 100, length=n1+1)
P_A2 =     seq(100, 100, length=n1+1)
A_A2 =     seq(1, 1, length=n1+1)
A_B_A2 =   seq(1, 1, length=n1+1)
A_En_A2 =  seq(1, 1, length=n1+1)
A_El_A2 =  seq(5, 5, length=n1+1)
NM_A2=seq(1000000, 1000000, length=n1+1)
zeta <- 0.6

for (i in 1:n1)
{
  
  E_A2[i+1]= E_A2[i]+ deltaT*(alphaA[i]*A_El_A2[i]-betaE*E_A2[i]-gammaL[i]*E_A2[i])
  L_A2[i+1]= L_A2[i]+ deltaT*(gammaL[i]*E_A2[i]-betaL[i]*L_A2[i]-(betaL[i]*L_A2[i]*L_A2[i])/KL-gammaP[i]*L_A2[i])
  P_A2[i+1]= P_A2[i]+ deltaT*(gammaP[i]*L_A2[i]-betaP[i]*P_A2[i]-gammaA[i]*P_A2[i])
  A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i])
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i])
  A_En_A2[i+1]= A_En_A2[i]+ deltaT*(gammaEn*A_B_A2[i]-betaA1[i]*A_En_A2[i]-gammaEl[i]*A_En_A2[i])
  A_El_A2[i+1]= A_El_A2[i]+ deltaT*(gammaEl[i]*A_En_A2[i]-betaA1[i]*A_El_A2[i])
  NM_A2[i+1]=E_A2[i+1]+L_A2[i+1]+P_A2[i+1]+A_A2[i+1]+A_B_A2[i]+A_En_A2[i]+A_El_A2[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3855 & i<3857) # Late July Once a week 
  {A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i]-zeta*A_A2[i]);
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i]-zeta*A_B_A2[i])
  print(i)
  }
  else if (i>3864 & i< 3866)
  {A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i]-zeta*A_A2[i]);
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i]-zeta*A_B_A2[i])
  print(i)
  } 
  else if (i>3873 & i< 3875)
  {A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i]-zeta*A_A2[i]);
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i]-zeta*A_B_A2[i])
  print(i)
  } 
  
  else if (i>3882 & i< 3884)
  {A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i]-zeta*A_A2[i]);
  
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i]-zeta*A_B_A2[i])
  print(i)
  }
  
  else if (i>3891 & i< 3893)
  {A_A2[i+1]= A_A2[i]+ deltaT*(gammaA[i]*P_A2[i]*sigmaA*exp(-gammaem*(1+P_A2[i]/KP))-betaA1[i]*A_A2[i] -gammaB*A_A2[i]-zeta*A_A2[i]);
  A_B_A2[i+1]= A_B_A2[i]+ deltaT*(gammaB*A_A2[i]-betaA1[i]*A_B_A2[i]-muB*A_B_A2[i]-gammaEn*A_B_A2[i]-zeta*A_B_A2[i])
  print(i)
  }
  
}

plot(t1, A1, "l", col="red")
lines(t1, A, "l", col="blue")
lines(t1, A_A2, "l", col="green")

plot(t1[1:365], A1[1:365], "l", col="red")
lines(t1[1:365], A[1:365], "l", col="blue")
lines(t1[1:365], A_A2[1:365], "l", col="green")

plot(t1[3845:3867], A_B1[3845:3867], "l", col="red")
lines(t1[3845:3867], A_B_A2[3845:3867], "l", col="green")


plot(t1[3654:4018], A_B1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_B_A2[3654:4018], "l", col="green")

plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A2[3654:4018], "l", col="green")


Second.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A2[3654:4018],
                                           A_B1[3654:4018],A_B_A2[3654:4018],
                                           t1[3654:4018], TempData$Date[3654:4018] )
names(Second.Trial_Dataframe) <- c("A1", "A_A2", "A_B1", "A_B_A2", "TimeFrame", "Date")

pracma::trapz(x = Second.Trial_Dataframe$TimeFrame, y = Second.Trial_Dataframe$A1)
pracma::trapz(x = Second.Trial_Dataframe$TimeFrame, y = Second.Trial_Dataframe$A_A2)


ggplot(data=Second.Trial_Dataframe, aes(x = TimeFrame))+
  geom_line(aes(y = A1), colour = "#607848", linewidth =1.5)+
  geom_line(aes(y = A_A2), colour = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180,color = "Legend")+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)
#, alpha = .2
ggplot(data=Second.Trial_Dataframe, aes(x = Date))+
geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_A2), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Second-Test_Emerging-AdultFlip.png',plot = last_plot(),width = 35, height = 25, units = "cm")

ggplot(data=Second.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = A_B_A2), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood Seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggplot(data=Second.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour =  "#607848", linewidth = 1.5)+
  geom_line(aes(y = A_B_A2), colour  = "#a84830", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood Seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Second-Test_Blood-feeding1.png',plot = last_plot(),width = 35, height = 25, units = "cm")

# library(ggstream)
# # Load required packages
# 
# library(tidyverse)
# library(wesanderson)
# ggplot(blockbusters, aes(year, box_office, fill = genre)) +
#   geom_stream() +
#   scale_fill_manual(values = wes_palette("Darjeeling2")) +
#   theme_minimal()
####################################################################
####################################################################
####################################################################
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Third Spraying Schedule 
### Spray start           Time	interval	               Number of times
### First week- July	     Twice a week for                    3 weeks
##############################################################################

E_A3 =     seq(1000000, 1000000, length=n1+1)
L_A3 =     seq(100, 100, length=n1+1)
P_A3 =     seq(100, 100, length=n1+1)
A_A3 =     seq(1, 1, length=n1+1)
A_B_A3 =   seq(1, 1, length=n1+1)
A_En_A3 =  seq(1, 1, length=n1+1)
A_El_A3 =  seq(5, 5, length=n1+1)
NM_A3=seq(1000000, 1000000, length=n1+1)
zeta <- 0.6

for (i in 1:n1)
{
  
  E_A3[i+1]= E_A3[i]+ deltaT*(alphaA[i]*A_El_A3[i]-betaE*E_A3[i]-gammaL[i]*E_A3[i])
  L_A3[i+1]= L_A3[i]+ deltaT*(gammaL[i]*E_A3[i]-betaL[i]*L_A3[i]-(betaL[i]*L_A3[i]*L_A3[i])/KL-gammaP[i]*L_A3[i])
  P_A3[i+1]= P_A3[i]+ deltaT*(gammaP[i]*L_A3[i]-betaP[i]*P_A3[i]-gammaA[i]*P_A3[i])
  A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i])
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i])
  A_En_A3[i+1]= A_En_A3[i]+ deltaT*(gammaEn*A_B_A3[i]-betaA1[i]*A_En_A3[i]-gammaEl[i]*A_En_A3[i])
  A_El_A3[i+1]= A_El_A3[i]+ deltaT*(gammaEl[i]*A_En_A3[i]-betaA1[i]*A_El_A3[i])
  NM_A3[i+1]=E_A3[i+1]+L_A3[i+1]+P_A3[i+1]+A_A3[i+1]+A_B_A3[i]+A_En_A3[i]+A_El_A3[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3835 & i<3837) # Late July Once a week 
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  }
  else if (i>3839 & i< 3841)
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  } 
  else if (i>3843 & i< 3845)
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  } 
  
  else if (i>3846 & i< 3848)
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  }
  
  else if (i>3850 & i< 3852)
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  }
  else if (i>3855 & i< 3857)
  {A_A3[i+1]= A_A3[i]+ deltaT*(gammaA[i]*P_A3[i]*sigmaA*exp(-gammaem*(1+P_A3[i]/KP))-betaA1[i]*A_A3[i] -gammaB*A_A3[i]-zeta*A_A3[i]);
  A_B_A3[i+1]= A_B_A3[i]+ deltaT*(gammaB*A_A3[i]-betaA1[i]*A_B_A3[i]-muB*A_B_A3[i]-gammaEn*A_B_A3[i]-zeta*A_B_A3[i])
  print(i)
  }
  
}

plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A2[3654:4018], "l", col="green")
lines(t1[3654:4018], A_A3[3654:4018], "l", col="blue")

plot(t1[3654:4018], A_B1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_B_A2[3654:4018], "l", col="green")
lines(t1[3654:4018], A_B_A3[3654:4018], "l", col="blue")

Third.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A3[3654:4018], 
                                          A_B1[3654:4018],A_B_A3[3654:4018],
                                          t1[3654:4018], TempData$Date[3654:4018])
names(Third.Trial_Dataframe) <- c("A1", "A_A3", "A_B1","A_B_A3","TimeFrame", "Date")

ggplot(data=Third.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = A_A3), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggplot(data=Third.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A1), colour =  "#607848", linewidth = 1.5)+
  geom_line(aes(y = A_A3), colour  = "#a84830", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Third-Test_Emerging-Adult1.png',plot = last_plot(),width = 35, height = 25, units = "cm")

ggplot(data=Third.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_B_A3), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggplot(data=Third.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour  = "#607848", linewidth = 1.5)+
  geom_line(aes(y = A_B_A3), colour = "#a84830", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)



ggsave(filename = 'Third-Test_Blood-Seeking.png',plot = last_plot(),width = 35, height = 25, units = "cm")
####################################################################
####################################################################
####################################################################
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Fourth Spraying Schedule 
### Spray start           Time	interval	               Number of times
### First week of June	   once a week	                      5 weeks
##############################################################################

E_A4 =     seq(1000000, 1000000, length=n1+1)
L_A4 =     seq(100, 100, length=n1+1)
P_A4 =     seq(100, 100, length=n1+1)
A_A4 =     seq(1, 1, length=n1+1)
A_B_A4 =   seq(1, 1, length=n1+1)
A_En_A4 =  seq(1, 1, length=n1+1)
A_El_A4 =  seq(5, 5, length=n1+1)
NM_A4=seq(1000000, 1000000, length=n1+1)
zeta <- 0.6
for (i in 1:n1)
{
  
  E_A4[i+1]= E_A4[i]+ deltaT*(alphaA[i]*A_El_A4[i]-betaE*E_A4[i]-gammaL[i]*E_A4[i])
  L_A4[i+1]= L_A4[i]+ deltaT*(gammaL[i]*E_A4[i]-betaL[i]*L_A4[i]-(betaL[i]*L_A4[i]*L_A4[i])/KL-gammaP[i]*L_A4[i])
  P_A4[i+1]= P_A4[i]+ deltaT*(gammaP[i]*L_A4[i]-betaP[i]*P_A4[i]-gammaA[i]*P_A4[i])
  A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i])
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i])
  A_En_A4[i+1]= A_En_A4[i]+ deltaT*(gammaEn*A_B_A4[i]-betaA1[i]*A_En_A4[i]-gammaEl[i]*A_En_A4[i])
  A_El_A4[i+1]= A_El_A4[i]+ deltaT*(gammaEl[i]*A_En_A4[i]-betaA1[i]*A_El_A4[i])
  NM_A4[i+1]=E_A4[i+1]+L_A4[i+1]+P_A4[i+1]+A_A4[i+1]+A_B_A4[i]+A_En_A4[i]+A_El_A4[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3807 & i<3809) # First week of June Once a week 
  {A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i]-zeta*A_A4[i]);
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i]-zeta*A_B_A4[i])
  print(i)
  }
  else if (i>3814 & i< 3816)
  {A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i]-zeta*A_A4[i]);
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i]-zeta*A_B_A4[i])
  print(i)
  }
  else if (i>3822 & i< 3824)
  {A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i]-zeta*A_A4[i]);
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i]-zeta*A_B_A4[i])
  print(i)
  }
  else if (i>3829 & i< 3831)
  {A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i]-zeta*A_A4[i]);
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i]-zeta*A_B_A4[i])
  print(i)
  }
  else if (i>3835 & i< 3837)
  {A_A4[i+1]= A_A4[i]+ deltaT*(gammaA[i]*P_A4[i]*sigmaA*exp(-gammaem*(1+P_A4[i]/KP))-betaA1[i]*A_A4[i] -gammaB*A_A4[i]-zeta*A_A4[i]);
  A_B_A4[i+1]= A_B_A4[i]+ deltaT*(gammaB*A_A4[i]-betaA1[i]*A_B_A4[i]-muB*A_B_A4[i]-gammaEn*A_B_A4[i]-zeta*A_B_A4[i])
  print(i)
  }
  
}
plot(t1[3654:4018], A_A4[3654:4018], "l", col="blue")

plot(t1[3807:3837], A1[3807:3837], "l", col="red")
lines(t1[3807:3837], A_A4[3807:3837], "l", col="blue")

Fourth.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A4[3654:4018], 
                                           A_B1[3654:4018],A_B_A4[3654:4018],
                                           t1[3654:4018], TempData$Date[3654:4018])
names(Fourth.Trial_Dataframe) <- c("A1", "A_A4","A_B1", "A_B_A4","TimeFrame", "Date")

ggplot(data=Fourth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = A_B_A4), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)



ggplot(data=Fourth.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour  = "#607848", linewidth =1.5)+
  geom_line(aes(y = A_B_A4), colour  = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggsave(filename = 'Fourth-Test_Emerging1.png',plot = last_plot(),width = 35, height = 25, units = "cm")

ggsave(filename = 'Fourth-Test_Seekingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")


plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A1[3654:4018], "l", col="orange")
lines(t1[3654:4018], A_A2[3654:4018], "l", col="green")
lines(t1[3654:4018], A_A3[3654:4018], "l", col="blue")
lines(t1[3654:4018], A_A4[3654:4018], "l", col="purple")

ggplot(data=Fourth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = A_B_A4), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood feeding Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

####################################################################
####################################################################
####################################################################
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Fourth Spraying Schedule 
### Spray start           Time	interval	               Number of times
### mid-May 	             Once a week	for                   5 weeks 
##############################################################################

E_A5 =     seq(1000000, 1000000, length=n1+1)
L_A5 =     seq(100, 100, length=n1+1)
P_A5 =     seq(100, 100, length=n1+1)
A_A5 =     seq(1, 1, length=n1+1)
A_B_A5 =   seq(1, 1, length=n1+1)
A_En_A5 =  seq(1, 1, length=n1+1)
A_El_A5 =  seq(5, 5, length=n1+1)
NM_A5=seq(1000000, 1000000, length=n1+1)
zeta <- 0.6
for (i in 1:n1)
{
  
  E_A5[i+1]= E_A5[i]+ deltaT*(alphaA[i]*A_El_A5[i]-betaE*E_A5[i]-gammaL[i]*E_A5[i])
  L_A5[i+1]= L_A5[i]+ deltaT*(gammaL[i]*E_A5[i]-betaL[i]*L_A5[i]-(betaL[i]*L_A5[i]*L_A5[i])/KL-gammaP[i]*L_A5[i])
  P_A5[i+1]= P_A5[i]+ deltaT*(gammaP[i]*L_A5[i]-betaP[i]*P_A5[i]-gammaA[i]*P_A5[i])
  A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i])
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i])
  A_En_A5[i+1]= A_En_A5[i]+ deltaT*(gammaEn*A_B_A5[i]-betaA1[i]*A_En_A5[i]-gammaEl[i]*A_En_A5[i])
  A_El_A5[i+1]= A_El_A5[i]+ deltaT*(gammaEl[i]*A_En_A5[i]-betaA1[i]*A_El_A5[i])
  NM_A5[i+1]=E_A5[i+1]+L_A5[i+1]+P_A5[i+1]+A_A5[i+1]+A_B_A5[i]+A_En_A5[i]+A_El_A5[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  
  if (i>3776 & i<3778) # Mid week of May Once a week 
  {A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i]-zeta*A_A5[i]);
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i]-zeta*A_B_A5[i])
  print(i)
  }
  else if (i>3783 & i< 3785)
  {A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i]-zeta*A_A5[i]);
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i]-zeta*A_B_A5[i])
  print(i)
  }
  else if (i>3789 & i< 3791)
  {A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i]-zeta*A_A5[i]);
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i]-zeta*A_B_A5[i])
  print(i)
  }
  else if (i>3797 & i< 3799)
  {A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i]-zeta*A_A5[i]);
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i]-zeta*A_B_A5[i])
  print(i)
  }
  else if (i>3804 & i< 3806)
  {A_A5[i+1]= A_A5[i]+ deltaT*(gammaA[i]*P_A5[i]*sigmaA*exp(-gammaem*(1+P_A5[i]/KP))-betaA1[i]*A_A5[i] -gammaB*A_A5[i]-zeta*A_A5[i]);
  A_B_A5[i+1]= A_B_A5[i]+ deltaT*(gammaB*A_A5[i]-betaA1[i]*A_B_A5[i]-muB*A_B_A5[i]-gammaEn*A_B_A5[i]-zeta*A_B_A5[i])
  print(i)
  }
}

plot(t1[3654:4018], A_A5[3654:4018], "l", col="blue")

Fifth.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A5[3654:4018],A_B1[3654:4018],
                                          A_B_A5[3654:4018], t1[3654:4018], TempData$Date[3654:4018])
names(Fifth.Trial_Dataframe) <- c("A1", "A_A5", "A_B1","A_B_A5","TimeFrame", "Date")

ggplot(data=Fifth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_A5), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggplot(data=Fifth.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour = "#607848", linewidth =1.5)+
  geom_line(aes(y = A_B_A5), colour  = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggsave(filename = 'Fifth-Test_Emergingflip.png',plot = last_plot(),width = 35, height = 25, units = "cm")


ggplot(data=Fifth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_B_A5), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Fifth-Test_blood-seekingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")

plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A1[3654:4018], "l", col="orange")
lines(t1[3654:4018], A_A2[3654:4018], "l", col="green")
lines(t1[3654:4018], A_A3[3654:4018], "l", col="blue")
lines(t1[3654:4018], A_A4[3654:4018], "l", col="purple")
lines(t1[3654:4018], A_A5[3654:4018], "l", col="black")
################################################################################
###############################################################################
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Fourth Spraying Schedule 
### Spray start           Time	interval	               Number of times
### First week of June 	   Everyday                     3 consecutive days 
##############################################################################
E_A6 =     seq(1000000, 1000000, length=n1+1)
L_A6 =     seq(100, 100, length=n1+1)
P_A6 =     seq(100, 100, length=n1+1)
A_A6 =     seq(1, 1, length=n1+1)
A_B_A6 =   seq(1, 1, length=n1+1)
A_En_A6 =  seq(1, 1, length=n1+1)
A_El_A6 =  seq(5, 5, length=n1+1)
NM_A6 = seq(1000000, 1000000, length=n1+1)
zeta <- 0.6

for (i in 1:n1)
{
  
  E_A6[i+1]= E_A6[i]+ deltaT*(alphaA[i]*A_El_A6[i]-betaE*E_A6[i]-gammaL[i]*E_A6[i])
  L_A6[i+1]= L_A6[i]+ deltaT*(gammaL[i]*E_A6[i]-betaL[i]*L_A6[i]-(betaL[i]*L_A6[i]*L_A6[i])/KL-gammaP[i]*L_A6[i])
  P_A6[i+1]= P_A6[i]+ deltaT*(gammaP[i]*L_A6[i]-betaP[i]*P_A6[i]-gammaA[i]*P_A6[i])
  A_A6[i+1]= A_A6[i]+ deltaT*(gammaA[i]*P_A6[i]*sigmaA*exp(-gammaem*(1+P_A6[i]/KP))-betaA1[i]*A_A6[i] -gammaB*A_A6[i])
  A_B_A6[i+1]= A_B_A6[i]+ deltaT*(gammaB*A_A6[i]-betaA1[i]*A_B_A6[i]-muB*A_B_A6[i]-gammaEn*A_B_A6[i])
  A_En_A6[i+1]= A_En_A6[i]+ deltaT*(gammaEn*A_B_A6[i]-betaA1[i]*A_En_A6[i]-gammaEl[i]*A_En_A6[i])
  A_El_A6[i+1]= A_El_A6[i]+ deltaT*(gammaEl[i]*A_En_A6[i]-betaA1[i]*A_El_A6[i])
  NM_A6[i+1]=E_A6[i+1]+L_A6[i+1]+P_A6[i+1]+A_A6[i+1]+A_B_A6[i]+A_En_A6[i]+A_El_A6[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3807 & i<3811) # Beginning of June 3 consecutive days
  {A_A6[i+1]= A_A6[i]+ deltaT*(gammaA[i]*P_A6[i]*sigmaA*exp(-gammaem*(1+P_A6[i]/KP))-betaA1[i]*A_A6[i] -gammaB*A_A6[i]-zeta*A_A6[i]);
  A_B_A6[i+1]= A_B_A6[i]+ deltaT*(gammaB*A_A6[i]-betaA1[i]*A_B_A6[i]-muB*A_B_A6[i]-gammaEn*A_B_A6[i]-zeta*A_B_A6[i])
  print(i)
  }

}


plot(t1[3654:4018], A1[3654:4018], "l", col="red")
lines(t1[3654:4018], A_A1[3654:4018], "l", col="orange")
lines(t1[3654:4018], A_A2[3654:4018], "l", col="green")
lines(t1[3654:4018], A_A3[3654:4018], "l", col="blue")
lines(t1[3654:4018], A_A4[3654:4018], "l", col="purple")
lines(t1[3654:4018], A_A5[3654:4018], "l", col="black")
lines(t1[3654:4018], A_A6[3654:4018], "l", col="yellow")
plot(t1[3654:4018], A_A6[3654:4018], "l", col="green4")

Sixth.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A6[3654:4018], A_B1[3654:4018],
                                          A_B_A6[3654:4018], TempData$Date[3654:4018], t1[3654:4018] )
names(Sixth.Trial_Dataframe) <- c("A1", "A_A6","A_B1","A_B_A6","Date","TimeFrame")

ggplot(data=Sixth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_A6), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Sixth-Test_EmergingFlip.png',plot = last_plot(),width = 35, height = 25, units = "cm")

ggplot(data=Sixth.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour  = "#607848", linewidth = 1.5)+
  geom_line(aes(y = A_B_A6), colour  = "#a84830", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Sixth-Test_Emergingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")


ggplot(data=Sixth.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_B_A6), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggsave(filename = 'Sixth-Test_Blood-Seekingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")

################################################################################
###############################################################################
################################################################################
############ Applying adulticide after letting the simulation to run for first ten years
#### Applying Adulticides: We are going to reduce emerging adult and blood seeking adults ####
##############################################################################
### Fourth Spraying Schedule 
### Spray start           Time	interval	               Number of times
### 3rd week of August 	    every day	                   3 consecutive days 
##############################################################################
################################################################################
E_A7 =     seq(1000000, 1000000, length=n1+1)
L_A7 =     seq(100, 100, length=n1+1)
P_A7 =     seq(100, 100, length=n1+1)
A_A7 =     seq(1, 1, length=n1+1)
A_B_A7 =   seq(1, 1, length=n1+1)
A_En_A7 =  seq(1, 1, length=n1+1)
A_El_A7 =  seq(5, 5, length=n1+1)
NM_A7 = seq(1000000, 1000000, length=n1+1)
zeta <- 0.6

for (i in 1:n1)
{
  
  E_A7[i+1]= E_A7[i]+ deltaT*(alphaA[i]*A_El_A7[i]-betaE*E_A7[i]-gammaL[i]*E_A7[i])
  L_A7[i+1]= L_A7[i]+ deltaT*(gammaL[i]*E_A7[i]-betaL[i]*L_A7[i]-(betaL[i]*L_A7[i]*L_A7[i])/KL-gammaP[i]*L_A7[i])
  P_A7[i+1]= P_A7[i]+ deltaT*(gammaP[i]*L_A7[i]-betaP[i]*P_A7[i]-gammaA[i]*P_A7[i])
  A_A7[i+1]= A_A7[i]+ deltaT*(gammaA[i]*P_A7[i]*sigmaA*exp(-gammaem*(1+P_A7[i]/KP))-betaA1[i]*A_A7[i] -gammaB*A_A7[i])
  A_B_A7[i+1]= A_B_A7[i]+ deltaT*(gammaB*A_A7[i]-betaA1[i]*A_B_A7[i]-muB*A_B_A7[i]-gammaEn*A_B_A7[i])
  A_En_A7[i+1]= A_En_A7[i]+ deltaT*(gammaEn*A_B_A7[i]-betaA1[i]*A_En_A7[i]-gammaEl[i]*A_En_A7[i])
  A_El_A7[i+1]= A_El_A7[i]+ deltaT*(gammaEl[i]*A_En_A7[i]-betaA1[i]*A_El_A7[i])
  NM_A7[i+1]=E_A7[i+1]+L_A7[i+1]+P_A7[i+1]+A_A7[i+1]+A_B_A7[i]+A_En_A7[i]+A_El_A7[i]
  
  ## Discarding 10 years and applying the adulticide on the 11th year ##
  if (i>3887 & i<3891) # 3rd week of August  3 consecutive days
  {A_A7[i+1]= A_A7[i]+ deltaT*(gammaA[i]*P_A7[i]*sigmaA*exp(-gammaem*(1+P_A7[i]/KP))-betaA1[i]*A_A7[i] -gammaB*A_A7[i]-zeta*A_A7[i]);
  A_B_A7[i+1]= A_B_A7[i]+ deltaT*(gammaB*A_A7[i]-betaA1[i]*A_B_A7[i]-muB*A_B_A7[i]-gammaEn*A_B_A7[i]-zeta*A_B_A7[i])
  print(i)
  }
  
}

Seventh.Trial_Dataframe <- cbind.data.frame(A1[3654:4018], A_A7[3654:4018], A_B1[3654:4018],
                                            A_B_A7[3654:4018], TempData$Date[3654:4018], t1[3654:4018] )
names(Seventh.Trial_Dataframe) <- c("A1", "A_A7","A_B1", "A_B_A7","Date","TimeFrame")

ggplot(data=Seventh.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "#607848")+
  geom_area(aes(y = -A_A7), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)



ggplot(data=Seventh.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A1), colour  = "#607848", linewidth =1.5)+
  geom_line(aes(y = A_A7), colour  = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)



ggsave(filename = 'Seventh-Test_Blood-Seekingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")


ggplot(data=Seventh.Trial_Dataframe, aes(x = Date))+
  geom_area(aes(y = A_B1), colour = "black", fill = "#607848")+
  geom_area(aes(y =-A_B_A7), colour = "black", fill = "#a84830")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)


ggplot(data=Seventh.Trial_Dataframe, aes(x = Date))+
  geom_line(aes(y = A_B1), colour  = "#607848", linewidth =1.5)+
  geom_line(aes(y =A_B_A7), colour  = "#a84830", linewidth =1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'Seventh-Test_Blood-Seekingline.png',plot = last_plot(),width = 35, height = 25, units = "cm")
################################################################################
################################################################################
############# Binding the dataframes ###########################################
All.simulations<- cbind.data.frame(A1[3654:4018], A_A1[3654:4018], A_A2[3654:4018],
                                   A_A3[3654:4018], A_A4[3654:4018], A_A5[3654:4018], 
                                   A_A6[3654:4018],A_A7[3654:4018],
                                   t1[3654:4018], TempData$Date[3654:4018])

names(All.simulations) <- c("A1","A_A1", "A_A2", "A_A3","A_A4","A_A5","A_A6","A_A7", "TimeFrame", "Date")



All.simulations.Blood.Feeding<- cbind.data.frame(A_B1[3654:4018], A_B_A1[3654:4018], A_B_A2[3654:4018],
                                                 A_B_A3[3654:4018], A_B_A4[3654:4018], A_B_A5[3654:4018], 
                                                 A_B_A6[3654:4018],A_B_A7[3654:4018],
                                   t1[3654:4018], TempData$Date[3654:4018])

names(All.simulations.Blood.Feeding) <- c("A_B1","A_B_A1", "A_B_A2", "A_B_A3","A_B_A4","A_B_A5","A_B_A6",
                            "A_B_A7", "TimeFrame", "Date")


ggplot(data=All.simulations, aes(x = Date))+
  geom_line(aes(y = -A1), colour  = "red")+
  geom_line(aes(y = A_A1), colour = "#a8c018")+
  geom_line(aes(y = A_A2), colour = "#787878")+
  geom_line(aes(y = A_A3), colour = "#96b4d2")+
  geom_line(aes(y = A_A4), colour = "#b4783c")+
  geom_line(aes(y = A_A5), colour = "#a84830")+
  geom_line(aes(y = A_A6), colour = "red4")+
  geom_line(aes(y = A_A7), colour = "blue")+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggsave(filename = 'All.png',plot = last_plot(),width = 35, height = 25, units = "cm")

ggplot(data=All.simulations, aes(x = Date))+
  geom_line(aes(y = A1), colour  = "red", linewidth = 1.5)+
  geom_line(aes(y = A_A1), colour = "#a8c018", linewidth = 1.5)+
  geom_line(aes(y = A_A2), colour = "#787878", linewidth = 1.5)+
  geom_line(aes(y = A_A3), colour = "#96b4d2", linewidth = 1.5)+
  geom_line(aes(y = A_A4), colour = "#b4783c", linewidth = 1.5)+
  geom_line(aes(y = A_A5), colour = "#a84830", linewidth = 1.5)+
  geom_line(aes(y = A_A6), colour = "red4", linewidth = 1.5)+
  geom_line(aes(y = A_A7), colour = "blue", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=All.simulations, aes(x = Date))+
  geom_area(aes(y = A1), colour = "black", fill = "red", alpha = 1/10)+
  geom_area(aes(y = A_A1), colour = "black", fill = "#a8c018",alpha = 1/5)+
  geom_area(aes(y = A_A2), colour = "black",fill = "#787878", alpha = 1/5)+
  geom_area(aes(y = A_A3), colour = "black",fill = "#96b4d2", alpha = 1/5)+
  geom_area(aes(y = A_A4), colour = "black",fill = "#b4783c", alpha = 1/5)+
  geom_area(aes(y = A_A5), colour = "black",fill = "#a84830", alpha = 1/5)+
  geom_area(aes(y = A_A6), colour = "black",fill = "red4", alpha = 1/5)+
  geom_area(aes(y = A_A7), colour = "black",fill = "blue",alpha = 1/5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)



ggplot(data=All.simulations.Blood.Feeding, aes(x = Date))+
  geom_area(aes(y = A_B1), colour  = "black",fill = "red",alpha = 1/5)+
  geom_area(aes(y = A_B_A1), colour = "black",fill = "#a8c018",alpha = 1/5)+
  geom_area(aes(y = A_B_A2), colour = "black",fill = "#787878",alpha = 1/5)+
  geom_area(aes(y = A_B_A3), colour = "black",fill = "#96b4d2",alpha = 1/5)+
  geom_area(aes(y = A_B_A4), colour = "black",fill = "#b4783c",alpha = 1/5)+
  geom_area(aes(y = A_B_A5), colour =  "black",fill ="#a84830",alpha = 1/5)+
  geom_area(aes(y = A_B_A6), colour = "black",fill = "red4",alpha = 1/5)+
  geom_area(aes(y = A_B_A7), colour = "black",fill = "blue",alpha = 1/5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Emerging Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)

ggplot(data=All.simulations.Blood.Feeding, aes(x = Date))+
  geom_line(aes(y = A_B1), colour  = "red", linewidth = 1.5)+
  geom_line(aes(y = A_B_A1), colour = "#a8c018", linewidth = 1.5)+
  geom_line(aes(y = A_B_A2), colour = "#787878", linewidth = 1.5)+
  geom_line(aes(y = A_B_A3), colour = "#96b4d2", linewidth = 1.5)+
  geom_line(aes(y = A_B_A4), colour = "#b4783c", linewidth = 1.5)+
  geom_line(aes(y = A_B_A5), colour = "#a84830", linewidth = 1.5)+
  geom_line(aes(y = A_B_A6), colour = "red4", linewidth = 1.5)+
  geom_line(aes(y = A_B_A7), colour = "blue", linewidth = 1.5)+
  theme_classic2()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=30))+
  theme(axis.title = element_text(color="black", face="bold", size=30))+
  labs(x = TeX('Time'), y= TeX('Population'), size = 180)+
  theme(axis.text = element_text(colour = "black", size = 40))+
  theme(axis.title = element_text( face="bold", size=50))+
  theme(axis.text = element_text(colour = "black", size = 30))+
  theme(axis.title = element_text( face="bold", size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.title.x=element_text(vjust=0,size=55),
        axis.title.y=element_text(vjust=0,size=55) )+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle=0),
        legend.text=element_text(size=25),
        legend.key.height=unit(.3, "cm"))+
  theme(legend.text=element_text(size=rel(5)))+
  theme(legend.text=element_text(size=rel(5)))+ ggtitle("Blood seeking Adult")+ theme(
    plot.title = element_text(color="Black", size=25, face="bold")
  )+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=0)
ggsave(filename = 'All_Blood_Seeking.png',plot = last_plot(),width = 35, height = 25, units = "cm")
