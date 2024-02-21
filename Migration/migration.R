library(dplyr)
library(ggplot2)
df_ref=readxl::read_excel("Migration/API_SM.POP.REFG.OR_DS2_en_excel_v2_5997665.xls",
                   sheet = 'Data',
                   col_names = T,
                   range ='A4:BO270')

##############
df_rem<-readxl::read_excel("Migration/API_BX.TRF.PWKR.CD.DT_DS2_en_excel_v2_6000901.xls",
                           sheet='Data',
                           col_names = T,
                           range ='A4:BO270')
########
df_gdp=readxl::read_excel("Iron&Steel/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_5795872.xls",
                          sheet = 'Data',
                          col_names = T,
                          range ='A4:BO270')
df_rem['2022rem']=df_rem['2022']
df_gdp['2022gdp']=df_gdp['2022']
#############
df_co=readxl::read_excel("Iron&Steel/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_5795872.xls",sheet ="Metadata - Countries" )

###############
df<-merge(merge(df_rem[,c('Country Code','Country Name','2022rem')],
                    df_gdp[,c('Country Code','2022gdp')],by='Country Code'),
     df_co[,c('Country Code','Region','IncomeGroup')],by='Country Code')

df<-df%>%filter(!is.na(df$IncomeGroup))  # use only countries and not region. Income group is not indicated for the regions.

# remove extrem values
boxplot(df$`2022gdp`)$out

df2<-df%>%filter(!(`2022rem` %in% boxplot(df$`2022rem`)$out))%>%filter(!(`2022gdp` %in% boxplot(df$`2022gdp`)$out))


df2%>%#filter(`Country Name`=='Haiti')
  ggplot2::ggplot(aes(`2022rem`,`2022gdp`))+
  geom_point()+
  geom_smooth(method = "loess",se=FALSE)

summary(lm(data = df2%>%filter(IncomeGroup %in% c("Low income","Lower middle income","Upper middle income")[1:2]),`2022ref`~`2022gdp`))

###############################################
df_ht<-df_ref%>%filter(`Country Name`=='Haiti')
df_gdp<-df_gdp%>%filter(`Country Name`=='Haiti')

plot(t(df_ht[,5:length(colnames(df_ht))])[-1])

dht<-data.frame(t(df_ht[,5:length(colnames(df_ht))]),t(df_gdp[,5:length(colnames(df_gdp))]))
colnames(dht)<-c('Refugees','GDP')
dht['Year']<-as.numeric(rownames(dht))
dht%>%
  ggplot(dht,aes(Year))+
  geom_line(aes(y=log(Refugees),colour='navy'))+
  geom_line(aes(y=log(GDP),colour='green'))+
  geom_point(data=dpeak,aes(x=x,y=log(ref)), color = "red", size = 3) +
  geom_text(data = dpeak, aes(x = x, y =log(ref), label = x),
            vjust = -0.5, hjust = 0.5, color = "black") +
  labs(title = "Peaks in the Data", x = "Year", y = "Values") +
  theme_minimal()


find_peaks <- function(data_vector) {
  # Find positions of peaks
  peak_positions <- which(diff(sign(diff(data_vector))) == -2) + 1
  
  return(peak_positions)
}

dpeak=data.frame(x=as.numeric(rownames(dht)[find_peaks(dht$Refugees)]), ref = dht[find_peaks(dht$Refugees),])
gref=dht%>%
  ggplot(aes(x=as.numeric(rownames(dht)),y=Refugees))+
  geom_line()+
geom_point(data=dpeak,aes(x=x,y=ref), color = "red", size = 3) +
  geom_text(data = dpeak, aes(x = x, y = ref, label = x),
            vjust = -0.5, hjust = 0.5, color = "black") +
  labs(title = "Peaks in the Data", x = "Year", y = "Values") +
  theme_minimal()

#######33333
dpeak3<-data.frame(x=as.numeric(rownames(dht)[find_peaks(dht$GDP)]), gdp = dht[find_peaks(dht$GDP),])

ggdp=dht%>%
  ggplot(aes(x=as.numeric(rownames(dht)),y=GDP))+
  geom_line()+
  geom_point(data=dpeak2,aes(x=x,y=gdp.GDP), color = "red", size = 3) +
  geom_text(data = dpeak2, aes(x = x, y = gdp.GDP, label = x),
            vjust = -0.5, hjust = 0.5, color = "black") +
  labs(title = "Peaks in the Data", x = "Year", y = "Values") +
  theme_minimal()

######################################################
library(zoo)
argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, 
                     align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

peaks <- argmax(x=rownames(dht), y=dht$Refugees, w=2, span=0.5)


##########################################
###############################################
df_htref<-data.frame(t(df_ref%>%filter(`Country Name`=='Haiti'))[5:length(dim(df_ref)[1]),])
df_htref['Variable']<-"Refugees"
df_htgdp<-t(df_gdp%>%filter(`Country Name`=='Haiti'))
df_gdp['Variable']<-'GDP'

dht<-data.frame(t(df_ht[,5:length(colnames(df_ht))]),t(df_gdp[,5:length(colnames(df_gdp))]))
colnames(dht)<-c('Refugees','GDP')

#################################################################################
###############################################
"API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_5994685.xls"
