library(priceR)
library(readxl)
library(dplyr)
library(xlsx)
library(ggplot2)
library(scales)
library(tidyr)
library(psych)
# Lectura de bases --------------------------------------------------------

#Pichincha
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/PICHINCHA/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b %>% mutate(BANCO="PICHINCHA",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  df.list[[i]] <- b
}
dfpich <- bind_rows(df.list)

#Produbanco
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/PRODUBANCO/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="PRODUBANCO",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
dfprodu <- bind_rows(df.list)
#Guayaquil
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/GUAYAQUIL/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="GUAYAQUIL",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
dfgye <- bind_rows(df.list)
df <- bind_rows(dfgye,dfpich,dfprodu)
View(df)

#Data Encaje
NUEVO <- df %>% filter(ANO==2001)

Dataencaje <- df %>% filter(CODIGO %in% 1102) %>% group_by(ANO) %>% summarise(ENCAJEREAL=sum(`SALDO`))
EncajeTotal <- df %>% filter(CODIGO %in% c(210105,210110,210115,210130,210135,210140,210145,210205,
                                           210305,210310,210315,210320,210325,2104,2301,270105,270115,2702,210125)) %>% 
  group_by(ANO) %>% summarise(ENCAJETOTAL=sum(`SALDO`)) %>% select(ENCAJETOTAL)
Dataencaje <- bind_cols(Dataencaje,EncajeTotal)

Dataencaje$Encajelegalporcentaje <- c(0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.05,0.05,
                                      0.05,0.05,0.05,0.05)
Dataencaje <- Dataencaje %>% mutate(EncajeLegal=Encajelegalporcentaje*ENCAJEREAL)



BASETASA <- df %>% filter(CODIGO %in% c(1,2,4,5)) %>% group_by(ANO,CODIGO) %>%
  summarise(SALDO=sum(`SALDO`))

BASETASA <- BASETASA %>% pivot_wider(names_from = CODIGO, values_from = SALDO) %>% ungroup(ANO) %>% select(-"ANO")
colnames(BASETASA) <- c("ACTIVO","PASIVO","GASTO","INGRESO")

Dataencaje$TasaReal <- (Dataencaje$ENCAJEREAL/Dataencaje$ENCAJETOTAL)*100
Dataencaje <- bind_cols(Dataencaje,BASETASA)

Dataencaje$Impacto <-Dataencaje$ENCAJEREAL-Dataencaje$EncajeLegal 
Dataencaje$T.ACTIVA.IMPLICITA <- Dataencaje$INGRESO / Dataencaje$ACTIVO
Dataencaje$T.PASIVA.IMPLICITA <- Dataencaje$GASTO / Dataencaje$PASIVO
Dataencaje$MARGEN.INTERMEDIACION <- Dataencaje$T.ACTIVA.IMPLICITA - Dataencaje$T.PASIVA.IMPLICITA

Dataencaje$T.ACTIVA.IMPACTO <- Dataencaje$INGRESO / (Dataencaje$ACTIVO - (Dataencaje$ENCAJEREAL-Dataencaje$EncajeLegal))
Dataencaje$MARGEN.INTERMEDIACION.2  <- (Dataencaje$T.ACTIVA.IMPACTO - Dataencaje$T.PASIVA.IMPLICITA)*100

Dataencaje <- Dataencaje[-1,]

View(Dataencaje)

imagen1 <- ggplot(data = Dataencaje,aes(x=ANO))+
  geom_line(aes(y=(Encajelegalporcentaje)),col="red")+
  geom_line(aes(y=(TasaReal/100)),col="blue")+
  scale_x_continuous("AÑO", labels = as.character(Dataencaje$ANO), breaks = Dataencaje$ANO)+
  ggtitle("Evolución del encaje bancario")+
  scale_y_continuous("%ENCAJE BANCARIO",labels = scales::percent)+
  geom_label(aes(x=2012,y=0.03),label="Porcentaje Legal Banco Central")+
  geom_label(aes(x=2012,y=0.1),label="Porcentaje Real Banca Privada")+
  theme_classic()


plot(imagen1)

imagen2 <- ggplot(data=Dataencaje,aes(x=ANO))+
  ggtitle("Encaje Bancario Valor Monetario")+
  geom_line(aes(y=round(log10(ENCAJEREAL),2)),size=2)+
  geom_line(aes(y=round(log10(EncajeLegal),2)),size=2)+
  geom_ribbon(aes(ymin=round(log10(EncajeLegal),2),ymax=round(log10(ENCAJEREAL),2)),fill="skyblue")+
  geom_text(aes(x= ANO ,y=round(log10(ENCAJEREAL),2), label=round(log10(ENCAJEREAL),2)),col="Blue")+
  geom_text(aes(x= ANO ,y=round(log10(EncajeLegal),2), label=round(log10(EncajeLegal),2)),col="Red")+
  scale_y_continuous("log10 $Encaje Bancario",trans='log10')+
  scale_x_continuous("AÑO", labels = as.character(Dataencaje$ANO), breaks = Dataencaje$ANO)+
  geom_label(aes(x=2012,y=6.5),label="Cantidad requeridad por el Banco Central")+
  geom_label(aes(x=2009,y=9),label="Cantidad aportada al Banco Central")+
  geom_label(aes(x=2011,y=8),label="Diferencia = Impacto Monetario",label.padding = unit(1, "lines"),label.size = 2,
             color="skyblue",fill="navy")+
  theme_classic()

plot(imagen2)

imagen3 <- ggplot(data=Dataencaje,aes(x=ANO))+
  geom_line(aes(y=T.ACTIVA.IMPLICITA),col="black",size=2)+
  geom_line(aes(y=T.ACTIVA.IMPACTO),col="Red",size=2)+
  xlim(2003,2021)+
  ylim(0.05,0.20)+
  geom_ribbon(aes(ymin=0.05,ymax=T.ACTIVA.IMPLICITA),fill="skyblue")+
  geom_ribbon(aes(ymin=T.ACTIVA.IMPLICITA,ymax=T.ACTIVA.IMPACTO),fill="Green")+
  geom_text(aes(x= ANO ,y=T.ACTIVA.IMPACTO, label=round(log10(Impacto),2)),col="Blue")+
  ggtitle("Tasa Activa Efectiva")+
  geom_label(aes(x=2010,y=0.138),label="Tasa considerando Ingresos")+
  geom_label(aes(x=2012,y=0.10),label="Tasa No considerando Ingresos")+
  theme_gray()
plot(imagen3)

GDP <- read_excel("GDP.xlsx")
GDP <- as.data.frame(GDP)
GDP <- GDP %>% filter(Ano > 2001)

RIESGO <- read_excel("RIESGOPAIS.xlsx")
RIESGO <- as.data.frame(RIESGO)
RIESGO <- RIESGO %>% group_by(Ano) %>% summarise(Riesgo=mean(TASA,na.rm=TRUE))
RIESGO <- RIESGO %>% filter(Ano > 01)

Infoextra <- bind_cols(GDP,RIESGO$Riesgo)
colnames(Infoextra) <- c("ANO","GDP","RIESGO")
Infoextra <- bind_cols(Infoextra,(Dataencaje$TasaReal/100))
colnames(Infoextra) <- c("ANO","GDP","RIESGO","TASA REAL")
View(Infoextra)

imagen4 <- ggplot(data=Infoextra,aes(x=ANO))+
  geom_line(aes(y=Infoextra$RIESGO))+
  geom_line(aes(y=Infoextra$`TASA REAL`))+
  scale_y_continuous(name="Riesgo Pais",sec.axis = sec_axis(name="Tasa Real"))

imagen4

#cor(Infoextra$RIESGO,Infoextra$`TASA REAL`)