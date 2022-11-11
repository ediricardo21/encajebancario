
# Lectura de Librarias ---------------------------------------------------------

library(priceR)
library(readxl)
library(dplyr)
library(xlsx)
library(ggplot2)
library(scales)
library(tidyr)
library(psych)
library(formattable)
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
DFPICHINCHA <- bind_rows(df.list)

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
DFPRODUBANCO <- bind_rows(df.list)
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
DFGYE <- bind_rows(df.list)

#AMAZONAS
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/AMAZONAS/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="AMAZONAS",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFAMAZONAS <- bind_rows(df.list)

#AUSTRO
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/AUSTRO/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="AUSTRO",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFAUSTRO <- bind_rows(df.list)

#BOLIVARIANO
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/BOLIVARIANO/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="BOLIVARIANO",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFBOLIVARIANO <- bind_rows(df.list)

#INTERNACIONAL
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/INTERNACIONAL/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="INTERNACIONAL",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFINTERNACIONAL <- bind_rows(df.list)

#MACHALA
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/MACHALA/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="MACHALA",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFMACHALA <- bind_rows(df.list)

#PACIFICO
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/PACIFICO/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="PACIFICO",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFPACIFICO <- bind_rows(df.list)

#RUMINAHUI
file.path <-"C:/Users/Edison/Desktop/TesisMaestria/Tesis/BASE/RUMINAHUI/"
file.list <- list.files(path = file.path, pattern = "*.xls",full.names = TRUE)
df.list <- lapply(file.list,read_excel,skip=19)

for (i in 1:length(df.list)) {
  a <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  b <- df.list[[i]]
  b <- b[,c(1,3,4)]
  b <- b %>% mutate(BANCO="RUMINAHUI",ANO=a[i])
  colnames(b) <- c("CODIGO","CUENTA","SALDO","BANCO","ANO")
  b$SALDO <- gsub(",", "", b$SALDO) 
  b$SALDO <- as.numeric(b$SALDO)
  b$CODIGO <- as.numeric(b$CODIGO)
  df.list[[i]] <- b
}
DFRUMINAHUI <- bind_rows(df.list)



df <- bind_rows(DFPICHINCHA,DFPRODUBANCO,DFGYE,DFAMAZONAS,DFAUSTRO,DFBOLIVARIANO,DFINTERNACIONAL,DFMACHALA,DFPACIFICO,DFRUMINAHUI)
View(df)


# Creación de base de Encaje ---------------------------------------------


DATA_ENCAJE <- df %>% filter(CODIGO %in% 1102) %>% group_by(ANO) %>% summarise(DEPOSITOS_PARA_ENCAJE=sum(`SALDO`))
CUENTAS_PARA_ENCAJE <- df %>% filter(CODIGO %in% c(210105,210110,210115,210130,210135,210140,210145,210205,
                                           210305,210310,210315,210320,210325,2104,2301,270105,270115,2702,210125)) %>% 
  group_by(ANO) %>% summarise(CUENTAS_PARA_ENCAJE=sum(`SALDO`)) %>% select(CUENTAS_PARA_ENCAJE)
DATA_ENCAJE <- bind_cols(DATA_ENCAJE,CUENTAS_PARA_ENCAJE)

DATA_ENCAJE$TASA_LEGAL_ENCAJE <- c(0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.05,0.05,
                                      0.05,0.05,0.05,0.05)
DATA_ENCAJE <- DATA_ENCAJE %>% mutate(ENCAJE_MINIMO=TASA_LEGAL_ENCAJE*CUENTAS_PARA_ENCAJE)
DATA_ENCAJE <- DATA_ENCAJE %>% mutate(TASA_LEGAL_ENCAJE=TASA_LEGAL_ENCAJE*100)
DATA_ENCAJE <- DATA_ENCAJE %>% filter(ANO>2001)
DATA_ENCAJE <- DATA_ENCAJE %>% mutate(TASA_REAL_ENCAJE=round(((DEPOSITOS_PARA_ENCAJE/CUENTAS_PARA_ENCAJE)*100),2))
DATA_ENCAJE <- DATA_ENCAJE %>% mutate(DIFERENCIA_ENCAJE=DEPOSITOS_PARA_ENCAJE-ENCAJE_MINIMO)
# DATA_TASAS_INTERES ------------------------------------------------------


DATA_TASA_INTERES <- df %>% filter(CODIGO %in% c(51,52,53,54,55,56)) %>% group_by(ANO) %>% 
  summarise(INGRESOS_FINACIEROS=sum(SALDO))
DATA_TASA_ACTIVOS_PRODUCTIVOS <- df %>% filter(CODIGO %in% c(1103,12,13,14,15,170505,170510,170515,19)) %>% group_by(ANO) %>% 
  summarise(ACTIVOS_PRODUCTIVOS=sum(SALDO)) %>% select(ACTIVOS_PRODUCTIVOS)

DATA_TASA_EGRESOS_FINANCIEROS <- df %>% filter(CODIGO %in% c(41,42,43,44,45,47,48)) %>% group_by(ANO) %>% 
  summarise(EGRESOS_FINANCIEROS=sum(SALDO)) %>% select(EGRESOS_FINANCIEROS)

DATA_TASA_PASIVOS_CON_COSTO <- df %>% filter(CODIGO %in% c(21,22,26,27,28,29)) %>% group_by(ANO) %>% 
  summarise(PASIVOS_CON_COSTO=sum(SALDO)) %>% select(PASIVOS_CON_COSTO)

DATA_TASA_INTERES <- bind_cols(DATA_TASA_INTERES,DATA_TASA_ACTIVOS_PRODUCTIVOS,DATA_TASA_EGRESOS_FINANCIEROS,DATA_TASA_PASIVOS_CON_COSTO)

DATA_TASA_INTERES <- DATA_TASA_INTERES %>% mutate(TASA_ACTIVA=(round((INGRESOS_FINACIEROS/ACTIVOS_PRODUCTIVOS),4)*100),
                                                  TASA_PASIVA=(round((EGRESOS_FINANCIEROS/PASIVOS_CON_COSTO),4)*100))
DATA_TASA_INTERES <- DATA_TASA_INTERES %>% mutate(TASA_MARGEN_DE_CONTRIBUCION=TASA_ACTIVA-TASA_PASIVA,
                                                  MARGEN_DE_CONTRIBUCION=INGRESOS_FINACIEROS-EGRESOS_FINANCIEROS)

DATA_TASA_INTERES <- DATA_TASA_INTERES %>% filter(ANO>2001)


# Data escenario cumpliendo tasa legal ------------------------------------


MEAN_TASA_ACTIVA <- mean(DATA_TASA_INTERES$TASA_ACTIVA)/100
MEAN_TASA_PASIVA <- mean(DATA_TASA_INTERES$TASA_PASIVA)/100

DATA_ESCENARIO_1 <- DATA_TASA_INTERES[,1:5]
DATA_ESCENARIO_1 <- bind_cols(DATA_ESCENARIO_1,DATA_ENCAJE$DIFERENCIA_ENCAJE)
DATA_ESCENARIO_1 <- DATA_ESCENARIO_1 %>% rename("DIFERENCIA_ENCAJE"=`...6`)
DATA_ESCENARIO_1 <- DATA_ESCENARIO_1 %>% mutate(INGRESOS_FINACIEROS=INGRESOS_FINACIEROS+(DIFERENCIA_ENCAJE *MEAN_TASA_ACTIVA),
                                                ACTIVOS_PRODUCTIVOS= ACTIVOS_PRODUCTIVOS,
                                                EGRESOS_FINANCIEROS=EGRESOS_FINANCIEROS,
                                                PASIVOS_CON_COSTO=PASIVOS_CON_COSTO)

DATA_ESCENARIO_1<- DATA_ESCENARIO_1 %>% mutate(TASA_ACTIVA_ESCENARIO1=round(((INGRESOS_FINACIEROS/ACTIVOS_PRODUCTIVOS)*100),2),
                                               TASA_PASIVA_ESCENARIO1=round(((EGRESOS_FINANCIEROS/PASIVOS_CON_COSTO)*100),2),
                                               TASA_MARGEN_DE_CONTRIBUCION_ESCENARIO1=TASA_ACTIVA_ESCENARIO1-TASA_PASIVA_ESCENARIO1,
                                               MARGEN_DE_CONTRIBUCION_ESCENARIO1=INGRESOS_FINACIEROS-EGRESOS_FINANCIEROS)




# COMPARATIVA DE ESCENARIOS -----------------------------------------------


COMPARATIVA <- DATA_TASA_INTERES %>% select(ANO,TASA_ACTIVA,TASA_PASIVA,MARGEN_DE_CONTRIBUCION,TASA_MARGEN_DE_CONTRIBUCION)
COMPARATIVA_ESCENARIO1 <- DATA_ESCENARIO_1 %>% select(TASA_ACTIVA_ESCENARIO1,TASA_PASIVA_ESCENARIO1,MARGEN_DE_CONTRIBUCION_ESCENARIO1,TASA_MARGEN_DE_CONTRIBUCION_ESCENARIO1)
COMPARATIVA <- bind_cols(COMPARATIVA,COMPARATIVA_ESCENARIO1)
MEAN_TASA_ACTIVA_ESCENARIO1 <- mean(COMPARATIVA$TASA_ACTIVA_ESCENARIO1)/100
MEAN_TASA_PASIVA_ESCENARIO1 <- mean(COMPARATIVA$TASA_PASIVA_ESCENARIO1)/100

write.xlsx2(Dataencaje,file = "comparativa_tasa_Activa.xlsx",sheetName = "Generales",append = FALSE)


# MOROSIDAD ---------------------------------------------------------------

MOROSIDAD <- df %>% filter(CODIGO %in% c(14,1411,1412,1413,1414,1415,1416,1417,1418,1421,1422,1423,1424,1425,1426,
                                         1427,1428)) %>% group_by(ANO,CODIGO) %>% summarise(SALDO=sum(SALDO))
MOROSIDAD_VENCIDA <- MOROSIDAD %>% filter(CODIGO %in% c(1411,1412,1413,1414,1415,1416,1417,1418,1421,1422,1423,1424,1425,1426,
                                                        1427,1428) ) %>% group_by(ANO) %>% summarise(CARTERA_VENCIDA=sum(SALDO))
MOROSIDAD <- MOROSIDAD %>% filter(CODIGO == 14,ANO>2001) %>% rename("CARTERA_BURTA"=SALDO) %>% select(ANO,CARTERA_BURTA)
MOROSIDAD <- bind_cols(MOROSIDAD,MOROSIDAD_VENCIDA$CARTERA_VENCIDA)
MOROSIDAD <- MOROSIDAD %>% rename("CARTERA_VENCIDA"=`...3`)
MOROSIDAD <- MOROSIDAD %>% mutate(MOROSIDAD=(round(((CARTERA_VENCIDA/CARTERA_BURTA)*100),2)))
MOROSIDAD <- MOROSIDAD %>% select(ANO,MOROSIDAD)

imagen11 <- ggplot(data=MOROSIDAD,aes(x=ANO,y=MOROSIDAD))+
  geom_line(color="Blue")
imagen11



# LIQUIDEZ ----------------------------------------------------------------

LIQUIDEZ <- df %>% filter(CODIGO %in% c(2101,11)) %>% group_by(ANO,CODIGO) %>% summarise(SALDO=sum(SALDO)) %>% ungroup(ANO,CODIGO)
LIQUIDEZ_FONDOS_DISPONIBLES <- LIQUIDEZ %>% filter(CODIGO==(11),ANO>2001) %>% rename("FONDOS_DISPONIBLES"=SALDO)
LIQUIDEZ_DEPOSITOS_A_LA_VISTA <- LIQUIDEZ %>% filter(CODIGO==(2101))
LIQUIDEZ <- LIQUIDEZ_FONDOS_DISPONIBLES
LIQUIDEZ$DEPOSITOS_A_LA_VISTA <- LIQUIDEZ_DEPOSITOS_A_LA_VISTA$SALDO
LIQUIDEZ <- LIQUIDEZ %>% mutate(COEFICIENTE = (round(((FONDOS_DISPONIBLES / DEPOSITOS_A_LA_VISTA)*100),2)))
LIQUIDEZ <- LIQUIDEZ %>% select(COEFICIENTE)

MOROSIDAD_LIQUIDEZ <- bind_cols(MOROSIDAD,LIQUIDEZ)
MOROSIDAD_LIQUIDEZ <- as.data.frame(MOROSIDAD_LIQUIDEZ)

write.xlsx2(MOROSIDAD_LIQUIDEZ,file = "MOROSIDAD_LIQUIDEZ.xlsx",sheetName = "Generales",append = FALSE)


# GRAFICOS ----------------------------------------------------------------

ggplot(data=COMPARATIVA,aes(x=ANO))+
  geom_line(aes(y=TASA_ACTIVA),color="BLUE")+
  geom_line(aes(y=TASA_ACTIVA_ESCENARIO1),color="SKYBLUE")+
  geom_line(aes(y=TASA_PASIVA),color="RED")
  
