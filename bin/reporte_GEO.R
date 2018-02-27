#Este script va a analizar una tabla obtenida a partir de los metadatos del GEO,
#generará datos descriptivos de la tabla original.
#Después la tabla se filtrará con criterios de búsqueda proporcionados por el usuario y se
#volverán a hacer gráficos descriptivos.

#A continuación se cargará la librería 'ggplot' para hacer gráficos
library("ggplot2")

#A continuación se leerán los datos
GEO<-read.csv(file="data/GEO_tablet.tsv", header = T, sep = "\t", 
              na.strings = "", stringsAsFactors = T) 
#####################################################################
#Aquí se hará con 200 líneas para facilitar las pruebas 
#Este bloque se tiene que comentar cuando se obtengan los datos completos
GEO<-droplevels(GEO[1:200,])


#####################################################################
#A continuación se desconcatenan y simplifican los datos de ORGANISMO que tienen el formato:
#"ORGANISMO,!Sample_"



FINAL_DF <- GEO[0, ]
INTERMEDIATE_DF <- GEO[0, ]
for(i in 1:nrow(GEO)){
  
  message(paste0("Procesando la línea ", i))
  CELDA<-as.character(GEO$ORGANISMO[i])
  ESPECIES<-unlist(strsplit(CELDA, split = ","))
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  #ESPECIES <- ESPECIES [!grepl('1:9', ESPECIES)]
  NUMERO_DE_ESPECIES <- length(ESPECIES)
  for(x in 1:NUMERO_DE_ESPECIES){
    INTERMEDIATE_DF[x,] <-GEO[i,]
    INTERMEDIATE_DF[x,"ORGANISMO"] <- ESPECIES[x]
  }
  FINAL_DF<-rbind(FINAL_DF, INTERMEDIATE_DF)
}

rm(INTERMEDIATE_DF)
GEO.DF<-FINAL_DF

GEO.DF$ORGANISMO<-droplevels(GEO.DF$ORGANISMO)


#####################################################################
#A continuación se generará un data frame con las 6 especies más abundantes de la tabla
#y el número de veces queaparecen. El resto de las especies se englobará en la categoría "other"

#NUMEROS_POR_ESPECIE.DF<-summary(GEO$ORGANISMO, maxsum = 7)

my.summary <- summary(GEO.DF$ORGANISMO, maxsum = 7)
NUMEROS_POR_ESPECIE.DF<-data.frame(ORGANISMO=names(my.summary), NUMERO_DE_ENTRADAS_EN_LA_TABLA=my.summary)

####
#A continuación, el piechart
# Barplot
bp<- ggplot(NUMEROS_POR_ESPECIE.DF, aes(x="", y=NUMERO_DE_ENTRADAS_EN_LA_TABLA,
                                        fill= ORGANISMO))+
  geom_bar(width = 0.3, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

pdf("results/piechart_especies_tabla_original.pdf")
pie
bp
dev.off()

####
#A continuación se realizarán gráfficos de comparación a lo largo del tiempo
GEO.DF$ANO <-0
GEO.DF$MES <-0
GEO.DF$DIA <-0


GEO.DF$FECHA_DE_PUBLICACION <- as.character(GEO.DF$FECHA_DE_PUBLICACION)
for(i in 1:nrow(GEO.DF)){
GEO.DF$ANO[i] <- unlist(strsplit(GEO.DF$FECHA_DE_PUBLICACION[i], split = "_"))[5]
GEO.DF$MES[i] <- unlist(strsplit(GEO.DF$FECHA_DE_PUBLICACION[i], split = "_"))[4]
GEO.DF$DIA[i] <- unlist(strsplit(GEO.DF$FECHA_DE_PUBLICACION[i], split = "_"))[3]
}
#GEO.DF$MES
#GEO.DF$DIA

GEO.DF$ANO <-as.factor(GEO.DF$ANO)
GEO.DF$MES <-as.factor(GEO.DF$MES)
GEO.DF$DIA <-as.factor(GEO.DF$DIA)


####
#A continuación se harán gráficos de puntos unidos por línea para comparar los datos del GEO
#en el tiempo. 
#Existen plots específicos para tiempo, pero actualmente no se conocen
#Buscar cómo convertir los valores de día, año y mes en un formato graficable por gráficos de tiempo

fechas<- summary(GEO.DF$ANO)
NUMEROS_POR_ANO <- data.frame(ANO=names(fechas), NUMERO_DE_ENTRADAS = fechas)
ggplot(NUMEROS_POR_ANO, aes(x=ANO, y=NUMERO_DE_ENTRADAS)) + 
  geom_point()

####
#A CONTINUACIÓN GRAFICARMOS EL NÚMERO DE PROYECTOS CON DESCRIPCIÓN RESPECTO AL AÑO
#PRIMERO CREAREMOS UN VECTOR CON TODOS LOS AÑOS POSIBLES 
anos <- levels(GEO.DF$ANO)
#A CONTINUACIÓN SE CREA UN DATA FRAME VACÍO CON EL FORMATO:
#ANO   NUMERO DE ESTUDIOS CON RESUMEN   NUMERO DE ESTUDIOS SIN RESUMEN  TOTAL DE CARACTERES EN LOS RESUMENES  
#NUMERO DE ESTUDIOS DE MICROARREGLOS  NUMERO_DE_ESTUDIOS_DE_SECUENCIACION NUMERO_DE_ESTUDIOS_EN_HUMANO
#NUMERO_DE_ESTUDIOS_EN_RATON

DATOS_POR_ANO.DF <- data.frame(ANO=factor(anos),
                 CON_RESUMEN=integer(length(anos)), 
                 SIN_RESUMEN=integer(length(anos)),
                 TOTAL_DE_CARACTERES=numeric(length(anos)),
                 ARRAY=integer(length(anos)),
                 SEQ=integer(length(anos)),
                 NUMERO_DE_ESTUDIOS_EN_HUMANO =integer(length(anos)),
                 NUMERO_DE_ESTUDIOS_EN_RATON=integer(length(anos)),
                 stringsAsFactors=FALSE) 

GEO.DF$RESUMEN_DEL_PROYECTO<-as.character(GEO.DF$RESUMEN_DEL_PROYECTO)
for (i in 1:nrow(DATOS_POR_ANO.DF)){
  DATOS_POR_ANO.DF$ANO[i] <-  anos[i]
  DATOS_POR_ANO.DF$CON_RESUMEN[i] <-sum(!is.na(GEO.DF[GEO.DF$ANO == anos[i],"RESUMEN_DEL_PROYECTO"]))
  DATOS_POR_ANO.DF$SIN_RESUMEN[i]<-sum(is.na(GEO.DF[GEO.DF$ANO == anos[i],"RESUMEN_DEL_PROYECTO"]))
  DATOS_POR_ANO.DF$TOTAL_DE_CARACTERES[i]<- nchar(paste(GEO.DF[GEO.DF$ANO == anos[i],"RESUMEN_DEL_PROYECTO"], collapse = "")) / DATOS_POR_ANO.DF$CON_RESUMEN[i]
  DATOS_POR_ANO.DF$ARRAY[i]<-length(grep("array", GEO.DF[GEO.DF$ANO == anos[i], "SERIES_TYPE"]))
  DATOS_POR_ANO.DF$SEQ[i]<-length(grep("seq", GEO.DF[GEO.DF$ANO == anos[i], "SERIES_TYPE"]))
  DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_HUMANO[i]<-length(grep("Homo_sapiens", GEO.DF[GEO.DF$ANO == anos[i], "ORGANISMO"]))
  DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_RATON[i]<-length(grep("Mus_musculus", GEO.DF[GEO.DF$ANO == anos[i], "ORGANISMO"]))
}


####
#A CONTINUACIÓN LA GRÁFICA DE PUNTOS PARA COMPARAR EL NÚMERO PROMEDIO DE CARACTERES 
#EN UN RESUMEN CONTRA EL AÑO EN QUE SE PUBLICÓ

ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=TOTAL_DE_CARACTERES)) + 
  geom_point()

