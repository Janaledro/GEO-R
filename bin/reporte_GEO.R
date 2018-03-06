#Este script va a analizar una tabla obtenida a partir de los metadatos del GEO,
#generará datos descriptivos de la tabla original.
#Después la tabla se filtrará con criterios de búsqueda proporcionados por el usuario y se
#volverán a hacer gráficos descriptivos.

#A continuación se cargará la librería 'ggplot' para hacer gráficos
library("ggplot2")
library("reshape2")
library("alluvial")
library("dplyr")
#A continuación se leerán los datos
GEO<-read.csv(file="data/GEO_tablet.tsv", header = T, sep = "\t", 
              na.strings = "", stringsAsFactors = T) 
#####################################################################
#Aquí se hará con 200 líneas para facilitar las pruebas 
#Este bloque se tiene que comentar cuando se obtengan los datos completos
#GEO<-droplevels(GEO[1:200,])

####
#EL SIGUIENTE BLOQUE SE ENCARGARÁ DE ARREGLAR LAS LÍNEAS DESFASADAS DE LA TABLA FINAL.

GEO.DF<-GEO[grepl(GEO$FECHA_DE_PUBLICACION, pattern = "Public_on_"),]

#####################################################################
#Este bloque es sólo para hacer pruebas con el data frame original, aquí se trabajará con la columna
#"ORGANISMO" de tal manera en que se pueda ejecutar posteriormente el código para gráficar 


SPPs <- as.character(GEO.DF$ORGANISMO)
SPPstr<-unlist(strsplit(SPPs, split = ","))
ESPECIES<-grep("!Sample_", SPPstr, value = T, invert = T)
ESPECIES.DF<- data.frame(ESPECIES, stringsAsFactors = T)
ESPECIES.DF<-droplevels(ESPECIES.DF)

my.summary<-summary.data.frame(ESPECIES.DF, maxsum = 7)
#ESPECIES.DF<-data.frame(ORGANISMO=, NUMERO_DE_ENTRADAS_EN_LA_TABLA=my.summary)
ESPECIES.DF<-data.frame(
  ORGANISMO=c("Homo_sapiens", "Mus_musculus", "Arabidopsis_thaliana",
              "Drosophila_melanogaster", "Rattus_novergicus", "Saccharomyces_cerevisiae", 
              "Other"),
  NUMERO_DE_ENTRADAS_EN_LA_TABLA=c(21905, 14898, 1876, 1792, 1506, 1289, 12609),
  stringsAsFactors = T)

bp<- ggplot(ESPECIES.DF, aes(x="", y=NUMERO_DE_ENTRADAS_EN_LA_TABLA, fill= ORGANISMO))+
  geom_bar(width = 1, stat = "identity") 
bp + scale_fill_brewer(palette="Dark2")

pie <- bp + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Dark2")

pdf("results/piechart_especies_tabla_original.pdf")
pie
bp

dev.off()

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

my.summary <- summary(GEO$ORGANISMO, maxsum = 7)
NUMEROS_POR_ESPECIE.DF<-data.frame(ORGANISMO=names(my.summary), NUMERO_DE_ENTRADAS_EN_LA_TABLA=my.summary)

####
#A continuación, el piechart
# Barplot
bp<- ggplot(NUMEROS_POR_ESPECIE.DF, aes(x="", y=NUMERO_DE_ENTRADAS_EN_LA_TABLA,
                                        fill= ORGANISMO))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


####
#A continuación se realizarán gráficos de comparación a lo largo del tiempo
GEO.DF$ANO <-0
GEO.DF$MES <-0
GEO.DF$DIA <-0


GEO.DF$FECHA_DE_PUBLICACION <- as.character(GEO.DF$FECHA_DE_PUBLICACION)
for(i in 1:nrow(GEO.DF)){
message(paste0("Operando sobre la línea ", i))
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
gp_numero_de_entradas<-ggplot(NUMEROS_POR_ANO, aes(x=ANO, y=NUMERO_DE_ENTRADAS)) + 
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
  message(paste0("Creando el data frame en la línea ", i))
  DATOS_POR_ANO.DF$ANO[i] <- anos[i]
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

gp_tdc<-ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=TOTAL_DE_CARACTERES)) + 
  geom_point()

#GRÁFICA DE ESTUDIOS CON MICROARREGOS VS AÑOS
gp_microarreglos_vs_años<-ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=ARRAY)) + 
  geom_point()

#GRÁFICA DE ESTUDIOS CON SECUENCIACIÓN VS AÑOS
gp_seq_vs_anos<-ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=SEQ)) + 
  geom_point()
#GRÁFICA DE ESTUDIOS EN HUMANO VS AÑOS
gp_hs_vs_anos<-ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=NUMERO_DE_ESTUDIOS_EN_HUMANO)) + 
  geom_point()

#GRÁFICA DE ESTUDIOS EN RATÓN VS AÑOS
gp_mm_vs_anos<-ggplot(DATOS_POR_ANO.DF, aes(x=ANO, y=NUMERO_DE_ESTUDIOS_EN_RATON)) + 
  geom_point()

#GRÁFICA DE ESTUDIOS EN HUMANO VS RATÓN A TRAVÉS DEL TIEMPO. PRIMERO HAY QUE ORGANIZAR 
#LOS DATOS PARA COMPARARLOS EN PARALELO
FRAME.DF<-data.frame(ANO=DATOS_POR_ANO.DF$ANO, 
                     NUMERO_DE_ESTUDIOS_EN_HUMANO=DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_HUMANO, 
                     NUMERO_DE_ESTUDIOS_EN_RATON=DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_RATON)

data.m<-melt(FRAME.DF, id.vars= "ANO")

#GRÁFICA QUE COMPARA ESTUDIOS REALIZADOS EN HUMANOS VS RATÓN CON BARRAS
bp_hs_vs_mm<-ggplot(data.m, aes(ANO, value))+ 
  geom_bar(aes( fill= variable), position= "dodge", stat="identity") + 
  scale_fill_brewer(palette="Dark2")

#GRÁFICA QUE COMPARA ESTUDIOS REALIZADOS EN HUMANOS VS RATÓN CON PUNTOS
gp_hs_vs_mm<-ggplot(data.m, aes(x=ANO, y=value)) + 
  geom_point(aes(fill=variable, color=variable)) + 
  scale_color_brewer(palette="Set1")
  
#REALIZACIÓN DE ALLUVIAL PLOT PARA SUMARIZAR LA INFORMACIÓN PREVIA
FRAME.DF<-data.frame(ANO=DATOS_POR_ANO.DF$ANO, 
                     NUMERO_DE_ESTUDIOS_EN_HUMANO=DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_HUMANO, 
                     NUMERO_DE_ESTUDIOS_EN_RATON=DATOS_POR_ANO.DF$NUMERO_DE_ESTUDIOS_EN_RATON,
                     ARRAY=DATOS_POR_ANO.DF$ARRAY,
                     SEQ=DATOS_POR_ANO.DF$SEQ, stringsAsFactors = T)
data.m<-melt(FRAME.DF, id.vars= "ANO")
alluvial1<-alluvial(data.m, freq = data.m$value)

ARRAYvsSEQ.DF<-data.frame(ANO=DATOS_POR_ANO.DF$ANO,
                        ARRAY=DATOS_POR_ANO.DF$ARRAY,
                        SEQ=DATOS_POR_ANO.DF$SEQ,
                        stringsAsFactors = T)
summary.data.frame(ARRAYvsSEQ.DF)
arrayvsseq.melt<-melt(ARRAYvsSEQ.DF, id.vars = "ANO")
arrayvsseq<-subset(arrayvsseq.melt, select = -ANO)
alluvial2<-alluvial(arrayvsseq, freq = arrayvsseq$value)

#GRÁFICA QUE COMPARA ESTUDIOS REALIZADOS EN MICROARREGLOS VS SECUENCIACIÓN
ggplot(arrayvsseq.melt, aes(ANO, value))+ 
  geom_bar(aes( fill= variable), position= "dodge", stat="identity") + 
  scale_fill_brewer(palette="Dark2")

#GRÁFICA QUE COMPARA ESTUDIOS REALIZADOS EN MICROARREGLOS VS SECUENCIACIÓN CON PUNTOS
ggplot(arrayvsseq.melt, aes(x=ANO, y=value)) + 
  geom_point(aes(fill=variable, color=variable)) + scale_color_brewer(palette="Set1")

###
#A CONTINUACIÓN SE GENERARÁ UN LOOP FOR PARA EXTRAER LOS ÍNDICES DONDE SE ENCUENTRAN
#MÚLTIPLES TÉRMINOS CON LA HERRAMIENTA "grepl"
#A CONTINUACIÓN CREAMOS UN DATA FRAME DE PRUEBA
PARA_PROBAR_GREPS.DF<- GEO.DF[1:100,]
#A CONTINUACIÓN CREAMOS EL VACTOR CON LOS TÉRMINOS DE BÚSQUEDA
palabras<-c("models", "memory","DNA")
####
#CREANDO UN VACTOR VACÍO DONDE SE CONCATENARÁN TODOS LOS VECTORES DE CADA PALABRA
VECTOR_FINAL_DE_GREPS<-NA
#A CONTINUACIÓN CREAMOS EL LOOP FOR PARA BUSCAR UN VECTOR DE MATCHES DE MANERA
#ITERATIVA
for(i in 1:length(palabras)){
  message(paste0("Estoy buscando la palabra ", palabras[i]))
  indices<-grep(pattern = palabras[i], PARA_PROBAR_GREPS.DF$RESUMEN_DEL_PROYECTO, 
        ignore.case = T)
  #aquí se tiene que concatenar el vector "indices" con el vector final de greps
  VECTOR_FINAL_DE_GREPS<-as.vector(rbind(VECTOR_FINAL_DE_GREPS, indices))
  #c( matrix(c(VECTOR_FINAL_DE_GREPS,indices), nrow=2, byrow=TRUE) ) 
}
#VAMOS A ELIMINAR LOS VALORES NA
VECTOR_FINAL_DE_GREPS <- VECTOR_FINAL_DE_GREPS[!is.na(VECTOR_FINAL_DE_GREPS)]

#Y AHORA VAMOS A ELIMINAR ELEMENTOS REPETIDOS
VECTOR_FINAL_DE_GREPS<-unique(VECTOR_FINAL_DE_GREPS)

#AHORA VAMOS A EXTRAER LAS FILAS QUE NOS INTERESAN
GEO_FILTRADO<-PARA_PROBAR_GREPS.DF[VECTOR_FINAL_DE_GREPS,]
GEO_FILTRADO<-PARA_PROBAR_GREPS.DF[VECTOR_FINAL_DE_GREPS,]





####
#BLOQUE FINAL
#EN ESTE BLOQUE SE AGRUPAN LAS GRÁFIAS GENERADAS HASTA AHORA PARA EXPORTARLAS EN PDF
pdf("results/graficas_de_tabla_original.pdf")
pie
bp
gp_hs_vs_anos
gp_mm_vs_anos
gp_microarreglos_vs_años
gp_seq_vs_anos
gp_tdc
bp_hs_vs_mm
gp_hs_vs_mm
alluvial1<-alluvial(data.m, freq = data.m$value)
alluvial2<-alluvial(arrayvsseq, freq = arrayvsseq$value)
dev.off()

