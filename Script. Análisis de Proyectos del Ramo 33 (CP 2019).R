rm(list=ls())
setwd("~")

####################################################
#####     Análisis de Proyectos de Ramo 33     #####
#####                                          #####
#####        Alejandro Grimaldi Ferreira       #####
#####            Monitoreo, CONEVAL            #####
#####        Última versión: 21/09/2020        #####
####################################################





######## Requerimiento de Paquetes ########

library(pacman)
p_load(tidyverse, MASS, epiDisplay, ggplot2, scales, openxlsx, data.table, taRifx)





######## Creación de Direcciones ########

input <- "C:/Users/ale_g/Dropbox/CONEVAL/Ramo 33/Análisis sobre Distribución de Proyectos/Distribución de Proyectos/Informe Definitivo 2019/itanfp22c_202002"
output <- "C:/Users/ale_g/Dropbox/CONEVAL/Ramo 33/Análisis sobre Distribución de Proyectos/Distribución de Proyectos/Informe Definitivo 2019"
graf <- "C:/Users/ale_g/Dropbox/CONEVAL/Ramo 33/Análisis sobre Distribución de Proyectos/Distribución de Proyectos/Gráficas y Tablas"




######## Limpieza de la Base ########


# Vector con el nombre de las bases.
bases <- c("Aguascalientes.xlsx", "Baja California Sur.xlsx", "Baja California.xlsx",
           "Campeche.xlsx", "Chiapas.xlsx", "Chihuahua.xlsx", "Ciudad de México.xlsx",
           "Coahuila de Zaragoza.xlsx", "Colima.xlsx", "Durango.xlsx", "Guanajuato.xlsx",
           "Guerrero.xlsx", "Hidalgo.xlsx", "Jalisco.xlsx", "México.xlsx",
           "Michoacán de Ocampo.xlsx", "Morelos.xlsx", "Nayarit.xlsx", "Nuevo León.xlsx",
           "Oaxaca.xlsx", "Puebla.xlsx", "Querétaro.xlsx", "Quintana Roo.xlsx",
           "San Luis Potosí.xlsx", "Sinaloa.xlsx", "Sonora.xlsx", "Tabasco.xlsx",
           "Tamaulipas.xlsx", "Tlaxcala.xlsx", "Veracruz de Ignacio de la Llave.xlsx",
           "Yucatán.xlsx", "Zacatecas.xlsx")

# Cascarón de la base de datos.
ramo <- data.frame()

# Barra de progreso.
pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

# Loop para abrir y juntar las bases.
for(x in 1:length(bases)) {
  
  datos <- read.xlsx(paste(input, bases[x], sep = "/"), sheet=1)
  fuentes <- read.xlsx(paste(input, bases[x], sep = "/"), sheet=2)
  metas <- read.xlsx(paste(input, bases[x], sep = "/"), sheet=3)
  avances <- read.xlsx(paste(input, bases[x], sep = "/"), sheet=6)
  
  datos <- left_join(datos, fuentes, metas, by = "FOLIO")
  datos <- left_join(datos, avances, by = c("FOLIO", "CICLO"))
  
  datos <- subset(datos, select = -c(FUENTES_FINANCIAMIENTO, METAS,
                                       TIPO_GEOREFERENCIA, GEOREFERENCIAS,
                                       CONTRATOS, AVANCES_FISICOS,
                                       CARPETA_FOTOS, PROGRAMA.ESTATAL.O.MUNICIPAL))
  
  datos <- datos[which(datos$RAMO
                       =="33-Aportaciones Federales para Entidades Federativas y Municipios"),]
  
  datos <- datos[which(datos$PROGRAMA.PRESUPUESTARIO!="I005-FORTAMUN" &
                           datos$PROGRAMA.PRESUPUESTARIO!="I011-FASP" &
                           datos$PROGRAMA.PRESUPUESTARIO!="I012-FAFEF"),]
  
  ramo <- rbind(ramo, datos) # Metemos la información en el cascarón.
  rm(datos)
  rm(fuentes)
  rm(metas)
  rm(avances)
  
  setTxtProgressBar(pb,x) # Prendemos barra de progreso.
  
}

# Nombres de los fondos.
xtabs(~PROGRAMA.PRESUPUESTARIO, data=ramo)

ramo <- separate(ramo, PROGRAMA.PRESUPUESTARIO, into = c("CLAVE", "X"), sep = "-")
ramo$X <- NULL

ramo$FONDO[ramo$CLAVE %in% "I002"] <- "FASSA"
ramo$FONDO[ramo$CLAVE %in% "I003"] <- "FISE"
ramo$FONDO[ramo$CLAVE %in% "I004"] <- "FISM"
ramo$FONDO[ramo$CLAVE %in% "I006"] <- "FAM AS"
ramo$FONDO[ramo$CLAVE %in% "I007"] <- "FAM IEB"
ramo$FONDO[ramo$CLAVE %in% "I008"] <- "FAM IEMS"
ramo$FONDO[ramo$CLAVE %in% "I014"] <- "FONE OGC"
ramo$FONDO[ramo$CLAVE %in% "I016"] <- "FONE FC"

xtabs(~FONDO, data=ramo)

# Solo nos quedamos con recursos de 2019.
ramo <- ramo[(ramo$CICLO.DEL.RECURSO==2019),]

# Nombres de las variables en minúsculas.
names(ramo) <- tolower(names(ramo))

# Guardamos la base de datos.
write.csv(ramo, paste(output, "Proyectos Ramo 33 - Informe Definitivo 2019.csv", sep="/"))

# Identificar proyectos duplicados.
ramo_sd <- ramo %>%
  distinct(categoria, monto_global_aprobado, id_entidad_responsable,
           id_entidad_responsable, id_municipio_responsable, poblacion_beneficiada,
           fecha_inicio, fecha_termino, unidad.de.medida, meta.modificada, avance,
           clasificacion, institucion_ejecutora, fondo, modificado, .keep_all = TRUE)
# Hay XXX proyectos que comparten todas estas características: ¿son duplicados o por qué?

write.csv(ramo, paste(output, "Proyectos Ramo 33 - Informe Definitivo 2019 sin duplicados.csv", sep="/"))





######## ANÁLISIS DESCRIPTIVO ########

ramo <- read.csv(paste(output, "Proyectos Ramo 33 - Informe Definitivo 2019.csv", sep="/"))
ramo <- as.data.frame(ramo)



##### Limpieza: Proyectos cuyo presupuesto sea mayor al 50% del presupuesto del fondo.

ramo <- ramo[which(ramo$fondo!="FAM AS" &
                     ramo$fondo!="FASSA" &
                     ramo$fondo!="FONE OGC" &
                     ramo$fondo!="FONE FC"),]



##### Número de programas por responsable #####

ramo$responsable <- ifelse(ramo$municipio_responsable=="Gobierno de la Entidad", "Entidad Federativa", "Municipio")

gr <- ramo %>%
  count(fondo, responsable) %>%
  group_by(fondo) %>%
  mutate(porc = n/sum(n) * 100) %>%
  ggplot() + aes(x=reorder(fondo, -n), porc, fill=responsable, label=paste0(round(porc, 2), "%")) +
  geom_bar(stat = "identity") +
  geom_text(position=position_stack(vjust=0.5), size=6.5) +
  labs(x="Fondos de Aportaciones", y="Porcentaje de Proyectos", fill="Responsable") +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=c("royalblue1", "gray70"))
gr

ggsave(paste(graf, "Porcentaje de programas por fondo y responsable.png", sep="/"), plot=gr, width=12, height=6)



##### Porcentaje de avance por fondo y clasificación #####

ramo$porcentaje <- destring(ramo$porcentaje)

summary(ramo$porcentaje) # El máximo es 3,600,000%.

temp <- ramo[which(ramo$porcentaje<=200),] # Atípicos: avance > 200%.

temp <- temp %>%
  group_by(fondo) %>%
  mutate(porc_avance = mean(porcentaje, na.rm=T)) %>%
  ungroup()

temp <- temp %>%
  group_by(fondo, porc_avance) %>%
  tally()
temp$n <- NULL

gr <- ggplot(temp, aes(x=reorder(fondo, -porc_avance), y=porc_avance)) +
  geom_bar(stat="identity", fill="chartreuse4") +
  labs(x="Fondos de Aportaciones", y="Porcentaje de avance promedio") +
  geom_text(aes(label=paste0(round(porc_avance, 2), "%")), position=position_stack(1.05), size=6.5) +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Porcentaje de avance promedio por fondo.png", sep="/"), plot=gr, width=12, height=6)



##### Número de beneficiarios por fondo #####

# Tabla:

ramo$beneficiarios <- destring(ramo$beneficiarios)

temp <- ramo %>%
  group_by(fondo) %>%
  summarise_at(vars(beneficiarios), funs(sum(., na.rm=T)))
temp

# Gráfica de barras (conteo):

ramo <- ramo %>%
  group_by(fondo) %>%
  mutate(benef_total = sum(beneficiarios, na.rm=T)) %>%
  ungroup()

gr <- ggplot(ramo, aes(x=reorder(fondo, -benef_total), y=beneficiarios)) +
  geom_bar(stat="identity", fill="dodgerblue3") +
  labs(x="Fondos de Aportaciones", y="Total de Beneficiarios") +
  geom_text(data=temp %>% 
              group_by(fondo) %>% 
              summarise(beneficiarios=sum(beneficiarios, na.rm=TRUE)),
            aes(label=comma(beneficiarios), x=fondo, y=beneficiarios),
            inherit.aes=FALSE, vjust=-0.5, size=5) +
  scale_y_continuous(labels=comma) +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Beneficiarios de programas por fondo.png", sep="/"), plot=gr, width=12, height=6)



##### Número de programas por fondo y clasificación #####

xtabs(~fondo, data=ramo)

# Tabla:

temp <- ramo %>%
  group_by(fondo, clasificacion) %>%
  tally()

# Gráfica de barras (conteo):

ramo <- ramo %>%
  group_by(fondo) %>%
  add_tally() %>%
  ungroup()
ramo$n_prog <- ramo$n
ramo$n <- NULL

gr <- ggplot(ramo, aes(x=reorder(fondo, -n_prog))) +
  geom_bar(aes(fill=clasificacion)) +
  geom_text(aes(label=comma(..count..)), stat="count", position=position_stack(1.05), size=6.5) +
  labs(x="Fondos de Aportaciones", y="Número de Proyectos", fill="Clasificación") +
  scale_y_continuous(labels=comma) +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Número de programas por fondo y clasificación.png", sep="/"), plot=gr, width=12, height=6)

# Gráfica de barras (Porcentajes):

gr <- ramo %>%
  count(fondo, clasificacion) %>%
  group_by(fondo) %>%
  mutate(porc = n/sum(n) * 100) %>%
  ggplot() + aes(x=reorder(fondo, -n), porc, fill=clasificacion, label=ifelse(porc>2, paste0(round(porc, 0), "%"), "")) +
  geom_bar(stat = "identity") +
  geom_text(position=position_stack(vjust=0.5), size=6.5) +
  labs(x="Fondos de Aportaciones", y="Porcentaje de Proyectos", fill="Clasificación") +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Porcentaje de programas por fondo y clasificación.png", sep="/"), plot=gr, width=12, height=6)



##### Presupuesto por fondo y clasificación #####

# Tabla:

temp <- ramo %>%
  group_by(fondo) %>%
  summarise_at(vars(modificado), funs(sum(., na.rm=T)))
temp

# Gráfica de barras (conteo):

ramo <- ramo %>%
  group_by(fondo) %>%
  mutate(monto = sum(modificado, na.rm=T)) %>%
  ungroup()
ramo$modificado_mdp <- ramo$modificado / 1000000

temp <- ramo[which(ramo$fondo!="FONE FC"),] # El proyecto de FONE FC no tiene presupuesto modificado.

gr <- ggplot(temp, aes(x=reorder(fondo, -monto), y=modificado_mdp)) +
  geom_bar(aes(fill=clasificacion), stat="identity") +
  labs(x="Fondos de Aportaciones", y="Monto Modificado (millones de pesos)", fill="Clasificación") +
  geom_text(data=temp %>% 
              group_by(fondo) %>% 
              summarise(modificado_mdp=sum(modificado_mdp, na.rm=TRUE)),
            aes(label=dollar(modificado_mdp), x=fondo, y=modificado_mdp),
            inherit.aes=FALSE, vjust=-0.5, size=5) +
  scale_y_continuous(labels=dollar) +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Monto de programas por fondo y clasificación.png", sep="/"), plot=gr, width=12, height=6)

# Gráfica de barras (Porcentajes):

ramo <- ramo %>%
  group_by(fondo, clasificacion) %>%
  mutate(monto_clas = sum(modificado, na.rm=T)) %>%
  ungroup()

temp <- ramo %>%
  group_by(fondo, clasificacion, monto_clas) %>%
  tally()
temp$n <- NULL

temp <- temp %>%
  group_by(fondo) %>%
  mutate(monto = sum(monto_clas, na.rm=T)) %>%
  ungroup()

temp$porc <- temp$monto_clas / temp$monto * 100

temp <- temp[which(temp$fondo!="FONE FC"),] # El proyecto de FONE FC no tiene presupuesto modificado.

gr <- temp %>%
  ggplot() + aes(x=reorder(fondo, -monto), porc, fill=clasificacion, label=ifelse(porc>2, paste0(round(porc, 0), "%"), "")) +
  geom_bar(stat = "identity") +
  geom_text(position=position_stack(vjust=0.5), size=6.5) +
  labs(x="Fondos de Aportaciones", y="Porcentaje del Presupuesto", fill="Clasificación") +
  theme(text = element_text(size=20))
gr

ggsave(paste(graf, "Porcentaje del presupuesto por fondo y clasificación.png", sep="/"), plot=gr, width=12, height=6)








