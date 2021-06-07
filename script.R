# -------------------------------------------- #
# Analisis de resultados - Proyecto Pirotecnia #
#           Tesis de Guillermo Bori            #
# -------------------------------------------- #

rm(list=ls())
packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown", 
              "readr", "caTools", "bitops")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(tidyverse)
library(ggpubr)
library(jtools)
library(emmeans)
library(cowplot)
library(dplyr)
library(readr)
source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")

# 1) Importacion de datos-----
tabla.datos = read.csv("datos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
tabla.datos = tabla.datos %>% filter( Fecha != "N" | Punto != 5)
tabla.datos = tabla.datos %>% filter( Fecha != "AN" | Punto != 4)
tabla.datos$Fecha = factor(tabla.datos$Fecha, levels= c("N","AN"), labels=c("Navidad","Año Nuevo"))
tabla.datos$Tiempo = factor(tabla.datos$Tiempo, levels= c("A","B","C"), labels=c("60 min","30 min","15 min"))
tabla.datos$Condicion = factor(tabla.datos$Condicion, levels= c("CON","SIN"), labels=c("Con pirotecnia","Sin pirotecnia"))
# 2) Tablas individuales y promedios ----
# Transformablos la tabla a tibble
tabla.datos = tibble(tabla.datos) 
tabla.datos %>% filter(!is.na(tabla.datos))
# Calculo de tabla promedio
tabla.promedios <- tabla.datos %>% 
  group_by(Condicion, Tiempo, Fecha) %>%
  summarise(mLeqAS = mean(LeqAS, na.rm = TRUE),
            mLmaxAS = mean(LmaxAS, na.rm = TRUE),
            mLminAS = mean(LminAS, na.rm = TRUE),
            mL1 = mean(L1, na.rm = TRUE),
            mL10 = mean(L10, na.rm = TRUE),
            mL50 = mean(L50, na.rm = TRUE),
            mL90 = mean(L90, na.rm = TRUE),
            mLpeak = mean(Lpeak, na.rm = TRUE),
            sdLeqAS = sd(LeqAS, na.rm = TRUE),
            sdLmaxAS = sd(LmaxAS, na.rm = TRUE),
            sdLminAS = sd(LminAS, na.rm = TRUE),
            sdL1 = sd(L1, na.rm = TRUE),
            sdL10 = sd(L10, na.rm = TRUE),
            sdL50 = sd(L50, na.rm = TRUE),
            sdL90 = sd(L90, na.rm = TRUE),
            sdLpeak = sd(Lpeak, na.rm = TRUE)) %>%
  ungroup()

# 3) Analisis de normalidad ------
#Shapiro test
Fecha = replicate(96,0)
tabla.normalidad = as.data.frame(Fecha)
factor_condicion = factor(tabla.datos$Condicion)
niveles_condicion = levels(factor_condicion)
factor_tiempo = factor(tabla.datos$Tiempo)
niveles_tiempo = levels(factor_tiempo)
factor_fecha = factor(tabla.datos$Fecha)
niveles_fecha = levels(factor_fecha)
i= 1
for (f in 1:length(niveles_fecha)) {
  for(c in 1:length(niveles_condicion)) {
    for(t in 1:length(niveles_tiempo)) {
        for(d in 5:length(names(tabla.datos))) {
      cat("Parametros:"," ",niveles_fecha[f]," ",niveles_condicion[c]," ", niveles_tiempo[t]," - ",names(tabla.datos)[d], '\':\n', sep = '')
      aux = tabla.datos %>% filter(Fecha == niveles_fecha[f] & 
                                   Condicion == niveles_condicion[c] & 
                                   Tiempo == niveles_tiempo[t])%>% pull(names(tabla.datos)[d])
      shapiro_aux =shapiro.test(aux)
      print(shapiro_aux)
      tabla.normalidad$Fecha[i] = niveles_fecha[f]
      tabla.normalidad$Condicion[i] = niveles_condicion[c]
      tabla.normalidad$Tiempo[i] = niveles_tiempo[t]
      tabla.normalidad$Parametro[i] = names(tabla.datos)[d]
      tabla.normalidad$W[i] = shapiro_aux$statistic
      tabla.normalidad$pvalue[i] = shapiro_aux$p.value
      i = i+1
      }
    }
  }
}
aux = tabla.normalidad$pvalue < 0.05
print(tabla.normalidad[aux,])
# Todos normales

# 4) Analisis LeqAS ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.LeqAS <- lm(LeqAS ~ Condicion*Tiempo*Fecha, 
              data = tabla.datos)
summary(m.LeqAS)
summ(m.LeqAS)
anova(m.LeqAS)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.LeqAS <- lm(LeqAS ~ Condicion*Tiempo, 
              data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.LeqAS)
summ(m.LeqAS)
anova(m.LeqAS)
m.emm.LeqAS <- emmeans(m.LeqAS, "Condicion", "Tiempo")
m.emm.LeqAS

#Navidad
m.LeqAS <- lm(LeqAS ~ Condicion*Tiempo, 
                 data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.LeqAS)
summ(m.LeqAS)
anova(m.LeqAS)
m.emm.LeqAS <- emmeans(m.LeqAS, "Condicion", "Tiempo")
m.emm.LeqAS
#Grafico nuevo LeqAS
fig.LeqAS <- ggplot(tabla.datos, aes(x = Tiempo, y = LeqAS, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = LeqAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = LeqAS, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "Leq Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.LeqAS
ggsave("Figuras/LeqAS.png", plot=fig.LeqAS, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 


# 5) Analisis Lmax ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.LmaxAS <- lm(LmaxAS ~ Condicion*Tiempo*Fecha, 
              data = tabla.datos)
summary(m.LmaxAS)
summ(m.LmaxAS)
anova(m.LmaxAS)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.LmaxAS <- lm(LmaxAS ~ Condicion*Tiempo, 
              data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.LmaxAS)
summ(m.LmaxAS)
anova(m.LmaxAS)
m.emm.LmaxAS <- emmeans(m.LmaxAS, "Condicion", "Tiempo")
m.emm.LmaxAS

#Navidad
m.LmaxAS <- lm(LmaxAS ~ Condicion*Tiempo, 
              data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.LmaxAS)
summ(m.LmaxAS)
anova(m.LmaxAS)
m.emm.LmaxAS <- emmeans(m.LmaxAS, "Condicion", "Tiempo")
m.emm.LmaxAS

#Grafico LmaxAS
fig.LmaxAS <- ggplot(tabla.datos, aes(x = Tiempo, y = LmaxAS, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = LmaxAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = LmaxAS, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "Lmax Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.LmaxAS
ggsave("Figuras/LmaxAS.png", plot=fig.LmaxAS, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 

# 6) Analisis Lmin ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.LminAS <- lm(LminAS ~ Condicion*Tiempo*Fecha, 
               data = tabla.datos)
summary(m.LminAS)
summ(m.LminAS)
anova(m.LminAS)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.LminAS <- lm(LminAS ~ Condicion*Tiempo, 
               data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.LminAS)
summ(m.LminAS)
anova(m.LminAS)
m.emm.LminAS <- emmeans(m.LminAS, "Condicion", "Tiempo")
m.emm.LminAS

#Navidad
m.LminAS <- lm(LminAS ~ Condicion*Tiempo, 
               data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.LminAS)
summ(m.LminAS)
anova(m.LminAS)
m.emm.LminAS <- emmeans(m.LminAS, "Condicion", "Tiempo")
m.emm.LminAS

#Grafico LminAS
fig.LminAS <- ggplot(tabla.datos, aes(x = Tiempo, y = LminAS, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = LminAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = LminAS, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "Lmin Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.LminAS
ggsave("Figuras/LminAS.png", plot=fig.LminAS, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 


# 7) Analisis L1 ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.L1<- lm(L1 ~ Condicion*Tiempo*Fecha, 
               data = tabla.datos)
summary(m.L1)
summ(m.L1)
anova(m.L1)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.L1 <- lm(L1 ~ Condicion*Tiempo, 
               data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.L1)
summ(m.L1)
anova(m.L1)
m.emm.L1 <- emmeans(m.L1, "Condicion", "Tiempo")
m.emm.L1

#Navidad
m.L1 <- lm(L1 ~ Condicion*Tiempo, 
               data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.L1)
summ(m.L1)
anova(m.L1)
m.emm.L1 <- emmeans(m.L1, "Condicion", "Tiempo")
m.emm.L1

#Grafico L1
fig.L1 <- ggplot(tabla.datos, aes(x = Tiempo, y = L1, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = L1, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = L1, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "L1 Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.L1
ggsave("Figuras/L1.png", plot=fig.L1, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 



# 8) Analisis L10 ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.L10<- lm(L10 ~ Condicion*Tiempo*Fecha, 
          data = tabla.datos)
summary(m.L10)
summ(m.L10)
anova(m.L10)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.L10 <- lm(L10 ~ Condicion*Tiempo, 
           data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.L10)
summ(m.L10)
anova(m.L10)
m.emm.L10 <- emmeans(m.L10, "Condicion", "Tiempo")
m.emm.L10

#Navidad
m.L10 <- lm(L10 ~ Condicion*Tiempo, 
           data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.L10)
summ(m.L10)
anova(m.L10)
m.emm.L10 <- emmeans(m.L10, "Condicion", "Tiempo")
m.emm.L10

#Grafico L10
fig.L10 <- ggplot(tabla.datos, aes(x = Tiempo, y = L10, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = L10, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = L10, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "L10 Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.L10
ggsave("Figuras/L10.png", plot=fig.L10, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 

# 9) Analisis L50 ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.L50<- lm(L50 ~ Condicion*Tiempo*Fecha, 
           data = tabla.datos)
summary(m.L50)
summ(m.L50)
anova(m.L50)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.L50 <- lm(L50 ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.L50)
summ(m.L50)
anova(m.L50)
m.emm.L50 <- emmeans(m.L50, "Condicion", "Tiempo")
m.emm.L50

#Navidad
m.L50 <- lm(L50 ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.L50)
summ(m.L50)
anova(m.L50)
m.emm.L50 <- emmeans(m.L50, "Condicion", "Tiempo")
m.emm.L50

#Grafico L50
fig.L50 <- ggplot(tabla.datos, aes(x = Tiempo, y = L50, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = L50, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = L50, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "L50 Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.L50
ggsave("Figuras/L50.png", plot=fig.L50, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 



# 10) Analisis L90 ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.L90<- lm(L90 ~ Condicion*Tiempo*Fecha, 
           data = tabla.datos)
summary(m.L90)
summ(m.L90)
anova(m.L90)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.L90 <- lm(L90 ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.L90)
summ(m.L90)
anova(m.L90)
m.emm.L90 <- emmeans(m.L90, "Condicion", "Tiempo")
m.emm.L90

#Navidad
m.L90 <- lm(L90 ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.L90)
summ(m.L90)
anova(m.L90)
m.emm.L90 <- emmeans(m.L90, "Condicion", "Tiempo")
m.emm.L90

#Grafico L90
fig.L90 <- ggplot(tabla.datos, aes(x = Tiempo, y = L90, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = L90, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = L90, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "L90 Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.L90
ggsave("Figuras/L90.png", plot=fig.L90, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 


# 11) Analisis Lpeak ----
# Ajuste lineal de efectos fixtos con Condicion,Tiempo y Fecha 
# como efectos fijos e interaccion como efecto aleatorio
m.Lpeak<- lm(Lpeak ~ Condicion*Tiempo*Fecha, 
           data = tabla.datos)
summary(m.Lpeak)
summ(m.Lpeak)
anova(m.Lpeak)

# Ajuste lineal de efectos fixtos con Condicion y Tiempo 
#como efectos fijos e interaccion como efecto aleatorio
#Año nuevo
m.Lpeak <- lm(Lpeak ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Año Nuevo"))
summary(m.Lpeak)
summ(m.Lpeak)
anova(m.Lpeak)
m.emm.Lpeak <- emmeans(m.Lpeak, "Condicion", "Tiempo")
m.emm.Lpeak

#Navidad
m.Lpeak <- lm(Lpeak ~ Condicion*Tiempo, 
            data = filter(tabla.datos,Fecha =="Navidad"))
summary(m.Lpeak)
summ(m.Lpeak)
anova(m.Lpeak)
m.emm.Lpeak <- emmeans(m.Lpeak, "Condicion", "Tiempo")
m.emm.Lpeak

#Grafico Lpeak
fig.Lpeak <- ggplot(tabla.datos, aes(x = Tiempo, y = Lpeak, fill = Condicion)) +
  geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Tiempo)-.15, y = Lpeak, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = Tiempo, y = Lpeak, fill = Condicion),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(Fecha~.) +
  labs(x = "Intervalo de tiempo", 
       y = "Lpeak Slow [dBA]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
fig.Lpeak
ggsave("Figuras/Lpeak.png", plot=fig.Lpeak, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 



# Comparacion pareada (Paired comparisons) -----
# ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 
#Año nuevo
idx = tabla.datos$Fecha == "Año Nuevo"
tabla.datos.AN = tabla.datos[idx,]

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="A")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="B")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="C")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS, 
       paired = FALSE)
#Navidad
idx = tabla.datos$Fecha == "Navidad"
tabla.datos.AN = tabla.datos[idx,]

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="A")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="B")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="CON" & Tiempo =="C")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="A")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS, 
       paired = FALSE)

t.test(filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="B")$LeqAS,
       filter(tabla.datos.AN, 
              Condicion =="SIN" & Tiempo =="C")$LeqAS, 
       paired = FALSE)



#### VIEJO #####
#Grafico LeqAS
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
fig.LeqAS = tabla.datos %>% 
  # filter(Fecha == "Navidad") %>%
  ggplot(aes(x = Condicion, 
             y = LeqAS, 
             colour = Condicion, 
             fill = Condicion)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               alpha = 0.7,
               size=2, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 4,
               position = position_dodge(width = 1)) +
  labs(x = "Condicion CON/SIN Pirotecnia", 
       y = "Leq Slow [dBA]") +
  ylim(35,90) +
  facet_grid(Fecha~Tiempo) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")+
  annotate("text", x = 1.5, y = 85,  label = "***", size = 7) +
  annotate("segment", x = 1, xend = 2, y = 84, yend = 84, colour = "black", size=.8, alpha=1,)
fig.LeqAS
ggsave("Figuras/LeqAS.png", plot=fig.LeqAS, width = 16, height = 16, units = "cm", dpi=600, limitsize=FALSE)  