library(ggplot2)
library(dplyr)

# Importación de datos
df <- read.csv(file = 'data/inter.csv')
df$Date <- as.Date(df$Date)

# Progreso de positivos una semana después en todos los países.
# Solo unos pocos muestran progresión, y normalmente es tras empezar marzo
p <- ggplot(df, aes(x=Date, y=ConfirmedCasesWeekLater)) +
  geom_line() + 
  xlab("Fechas") + ylab("Casos confimados una semana después") +
  facet_wrap(~Zone)+
  labs(title='Casos confirmados por país una semana después en febrero y marzo')
p 

# Si tenemos pocos casos no podemos apreciarlos. Cambiamos a escala logarítmica.
p <- ggplot(df, aes(x=Date, y=ConfirmedCasesWeekLater)) +
  geom_line() + 
  xlab("Fechas") + ylab("Casos confimados una semana después") +
  scale_y_log10() + facet_wrap(~Zone)+
  geom_vline(xintercept=as.Date("2020-02-15"), linetype="dashed", color = "red")
  # labs(title='Casos confirmados por país una semana después (log)')
p 
ggsave("evolution.png", p, "png", path = "img/discusion/", scale = 3)
# La mayoría de países experimentan sus primeros casos una semana después de finales
# de febrero.

# Ahora, añadimos la estimación de casos importados para ver si existe alguna relación.
confirmedColor = 'blue'
estimationColor = 'black'
p <- ggplot(df, aes(x=Date)) +
  geom_line(aes(y=ConfirmedCasesWeekLater),color=confirmedColor) + 
  geom_line(aes(y=CasesEstimation),color=estimationColor) + 
  scale_y_log10(
    name = "Casos confimados una semana después",
    
    sec.axis = sec_axis(~.*0.01, name="Estimación de casos importados")
  ) + facet_wrap(~Zone) +
  theme(
    axis.title.y = element_text(color = confirmedColor, size=13),
    axis.title.y.right = element_text(color = estimationColor, size=13)
  )+
  geom_vline(xintercept=as.Date("2020-03-15"), linetype="dashed", color = "red")
  # labs(title='Comparativa de casos confirmados e importados en vuelos')
p 

ggsave("confEstimados.png", p, "png", path = "img/discusion/", scale = 3)
# Como se puede ver, hay un aumento conforme aumentan los casos (recordemos que
# la escala es logarítmica), pero a partir de la segunda quincena empiezan a bajar
# los casos importados aunque siguieran subiendo los casos (se empezaron a tomar políticas)

# Correlación entre Casos Confirmados una semana después y Casos estimados por vuelos
correlate <- df %>%
  group_by(Date) %>% 
  summarise(pearson = cor(log10(ConfirmedCasesWeekLater + 1), CasesEstimation))

p <- ggplot(correlate, aes(x=Date, y=pearson)) +
  geom_line()+
  labs(y='Correlación')
  # labs(title='Correlación entre casos confirmados a la semana y casos importados')
p
ggsave("corr2.png", p, "png", path = "img/discusion/", scale = 1)
# Tras estas gráficas, se considera que lo mejor será considerar los resultados
# desde el 15/02 hasta el 15/03
df <- df[df$Date >= as.Date("2020-02-15") & df$Date <= as.Date("2020-03-15"),]

# Qué relación existe entre el riesgo importado y los positivos estimados?
# El valor de riesgo importado siempre es algo superior. Veamos la distribución
# de la diferencia de estas dos variables
df$diff <- df$ImportedRisk - df$CasesEstimation
p <- ggplot(df, aes(diff)) + 
  geom_histogram(bins=150, color="black", fill="white")+
  geom_density(alpha=.6, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(diff)),
             color="blue", linetype="dashed", size=1)+
  labs(title='Densidad de la diferencia entre casos importados y riesgo importado',
       x='ImportedRisk - CasesEstimation',
       y='Frecuencia')
p

# Haciendo zoom en la zona más interesante...
p <- ggplot(df, aes(diff)) + 
  geom_histogram(bins=80, color="black", fill="white")+
  geom_density(alpha=.6, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(diff)),
             color="blue", linetype="dashed", size=1)+
  xlim(-0.01,0.02)+
  # labs(title='Densidad de la diferencia entre casos importados y riesgo importado',
  #      x='ImportedRisk - CasesEstimation',
  #      y='Frecuencia')
  labs(x='ImportedRisk - CasesEstimation',
       y='Frecuencia')
p
ggsave("diff.png", p, "png", path = "img/discusion/", scale = 1)

# Proporción de valores entre 0 y 0.01
nrow(df[df$diff < -0.0001,])/nrow(df)
mean(df$diff)
sd(df$diff)

# t-test correspondiente
t.test(df$ImportedRisk, df$CasesEstimation)
