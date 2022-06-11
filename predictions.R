library(ggplot2)
library(dplyr)
library(sandwich)
library(msm)
library(useful)
library(Metrics)
library(MASS)
library(knitr)
library(mosaic)
library(xtable)
library(pscl) 
library(multcomp)
library(pander)

# Importación de datos
df <- read.csv(file = 'data/inter.csv')
df$Date <- as.Date(df$Date)

# Cortamos a un cierto número de fechas
min.date = as.Date("2020-02-15")
max.date = as.Date("2020-03-15")
df <- df[df$Date >= min.date & df$Date <= max.date,]

# De momento, quitamos Francia e Italia
df <- df[df$Zone != "France" & df$Zone != 'Italy',]

# Añadimos columnas auxiliares
df$logX <- log10(df$CasesEstimation + 1)
df$logY <- log10(df$ConfirmedCasesWeekLater + 1) + 1
df$EstimationCubeRoot <- df$CasesEstimation^(1/3)
df$ImportedCubeRoot <- df$ImportedRisk^(1/3)
df$ConfirmedRoot <- df$ConfirmedCases^(1/5)

# División train/test
prop = 0.9
limite = (max.date - min.date)*prop + min.date
df.train <- df[df$Date < limite,]
df.test <- df[df$Date >= limite,]

# Antes de empezar con regresiones, vamos a crear algunas funciones básicas
plot.df <- function(df, country = NULL, log.plot = TRUE, names=c('prediction')){
  if(!is.null(country)){
    df <- df[df$Zone == country,]
  }
  confirmedColor = 'blue'
  estimationColor = 'red'
  p <- ggplot(df, aes(x=Date)) +
    geom_line(aes(y=ConfirmedCasesWeekLater,colour="Confirmed")) +
    facet_wrap(~Zone)
  if(log.plot){
    p <- p + scale_y_log10(limits=c(1,NA)) 
  }
  for(name in names){
    p <- p + geom_line(aes_string(y=name,colour=shQuote(name)))
  }
  p 
}

####################################################################
# Análisis exploratorio inicial
####################################################################

# En primer lugar: sigue nuestra v.a. una distribución de Poisson?
p <- ggplot(df, aes(ConfirmedCasesWeekLater)) + 
  geom_histogram(bins=150, color="black", fill="white")+
  geom_density(alpha=.6, fill="#FF6666") +
  labs(y='Frecuencia')
p

# Separando por países
p <- ggplot(df, aes(ConfirmedCasesWeekLater)) + 
  geom_histogram(bins=150, color="black", fill="white")+
  geom_density(alpha=.6, fill="#FF6666") +
  labs(title="Histograma de casos por país",y='Frecuencia') +
  facet_wrap(~Zone)
p

# Separando por fechas
p <- ggplot(df, aes(ConfirmedCasesWeekLater)) + 
  geom_histogram(bins=150, color="black", fill="white")+
  geom_density(alpha=.6, fill="#FF6666") +
  labs(title=paste('Histograma de casos por fechas'),y='Frecuencia') +
  facet_wrap(~Date)
p

# Medias y varianzas deben ser iguales
df.meanvar <- df %>%
  group_by(Date) %>%
  summarise(mean(ConfirmedCasesWeekLater), var(ConfirmedCasesWeekLater))
df.meanvar

# Hay un claro problema con esta hipótesis, ya que las varianzas son mucho más
# grandes que las medias

# Es lineal el logaritmo de la variable a predecir y la estimación de infectados
# entrantes?
p <- ggplot(df, aes(x=CasesEstimation,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados entrantes',y='log(Casos Confirmados una semana después)')
p

# Veamos por países:
p <- ggplot(df, aes(x=CasesEstimation,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados entrantes',y='log(Casos Confirmados una semana después)')+
  facet_wrap(~Zone)
p

# Veamos por fechas
p <- ggplot(df, aes(x=CasesEstimation,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados entrantes',y='log(Casos Confirmados una semana después)')+
  facet_wrap(~Date)
p

# Parece que siguen una exponencial. Podemos hallar una buena potencia, pero parece más útil
# aplicar logaritmos a las dos variables
p <- ggplot(df, aes(x=logX,y=log10(logY))) + 
  geom_point()+
  labs(x='log(Infectados entrantes)',y='log(log(Casos Confirmados una semana después))')+
  facet_wrap(~Date)
p

# Aunque sigue habiendo problemas, ahora parece que siguen mejor una recta en la mayoría
# de casos. Sin embargo, el logaritmo del logaritmo no siempre es calculable, ya que en muchos
# casos la variable log(ConfirmedCases) es 0.

# Con una potencia:
pot = 1/3
p <- ggplot(df, aes(x=CasesEstimation^pot,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados entrantes ^ 1/3',y='log(Casos Confirmados una semana después)')+
  facet_wrap(~Zone)
p

# Aparte de separar por países, podemos separar por fechas, en especial el test set
pot = 1/3
p <- ggplot(df, aes(x=ConfirmedCases^pot,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados entrantes ^ 1/3',y='log(Casos Confirmados una semana después)')+
  facet_wrap(~Date)
p

# Ahora, veamos la relación con los casos confirmados
pot = 1/5
p <- ggplot(df, aes(x=ConfirmedCases^pot,y=log10(ConfirmedCasesWeekLater))) + 
  geom_point()+
  labs(x='Infectados confirmados ^ 1/5',y='log(Casos Confirmados una semana después)')+
  facet_wrap(~Date)
p

####################################################################
# Regresión de Poisson
####################################################################

summary(m1 <- glm(ConfirmedCasesWeekLater ~ ConfirmedCases + CasesEstimation + Population,
                  family="poisson", data=df.train))

df.test$predictions <- predict(m1, newdata=df.test, type="response")

# head(df.test)

# Plot
plot.df(df.test, log.plot = TRUE, names=c('predictions'))

rmse(df.test$ConfirmedCasesWeekLater,df.test$predictions)

####################################################################
# Regresión de Quasi-Poisson
####################################################################
summary(m1 <- glm(ConfirmedCasesWeekLater ~ ConfirmedCases + CasesEstimation + Population,
                  family=quasipoisson, data=df.train))

df.test$predictions <- predict(m1, newdata=df.test, type="response")

# head(df.test)

# Plot
plot.df(df.test, log.plot = TRUE, names=c('predictions'))

rmse(df.test$ConfirmedCasesWeekLater,df.test$predictions)


####################################################################
# Regresión de Binomial Negativa
####################################################################

summary(m1 <- glm.nb(ConfirmedCasesWeekLater ~ ConfirmedCases + CasesEstimation + Population,
                  data=df.train))

df.test$predictions <- predict(m1, newdata=df.test, type="response")

head(df.test)

# Plot
plot.df(df.test, log.plot = TRUE, names=c('predictions'))

rmse(df.test$ConfirmedCasesWeekLater,df.test$predictions)

####################################################################
# Regresión de Zero-Inflated Regression
####################################################################

summary(m1 <- zeroinfl(ConfirmedCasesWeekLater ~ ConfirmedCases + CasesEstimation,
                     data=df.train))

df.test$predictions <- predict(m1, newdata=df.test, type="response")

head(df.test)

# Plot
plot.df(df.test, log.plot = TRUE, names=c('predictions'))

rmse(df.test$ConfirmedCasesWeekLater,df.test$predictions)

# En general, este punto de vista no parece positivo. Vamos a probar a hacerlo
# de forma que cada día se prediga con el modelo entrenado con los datos del día
# anterior

#######################################################################
# Predicción de modelos day-by-day
#######################################################################
day.by.day <- function(data, family, formula = 1){
  data[,family] <- NaN
  dates <- unique(data$Date)
  for(date in as.list(dates[-1])){
    prev.date <- date - 1
    
    data.train <- data[data$Date == prev.date,]
    data.test <- data[data$Date == date,]
    
    if(formula == 1){
      formula = ConfirmedCasesWeekLater ~ CasesEstimation
    }
    else if(formula == 2){
      formula = ConfirmedCasesWeekLater ~ CasesEstimation + ConfirmedCases
    }
    # else if(formula == 3){
    #   formula = logY ~ logX
    # }
    else if(formula == 3){
      formula = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
    }
    else if(formula == 4){
      formula = ConfirmedCasesWeekLater ~ ImportedCubeRoot + ConfirmedRoot
    }
    
    if(family == "poisson"){
      modelo <- glm(formula,
                   family="poisson", data=data.train)
    }
    else if(family == "quasipoisson"){
      modelo <- glm(formula,
                   family="quasipoisson", data=data.train, offset=log10(Population))
    }
    else if(family == "binomial"){
      modelo <-glm.nb(formula,
                     data=data.train, offset(log10(Population)))
    }
    else if(family == "ZINB"){
      modelo <- zeroinfl(formula,
             data=data.train)
    }
    
    data[data$Date == date,family] <- predict(modelo, newdata=data.test, type="response")
  }

  return(data[data$Date != dates[1],])
}

names = c('quasipoisson')
results <- df
for(name in names){
  results <- day.by.day(results, name, formula = 3)
}

# results$poisson <- 10^(results$poisson+1) - 1
plot.df(results, log.plot = TRUE, names=names)

rmse(results$ConfirmedCasesWeekLater,results$poisson)

# Veamos el RMSE
names = c('poisson', 'quasipoisson', 'binomial')
formulas = c(1,2,3)
rmse.df <- data.frame(row.names=formulas)
for(name in names){
  col <- c()
  for (form in formulas){
    result <- day.by.day(df,name,formula=form)
    col <- c(col,rmse(result[,"ConfirmedCasesWeekLater"],result[,name]))
  }
  rmse.df[,name] <- col
}

# heatmap(as.matrix(rmse.df))

################################################################
# Tablas de estimación
###############################################################
create.confint <- function(df, formula, family, feature){
  name.sup <- paste0(feature,"_high")
  name.inf <- paste0(feature,"_low")

  dates <- unique(df$Date)
  
  df.intervals <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df.intervals) <- c("Date",feature, name.inf, name.sup)
  df.intervals$Date <- as.Date(df.intervals$Date)
  for(fecha in as.list(dates[-1])){
    prev.date <- fecha - 1
    
    data.train <- df[df$Date == prev.date,]
    data.test <- df[df$Date == fecha,]
    
    if(formula == 1){
      formula = ConfirmedCasesWeekLater ~ CasesEstimation
    }
    else if(formula == 2){
      formula = ConfirmedCasesWeekLater ~ CasesEstimation + ConfirmedCases
    }
    else if(formula == 3){
      formula = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
    }
    
    if(family == "poisson"){
      modelo <- glm(formula,
                    family="poisson", data=data.train)
    }
    else if(family == "quasipoisson"){
      modelo <- glm(formula,
                    family="quasipoisson", data=data.train, offset=log10(Population))
    }
    else if(family == "binomial"){
      modelo <-glm.nb(formula,
                      data=data.train, offset(log10(Population)))
    }
    else if(family == "ZINB"){
      modelo <- zeroinfl(formula,
                         data=data.train)
    }
    
    conf.interval <- confint(modelo)
    
    new.row <- nrow(df.intervals)+1 
    df.intervals[new.row,"Date"] <- fecha
    df.intervals[new.row,feature] <- modelo$coefficients[feature]
    df.intervals[new.row,name.inf] <- conf.interval[feature,1]
    df.intervals[new.row,name.sup] <- conf.interval[feature,2]
  }
  return(df.intervals)
}


features = c("EstimationCubeRoot", "ConfirmedRoot", '(Intercept)')
families = c("poisson","quasipoisson",'binomial')

for(family in families){
  df.confint <- NULL
  for(feat in features){
    if (is.null(df.confint)){
      df.confint <- create.confint(df, formula = 3, family = family, feature = feat)
    }
    else{
      df.confint <- merge(df.confint,create.confint(df, formula = 3, family = family, feature = feat))
    }
    # Eliminamos paréntesis
    colnames(df.confint) <-  gsub("\\(","",colnames(df.confint))
    colnames(df.confint) <-  gsub("\\)","",colnames(df.confint))
  }
  p <- ggplot(df.confint, aes(x=Date)) +
    ggtitle(paste0("Estimación de coeficientes en el modelo ", family))+
    labs(y="Coeficientes")
  for(feat in features){
    feat <- gsub("\\(","",feat)
    feat <- gsub("\\)","",feat)
    
    p <- p + geom_line(aes_string(y=feat,colour=shQuote(feat)))+
      geom_ribbon(aes_string(ymin = paste0(feat,"_low"), ymax = paste0(feat,"_high")), alpha = 0.1)
  }
  x11()
  plot(p)
}

################################################################
# Evolución de los p-valores
###############################################################
create.pvals <- function(df, formula, family){
  if(formula == 1){
    formula = ConfirmedCasesWeekLater ~ CasesEstimation
    cols <- c("Date","(Intercept)", "CasesEstimation")
  }
  else if(formula == 2){
    formula = ConfirmedCasesWeekLater ~ CasesEstimation + ConfirmedCases
    cols <- c("Date","(Intercept)", "CasesEstimation", "ConfirmedCases")
  }
  else if(formula == 3){
    formula = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
    cols <- c("Date","(Intercept)", "EstimationCubeRoot", "ConfirmedRoot")
  }
  
  dates <- unique(df$Date)
  
  df.pvals <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(df.pvals) <- cols
  df.pvals$Date <- as.Date(df.pvals$Date)
  for(fecha in as.list(dates[-1])){
    prev.date <- fecha - 1
    
    data.train <- df[df$Date == prev.date,]
    data.test <- df[df$Date == fecha,]
    
    if(family == "poisson"){
      modelo <- glm(formula,
                    family="poisson", data=data.train)
    }
    else if(family == "quasipoisson"){
      modelo <- glm(formula,
                    family="quasipoisson", data=data.train, offset=log10(Population))
    }
    else if(family == "binomial"){
      modelo <-glm.nb(formula,
                      data=data.train, offset(log10(Population)))
    }
    else if(family == "ZINB"){
      modelo <- zeroinfl(formula,
                         data=data.train)
    }
    
    new.row <- nrow(df.pvals)+1 
    
    df.pvals[new.row,"Date"] <- fecha
    for(name in cols[-1]){
      if(family == 'quasipoisson'){
        df.pvals[new.row,name] <- coef(summary(modelo))[name,'Pr(>|t|)']
      }
      else{
        df.pvals[new.row,name] <- coef(summary(modelo))[name,'Pr(>|z|)']
      }
    }
  }
  colnames(df.pvals) <-  gsub("\\(","",colnames(df.pvals))
  colnames(df.pvals) <-  gsub("\\)","",colnames(df.pvals))
  
  return(df.pvals)
}

families <- c('poisson','quasipoisson','binomial')
for(family in families){
  df.pvals <- create.pvals(df, formula = 3, family = family)
  p <- ggplot(df.pvals, aes(x=Date)) +
    geom_line(aes_string(y='Intercept',colour=shQuote('Intercept')))+
    geom_line(aes(y=EstimationCubeRoot,colour='FlightEstimation'))+
    geom_line(aes(y=ConfirmedRoot,colour='ConfirmedCases'))+
    geom_hline(yintercept=0.05, linetype="dashed", color = "red")+
    labs(title=paste0("Evolución del p-valor en ",family),y='Coeficientes')
  x11()
  plot(p)
}


#################################################################
# Tablas ANOVA para comprobar si los atributos mejoran
#################################################################
# Recoge los p-valores de la tabla ANOVA con dos fórmulas (deben estar anidadas)
create.anova <- function(df, formula1, formula2, families){
  dates <- unique(df$Date)
  
  df.anova <- data.frame(matrix(ncol = length(families)+1, nrow = 0))
  colnames(df.anova) <- c("Date",families)
  df.anova$Date <- as.Date(df.anova$Date)
  for(fecha in as.list(dates[-1])){
    prev.date <- fecha - 1
    
    data.train <- df[df$Date == prev.date,]
    data.test <- df[df$Date == fecha,]
    
    new.row <- nrow(df.anova)+1 
    df.anova[new.row,"Date"] <- fecha
    
    for(family in families){
      if(family == "poisson"){
        modelo1 <- glm(formula1,
                      family="poisson", data=data.train)
        modelo2 <- glm(formula2,
                       family="poisson", data=data.train)
      }
      else if(family == "quasipoisson"){
        modelo1 <- glm(formula1,
                      family="quasipoisson", data=data.train, offset=log10(Population))
        modelo2 <- glm(formula2,
                      family="quasipoisson", data=data.train, offset=log10(Population))
      }
      else if(family == "binomial"){
        modelo1 <-glm.nb(formula1,
                        data=data.train, offset(log10(Population)))
        modelo2 <-glm.nb(formula2,
                        data=data.train, offset(log10(Population)))
      }
      else if(family == "ZINB"){
        modelo1 <- zeroinfl(formula1,
                           data=data.train)
        modelo2 <- zeroinfl(formula2,
                           data=data.train)
      }
      
      drop_in_dev <- anova(modelo1, modelo2, test = "Chisq")
      df.anova[new.row,family] <- drop_in_dev[2,ncol(drop_in_dev)]
    }
  }
  
  return(df.anova)
}

plot.anova <- function(df, formula1, formula2, families){
  df.anova <- create.anova(df, formula1 = formula1,
                           formula2 = formula2,
                           families = families)
  
  p <- ggplot(df.anova, aes(x=Date)) +
    labs(title="Evolución del p-valor de ANOVA",y='p-valor')+
    geom_hline(yintercept=0.05, linetype="dashed", color = "red")
  for(family in families){
    p <- p + geom_line(aes_string(y=family,colour=shQuote(family)))
  }
  plot(p)
  return(df.anova)
}

formula1 = ConfirmedCasesWeekLater ~ EstimationCubeRoot
formula2 = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
families = c("poisson","quasipoisson",'binomial')
df.anova <- plot.anova(df,formula1,formula2,families)

# Probamos algunas columnas extra:
families = c("poisson","quasipoisson")
df$aux <- df$ImportedRisk^(1/3)
formula1 = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot 
formula2 = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot + aux
plot.anova(df,formula1,formula2,families)

# Bien para la regresión de poisson, pero quasipoisson considera esta variable redundante

#####################################################################
# Análisis de residuos
#####################################################################
create.residuals <- function(df, formula, families){
  dates <- unique(df$Date)
  
  df.residuals <- data.frame(matrix(ncol = length(families)+1, nrow = 0))
  colnames(df.residuals) <- c("Date",families)
  df.residuals$Date <- as.Date(df.residuals$Date)
  for(family in families){
    num.row <- 0
    for(fecha in as.list(dates[-1])){
      prev.date <- fecha - 1
      
      data.train <- df[df$Date == prev.date,]
      data.test <- df[df$Date == fecha,]
      
      if(family == "poisson"){
        modelo <- glm(formula,
                      family="poisson", data=data.train)
      }
      else if(family == "quasipoisson"){
        modelo <- glm(formula,
                      family="quasipoisson", data=data.train, offset=log10(Population))
      }
      else if(family == "binomial"){
        modelo <-glm.nb(formula,
                        data=data.train, offset(log10(Population)))
      }
      else if(family == "ZINB"){
        modelo <- zeroinfl(formula,
                           data=data.train)
      }
      for(res in modelo$residuals){
        num.row <- num.row+1 
        df.residuals[num.row,"Date"] <- fecha
        df.residuals[num.row,family] <- res
      }
    }
  }
  return(df.residuals)
}

plot.residuals <- function(df, formula, families){
  df.residuals <- create.residuals(df, formula = formula,
                                 families = families)
  
  for(family in families){
    p <- ggplot(df.residuals, aes(x=Date)) +
      labs(title=paste0("Evolución de los residuos en ",family),y='Residuos')+
      geom_point(aes_string(y=family))
      x11()
      plot(p)
  }
  return(df.residuals)
}

formula = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
families = c("poisson","quasipoisson",'binomial')
df.residuals <- plot.residuals(df,formula,families)

# Residuo = obs - pred?
data.train <- df[df$Date == "2020-02-27",]
modelo <- glm(formula,
              family="quasipoisson",
              data=data.train,
              offset=log10(Population))
data.frame(residuals = modelo$residuals, 
           observed = data.train$ConfirmedCasesWeekLater,
           predicted = modelo$fitted.values,
           valores2 = (data.train$ConfirmedCasesWeekLater -modelo$fitted.values)/sqrt(modelo$fitted.values))
         
  
####################################################################
# Goodness of fit
#####################################################################
# Tipo de goodness:
# 1) Análisis de la residual deviance
# 2) Análisis de null-residual
create.goodness <- function(df, formula, families, type=1){
  dates <- unique(df$Date)
  
  df.goodness <- data.frame(matrix(ncol = length(families)+1, nrow = 0))
  colnames(df.goodness) <- c("Date",families)
  df.goodness$Date <- as.Date(df.goodness$Date)
  for(fecha in as.list(dates[-1])){
    prev.date <- fecha - 1
    
    data.train <- df[df$Date == prev.date,]
    data.test <- df[df$Date == fecha,]
    
    new.row <- nrow(df.goodness)+1 
    df.goodness[new.row,"Date"] <- fecha
    
    for(family in families){
      if(family == "poisson"){
        modelo <- glm(formula,
                       family="poisson", data=data.train)
        null.modelo <- glm(ConfirmedCasesWeekLater ~ 1, family = "poisson", data = data.train)
      }
      else if(family == "quasipoisson"){
        modelo <- glm(formula,
                       family="quasipoisson", data=data.train, offset=log10(Population))
        null.modelo <- glm(ConfirmedCasesWeekLater ~ 1, family = "quasipoisson", data = data.train,
                           offset=log10(Population))
      }
      else if(family == "binomial"){
        modelo <-glm.nb(formula,
                         data=data.train, offset(log10(Population)))
        null.modelo <- glm.nb(ConfirmedCasesWeekLater ~ 1,
                              data=data.train, offset(log10(Population)))
      }
      else if(family == "ZINB"){
        modelo <- zeroinfl(formula,
                            data=data.train)
        null.modelo <- zeroinfl(ConfirmedCasesWeekLater ~ 1, data = data.train)
      }
      
      if(type == 1){
        goodness <- 1-pchisq(modelo$deviance, modelo$df.residual)
      }
      else if(type == 2){
        goodness.table <- anova(null.modelo, modelo, test = "Chisq")
        goodness <- goodness.table[2,ncol(goodness.table)]
      }
      df.goodness[new.row,family] <- goodness
    }
  }
  
  return(df.goodness)
}

plot.goodness <- function(df, formula, families, type=1){
  df.goodness <- create.goodness(df, formula = formula,
                           families = families, type=type)
  
  if(type==1){
    title = "Residual deviance"
  }
  else{
    title = "Null deviance"
  }
  
  p <- ggplot(df.goodness, aes(x=Date)) +
    labs(title=paste0("Evolución de la bondad de ajuste: ",title),y='p-valor')+
    geom_hline(yintercept=0.05, linetype="dashed", color = "red")
  for(family in families){
    p <- p + geom_line(aes_string(y=family,colour=shQuote(family)))
  }
  plot(p)
  return(df.goodness)
}

formula = ConfirmedCasesWeekLater ~ EstimationCubeRoot + ConfirmedRoot
families = c("poisson","quasipoisson",'binomial')
df.goodness <- plot.goodness(df,formula,families, type=2)

####################################################################
# Overdispersion
####################################################################

# Medias y varianzas deben ser iguales en la variable respuesta
df.meanvar <- df %>%
  group_by(Date) %>%
  summarise(mean(ConfirmedCasesWeekLater), var(ConfirmedCasesWeekLater))

colnames(df.meanvar) <- c("Date", "Mean", "Var")

df.meanvar$Div <- df.meanvar$Var / df.meanvar$Mean
df.meanvar

p <- ggplot(df.meanvar, aes(x=Date, y=Div)) +
  labs(title=paste0("Evolución del cociente de varianza y media"),y='Varianza/Media')+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_line()+
  scale_y_log10()
  
plot(p)

# Obviamente, no se cumple la suposición inicial de Poisson
# Veamos cómo evoluciona el parámetro de dispersión en una quasi-Poisson
create.overdispersion <- function(df, formula, family='quasipoisson'){  
  dates <- unique(df$Date)
  
  df.over <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df.over) <- c("Date","Overdispersion")
  df.over$Date <- as.Date(df.over$Date)
  for(fecha in as.list(dates[-1])){
    prev.date <- fecha - 1
    
    data.train <- df[df$Date == prev.date,]
    data.test <- df[df$Date == fecha,]
    
    new.row <- nrow(df.over)+1 
    df.over[new.row,"Date"] <- fecha
    
    if(family == 'quasipoisson'){
      modelo <- glm(formula,
                    family="quasipoisson", data=data.train, offset=log10(Population))
    }
    else if(family == 'binomial'){
      modelo <-glm.nb(formula,
                      data=data.train, offset(log10(Population)))
    }
    
    over <- sum(residuals(modelo,"pearson")^2)/modelo$df.residual
    df.over[new.row,"Overdispersion"] <- over
  }
  
  return(df.over)
}

family = 'quasipoisson'
df.over <- create.overdispersion(df, formula, family=family)

p <- ggplot(df.over, aes(x=Date, y=Overdispersion)) +
  labs(title=paste0("Evolución de la sobredispersión en un modelo ", family),y='Parámetro de Sobredisp.')+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_y_log10()+
  geom_line()
p


#############################################################################
# Comparativa de Casos Confirmados y riesgo importado
############################################################################

names = c('quasipoisson')
results.conf <- df
for(name in names){
  results.conf <- day.by.day(results, name, formula = 3)
}

results.imp <- df
for(name in names){
  results.imp <- day.by.day(results, name, formula = 4)
}

results.imp$quasi.conf <- results.conf$quasipoisson

p <- ggplot(results.imp, aes(x=Date)) +
  geom_line(aes(y=ConfirmedCasesWeekLater,colour="Confirmed")) +
  geom_line(aes(y=quasi.conf,colour="Standard Prediction")) +
  geom_line(aes(y=quasipoisson,colour="Imported Risk Prediction")) +
  scale_y_log10(limits=c(1,NA)) +
  facet_wrap(~Zone)
p

# Son prácticamente iguales
# Veamos el RMSE
names = c('poisson', 'quasipoisson', 'binomial')
formulas = c(3,4)
rmse.df <- data.frame(row.names=formulas)
for(name in names){
  col <- c()
  for (form in formulas){
    result <- day.by.day(df,name,formula=form)
    col <- c(col,rmse(result[,"ConfirmedCasesWeekLater"],result[,name]))
  }
  rmse.df[,name] <- col
}
row.names(rmse.df) <- c("Casos Estimados", "Riesgo Importado")
rmse.df

# Mejor una métrica porcentual. Como hay resultados muy próximos a 0, debemos usar el
# SMAPE
mape.df <- data.frame(row.names=formulas)
for(name in names){
  col <- c()
  for (form in formulas){
    result <- day.by.day(df[df$Date > as.Date("2020-03-01"),],name,formula=form)
    col <- c(col,smape(result[,"ConfirmedCasesWeekLater"],result[,name]))
  }
  mape.df[,name] <- col
}
row.names(mape.df) <- c("Casos Estimados", "Riesgo Importado")
mape.df
