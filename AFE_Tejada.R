pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","raster","sf","ggspatial","cluster","factoextra",
              "NbClust","tidyr","forecast","semPlot","semTools","corrplot",
              "corrr","haven","psych","dplyr","lavaan","readr","cvms","tm",
              "NLP","SnowballC","RColorBrewer","wordcloud","wordcloud2",
              "RefManageR","bibliometrix","GGally","quanteda","ggplot2",
              "ggpubr","Factoshiny","syuzhet","tm","RColorBrewer","tokenizers",
              "stringr","sentimentr","stringi","stopwords","twitteR",
              "mscstexta4r","plyr","corrplot","psych","corrr","latticeExtra",
              "semPlot","lavaan","readr","lme4","sjPlot","gvlma","Rcmdr",
              "tidymodels","caret","lmtest")

pkg(packages)




'''Analisis Factorial exploratorio'''
#split data 50
#cargo el documento
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
#Hago el split de los datos con el filtro elegido
data50 <-data%>% filter(retweet_count >=50)
#corro el odelo 
modpoisson50<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data50(link = "log"))
#corro el sumario de datos
S50<-summary(modpoisson50)
#corro coeficientes restando 1 para ver efectivamente como queda la relación
exp(S50coefficients)-1
#Bondad de ajuste del modelo
lrtest(modpoisson50)
#split data 100
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data100 <-data%>% filter(retweet_count >=100)
modpoisson100<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data100(link = "log"))
S100<-summary(modpoisson100)
exp(S100$coefficients)-1
lrtest(modpoisson100)
#split data 200
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data200 <-data%>% filter(retweet_count >=200)
modpoisson200<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data200(link = "log"))
S200<-summary(modpoisson200)
exp(S200$coefficients)-1
lrtest(modpoisson200)
#split data 500
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data500 <-data%>% filter(retweet_count >=500)
modpoisson500<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data500(link = "log"))
S500<-summary(modpoisson500)
exp(S500$coefficients)-1
lrtest(modpoisson500)
#split data 1000
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data1000 <-data%>% filter(retweet_count >=100)
modpoisson<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data1000(link = "log"))
S<-summary(modpoisson)
exp(S$coefficients)-1
lrtest(modpoisson)
#exporto los documentos a un excel 
write.table(MFCM, file="most frequent cited manuscripts source 
            covid.csv", sep=";", row.names= T) 
#aquí engancho el data para que quede fijo
attach(data)
################# Lineal modelo generalizado ################################
##############################################################################

#Aquí tengo el modelo poisson con las variables correspondientes. 
modpoisson<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
            quote+hour+weekday+Hashtag_Cod+month,family= poisson(link = "log")) 
#Sacoel sumario del modelo para ver como se ajusta el modelo
S<-summary(modpoisson)
S
#aquí tomo cada valor del error standar y lo divido entre el 
#valor Z para tener el valor Z o P valor del modelo y ver la 
# significancia estadística de los valores. 
4.199/0.002168
#saco los coeficientes del modelo que me dirán la predicción
#de las variables sobre mi variable Y  
exp(S$coefficients)-1
#Bondad de ajuste del modelo
lrtest(modpoisson)

#gvlma(S)   ----> Solo sirve para lm y no para GLM
#+is_retweet

#GLM BINOM
#si la variable es dicotomica entonces se usa binomial con un link de tipo
#LOGIT O PROVIT.
#cambio el attach para partir la database en una porción menor
attach(data)
modbinom1<-glm(is_retweet~ENTIDAD_CODIGO+favorite_count+Photo+Vídeo+Only_text+Url+
            quote+hour+weekday+Hashtag_Cod+month,family= binomial(link = "logit")) 
S1<-summary(modbinom1)
S1
#corro step para comparar modelos y que R me diga cual es mejor.
step(modbinom1)
exp(S1$coefficients)-1
lrtest(modbinom1)
#comparar los dos modelos para ver cual es más significativa
'''Efectivamente se puede nservar que el modelo poisson es más significativo
debido a que los tweeets son un conteo en sí mismos, por tal razón contar tweets
tiene más sentido que convertirlos a categoricas en 0,1. 
aqí se debe citar a Neldery waterman para referirse a las mejorías del link log y no 
al link logit en este tipo de casos'''
lrtest(modbinom,modpoisson)
'''entonces se observa que el poisson es mucho mejor'''
#monto el binom2
modbinom2<-glm(is_retweet~ENTIDAD_CODIGO+Photo+Vídeo+Only_text+Url+
                hour+weekday+month,family= binomial(link = "logit"))
S2<-summary(modbinom2)
S2
#corro step para comparar modelos y que R me diga cual es mejor.
exp(S2$coefficients)-1
lrtest(modbinom2, modbinom1)

#Reviso la tabla de Odds ratio para identificar la razón de riego del modelo
tab_model(mod1)
exp(1)


#por cada entidad de codigo el conteo de retweet aumenta en 0.77, si aumenta 
#only_text en 1 unidad, entonces los retweets aumentan en 0.68, si aumenta el 
#video 1 unidad el retweet aumenta en 2 conteos. (la relacion siempre de 1 a lo 
#que quede despues de 1)
xtable(S)
write.table(S, file="Modelo2.csv", sep=";", row.names= T)


#### Prueba con LM para verificar los cambios en el modelo #####
mod2<-lm(retweet_count~ENTIDAD_CODIGO+favorite_count+Photo+Vídeo+Only_text+Url+
            quote+hour+weekday+Hashtag_Cod+month,family= poisson(link = "provit"))
S2<-summary(mod2)
S2
anova(mod2)



##############################################################################
######### Creando tidymodels ### building tidymodels ########################
#############################################################################
#first see de documentation to get guide
?tidymodels
#first i will split my data in 2 parts for my models.
L <- data %>% initial_split(prop = .8)

#procesing the data
Dt = training(L) %>% 
  recipe(retweet_count~ENTIDAD_CODIGO)%>%
  #step_corr(all_predictors())%>% step_center(all_predictors(),
  #-all_outcomes()) %>% step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
#pre-procesing
gt = Dt%>%
  bake(testing(L))
#LM
g_freq = linear_reg()%>% #it could be a random forest, or other method.
  set_engine("lm") %>%  #i declare the pakage that i will use in this case is lm
  fit(retweet_count~ENTIDAD_CODIGO, data=Dt %>%juice())
#finaly i declare the fit of the model and call the juice wich "exprime" el modelo
#para ver si se ajusta al máximo.
g_freq
summary(g_freq)
gvlma(g_freq)
