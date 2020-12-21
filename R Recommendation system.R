#1.- Install libraries
library(tidyverse)
library(magrittr)
library(lubridate)
library(randomForest)
library(caret)
library(readr)
library(ggplot2)
memory.limit(size = 1.7e+13)

#2.- CREATE DATA.RDA: This process has ordered genres and join imdb database to obtain a better accuracy at random forrest: 
imdbratings = read_tsv(file = "imdbratings.tsv")
links = read.csv(file="links.csv", header = TRUE, sep = ",")
links$tconst = str_c("tt",sprintf("%.7d", links$imdbId),sep="")
imdb = imdbratings %>% inner_join(links) %>% select(movieId,Promedio_Idbm= averageRating,Numero_Votos=numVotes)

movies = read.csv("movies.csv",sep = ",",header = T,stringsAsFactors = F) 
movies = movies %>%  mutate(Ano = as.numeric(str_sub(str_trim(title,side = "right"), start = -5,end = -2)))
movies[which(movies$Ano == 201),"Ano"]=c(2013,2017) 
movies[18790,"Ano"]=1983
movies[which(movies$Ano == 6),"Ano"]=NA
movies[which(is.na(movies$Ano)),] = 1992 # is the year average
movies = movies %>% inner_join(imdb) 

ratings = read.csv("ratings.csv",sep = ",",header = T,stringsAsFactors = F)

#genomescores = read.csv("genome-scores.csv",sep = ",",header = T,stringsAsFactors = F)
#genomestags = read.csv("genome-tags.csv",sep = ",",header = T,stringsAsFactors = F)
#tags = genomescores %>% inner_join(genomestags)
#data = data %>% left_join(tags %>% select(Id_pelicula = movieId, Tag = tag, Tag_Relevancia = relevance))

data = ratings %>% inner_join(movies) #%>% mutate(timestamp = as_datetime(timestamp))
data = data %>% rename(Id_usuario = userId,Id_pelicula = movieId,Indice = rating,Marca_temporal = timestamp,Titulo = title, Genero = genres)

data = data %>% separate(Genero,str_c("genero_",1:10),sep = "([|])",remove = F)
data = data %>% gather(str_c("genero_",1:10), key = "Generos_Categoria",value = "Generos",na.rm = T)

data = data %>% select(Id_usuario, Id_pelicula, Titulo, Generos, Ano, Promedio_Idbm, Numero_Votos, Marca_temporal, Indice)

#3.- THIS IS THE DATA.RDA:
save(data,file="data.rda")
rm(list = ls())
###########################################################################################################
###################################### MODEL ##############################################################
load("data.rda")

data = data %>% filter(Genero != "(no genres listed)" & Indice >= 4)

Usuarios = unique(data$Id_usuario)
training_u = sample(Usuarios, length(Usuarios)*0.50,replace = F)
testing_u = Usuarios[-training_u]

training = data %>% filter(Id_usuario %in% training_u)  %>% select(Indice,Ano,Genero,Marca_temporal,Promedio_Idbm,Numero_Votos) %>% mutate(Genero = factor(Genero)) 
rm(data,Usuarios)

Genero ~ Indice + Ano + Marca_Temporal + Promedio_Idbm + Numero_Votos

set.seed(0)
modelo = randomForest(Genero ~., data = training, ntree=30, method="class", norm.votes=FALSE, do.trace=10, proximity = FALSE, importance=TRUE)

muestra_2 = list(testing_u, modelo)
save(muestra_2, file="muestra_2.rda")

#########################################################################################
##################################### Forecast category #################################
load("muestra_2.rda")
load("data.rda")
modelo = muestra_2[[2]]
testing_u = muestra_2[[1]]
testing = data %>% filter(Id_usuario %in% testing_u)

# Let´s view users... and make a testing with the number 1900 user recommending movies:

unique(testing$Id_usuario)
testing_1900 = testing %>% filter(Id_usuario == 1900) %>% select(Indice,Ano,Genero,Marca_temporal,Promedio_Idbm,Numero_Votos) %>% mutate(Generos = factor(Genero))
testing_1900$Genero = fct_expand(testing_1900$Genero,levels(factor(testing$Genero))[2:20]) # More levels added to analize the error

set.seed(100)
prediccion_1900 = predict(modelo, newdata = testing_1900, type = "class") # type class so it doesn´t affect the number of levels less

confusionMatrix(prediccion_1900, testing_1900$Generos)

cm = table(prediccion_1900,testing_1900$Generos)
i = 
  j = 1:dim(cm)[2]

se = 0;
for (i in 1:dim(cm)[1])
{
  for (j in 1:dim(cm)[2])
  {
    se = se + cm[i,j] * (i-j)^2;
  }
}

mse = se / sum(sum(cm))
(rmse = sqrt(mse))



# > 28.97% accuracy

generos_sugerir = names(sort(table(prediccion_1900),decreasing = T)[1:3])
#To the user number 1900 the recomentations are Drama, Adventure and Comedy

testing_1900 %>% filter(Generos %in% generos_sugerir) %>% group_by(Generos) %>% summarise(idbm = mean(Promedio_Idbm)) %>% arrange(desc(idbm))

#peliculas a sugerir 
i1 = data %>% filter(Generos %in% "Drama" & Promedio_Idbm >= 8 & Ano >= 2015) %>% distinct(Titulo)
i2 = data %>% filter(Generos %in% "Crime" & Promedio_Idbm >= 8 & Ano >= 2015) %>% distinct(Titulo)
i3 = data %>% filter(Generos %in% "Action" & Promedio_Idbm >= 8 & Ano >= 2015) %>% distinct(Titulo)

i1 %>% filter(Titulo %in% c(i2$Titulo,i3$Titulo))

Promedio_anio = data %>% group_by(Ano) %>% summarise(Raiting = mean(Indice,na.rm=T))
Promedio_anio = Promedio_anio %>% filter(Ano %in% 2000:2019) %>% rename(Year = Ano) %>% mutate(Year = as.factor(Year))
color = colorRampPalette(c("red","yellow","blue"))(20)
ggplot(data = Promedio_anio) +
  geom_bar(mapping = aes(x = Year, y = Raiting, color = Year,fill = Year , group = 1), stat = "identity") + 
  theme_bw() + scale_color_manual(values = color) + scale_fill_manual(values = color) +
  labs(x="Year",title ="Average Raiting by Year")
muestra = data %>% filter(Ano %in% 2015:2019 & Genero != "(no genres listed)") %>% sample_n(1000) %>% mutate(Year = as.factor(Ano)) 

ggplot(data = muestra, aes(x = Genero, y = Indice)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Year))+
  geom_point(position = "jitter", aes(color = Genero))+
  theme_bw() +
  labs(x="Genres",title ="Boxplot of Genres ~ Ratings by last 5 Years")

muestra = data %>% filter(Ano %in% 2015:2019 & Genero != "(no genres listed)" & Genero %in% c("Action","Comedy","Drama","Mystery","Romance","Horror")) %>% mutate(Year = as.factor(Ano),Raiting = Indice) %>% sample_n(1500)
  ggplot(data = muestra, aes(x = Genero, y = Raiting, fill = Genero)) +
  geom_boxplot(outlier.shape = NA)+ geom_point(position = "jitter", aes(color = Year))+
  theme_bw() +
  labs(x="Genres",title ="Boxplot of Ratings by Genres in the last 5 Years")
