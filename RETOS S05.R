MIT <- read.csv( "Metro_Interstate_Traffic_Volume.csv")
str(MIT)


sapply(MIT, function(x) sum(is.na(x)))
interest.mit <- c("temp","weather_main","traffic_volume")
MIT.filter <- MIT[,interest.mit]
interest.col1 <- MIT.filter$temp
interes.col2 <- MIT.filter$weather_main
interest.col3 <- MIT.filter$traffic_volume

paste('Promedio :', mean(interest.col1))
paste('Promedio :', mean(interes.col2)) #aqui no pude sacar nada porque vi que puse una columna sin datos numericos
paste('Promedio :', mean(interest.col3))

paste('Mediana :', median(interest.col1))
paste('Mediana :', median(interest.col3))

table.col <- table(interest.col3)
print(table.col)

max.table.col <- max(table.col)
mode.col <- table.col[table.col == max.table.col]
print(paste('Moda:', names(mode.col)))


var.radius <- var(interest.col3)
round(var.radius,2)
print(paste('Varianza:', round(var.radius,2)))

sd.radius <- sd(interest.col3)
print(paste('Desviación estándar:', round(sd.radius,3)))


sd.radius**2 == var.radius

max(interest.col3)
min(interest.col3)

max(interest.col) - min(interest.col)


quant.col1 <- quantile(interest.col1, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
MIT.filter$qcuts <- cut(interest.col1, breaks = quant.col)
table(MIT.filter$qcuts)

head(MIT.filter,10)

summary(MIT.filter)


dtypes <- sapply(MIT, class)
dtypes


col.factor <- dtypes[dtypes == 'factor']
col.factor
col.factor <- names(col.factor)
col.factor


MIT.factor <- MIT[, c(col.factor)]
(MIT.factor)
Data Types
# Análisis de Frecuencias
table(MIT.factor)

View(MIT)

str(MIT)
MIT.num <- MIT[,-c(1,6,7,8)]
MIT.num

cor.mit <- melt(cor(MIT.num))


cor.mit %>% ggplot(aes(X1,X2)) + 
  geom_tile(aes(fill = value)) + 
  ggtitle('Matriz de correlación')+
  scale_fill_gradient(low = 'white', high = 'black') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))
