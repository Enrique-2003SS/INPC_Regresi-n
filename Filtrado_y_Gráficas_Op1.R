library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

Datos_MySd_X_CdyPrdct=readxl::read_xlsx("media_y_desviacion_por_producto_por_ciudad.xlsx")

#Corregimos algunas cosillas
Datos_MySd_X_CdyPrdct$nom_ent=gsub(", Índice de precios al consumidor, por objeto del gasto","",
                                   Datos_MySd_X_CdyPrdct$nom_ent)
Datos_MySd_X_CdyPrdct$nom_ent=gsub("7. ","",Datos_MySd_X_CdyPrdct$nom_ent)


#Renombramos las columnas
colnames(Datos_MySd_X_CdyPrdct)[colnames(Datos_MySd_X_CdyPrdct)=="nom_ent"]="Ciudad"
colnames(Datos_MySd_X_CdyPrdct)[colnames(Datos_MySd_X_CdyPrdct)=="class_5"]="Genérico"
colnames(Datos_MySd_X_CdyPrdct)[colnames(Datos_MySd_X_CdyPrdct)=="mean"]="Estabilidad"
colnames(Datos_MySd_X_CdyPrdct)[colnames(Datos_MySd_X_CdyPrdct)=="sd"]="Variabilidad"

#Calculamos los promedios de la estabilidad y variabilidad
Medias_X_Productos=Datos_MySd_X_CdyPrdct |>
  dplyr::group_by(Genérico) |>
  dplyr::summarise(Media_Estabilidad=mean(Estabilidad,na.rm = T),
                   Media_Variabilidad=mean(Variabilidad,na.rm = T))

#Creamos las variables que nos dirán si la estabilidad y la variabilidad son
#estrictamente menores que sus respectivos promedios
Datos_MySd_X_CdyPrdct$Estabilidad_Competitiva=numeric(nrow(Datos_MySd_X_CdyPrdct))
Datos_MySd_X_CdyPrdct$Variabilidad_Competitiva=numeric(nrow(Datos_MySd_X_CdyPrdct))

#Y rellenamos
for(producto in Medias_X_Productos$Genérico){
  Datos_MySd_X_CdyPrdct$Estabilidad_Competitiva[Datos_MySd_X_CdyPrdct$Genérico==producto]=ifelse(Datos_MySd_X_CdyPrdct$Estabilidad[Datos_MySd_X_CdyPrdct$Genérico==producto] < Medias_X_Productos$Media_Estabilidad[Medias_X_Productos$Genérico==producto],
         1,0)
  
  Datos_MySd_X_CdyPrdct$Variabilidad_Competitiva[Datos_MySd_X_CdyPrdct$Genérico==producto]=ifelse(Datos_MySd_X_CdyPrdct$Variabilidad[Datos_MySd_X_CdyPrdct$Genérico==producto] < Medias_X_Productos$Media_Variabilidad[Medias_X_Productos$Genérico==producto],
                                                                                         1,0)
}

#Ahora solo generamos la nueva columna que nos diga si el producto tiene bajo
#costo promedio (Estabilidad cercana a 100) y bajo cambio en sus precios
#(Variabilidad cercana a 0)
Datos_MySd_X_CdyPrdct$Competitivo=Datos_MySd_X_CdyPrdct$Estabilidad_Competitiva*Datos_MySd_X_CdyPrdct$Variabilidad_Competitiva
openxlsx::write.xlsx(Datos_MySd_X_CdyPrdct,"INPC_Op1.xlsx",overwrite = T)

#Ahora creamos las graficas por producto
Gráficas_Op1=list() #Esta será la que guarde las gráficas para verlas después
s=1
for(producto in Medias_X_Productos$Genérico){
  #Primero pasamos de donde salen los datos 
  grafica_chafa <- ggplot(Datos_MySd_X_CdyPrdct[Datos_MySd_X_CdyPrdct$Genérico==producto,],
              aes(x = Estabilidad, y = Variabilidad, label = Ciudad,
                              #Aquí se crea el contenido del hover (curiosamente será del mismo color del punto)
                               text = paste(Ciudad, 
                                            "<br>Estabilidad:", round(Estabilidad, 3), #Van a ir redondeadas a 3 digitos 
                                            "<br>Variabilidad:", round(Variabilidad, 3)))) +
    #Aquí definimos los puntos
    geom_point(color = rgb(98,17,50,maxColorValue = 255)) +
    #Para el nombre de la etiqueta (es la variable que antes se define como "label")
    geom_text(vjust = -1, size = 3) + 
    #las ablines
    geom_vline(xintercept = Medias_X_Productos$Media_Estabilidad[Medias_X_Productos$Genérico==producto], color = "blue", linetype = "dashed") +
    geom_hline(yintercept = Medias_X_Productos$Media_Variabilidad[Medias_X_Productos$Genérico==producto], color = "red", linetype = "dashed") +
    #y los nombres de los ejes
    labs(title = producto, x = "Estabilidad (mean)", y = "Variabilidad (sd)") +
    theme_minimal()
  
  grafica_buena=plotly::ggplotly(grafica_chafa,tooltip = "text")
  Gráficas_Op1[[s]]=grafica_buena
  names(Gráficas_Op1)[s]=producto
  s=s+1
  # plot(Datos_MySd_X_CdyPrdct$mean[Datos_MySd_X_CdyPrdct$class_5==producto],
  #      Datos_MySd_X_CdyPrdct$sd[Datos_MySd_X_CdyPrdct$class_5==producto],
  #      main=producto,xlab = "Media",ylab="Desviación Estándar",col="purple")
  # 
  # text(Datos_MySd_X_CdyPrdct$mean[Datos_MySd_X_CdyPrdct$class_5==producto],
  #      Datos_MySd_X_CdyPrdct$sd[Datos_MySd_X_CdyPrdct$class_5==producto],
  #      labels = Datos_MySd_X_CdyPrdct$nom_ent[Datos_MySd_X_CdyPrdct$class_5==producto],
  #      cex = 0.65,pos = 3)
  # 
  # abline(v=Medias_X_Productos$Mean_Mean[Medias_X_Productos$class_5==producto],col="blue")
  # abline(h=Medias_X_Productos$Mean_Sd[Medias_X_Productos$class_5==producto],col="red")
}


library(tidyr)

#Aquí vamos a hacer las matrices chidas de 0 y 1
Pivot_MySd=Datos_MySd_X_CdyPrdct|> select(-c(Estabilidad,Variabilidad,Estabilidad_Competitiva,Variabilidad_Competitiva))|>
  pivot_wider(names_from = Genérico,
              values_from = Competitivo)

openxlsx::write.xlsx(Pivot_MySd,"Pivot_INPC_Op1.xlsx",overwrite = T)
