library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

Datos_MySd_X_CdyPrdct2=readxl::read_xlsx("media_y_desviacion_por_producto_por_ciudad.xlsx")

#Corregimos algunas cosillas
Datos_MySd_X_CdyPrdct2$nom_ent=gsub(", Índice de precios al consumidor, por objeto del gasto","",
                                   Datos_MySd_X_CdyPrdct2$nom_ent)
Datos_MySd_X_CdyPrdct2$nom_ent=gsub("7. ","",Datos_MySd_X_CdyPrdct2$nom_ent)


#Renombramos las columnas
colnames(Datos_MySd_X_CdyPrdct2)[colnames(Datos_MySd_X_CdyPrdct2)=="nom_ent"]="Ciudad"
colnames(Datos_MySd_X_CdyPrdct2)[colnames(Datos_MySd_X_CdyPrdct2)=="class_5"]="Genérico"
colnames(Datos_MySd_X_CdyPrdct2)[colnames(Datos_MySd_X_CdyPrdct2)=="mean"]="Estabilidad"
colnames(Datos_MySd_X_CdyPrdct2)[colnames(Datos_MySd_X_CdyPrdct2)=="sd"]="Variabilidad"

#Normalizamos los datos
Datos_Escalados <- Datos_MySd_X_CdyPrdct2 |>
  dplyr::group_by(Genérico) |>
  dplyr::mutate(
    Estabilidad_Normalizada=as.numeric(scale(Estabilidad)),
    Variabilidad_Normalizada=as.numeric(scale(Variabilidad)))|> 
  dplyr::ungroup()

Datos_Escalados$Indice=0.5*Datos_Escalados$Estabilidad_Normalizada + 0.5*Datos_Escalados$Variabilidad_Normalizada

Medias_Datos_Escalados=Datos_Escalados|>
  dplyr::group_by(Genérico) |>
  dplyr::summarise(Promedio_Indice=mean(Indice,na.rm = T))

Datos_Escalados$Indice_Competitivo=numeric(nrow(Datos_Escalados))

for(producto in Datos_Escalados$Genérico){
  Datos_Escalados$Indice_Competitivo[Datos_Escalados$Genérico==producto]=ifelse(Datos_Escalados$Indice[Datos_Escalados$Genérico==producto] < Medias_Datos_Escalados$Promedio_Indice[Medias_Datos_Escalados$Genérico==producto],
                                                                                         1,0)
}
Datos_Escalados$Indice_Competitivo[is.na(Datos_Escalados$Indice)]=0

#Para quitar los Na
openxlsx::write.xlsx(Datos_Escalados,"INPC_Op2.xlsx",overwrite = T)


Gráficas_Op2=list() #Esta será la que guarde las gráficas para verlas después
s=1
for(producto in Medias_Datos_Escalados$Genérico){
  
  valor_linea <- Medias_Datos_Escalados$Promedio_Indice[Medias_Datos_Escalados$Genérico == producto]
  
  
  #Primero pasamos de donde salen los datos 
  grafica_chafa <- ggplot(Datos_Escalados[Datos_Escalados$Genérico==producto,],
                          aes(x = Ciudad, y = Indice, label = Ciudad,
                              #Aquí se crea el contenido del hover (curiosamente será del mismo color del punto)
                              text = paste(Ciudad, 
                                           "<br>Indice:", round(Indice, 5)))) +
    #Aquí definimos los puntos
    geom_point(color = rgb(98,17,50,maxColorValue = 255)) +
    #Para el nombre de la etiqueta (es la variable que antes se define como "label")
    geom_text(vjust = -1, size = 3) + 
    
    
    # #las ablines
    geom_hline(yintercept = Medias_Datos_Escalados$Promedio_Indice[Medias_Datos_Escalados$Genérico==producto], color = "red", linetype = "dashed") +
    
  
    #y los nombres de los ejes
    labs(title = producto, x = "Ciudades", y = "Indice=(0.5Normalizado(mean) + 0.5Normalizado(sd))") +
    theme_minimal()
  
  grafica_buena=plotly::ggplotly(grafica_chafa,tooltip = "text")
  Gráficas_Op2[[s]]=grafica_buena
  names(Gráficas_Op2)[s]=producto
  s=s+1
}


Pivot_Indice=Datos_Escalados|> select(-c(Estabilidad,Variabilidad,Estabilidad_Normalizada,Variabilidad_Normalizada,Indice))|>
  pivot_wider(names_from = Genérico,
              values_from = Indice_Competitivo)

openxlsx::write.xlsx(Pivot_Indice,"Pivot_INPC_Op2.xlsx",overwrite = T)
