#Ya tenemos ambas propuestas

#Doble dimensional (mean y sd [y a mi gusto mejor]):
Pivot_MySd=readxl::read_xlsx("Pivot_INPC_Op1.xlsx")

#Unidimensional (Indice = 0.5scale(mean) + 0.5scale(sd))
Pivot_Indice=readxl::read_xlsx("Pivot_INPC_Op2.xlsx")

#Calculo de Ubicuidad

#Doble
Ubicuidad_MySd=vector("list",(ncol(Pivot_MySd)-1))
names(Ubicuidad_MySd)=colnames(Pivot_MySd[-1])
for(producto in colnames(Pivot_MySd[-1])){
  n=which(names(Ubicuidad_MySd)==producto)
  Ubicuidad_MySd[[n]]=Pivot_MySd$Ciudad[Pivot_MySd[[producto]]==1]
}

#Uni
Ubicuidad_Indice=vector("list",(ncol(Pivot_Indice)-1))
names(Ubicuidad_Indice)=colnames(Pivot_Indice[-1])
for(producto in colnames(Pivot_Indice[-1])){
  n=which(names(Ubicuidad_Indice)==producto)
  Ubicuidad_Indice[[n]]=Pivot_Indice$Ciudad[Pivot_Indice[[producto]]==1]
}

Comparación=as.data.frame(cbind(colnames(Pivot_Indice[-1])))
Comparación$MySd=numeric(nrow(Comparación))
Comparación$Indice=numeric(nrow(Comparación))
colnames(Comparación)[1]="Genérico"
for(i in 1:nrow(Comparación)){
  Comparación[i,2]=length(Ubicuidad_MySd[[which(names(Ubicuidad_MySd)==Comparación$Genérico[i])]])
  Comparación[i,3]=length(Ubicuidad_Indice[[which(names(Ubicuidad_Indice)==Comparación$Genérico[i])]])
}




(3*exp(3/2)-exp(7/2)- sqrt(exp(7)-18*exp(5)+42*exp(4)-27*exp(3)))/(12-6*exp(1))
(3*exp(3/2)-exp(7/2)+ sqrt(exp(7)-18*exp(5)+42*exp(4)-27*exp(3)))/(12-6*exp(1))


(exp(1/2) - 1.485847)/(7.642596-1.485847)
1-0.026455

((7.642596)^(-1))+((1.485847)^(-1))
((7.642596)^(-1))*((1.485847)^(-1))
((0.026455)*(1.485847)^(-1))+((0.973545)*(7.642596)^(-1))


0.803862 - 9
0.088061 - 9*(0.145189)


# Aproximar raíz de f(x) en [0, 3]
f <- function(x)  x^2 - 8.196138*x -1.21864
solucion <- uniroot(f, interval = c(-10, 8.196138/2))
solucion$root # Retorna 2
(-0.146081)^2 -8.196138*(-0.146081) -1.21864

# Aproximar raíz de f(x) en [0, 3]
f <- function(x)  x^2 - 8.196138*x -1.21864
solucion <- uniroot(f, interval = c(8.196138/2,14))
solucion$root # Retorna 2


(7.642596)*(0.05319)+(1.485847)*(0.66179)
(7.642596)*(-0.19014)+(1.485847)*(-7.49164)


u=10
(1.38983*exp(0.146081*u)) - (12.58459*exp(-8.342237*u))
