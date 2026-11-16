# Muestra didactica reproducible de Casen 2020. Ejecutar desde clase_09.
# El archivo original se obtiene de la descarga oficial SPSS revisada en 2022.
# No borra ni modifica el original. No produce estimaciones oficiales.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
library(haven)
origen <- "../../../tmp/casen-clase9/Casen_en_Pandemia_2020_revisada202209.sav"
if (!file.exists(origen)) stop("Indica en origen la ruta al SAV original descomprimido.")
d <- read_sav(origen, col_select = tidyselect::all_of(c("region","comuna","sexo","edad","y1")))
n_objetivo <- 20000L
# La estratificacion conserva aproximadamente la distribucion de region, sexo,
# y ausencia/presencia/nivel de y1. No se rellenan los ingresos ausentes con cero.
y <- as.numeric(d$y1)
cortes <- unique(quantile(y, seq(0,1,.1), na.rm = TRUE))
tramo <- as.character(cut(y, cortes, include.lowest = TRUE, labels = FALSE))
tramo[is.na(y)] <- "sin_y1"
estrato <- interaction(as.numeric(d$region),as.numeric(d$sexo),tramo,drop=TRUE)
grupos <- split(seq_len(nrow(d)),estrato)
nh <- lengths(grupos)
cuota_exacta <- n_objetivo*nh/sum(nh)
cuota <- floor(cuota_exacta)
faltan <- n_objetivo-sum(cuota)
if(faltan>0) {
  orden <- order(cuota_exacta-cuota,decreasing=TRUE)
  cuota[orden[seq_len(faltan)]] <- cuota[orden[seq_len(faltan)]]+1L
}
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(20261116)
filas <- unlist(Map(function(g,k) if(k>0) g[sample.int(length(g),k)] else integer(),grupos,cuota),use.names=FALSE)
filas <- filas[sample.int(length(filas))]
m <- d[filas,]
stopifnot(nrow(m)==n_objetivo,length(unique(filas))==n_objetivo)
dir.create("datos",showWarnings=FALSE)
write_sav(m,"datos/casen2020_clase9_20000.sav",compress="byte")
csv <- as.data.frame(m)
for(v in c("region","comuna","sexo")) csv[[v]] <- as.character(as_factor(csv[[v]]))
csv$edad <- as.numeric(csv$edad)
csv$y1 <- as.numeric(csv$y1)
write.csv(csv,"datos/casen2020_clase9_20000.csv",row.names=FALSE,na="",fileEncoding="UTF-8")
# Auditoria de proporciones sobre las personas de la base, sin ponderar.
dir.create("auditoria_muestra",showWarnings=FALSE)
audit <- do.call(rbind,lapply(c("region","sexo"),function(v){
  niveles <- sort(unique(as.numeric(d[[v]])))
  p <- table(factor(as.numeric(d[[v]]),levels=niveles))/nrow(d)*100
  q <- table(factor(as.numeric(m[[v]]),levels=niveles))/nrow(m)*100
  data.frame(variable=v,codigo=niveles,original_pct=as.numeric(p),muestra_pct=as.numeric(q),diferencia_pp=as.numeric(q-p))
}))
write.csv(audit,"auditoria_muestra/proporciones.csv",row.names=FALSE)
resumen <- function(x)c(n=sum(!is.na(x)),media=mean(x,na.rm=TRUE),mediana=median(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE),q25=quantile(x,.25,na.rm=TRUE,names=FALSE),q75=quantile(x,.75,na.rm=TRUE,names=FALSE),q99=quantile(x,.99,na.rm=TRUE,names=FALSE),max=max(x,na.rm=TRUE))
comparacion <- do.call(rbind,lapply(c("edad","y1"),function(v)data.frame(variable=v,medida=names(resumen(d[[v]])),original=resumen(d[[v]]),muestra=resumen(m[[v]]))))
write.csv(comparacion,"auditoria_muestra/resumenes.csv",row.names=FALSE)
cat("Muestra:",nrow(m),"filas y",ncol(m),"columnas\n")
cat("Mayor diferencia por region/sexo:",max(abs(audit$diferencia_pp)),"puntos porcentuales\n")
cat("Ingresos validos:",sum(!is.na(m$y1)),"\n")
print(comparacion,row.names=FALSE)
print(table(m$region))
cat("Comunas presentes:",length(unique(m$comuna)),"de",length(unique(d$comuna)),"\n")
# Reabrir para verificar formatos, etiquetas y valores ausentes.
recsv <- read.csv("datos/casen2020_clase9_20000.csv",fileEncoding="UTF-8")
resav <- read_sav("datos/casen2020_clase9_20000.sav")
stopifnot(nrow(recsv)==n_objetivo,identical(names(recsv),names(m)),
          identical(recsv$y1,as.numeric(m$y1)),
          identical(as.numeric(resav$region),as.numeric(m$region)),
          identical(attr(resav$region,"labels"),attr(d$region,"labels")))
