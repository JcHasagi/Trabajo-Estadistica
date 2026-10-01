# ============================================================================
# FASE 3 - BLOQUE 1: DESCRIPTIVOS
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: pegar bloque por bloque en la consola de RStudio (Ctrl + 2).
# Insumo: salidas/base_directorio_mes.rds (generado por 01b_construccion_estilo_docente.R)
# ============================================================================


# ---------------------------------------------------------------------------
# 1. CARGA
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# RUTAS RELATIVAS: el directorio de trabajo debe ser la carpeta del trabajo,
# la que contiene "salidas/" y las carpetas de mes. En RStudio, abrir el
# proyecto ahi, o hacer setwd() una vez antes de correr esto.
# ---------------------------------------------------------------------------
options(scipen = 999)   # sin notacion cientifica: 400000, nunca 4e+05.
                        # Se fija aqui para que la salida no dependa del
                        # estado de la sesion ni del orden de ejecucion.

RUTA        <- "."
RUTA_SALIDA <- file.path(RUTA, "salidas")
if (!dir.exists(RUTA_SALIDA))
  stop("No encuentro la carpeta 'salidas/' desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R.")

base_directorio_mes <- readRDS(file.path(RUTA_SALIDA, "base_directorio_mes.rds"))
d <- base_directorio_mes

cat("filas:", nrow(d), " columnas:", ncol(d), "\n")   # debe decir 72737 x 22


# ---------------------------------------------------------------------------
# 2. FUNCIONES DE ASIMETRIA Y CURTOSIS
#    R base no las trae. Se usan los momentos centrales con denominador n,
#    divididos por sd() (que usa n-1). Con n > 29.000 la diferencia frente a
#    la version totalmente poblacional es inapreciable, pero hay que declarar
#    la convencion en el informe.
#    Referencia: curtosis 3 = normal. Menor que 3 = platicurtica.
# ---------------------------------------------------------------------------
asim <- function(y){ y <- y[!is.na(y)]; mean((y - mean(y))^3) / sd(y)^3 }
curt <- function(y){ y <- y[!is.na(y)]; mean((y - mean(y))^4) / sd(y)^4 }


# ---------------------------------------------------------------------------
# 3. TABLA DE DESCRIPTIVOS DE LAS TRES VARIABLES CUANTITATIVAS
#    Los NA se excluyen del calculo pero se REPORTAN. No se imputan.
# ---------------------------------------------------------------------------
descriptivos <- function(y){
  yv <- y[!is.na(y)]
  q  <- quantile(yv, c(.25, .50, .75), names = FALSE)   # type 7, el de R
  c(n         = length(yv),
    faltantes = sum(is.na(y)),
    media     = mean(yv),
    mediana   = q[2],
    desv_est  = sd(yv),
    minimo    = min(yv),
    Q1        = q[1],
    Q3        = q[3],
    maximo    = max(yv),
    asimetria = asim(yv),
    curtosis  = curt(yv))
}

num <- c("prop_educ_superior_18mas",
         "prop_mujeres_adultas",
         "arriendo_mensual_vivienda")

tabla_desc <- t(sapply(d[num], descriptivos))

# Impresion legible: proporciones con 4 decimales, dinero con 2.
cat("\n=== DESCRIPTIVOS - V1 y V2 (proporciones) ===\n")
print(round(tabla_desc[1:2, ], 4))

cat("\n=== DESCRIPTIVOS - V3 (pesos) ===\n")
print(format(round(tabla_desc[3, ], 2), big.mark = ".", decimal.mark = ",",
             scientific = FALSE), quote = FALSE)


# ---------------------------------------------------------------------------
# 4. VARIABLE CUALITATIVA V4: FRECUENCIAS
#    A una cualitativa no se le calcula media ni desviacion. Lo que se
#    reporta es la tabla de frecuencias y la moda.
# ---------------------------------------------------------------------------
cat("\n=== V4 presencia_analfabetismo ===\n")
fa <- table(d$presencia_analfabetismo, useNA = "ifany")   # frecuencia absoluta
fr <- prop.table(fa)                                      # frecuencia relativa

tabla_v4 <- cbind(frec_absoluta = as.vector(fa),
                  frec_relativa = round(as.vector(fr), 6),
                  porcentaje    = round(100 * as.vector(fr), 2))
rownames(tabla_v4) <- names(fa)
print(tabla_v4)

cat("moda:", names(fa)[which.max(fa)], "\n")
cat("total:", sum(fa), "\n")


# ---------------------------------------------------------------------------
# 5. POR QUE V1 SE VE COMO SE VE: TABLA DE FRECUENCIAS Y DENOMINADOR
#    V1 solo toma 31 valores distintos porque su denominador es el numero
#    de adultos de la vivienda, casi siempre 1 o 2.
# ---------------------------------------------------------------------------
cat("\n=== V1: valores mas frecuentes ===\n")
f1 <- sort(table(round(d$prop_educ_superior_18mas, 4)), decreasing = TRUE)
tabla_v1 <- cbind(viviendas  = as.vector(f1),
                  porcentaje = round(100 * as.vector(f1) / sum(f1), 2))
rownames(tabla_v1) <- names(f1)
print(head(tabla_v1, 8))
cat("valores distintos que toma V1:", length(f1), "\n")

cat("\n=== V1: adultos por vivienda (el denominador) ===\n")
print(table(d$den_educ[!is.na(d$prop_educ_superior_18mas)]))


# ---------------------------------------------------------------------------
# 6. EXPORTAR PARA PEGAR EN EL WORD
# ---------------------------------------------------------------------------
write.csv2(round(tabla_desc, 6),
           file.path(RUTA_SALIDA, "fase3_descriptivos.csv"),
           row.names = TRUE, fileEncoding = "UTF-8")
write.csv2(tabla_v4,
           file.path(RUTA_SALIDA, "fase3_frecuencias_v4.csv"),
           row.names = TRUE, fileEncoding = "UTF-8")
cat("\nexportado a salidas/fase3_descriptivos.csv y fase3_frecuencias_v4.csv\n")
