# ============================================================================
# FASE 3 - GRAFICOS DE V3 Y DE SU LOGARITMO
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: pegar bloque por bloque en la consola de RStudio (Ctrl + 2).
# Insumo: salidas/base_directorio_mes.rds (generado por 01b_construccion_estilo_docente.R)
# Salida: PNG en salidas/graficos/
#
# Paleta: escala de grises para las barras y dos colores de la paleta
# Okabe-Ito para las lineas de referencia. Son distinguibles con daltonismo
# y, por si acaso, van ademas diferenciadas por tipo de linea (solida /
# punteada) y por la leyenda. No se usa rojo-verde.
# ============================================================================


# ---------------------------------------------------------------------------
# 0. PREPARACION
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
if (!file.exists(file.path(RUTA_SALIDA, "base_directorio_mes.rds")))
  stop("No encuentro 'salidas/base_directorio_mes.rds' desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R.")

base_directorio_mes <- readRDS(file.path(RUTA_SALIDA, "base_directorio_mes.rds"))
d <- base_directorio_mes

RUTA_G <- file.path(RUTA_SALIDA, "graficos")
dir.create(RUTA_G, showWarnings = FALSE, recursive = TRUE)

v  <- d$arriendo_mensual_vivienda[!is.na(d$arriendo_mensual_vivienda)]
lv <- d$log_arriendo_mensual[!is.na(d$log_arriendo_mensual)]
cat("observaciones:", length(v), " (debe decir 29565)\n")

# Rotulos calculados, no escritos a mano: si cambia la base, cambian solos.
# Asimetria y curtosis con la misma convencion del script 03.
asim <- function(y){ y <- y[!is.na(y)]; mean((y - mean(y))^3) / sd(y)^3 }
curt <- function(y){ y <- y[!is.na(y)]; mean((y - mean(y))^4) / sd(y)^4 }
num_lab <- function(z, dig) formatC(z, format = "f", digits = dig,
                                    big.mark = ".", decimal.mark = ",")

BARRA  <- "#BDBDBD"   # gris de las barras
BORDE  <- "#FFFFFF"
MEDIA  <- "#D55E00"   # bermellon  - media
MEDIANA<- "#0072B2"   # azul       - mediana
EJE    <- "#4D4D4D"

# Marcas del eje en pesos, para reetiquetar la escala logaritmica.
# No es un segundo eje con otra variable: es el MISMO eje, escrito en las
# unidades originales para que se pueda leer sin calculadora.
# Etiquetas cortas para que no se solapen: "600.000.000" no cabe once veces.
# Marcas espaciadas para que R no descarte rotulos por falta de sitio: antes
# desaparecian '100 mil' y '500 mil', justo donde caen la media geometrica y
# la mediana. Se quitan 250 mil y 2 M, que no aportan, y se baja cex.axis.
marcas <- c(10e3, 5e4, 1e5, 5e5, 1e6, 5e6, 2e7, 1e8, 6e8)
etiq   <- c("10 mil","50 mil","100 mil","500 mil",
            "1 M","5 M","20 M","100 M","600 M")

# Eje Y con separador de miles, para que 30000 se lea 30.000 como en el resto
# del trabajo. R base no lo hace solo en hist().
eje_y_miles <- function() {
  ay <- axTicks(2)
  axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",",
                                   scientific = FALSE, trim = TRUE))
}


# ---------------------------------------------------------------------------
# 1. EL PROBLEMA: HISTOGRAMA EN PESOS
#    Con 600 millones de rango y el 99% de los datos bajo 2,5 millones, todo
#    se apila en la primera barra. Este grafico NO sirve para leer la
#    distribucion: sirve para mostrar por que hace falta el logaritmo.
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "01_hist_arriendo_pesos.png"),
    width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 6.5, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.3, 0.9, 0))
hist(v, breaks = 60, col = BARRA, border = BORDE,
     main = "V3 en pesos: la escala original no deja ver nada",
     xlab = "Arriendo mensual (pesos)", ylab = "Viviendas",
     xaxt = "n", yaxt = "n", cex.main = 1.1, font.main = 1)
eje_y_miles()
axis(1, at = seq(0, 6e8, by = 1e8),
     labels = c("0","100 M","200 M","300 M","400 M","500 M","600 M"))
mtext(sprintf("%s viviendas. La primera barra concentra practicamente todo.",
              num_lab(length(v), 0)),
      side = 3, line = 0.2, cex = 0.8, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 2. LA SOLUCION: HISTOGRAMA DEL LOGARITMO, CON EL EJE ESCRITO EN PESOS
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "02_hist_log_arriendo.png"),
    width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 6.5, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.3, 0.9, 0))
hist(lv, breaks = 45, col = BARRA, border = BORDE,
     main = "V3 en escala logaritmica: aparece la forma",
     xlab = "Arriendo mensual (pesos, escala logaritmica)", ylab = "Viviendas",
     xaxt = "n", yaxt = "n", cex.main = 1.1, font.main = 1)
eje_y_miles()
# R descarta rotulos que cree que se solapan, y asi desaparecia "100 mil".
# Dibujando cada marca en su propia llamada a axis() no puede descartar
# ninguna: la supresion solo actua dentro de una misma llamada.
for (k in seq_along(marcas))
  axis(1, at = log(marcas[k]), labels = etiq[k], cex.axis = 0.78)

abline(v = mean(lv),   col = MEDIA,   lwd = 2)
abline(v = median(lv), col = MEDIANA, lwd = 2, lty = 2)

legend("topright", bty = "n", lwd = 2, lty = c(1, 2),
       col = c(MEDIA, MEDIANA), cex = 0.85, text.col = EJE,
       legend = c(sprintf("media geometrica  $%s",
                          format(round(exp(mean(lv))), big.mark = ".", decimal.mark = ",",
                                 scientific = FALSE, trim = TRUE)),
                  sprintf("mediana           $%s",
                          format(round(exp(median(lv))), big.mark = ".", decimal.mark = ",",
                                 scientific = FALSE, trim = TRUE))))
mtext(sprintf("Asimetria %s y curtosis %s, frente a %s y %s en pesos.",
              num_lab(asim(lv), 2), num_lab(curt(lv), 2),
              num_lab(asim(v), 1), num_lab(curt(v), 0)),
      side = 3, line = 0.2, cex = 0.8, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 3. LAMINA COMPARATIVA (la imagen que justifica la decision ante el docente)
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "03_comparacion_pesos_vs_log.png"),
    width = 2000, height = 900, res = 170)
par(mfrow = c(1, 2), mar = c(5, 6.2, 4, 1.5), col.axis = EJE, col.lab = EJE,
    las = 1, mgp = c(4.2, 0.9, 0))

hist(v, breaks = 60, col = BARRA, border = BORDE,
     main = "Escala original (pesos)", xlab = "Arriendo mensual", ylab = "Viviendas",
     xaxt = "n", yaxt = "n", cex.main = 1, font.main = 1)
eje_y_miles()
axis(1, at = seq(0, 6e8, by = 2e8),
     labels = c("0", "200 M", "400 M", "600 M"), cex.axis = 0.85)

hist(lv, breaks = 45, col = BARRA, border = BORDE,
     main = "Escala logaritmica", xlab = "Arriendo mensual", ylab = "Viviendas",
     xaxt = "n", yaxt = "n", cex.main = 1, font.main = 1)
eje_y_miles()
axis(1, at = log(c(1e4, 1e5, 1e6, 1e7, 1e8)),
     labels = c("10 mil", "100 mil", "1 M", "10 M", "100 M"), cex.axis = 0.85)
dev.off()   # cierra el png; su par(mfrow) se descarta con el dispositivo


# ---------------------------------------------------------------------------
# 4. CAJA Y BIGOTES DEL LOGARITMO, POR MES
#    Sirve para dos cosas: ver los atipicos uno por uno y comprobar que los
#    tres meses se comportan igual.
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "04_boxplot_log_por_mes.png"),
    width = 1500, height = 1000, res = 180)
par(mar = c(4.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.6, 0.9, 0))
sub <- d[!is.na(d$log_arriendo_mensual), ]
boxplot(log_arriendo_mensual ~ MES_NOMBRE, data = sub,
        col = BARRA, border = EJE, outcol = "#00000055", outpch = 20, outcex = 0.5,
        main = "Logaritmo del arriendo por mes", font.main = 1, cex.main = 1.1,
        xlab = "", ylab = "Arriendo mensual (pesos)", yaxt = "n")
axis(2, at = log(marcas), labels = etiq, cex.axis = 0.8)
dev.off()


# ---------------------------------------------------------------------------
# 5. GRAFICO CUANTIL-CUANTIL DEL LOGARITMO
#    Si los puntos siguen la recta, el log es aproximadamente normal.
#    Aqui el centro se pega bien y las dos puntas se despegan: por eso la
#    curtosis queda en 6,97 y no en 3.
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "05_qqnorm_log_arriendo.png"),
    width = 1400, height = 1100, res = 180)
par(mar = c(5.5, 6.2, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.0, 0.9, 0))
qqnorm(lv, pch = 20, cex = 0.3, col = "#00000044",
       main = "Normalidad del logaritmo del arriendo", font.main = 1, cex.main = 1.1,
       xlab = "Cuantiles teoricos de una normal", ylab = "Cuantiles observados (log)")
qqline(lv, col = MEDIA, lwd = 2)
mtext("El centro se ajusta; las dos colas se despegan.",
      side = 3, line = 0.2, cex = 0.8, col = EJE)
dev.off()


cat("\nListo. 5 archivos PNG en:", RUTA_G, "\n")
print(list.files(RUTA_G))
