# ============================================================================
# FASE 3 - GRAFICOS DE V1: prop_educ_superior_18mas
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: source() completo, o pegar bloque por bloque en la consola.
# Insumo: salidas/base_directorio_mes.rds (generado por 01b_construccion_estilo_docente.R)
# Salida: PNG en salidas/graficos/
#
# NOTA DE FORMA - por que estos graficos y no otros:
# V1 NO es continua. Toma 31 valores distintos y el 75,25% de las viviendas
# esta exactamente en 0 o en 1, porque el denominador es el numero de adultos
# de la vivienda y el 72,56% tiene uno o dos. Un histograma con intervalos
# automaticos sugiere una continuidad que no existe y esconde que 0 y 1 son
# valores puntuales, no rangos. Por eso el grafico principal es de agujas
# (una linea por valor observado, en su posicion real del eje) y el
# histograma (grafico 07), con intervalos fijos y declarados, se produce
# como apoyo; no se incluye en el informe.
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

x <- d$prop_educ_superior_18mas[!is.na(d$prop_educ_superior_18mas)]
cat("observaciones:", length(x), " (debe decir 72649)\n")

BARRA <- "#8C8C8C"
MEDIA <- "#D55E00"   # bermellon
MEDIANA <- "#0072B2" # azul
EJE   <- "#4D4D4D"

# Azules secuenciales: el numero de adultos es una variable ORDINAL,
# asi que su color va de claro a oscuro en un solo tono, no en tonos sueltos.
AZ <- c("#DEEBF7", "#9ECAE1", "#4292C6", "#08519C")

# Rotulos calculados, no escritos a mano: si cambia la base, cambian solos.
pct_lab <- function(p, dig = 1) paste0(formatC(100 * p, format = "f", digits = dig,
                                               decimal.mark = ","), " %")
num_lab <- function(z, dig = 4) formatC(z, format = "f", digits = dig,
                                        big.mark = ".", decimal.mark = ",")


# ---------------------------------------------------------------------------
# 1. GRAFICO PRINCIPAL: AGUJAS EN LOS VALORES REALMENTE OBSERVADOS
#    Cada linea vertical es un valor que V1 puede tomar, en su posicion real
#    del eje. Se ve de una vez que la distribucion es bimodal.
# ---------------------------------------------------------------------------
tab <- table(round(x, 6))
val <- as.numeric(names(tab))
fre <- as.vector(tab)

png(file.path(RUTA_G, "06_v1_agujas.png"), width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.6, 0.9, 0))
plot(val, fre, type = "h", lwd = 4, col = BARRA, xlim = c(-0.03, 1.03),
     ylim = c(0, max(fre) * 1.12), xaxt = "n", yaxt = "n", bty = "n",
     main = "V1: proporcion de adultos con educacion tecnica o superior",
     font.main = 1, cex.main = 1.05,
     xlab = "Proporcion dentro de la vivienda", ylab = "Viviendas")
axis(1, at = seq(0, 1, 0.25), labels = c("0", "0,25", "0,50", "0,75", "1"))
ay <- pretty(c(0, max(fre)))   # con los datos reales: 0 a 40.000 de 10.000 en 10.000
axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",", trim = TRUE))

# Etiquetas directas solo en los dos modos y en el tercer valor: no en todos.
text(0,   fre[val == 0],              pct_lab(mean(x == 0)), pos = 3, cex = 0.8, col = EJE)
text(1,   fre[val == 1],              pct_lab(mean(x == 1)), pos = 3, cex = 0.8, col = EJE)
text(0.5, fre[abs(val - 0.5) < 1e-9], pct_lab(mean(abs(x - 0.5) < 1e-9)),
     pos = 3, cex = 0.8, col = EJE)

mtext(sprintf("%d valores distintos. El %s de las viviendas esta exactamente en 0 o en 1.",
              length(tab), pct_lab(mean(x %in% c(0, 1)))),
      side = 3, line = 0.3, cex = 0.8, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 2. HISTOGRAMA CON INTERVALOS FIJOS DE 0,1
#    Se declara el criterio de corte: intervalos cerrados a la izquierda,
#    [0 - 0,1), [0,1 - 0,2), ..., [0,9 - 1]. Asi el 0 exacto cae en el primero
#    y el 1 exacto en el ultimo, sin ambiguedad.
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "07_v1_histograma.png"), width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.6, 0.9, 0))
h <- hist(x, breaks = seq(0, 1, by = 0.1), right = FALSE, include.lowest = TRUE,
          col = BARRA, border = "white",
          main = "V1: histograma en intervalos de 0,1",
          font.main = 1, cex.main = 1.05,
          xlab = "Proporcion dentro de la vivienda", ylab = "Viviendas",
          xaxt = "n", yaxt = "n")
axis(1, at = seq(0, 1, 0.1), labels = format(seq(0, 1, 0.1), decimal.mark = ","))
ay <- pretty(c(0, max(h$counts)))
axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",", trim = TRUE))
abline(v = mean(x),   col = MEDIA,   lwd = 2)
abline(v = median(x), col = MEDIANA, lwd = 2, lty = 2)
legend("topright", bty = "n", lwd = 2, lty = c(1, 2), col = c(MEDIA, MEDIANA),
       cex = 0.85, text.col = EJE, inset = c(0.02, 0.02),
       legend = c(paste("media", num_lab(mean(x))), paste("mediana", num_lab(median(x)))))
mtext("Intervalos cerrados a la izquierda. La media y la mediana cuentan historias distintas.",
      side = 3, line = 0.3, cex = 0.8, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 3. DISTRIBUCION ACUMULADA EMPIRICA
#    Para una variable discreta este es el grafico mas informativo: cada
#    escalon es un valor y su altura es cuanta masa hay ahi. El salto inicial
#    del 53,6% se ve de inmediato.
# ---------------------------------------------------------------------------
png(file.path(RUTA_G, "08_v1_acumulada.png"), width = 1500, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.6, 0.9, 0))
plot(ecdf(x), verticals = TRUE, do.points = FALSE, lwd = 2.2, col = "#08519C",
     main = "V1: distribucion acumulada", font.main = 1, cex.main = 1.05,
     xlab = "Proporcion dentro de la vivienda",
     ylab = "Proporcion acumulada de viviendas",
     xaxt = "n", yaxt = "n", bty = "n")
axis(1, at = seq(0, 1, 0.25), labels = c("0", "0,25", "0,50", "0,75", "1"))
axis(2, at = seq(0, 1, 0.25), labels = c("0", "0,25", "0,50", "0,75", "1"))
# Misma solucion que en el grafico 12: la linea de referencia cruza todo el
# ancho y la nota va despejada de la curva, no encima de ella.
abline(h = 0.5, col = MEDIANA, lwd = 1.6, lty = 2)
text(0.30, 0.30, sprintf("el salto inicial se lleva el %s:\nla mediana ya queda dentro de el",
                         pct_lab(mean(x == 0))),
     pos = 4, cex = 0.78, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 4. POR QUE SE VE ASI: COMPOSICION SEGUN EL NUMERO DE ADULTOS
#    Barras apiladas al 100%. Muestra que los valores 0 y 1 vienen sobre todo
#    de viviendas de uno o dos adultos, donde no caben valores intermedios.
# ---------------------------------------------------------------------------
sub <- d[!is.na(d$prop_educ_superior_18mas), ]
gr  <- ifelse(sub$den_educ >= 4, "4 o mas", as.character(sub$den_educ))
gr  <- factor(gr, levels = c("1", "2", "3", "4 o mas"))
cat3 <- cut(sub$prop_educ_superior_18mas, breaks = c(-1, 0, 0.999999, 1),
            labels = c("ninguno (0)", "algunos", "todos (1)"))

tt  <- table(cat3, gr)
pct <- prop.table(tt, margin = 2) * 100

png(file.path(RUTA_G, "09_v1_por_numero_adultos.png"),
    width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 9), col.axis = EJE, col.lab = EJE, las = 1, xpd = TRUE,
    mgp = c(4.6, 0.9, 0))
bp <- barplot(pct, col = AZ[c(1, 2, 4)], border = "white", space = 0.45,
              main = "De donde salen los ceros y los unos de V1",
              font.main = 1, cex.main = 1.05,
              xlab = "Adultos en la vivienda", ylab = "Porcentaje de viviendas",
              yaxt = "n")
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), " %"))
legend(max(bp) + 0.9, 100, bty = "n", fill = AZ[c(1, 2, 4)], border = "white",
       cex = 0.85, text.col = EJE, legend = rownames(pct))
text(bp, 100, paste0("n = ", format(colSums(tt), big.mark = ".", decimal.mark = ",", trim = TRUE)),
     pos = 3, cex = 0.72, col = EJE)
par(xpd = FALSE)
dev.off()


cat("\nListo. 4 archivos PNG nuevos en:", RUTA_G, "\n")
print(list.files(RUTA_G))

# Cifras que respaldan el grafico 4 (por si se quieren citar en el texto):
cat("\nComposicion por numero de adultos (%):\n")
print(round(pct, 1))
