# ============================================================================
# FASE 3 - GRAFICOS DE V4: presencia_analfabetismo
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: source() completo, o pegar bloque por bloque en la consola.
# Salida: 3 PNG en salidas/graficos/ (numerados 14 a 16)
#
# NOTA DE FORMA - por que estos graficos y no otros:
# V4 es CUALITATIVA BINARIA: no tiene histograma, ni media, ni cuartiles. Lo
# unico que se puede graficar de ella es la frecuencia de sus dos categorias.
# Por eso van solo tres laminas: (1) las dos categorias, (2) la estabilidad
# entre los tres meses -que sirve de control de calidad-, y (3) el efecto
# mecanico del tamano del hogar, que es la advertencia mas importante al
# interpretarla. Nada de graficos de torta: comparar angulos es peor que
# comparar longitudes, y con dos categorias la barra ya lo dice todo.
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
if (!dir.exists(RUTA_SALIDA))
  stop("No encuentro la carpeta 'salidas/' desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R.")

base_directorio_mes <- readRDS(file.path(RUTA_SALIDA, "base_directorio_mes.rds"))
d <- base_directorio_mes

RUTA_G <- file.path(RUTA_SALIDA, "graficos")
dir.create(RUTA_G, showWarnings = FALSE, recursive = TRUE)

# Orden fijo de las categorias en los tres graficos: primero "Sin", luego "Con".
NIV <- c("Sin personas analfabetas", "Con al menos una persona analfabeta")
v4  <- factor(d$presencia_analfabetismo, levels = NIV)
cat("observaciones:", sum(!is.na(v4)), " (debe decir 72737)\n")
print(table(v4))

EJE  <- "#4D4D4D"
CLARO  <- "#DEEBF7"   # "Sin": la categoria mayoritaria, en claro
OSCURO <- "#08519C"   # "Con": la categoria de interes, en oscuro
MEDIO  <- "#4292C6"

# Etiquetas cortas para los ejes, para que no se salgan de la lamina.
CORTAS <- c("Sin personas\nanalfabetas", "Con al menos una\npersona analfabeta")


# ---------------------------------------------------------------------------
# 1. GRAFICO PRINCIPAL: LAS DOS CATEGORIAS
#    Frecuencia absoluta en las barras y relativa encima. Las dos cifras
#    juntas porque el informe pide las dos.
# ---------------------------------------------------------------------------
tt  <- table(v4)
pct <- prop.table(tt) * 100

png(file.path(RUTA_G, "14_v4_barras.png"), width = 1500, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.8, 1.6, 0))
bp <- barplot(as.vector(tt), col = c(CLARO, OSCURO), border = "white",
              space = 0.6, width = 0.7, names.arg = CORTAS,
              ylim = c(0, max(tt) * 1.15), yaxt = "n",
              main = "V4: presencia de analfabetismo en la vivienda",
              font.main = 1, cex.main = 1.05,
              xlab = "", ylab = "Viviendas", cex.names = 0.85)
ay <- pretty(c(0, max(tt)))
axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",", trim = TRUE))
text(bp, as.vector(tt),
     paste0(format(as.vector(tt), big.mark = ".", decimal.mark = ",", trim = TRUE), "\n",
            format(round(as.vector(pct), 2), decimal.mark = ",", trim = TRUE), " %"),
     pos = 3, cex = 0.85, col = EJE)
mtext("72.737 viviendas, sin ningun faltante. p estimado = 0,188226.",
      side = 3, line = 0.3, cex = 0.8, col = EJE)
dev.off()


# ---------------------------------------------------------------------------
# 2. ESTABILIDAD ENTRE LOS TRES MESES
#    Control de calidad: mayo, junio y julio son tres muestras independientes
#    (DIRECTORIO no se traslapa). Si la proporcion se moviera mucho entre
#    ellas habria que sospechar de la construccion. No se mueve.
# ---------------------------------------------------------------------------
mes <- factor(d$MES_NOMBRE, levels = c("Mayo", "Junio", "Julio"))
tm  <- table(v4, mes)
pm  <- prop.table(tm, margin = 2) * 100

png(file.path(RUTA_G, "15_v4_por_mes.png"), width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 9), col.axis = EJE, col.lab = EJE, las = 1, xpd = TRUE,
    mgp = c(4.6, 0.9, 0))
bp <- barplot(pm, col = c(CLARO, OSCURO), border = "white", space = 0.5,
              main = "V4 por mes: las tres muestras se comportan igual",
              font.main = 1, cex.main = 1.05,
              xlab = "Mes de recoleccion", ylab = "Porcentaje de viviendas",
              yaxt = "n")
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), " %"))
legend(max(bp) + 0.75, 100, bty = "n", fill = c(CLARO, OSCURO), border = "white",
       cex = 0.8, text.col = EJE, legend = c("Sin analfabetas", "Con al menos una"))
# El porcentaje de la categoria de interes, escrito dentro de su franja.
text(bp, pm[1, ] + pm[2, ] / 2,
     paste0(format(round(pm[2, ], 2), decimal.mark = ",", trim = TRUE), " %"),
     cex = 0.8, col = "white")
text(bp, 100, paste0("n = ", format(colSums(tm), big.mark = ".", decimal.mark = ",", trim = TRUE)),
     pos = 3, cex = 0.72, col = EJE)
par(xpd = FALSE)
dev.off()


# ---------------------------------------------------------------------------
# 3. LA ADVERTENCIA: EFECTO MECANICO DEL TAMANO DEL HOGAR
#    V4 se define por PRESENCIA, asi que entre mas personas tenga la vivienda
#    mas probable es que alguna sea analfabeta, aunque la tasa individual sea
#    la misma. Esto es aritmetica, no un hallazgo social, y hay que decirlo al
#    interpretar la variable.
# ---------------------------------------------------------------------------
tam <- ifelse(d$n_personas >= 6, "6 o mas", as.character(d$n_personas))
tam <- factor(tam, levels = c("1", "2", "3", "4", "5", "6 o mas"))
tp  <- table(v4, tam)
p_con <- prop.table(tp, margin = 2)[2, ] * 100

png(file.path(RUTA_G, "16_v4_por_tamano_hogar.png"),
    width = 1600, height = 1000, res = 180)
par(mar = c(5.5, 7, 4.5, 2), col.axis = EJE, col.lab = EJE, las = 1,
    mgp = c(4.6, 0.9, 0))
bp <- barplot(p_con, col = MEDIO, border = "white", space = 0.45,
              ylim = c(0, 60), yaxt = "n",
              main = "Efecto mecanico del tamano de la vivienda",
              font.main = 1, cex.main = 1.05,
              xlab = "Personas en la vivienda",
              ylab = "Viviendas con al menos una persona analfabeta")
axis(2, at = seq(0, 60, 10), labels = paste0(seq(0, 60, 10), " %"))
text(bp, p_con, paste0(format(round(p_con, 1), decimal.mark = ",", trim = TRUE), " %"),
     pos = 3, cex = 0.8, col = EJE)
text(bp, 0, paste0("n = ", format(colSums(tp), big.mark = ".", decimal.mark = ",", trim = TRUE)),
     pos = 3, cex = 0.68, col = "white")
mtext("De 6,6 % a 53,5 %: es aritmetica de la regla de presencia, no un hallazgo social.",
      side = 3, line = 0.3, cex = 0.8, col = EJE)
dev.off()


cat("\nListo. 3 archivos PNG nuevos en:", RUTA_G, "\n")
print(list.files(RUTA_G))

# Cifras que respaldan los graficos (por si se quieren citar en el texto):
cat("\nFrecuencias de V4:\n");        print(tt); print(round(pct, 4))
cat("\nPorcentaje por mes:\n");       print(round(pm, 2))
cat("\nPorcentaje por tamano:\n");    print(round(p_con, 1))
