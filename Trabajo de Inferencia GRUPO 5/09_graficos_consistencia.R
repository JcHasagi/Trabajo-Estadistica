# ============================================================================
# FASE 3 - GRAFICOS DE CONSISTENCIA
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: haber corrido 08_estimadores_simulacion.R, que deja las
#             replicas en salidas/fase3_distribuciones.rds. R base, sin paquetes.
# Modo de uso: source("09_graficos_consistencia.R")
# Salida: 4 PNG en salidas/graficos/ (archivos 17 a 20)
#
# TIEMPO: unos segundos. NO vuelve a simular.
#
# QUE HACE Y POR QUE
# Lee las replicas que guardo el script 08 en salidas/fase3_distribuciones.rds
# y dibuja, para cada variable, la distribucion de la media muestral con
# n = N (el conjunto analitico completo, con reemplazo) para B = 100, 1.000 y
# 10.000 replicas, un panel por B. Como lee el mismo archivo del que salen
# las tablas del 08, graficos y tablas coinciden sin necesidad de semilla (la
# seccion 2 lo comprueba).
#
# DECISIONES DE FORMA
# - Los tres paneles comparten el MISMO eje horizontal, los mismos intervalos
#   del histograma y el mismo eje vertical. Lo unico que cambia entre paneles
#   es B, de modo que lo que se ve es el efecto de B y nada mas.
# - El eje vertical es la PROPORCION de replicas en cada intervalo, no el
#   conteo: con conteos el panel de B = 10.000 seria cien veces mas alto que
#   el de B = 100 y no se podrian comparar.
# - La curva punteada es la normal con media mu y desviacion sigma/raiz(N)
#   (en V4, p y raiz(p(1-p)/N)), escalada al ancho del intervalo: la
#   distribucion a la que debe acercarse el histograma al crecer B. El ancho
#   NO cambia con B: con N fijo, la desviacion es la misma en los tres paneles.
# - El tamano de la imagen (1800 x 1250) es el de los graficos que ya estan en
#   el informe, para poder sustituirlos sin deformarlos.
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
ARCH_SIM    <- file.path(RUTA_SALIDA, "fase3_distribuciones.rds")
if (!file.exists(ARCH_SIM))
  stop("No encuentro ", ARCH_SIM, " desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 08_estimadores_simulacion.R, que guarda las replicas.")

SIM   <- readRDS(ARCH_SIM)
DIST  <- SIM$distribuciones
PARAM <- SIM$parametros
RES   <- SIM$resumen
BS    <- SIM$BS

RUTA_G <- file.path(RUTA_SALIDA, "graficos")
dir.create(RUTA_G, showWarnings = FALSE, recursive = TRUE)

TITULO <- c(
  V1 = "V1: distribucion de la media muestral con n = N (prop. con educacion superior)",
  V2 = "V2: distribucion de la media muestral con n = N (prop. de mujeres adultas)",
  V3 = "V3: distribucion de la media muestral con n = N (log del arriendo)",
  V4 = "V4: distribucion de la proporcion muestral con n = N (presencia de analfabetismo)")
EJEX <- c(V1 = "Media muestral", V2 = "Media muestral",
          V3 = "Media muestral del logaritmo", V4 = "Proporcion muestral")
ARCH <- c(V1 = "17_consistencia_v1.png", V2 = "18_consistencia_v2.png",
          V3 = "19_consistencia_v3.png", V4 = "20_consistencia_v4.png")

BARRA <- "#8C8C8C"
PARAM_COL <- "#D55E00"   # bermellon: el parametro poblacional
CURVA <- "#0072B2"       # azul: la normal teorica
EJE   <- "#4D4D4D"

coma <- function(z, dig) formatC(z, format = "f", digits = dig,
                                 big.mark = ".", decimal.mark = ",")
miles <- function(z) formatC(z, format = "d", big.mark = ".", decimal.mark = ",")


# ---------------------------------------------------------------------------
# 1. UN GRAFICO POR VARIABLE, UN PANEL POR B, EJES COMPARTIDOS
# ---------------------------------------------------------------------------
for (v in names(DIST)) {
  p  <- PARAM[PARAM$variable == v, ]
  MU <- p$media; N <- p$N
  EE <- RES$ee_teorico[RES$variable == v][1]   # error estandar del IC (08)

  # Eje horizontal comun: todas las replicas y, como minimo, mu +- 4 EE.
  todos  <- unlist(lapply(DIST[[v]], function(r) r$media))
  rango  <- range(c(todos, MU + c(-4, 4) * EE))
  cortes <- seq(rango[1], rango[2], length.out = 41)   # 40 intervalos iguales
  ancho  <- diff(cortes)[1]

  # Eje vertical comun: el mayor de los tres histogramas o de la curva.
  prop <- unlist(lapply(DIST[[v]], function(r)
    hist(r$media, breaks = cortes, plot = FALSE)$counts / length(r$media)))
  ymax <- 1.08 * max(c(prop, dnorm(0, 0, EE) * ancho))

  png(file.path(RUTA_G, ARCH[[v]]), width = 1800, height = 1250, res = 165)
  op <- par(mfrow = c(length(BS), 1), oma = c(3.6, 1, 4.6, 1),
            mar = c(2.1, 5.2, 2.2, 1), col.axis = EJE, col.lab = EJE,
            las = 1, mgp = c(3.7, 0.6, 0))

  for (B in BS) {
    ms <- DIST[[v]][[paste0("B_", B)]]$media
    h  <- hist(ms, breaks = cortes, plot = FALSE)
    h$counts <- h$counts / B                 # proporcion de replicas
    plot(h, freq = TRUE, xlim = rango, ylim = c(0, ymax),
         col = BARRA, border = "white", main = "",
         xlab = "", ylab = "Prop. de replicas", xaxt = "n", yaxt = "n",
         cex.lab = 0.9)
    title(main = sprintf("B = %s replicas", miles(B)),
          font.main = 1, cex.main = 1.05, line = 0.5, adj = 0)
    # Eje X con coma decimal, para no mezclar punto y coma en el mismo panel.
    ax <- axTicks(1)
    axis(1, at = ax, labels = format(ax, decimal.mark = ",", trim = TRUE),
         cex.axis = 0.8)
    ay <- axTicks(2)
    axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",", trim = TRUE),
         cex.axis = 0.8)
    curve(dnorm(x, MU, EE) * ancho, from = rango[1], to = rango[2], n = 400,
          add = TRUE, col = CURVA, lwd = 1.8, lty = 2)
    abline(v = MU, col = PARAM_COL, lwd = 2)
    # Lo que se lee en la tabla de consistencia, escrito en el propio panel.
    mtext(sprintf("promedio = %s   desv. est. = %s",
                  coma(mean(ms), 6), coma(sd(ms), 6)),
          side = 3, line = 0.5, cex = 0.72, col = EJE, adj = 1)
  }

  # Subtitulo con simbolos matematicos (plotmath): mu, sigma, raiz.
  # Las llaves { } agrupan sin dibujarse.
  if (v == "V4") {
    sub_txt <- bquote("n = N = " * .(miles(N)) * " viviendas en cada replica, con reemplazo.  " *
                      "Linea vertical: " * {p == .(coma(MU, 6))} *
                      ".  Curva punteada: normal con media " * p * " y desv. est. " *
                      {sqrt(p(1 - p) / N) == .(coma(EE, 6))} * ".")
  } else {
    sub_txt <- bquote("n = N = " * .(miles(N)) * " viviendas en cada replica, con reemplazo.  " *
                      "Linea vertical: " * {mu == .(coma(MU, 6))} *
                      ".  Curva punteada: normal con media " * mu * " y desv. est. " *
                      {sigma / sqrt(N) == .(coma(EE, 6))} * ".")
  }
  mtext(TITULO[[v]], side = 3, outer = TRUE, line = 2.7, cex = 1.0)
  mtext(sub_txt, side = 3, outer = TRUE, line = 0.9, cex = 0.66, col = EJE)
  mtext(EJEX[[v]], side = 1, outer = TRUE, line = 1.0, cex = 0.9, col = EJE)
  mtext("Los tres paneles comparten ejes e intervalos: solo cambia B. El ancho no cambia porque N esta fijo; lo que mejora es el parecido del histograma con la curva.",
        side = 1, outer = TRUE, line = 2.4, cex = 0.62, col = EJE)
  par(op)
  dev.off()
  cat("escrito:", ARCH[[v]], "\n")
}


# ---------------------------------------------------------------------------
# 2. CONTROL: los graficos usan las mismas replicas que las tablas del 08
#    La desviacion de las replicas dibujadas debe coincidir con de_media de
#    salidas/fase3_simulacion.csv. Ademas debe ser parecida en los tres B (se
#    estabiliza, no baja) y cercana al error estandar teorico, el del IC.
# ---------------------------------------------------------------------------
ctrl <- data.frame()
for (v in names(DIST)) for (B in BS) {
  ms <- DIST[[v]][[paste0("B_", B)]]$media
  ctrl <- rbind(ctrl, data.frame(
    variable     = v,
    B            = B,
    de_grafico   = sd(ms),
    de_tabla     = RES$de_media[RES$variable == v & RES$B == B],
    ee_teorico   = RES$ee_teorico[RES$variable == v & RES$B == B],
    row.names    = NULL))
}
ctrl$dif_relativa <- abs(ctrl$de_grafico - ctrl$de_tabla) / ctrl$de_tabla
cat("\nDesviacion estandar de las replicas de la media (grafico frente a tabla):\n")
print(format(ctrl, digits = 6), row.names = FALSE)
if (any(ctrl$dif_relativa > 1e-10))
  stop("Las replicas del .rds no coinciden con su propio resumen. Vuelve a correr el 08.")
cat("\nControl superado: los graficos dibujan las replicas de las tablas del 08.\n")
cat("Replicas generadas el", format(SIM$fecha, "%Y-%m-%d %H:%M"), "con", SIM$version_R, "\n")
