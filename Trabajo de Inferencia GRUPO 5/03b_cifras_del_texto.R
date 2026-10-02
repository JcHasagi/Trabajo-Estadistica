# ============================================================================
# FASE 3 - BLOQUE 1b: CIFRAS QUE EL INFORME CITA EN LOS COMENTARIOS
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: pegar bloque por bloque en la consola de RStudio (Ctrl + 2),
# o source("03b_cifras_del_texto.R"), despues de 03_descriptivos_fase3.R.
# Insumo: salidas/base_directorio_mes.rds (generado por 01b_construccion_estilo_docente.R)
# Salida: salidas/fase3_cifras_del_texto.csv y la misma lista en consola.
#
# POR QUE EXISTE: cada cifra de los comentarios del informe debe salir de una
# linea de codigo. Las que no imprimen 01b, 03 ni 07 se calculan aqui, una
# por fila, con la variable y el apartado del informe donde se citan. La
# columna "en_el_informe" copia la cifra tal como esta escrita en el informe
# entregado, para comparar de un vistazo con la calculada.
#
# NO estan aqui:
#  - las de la simulacion (sesgos, varianzas, ECM, razones) ni las de sigma^2:
#    las calcula 08_estimadores_simulacion.R y quedan en
#    salidas/fase3_valores_informe.csv;
#  - las que necesitan la tabla de personas o la de hogares (81 adultos con
#    P3039 = 3 o 4, 25.608 viviendas de hombre y mujer, 20.615 de jefe(a) y
#    pareja, 3.212 de jefe(a) e hijo(a), 5.899, 50.971, 41 hogares con 98 o
#    99, 29.816 hogares en arriendo, 175 viviendas con dos o mas hogares
#    arrendatarios y la suma de FEX_C18 por mes): las imprime
#    01b_construccion_estilo_docente.R;
#  - las que ya imprimen 03 (descriptivos, frecuencias de V1, V2 y V4) y 07
#    (V4 por tamano de la vivienda: 6,6 % y 53,5 %). Los porcentajes de V4
#    por mes, que 07 tambien imprime, se repiten aqui porque el contraste z
#    los usa.
# ============================================================================


# ---------------------------------------------------------------------------
# 1. CARGA
#    Rutas relativas: el directorio de trabajo debe ser la carpeta del
#    trabajo, la que contiene "salidas/".
# ---------------------------------------------------------------------------
options(scipen = 999)   # sin notacion cientifica: 400000, nunca 4e+05.

RUTA        <- "."
RUTA_SALIDA <- file.path(RUTA, "salidas")
if (!file.exists(file.path(RUTA_SALIDA, "base_directorio_mes.rds")))
  stop("No encuentro 'salidas/base_directorio_mes.rds' desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R.")

d <- readRDS(file.path(RUTA_SALIDA, "base_directorio_mes.rds"))


# ---------------------------------------------------------------------------
# 2. TABLA DONDE SE ANOTAN LAS CIFRAS
#    Una fila por cifra: clave, variable, apartado del informe, descripcion,
#    valor calculado, tipo (conteo, pct, pesos o num) y la cifra tal como
#    aparece en el informe. La variable es el prefijo de la clave (V1 a V4).
# ---------------------------------------------------------------------------
cifras <- data.frame(clave = character(0), variable = character(0),
                     apartado = character(0), cifra = character(0),
                     valor = numeric(0), tipo = character(0),
                     en_el_informe = character(0), stringsAsFactors = FALSE)

# anotar() agrega una fila. El <<- escribe en la tabla 'cifras' de arriba,
# que esta fuera de la funcion.
anotar <- function(clave, apartado, cifra, valor, tipo, en_el_informe) {
  cifras[nrow(cifras) + 1, ] <<- list(clave, sub("_.*", "", clave), apartado,
                                      cifra, valor, tipo, en_el_informe)
}


# ---------------------------------------------------------------------------
# 3. V1 - prop_educ_superior_18mas
# ---------------------------------------------------------------------------
v1  <- d$prop_educ_superior_18mas
ok1 <- !is.na(v1)
x1  <- v1[ok1]
anotar("V1_pct_en_0", "Descriptivos basicos y Analisis grafico",
       "% de viviendas con V1 = 0", 100 * mean(x1 == 0), "pct", "53,61 %")
anotar("V1_pct_en_1", "Analisis grafico",
       "% de viviendas con V1 = 1", 100 * mean(x1 == 1), "pct", "21,64 %")
anotar("V1_pct_en_0_o_1", "Analisis grafico",
       "% de viviendas en 0 o en 1", 100 * mean(x1 %in% c(0, 1)), "pct", "75,25 %")
anotar("V1_pct_en_0_5", "Analisis grafico",
       "% de viviendas con V1 = 0,5", 100 * mean(abs(x1 - 0.5) < 1e-9), "pct", "11,79 %")
anotar("V1_valores_distintos", "Variables seleccionadas y Analisis grafico",
       "valores distintos que toma V1", length(unique(round(x1, 6))), "conteo", "31")
# El denominador de V1 es den_educ: adultos con dato en P3042.
anotar("V1_pct_uno_dos_adultos", "Variables seleccionadas y Analisis grafico",
       "% de viviendas con uno o dos adultos (den_educ)",
       100 * mean(d$den_educ[ok1] %in% 1:2), "pct", "72,56 %")
anotar("V1_pct_uno_dos_adultos_si_V1_1", "Analisis grafico",
       "% de las viviendas con V1 = 1 que tienen uno o dos adultos",
       100 * mean(d$den_educ[ok1 & v1 == 1] %in% 1:2), "pct", "88,4 %")
# Media de las razones frente a la razon agregada (cociente de sumas).
anotar("V1_suma_num_educ", "Descriptivos basicos",
       "adultos con educacion superior (suma de num_educ)",
       sum(d$num_educ[ok1]), "conteo", "50.655")
anotar("V1_suma_den_educ", "Descriptivos basicos",
       "adultos con dato (suma de den_educ)", sum(d$den_educ[ok1]), "conteo", "153.741")
anotar("V1_razon_agregada", "Descriptivos basicos",
       "razon agregada suma(num_educ) / suma(den_educ)",
       sum(d$num_educ[ok1]) / sum(d$den_educ[ok1]), "num", "0,329483")


# ---------------------------------------------------------------------------
# 4. V2 - prop_mujeres_adultas
# ---------------------------------------------------------------------------
v2  <- d$prop_mujeres_adultas
ok2 <- !is.na(v2)
x2  <- v2[ok2]
anotar("V2_pct_en_0_5", "Analisis grafico e Insesgamiento",
       "% de viviendas con V2 = 0,5 (la moda)", 100 * mean(abs(x2 - 0.5) < 1e-9),
       "pct", "39,15 %")
anotar("V2_pct_en_1", "Analisis grafico",
       "% de viviendas con V2 = 1", 100 * mean(x2 == 1), "pct", "25,42 %")
anotar("V2_pct_en_0", "Analisis grafico",
       "% de viviendas con V2 = 0", 100 * mean(x2 == 0), "pct", "13,56 %")
anotar("V2_valores_distintos", "Variables seleccionadas y Analisis grafico",
       "valores distintos que toma V2", length(unique(round(x2, 6))), "conteo", "26")
# El bloque de ceros dista de la media lo mismo que la media (0,5625); el de
# unos, 1 - media.
anotar("V2_dist_unos_media", "Descriptivos basicos",
       "distancia del bloque de unos a la media (1 - media)", 1 - mean(x2), "num", "0,4375")
# El denominador de V2 es den_gen: adultos que responden P3039.
dos <- ok2 & d$den_gen == 2
anotar("V2_viv_dos_adultos", "Analisis grafico",
       "viviendas con dos adultos (den_gen = 2)", sum(dos), "conteo", "31.159")
anotar("V2_dos_adultos_una_mujer", "Analisis grafico",
       "de esas, con exactamente una mujer", sum(dos & d$num_muj == 1), "conteo", "25.630")
anotar("V2_pct_dos_adultos_una_mujer", "Analisis grafico",
       "% de las viviendas de dos adultos con exactamente una mujer",
       100 * sum(dos & d$num_muj == 1) / sum(dos), "pct", "82,26 %")
uno <- ok2 & d$den_gen == 1
anotar("V2_pct_unipersonal_mujer", "Descriptivos basicos y Analisis grafico",
       "% de las viviendas de un adulto que son una mujer sola",
       100 * mean(d$num_muj[uno] == 1), "pct", "60,5 %")
anotar("V2_suma_num_muj", "Descriptivos basicos",
       "adultas que se reconocen como mujeres (suma de num_muj)",
       sum(d$num_muj[ok2]), "conteo", "84.505")
anotar("V2_suma_den_gen", "Descriptivos basicos",
       "adultos con dato (suma de den_gen)", sum(d$den_gen[ok2]), "conteo", "153.742")
anotar("V2_razon_agregada", "Descriptivos basicos",
       "razon agregada suma(num_muj) / suma(den_gen)",
       sum(d$num_muj[ok2]) / sum(d$den_gen[ok2]), "num", "0,549655")


# ---------------------------------------------------------------------------
# 5. V3 - arriendo_mensual_vivienda (pesos) y su logaritmo
# ---------------------------------------------------------------------------
x  <- d$arriendo_mensual_vivienda[!is.na(d$arriendo_mensual_vivienda)]
lx <- log(x)
xo <- sort(x, decreasing = TRUE)   # montos de mayor a menor
anotar("V3_cobertura", "Descriptivos basicos",
       "cobertura: % de viviendas con arriendo y monto", 100 * length(x) / nrow(d),
       "pct", "40,65 %")
anotar("V3_media_geometrica", "Descriptivos basicos y Calculo de los estimadores",
       "media geometrica exp(media del log)", exp(mean(lx)), "pesos", "$542.485")

# La cola derecha: no es un atipico, son varios.
anotar("V3_viv_5M_o_mas", "Descriptivos basicos",
       "viviendas que pagan 5 millones o mas", sum(x >= 5e6), "conteo", "64")
anotar("V3_viv_10M_o_mas", "Descriptivos basicos",
       "viviendas que pagan 10 millones o mas", sum(x >= 1e7), "conteo", "19")
anotar("V3_monto_15", "Descriptivos basicos",
       "menor de los 15 montos mas altos", xo[15], "pesos", "12 millones")
anotar("V3_media_sin_max", "Descriptivos basicos",
       "media sin el maximo", mean(xo[-1]), "pesos", "$670.853")
anotar("V3_de_sin_max", "Descriptivos basicos",
       "desviacion estandar sin el maximo", sd(xo[-1]), "pesos", "$2.243.696")
anotar("V3_de_sin_5_mayores", "Descriptivos basicos",
       "desviacion estandar sin los 5 mayores", sd(xo[-(1:5)]), "pesos", "$583.842")
anotar("V3_max_desv_log", "Descriptivos basicos",
       "maximo: desviaciones estandar sobre la media, en escala log",
       (max(lx) - mean(lx)) / sd(lx), "num", "12,08")
# Peso del maximo en la asimetria y la curtosis: la parte de la suma de
# (x - media)^3 y de (x - media)^4 que aporta el termino del maximo. Con las
# cifras de la tabla del informe (media 691.124,60; desv. 4.145.280,28;
# asimetria 117,606176; curtosis 15.678,015509; n 29.565) da 86,9 % y 94,3 %.
anotar("V3_pct_max_en_asimetria", "Descriptivos basicos",
       "aporte del maximo a la suma de (x - media)^3, que define la asimetria",
       100 * (max(x) - mean(x))^3 / sum((x - mean(x))^3), "pct", "87 %")
anotar("V3_pct_max_en_curtosis", "Descriptivos basicos",
       "aporte del maximo a la suma de (x - media)^4, que define la curtosis",
       100 * (max(x) - mean(x))^4 / sum((x - mean(x))^4), "pct", "94 %")

# La cola izquierda: arriendos de cuantia simbolica, en cifras redondas.
anotar("V3_viv_menos_100mil", "Descriptivos basicos",
       "viviendas que pagan menos de $100.000", sum(x < 1e5), "conteo", "107")
anotar("V3_pct_menos_100mil", "Descriptivos basicos",
       "% de viviendas que pagan menos de $100.000", 100 * mean(x < 1e5), "pct", "0,36 %")
for (m in c(20000, 30000, 50000)) {
  anotar(paste0("V3_viv_paga_", m), "Descriptivos basicos",
         paste0("viviendas que pagan exactamente $",
                formatC(m, format = "d", big.mark = ".", decimal.mark = ",")),
         sum(x == m), "conteo", "se cita como valor redondo observado")
}

# Rango tipico en pesos: exp(media del log -/+ desviacion del log).
anotar("V3_rango_LI", "Descriptivos basicos",
       "exp(media del log - desviacion del log)", exp(mean(lx) - sd(lx)), "pesos", "$303.691")
anotar("V3_rango_LS", "Descriptivos basicos",
       "exp(media del log + desviacion del log)", exp(mean(lx) + sd(lx)), "pesos", "$969.044")
anotar("V3_pct_menos_2_5M", "Analisis grafico",
       "% de viviendas que pagan menos de $2.500.000", 100 * mean(x < 2.5e6), "pct", "99 %")

# Media del arriendo que implicaria el modelo lognormal, frente a la observada.
media_lognormal <- exp(mean(lx) + var(lx) / 2)
anotar("V3_media_lognormal", "Calculo de los estimadores",
       "E[arriendo] bajo el modelo lognormal: exp(media del log + varianza del log / 2)",
       media_lognormal, "pesos", "$641.910")
anotar("V3_pct_lognormal_bajo_media", "Calculo de los estimadores",
       "% en que esa E[arriendo] queda por debajo de la media aritmetica",
       100 * (1 - media_lognormal / mean(x)), "pct", "7,1 %")
anotar("V3_pct_igual_mediana", "Insesgamiento y Eficiencia",
       "% de viviendas que pagan exactamente la mediana ($550.000)",
       100 * mean(x == median(x)), "pct", "3,73 %")


# ---------------------------------------------------------------------------
# 6. V4 - presencia_analfabetismo
# ---------------------------------------------------------------------------
y4 <- as.numeric(d$presencia_analfabetismo == "Con al menos una persona analfabeta")
# Contraste de dos proporciones independientes, julio frente a mayo
# (estadistico z con proporcion combinada).
may <- d$MES == "05"; jun <- d$MES == "06"; jul <- d$MES == "07"
p_may <- mean(y4[may]); p_jul <- mean(y4[jul])
p_c   <- mean(y4[may | jul])
z_mj  <- (p_jul - p_may) / sqrt(p_c * (1 - p_c) * (1 / sum(may) + 1 / sum(jul)))
anotar("V4_pct_mayo", "Analisis grafico",
       "% de viviendas con al menos una persona analfabeta, mayo", 100 * p_may,
       "pct", "18,66 %")
anotar("V4_pct_junio", "Analisis grafico",
       "% de viviendas con al menos una persona analfabeta, junio", 100 * mean(y4[jun]),
       "pct", "18,65 %")
anotar("V4_pct_julio", "Analisis grafico",
       "% de viviendas con al menos una persona analfabeta, julio", 100 * p_jul,
       "pct", "19,15 %")
anotar("V4_z_julio_mayo", "Analisis grafico",
       "estadistico z de la diferencia julio - mayo", z_mj, "num", "aproximadamente 1,4")
anotar("V4_valor_p_julio_mayo", "Analisis grafico",
       "valor p bilateral de ese z", 2 * pnorm(-abs(z_mj)), "num",
       "no se cita; respalda que no se rechaza la igualdad al 5 %")
# Correlacion punto-biserial = correlacion de Pearson con la binaria en 0/1.
anotar("V4_cor_V1_V4", "Descriptivos basicos",
       "correlacion punto-biserial entre V1 y V4",
       cor(d$prop_educ_superior_18mas, y4, use = "complete.obs"), "num", "-0,153")
# Por que la regla es de PRESENCIA y no de mayoria: con mayoria estricta
# (mas de la mitad de las personas de 3 anios o mas son analfabetas) casi
# ninguna vivienda caeria en la categoria de interes, y los empates exigirian
# una regla de desempate.
anotar("V4_pct_mayoria_estricta", "Sintaxis de V4 (comentario del codigo)",
       "% de viviendas en que mas de la mitad de las personas de 3 anios o mas es analfabeta",
       100 * mean(d$n_no_alfab > d$den_alfab / 2), "pct", "2,4 % (y 97,6 % en la otra categoria)")
anotar("V4_viv_empate_mayoria", "Sintaxis de V4 (comentario del codigo)",
       "viviendas con empate exacto (la mitad analfabeta)",
       sum(d$n_no_alfab == d$den_alfab / 2), "conteo", "no se cita; son las que exigen desempate")


# ---------------------------------------------------------------------------
# 7. IMPRESION Y EXPORTACION
#    valor_texto: la cifra calculada con coma decimal y punto de miles, en la
#    forma en que la escribe el informe (conteos enteros, porcentajes con dos
#    decimales, pesos redondeados y lo demas con seis decimales).
# ---------------------------------------------------------------------------
texto_cifra <- function(valor, tipo) {
  if (is.na(valor)) return("no definida")
  switch(tipo,
    conteo = formatC(round(valor), format = "d", big.mark = ".", decimal.mark = ","),
    pct    = paste(formatC(valor, format = "f", digits = 2, decimal.mark = ","), "%"),
    pesos  = paste0("$", formatC(round(valor), format = "d", big.mark = ".",
                                 decimal.mark = ",")),
    formatC(valor, format = "f", digits = 6, big.mark = ".", decimal.mark = ","))
}
cifras$valor_texto <- mapply(texto_cifra, cifras$valor, cifras$tipo)

cat("\n=== CIFRAS DE LOS COMENTARIOS DEL INFORME ===\n")
for (v in unique(cifras$variable)) {
  cat("\n---", v, "---\n")
  z <- cifras[cifras$variable == v, ]
  cat(sprintf("  %-31s %s [%s]\n  %-31s calculado: %-14s | en el informe: %s\n",
              z$clave, z$cifra, z$apartado, "", z$valor_texto, z$en_el_informe),
      sep = "")
}

write.csv2(cifras[, c("clave", "variable", "apartado", "cifra", "valor",
                      "valor_texto", "en_el_informe")],
           file.path(RUTA_SALIDA, "fase3_cifras_del_texto.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
cat("\nexportado a salidas/fase3_cifras_del_texto.csv (", nrow(cifras), " cifras)\n", sep = "")
