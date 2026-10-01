# Trabajo-Estadistica

Primer Trabajo de Estadística II: análisis de cuatro variables construidas a partir de la
**Gran Encuesta Integrada de Hogares (GEIH) 2025 del DANE**. Para cada variable se hace la
descripción, se estima la media o la proporción con intervalos de confianza y se evalúan los
estimadores (insesgamiento, consistencia y eficiencia).

## Entregables

- **Informe:** `Primer Trabajo Estadistica II - Fase 3.docx`.
- **Sintaxis en R:** scripts numerados del `00` al `09` que reproducen todos los resultados.
  `00_ejecutar_todo.R` corre la secuencia completa.

La calificación sigue una rúbrica de seis criterios de 5 puntos cada uno: selección de variables,
uso de herramientas y un criterio por cada una de las cuatro variables.

## Datos

- **Fuente:** GEIH del DANE, meses de mayo, junio y julio de 2025. Son tres muestras
  independientes, no un panel.
- **Unidad de análisis:** la vivienda. Son 72.737 viviendas de 33 departamentos.
- Los microdatos no se incluyen en el repositorio. Se generan al ejecutar los scripts sobre los
  archivos originales del DANE.
- No se usa el factor de expansión `FEX_C18` porque expande a personas, no a viviendas.

## Variables

| # | Variable | Qué mide | Tipo | n |
|---|----------|----------|------|---|
| V1 | `prop_educ_superior_18mas` | Proporción de adultos de la vivienda que alcanzaron educación técnica o superior | Cuantitativa (proporción) | 72.649 |
| V2 | `prop_mujeres_adultas` | Proporción de adultos que se reconocen como mujeres | Cuantitativa (proporción) | 72.649 |
| V3 | `arriendo_mensual_vivienda` | Arriendo mensual pagado (solo viviendas arrendadas) | Cuantitativa continua | 29.565 |
| V4 | `presencia_analfabetismo` | Si en la vivienda hay al menos una persona analfabeta | Cualitativa binaria | 72.737 |

V3 tiene una columna auxiliar, `log_arriendo_mensual`, que se agregó por sugerencia del docente:
el arriendo tiene valores extremos y el logaritmo los vuelve manejables. No es una quinta variable.

## Metodología por fases

1. **Fase 1: auditoría y selección.** Se revisaron los archivos del DANE y muchas variables
   candidatas. La mayoría se descartó por baja cobertura o poca variación, o porque el docente
   prohibió usar el ejemplo de clase (ingreso laboral).
2. **Fase 2: fichas y validaciones.** Se hizo una ficha técnica de 12 puntos por variable, se
   definió el modelo de datos y se corrieron comprobaciones de calidad, que pasan todas.
3. **Fase 3: análisis.**
   - Estadísticos descriptivos y 20 gráficos.
   - Estimadores por analogía y por máxima verosimilitud.
   - Intervalos de confianza al 95 %.
   - Simulación con B = 10.000 réplicas (semilla 2026) para comparar la media con la mediana
     como estimadores.
4. **Cierre.**
   - Prueba de reproducibilidad: dos corridas completas comparadas por hash.
   - Revisión contra la rúbrica: se añadió al informe la sintaxis que produce cada resultado y se
     unificó la clasificación de V1 y V2.

## Resultados principales

| Variable | Estimación | IC 95 % | Forma de la distribución |
|----------|-----------|---------|--------------------------|
| V1 | 0,3325 | [0,3296 ; 0,3355] | Bimodal: 54 % de las viviendas en 0 y 22 % en 1 |
| V2 | 0,5625 | [0,5602 ; 0,5648] | Trimodal, con pico en 0,5 (parejas) |
| V3 (log) | 13,2039 → $542.485 | [$538.910 ; $546.085] | Cola larga a la derecha (máximo de $600 millones) |
| V4 | 18,82 % | [18,54 % ; 19,11 %] | El 81 % de las viviendas no tiene personas analfabetas |

- En las cuatro variables, el estimador por analogía y el de máxima verosimilitud coinciden.
- La simulación muestra que la media muestral es **insesgada y consistente**: su varianza cae
  como 1/n.
- **Hallazgo central:** en V1, V2 y V4 la mediana parece "más eficiente" que la media, pero solo
  porque se queda fija en un valor. Frente a la media poblacional, la mediana tiene un sesgo que no
  desaparece al aumentar n.

## Decisiones y advertencias

- **V3 se estima sobre el logaritmo** (confirmado por el docente). Al devolverlo con `exp()` se
  obtiene la media geométrica, que corresponde al arriendo típico ($542.485). La media aritmética
  es otra cifra: $691.125.
- **V1 mide acceso, no graduación:** cuenta a quien cursó estudios técnicos o superiores, tenga o
  no el título. La formación normalista no cuenta.
- **V2 cuenta solo a quien se reconoce como mujer.** No es una comparación de mujeres frente a
  hombres.
- **V3 aplica solo a viviendas arrendadas**, que son el 41 % del total. Los valores extremos se
  conservan y se declaran en el informe.
- **V4 crece con el tamaño del hogar**, de 6,6 % a 53,5 %. Es un efecto aritmético (más personas,
  más probabilidad de que una sea analfabeta), no un hallazgo social.
- **Los intervalos se justifican por el teorema del límite central**, no por normalidad de las
  variables, porque ninguna de las cuatro es normal.

## Cómo reproducir los resultados

1. Copiar los scripts de R en la misma carpeta que contiene los datos del DANE, organizados por mes
   (`Mayo 2025/CSV/` y las carpetas equivalentes de junio y julio).
2. Instalar los paquetes necesarios:

   ```r
   install.packages(c("readr", "dplyr"))
   ```

3. Ejecutar el script maestro:

   ```r
   source("00_ejecutar_todo.R", encoding = "UTF-8")
   ```

La ejecución tarda de 10 a 20 minutos. Al terminar crea la carpeta `salidas/` con las bases
construidas, las tablas y los gráficos.

## Documentación complementaria

- `Memoria_Metodologica_GEIH2025.md`: explica la metodología en lenguaje corriente e incluye
  posibles preguntas del profesor.
- Notas de la Fase 1 y de la Fase 2.
