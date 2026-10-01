# Trabajo-Estadistica

Primer Trabajo de Estadística II: análisis de cuatro variables construidas a partir de la
**Gran Encuesta Integrada de Hogares (GEIH) 2025 del DANE**. Para cada variable se hace la
descripción, se estima la media o la proporción con intervalos de confianza y se evalúan los
estimadores (insesgamiento, consistencia y eficiencia).

**Grupo 5:** David Ricardo Martínez, Mariana Gómez González y Juan Camilo Arias Sarabia.

## Contenido del repositorio

Todo el trabajo está en la carpeta
[`Trabajo de Inferencia GRUPO 5/`](Trabajo%20de%20Inferencia%20GRUPO%205/):

| Archivo | Qué contiene |
|---------|--------------|
| `Primer_Trabajo_Estadistica_II_-_Fase_3.docx` | El informe final |
| `Instrucciones para ejecucion.txt` | Cómo ejecutar el código, qué produce y qué comprueba |
| `00_ejecutar_todo.R` | Corre los demás scripts en orden. No tiene cálculos propios |
| `01b_construccion_estilo_docente.R` | Lee los CSV del DANE, valida los datos, construye las cuatro variables y exporta las bases |
| `03_descriptivos_fase3.R` | Descriptivos de V1, V2 y V3, y frecuencias de V4 |
| `04_graficos_log_arriendo.R` | Gráficos 1 a 5 (V3 en pesos y en logaritmo) |
| `05_graficos_educacion_superior.R` | Gráficos 6 a 9 (V1) |
| `06_graficos_v2_mujeres.R` | Gráficos 10 a 13 (V2) |
| `07_graficos_v4_analfabetismo.R` | Gráficos 14 a 16 (V4) |
| `08_estimadores_simulacion.R` | Estimadores, intervalos de confianza y simulación |
| `09_graficos_consistencia.R` | Gráficos 17 a 20 (consistencia de la media) |

La calificación sigue una rúbrica de seis criterios de 5 puntos cada uno: selección de variables,
uso de herramientas y un criterio por cada una de las cuatro variables.

## Datos

- **Fuente:** [microdatos de la GEIH 2025 del DANE](https://microdatos.dane.gov.co/index.php/catalog/853),
  meses de mayo, junio y julio. Son tres muestras independientes, no un panel: ningún
  `DIRECTORIO` se repite entre meses.
- **Archivos usados:** de las ocho tablas de cada mes se usan dos, en formato CSV:
  *Características generales, seguridad social en salud y educación* (una fila por persona) y
  *Datos del hogar y la vivienda* (una fila por hogar). En total son seis archivos.
- **Unidad de análisis:** la vivienda, identificada por `PERIODO + MES + DIRECTORIO`. Son
  72.737 viviendas y 204.713 personas de 33 departamentos: 24.188 viviendas en mayo, 24.174 en
  junio y 24.375 en julio.
- Los microdatos no se incluyen en el repositorio. Los scripts construyen las bases a partir de
  los archivos originales del DANE.
- No se usa el factor de expansión `FEX_C18` porque expande a personas, no a viviendas.

## Variables

| # | Variable | Qué mide | Campos GEIH | Tipo | n |
|---|----------|----------|-------------|------|---|
| V1 | `prop_educ_superior_18mas` | Proporción de adultos de la vivienda que alcanzaron educación técnica o superior | `P3042`, `POB_MAY18` | Cuantitativa (proporción) | 72.649 |
| V2 | `prop_mujeres_adultas` | Proporción de adultos que se reconocen como mujeres | `P3039` | Cuantitativa (proporción) | 72.649 |
| V3 | `arriendo_mensual_vivienda` | Arriendo mensual pagado (solo viviendas arrendadas) | `P5090`, `P5140` | Cuantitativa continua | 29.565 |
| V4 | `presencia_analfabetismo` | Si en la vivienda hay al menos una persona analfabeta | `P6160` | Cualitativa binaria | 72.737 |

- V1 y V2 quedan sin dato en las 88 viviendas que no tienen adultos.
- V3 tiene una columna auxiliar, `log_arriendo_mensual`, que se agregó por sugerencia del docente:
  el arriendo tiene valores extremos y el logaritmo los vuelve manejables. No es una quinta
  variable.

## Metodología por fases

1. **Fase 1: auditoría y selección.** Se revisaron los archivos del DANE y muchas variables
   candidatas. La mayoría se descartó por baja cobertura o poca variación, o porque el docente
   prohibió usar el ejemplo de clase (ingreso laboral).
2. **Fase 2: fichas y validaciones.** Se hizo una ficha técnica de 12 puntos por variable y se
   definió el modelo de datos. En cada ejecución, el script `01b` comprueba:
   - la codificación de cada archivo, leyendo sus bytes;
   - los dominios de los campos y los saltos de patrón del cuestionario;
   - la unicidad de las llaves de persona, hogar y vivienda;
   - que la unión de las dos tablas sea uno a uno.
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
- **V1 mide acceso, no graduación:** cuenta a quien cursó estudios técnicos o superiores
  (códigos 8 a 13 de `P3042`), tenga o no el título. La formación normalista (código 7) no cuenta.
- **V2 cuenta solo a quien se reconoce como mujer.** No es una comparación de mujeres frente a
  hombres.
- **V3 aplica solo a viviendas arrendadas**, que son el 41 % del total. Los valores extremos se
  conservan y se declaran en el informe.
- **V4 crece con el tamaño del hogar**, de 6,6 % a 53,5 %. Es un efecto aritmético (más personas,
  más probabilidad de que una sea analfabeta), no un hallazgo social.
- **Los intervalos se justifican por el teorema del límite central**, no por normalidad de las
  variables, porque ninguna de las cuatro es normal.
- **Los faltantes nunca se convierten en cero.** El código 99 de `P3042` y los códigos 98 y 99 de
  `P5140` se tratan como dato faltante.

## Cómo reproducir los resultados

1. Copiar los scripts de R a la carpeta que contiene los microdatos del DANE, es decir, la que
   tiene dentro las carpetas `Mayo 2025/CSV/`, `Junio 2025/CSV/` y `Julio 2025/CSV/`. Los scripts
   usan rutas relativas, así que no importa dónde esté esa carpeta.
2. Instalar los paquetes necesarios. Solo los usa el script `01b`; los demás usan R base:

   ```r
   install.packages(c("readr", "dplyr"))
   ```

3. Poner el directorio de trabajo en esa carpeta. En RStudio:
   **Session > Set Working Directory > To Source File Location**.
4. Ejecutar el script maestro:

   ```r
   source("00_ejecutar_todo.R", encoding = "UTF-8")
   ```

La ejecución tarda de 10 a 20 minutos; la mayor parte se va en los scripts `08` y `09`. Los
scripts también se pueden correr uno por uno en orden, pero del `03` al `09` necesitan que el `01b`
se haya corrido antes.

Al terminar, la carpeta `salidas/` contiene:

| Archivo | Contenido |
|---------|-----------|
| `base_directorio_mes.csv` y `.rds` | Base analítica: 72.737 filas × 23 columnas, una fila por vivienda |
| `base_departamento_mes.csv` y `.rds` | Resumen auxiliar: 99 filas × 29 columnas, una fila por departamento y mes |
| `diccionario_variables_derivadas.csv` y `.rds` | Documentación de las 23 variables de la base |
| `fase3_descriptivos.csv` | Descriptivos de V1, V2 y V3 |
| `fase3_frecuencias_v4.csv` | Frecuencias de V4 |
| `fase3_estimadores_puntuales.csv` | Estimadores e intervalos de confianza |
| `fase3_simulacion.csv` | Resultados de la simulación |
| `graficos/` | Los 20 gráficos en PNG |
