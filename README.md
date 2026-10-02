# Trabajo-Estadistica

Primer Trabajo de Estadística II: análisis de cuatro variables construidas a partir de la
**Gran Encuesta Integrada de Hogares (GEIH) 2025 del DANE**. Para cada variable se hace la
descripción, se estima la media o la proporción (y, en las tres cuantitativas, la varianza σ²)
con intervalos de confianza y se evalúan los estimadores (insesgamiento, consistencia y
eficiencia).

**Grupo 5:** David Ricardo Martínez, Mariana Gómez González y Juan Camilo Arias Sarabia.

## Contenido del repositorio

Todo el trabajo está en la carpeta
[`Trabajo de Inferencia GRUPO 5/`](Trabajo%20de%20Inferencia%20GRUPO%205/):

| Archivo | Qué contiene |
|---------|--------------|
| `Primer_Trabajo_Estadistica_II_-_Fase_3.docx` | El informe final |
| `Instrucciones para ejecucion.txt` | Cómo ejecutar el código, qué produce, qué comprueba y cómo verificar las cifras del informe |
| `00_ejecutar_todo.R` | Corre los otros diez scripts en orden. No tiene cálculos propios |
| `01b_construccion_estilo_docente.R` | Lee los CSV del DANE, valida los datos (se detiene si una comprobación falla), construye las cuatro variables y exporta las bases |
| `03_descriptivos_fase3.R` | Descriptivos de V1, V2 y V3 (en pesos y en logaritmo) y frecuencias de V4 |
| `03b_cifras_del_texto.R` | Las cifras que el informe cita en los comentarios y que no salen de otro script |
| `04_graficos_log_arriendo.R` | Gráficos 1 a 5 (V3 en pesos y en logaritmo) |
| `05_graficos_educacion_superior.R` | Gráficos 6 a 9 (V1) |
| `06_graficos_v2_mujeres.R` | Gráficos 10 a 13 (V2) |
| `07_graficos_v4_analfabetismo.R` | Gráficos 14 a 16 (V4) |
| `08_estimadores_simulacion.R` | Estimadores e intervalos de confianza de la media o la proporción y de σ², y simulación sin semilla; guarda todas las réplicas en `salidas/fase3_distribuciones.rds` |
| `09_graficos_consistencia.R` | Gráficos 17 a 20 (consistencia: distribución de la media para B = 100, 1.000 y 10.000), dibujados con las réplicas que guardó el `08` |
| `10_variacion_entre_ejecuciones.R` | Tres ejecuciones de la simulación sin semilla, comparadas con el error de Monte Carlo |

Con el trabajo se entregan además `salidas/fase3_distribuciones.rds` y los CSV
`salidas/fase3_*.csv` de la ejecución que reporta el informe. No contienen microdatos, y como
la simulación no fija semilla son la única forma de verificar sus cifras de Monte Carlo.

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
   definió el modelo de datos. En cada ejecución, el script `01b` comprueba lo siguiente y se
   detiene si algo falla:
   - los dominios de los campos y los saltos de patrón del cuestionario;
   - la unicidad de las llaves de persona, hogar y vivienda;
   - que la unión de las dos tablas sea uno a uno;
   - los rangos y la coherencia de las cuatro variables en la base final.

   Además determina la codificación de cada archivo leyendo sus bytes. La lista completa está en
   las Instrucciones.
3. **Fase 3: análisis.**
   - Estadísticos descriptivos y 20 gráficos (16 van en el informe).
   - Estimadores por analogía y por máxima verosimilitud de la media o la proporción y, en V1,
     V2 y V3, de la varianza σ².
   - Intervalos de confianza al 95 %. Para σ² se usa el intervalo chi-cuadrado y, como
     complemento, uno asintótico robusto a la curtosis, con el nivel real del chi-cuadrado
     declarado.
   - Simulación con n igual al tamaño del conjunto analítico de cada variable y B = 100, 1.000 y
     10.000 réplicas en el mismo ciclo, sin semilla, para comparar la media con la mediana como
     estimadores. El insesgamiento y la eficiencia se leen con B = 10.000, y la consistencia
     compara los tres valores de B con n fijo. Cada ejecución produce réplicas nuevas; el script
     `10` compara tres ejecuciones y muestra que las cifras cambian en una magnitud del orden del
     error de Monte Carlo, que se reduce al crecer B.
4. **Cierre.**
   - Prueba de reproducibilidad: dos corridas completas comparadas por hash. Como la simulación
     no fija semilla, la comparación aplica solo a las salidas deterministas: bases,
     descriptivos, cifras del texto, estimadores, intervalos de la media y de σ², y gráficos 1 a
     16. Las salidas de Monte Carlo cambian por diseño en cada corrida: el resumen y las réplicas
     de la simulación, los valores del informe que salen de ellas, la comparación entre
     ejecuciones y los gráficos 17 a 20. El código de la comparación está en las Instrucciones.
   - Revisión contra la rúbrica: se añadió al informe la sintaxis que produce cada resultado y se
     unificó la clasificación de V1 y V2.
5. **Corrección después de la calificación.** Se atendieron los tres puntos del docente:
   - la consistencia se estudia con n fijo y B variable en un mismo ciclo, guardando las
     distribuciones;
   - se retiró la semilla de los scripts y de la sintaxis del informe;
   - se agregó el intervalo de confianza de σ² en las tres variables cuantitativas.

   Además, las cifras citadas en los comentarios salen de una línea de código (scripts `03b` y
   `01b`) y las cifras de los rótulos de los gráficos se calculan con los datos.

## Resultados principales

| Variable | Estimación | IC 95 % | Forma de la distribución |
|----------|-----------|---------|--------------------------|
| V1 | 0,3325 | [0,3296 ; 0,3355] | Bimodal: 54 % de las viviendas en 0 y 22 % en 1 |
| V2 | 0,5625 | [0,5602 ; 0,5648] | Trimodal, con pico en 0,5 (parejas) |
| V3 (log) | 13,2039 → $542.485 | [$538.910 ; $546.085] | Cola larga a la derecha (máximo de $600 millones) |
| V4 | 18,82 % | [18,54 % ; 19,11 %] | El 81 % de las viviendas no tiene personas analfabetas |

- Para la media o la proporción, el estimador por analogía y el de máxima verosimilitud coinciden
  en las cuatro variables. Para σ² (V1, V2 y V3) difieren en el factor (n − 1)/n, una diferencia
  despreciable con estos tamaños de muestra.
- **Varianza σ²** (V1, V2 y V3; en V3, la del logaritmo). El informe presenta S², el estimador de
  máxima verosimilitud y el IC 95 % chi-cuadrado; en el comentario agrega el intervalo robusto a
  la curtosis. La curtosis de los descriptivos es 1,85 en V1, 2,30 en V2 y 6,97 en V3 (escala
  logarítmica). Con ella, el nivel real aproximado del chi-cuadrado es cercano al 99,7 %, al
  98,5 % y al 74 %, respectivamente. En V1 y V2 el intervalo es conservador; en V3 es demasiado
  angosto, así que ahí la inferencia sobre σ² se apoya en el intervalo robusto. Las cifras están
  en `salidas/fase3_estimadores_varianza.csv` y no dependen de la simulación.
- La simulación, con n igual al tamaño del conjunto analítico y B = 100, 1.000 y 10.000, muestra
  que la media muestral es **insesgada** y que la varianza de sus réplicas se ajusta a σ²/n, de
  donde se sigue su consistencia. Al crecer B no cambia la distribución de la media, sino la
  precisión con que la simulación la aproxima: el error de Monte Carlo se divide por diez de
  B = 100 a B = 10.000, y la dispersión de las réplicas se estabiliza en σ/√n.
- **Hallazgo central:** con n igual al tamaño de la base, la mediana muestral de V1, V2 y V4 queda
  prácticamente fija en un valor, porque la mediana poblacional cae en una masa puntual grande
  (0 en V1 y V4, 0,5 en V2). Su varianza casi nula la hace parecer "más eficiente" que la media.
  Pero frente a la media poblacional la mediana tiene un sesgo que ningún valor de B corrige, y
  por error cuadrático medio la media es el estimador eficiente en las cuatro variables.

## Decisiones y advertencias

- **V3 se estima sobre el logaritmo** (confirmado por el docente). Al devolverlo con `exp()` se
  obtiene la media geométrica, que corresponde al arriendo típico ($542.485). La media aritmética
  es otra cifra: $691.125. La varianza σ² de V3 también se estima en la escala logarítmica, y a
  sus límites no se les aplica `exp()`.
- **V1 mide acceso, no graduación:** cuenta a quien cursó estudios técnicos o superiores
  (códigos 8 a 13 de `P3042`), tenga o no el título. La formación normalista (código 7) no cuenta.
- **V2 cuenta solo a quien se reconoce como mujer.** No es una comparación de mujeres frente a
  hombres.
- **V3 aplica solo a viviendas arrendadas**, que son el 41 % del total. Los valores extremos se
  conservan y se declaran en el informe.
- **V4 crece con el tamaño del hogar**, de 6,6 % a 53,5 %. Es un efecto aritmético (más personas,
  más probabilidad de que una sea analfabeta), no un hallazgo social.
- **Los intervalos de la media y la proporción se justifican por el teorema del límite
  central**, no por normalidad de las variables, porque ninguna de las cuatro es normal. El de
  σ² no tiene esa protección: el chi-cuadrado supone normalidad (curtosis 3). Por eso se acompaña
  del intervalo robusto a la curtosis.
- **Convención para σ²:** por analogía se usa S², con divisor n − 1 (`var()` de R); por máxima
  verosimilitud, el divisor n. V4 no lleva intervalo de σ², porque en una Bernoulli σ² = p(1 − p)
  queda determinada por p.
- **La simulación no fija semilla.** Cada ejecución da cifras de Monte Carlo algo distintas. El
  informe reporta la ejecución guardada en `salidas/fase3_distribuciones.rds`, de la que salen
  las tablas y los gráficos 17 a 20; por eso ese archivo se entrega junto con los CSV de la
  fase 3.
- **Los faltantes nunca se convierten en cero.** El código 99 de `P3042` y los códigos 98 y 99 de
  `P5140` se tratan como dato faltante.

## Cómo reproducir los resultados

1. Copiar los once scripts de R a la carpeta que contiene los microdatos del DANE, es decir, la que
   tiene dentro las carpetas `Mayo 2025/CSV/`, `Junio 2025/CSV/` y `Julio 2025/CSV/`. Los scripts
   usan rutas relativas, así que no importa dónde esté esa carpeta.
2. Instalar los paquetes necesarios. Solo los usa el script `01b`; los demás usan R base:

   ```r
   install.packages(c("readr", "dplyr"))
   ```

3. Poner el directorio de trabajo en esa carpeta. En RStudio:
   **Session > Set Working Directory > To Source File Location**.
4. Abrir R sin restaurar un espacio de trabajo guardado (`.RData`). Si se restaura, el generador
   aleatorio arranca siempre en el mismo estado.
5. Ejecutar el script maestro:

   ```r
   source("00_ejecutar_todo.R", encoding = "UTF-8")
   ```

El propio `00` imprime los minutos acumulados al terminar cada script y el total. Como
referencia, en una prueba con datos sintéticos del mismo tamaño la corrida completa tardó unos 12
minutos. Casi todo ese tiempo se va en la simulación del `08` (unos 4 minutos) y en las dos
ejecuciones adicionales del `10` (unos 7,5); el `09` tarda segundos porque solo lee las réplicas.
Los scripts también se pueden correr uno por uno, en orden. Del `03` al `08` y el `10` leen la
base que produce el `01b`; el `09` y el `10` leen las réplicas que guarda el `08`. En Linux o
macOS, R debe correr con una configuración regional UTF-8.

Las salidas deterministas se reproducen idénticas. Las de la simulación cambian en cada corrida,
en una magnitud del orden del error de Monte Carlo. El `00` reescribe la carpeta `salidas/`: para
conservar la ejecución que reporta el informe, hay que copiarla antes. Los gráficos 17 a 20 se
pueden volver a dibujar sin los microdatos: basta tener `salidas/fase3_distribuciones.rds` y
correr el `09`.

Al terminar, la carpeta `salidas/` contiene lo siguiente (MC = cambia en cada ejecución porque
depende de la simulación):

| Archivo | Contenido |
|---------|-----------|
| `base_directorio_mes.csv` y `.rds` | Base analítica: 72.737 filas × 23 columnas, una fila por vivienda |
| `base_departamento_mes.csv` y `.rds` | Resumen auxiliar: 99 filas × 29 columnas, una fila por departamento y mes |
| `diccionario_variables_derivadas.csv` y `.rds` | Documentación de las 23 variables de la base |
| `fase3_descriptivos.csv` | Descriptivos de V1, V2 y V3 (en pesos y en logaritmo) |
| `fase3_frecuencias_v4.csv` | Frecuencias de V4 |
| `fase3_cifras_del_texto.csv` | Cifras citadas en los comentarios del informe, con su clave y el apartado donde aparecen |
| `fase3_estimadores_puntuales.csv` | Estimadores e intervalos de confianza de la media o la proporción |
| `fase3_estimadores_varianza.csv` | Estimadores e intervalos de confianza de σ² (V1, V2 y V3), con el intervalo robusto y el nivel real del chi-cuadrado |
| `fase3_simulacion.csv` (MC) | Resumen de la simulación: una fila por variable y valor de B |
| `fase3_distribuciones.rds` (MC) | Todas las réplicas de la media y la mediana, por variable y valor de B (las leen el `09` y el `10`) |
| `fase3_valores_informe.csv` (MC) | Cifras del `08` escritas como en el informe, cada una con su clave |
| `fase3_variacion_ejecuciones.csv` (MC) | Tres ejecuciones sin semilla frente al error de Monte Carlo |
| `fase3_valores_informe_10.csv` (MC) | Cifras de esa comparación, cada una con su clave |
| `graficos/` | Los 20 gráficos en PNG; del 17 al 20 (MC), un panel por cada valor de B |
