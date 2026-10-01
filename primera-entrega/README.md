# Plantilla LaTeX compatible

Esta carpeta contiene una reconstrucción de la plantilla Word suministrada. El archivo LaTeX original no estuvo disponible entre los adjuntos, por lo que no se realizó una corrección línea por línea de esa fuente.

## Overleaf

1. Comprima o cargue el contenido completo de esta carpeta, conservando `main.tex` y la carpeta `figuras/`.
2. Defina `main.tex` como documento principal.
3. Seleccione **pdfLaTeX** como compilador.
4. Compile. Las imágenes de ejemplo pueden reemplazarse sin cambiar las rutas si se conservan sus nombres, o pueden cambiarse los nombres dentro de `main.tex`.

## Texmaker

1. Instale una distribución LaTeX actual, por ejemplo TeX Live o MiKTeX. Texmaker es el editor y necesita que los comandos de la distribución estén correctamente configurados.
2. Abra `main.tex` desde la carpeta que contiene también `figuras/`.
3. En **Opciones > Configurar Texmaker > Editor**, use codificación UTF-8.
4. En **Opciones > Configurar Texmaker > Compilación rápida**, seleccione una secuencia basada en `pdfLaTeX` y visualización del PDF.
5. Compile con **F1**. Si Texmaker no encuentra `pdflatex`, revise las rutas en **Opciones > Configurar Texmaker > Comandos**.

## Decisiones de compatibilidad

- El documento usa `pdfLaTeX`, paquetes habituales de TeX Live y rutas relativas.
- El archivo principal no contiene espacios ni tildes en el nombre.
- No usa `minted`, `fontspec`, `shell-escape`, fuentes instaladas en el sistema ni rutas absolutas.
- Las imágenes faltantes generan un recuadro de aviso en vez de detener la compilación.
- El código R se presenta mediante `listings`, sin depender de Python o Pygments.

## Fuentes oficiales consultadas

- Overleaf, configuración del compilador y versión de TeX Live: https://www.overleaf.com/learn/how-to/Using_the_Overleaf_project_menu#Compiler
- Overleaf, soporte para español y archivos UTF-8: https://www.overleaf.com/learn/latex/Spanish
- Texmaker, configuración de comandos y compilación: https://www.xm1math.net/texmaker/doc.html
