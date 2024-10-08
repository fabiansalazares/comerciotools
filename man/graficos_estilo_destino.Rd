% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funciones_comunes.R
\name{graficos_estilo_destino}
\alias{graficos_estilo_destino}
\title{Fijar estilo de gráfico estándar para SGEstudios, con algunos parámetros opcionales}
\usage{
graficos_estilo_destino(
  .x,
  .tipo_grafico = NULL,
  .scale = 1,
  .accuracy = 1,
  .suffix = "",
  .minimo_eje_y = NULL,
  .maximo_eje_y = NULL,
  .date_labels = "\%Y",
  .date_breaks = "1 year",
  .position = "right",
  .n_colors = 10,
  .text_axis_size = 12
)
}
\arguments{
\item{.tipo_grafico}{Cadena de caracteres. Puede tomar valores "millones", "porcentaje" o NULL. Si es distinto a NULL, el resto de argumentos no se tienen en cuenta en la medida en que colisionen con los definidos para el tipo de gráfico.}

\item{.scale}{Número. Factor por el que dividir los números del eje. Por defecto, 1. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.}

\item{.accuracy}{Número. Número de decimales a mostrar en eje. Por defecto, 1. Para mostrar 0 decimales, fijar a 1; para 1 decimal, fijar a 0.1; para 2, a 0.01 y así...}

\item{.suffix}{Cadena de caracteres. Por defecto, cadena vacía.}

\item{.minimo_eje_y}{Número. Valor mínimo en el eje. Por defecto, NULL y el mínimo se adapta al gráfico}

\item{.maximo_eje_y}{Número. Valor máximo en el eje. Por defecto, NULL y el mínimo se adapta al gráfico}

\item{.date_labels}{Cadena de caracteres. Formato de las fechas en eje de abscisas. Por defecto, "\%Y". Para mostrar el año y mes abreviado, fijar a "\%Y-\%b"; para fijar sólo el número del año "\%Y". Para otros casos, ver documentación de scale_x_date.}

\item{.date_breaks}{Cadena de caracteres. Frecuencia de las fechas en eje de abscisas. Por defecto, "1 year". Para mostrar una fecha cada seis meses, fijar a "6 months"; para mostrar fechas cada 2 años, fijar a "2 years".}

\item{.position}{Cadena de caracteres. Posición del eje de ordenadas. Por defecto, "right".}

\item{.n_colors}{Número. Número de colores a generar en la paleta. No tiene efecto.}

\item{.text_axis_size}{Número. Tamaño de la fuente en los títulos de los ejes. Sólo aplicable para \code{.tipo_grafico = "nofecha"}}
}
\description{
Fijar estilo de gráfico estándar para SGEstudios, con algunos parámetros opcionales
}
