#' @export
colores_tesoro <- function() {
  c(
    "#FFC000",
    "#843C0C",
    "#A5A5A5",
    "#F8CBAD",
    "#ED7D31",
    "#421E06",
    "#800000",
    "#d00000",
    "#cfd4fc",
    "#576af4",
    "#081482",
    "#777777"
  )
}

#' @export
colores_sgestudios <- function() {
  colores_tesoro()
}

tema_sgestudios <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(hjust = 1, angle = 45),
    axis.ticks = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = NA, colour = NA),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major = ggplot2::element_line(colour = "#D9D9D9", linewidth = 0.75, linetype = "dotted", arrow = FALSE),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    strip.background = ggplot2::element_rect(fill = "white")
  )
}


# funciones comunes para reportes mensuales de tendencias de comercio exterior
#' @export
graficos_estilo <- function(.x) {
  .x +
    + tema_sgestudios() +
    scale_y_continuous(position="right", labels = scales::number_format(scale=1e-6, suffix="M", big.mark=".", decimal.mark=",")) +
    theme(
      text = element_text(size=14),
      legend.text = element_text(size=18),
      title = element_text()
    )
}
# Sectores ----
## conjunto de la balanza comercial ----
#' @export
graficos_estilo_conjunto_bc <- function(.x, n_colors=10) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")
  pal <- viridisLite::inferno(n=4, begin=0.3, end=0.8)
  pal <- c("Energético" = "red",
              "No energético"="blue",
              "Total"="black")
  graficos_estilo(.x) +
    guides(color=guide_legend(ncol=3,bycol=TRUE)) +
    scale_color_manual(values = pal)
}

## conjunto de la balanza comercial - corto ----
#' @export
graficos_estilo_conjunto_bc_corto <- function(.x, n_colors=10) {
  graficos_estilo_conjunto_bc(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## conjunto de la balanza comercial - largo ----
#' @export
graficos_estilo_conjunto_bc_largo <- function(.x, n_colors=10) {
  graficos_estilo_conjunto_bc(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

## energeticos ----
#' @export
graficos_estilo_energeticos <- function(.x, n_colors=10) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")
  pal <- viridisLite::inferno(n=4, begin=0.3, end=0.8)
  graficos_estilo(.x) +
    guides(color=guide_legend(nrow=3,byrow=TRUE)) +
    scale_color_manual(values = pal)
}

## energeticos - corto ----
#' @export
graficos_estilo_energeticos_corto <- function(.x, n_colors=10) {
  graficos_estilo_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## energeticos - largo ----
#' @export
graficos_estilo_energeticos_largo <- function(.x, n_colors=10) {
  graficos_estilo_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

# no energeticos ----
#' @export
graficos_estilo_no_energeticos <- function(.x, n_colors=10) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")
  palette("Tableau 10")
  pal <- palette()
  graficos_estilo(.x) +
    theme(legend.text = element_text(size=15))+
    guides(color=guide_legend(nrow=4,byrow=TRUE)) +
    scale_color_manual(values = pal) #  viridisLite::inferno(n_colors))
}

## corto ----
#' @export
graficos_estilo_no_energeticos_corto <- function(.x, n_colors=10) {
  graficos_estilo_no_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## largo ----
#' @export
graficos_estilo_no_energeticos_largo <- function(.x, n_colors=10) {
  graficos_estilo_no_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

# Destinos ----
#' Fijar estilo de gráfico estándar para SGEstudios, con algunos parámetros opcionales
#' @param .tipo_grafico Cadena de caracteres. Puede tomar valores "millones", "porcentaje" o NULL. Si es distinto a NULL, el resto de argumentos no se tienen en cuenta en la medida en que colisionen con los definidos para el tipo de gráfico.
#' @param .scale Número. Factor por el que dividir los números del eje. Por defecto, 1. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.
#' @param .accuracy Número. Número de decimales a mostrar en eje. Por defecto, 1. Para mostrar 0 decimales, fijar a 1; para 1 decimal, fijar a 0.1; para 2, a 0.01 y así...
#' @param .suffix Cadena de caracteres. Por defecto, cadena vacía.
#' @param .minimo_eje_y Número. Valor mínimo en el eje. Por defecto, NULL y el mínimo se adapta al gráfico
#' @param .maximo_eje_y Número. Valor máximo en el eje. Por defecto, NULL y el mínimo se adapta al gráfico
#' @param .date_labels Cadena de caracteres. Formato de las fechas en eje de abscisas. Por defecto, "%Y". Para mostrar el año y mes abreviado, fijar a "%Y-%b"; para fijar sólo el número del año "%Y". Para otros casos, ver documentación de scale_x_date.
#' @param .date_breaks Cadena de caracteres. Frecuencia de las fechas en eje de abscisas. Por defecto, "1 year". Para mostrar una fecha cada seis meses, fijar a "6 months"; para mostrar fechas cada 2 años, fijar a "2 years".
#' @param .position Cadena de caracteres. Posición del eje de ordenadas. Por defecto, "right".
#' @param .n_colors Número. Número de colores a generar en la paleta. No tiene efecto.
#' @export
graficos_estilo_destino <- function(
    .x,
    .tipo_grafico = NULL,
    .scale = 1,
    .accuracy=1,
    .suffix="",
    .minimo_eje_y=NULL,
    .maximo_eje_y=NULL,
    .date_labels="%Y",
    .date_breaks="1 year",
    .position="right",
    .n_colors=10
    ) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")

  palette("Tableau 10")
  pal <- palette()

  .plot_to_return_df <- .x +
    tema_sgestudios() +
    theme(
      text = element_text(size=14),
      legend.text = element_text(size=18),
      title = element_text()
    )   +
    guides(color=guide_legend(nrow=3,byrow=TRUE)) +
    scale_color_manual(values = pal)

  suffix_definido <- .suffix
  scale_definido <- .scale
  accuracy_definido <- .accuracy

  if(length(.tipo_grafico) > 0) {
    message("x")
    if (.tipo_grafico == "millones") {
      suffix_definido <- "M"
      scale_definido <- 1
      accuracy_definido <- 1
    } else if (.tipo_grafico == "porcentaje") {
      suffix_definido <- "%"
      scale_definido <- 1e2
      accuracy_definido <- 0.1
    }
  }

  if(!is.null(.minimo_eje_y) & !is.null(.maximo_eje_y)) {
    .plot_to_return_df <- .plot_to_return_df +
      scale_x_date(
        date_labels = .date_labels,
        date_breaks = .date_breaks
      ) +
      scale_y_continuous(
        labels=scales::number_format(
          accuracy=accuracy_definido,
          scale=scale_definido,
          suffix=suffix_definido,
          limits = c(.minimo_eje_y, .maximo_eje_y)
        ),
        position=.position
      )
  } else {
    .plot_to_return_df <- .plot_to_return_df +
      scale_x_date(
        date_labels = .date_labels,
        date_breaks = .date_breaks
      ) +
      scale_y_continuous(
        labels=scales::number_format(
          accuracy=accuracy_definido,
          scale=scale_definido,
          suffix=suffix_definido
        ),
        position=.position
      )
  }

  return(.plot_to_return_df)
}

## conjunto de la balanza comercial - corto ----
#' @export
graficos_estilo_destino_conjunto_bc_corto <- function(.x, n_colors=10) {
  graficos_estilo_destino_conjunto_bc(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## conjunto de la balanza comercial - largo ----
#' @export
graficos_estilo_destino_conjunto_bc_largo <- function(.x, n_colors=10) {
  graficos_estilo_destino_conjunto_bc(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

## energeticos ----
#' @export
graficos_estilo_destino_energeticos <- function(.x, n_colors=10) {
  pal <- palette()
  graficos_estilo_destino(.x) +
    guides(color=guide_legend(nrow=3,byrow=TRUE)) +
    scale_color_manual(values = pal)
}

## energeticos - corto ----
#' @export
graficos_estilo_destino_energeticos_corto <- function(.x, n_colors=10) {
  graficos_estilo_destino_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## energeticos - largo ----
#' @export
graficos_estilo_destino_energeticos_largo <- function(.x, n_colors=10) {
  graficos_estilo_destino_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

# no energeticos ----
#' @export
graficos_estilo_destino_no_energeticos <- function(.x, n_colors=10) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")
  palette("Tableau 10")
  pal <- palette()
  graficos_estilo_destino(.x) +
    theme(legend.text = element_text(size=15))+
    guides(color=guide_legend(nrow=2,byrow=TRUE)) +
    scale_color_manual(values = pal) #  viridisLite::inferno(n_colors))
}

## corto ----
#' @export
graficos_estilo_destino_no_energeticos_corto <- function(.x, n_colors=10) {
  graficos_estilo_destino_no_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
}

## largo ----
#' @export
graficos_estilo_destino_no_energeticos_largo <- function(.x, n_colors=10) {
  graficos_estilo_destino_no_energeticos(.x, n_colors=n_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

#' @export
to_giraph <- function(.x) {
  ggiraph::girafe(ggobj=.x,
                  options=list(
                    ggiraph::opts_hover(css = ''), ## CSS code of line we're hovering over
                    ggiraph::opts_hover_inv(css = "opacity:0.8;"), ## CSS code of all other lines
                    ggiraph::opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
                    position = "topright",
                    saveaspng = T
                  )
  )

}


# estatico ---
#' @export
estatico <- function(.plot, .estatico) {
  palette("Tableau 10")
  pal <- palette()

  .plot_return <- .plot +
    scale_color_manual(values = pal)

  if(!.estatico) {
    .plot_return <- .plot_return |> comerciotools::to_giraph() |>
      ggiraph::girafe_options(
        ggiraph::opts_sizing(rescale = TRUE, width = 1)
      )

    return(.plot_return)
  }
  return(.plot_return)
}
