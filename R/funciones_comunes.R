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

## conjunto de la balanza comercial ----
#' @export
graficos_estilo_destino <- function(.x, n_colors=10) {
  # pal <- wesanderson::wes_palette("BottleRocket2", n_colors, type = "continuous")

  palette("Tableau 10")
  pal <- palette()
   .x +
    colores_sgestudios() +
    scale_y_continuous(position="right", labels = scales::number_format(scale=1e-6, suffix="M", big.mark=".", decimal.mark=",")) +
    theme(
      text = element_text(size=14),
      legend.text = element_text(size=18),
      title = element_text()
    )   +
    guides(color=guide_legend(nrow=3,byrow=TRUE)) +
    scale_color_manual(values = pal)
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
