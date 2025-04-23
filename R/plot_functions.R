#' @title plot xvg data
#' @description plot xvg data using ggplot2
#'
#' @param xvg_data xvg data object returned by read_xvg
#' @param title chart title (default uses xvg file's title)
#' @param subtitle chart subtitle (default uses xvg file's subtitle)
#' @param ... additional parameters passed to ggplot2::geom_line
#'
#' @return a ggplot2 object
#' @import ggplot2
#' @importFrom stats setNames
#' @examples
#' \donttest{
#' library(xvm)
#' rmsd_file_path <- system.file("extdata/rmsd.xvg", package = "xvm")
#' rmsd_data <- read_xvg(rmsd_file_path)
#' plot_xvg(rmsd_data) # plot the xvg data using plot_xvg() function
#' }
#' @export
plot_xvg <- function(xvg_data, title = NULL, subtitle = NULL,...) {
  if (is.list(xvg_data)) {
    if ("data" %in% names(xvg_data) && "metadata" %in% names(xvg_data)) {
      data <- xvg_data$data
      metadata <- xvg_data$metadata
    } else if (length(xvg_data) > 0) {
      first_key <- names(xvg_data)[1]
      if (!is.null(first_key) && is.list(xvg_data[[first_key]])) {
        if ("data" %in% names(xvg_data[[first_key]]) && "metadata" %in% names(xvg_data[[first_key]])) {
          data <- xvg_data[[first_key]]$data
          metadata <- xvg_data[[first_key]]$metadata
          if (length(xvg_data) > 1) {
            warning("Multiple XVG datasets provided, using the first one: ", first_key)
          }
        } else {
          stop("Invalid XVG structure: nested element missing data or metadata")
        }
      } else {
        stop("Unrecognized XVG data format")
      }
    } else {
      stop("Empty XVG data list provided")
    }
  } else {
    stop("xvg_data must be a list object")
  }

  x_col <- colnames(data)[1]

  y_cols <- colnames(data)[-1]

  has_legend_special <- FALSE
  legend_labels <- NULL

  if (!is.null(metadata$legends_formatted) && length(metadata$legends_formatted) > 0) {
    legend_labels <- metadata$legends_formatted
    has_legend_special <- any(grepl("\\^|\\[", legend_labels))
    legend_mapping <- setNames(legend_labels, y_cols)
  }
  plot_data <- tidyr::pivot_longer(
    data,
    cols = y_cols,
    names_to = "variable",
    values_to = "value"
  )

  if (is.null(title) && !is.null(metadata$title)) {
    title <- metadata$title
  }

  if (is.null(subtitle) && !is.null(metadata$subtitle)) {
    subtitle <- metadata$subtitle
  }

  x_label <- metadata$xaxis_formatted
  y_label <- metadata$yaxis_formatted


  has_x_special <- grepl("\\^|\\[", x_label)
  has_y_special <- grepl("\\^|\\[", y_label)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data$value, color = .data$variable)) +
    ggplot2::geom_line(...) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      color = "Legend"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  if (has_x_special) {
    p <- p + ggplot2::xlab(parse(text = x_label))
  } else {
    p <- p + ggplot2::xlab(x_label)
  }

  if (has_y_special) {
    p <- p + ggplot2::ylab(parse(text = y_label))
  } else {
    p <- p + ggplot2::ylab(y_label)
  }

  if (!is.null(legend_labels) && has_legend_special) {
    parsed_labels <- sapply(legend_labels, function(l) parse(text = l))
    p <- p + ggplot2::scale_color_discrete(
      name = "Legend",
      labels = parsed_labels
    )
  } else if (!is.null(legend_labels)) {
    p <- p + ggplot2::scale_color_discrete(
      name = "Legend",
      labels = function(x) legend_labels[match(x, names(legend_mapping))]
    )
  }
  return(p)
}

#' @title plot xpm data
#' @description plot xpm data using ggplot2
#'
#' @param xpm_data a xpm object returned by read_xpm
#' @param interpolate logical indicating whether to use raster interpolation (TRUE)
#'        or discrete tiles (FALSE). Default is FALSE.
#' @return a ggplot2 object
#' @import ggplot2
#' @examples
#' \donttest{
#' library(xvm)
#' xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
#' xpm_data <- read_xpm(xpm_file_path)
#' plot_xpm(xpm_data) # plot the xpm data using plot_xpm() function
#' }
#' @export
plot_xpm<-function(xpm_data,interpolate = FALSE){
  if (is.list(xpm_data)) {
    if (!"data" %in% names(xpm_data)) {
      if(length(xpm_data)>=1){
        first_key <- names(xpm_data)[1]
        if (!is.null(first_key) && is.list(xpm_data[[first_key]])) {
          if(length(xpm_data)>1)warning("Multiple XPM datasets provided, using the first one: ", first_key)
          xpm_data <- xpm_data[[first_key]]
        }
      }
    }
  }else {
    stop("xpm_data must be a list object")
  }
  if(interpolate){
    p <- ggplot(xpm_data$data, aes(x = x_actual, y = y_actual, fill = value)) +
      geom_raster(interpolate = T)
  }else{
    p <- ggplot(xpm_data$data, aes(x = x_actual, y = y_actual, fill = value)) +
      geom_tile()
  }
  has_x_special <- grepl("\\^|\\[", xpm_data$x_label)
  has_y_special <- grepl("\\^|\\[", xpm_data$y_label)
  has_legend_special <- grepl("\\^|\\[", xpm_data$legend)
  p <- p+
    labs(
      title = xpm_data$title,
      x = xpm_data$x_label,
      y = xpm_data$y_label,
      fill = xpm_data$legend
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c()+
    coord_fixed(
      ratio = (max(xpm_data$data$x_actual, na.rm = TRUE) - min(xpm_data$data$x_actual, na.rm = TRUE)) /
        (max(xpm_data$data$y_actual, na.rm = TRUE) - min(xpm_data$data$y_actual, na.rm = TRUE))
    )
  if (has_x_special) {
    p <- p + ggplot2::xlab(parse(text = x_label))
  }

  if (has_y_special) {
    p <- p + ggplot2::ylab(parse(text = y_label))
  }
  if (!is.null(xpm_data$legend) && has_legend_special) {
    p <- p + ggplot2::labs(fill = parse(text=xpm_data$legend))
  }
  return(p)
}

#' @title generate faceted plots from xpm Data
#' @description creates dual-panel visualizations of xpm data with scatter or area plots.
#' @param xpm_data a xpm object (from [read_xpm()]) or list containing parsed objects.
#' @param plot_type visualization type: "scatter" (default) or "area".
#'
#' @return a ggplot2 object with:
#' - Dual facets showing x/y axis relationships
#' - Automatic data transformation for visualization
#' - NULL if invalid plot_type specified
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color
#' @importFrom ggnewscale new_scale_fill
#' @examples
#' \donttest{
#' library(xvm)
#' xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
#' xpm_data <- read_xpm(xpm_file_path)
#' plot_xpm_facet(xpm_data) # plot pseudo-3D from xpm file
#' }
#' @export
plot_xpm_facet<-function(xpm_data,plot_type = "scatter"){
  if (is.list(xpm_data)) {
    if (!"data" %in% names(xpm_data)) {
      if(length(xpm_data)>=1){
        first_key <- names(xpm_data)[1]
        if (!is.null(first_key) && is.list(xpm_data[[first_key]])) {
          if(length(xpm_data)>1)warning("Multiple XPM datasets provided, using the first one: ", first_key)
          xpm_data <- xpm_data[[first_key]]
        }
      }
    }
  }else {
    stop("xpm_data must be a list object")
  }
  if(plot_type == "scatter"){
    p<-ggplot() +
      geom_point(
        data = transform(xpm_data$data,
                         facet_group = xpm_data$x_label,
                         color_var = xpm_data$data$y_actual),
        aes(x = x_actual, y = value, color = color_var),
        size = 2, alpha = 0.8
      ) +
      scale_color_viridis_c(option = "C", name = xpm_data$y_label)+
      new_scale_color() +
      geom_point(
        data = transform(xpm_data$data,
                         facet_group = xpm_data$y_label,
                         color_var = xpm_data$data$x_actual),
        aes(x = y_actual, y = value, color = color_var),
        size = 2, alpha = 0.8
      ) +
      scale_color_viridis_c(option = "D", name = xpm_data$x_label) +
      facet_wrap(
        ~ facet_group,
        scales = "free_x",
        ncol = 1
      ) +
      labs(
        title = xpm_data$title,
        y = xpm_data$legend
      ) +
      theme_bw() +
      theme(
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(face = "bold", size = 12),
        axis.title.x = element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
  }else if(plot_type == "area"){
    p<-ggplot() +
      geom_line(
        data = transform(xpm_data$data, facet_group = xpm_data$x_label),
        aes(x = x_actual, y = value, group = y_actual, color = y_actual),
        linewidth = 0.3
      ) +
      geom_ribbon(
        data = transform(xpm_data$data, facet_group = xpm_data$x_label),
        aes(x = x_actual, y = value, group = y_actual, ymin = value, ymax = max(value), fill = y_actual),
        alpha = 0.3, color = NA
      ) +
      scale_color_viridis_c(option = "B", name = xpm_data$y_label) +
      scale_fill_viridis_c(option = "B", name = xpm_data$y_label) +
      new_scale_color() +
      new_scale_fill() +
      geom_line(
        data = transform(xpm_data$data, facet_group = xpm_data$y_label),
        aes(x = y_actual, y = value, group = x_actual, color = x_actual),
        linewidth = 0.3
      ) +
      geom_ribbon(
        data = transform(xpm_data$data, facet_group = xpm_data$y_label),
        aes(x = y_actual, y = value, group = x_actual, ymin = value, ymax = max(value), fill = x_actual),
        alpha = 0.3, color = NA
      ) +
      scale_color_viridis_c(option = "D", name = xpm_data$x_label) +
      scale_fill_viridis_c(option = "D", name = xpm_data$x_label) +
      facet_wrap(
        ~ facet_group,
        scales = "free_x",
        ncol = 1
      ) +
      labs(
        title = xpm_data$title,
        y = xpm_data$legend
      ) +
      theme_bw() +
      theme(
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(face = "bold", size = 12),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )

  }else{p <- NULL}
  return(p)
}

#' @title generate 3d scatter plot from xpm Data
#' @description creates 3d visualization of xpm data with scatter plot.
#' @param xpm_data a xpm object (from [read_xpm()]) or list containing parsed objects.
#' @param reversescale whether to reverse the color scale; default is FALSE
#' @param point_size the size of the points in the scatter plot; default is 2
#' @return a plotly object
#' @importFrom plotly plot_ly layout %>%
#' @examples
#' library(xvm)
#' xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
#' xpm_data <- read_xpm(xpm_file_path)
#' plot_xpm_3d(xpm_data) # plot 3D scatter plot from xpm file
#' @export
plot_xpm_3d<-function(xpm_data, reversescale = FALSE, point_size = 2){
  if (is.list(xpm_data)) {
    if (!"data" %in% names(xpm_data)) {
      if(length(xpm_data)>=1){
        first_key <- names(xpm_data)[1]
        if (!is.null(first_key) && is.list(xpm_data[[first_key]])) {
          if(length(xpm_data)>1)warning("Multiple XPM datasets provided, using the first one: ", first_key)
          xpm_data <- xpm_data[[first_key]]
        }
      }
    }
  }else {
    stop("xpm_data must be a list object")
  }
  scatter_3d <- plot_ly(
    data = xpm_data$data,
    x = ~x_actual,
    y = ~y_actual,
    z = ~value,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = point_size,
      color = ~value,
      colorscale = "Viridis",
      reversescale = reversescale,
      colorbar = list(title = xpm_data$legend),
      opacity = 0.8
    )
  ) %>%
    layout(
      title = xpm_data$title,
      scene = list(
        xaxis = list(title = xpm_data$x_label),
        yaxis = list(title = xpm_data$y_label),
        zaxis = list(title = xpm_data$legend),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
      )
    )
  return(scatter_3d)
}


