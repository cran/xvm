#' @title parse xpm (X PixMap) file content into structured data
#' @description This function parses xpm file content, extracts metadata, color mappings,
#' and matrix data, returning a structured list for further processing.
#' @param xpm_content a character string containing the xpm file content.
#' @return a list with the following components:
#' \itemize{
#'   \item data - Data frame containing matrix values with coordinates
#'   \item title - Chart title extracted from xpm
#'   \item legend - Legend text extracted from xpm
#'   \item x_label - X-axis label extracted from xpm
#'   \item y_label - Y-axis label extracted from xpm
#'   \item color_map - Named list mapping color codes to hex values
#'   \item color_values - Named list mapping color codes to numeric values
#' }
#'
#' @keywords internal
parse_xpm <- function(xpm_content) {
  # Split content into lines
  lines <- strsplit(xpm_content, "\n")[[1]]

  # Extract metadata
  title_line <- grep("title:", lines, value = TRUE)
  title <- gsub(".*\"(.*)\".*", "\\1", title_line)

  legend_line <- grep("legend:", lines, value = TRUE)
  legend <- gsub(".*\"(.*)\".*", "\\1", legend_line)

  x_label_line <- grep("x-label:", lines, value = TRUE)
  x_label <- gsub(".*\"(.*)\".*", "\\1", x_label_line)

  y_label_line <- grep("y-label:", lines, value = TRUE)
  y_label <- gsub(".*\"(.*)\".*", "\\1", y_label_line)

  # Extract matrix dimensions and color count
  dim_line <- grep("^\"[0-9]+ [0-9]+", lines, value = TRUE)
  dim_clean <- gsub("\"(.*)\".*", "\\1", dim_line)
  dims <- as.numeric(strsplit(dim_clean, "[ \t]+")[[1]])

  width <- dims[1]
  height <- dims[2]
  num_colors <- dims[3]

  message("Dimensions parsed: width = ", width, ", height = ", height, ", colors = ", num_colors, "\n")

  # Extract color mappings
  color_map <- list()
  color_values <- list()

  color_lines <- grep("^\".*c #[0-9A-Fa-f].*(/\\*.*\\*/)?", lines)

  # Verify color count
  detected_colors <- length(color_lines)
  if (detected_colors != num_colors) {
    warning(sprintf("Color count mismatch: XPM declares %d colors, but %d color definitions found",
                    num_colors, detected_colors))
    # Use the actual detected color count
    num_colors <- min(num_colors, detected_colors)
  }

  for (i in 1:num_colors) {
    color_line <- lines[color_lines[i]]

    color_code <- gsub("^\"([^\"]+)\".*", "\\1", color_line)
    color_code <- gsub("^([^ ]+).*", "\\1", color_code)

    # Extract hex color
    hex_color <- gsub(".*#([0-9A-Fa-f]+).*", "\\1", color_line)

    # Extract value with improved pattern matching
    value_pattern <- "/\\* *\"([-0-9.]+)\" *\\*/"
    value_match <- regexec(value_pattern, color_line)

    if (value_match[[1]][1] > 0) {
      value_str <- substr(color_line,
                          value_match[[1]][2],
                          value_match[[1]][2] + attr(value_match[[1]], "match.length")[2] - 1)
      value <- as.numeric(value_str)
    } else {
      # Fallback if pattern doesn't match
      value_parts <- strsplit(color_line, "/\\*")[[1]]
      if (length(value_parts) > 1) {
        value_str <- gsub("\"([-0-9.]+)\".*", "\\1", value_parts[2])
        value <- suppressWarnings(as.numeric(value_str))
        if (is.na(value)) {
          # Final fallback: use index-based value
          value <- i - 1  # 0-based index common in color maps
        }
      } else {
        value <- i - 1
      }
    }

    color_map[[color_code]] <- hex_color
    color_values[[color_code]] <- value
  }

  x_axis_lines <- grep("x-axis:", lines, value = TRUE)
  if (length(x_axis_lines) > 0) {
    x_values <- numeric(0)
    for (x_line in x_axis_lines) {
      x_values_str <- gsub(".*: *(.*)", "\\1", x_line)
      x_values_str <- gsub("[ \t]+", " ", trimws(x_values_str))
      x_values_part <- suppressWarnings(as.numeric(strsplit(x_values_str, " ")[[1]]))
      x_values <- c(x_values, x_values_part[!is.na(x_values_part)])
    }
  } else {
    x_values <- 1:width
  }

  y_axis_lines <- grep("y-axis:", lines, value = TRUE)
  if (length(y_axis_lines) > 0) {
    y_values <- numeric(0)
    for (y_line in y_axis_lines) {
      y_values_str <- gsub(".*: *(.*)", "\\1", y_line)
      y_values_str <- gsub("[ \t]+", " ", trimws(y_values_str))
      y_values_part <- suppressWarnings(as.numeric(strsplit(y_values_str, " ")[[1]]))
      y_values <- c(y_values, y_values_part[!is.na(y_values_part)])
    }
  } else {
    y_values <- 1:height
  }

  color_chars <- paste0(names(color_values), collapse="")
  data_pattern <- sprintf("^\"[%s]+\"", color_chars)
  data_lines <- grep(data_pattern, lines)

  data_start <- data_lines[length(data_lines) - height + 1]

  data_matrix <- matrix(NA, nrow = height, ncol = width)

  for (i in 1:height) {
    row_line <- lines[data_start + i - 1]
    row_data <- gsub("\"(.*)\".*", "\\1", row_line)

    for (j in 1:width) {
      if (j <= nchar(row_data)) {
        char <- substr(row_data, j, j)
        if (char %in% names(color_values)) {
          data_matrix[i, j] <- color_values[[char]]
        } else {
          data_matrix[i, j] <- NA
        }
      } else {
        data_matrix[i, j] <- NA
      }
    }
  }

  # Create data frame
  df <- expand.grid(x = 1:width, y = 1:height)
  df$value <- as.vector(t(data_matrix))


  if (length(x_values) >= width) {
    df$x_actual <- x_values[df$x]
  } else {
    df$x_actual <- df$x
  }

  if (length(y_values) >= height) {
    df$y_actual <- y_values[height - df$y + 1]
  } else {
    df$y_actual <- df$y
  }

  return(list(
    data = df,
    title = title,
    legend = legend,
    x_label = x_label,
    y_label = y_label,
    color_map = color_map,
    color_values = color_values
  ))
}

#' @title read xpm files
#' @description This function reads xpm (X PixMap) files, validates their existence,
#' and returns parsed data structures in a list format.
#' @param xpm_files a character vector containing paths to one or more xpm files.
#' @return list with the following components:
#' \itemize{
#'   \item data - Data frame containing matrix values with coordinates
#'   \item title - Chart title extracted from xpm
#'   \item legend - Legend text extracted from xpm
#'   \item x_label - X-axis label extracted from xpm
#'   \item y_label - Y-axis label extracted from xpm
#'   \item color_map - Named list mapping color codes to hex values
#'   \item color_values - Named list mapping color codes to numeric values
#' }
#' @details The function performs the following operations:
#' \enumerate{
#'   \item Validates input type (must be character vector)
#'   \item Checks for file existence and filters missing files with warnings
#'   \item Reads valid files and parses them using [parse_xpm()]
#'   \item Returns aggregated results in a named list
#' }
#' @examples
#' \donttest{
#' library(xvm)
#' # Retrieve the path to the example file included in the package
#' xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
#' xpm_data <- read_xpm(xpm_file_path) # read the xpm file using read_xpm() function
#' names(xpm_data)
#' }
#' @export
read_xpm <- function(xpm_files) {
  # Validate input type
  if (!is.character(xpm_files)) {
    stop("xpm_files must be a character vector containing one or more XPM file paths")
  }

  # Check file existence
  missing_files <- xpm_files[!file.exists(xpm_files)]
  if (length(missing_files) > 0) {
    warning("The following files do not exist: ", paste(missing_files, collapse = ", "))
    xpm_files <- xpm_files[file.exists(xpm_files)]
  }

  if (length(xpm_files) == 0) {
    stop("No valid XPM files to read")
  }

  # Process files
  results <- list()
  for (file_path in xpm_files) {
    xpm_content <- paste(readLines(file_path), collapse = "\n")
    parsed_result <- parse_xpm(xpm_content)
    results[[basename(file_path)]] <- parsed_result
  }

  return(results)
}



#' @title format special text
#' @description processes special formatting in xvg files, converting super/subscripts
#' to ggplot2-compatible expressions. Specifically handles:
#' - Superscripts: Converts \\S...\\N to ^ notation
#' - Subscripts: Converts \\s...\\N to [] notation
#' @param text character string containing xvg-formatted text
#' @return formatted character string with ggplot2-compatible expressions
#' @keywords internal
format_text <- function(text) {
  if (is.null(text)) return(NULL)
  text <- gsub("\\\\S([^\\\\]*)\\\\N", "^\\1", text)
  text <- gsub("\\\\s([^\\\\]*)\\\\N", "[\\1]", text)
  return(text)
}

#' @title parse xvg File Content
#' @description parses content from a single GROMACS-generated xvg file
#' @param lines character vector of text lines from xvg file
#' @param skip_comments logical indicating whether to skip comment lines (default: TRUE)
#' @return list containing xvg data and metadata with following structure:
#' \itemize{
#'   \item data - Data frame containing numerical data
#'   \item metadata - List containing title, axis labels, legends and their formatted versions
#' }
#' @keywords internal
parse_xvg <- function(lines, skip_comments = TRUE) {

  title_line <- grep("@\\s+title", lines, value = TRUE)
  title <- if(length(title_line) > 0) {
    gsub("@\\s+title\\s+\"(.*)\"", "\\1", title_line)
  } else {
    NULL
  }
  subtitle_line <- grep("@\\s+subtitle", lines, value = TRUE)
  subtitle <- if(length(subtitle_line) > 0) {
    gsub("@\\s+subtitle\\s+\"(.*)\"", "\\1", subtitle_line)
  } else {
    NULL
  }
  xaxis_line <- grep("@\\s+xaxis\\s+label", lines, value = TRUE)
  xaxis_label <- if(length(xaxis_line) > 0) {
    gsub("@\\s+xaxis\\s+label\\s+\"(.*)\"", "\\1", xaxis_line)
  } else {
    "X"
  }
  xaxis_formatted <- format_text(xaxis_label)

  yaxis_line <- grep("@\\s+yaxis\\s+label", lines, value = TRUE)
  yaxis_label <- if(length(yaxis_line) > 0) {
    gsub("@\\s+yaxis\\s+label\\s+\"(.*)\"", "\\1", yaxis_line)
  } else {
    "Y"
  }
  yaxis_formatted <- format_text(yaxis_label)

  legend_lines <- grep("@\\s+s[0-9]+\\s+legend", lines, value = TRUE)
  legends <- gsub("@\\s+s([0-9]+)\\s+legend\\s+\"(.*)\"", "\\2", legend_lines)
  legends_formatted <- sapply(legends, format_text)

  if (skip_comments) {
    data_lines <- lines[!grepl("^[#@]", lines)]
  } else {
    data_lines <- lines[!grepl("^@", lines)]
  }
  invalid_lines <- grepl("[^0-9. -]", data_lines)
  invalid_indices <- which(invalid_lines)
  data_lines <- data_lines[!invalid_lines]
  data_text <- paste(data_lines, collapse = "\n")
  data <- utils::read.table(text = data_text, header = FALSE, stringsAsFactors = FALSE)

  if (length(legends) > 0 && ncol(data) == length(legends) + 1) {
    colnames(data) <- c(xaxis_label, legends)
  } else if (ncol(data) == 2) {
    colnames(data) <- c(xaxis_label, yaxis_label)
  } else {
    colnames(data)[1] <- xaxis_label
    if (ncol(data) > 1) {
      colnames(data)[-1] <- paste0(yaxis_label, "_", seq_len(ncol(data) - 1))
    }
  }

  result <- list(
    data = data,
    metadata = list(
      title = title,
      subtitle = subtitle,
      xaxis = xaxis_label,
      yaxis = yaxis_label,
      xaxis_formatted = xaxis_formatted,
      yaxis_formatted = yaxis_formatted,
      legends = legends,
      legends_formatted = legends_formatted
    )
  )

  return(result)
}

#' @title read xvg files
#' @description read one or more GROMACS-generated xvg files
#'
#' @param xvg_files character vector of xvg file paths
#' @param skip_comments logical indicating whether to skip comment lines (default: TRUE)
#'
#' @return Named list containing xvg data, using filenames (without extension) as keys
#' @examples
#' \donttest{
#' library(xvm)
#' # Retrieve the path to the example file included in the package:
#' rmsd_file_path <- system.file("extdata/rmsd.xvg", package = "xvm")
#' rmsd_data <- read_xvg(rmsd_file_path) # read the xvg file using read_xvg() function
#' names(rmsd_data)
#' }
#' @export
read_xvg <- function(xvg_files, skip_comments = TRUE) {
  if (!is.character(xvg_files)) {
    stop("xvg_files must be a character vector containing one or more XVG file paths")
  }
  missing_files <- xvg_files[!file.exists(xvg_files)]
  if (length(missing_files) > 0) {
    warning("The following files do not exist: ", paste(missing_files, collapse = ", "))
    xvg_files <- xvg_files[file.exists(xvg_files)]
  }

  if (length(xvg_files) == 0) {
    stop("No valid XVG files to read")
  }

  results <- list()
  for (file_path in xvg_files) {
    lines <- readLines(file_path)
    parsed_result <- parse_xvg(lines, skip_comments = skip_comments)
    parsed_result$metadata$file_path <- file_path
    results[[basename(file_path)]] <- parsed_result
  }

  return(results)
}


