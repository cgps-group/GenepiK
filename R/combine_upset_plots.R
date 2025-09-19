#' Combine Multiple Upset Plots into a Single Image
#'
#' This function reads a set of pre-saved Upset plots (PNG files),
#' combines them into a single image, and saves the final composite plot.
#' It's useful for creating a multi-paneled figure for publication or presentation.
#'
#' @param plot_dir A character string specifying the directory where the plots are located.
#' @param output_dir A character string specifying the directory where the combined plot will be saved.
#' @param plot_filenames A character vector of the names of the PNG files to be combined.
#' @param output_filename A character string for the name of the final combined PNG file.
#' @importFrom magick image_read image_append image_write
#' @return The function does not return a value. It saves a single combined PNG file
#'   in the specified output directory and prints a message upon completion.
#'
#' @export
#'
#' @examples
#' # Assuming the plots are saved in a folder named 'plots/'
#' # and your working directory is set to the project root.
#' # plot_files <- c(
#' #   "virulence_upset_CARBA-S.png",
#' #   "virulence_upset_CARBA-R.png",
#' #   "resistance_upset_CARBA-S.png",
#' #   "resistance_upset_CARBA-R.png"
#' # )
#' # combine_upset_plots(
#' #   plot_dir = "plots",
#' #   output_dir = "plots",
#' #   plot_filenames = plot_files,
#' #   output_filename = "combined_upset_plot.png"
#' # )
combine_upset_plots <- function(plot_dir, output_dir, plot_filenames, output_filename) {

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create a full path for each plot file
  plot_paths <- file.path(plot_dir, plot_filenames)

  # Check if all files exist
  if (!all(file.exists(plot_paths))) {
    missing_files <- plot_filenames[!file.exists(plot_paths)]
    stop("The following plot files were not found: ", paste(missing_files, collapse = ", "))
  }

  # Read the saved plots as magick objects
  plot_images <- image_read(plot_paths)

  # Combine the images. We'll arrange them in a 2x2 grid.
  # First, append the first two horizontally.
  row1 <- image_append(plot_images[1:2], stack = FALSE)
  # Then, append the next two horizontally.
  row2 <- image_append(plot_images[3:4], stack = FALSE)

  # Finally, append the two rows vertically.
  combined_plot <- image_append(c(row1, row2), stack = TRUE)

  # Save the final combined plot
  output_path <- file.path(output_dir, output_filename)
  image_write(combined_plot, path = output_path, format = "png")

  message("✅ Combined plot saved to: ", output_path)
}
