#' Create Upset Plots for Resistance Determinants, Faceted by Isolate Type
#'
#' This function generates and saves a series of Upset plots, one for each
#' unique `Isolate_type` present in the data. Each plot visualizes the
#' co-occurrence of virulence factors and colors the intersections by
#' `virulence_score`. The plots are saved as PNG files.
#'
#' @param data A data frame containing the data. It must include the columns
#'   `Isolate_type`, `resistance_score`, and the resistance factor columns defined
#'   within the function (`Bla_inhR_acquired`, `Bla_ESBL_acquired`, etc.).
#' @param output_dir A character string specifying the directory where the plots will be saved.
#' @importFrom dplyr %>% count pull filter mutate across all_of filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs facet_wrap theme element_text element_blank element_rect element_line
#' @importFrom grDevices png dev.off
#' @importFrom utils file.exists
#' @importFrom ComplexUpset upset intersection_size upset_default_themes
#' @importFrom stats filter
#' @return The function does not return a value. It creates one or more PNG files
#'   in the specified output directory.
#'
#' @export
#'
#' @examples
#' # create_virulence_upset_plots(data = my_data, output_dir = "plots")
create_resistance_upset_plots <- function(data, output_dir) {
  # Define columns of interest
  resistance_classes <- c("Bla_acquired", "Bla_inhR_acquired", "Bla_ESBL_acquired", "Bla_ESBL_inhR_acquired", "Bla_Carb_acquired", "Bla_chr", "SHV_mutations", "Omp_mutations")

  # Convert "-" to FALSE, and anything else to TRUE
  data_converted <- data %>%
    mutate(across(all_of(resistance_classes), ~ . != "-"))

  # Get unique Isolate Types
  isolate_types <- unique(data_converted$Isolate_type)

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create a plot for each Isolate Type
  for (type in isolate_types) {
    # Subset the data for the current Isolate Type
    subset_data <- data_converted %>%
      filter(Isolate_type == type)

    # Create the plot
    upset_plot <- upset(
      subset_data,
      intersect = resistance_classes,
      name = "Resistance Determinants",
      width_ratio = 0.1,
      themes = upset_default_themes(
        default = theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)
        ),
        intersection_size = theme(
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "grey90")
        )
      ),
      set_sizes = FALSE,
      base_annotations = list(
        'Frequency' = intersection_size(
          aes(fill = as.factor(`resistance_score`))
        ) +
          scale_fill_manual(name = "Resistance score", values = c("0" = "#B2B09B", "1" = "mediumblue", "2" = "#43AA8B", "3" = "deeppink3"))
      )
    )

    # Save the plot
    file_name <- paste0("resistance_upset_", type, ".png")
    plot_path <- file.path(output_dir, file_name)
    png(filename = plot_path, height = 12, width = 18, res = 350, units = "in")
    print(upset_plot)
    dev.off()
    message("✅ Plot for '", type, "' saved to: ", plot_path)
  }
}
