#' Count Carbapenem Gene Combinations and Plot (CARB-S for '-')
#'
#' This function counts the occurrences of each unique combination of
#' carbapenem genes in the `Bla_Carb_acquired` column, keeping combinations intact,
#' calculates percentages, saves a CSV, and generates a bar plot.
#' The "-" entries are treated as "CARB-S".
#'
#' @param masterdata A data frame containing the column `Bla_Carb_acquired`.
#' @param output_dir A character string specifying the directory to save the CSV and plot.
#' @return Invisibly returns a data frame with columns: Gene_Combination, Count, Percentage.
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs theme element_text
#' @importFrom grDevices png dev.off
#' @export
#'
#' @examples
#' count_carb_gene_combinations_plot(masterdata, "~/git_repos/GenepiK/test_output/")
count_carb_gene_combinations_plot <- function(masterdata, output_dir) {
  
  if(!"Bla_Carb_acquired" %in% colnames(masterdata)) {
    stop("masterdata must contain a column named 'Bla_Carb_acquired'")
  }

  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Replace missing/empty with "CARB-S"
  gene_col <- as.character(masterdata$Bla_Carb_acquired)
  gene_col[is.na(gene_col) | gene_col == "" | gene_col == "-"] <- "No carbapenemase"

  # Count unique combinations
  combo_counts <- as.data.frame(table(gene_col))
  colnames(combo_counts) <- c("Gene_Combination", "Count")

  # Calculate percentage relative to total isolates
  total_isolates <- nrow(masterdata)
  combo_counts$Percentage <- round((combo_counts$Count / total_isolates) * 100, 2)

  # Save CSV
  output_csv <- file.path(output_dir, "carbapenem_gene_combinations.csv")
  write.csv(combo_counts, file = output_csv, row.names = FALSE)
  message("✅ Carbapenem gene combination summary saved to: ", output_csv)

  # Create bar plot
  output_png <- file.path(output_dir, "carbapenem_gene_combinations_plot.png")
  png(filename = output_png, height = 8, width = 12, res = 300, units = "in")
  
# Explicitly print the ggplot object
print(
  ggplot(combo_counts, aes(x = reorder(Gene_Combination, -Count), y = Count, fill = Gene_Combination)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number of Isolates", fill = "Gene Combination") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position="none")
  
)
  dev.off()
  message("✅ Carbapenem gene combination plot saved to: ", output_png)

  invisible(combo_counts)
}
