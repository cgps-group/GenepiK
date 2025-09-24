#' Create Pivot Table of ST vs Carbapenem Genes
#'
#' This function creates a pivot table of sequence types (ST) versus carbapenemase genes
#' found in the `Bla_Carb_acquired` column. It counts occurrences of each gene,
#' adds row totals (per ST) and column totals (across STs), and saves the result as a CSV.
#'
#' @param masterdata A data frame containing at least the columns `ST` and `Bla_Carb_acquired`.
#' @param output_dir A character string specifying the directory to save the CSV.
#' @return Invisibly returns a data frame with ST as rows and carbapenem genes as columns,
#'         including row and column totals.
#' @importFrom dplyr count mutate summarise across bind_rows
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' pivot <- create_ST_gene_pivot(masterdata, "~/git_repos/GenepiK/test_output/")
#' head(pivot)
create_ST_carb_gene_pivot <- function(masterdata, output_dir) {
  
  # Check required columns
  required_cols <- c("ST", "Bla_Carb_acquired")
  missing <- setdiff(required_cols, colnames(masterdata))
  if (length(missing) > 0) {
    stop("masterdata must contain columns: ", paste(missing, collapse = ", "))
  }
  
  # Create pivot
  pivot <- masterdata %>%
    count(ST, Bla_Carb_acquired) %>%
    pivot_wider(
      names_from = Bla_Carb_acquired,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(Total = rowSums(across(where(is.numeric))))
  
  # Column totals
  totals <- pivot %>%
    summarise(across(where(is.numeric), sum)) %>%
    mutate(ST = "Total")
  
  # Bind total row
  pivot <- bind_rows(pivot, totals)
  
  # Save to CSV
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_csv <- file.path(output_dir, "ST_vs_carbapenem_genes.csv")
  write.csv(pivot, file = output_csv, row.names = FALSE)
  message("✅ Pivot table saved to: ", output_csv)
  
  invisible(pivot)
}
