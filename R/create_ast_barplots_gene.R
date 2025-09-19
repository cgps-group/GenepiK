#' Create Barplots of AST by Carbapenem Resistance (Gene)
#'
#' This function generates a barplot to visualize the proportion of AST
#' interpretations (S, I, R) for a set of drugs, faceted by a carbapenem
#' resistance group based on the presence of the 'Bla_Carb_acquired' gene.
#' The plot is saved as a PNG file.
#'
#' @param masterdata A data frame containing the AST data. It must include
#'   the columns 'ghru_id', 'Bla_Carb_acquired', and other specified antimicrobial columns.
#' @importFrom dplyr %>% count pull filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs facet_wrap theme element_text
#' @importFrom grDevices png dev.off
#' @importFrom utils file.exists
#' @importFrom stats filter
#' @return The function does not return a value. It prints a ggplot object to a PNG file
#'   and provides a message confirming the save location.
#'
#' @export
#'
#' @examples
#' # Assuming 'masterdata' is a data frame loaded in the environment
#' # create_ast_barplots_gene(masterdata = my_data, output_dir = "plots")
create_ast_barplots_gene <- function(masterdata) {
  # Use global variable for output directory
  if (!exists("output_dir")) {
    stop("Global variable 'output_dir' not found. Please use import_data to define it first.")
  }
  output_dir <- output_dir
  # 1. Subset relevant AST columns
  AST_data <- masterdata[c(
    "ghru_id", "Bla_Carb_acquired", "AMK", "AMP", "FEP", "CRO", "CIP",
    "COL", "GEN", "IPM", "MEM", "TZP", "SXT"
  )]

  # 2. Create resistance indicator
  AST_data$carba_resistance<-ifelse(AST_data$Bla_Carb_acquired == "-"|is.na(AST_data$Bla_Carb_acquired),
                                    "CARBA-S","CARBA-R")

  # 3. Pivot from wide to long format
  AST_data_long <- AST_data %>%
    pivot_longer(
      cols = AMK:SXT,
      names_to = "Antimicrobial",
      values_to = "Interpretation"
    )

  # 4. Set factor levels for interpretation
  AST_data_long$Interpretation <- factor(
    AST_data_long$Interpretation,
    levels = c("S", "I", "R")
  )

  # 5. Plot and save to PNG
  plot_path <- file.path(output_dir, "ast_barplots_gene_facet.png")
  png(filename = plot_path, height = 12, width = 12, res = 350, units = "in")

  print(
    ggplot(AST_data_long, aes(x = Antimicrobial, fill = Interpretation)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c(
        "R" = '#BC4B51',
        "I" = "#F4A259",
        "S" = "#8CB369"
      )) +
      labs(y = "Proportion") +
      facet_wrap(~carba_resistance, ncol = 1)
  )

  dev.off()
  message("✅ AST barplot saved to: ", plot_path)
}
