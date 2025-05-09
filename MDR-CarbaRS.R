analyze_MDR_CarbR <- function(file_path, save_output = TRUE, output_prefix = "GHRU_output", show_plots = TRUE) {
  # Load required libraries
  library(readxl)
  library(dplyr)
  library(writexl)
  library(ggplot2)
  library(ggpubr)
  library(tidyr)
  
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Define antimicrobial categories
  aminoglycosides <- c("Amikacin_int", "Gentamicin_int", "Tobramycin_int")
  beta_lactams <- c("Amoxicillin_clavulanic_acid_int", "Ampicillin_int", "Aztreonam_int",
                    "Cefalexin_int", "Cefepime_int", "Cefixime_int", "Cefotaxime_int",
                    "Cefoxitin_int", "Ceftazidime_int", "Ceftriaxone_int", "Cefuroxime_int",
                    "Ertapenem_int", "Imipenem_int", "Mecillinam_int", "Meropenem_int",
                    "Piperacillin_int", "Piperacillin_tazobactam_int", "Temocillin_int",
                    "Ticarcillin_clavulanic_acid_int")
  fluoroquinolones <- c("Ciprofloxacin_int", "Levofloxacin_int", "Nalidixic_acid_int", "Norfloxacin_int")
  folate_pathway_inhibitors <- c("Trimethoprim_int", "Trimethoprim_sulfamethoxazole_int")
  
  colistin <- "Colistin_int"
  fosfomycin <- "Fosfomycin_G6P_int"
  nitrofurantoin <- "Nitrofurantoin_int"
  tigecycline <- "Tigecycline_int"
  
  # Function to check MDR phenotype
  check_mdr <- function(row) {
    aminoglycosides_resistant <- any(row[aminoglycosides] == "R", na.rm = TRUE)
    beta_lactams_resistant <- any(row[beta_lactams] == "R", na.rm = TRUE)
    fluoroquinolones_resistant <- any(row[fluoroquinolones] == "R", na.rm = TRUE)
    folate_inhibitors_resistant <- any(row[folate_pathway_inhibitors] == "R", na.rm = TRUE)
    
    colistin_resistant <- row[[colistin]] == "R"
    fosfomycin_resistant <- row[[fosfomycin]] == "R"
    nitrofurantoin_resistant <- row[[nitrofurantoin]] == "R"
    tigecycline_resistant <- row[[tigecycline]] == "R"
    
    categories_resistant <- sum(aminoglycosides_resistant, beta_lactams_resistant, 
                                fluoroquinolones_resistant, folate_inhibitors_resistant,
                                colistin_resistant, fosfomycin_resistant, 
                                nitrofurantoin_resistant, tigecycline_resistant)
    
    if (categories_resistant >= 3) {
      return("MDR")
    } else {
      return("Non-MDR")
    }
  }
  
  # Apply MDR classification
  data <- data %>%
    rowwise() %>%
    mutate(MDR_phenotype = check_mdr(cur_data())) %>%
    ungroup()
  
  # Create Carbapenem resistance column
  data <- data %>%
    mutate(any_CarbR = ifelse(
      Ertapenem_int == "R" | Imipenem_int == "R" | Meropenem_int == "R",
      "R", "S"
    ))
  
  # Calculate proportions
  mdr_plot_data <- data %>%
    group_by(any_CarbR, MDR_phenotype) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(any_CarbR) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Bar plot
  bar_plot <- ggplot(data, aes(x = any_CarbR, fill = MDR_phenotype)) +
    geom_bar(position = "fill") +
    labs(
      title = "Proportion of MDR phenotypes vs Carbapenem susceptibility",
      x = "Carbapenem susceptibility",
      y = "Proportion",
      fill = "MDR Phenotype"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("MDR" = "darkmagenta", "Non-MDR" = "turquoise4"))
  
  # Fisher’s exact test
  conTable <- table(data$MDR_phenotype, data$any_CarbR)
  results <- fisher.test(conTable)
  
  # Odds ratio data
  mdr_odds_data <- data.frame(
    Variable = "MDR_phenotype",
    OddsRatio = ifelse(is.null(results$estimate), NA, unname(results$estimate)),
    CI_Lower = ifelse(is.null(results$conf.int), NA, results$conf.int[1]),
    CI_Upper = ifelse(is.null(results$conf.int), NA, results$conf.int[2]),
    p_value = results$p.value
  ) %>%
    mutate(Significance = ifelse(p_value < 0.01, "p < 0.01", "p > 0.01"))
  
  # Odds ratio plot
  or_plot <- ggplot(mdr_odds_data, aes(x = OddsRatio, y = Variable)) +
    geom_point(aes(color = Significance), size = 3) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
    scale_x_log10() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
    labs(
      x = "Odds Ratio (log scale)",
      y = "",
      title = "Odds Ratio with 95% Confidence Intervals"
    ) +
    scale_color_manual(values = c("p < 0.01" = "blue", "p > 0.01" = "grey")) +
    theme_minimal()
  
  # Display plots automatically if requested
  if (show_plots) {
    print(bar_plot)
    print(or_plot)
  }
  
  # Optionally save outputs
  if (save_output) {
    write_xlsx(data, paste0(output_prefix, "_with_MDR_CarbR.xlsx"))
    ggsave(paste0(output_prefix, "_MDR_barplot.png"), bar_plot, width = 7, height = 5)
    ggsave(paste0(output_prefix, "_MDR_ORplot.png"), or_plot, width = 7, height = 5)
    cat("Outputs saved with prefix:", output_prefix, "\n")
  }
  
  # Return a list with key outputs
  return(list(
    mdr_plot_data = mdr_plot_data,
    fisher_test_results = results,
    MDR_bar_plot = bar_plot,
    MDR_OR_plot = or_plot,
    processed_data = data
  ))
}