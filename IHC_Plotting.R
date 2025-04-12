# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load data (assumes dataset is available)
data <- mia_6mo_hipp

# Print column names to verify
print(colnames(data))

# Clean column names to make them R-compliant
colnames(data) <- make.names(colnames(data), unique = TRUE)
print(colnames(data))  # Check column names after cleaning

# Check if "Genotype" exists as a column
if (!"Genotype" %in% colnames(data)) {
  stop("Error: 'Genotype' column not found. Check the column names in your dataset.")
}

# Define valid Genotype values
valid_genotypes <- c("MIA_WT;5XFAD_HET", "MIA_HET;5XFAD_HET", "MIA_WT;5XFAD_WT")

# Filter the dataset for Genotype values
filtered_data <- data %>%
  filter(Genotype %in% valid_genotypes) %>%
  mutate(Genotype = factor(Genotype, levels = valid_genotypes))

# Check filtered data
print(head(filtered_data))

# Calculate summary statistics
summary_stats <- filtered_data %>%
  group_by(Genotype) %>%
  summarize(
    mean_value = mean(Stain2Strong, na.rm = TRUE),
    se_value = sd(Stain2Strong, na.rm = TRUE) / sqrt(n()),  # Standard error calculation
    .groups = 'drop'
  )

# Print summary statistics
print(summary_stats)

# Dynamically determine y-axis limits
y_min <- min(filtered_data$Stain2Strong, na.rm = TRUE)
y_max <- max(filtered_data$Stain2Strong, na.rm = TRUE)
y_breaks <- seq(y_min, y_max, length.out = 5)  # Generate 5 evenly spaced tick marks

# Create the plot with automatic scaling
ggplot() +
  geom_point(data = filtered_data, 
             aes(x = Genotype, y = Stain2Strong, color = Mouse, fill = Slide), 
             position = position_dodge(width = 0.6), size = 2, shape = 21, alpha = 0.8) +
  geom_point(data = summary_stats, aes(x = Genotype, y = mean_value), 
             size = 3, color = "black") +
  geom_errorbar(data = summary_stats, aes(x = Genotype, ymin = mean_value - se_value, ymax = mean_value + se_value), 
                width = 0.2, color = "black") +
  scale_y_continuous(breaks = y_breaks, limits = c(y_min, y_max)) +  # Automatically scale y-axis
  scale_x_discrete(labels = c("MIA_WT;5XFAD_HET" = "MIA_WT;5XFAD_HET", "MIA_HET;5XFAD_HET" = "MIA_HET;5XFAD_HET",
                              "MIA_WT;5XFAD_WT" = "MIA_WT;5XFAD_WT")) +
  scale_color_manual(values = c("44C" = "red", "55C" = "blue", "6FS" = "green", "7A0" = "purple", 
                                "7D0" = "orange", "8D4-25" = "yellow", "DF2" = "gray", 
                                "F80-22" = "pink", "FCB-23" = "deepskyblue", "OC2-28" = "olivedrab")) +
  scale_fill_gradient(low = "white", high = "black") +  # Continuous gradient for Slide
  labs(title = "AB Strong Stain Area by MIA Genotype + 5XFAD KO", 
       x = "Genotype", 
       y = "NAB228 Plaque Mean Strong Stain Area (μm²)", 
       fill = "Slide (Continuous)") +
  theme_minimal()
