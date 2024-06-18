# Assuming 'genotypes' is your genotype matrix
# Make sure to replace 'your_gbs_file.vcf' with the actual path to your GBS VCF file
# Install and load the required packages
install.packages(c("VariantAnnotation", "ggplot2"))
library(VariantAnnotation)
library(ggplot2)
# Load the GBS VCF file
vcf_file <- "your_gbs_file.vcf"
vcf <- readVcf(vcf_file, "gbs")

# Extract genotype matrix
genotypes <- geno(vcf)

# Check if the genotype matrix is empty
if (length(genotypes) == 0 || all(sapply(genotypes, function(mat) length(dim(mat)) == 0))) {
  stop("Genotype matrix is empty. Check your GBS VCF file.")
}

# Calculate heterozygosity for each sample (line)
heterozygosity <- apply(genotypes[[1]], 2, function(col) {
  heterozygous_count <- sum(col %in% c("0/1", "1/0"))
  total_genotypes <- length(col)
  
  if (total_genotypes == 0) {
    cat("Warning: Empty genotype column.\n")
    return(NA)
  } else {
    return(heterozygous_count / total_genotypes)
  }
})

# Create a data frame with Sample and Heterozygosity columns
result <- data.frame(Sample = colnames(genotypes[[1]]), Heterozygosity = heterozygosity)

# Filter out NA values
result <- result[!is.na(result$Heterozygosity), ]

# Define ranges and colors for heterozygosity values
ranges <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
colors <- c("red", "orange", "yellow", "green", "blue")

# Create a new variable indicating the range for each heterozygosity value
result$Heterozygosity_Range <- cut(result$Heterozygosity, breaks = ranges, include.lowest = TRUE, labels = colors)

# Create a histogram without explicit bins and using different colors for each range
colored_histogram_plot <- ggplot(result, aes(x = Heterozygosity, fill = Heterozygosity_Range)) +
  geom_histogram(color = "black", alpha = 0.7) +
  labs(title = "Colored Histogram of Heterozygosity Ranges", x = "Heterozygosity", y = "Count") +
  theme(legend.title = element_blank())

# Print the colored histogram plot
print(colored_histogram_plot)

# Save the result data frame to a CSV file
write.csv(result, "heterozygosity_ranges_colored_histogram_results_gbs.csv", row.names = FALSE)

# Save the colored histogram plot to a PNG file
ggsave("heterozygosity_ranges_colored_histogram_plot_gbs.png", plot = colored_histogram_plot, width = 10, height = 6)


