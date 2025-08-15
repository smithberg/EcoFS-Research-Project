# Watson U² Test for Pine vs Fir Aspect Distribution

# Step 1: Install and load required packages
# install.packages("circular")  # Run this if you haven't installed it
library(circular)

# Step 2: Load your data
# Assuming your CSV file is in your working directory
data <- read.csv("raw data.csv")

# Step 3: Prepare the data for circular analysis
# Extract aspects and tree counts
aspects <- data$Aspect....
pine_counts <- data$X..Pine
fir_counts <- data$X..Fir

# Step 4: Create weighted aspect vectors for each species
# We need to repeat each aspect by the number of trees of that species

# First, clean the data - remove any rows with NA or negative counts
valid_data <- !is.na(aspects) & !is.na(pine_counts) & !is.na(fir_counts) & 
  pine_counts >= 0 & fir_counts >= 0

clean_aspects <- aspects[valid_data]
clean_pine_counts <- pine_counts[valid_data]
clean_fir_counts <- fir_counts[valid_data]

# For Pine trees - only include zones that have pine trees (count > 0)
pine_zones <- clean_pine_counts > 0
pine_aspects_to_repeat <- clean_aspects[pine_zones]
pine_counts_to_use <- clean_pine_counts[pine_zones]

# Create weighted vector for pines
pine_aspects <- rep(pine_aspects_to_repeat, pine_counts_to_use)

# For Fir trees - only include zones that have fir trees (count > 0)
fir_zones <- clean_fir_counts > 0
fir_aspects_to_repeat <- clean_aspects[fir_zones]
fir_counts_to_use <- clean_fir_counts[fir_zones]

# Create weighted vector for firs
fir_aspects <- rep(fir_aspects_to_repeat, fir_counts_to_use)

# Check if we have data for both species
if(length(pine_aspects) == 0) {
  stop("No Pine trees found in the data!")
}
if(length(fir_aspects) == 0) {
  stop("No Fir trees found in the data!")
}

# Step 5: Convert to circular objects (degrees to radians)
pine_circular <- circular(pine_aspects, units="degrees", template="geographic")
fir_circular <- circular(fir_aspects, units="degrees", template="geographic")

# Step 6: Perform Watson U² test
print("Performing Watson U² test...")
watson_test <- watson.two.test(pine_circular, fir_circular)

# Step 7: Display results with error handling
print("=== Watson U² Test Results ===")

# Check what the test actually returned
print("Test object structure:")
print(str(watson_test))

# Handle results more carefully
if(!is.null(watson_test$statistic) && is.numeric(watson_test$statistic)) {
  print(paste("Test Statistic (U²):", round(watson_test$statistic, 4)))
} else {
  print("Test Statistic: Unable to extract")
}

if(!is.null(watson_test$p.value) && is.numeric(watson_test$p.value)) {
  p_val <- watson_test$p.value
  print(paste("p-value:", round(p_val, 4)))
  
  # Interpretation
  if(p_val < 0.05) {
    print("CONCLUSION: Pine and Fir trees have SIGNIFICANTLY DIFFERENT aspect distributions (p < 0.05)")
    print("This suggests these species prefer different slope orientations.")
  } else {
    print("CONCLUSION: No significant difference in aspect distributions (p ≥ 0.05)")
    print("Pine and Fir trees appear to be distributed similarly across slope aspects.")
  }
} else {
  print("p-value: Unable to extract numeric p-value")
  print("Raw p-value object:")
  print(watson_test$p.value)
  
  # Try alternative interpretation
  if(length(watson_test) > 0) {
    print("Full test results:")
    print(watson_test)
  }
}

print(paste("Sample size Pine:", length(pine_aspects)))
print(paste("Sample size Fir:", length(fir_aspects)))

# Step 8: Additional descriptive statistics
cat("\n=== Descriptive Statistics ===\n")

# Calculate mean direction and concentration for each species
pine_mean <- mean.circular(pine_circular)
fir_mean <- mean.circular(fir_circular)

pine_concentration <- rho.circular(pine_circular)
fir_concentration <- rho.circular(fir_circular)

cat("Pine trees:\n")
cat(paste("  Mean aspect:", round(as.numeric(pine_mean), 1), "degrees\n"))
cat(paste("  Concentration (0=uniform, 1=highly directional):", round(pine_concentration, 3), "\n"))

cat("Fir trees:\n")
cat(paste("  Mean aspect:", round(as.numeric(fir_mean), 1), "degrees\n"))
cat(paste("  Concentration:", round(fir_concentration, 3), "\n"))

# Step 9: Create circular plots for visualization
par(mfrow=c(1,2))

# Plot Pine aspect distribution
plot(pine_circular, main="Pine Tree Aspect Distribution", 
     col="darkgreen", pch=16, cex=0.8)
arrows.circular(pine_mean, length=0.1, col="red", lwd=3)

# Plot Fir aspect distribution  
plot(fir_circular, main="Fir Tree Aspect Distribution", 
     col="forestgreen", pch=16, cex=0.8)
arrows.circular(fir_mean, length=0.1, col="red", lwd=3)

# Reset plotting parameters
par(mfrow=c(1,1))

# Step 10: Optional - Rose diagrams for better visualization
# Create rose diagrams (circular histograms)
par(mfrow=c(1,2))

rose.diag(pine_circular, bins=16, main="Ponderosa pine - Rose Diagram", 
          col="lightgreen", prop=2)

rose.diag(fir_circular, bins=16, main="Douglass fir - Rose Diagram", 
          col="lightblue", prop=2)

par(mfrow=c(1,1))

cat("\n=== Test Interpretation ===\n")
cat("The Watson U² test compares the circular distributions of aspects\n")
cat("where Pine and Fir trees are found, weighted by tree abundance.\n")
cat("Red arrows in plots show mean preferred direction.\n")
cat("Rose diagrams show frequency distribution around the compass.\n")