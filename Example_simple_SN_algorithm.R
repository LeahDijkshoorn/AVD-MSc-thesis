#### COST FUNCTIONS ####

# Mean-based cost function
MeanCostFunction <- function(segment, penalty, numChangepoints) {
  meanValue <- mean(segment)
  # Apply penalty based on the number of changepoints
  return(sum((segment - meanValue)^2) + penalty * numChangepoints)
}

# MLE-based cost function for linear model
mleLinearModelCostFunction <- function(segment, penalty, numChangepoints) {
  if (length(segment) <= 2) {
    # Apply penalty directly if not enough data to fit a model
    return(penalty * numChangepoints)
  }
  
  x <- seq_along(segment)
  fit <- lm(segment ~ x)
  residuals <- residuals(fit)
  sigma2_hat <- sum(residuals^2) / (length(segment) - 2)
  nll <- -length(segment) / 2 * log(2 * pi * sigma2_hat) - 1 / (2 * sigma2_hat) * sum(residuals^2)
  
  # Include the penalty for the number of changepoints
  return(-nll + penalty * numChangepoints)
}


#### SIMULATED DATA ####

# No changepoints

# Test data: a constant sequence
data <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
time <- 1:10
maxChangepoints <- 3
penalty <- 100

# Run the segmentation function
result <- segmentNeighbourhood(data, maxChangepoints, mleLinearModelCostFunction, penalty)
result

# Set penalty
penalty <- 10 

# Simple simulated data with two change points
set.seed(123)
data <- c(rnorm(50, mean = 0), rnorm(50, mean = 5), rnorm(50, mean = 10))

# Plot
plot(data, type = 'o', col = 'blue', main = 'Simple simulated Data for Segmentation', xlab = 'Index', ylab = 'Value')
abline(v = 50, col = "red", lty = 2)
abline(v = 100, col = "red", lty = 2)

result <- segmentNeighbourhood(data, 3, MeanCostFunction, penalty)
result_2 <- segmentNeighbourhood(data, 3, mleLinearModelCostFunction, penalty)


### SIMULATED DATA
# Resembles exaggerated, simple version of trend observed in donor data

# Generate first and third segment
segment1 <- rnorm(50, mean = 0)
segment3 <- rnorm(50, mean = 0)

# Generate second segment
x <- seq(1, 50)
slope <- -0.08  # Setting a slope for visible negative trend
intercept <- 2  # Initial intercept to create overall declining trend
noise_level <- 0.5
segment2_line <- slope * x + intercept
segment2 <- segment2_line + rnorm(50, sd = noise_level)

# Adjust second segment to have mean 0
segment2 <- segment2 - mean(segment2)

# Concatenate all segments
data_trend <- c(segment1, segment2, segment3)

### Bespoke Segment Neighbourhood Algorithm
result_trend_mean <- segmentNeighbourhood(data_trend, 4, MeanCostFunction, penalty)
result_trend_LM <- segmentNeighbourhood(data_trend, 4, mleLinearModelCostFunction, penalty)
result_trend_LM_window  <- segmentNeighbourhood(data_trend, 4, mleLinearModelCostFunction, penalty, minWindowSize = 40) #Much faster!

# Create a data frame
df <- data.frame(
  y = data_trend,
  x = 1:150,
  segment = factor(c(rep(1, 50), rep(2, 50), rep(3, 50)))
)

# Fit separate linear models for each segment
model1 <- lm(y ~ x, data = df[df$segment == "1", ])
model2 <- lm(y ~ x, data = df[df$segment == "2", ])
model3 <- lm(y ~ x, data = df[df$segment == "3", ])

# Add predictions
df$fitted <- NA
df$fitted[df$segment == "1"] <- predict(model1, newdata = df[df$segment == "1", ])
df$fitted[df$segment == "2"] <- predict(model2, newdata = df[df$segment == "2", ])
df$fitted[df$segment == "3"] <- predict(model3, newdata = df[df$segment == "3", ])

# Plotting
ggplot(df, aes(x = x)) +
  geom_line(aes(y = y)) +
  geom_point(aes(y = y)) + 
  geom_line(data = df[df$segment == "1", ], aes(y = fitted), color = "blue") +
  geom_line(data = df[df$segment == "2", ], aes(y = fitted), color = "blue") +
  geom_line(data = df[df$segment == "3", ], aes(y = fitted), color = "blue") +
  geom_vline(xintercept = c(50, 100), linetype = "dashed", color = "red") +  
  labs(x = "Index",
       y = "Value") +
  theme_minimal()

### Changepoint package
library(changepoint)

# PELT method
cpt_pelt <- cpt.mean(data_trend, method = "PELT")

# Data and changepoints (and start and end points)
pelt_cpts <- c(1, cpt_pelt@cpts, length(data_trend))  # Ensure the full range

# Calculate means
pelt_segments <- data.frame(
  start = pelt_cpts[-length(pelt_cpts)],
  end = pelt_cpts[-1] - 1,
  mean = sapply(2:length(pelt_cpts), function(i) {
    mean(data_trend[(pelt_cpts[i-1]):(pelt_cpts[i]-1)])
  })
)

# Actual changepoints minus the final two points (always includes the final index twice)
pelt_cpts_to_plot <- cpt_pelt@cpts[-length(cpt_pelt@cpts)]

# Plot
ggplot(data.frame(Index = 1:length(data_trend), Value = data_trend), aes(x = Index, y = Value)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = pelt_cpts_to_plot, col = "red", linetype = "dashed") +
  geom_segment(data = pelt_segments, aes(x = start, xend = end, y = mean, yend = mean), col = "blue") +
  theme_minimal()

# SegNeigh method
cpt_segneigh <- cpt.mean(data_trend, method = "SegNeigh", penalty = "SIC")

# Data and changepoints
segneigh_cpts <- c(1, cpt_segneigh@cpts, length(data_trend))

# Calculate means
segneigh_segments <- data.frame(
  start = segneigh_cpts[-length(segneigh_cpts)],
  end = segneigh_cpts[-1] - 1,
  mean = sapply(2:length(segneigh_cpts), function(i) {
    mean(data_trend[(segneigh_cpts[i-1]):(segneigh_cpts[i]-1)])
  })
)

segneigh_cpts_to_plot <- cpt_segneigh@cpts[-length(cpt_segneigh@cpts)]

# Plot
ggplot(data.frame(Index = 1:length(data_trend), Value = data_trend), aes(x = Index, y = Value)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = segneigh_cpts_to_plot, col = "red", linetype = "dashed") +
  geom_segment(data = segneigh_segments, aes(x = start, xend = end, y = mean, yend = mean), col = "blue") +
  theme_minimal()

# BinSeg method
cpt_binseg <- cpt.mean(data_trend, method = "BinSeg")

# Data and changepoints
binseg_cpts <- c(1, cpt_binseg@cpts, length(data_trend))

binseg_cpts_to_plot <- cpt_binseg@cpts[-length(cpt_binseg@cpts)]

# Calculate means
binseg_segments <- data.frame(
  start = binseg_cpts[-length(binseg_cpts)],
  end = binseg_cpts[-1] - 1,
  mean = sapply(2:length(binseg_cpts), function(i) {
    mean(data_trend[(binseg_cpts[i-1]):(binseg_cpts[i]-1)])
  })
)

# Plot
ggplot(data.frame(Index = 1:length(data_trend), Value = data_trend), aes(x = Index, y = Value)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = binseg_cpts_to_plot, col = "red", linetype = "dashed") +
  geom_segment(data = binseg_segments, aes(x = start, xend = end, y = mean, yend = mean), col = "blue") +
  theme_minimal()

### Segmented package
library(segmented)

# Fit a linear model
lin.model <- lm(y ~ x, data = df)

# Using segmented to detect breakpoints in the linear model
seg.model <- segmented(lin.model, seg.Z = ~x, psi = c(50, 100))

# Check the results
summary(seg.model)

# Plot the segmented model
df$predicted_y <- predict(seg.model, newdata=data.frame(x=df$x))

# Create the ggplot
p <- ggplot(df, aes(x=x, y=y)) +
  geom_line() +
  geom_point() +  # Plot the original data points
  geom_line(aes(y=predicted_y), color="blue") +  # Add the segmented line
  labs(x="Index", y="Value") +
  theme_minimal()

# Extract breakpoints from the model and add to plot
breakpoints <- seg.model$psi[, "Est."]
for(bp in breakpoints) {
  p <- p + geom_vline(xintercept=bp, color="red", linetype="dashed")
}

# Print the plot
print(p)

### Strucchange package
library(strucchange)

strucchange.model <- breakpoints(y ~ x, h = 0.1, data = df)
summary(strucchange.model)

plot(data_trend)
lines(fitted(strucchange.model), col = 2)
lines(confint(strucchange.model, breaks = 4))

breakdates(strucchange.model)
coef(strucchange.model)
