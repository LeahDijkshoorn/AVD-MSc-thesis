segmentNeighbourhood <- function(data, maxChangepoints, costFunction, penalty, minWindowSize = 1) {
  n <- length(data)
  
  # Initialise matrices to store costs and positions of last changepoints
  costMatrix <- matrix(Inf, n + 1, maxChangepoints + 1)
  lastChangepointMatrix <- matrix(-1, n + 1, maxChangepoints + 1)
  totalCost <- rep(Inf, maxChangepoints + 1)
  
  # Initialise the cost of zero segments, no data
  costMatrix[1, 1] <- 0
  
  # Dynamic programming to fill the matrices
  for (q in 1:(maxChangepoints + 1)) {
    for (i in 2:(n + 1)) {
      if (q == 1) {
        # Only calculate if segment is large enough to be valid
        if (i - 1 >= minWindowSize) {
          costMatrix[i, q] <- costFunction(data[1:(i - 1)], penalty, q - 1)
        }
      } else {
        # Ensure j starts at least minWindowSize before i
        for (j in 1:(i - 1)) {
          if (i - j >= minWindowSize) {
            currentCost <- costFunction(data[j:(i - 1)], penalty, q - 2) + costMatrix[j, q - 1]
            if (!is.na(currentCost) && currentCost < costMatrix[i, q]) {
              costMatrix[i, q] <- currentCost
              lastChangepointMatrix[i, q] <- j
            }
          }
        }
      }
    }
    totalCost[q] <- costMatrix[n+1, q] + penalty * (q - 1)
  }
  
  # Determine optimal number of changepoints based on total cost
  optimalChangepoints <- which.min(totalCost) - 1
  segments <- numeric(optimalChangepoints)
  
  # Reconstruct optimal segmentation
  currentSegment <- optimalChangepoints
  currentPosition <- n + 1
  while (currentSegment > 0) {
    segments[currentSegment] <- lastChangepointMatrix[currentPosition, currentSegment + 1] - 1 # First run starts at end of data
    currentPosition <- lastChangepointMatrix[currentPosition, currentSegment + 1] # Update current position to end of previous segment
    currentSegment <- currentSegment - 1
  }
  
  return(list(changepoints = segments, cost = totalCost[optimalChangepoints + 1]))
}
