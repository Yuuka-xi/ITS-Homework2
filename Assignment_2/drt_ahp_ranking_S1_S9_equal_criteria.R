# AHP ranking for the new S1-S9 Cottbus DRT scenarios
#
# Workflow:
# 1. Read the S1-S9 scenario summary.
# 2. Plot one raw-result bar chart for each criterion.
# 3. Build one pairwise comparison matrix for alternatives under each criterion.
# 4. Use an all-ones criteria matrix, meaning all criteria are equally important.
# 5. Compute final AHP scores and plot the ranking as a pie chart.

assignment_dir <- "C:/Study/S2/ITS/ITS-Homework2/Assignment_2"
base_dir <- file.path(assignment_dir, "Task3-Cottbus")
input_csv <- file.path(
  base_dir,
  "output",
  "new_scenarios_S1_S9",
  "new_scenarios_metrics_summary.csv"
)
output_dir <- file.path(
  base_dir,
  "output",
  "new_scenarios_S1_S9",
  "ahp_equal_criteria_outputs"
)
plot_dir <- file.path(output_dir, "plots")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

old_png <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
if (length(old_png) > 0) {
  file.remove(old_png)
}

metrics <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)
alternatives <- metrics$Set
rownames(metrics) <- alternatives

# -------------------------------
# 1. AHP helper functions
# -------------------------------

getWeights <- function(mat) {
  eigen_result <- eigen(mat)
  max_index <- which.max(Re(eigen_result$values))
  eigenvec <- Re(eigen_result$vectors[, max_index])
  weights <- abs(eigenvec)
  weights <- weights / sum(weights)
  return(weights)
}

AHP <- function(criteria_matrix, alternatives_mats) {
  criteria_w <- getWeights(criteria_matrix)
  alt_weights <- sapply(alternatives_mats, getWeights)
  result <- alt_weights %*% criteria_w
  return(as.vector(result))
}

consistencyRatio <- function(mat) {
  n <- nrow(mat)
  if (n <= 2) {
    return(0)
  }

  lambda_max <- max(Re(eigen(mat)$values))
  ci <- (lambda_max - n) / (n - 1)

  ri_table <- c(
    "1" = 0.00, "2" = 0.00, "3" = 0.58, "4" = 0.90,
    "5" = 1.12, "6" = 1.24, "7" = 1.32, "8" = 1.41,
    "9" = 1.45, "10" = 1.49
  )

  ri <- ri_table[as.character(n)]
  if (is.na(ri) || ri == 0) {
    return(0)
  }

  return(ci / ri)
}

# Convert relative performance gaps to Saaty's 1-9 scale.
gapToSaaty <- function(relative_gap) {
  if (relative_gap < 0.01) {
    return(1)
  } else if (relative_gap < 0.03) {
    return(2)
  } else if (relative_gap < 0.07) {
    return(3)
  } else if (relative_gap < 0.12) {
    return(4)
  } else if (relative_gap < 0.20) {
    return(5)
  } else if (relative_gap < 0.30) {
    return(6)
  } else if (relative_gap < 0.45) {
    return(7)
  } else if (relative_gap < 0.65) {
    return(8)
  } else {
    return(9)
  }
}

buildSaatyMatrix <- function(values, direction = "cost") {
  n <- length(values)
  mat <- matrix(1, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        mat[i, j] <- 1
      } else {
        if (direction == "cost") {
          better_i <- values[i] < values[j]
          better_j <- values[j] < values[i]
          better_value <- min(values[i], values[j])
          worse_value <- max(values[i], values[j])
        } else if (direction == "benefit") {
          better_i <- values[i] > values[j]
          better_j <- values[j] > values[i]
          better_value <- max(values[i], values[j])
          worse_value <- min(values[i], values[j])
        } else {
          stop("direction must be either 'cost' or 'benefit'")
        }

        if (better_value == 0 && worse_value == 0) {
          scale_value <- 1
        } else if (better_value == 0) {
          scale_value <- 9
        } else {
          relative_gap <- abs(worse_value - better_value) / abs(better_value)
          scale_value <- gapToSaaty(relative_gap)
        }

        if (better_i) {
          mat[i, j] <- scale_value
        } else if (better_j) {
          mat[i, j] <- 1 / scale_value
        } else {
          mat[i, j] <- 1
        }
      }
    }
  }

  rownames(mat) <- alternatives
  colnames(mat) <- alternatives
  return(mat)
}

# -------------------------------
# 2. Criteria and matrices
# -------------------------------

criteria_values <- list(
  avg_wait = metrics[["Avg wait"]],
  p95_wait = metrics[["P95 wait"]],
  total_travel = metrics[["Total travel"]],
  rejections = metrics[["Rejections"]],
  idle_share = metrics[["Idle share"]],
  dp_dt = metrics[["d_p/d_t"]]
)

criteria_directions <- c(
  avg_wait = "cost",
  p95_wait = "cost",
  total_travel = "cost",
  rejections = "cost",
  idle_share = "cost",
  dp_dt = "benefit"
)

criteria_labels <- c(
  avg_wait = "Average waiting time",
  p95_wait = "P95 waiting time",
  total_travel = "Average total travel time",
  rejections = "Rejected requests",
  idle_share = "Minimum idle vehicle share",
  dp_dt = "Passenger-km / vehicle-km"
)

criteria_units <- c(
  avg_wait = "Minutes",
  p95_wait = "Minutes",
  total_travel = "Minutes",
  rejections = "Requests",
  idle_share = "Share",
  dp_dt = "d_p/d_t"
)

APCL <- list()
for (criterion in names(criteria_values)) {
  APCL[[criterion]] <- buildSaatyMatrix(
    criteria_values[[criterion]],
    criteria_directions[[criterion]]
  )
}

criteria <- names(APCL)

# All criteria are treated as equally important.
CWPC <- matrix(
  1,
  nrow = length(criteria),
  ncol = length(criteria),
  dimnames = list(criteria, criteria)
)

criteria_weights <- getWeights(CWPC)
names(criteria_weights) <- criteria

alternative_weights <- sapply(APCL, getWeights)
final_score <- AHP(CWPC, APCL)

ranking <- data.frame(
  Set = alternatives,
  Fleet = metrics$Fleet,
  Seats = metrics$Seats,
  Max_wait = metrics[["Max wait"]],
  Avg_wait = metrics[["Avg wait"]],
  P95_wait = metrics[["P95 wait"]],
  Total_travel = metrics[["Total travel"]],
  Rejections = metrics$Rejections,
  Idle_share = metrics[["Idle share"]],
  d_p_d_t = metrics[["d_p/d_t"]],
  AHP_score = as.vector(final_score),
  rank = rank(-as.vector(final_score), ties.method = "min"),
  stringsAsFactors = FALSE
)
ranking <- ranking[order(ranking$rank), ]

consistency <- data.frame(
  matrix_name = c("CWPC_criteria", names(APCL)),
  consistency_ratio = c(
    consistencyRatio(CWPC),
    sapply(APCL, consistencyRatio)
  ),
  stringsAsFactors = FALSE
)

# -------------------------------
# 3. Write AHP tables
# -------------------------------

write.csv(round(CWPC, 4), file.path(output_dir, "ahp_matrix_criteria_equal.csv"))

for (matrix_name in names(APCL)) {
  write.csv(
    round(APCL[[matrix_name]], 4),
    file.path(output_dir, paste0("ahp_matrix_", matrix_name, ".csv"))
  )
}

write.csv(
  data.frame(criteria = names(criteria_weights), weight = criteria_weights),
  file.path(output_dir, "ahp_criteria_weights_equal.csv"),
  row.names = FALSE
)

write.csv(
  alternative_weights,
  file.path(output_dir, "ahp_alternative_weights_by_criterion.csv")
)

write.csv(
  ranking,
  file.path(output_dir, "ahp_ranking_S1_S9_equal_criteria.csv"),
  row.names = FALSE
)

write.csv(
  consistency,
  file.path(output_dir, "ahp_consistency_ratios.csv"),
  row.names = FALSE
)

# -------------------------------
# 4. Plots
# -------------------------------

plotCriterionBar <- function(values, criterion, lower_is_better) {
  file_name <- paste0("criterion_", criterion, "_bar.png")
  png(file.path(plot_dir, file_name), width = 1200, height = 800, res = 140)
  colors <- rep("#5B9BD5", length(values))
  best_index <- if (lower_is_better) which.min(values) else which.max(values)
  colors[best_index] <- "#70AD47"
  barplot(
    values,
    names.arg = alternatives,
    col = colors,
    main = criteria_labels[[criterion]],
    ylab = criteria_units[[criterion]],
    xlab = "Scenario",
    ylim = c(0, max(values) * 1.18)
  )
  grid(nx = NA, ny = NULL, col = "gray85")
  dev.off()
}

for (criterion in names(criteria_values)) {
  plotCriterionBar(
    criteria_values[[criterion]],
    criterion,
    criteria_directions[[criterion]] == "cost"
  )
}

pie_values <- ranking$AHP_score
names(pie_values) <- ranking$Set
pie_labels <- paste0(ranking$Set, " (", round(100 * pie_values / sum(pie_values), 1), "%)")

png(file.path(plot_dir, "final_ahp_ranking_pie_equal_criteria.png"),
    width = 1200, height = 800, res = 140)
pie(
  pie_values,
  labels = pie_labels,
  col = c(
    "#5B9BD5", "#70AD47", "#FFC000", "#ED7D31", "#A5A5A5",
    "#4472C4", "#264478", "#9E480E", "#636363"
  )[seq_along(pie_values)],
  main = "AHP ranking of S1-S9 DRT scenarios"
)
dev.off()

# -------------------------------
# 5. Console output
# -------------------------------

cat("\nCriteria matrix: all criteria are equally important.\n")
print(round(CWPC, 4))

cat("\nCriteria weights:\n")
print(round(criteria_weights, 4))

cat("\nAHP final ranking:\n")
print(ranking[, c("Set", "AHP_score", "rank")])

cat("\nConsistency ratios:\n")
print(consistency)

cat("\nOutput CSV files written to:\n")
cat(output_dir, "\n")

cat("\nPlots written to:\n")
cat(plot_dir, "\n")
