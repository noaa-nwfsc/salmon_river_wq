# Explore visual patterns in thermal metrics output
# Generated script using GitHub Copilot GPT-5 mini 7/30/26 AHF

library(data.table)
library(ggplot2)

metrics <- fread('salmon_river_wq/data/thermal_metrics_empirical.csv')
names(metrics)
# Ensure Year is integer
metrics[, Year := as.integer(Year)]

### 10-6-26
mets <- c("q05", "q50", "q95", "min_7d_min", "mean_7d_mean", "max_7d_max", 
 "met_sd", "median_weekly_range",
 "exceed_1st_week", "below_1st_week", "max_7d_week", "min_7d_week",
 "days_above", "days_below", "days_in_range",
 "mean_consec_above", "mean_consec_below", "mean_consec_inrange", "degree_days")

for(i in 1:length(mets)) {
  met <- mets[i]
  cat("Plotting metric:", met, "\n")
  p <- ggplot(metrics, aes(x = Year, y = .data[[met]], color = LHS_Code, group = LHS_Code)) +
    geom_point(alpha = 0.6, size = 0.8, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
    facet_wrap(~ SiteCode, scales = "free_y") +
    labs(y = met) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  ggsave(filename = paste0("salmon_river_wq/outputs/metric_plot_", met, ".png"), plot = p, width = 7, height = 7)
}

metrics <- fread('salmon_river_wq/data/depth_metrics_empirical.csv')
names(metrics)
# Ensure Year is integer
metrics[, Year := as.integer(Year)]

mets <- c("q05", "q50", "q95", "min_7d_min", "mean_7d_mean", "max_7d_max", 
 "met_sd", "median_weekly_range", "max_7d_week", "min_7d_week")

for(i in 1:length(mets)) {
  met <- mets[i]
  cat("Plotting metric:", met, "\n")
  p <- ggplot(metrics, aes(x = Year, y = .data[[met]], color = LHS_Code, group = LHS_Code)) +
    geom_point(alpha = 0.6, size = 0.8, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
    facet_wrap(~ SiteCode, scales = "free_y") +
    labs(y = met) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  ggsave(filename = paste0("salmon_river_wq/outputs/metric_depth_", met, ".png"), plot = p, width = 7, height = 7)
}

##################### other stuff to explore #####

# Small-multiples: thermal metric by Year, facet by SiteCode, color by LHS_Code
# Time series with a modest point size so many-year plots remain readable
ggplot(metrics, aes(x = Year, y = q95, group = LHS_Code, color = LHS_Code)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 0.4, na.rm = TRUE) +
  facet_wrap(~ SiteCode, scales = "free_y") +
  labs(y = "Thermal metric", title = "Thermal metric over Years — facets by SiteCode, color = LHS_Code") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


# Plot: scatter + GAM smoother (k = 3) by LHS_Code, faceted by SiteCode
# Using k = 3 to avoid errors for groups with few unique years

 # Identify groups with at least 4 unique years and filter
  keep_groups <- metrics[, .(n_years = uniqueN(Year)), by = .(SiteCode, LHS_Code)][n_years >= 4]
  metrics_filt <- metrics[keep_groups, on = .(SiteCode, LHS_Code)]
  # Informative counts
  cat("Original rows:", nrow(metrics), "\n")
  cat("Rows after filtering groups with >=4 unique years:", nrow(metrics_filt), "\n")
  cat("Number of SiteCode×LHS_Code groups kept:", nrow(keep_groups), "\n")

ggplot(metrics_filt, aes(x = Year, y = days_in_range, color = LHS_Code, group = LHS_Code)) +
  geom_point(alpha = 0.6, size = 0.8, na.rm = TRUE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 2), se = FALSE, na.rm = TRUE) +
  facet_wrap(~ SiteCode, scales = "free_y") +
  labs(title = "Year vs Thermal metric with GAM smoother (filtered groups) — faceted by SiteCode",
       y = "Thermal metric") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


# q95 heatmap
ggplot(metrics, aes(x = Year, y = SiteCode, fill = q95)) +
  geom_tile(na.rm = TRUE) +
  facet_wrap(~ LHS_Code, ncol = 4, scales = "free_y") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "q95 by Year and SiteCode — faceted by LHS_Code", x = "Year", y = "SiteCode", fill = "q95") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", strip.text = element_text(size = 8))



# --- Correlation analysis: per SiteCode x LHS_Code correlation matrices and mean clustered heatmap ---
# Computes per-group correlation matrices for a selected set of metrics, averages them elementwise,
# clusters variables and draws a mean correlation heatmap.

# Ensure a filtered metrics object exists (created above); otherwise use full metrics
if (!exists("metrics_filt")) metrics_filt <- metrics

metric_cols <- c("temp_mean","temp_sd","q05","q25","q50","q75","q95",
                 "degree_days","days_above_thresh","days_below_thresh",
                 "max_consec_above","n_heat_events","mean_7d","max_7d","median_weekly_range")
metric_cols <- intersect(metric_cols, names(metrics_filt))

# Build per-group correlation matrices
groups <- unique(metrics_filt[, .(SiteCode, LHS_Code)])
cors_list <- list()
group_sizes <- integer()
for (i in seq_len(nrow(groups))) {
  g <- groups[i]
  sub <- metrics_filt[SiteCode == g$SiteCode & LHS_Code == g$LHS_Code, ..metric_cols]
  if (nrow(sub) < 3) next
  good_cols <- sapply(sub, function(x) !(all(is.na(x)) || var(x, na.rm = TRUE) == 0))
  sub2 <- sub[, names(sub)[good_cols], with = FALSE]
  if (ncol(sub2) < 2) next
  cm <- tryCatch(cor(sub2, use = "pairwise.complete.obs"), error = function(e) NULL)
  if (!is.null(cm)) {
    full_cm <- matrix(NA_real_, nrow = length(metric_cols), ncol = length(metric_cols),
                      dimnames = list(metric_cols, metric_cols))
    keep <- intersect(rownames(cm), metric_cols)
    full_cm[keep, keep] <- cm[keep, keep]
    id <- paste(g$SiteCode, g$LHS_Code, sep = "_")
    cors_list[[id]] <- full_cm
    group_sizes[id] <- nrow(sub)
  }
}

n_groups_used <- length(cors_list)
cat("Groups used for correlation:", n_groups_used, "\n")
if (n_groups_used == 0) stop("No groups produced valid correlation matrices")

# Stack into an array and compute elementwise mean (ignore NA)
array_list <- simplify2array(cors_list)
if (is.matrix(array_list)) array_list <- array(array_list, dim = c(dim(array_list), 1))
mean_cor <- apply(array_list, c(1,2), function(x) mean(x, na.rm = TRUE))
mean_cor[is.nan(mean_cor)] <- 0

# Cluster variables using 1 - mean_cor as a distance
# Ensure the distance matrix is valid (convert to dissimilarity)
dmat <- as.dist(1 - mean_cor)
hc <- hclust(dmat, method = "complete")
metric_order <- rownames(mean_cor)[hc$order]

# Prepare data for ggplot heatmap
mc_ord <- mean_cor[metric_order, metric_order]
mc_dt <- as.data.table(mc_ord, keep.rownames = "Var1")
mc_long <- melt(mc_dt, id.vars = "Var1", variable.name = "Var2", value.name = "cor")
mc_long[, Var2 := as.character(Var2)]
mc_long[, Var1 := factor(Var1, levels = rev(metric_order))]
mc_long[, Var2 := factor(Var2, levels = metric_order)]

# Plot mean correlation heatmap
mean_cor_plot <- ggplot(mc_long, aes(x = Var2, y = Var1, fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick", midpoint = 0, limits = c(-1,1)) +
  labs(title = sprintf("Mean correlation across %d groups", n_groups_used), x = "", y = "", fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(mean_cor_plot)

# Optionally save mean correlation matrix for later use
saveRDS(mean_cor, file = 'salmon_river_wq/outputs/mean_cor_metrics_by_group.rds')


# --- Additional analyses requested: (1) weighted mean correlation, (2) correlations after z-scoring within group,
#     (3) small-multiple correlation heatmaps for top N groups by sample size ---

# Recreate array_list (safe if previous object removed)
array_list <- simplify2array(cors_list)
if (is.matrix(array_list)) array_list <- array(array_list, dim = c(dim(array_list), 1))

# 1) Weighted mean correlation (weights = group sample sizes)
ids <- names(cors_list)
weights <- as.numeric(group_sizes[ids])
# initialize numerator and denominator
nvar <- length(metric_cols)
num <- matrix(0, nvar, nvar, dimnames = list(metric_cols, metric_cols))
den <- matrix(0, nvar, nvar, dimnames = list(metric_cols, metric_cols))
for (j in seq_along(ids)) {
  cm <- cors_list[[ids[j]]]
  wj <- weights[j]
  if (is.null(cm)) next
  ok <- !is.na(cm)
  num[ok] <- num[ok] + wj * cm[ok]
  den[ok] <- den[ok] + wj
}
mean_cor_weighted <- num / den
mean_cor_weighted[is.nan(mean_cor_weighted)] <- 0
# save weighted mean
saveRDS(mean_cor_weighted, file = 'salmon_river_wq/outputs/mean_cor_weighted_by_group.rds')

# 2) Correlations after z-scoring within each SiteCode x LHS_Code group
cors_list_z <- list()
for (i in seq_len(nrow(groups))) {
  g <- groups[i]
  sub <- metrics_filt[SiteCode == g$SiteCode & LHS_Code == g$LHS_Code, ..metric_cols]
  if (nrow(sub) < 3) next
  good_cols <- sapply(sub, function(x) !(all(is.na(x)) || var(x, na.rm = TRUE) == 0))
  sub2 <- sub[, names(sub)[good_cols], with = FALSE]
  if (ncol(sub2) < 2) next
  # z-score within group (center and scale)
  sub_z <- as.data.table(lapply(sub2, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)))
  if (ncol(sub_z) < 2) next
  cmz <- tryCatch(cor(sub_z, use = "pairwise.complete.obs"), error = function(e) NULL)
  if (!is.null(cmz)) {
    full_cmz <- matrix(NA_real_, nrow = length(metric_cols), ncol = length(metric_cols),
                       dimnames = list(metric_cols, metric_cols))
    keep <- intersect(rownames(cmz), metric_cols)
    full_cmz[keep, keep] <- cmz[keep, keep]
    id <- paste(g$SiteCode, g$LHS_Code, sep = "_")
    cors_list_z[[id]] <- full_cmz
  }
}

# Aggregate mean of z-scored correlations
array_list_z <- simplify2array(cors_list_z)
if (is.matrix(array_list_z)) array_list_z <- array(array_list_z, dim = c(dim(array_list_z), 1))
mean_cor_z <- apply(array_list_z, c(1,2), function(x) mean(x, na.rm = TRUE))
mean_cor_z[is.nan(mean_cor_z)] <- 0
saveRDS(mean_cor_z, file = 'salmon_river_wq/outputs/mean_cor_z_metrics_by_group.rds')

# 3) Small-multiple correlation heatmaps for top N groups by sample size
Ntop <- 9
# order group ids by sample size descending
ord_ids <- names(sort(group_sizes, decreasing = TRUE))
top_ids <- head(ord_ids, Ntop)
# prepare long table for top groups
library(reshape2)
long_list <- list()
for (id in top_ids) {
  cm <- cors_list[[id]]
  if (is.null(cm)) next
  # reorder rows/cols to metric_order for consistency
  # if metric_order missing (edge case), use rownames(cm)
  ord <- if (exists("metric_order")) metric_order else rownames(cm)
  # ensure ord intersects
  ord <- intersect(ord, rownames(cm))
  cm_ord <- cm[ord, ord, drop = FALSE]
  cm_dt <- as.data.table(melt(cm_ord, varnames = c("Var1", "Var2"), value.name = "cor"))
  cm_dt[, group := id]
  long_list[[id]] <- cm_dt
}
if (length(long_list) > 0) {
  top_long <- rbindlist(long_list)
  # ensure factor levels
  top_long[, Var1 := factor(Var1, levels = rev(metric_order))]
  top_long[, Var2 := factor(Var2, levels = metric_order)]
  # plot with facets
  p_top <- ggplot(top_long, aes(x = Var2, y = Var1, fill = cor)) +
    geom_tile() +
    facet_wrap(~ group, ncol = 3) +
    scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick", midpoint = 0, limits = c(-1,1)) +
    labs(title = sprintf("Correlation matrices — top %d groups by sample size", Ntop), x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 8))
  print(p_top)
  # save
  ggsave(filename = 'salmon_river_wq/outputs/cor_heatmaps_top9.png', plot = p_top, width = 10, height = 8)
}

# Also save the list of per-group correlation matrices and group sizes for downstream inspection
saveRDS(cors_list, file = 'salmon_river_wq/outputs/cors_list_by_group.rds')
saveRDS(group_sizes, file = 'salmon_river_wq/outputs/group_sizes_by_group.rds')



# PCA
library(data.table)
library(ggplot2)
library(reshape2)

dir.create("salmon_river_wq/outputs", recursive = TRUE, showWarnings = FALSE)

# PCA A: eigen decomposition of mean_cor_z
if (exists("mean_cor_z")) {
  eig <- eigen(mean_cor_z)
  eigvals <- eig$values
  eigvecs <- eig$vectors
  prop_var <- eigvals / sum(eigvals)
  metric_names <- rownames(mean_cor_z)

  pcaA_loadings_df <- as.data.frame(eigvecs)
  pcaA_loadings_df <- cbind(Metric = metric_names, pcaA_loadings_df)
  pc_names <- paste0("PC", seq_len(ncol(eigvecs)))
  colnames(pcaA_loadings_df) <- c("Metric", pc_names)

  fwrite(as.data.table(pcaA_loadings_df), file = "salmon_river_wq/outputs/pcaA_loadings_mean_cor_z.csv")
  saveRDS(list(values = eigvals, vectors = eigvecs, prop_var = prop_var), file = "salmon_river_wq/outputs/pcaA_eigen_mean_cor_z.rds")

  # Scree
  screeA <- data.frame(PC = pc_names, prop = prop_var)
  p_screeA <- ggplot(screeA, aes(x = PC, y = prop)) + geom_col(fill = "grey40") +
    geom_line(aes(group = 1), color = "firebrick") + geom_point(color = "firebrick") +
    labs(title = "Scree (PCA from mean_cor_z)", y = "Proportion variance explained", x = "") +
    theme_minimal()
  print(p_screeA)
  ggsave("salmon_river_wq/outputs/pcaA_scree.png", p_screeA, width = 6, height = 4)

  # Top loadings PC1 & PC2
  load_longA <- melt(pcaA_loadings_df, id.vars = "Metric", variable.name = "PC", value.name = "loading")
  load_longA$abs_loading <- abs(load_longA$loading)
  topA <- subset(load_longA, PC %in% c("PC1","PC2"))
  topA <- topA[order(topA$PC, -topA$abs_loading), ]
  fwrite(as.data.table(topA), file = "salmon_river_wq/outputs/pcaA_top_loadings_PC1_PC2.csv")

  p_loadA <- ggplot(topA, aes(x = reorder(Metric, loading), y = loading, fill = PC)) +
    geom_col(position = position_dodge(width = 0.8)) + coord_flip() +
    labs(title = "PCA (mean_cor_z) loadings: PC1 & PC2", x = "Metric", y = "Loading") +
    theme_minimal()
  print(p_loadA)
  ggsave("salmon_river_wq/outputs/pcaA_loadings_PC1_PC2.png", p_loadA, width = 8, height = 6)

  cat(sprintf("PCA A completed: PC1=%.2f%%, PC2=%.2f%%\n", 100*prop_var[1], 100*prop_var[2]))
} else cat("mean_cor_z not found; skipping PCA A\n")

# PCA B: pooled PCA on within-group z-scored observations
if (!exists("metric_cols")) {
  metric_cols <- c("temp_mean","temp_sd","q05","q25","q50","q75","q95",
                   "degree_days","days_above_thresh","days_below_thresh",
                   "max_consec_above","n_heat_events","mean_7d","max_7d","median_weekly_range")
  metric_cols <- intersect(metric_cols, names(metrics_filt))
}

zdt <- copy(metrics_filt)
# z-score within group; if sd is zero/NA produce NA
zdt[, (metric_cols) := lapply(.SD, function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE))/s
}), by = .(SiteCode, LHS_Code), .SDcols = metric_cols]

z_pooled_clean <- zdt[complete.cases(zdt[, ..metric_cols])]
cat("Pooled z-scored rows:", nrow(z_pooled_clean), "\n")
if (nrow(z_pooled_clean) < 3) stop("Not enough pooled rows for PCA")

var_cols <- sapply(z_pooled_clean[, ..metric_cols], function(x) var(x, na.rm = TRUE))
keep_cols <- names(var_cols)[var_cols > 0]
if (length(keep_cols) < 2) stop("Not enough variables for PCA after cleaning")
mat <- as.matrix(z_pooled_clean[, ..keep_cols])

pcaB <- prcomp(mat, center = FALSE, scale. = FALSE)
saveRDS(pcaB, file = "salmon_river_wq/outputs/pcaB_prcomp_pooled_z.rds")

var_explained <- (pcaB$sdev^2)/sum(pcaB$sdev^2)
pc_names <- paste0("PC", seq_along(var_explained))
screeB <- data.frame(PC = pc_names, prop = var_explained)

p_screeB <- ggplot(screeB, aes(x = PC, y = prop)) + geom_col(fill = "grey40") +
  geom_line(aes(group=1), color = "steelblue") + geom_point(color = "steelblue") +
  labs(title = "Scree (PCA on pooled within-group z-scored obs)", y = "Proportion variance explained", x = "") +
  theme_minimal()
print(p_screeB)
ggsave("salmon_river_wq/outputs/pcaB_scree.png", p_screeB, width = 6, height = 4)
 
loadingsB <- as.data.frame(pcaB$rotation)
loadingsB <- cbind(Metric = rownames(loadingsB), loadingsB)
fwrite(as.data.table(loadingsB), file = "salmon_river_wq/outputs/pcaB_loadings_allPCs.csv")

load_longB <- melt(loadingsB, id.vars = "Metric", variable.name = "PC", value.name = "loading")
load_longB$abs_loading <- abs(load_longB$loading)
topB <- subset(load_longB, PC %in% c("PC1","PC2"))
topB <- topB[order(topB$PC, -topB$abs_loading), ]
fwrite(as.data.table(topB), file = "salmon_river_wq/outputs/pcaB_top_loadings_PC1_PC2.csv")
 
p_loadB <- ggplot(topB, aes(x = reorder(Metric, loading), y = loading, fill = PC)) +
  geom_col(position = position_dodge(width = 0.8)) + coord_flip() +
  labs(title = "PCA (pooled z) loadings: PC1 & PC2", x = "Metric", y = "Loading") +
  theme_minimal()
print(p_loadB)
ggsave("salmon_river_wq/outputs/pcaB_loadings_PC1_PC2.png", p_loadB, width = 8, height = 6)
 
# Biplot-style centroids
scores <- as.data.frame(pcaB$x)
scores$SiteCode <- z_pooled_clean$SiteCode
scores$LHS_Code <- z_pooled_clean$LHS_Code
centroids <- aggregate(. ~ SiteCode + LHS_Code, data = scores[, c("SiteCode","LHS_Code", pc_names)], FUN = mean)

load_mat <- pcaB$rotation[, 1:2, drop = FALSE]
score_range <- apply(centroids[, pc_names[1:2]], 2, function(x) diff(range(x)))
lf <- min(score_range) * 0.5 / max(abs(load_mat))
load_df <- as.data.frame(load_mat * lf)
load_df$Metric <- rownames(load_mat)

library(grid)

p_biplot <- ggplot(centroids, aes_string(x = pc_names[1], y = pc_names[2])) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_text(aes(label = paste(SiteCode, LHS_Code, sep = "_")), size = 2, vjust = -0.5) +
  geom_segment(data = load_df, aes(x = 0, y = 0, xend = load_df[,1], yend = load_df[,2]), arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = load_df, aes(x = load_df[,1], y = load_df[,2], label = Metric), color = "red", vjust = -0.5, size = 3) +
  labs(title = "Biplot: group centroids and variable loadings (PC1 vs PC2)") + theme_minimal()
print(p_biplot)
ggsave("salmon_river_wq/outputs/pcaB_biplot_centroids_loadings.png", p_biplot, width = 10, height = 7)

# Save summary
saveRDS(list(pcaA = if (exists("eig")) list(values = eigvals, vectors = eigvecs, prop_var = prop_var) else NULL,
             pcaB = pcaB,
             pooled_z_rows = nrow(z_pooled_clean)), file = "salmon_river_wq/outputs/pca_summary_objects.rds")

cat("PCA run complete. Files written to salmon_river_wq/outputs/\n")
#PCA A completed: PC1=52.71%, PC2=16.60%
#There were 50 or more warnings (use warnings() to see the first 50)
#Pooled z-scored rows: 667 



