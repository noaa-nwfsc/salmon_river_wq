# ==============================================================================
# R Script: Replicate Chinook Salmon Life Cycle Periodicity Chart
# Based on periodicity.png Created in Gemini Notebook and refined with CoPilot GPT-5 mini 10/2/26 AHF
# ==============================================================================

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggtext, scales, ragg)
library(ggtext)

# ------------------------------------------------------------------------------
# 1. Define Column Headers (Bi-weekly Date Intervals)
# ------------------------------------------------------------------------------
# date_cols <- c(
#   # Year 1 (Adult Migration, Spawning & Egg Incubation Start)
#   "15-Apr", "1-May", "15-May", "1-Jun", "15-Jun", "1-Jul", "15-Jul", 
#   "1-Aug", "15-Aug", "1-Sep", "15-Sep", "1-Oct", "15-Oct", "1-Nov", 
#   "15-Nov", "1-Dec", "15-Dec", "1-Jan", "15-Jan", "1-Feb", "15-Feb", 
#   "1-Mar", "15-Mar",
#   # Year 2 (Parr Rearing & Early Outmigration)
#   "1-Apr", "15-Apr", "1-May", "15-May", "1-Jun", "15-Jun", "1-Jul", 
#   "15-Jul", "1-Aug", "15-Aug", "1-Sep", "15-Sep", "1-Oct", "15-Oct", 
#   "1-Nov", "15-Nov", "1-Dec", "15-Dec", "1-Jan", "15-Jan", "1-Feb", 
#   "15-Feb", "1-Mar", "15-Mar",
#   # Year 3 (Smolt Outmigration)
#   "1-Apr", "15-Apr", "1-May", "15-May", "1-Jun", "15-Jun", "1-Jul", "15-Jul"
# )
date_cols <- c(
    # Year 1 (Adult Migration, Spawning & Egg Incubation Start)
    "", "1-May", "", "1-Jun", "", "1-Jul", "", "1-Aug", "", "1-Sep", "", "1-Oct", "", "1-Nov",
    "", "1-Dec", "", "1-Jan", "", "1-Feb", "", "1-Mar", "",
    # Year 2 (Parr Rearing & Early Outmigration)
    "1-Apr", "", "1-May", "", "1-Jun", "", "1-Jul", "", "1-Aug", "", "1-Sep", "", "1-Oct", "",
    "1-Nov", "", "1-Dec", "", "1-Jan", "", "1-Feb", "", "1-Mar", "",
    # Year 3 (Smolt Outmigration)
    "1-Apr", "", "1-May", "", "1-Jun", "", "1-Jul", ""
)

n_cols <- length(date_cols)

# ------------------------------------------------------------------------------
# 2. Define Life Stage Rows Data
# ------------------------------------------------------------------------------
# Stage order from top to bottom (14 rows)
stages_df <- tibble::tribble(
  ~row_id, ~label, ~group, ~color, ~start_col, ~end_col, ~bar_text, ~annotation,
  1,  "Adults migrating",                  "Adult",      "#334488", 1,  7,  "Snake R", NA,
  2,  "Adults migrating",                  "Adult",      "#334488", 2,  6,  "Salmon R ", NA,
  3,  "Adults holding",                    "Adult",      "#334488", 4,  9,  "Tribs", NA,
  4,  "Adults spawning",                   "Adult",      "#334488", 8,  11, "Tribs", NA,
  5,  "Eggs incubating",                   "Egg",        "#882255", 9,  26, "Tributaries", NA,
  6,  "Fall-migrant parr rearing","Parr_Fall",           "#117733", 24, 33, "Tributaries", NA,
  7,  "Fall-migrant smolts migrating","Smolt_Fall",      "#117733", 30, 35, "Salmon R", NA,
  8,  "Fall-migrant smolts migrating", "Smolt_Fall",     "#117733", 34, 55, "Snake R", "LGD",
  9,  "Winter-migrant parr rearing", "Parr_Win",         "#44aa99", 24, 44, "Tributaries", NA,
  10, "Winter-migrant smolts migrating", "Smolt_Win",    "#44aa99", 37, 48, "Salmon R", NA,
  11, "Winter-migrant smolts migrating",  "Smolt_Win",   "#44aa99", 46, 54, "Snake R", "LGD",
  12, "Spring-migrant parr rearing", "Parr_Spr",         "#cc6677", 24, 50, "Tributaries", NA,
  13, "Spring-migrant smolts migrating", "Smolt_Spr",    "#cc6677", 46, 52, "Salmon R", NA,
  14, "Spring-migrant smolts migrating",  "Smolt_Spr",   "#cc6677", 50, 53, "Snake R", "LGD"
)

# Invert row_id for plotting (so row 1 is at the top)
stages_df <- stages_df %>%
  mutate(
    y_pos = 15 - row_id,
    label_colored = paste0("<span style=\"color:", color, "\">", label, "</span>")
  )

# Create row label factor in correct order
row_labels <- stages_df %>% arrange(row_id) %>% pull(label)
# use HTML-styled labels for axis rendering via ggtext
colored_labels <- stages_df %>% arrange(row_id) %>% pull(label_colored)

# ------------------------------------------------------------------------------
# 3. Create Full Grid Background (Excel-style Table cells)
# ------------------------------------------------------------------------------
grid_df <- expand.grid(
  col = 1:n_cols,
  y_pos = 1:14
)

# ------------------------------------------------------------------------------
# 4. Build ggplot2 Visualization
# ------------------------------------------------------------------------------
p <- ggplot() +
  # Background table grid cells (light gray borders, white fill)
  geom_tile(
    data = grid_df,
    aes(x = col, y = y_pos),
    fill = "white",
    color = "#D9D9D9",
    linewidth = 0.25
  ) +
  # Colored activity bars
  geom_rect(
    data = stages_df,
    aes(
      xmin = start_col - 0.5,
      xmax = end_col + 0.5,
      ymin = y_pos - 0.5,
      ymax = y_pos + 0.5,
      fill = color
    ),
    color = "white",
    linewidth = 0.3
  ) +
  # White text labels inside bars (left-aligned inside bar)
  geom_text(
    data = stages_df,
    aes(
      x = start_col - 0.4,
      y = y_pos,
      label = bar_text
    ),
    color = "white",
    hjust = 0,
    size = 3.2,
    fontface = "bold"
  ) +
  # External annotations (e.g. 'LGD' right next to Snake River smolt bars)
  geom_text(
    data = stages_df %>% filter(!is.na(annotation)),
    aes(
      x = end_col + 0.8,
      y = y_pos,
      label = annotation
    ),
    color = "black",
    hjust = 0,
    size = 3.2,
    fontface = "plain"
  ) +
  # Render colored row labels using geom_richtext so HTML span tags are interpreted
  ggtext::geom_richtext(
    data = stages_df,
    aes(x = 0, y = y_pos, label = label_colored),
    fill = NA, label.color = NA, hjust = 1, size = 4,
    inherit.aes = FALSE
  ) +
  # Title centered above date labels
  annotate(
    "text",
    x = (n_cols + 1)/2, y = 17,
    label = "spring/summer Chinook salmon",
    color = "gray20",
    fontface = "bold",
    hjust = 0.5,
    size = 5
  ) +
  # Draw date labels manually closer to the plot
  geom_text(
    data = data.frame(x = 1:n_cols, y = rep(15, n_cols), label = date_cols),
    aes(x = x, y = y, label = label),
    angle = 45, hjust = 0, vjust = 0, size = 4, color = "#333333",
    inherit.aes = FALSE
  ) +
  # Set Fill Scale
  scale_fill_identity() +
  # Configure X Axis (date axis labels suppressed; drawn manually below)
  scale_x_continuous(
    breaks = 1:n_cols,
    labels = rep("", n_cols),
    expand = c(0, 0),
    position = "top"
  ) +
  # Configure Y Axis (Life Stage Row Labels)
  scale_y_continuous(
    breaks = 14:1,
    labels = rep("", 14),
    expand = c(0, 0)
  ) +
  # Set Plot Limits (extended to allow date labels and title above)
  coord_cartesian(xlim = c(0.5, n_cols + 2.5), ylim = c(0.5, 17), clip = "off") +
  # Styling & Theme Configuration
  theme_minimal(base_family = "sans") +
  theme(
    # X-axis date labels: rotated 45 degrees
    axis.text.x.top = element_text(
      angle = 45, 
      hjust = 0, 
      vjust = 0, 
      size = 10, 
      color = "#333333"
    ),
    # Y-axis stage labels are hidden since geom_richtext draws them
    axis.text.y = element_blank(),
    # Remove axis titles and tick marks
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    # Panel styling
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 25, b = 10, l = 200, unit = "pt")
  )

# Display Plot
print(p)

# Save high-resolution copy
ragg::agg_png("chinook_periodicity.png", width = 10, height = 7, units = "in", res = 300)
print(p)
dev.off()
