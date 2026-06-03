library(ggplot2)

# -----------------------------
# Data
# -----------------------------
res_current <- read.csv(text = "
condition,approach,reliability_current,n_current
overall,approach_1_mixed_icc,0.925612201454438,2475
overall,approach_3_split_half,0.880973171158849,2475
overall,brysbaert_icc1k,0.903129754089478,2475
overall,brysbaert_icc2k,0.902184784315358,2475
overall,brysbaert_icc3k,0.936835151252843,2475
words_only,approach_1_mixed_icc,0.849390733891818,2475
words_only,approach_3_split_half,0.858355073424158,2475
words_only,brysbaert_icc1k,0.892179034342505,2475
words_only,brysbaert_icc2k,0.892653620076491,2475
words_only,brysbaert_icc3k,0.930695751112529,2475
pseudo_only,approach_1_mixed_icc,0.84529378298959,2475
pseudo_only,approach_3_split_half,0.854545290381805,2475
pseudo_only,brysbaert_icc1k,0.879240173922392,2475
pseudo_only,brysbaert_icc2k,0.8788068186955,2475
pseudo_only,brysbaert_icc3k,0.92691669005727,2475
")

# Preserve desired order
res_current$approach <- factor(
  res_current$approach,
  levels = c(
    "approach_3_split_half",
    "approach_1_mixed_icc",
    "brysbaert_icc1k",
    "brysbaert_icc2k",
    "brysbaert_icc3k"
  )
)

res_current$condition <- factor(
  res_current$condition,
  levels = c("overall", "words_only", "pseudo_only")
)

# Short labels
nice_labels <- c(
  approach_3_split_half = "A1: classic split-half SB",
  approach_1_mixed_icc  = "A2: Sascha's ICC3k",
  brysbaert_icc1k       = "A3: Marc's ICC1k\n(conservative)",
  brysbaert_icc2k       = "A4: Marc's ICC2k\n(generalisability)",
  brysbaert_icc3k       = "A5: Marc's ICC3k\n(= Cronbach alpha)"
)

# -----------------------------
# Barplot
# -----------------------------
p <- ggplot(
  res_current,
  aes(
    x = approach,
    y = reliability_current,
    fill = condition
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72
  ) +
  geom_text(
    aes(label = sprintf("%.3f", reliability_current)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 2.6
  ) +
  geom_hline(
    yintercept = 0.8,
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_x_discrete(labels = nice_labels) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0.70, 1.0)) +
  labs(
    x = NULL,
    y = "Item-level reliability",
    fill = "Condition"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 8)
  )

# Save PDF
ggsave(
  filename = "/Users/louis/Desktop/HU_Berlin_Computational_Modelling/German_Lexicon_Project/00_analysis/output/plots/reliability_new.pdf",
  plot = p,
  width = 9.5,
  height = 5
)