# ## Pew Chart Recreation

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(cowplot)

# Raw data creation -------------------------------------------------------


group <- c(
  "Mormon",
  "White evangelical Protestant",
  "White southerners",
  "White men, some college or less",
  "White",
  "Silent generation (ages 69-86)",
  "Black",
  "Asian",
  "Religiously unaffiliated",
  "Post-graduate women",
  "Jewish",
  "Hispanic",
  "Millenial generation (ages 18-33)",
  "Mormon",
  "White evangelical Protestant",
  "White southerners",
  "White men, some college or less",
  "White",
  "Silent generation (ages 69-86)",
  "Black",
  "Asian",
  "Religiously unaffiliated",
  "Post-graduate women",
  "Jewish",
  "Hispanic",
  "Millenial generation (ages 18-33)",
  "TOTAL",
  "TOTAL"
)

affiliation <- c(
  0.22,
  0.22,
  0.34,
  0.33,
  0.40,
  0.43,
  0.80,
  0.65,
  0.61,
  0.64,
  0.61,
  0.56,
  0.51,
  0.70,
  0.68,
  0.55,
  0.54,
  0.49,
  0.47,
  0.11,
  0.23,
  0.25,
  0.29,
  0.31,
  0.26,
  0.35,
  0.48,
  0.39
)

party <- c(
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Democrat/Lean Democrat",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Republican/Lean Republican",
  "Democrat/Lean Democrat",
  "Republican/Lean Republican"
)
title <- c(
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Header",
  "Header"
)

# Create dataset ----------------------------------------------------------

df <- tibble(group = group, affiliation = affiliation, party = party, title = title)


# Data cleaning -----------------------------------------------------------

# affiliaton_swap reverse values for Democrats for diverging line plots
df <- df %>%
  mutate(affiliation_swap = ifelse(party == "Democrat/Lean Democrat", affiliation * -1, affiliation))

# Create a data.frame just of totals rows.  For diverging plot 1
df_total <- df %>%
  filter(group == "TOTAL")

# Create a data.frame just of Republican groups.  For diverging plot 2
df_rep <- df %>%
  filter(title == "Republican") %>%
  mutate(group = fct_rev(factor(group, levels = c(
    "Mormon",
    "White evangelical Protestant",
    "White southerners",
    "White men, some college or less",
    "White",
    "Silent generation (ages 69-86)"
  ))))

# Create a data.frame just of Democratic groups. For diverging plot 3
df_dem <- df %>%
  filter(title == "Democrat") %>%
  mutate(group = fct_rev(factor(group, levels = c(
    "Black",
    "Asian",
    "Religiously unaffiliated",
    "Post-graduate women",
    "Jewish",
    "Hispanic",
    "Millenial generation (ages 18-33)"
  ))))

# Create a data.frame for the advantage bar charts.
temp1 <- df %>%
  select(-affiliation_swap) %>%
  pivot_wider(names_from = party, values_from = affiliation) %>%
  mutate(advantage = abs(100 * `Republican/Lean Republican` - 100 * `Democrat/Lean Democrat`)) %>%
  mutate(
    groupA = fct_rev(factor(group, levels = c(
      "Mormon",
      "White evangelical Protestant",
      "White southerners",
      "White men, some college or less",
      "White",
      "Silent generation (ages 69-86)"
    ))),
    groupB = fct_rev(factor(group, levels = c(
      "Black",
      "Asian",
      "Religiously unaffiliated",
      "Post-graduate women",
      "Jewish",
      "Hispanic",
      "Millenial generation (ages 18-33)"
    )))
  )


temp2 <- temp1 %>%
  filter(!group == "TOTAL") %>%
  select(group, title, groupA, groupB) %>%
  group_by(group) %>%
  filter(row_number() == 1) %>%
  mutate(
    advantage = ifelse(title == "Republican", 9, -9),
    title = "spacer"
  )

df_adv <- bind_rows(temp1, temp2)

# Hack needed for the first diverging bar chart to align label properly
df_total <- df_total %>%
  mutate(group = "TOTAL                                          ")

# data.frames needed for Republican bar chart
# a/b necessary to have some points inside bars, some out
df2 <- df_adv %>%
  filter(!is.na(groupA)) %>%
  mutate(lab = ifelse(title == "spacer", "", paste0("+", advantage)))

df2a <- df2 %>%
  filter(advantage > 40) %>%
  mutate(lab = ifelse(lab == "+48", "+48R", lab))

df2b <- df2 %>%
  filter(advantage < 40)

df3 <- df_adv %>%
  filter(!is.na(groupB)) %>%
  mutate(lab = ifelse(title == "spacer", "", paste0("+", advantage)))

# data.frames needed for Democrat bar chart
# a/b necessary to have some points inside bars, some out
df3a <- df3 %>%
  filter(advantage < 20)

df3b <- df3 %>%
  filter(advantage > 20) %>%
  mutate(lab = ifelse(lab == "+69", "+69D", lab))


rm(df, temp1, temp2, affiliation, group, party, title)

# Parameters --------------------------------------------------------------

fiddly_bit1 <- 0.3
fiddly_bit2 <- 20
fiddly_bit3 <- 25
fiddly_bit4 <- -13
fiddly_bit5 <- 0.2
fiddly_bit6 <- -8
fiddly_bit7 <- 12

# point sizes
point_size <- 2
line_size <- 3
plot_limits <- 1

blue_color <- "#456B84"
red_color <- "#C13B28"
light_blue_color <- "#D5E3ED"
light_red_color <- "#F7D5D0"

# graph 1 -----------------------------------------------------------------

plt1 <- ggplot(data = df_total) +
  # Add segments
  geom_segment(data = df_total %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_blue_color, size = line_size) +
  geom_segment(data = df_total %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_red_color, size = line_size) +

  # add labels
  geom_text(data = df_total %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, y = group, label = scales::percent(affiliation)), nudge_x = -fiddly_bit5, col = blue_color, size = 3) +
  geom_text(data = df_total %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, y = group, label = scales::percent(affiliation)), nudge_x = fiddly_bit1, col = red_color, size = 3) +
  scale_x_continuous(breaks = c(0), limits = c(-plot_limits, plot_limits)) +

  # add points
  geom_point(aes(x = affiliation_swap, y = group, color = party), size = point_size) +
  scale_color_manual(values = c(blue_color, red_color)) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  annotate("text", x = -.6, y = 1.5, label = "DEMOCRAT/LEAN\nDEMOCRAT", size = 2.5, fontface = "bold") +
  annotate("text", x = .6, y = 1.5, label = "REPUBLICAN/LEAN\nREPUBLICAN", size = 2.5, fontface = "bold") +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 0, size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    plot.title = element_text(hjust = 0, size = 8),
    plot.title.position = "plot"
  ) +
  labs(title = "")


plt1

# graph 2 -----------------------------------------------------------------


plt2 <- ggplot(data = df_rep) +
  # Add segments
  geom_segment(data = . %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_blue_color, size = line_size) +
  geom_segment(data = . %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_red_color, size = line_size) +

  # add labels
  geom_text(data = . %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, y = group, label = affiliation * 100), nudge_x = -fiddly_bit5, col = blue_color, size = 3) +
  geom_text(data = . %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, y = group, label = affiliation * 100), nudge_x = fiddly_bit1, col = red_color, size = 3) +
  scale_x_continuous(breaks = c(0), limits = c(-plot_limits, plot_limits)) +
  # add points
  geom_point(aes(x = affiliation_swap, y = group, color = party), size = point_size) +
  scale_color_manual(values = c(blue_color, red_color)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 0, size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    plot.title = element_text(hjust = 0, size = 8),
    plot.title.position = "plot"
  ) +
  labs(title = "Groups that Tilt Republican")


plt2

# Plot 3 (lean democratic diverging ---------------------------------------


plt3 <- ggplot(data = df_dem) +
  # Add segments
  geom_segment(data = . %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_blue_color, size = line_size) +
  geom_segment(data = . %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, xend = 0, y = group, yend = group), col = light_red_color, size = line_size) +

  # add labels
  geom_text(data = . %>% filter(party == "Democrat/Lean Democrat"), aes(x = affiliation_swap, y = group, label = affiliation * 100), nudge_x = -fiddly_bit5, col = blue_color, size = 3) +
  geom_text(data = . %>% filter(party == "Republican/Lean Republican"), aes(x = affiliation_swap, y = group, label = affiliation * 100), nudge_x = fiddly_bit1, col = red_color, size = 3) +
  scale_x_continuous(breaks = c(0), limits = c(-plot_limits, plot_limits), labels = "0%") +

  # add points
  geom_point(aes(x = affiliation_swap, y = group, color = party), size = point_size) +
  scale_color_manual(values = c(blue_color, red_color)) +
  theme_minimal() +
  labs(title = "Groups that Tilt Democrat") +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.y = element_text(hjust = 0, size = 7),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    plot.title = element_text(hjust = 0, size = 8),
    plot.title.position = "plot"
  )


plt3

# plot 4 total bar --------------------------------------------------------

plt4 <- ggplot(data = df_adv) +
  # Add segments
  geom_bar(data = . %>% filter(group == "TOTAL"), aes(x = group, y = advantage), width = .25, stat = "identity", fill = blue_color) +
  # add labels
  geom_text(data = . %>% filter(group == "TOTAL"), aes(x = group, y = advantage, label = paste0("+", advantage, "D")), nudge_y = fiddly_bit2, col = blue_color, size = 3) +
  scale_y_continuous(breaks = 0, limits = c(-10, 75)) +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.text.y = element_blank()
  )

plt4

# plot 5 total bar --------------------------------------------------------


plt5 <- ggplot(data = df2) +
  # Add segments
  geom_bar(aes(x = groupA, y = advantage, group = title, fill = title), stat = "identity", width = 0.5) +
  geom_text(data = df2a, aes(x = groupA, y = advantage, label = lab), nudge_y = fiddly_bit6, col = "white", size = 2.5) +
  geom_text(data = df2b, aes(x = groupA, y = advantage, label = lab), nudge_y = fiddly_bit3, col = red_color, size = 2.5) +
  scale_y_continuous(breaks = c(0, 9), limits = c(-10, 70), labels = c("", "+9")) +
  scale_fill_manual(values = c(red_color, "#FFFFFF00")) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line()
  )

plt5

# plot 6 total bar --------------------------------------------------------


plt6 <- ggplot(data = df3) +
  # Add segments
  geom_bar(aes(x = groupB, y = advantage), stat = "identity", fill = blue_color, width = 0.5) +
  # add labels
  geom_text(data = df3a, aes(x = groupB, y = advantage, label = lab), nudge_y = fiddly_bit7, col = blue_color, size = 2.5) +
  geom_text(data = df3b, aes(x = groupB, y = advantage, label = lab), nudge_y = fiddly_bit4, col = "white", size = 2.5) +
  scale_y_continuous(breaks = c(-9, 0), limits = c(-10, 70), labels = c("-9", "0")) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7, hjust = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.text.y = element_blank()
  )

plt6


# Create combined plot ----------------------------------------------------

final_plot <- plt1 + plt4 + plt2 + plt5 + plt3 + plt6 +
  plot_layout(ncol = 2, widths = c(10, 4), heights = c(4, 9, 13)) +
  plot_annotation(
    title = "Strong Groups for the Democratic\nand Republican Parties",
    subtitle = "% of each group that identifies as ...",
    caption = "Note: Whites and blacks include only those who are not Hispanic; Hispanics are of any\nrace. Asians are non-Hispanic and English speaking only.\nSource: All Pew Research Center political surveys from 2014. Based on the general public.\n\nRECREATION OF GRAPH BY PEW RESEARCH CENTER"
  ) &
  theme(
    plot.caption = element_text(hjust = 0, color = "gray40", size = 7.5),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(colour = "gray40", face = "italic", family = "Times New Roman")
  )

ggdraw(final_plot) + draw_text("ADVANTAGE", size = 7, x = 0.905, y = 0.85)



# Save Results ------------------------------------------------------------

ggsave("charts/new.png", w = 422, h = 656, units = "px", dpi = 96)
