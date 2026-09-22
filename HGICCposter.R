# ==========================================================
# HGICC Supportive Care Integration & Community Impact
# Poster Figures
#
# Author: Jorge Heneche
# Date:   09/21/2026
#
# Purpose:
# Generates two poster-ready figures highlighting:
#   1) Supportive care integration
#   2) Community impact
#
# Poster:
# "Operational and Community Impact of a Specialized
# Hispanic Gastrointestinal Oncology Clinic"
#
# Metrics:
#   1) 22 patients referred to social work (37.9%)
#   2) 11 patients receiving integrated palliative care
#   3) 36 palliative care encounters
#   4) 27 community outreach and educational events
#
# Outputs:
#   - HGICC_Supportive_Care_Integration_Poster.png
#   - HGICC_Supportive_Care_Integration_Poster.pdf
#   - HGICC_Community_Impact_Poster.png
#   - HGICC_Community_Impact_Poster.pdf
#
# Output location:
#   User's Downloads folder
#
# Northwestern Purple:
#   #4E2A84
#
# ==========================================================


# ==========================================================
# PART A — PACKAGES & GLOBAL SETTINGS
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

NU_PURPLE <- "#4E2A84"
LIGHT_PURPLE <- "#E4D9F2"
DARK_TEXT <- "#333333"


# ==========================================================
# PART B — SET OUTPUT LOCATION
# ==========================================================

HOME_DIR <- path.expand("~")
DOWNLOADS_DIR <- file.path(HOME_DIR, "Downloads")

if (!dir.exists(DOWNLOADS_DIR)) {
  stop(
    paste(
      "Downloads folder was not found at:",
      DOWNLOADS_DIR
    )
  )
}

PNG_OUTPUT <- file.path(
  DOWNLOADS_DIR,
  "HGICC_Supportive_Care_Integration_Poster.png"
)

PDF_OUTPUT <- file.path(
  DOWNLOADS_DIR,
  "HGICC_Supportive_Care_Integration_Poster.pdf"
)

COMMUNITY_PNG_OUTPUT <- file.path(
  DOWNLOADS_DIR,
  "HGICC_Community_Impact_Poster.png"
)

COMMUNITY_PDF_OUTPUT <- file.path(
  DOWNLOADS_DIR,
  "HGICC_Community_Impact_Poster.pdf"
)


# ==========================================================
# PART C — CREATE SUPPORTIVE CARE METRIC DATA
# ==========================================================

metrics <- tibble(
  x = c(1, 2, 1, 2),
  y = c(2, 2, 1, 1),
  
  value = c(
    "22 (37.9%)",
    "11",
    "36",
    "27"
  ),
  
  label = c(
    "Patients Referred\nto Social Work",
    "Patients Receiving\nPalliative Care",
    "Palliative Care\nEncounters",
    "Community Outreach &\nEducational Events"
  ),
  
  card_type = c(
    "dark",
    "light",
    "light",
    "dark"
  )
)


# ==========================================================
# PART D — CREATE SUPPORTIVE CARE FIGURE
# ==========================================================

p_supportive <- ggplot() +
  
  # --------------------------------------------------------
# Dark purple cards
# --------------------------------------------------------

geom_tile(
  data = metrics %>%
    filter(card_type == "dark"),
  aes(
    x = x,
    y = y
  ),
  width = 0.94,
  height = 0.90,
  fill = NU_PURPLE
) +
  
  # --------------------------------------------------------
# Light purple cards
# --------------------------------------------------------

geom_tile(
  data = metrics %>%
    filter(card_type == "light"),
  aes(
    x = x,
    y = y
  ),
  width = 0.94,
  height = 0.90,
  fill = LIGHT_PURPLE
) +
  
  # --------------------------------------------------------
# Large values — dark cards
# --------------------------------------------------------

geom_text(
  data = metrics %>%
    filter(card_type == "dark"),
  aes(
    x = x,
    y = y + 0.11,
    label = value
  ),
  size = 11,
  fontface = "bold",
  color = "white"
) +
  
  # --------------------------------------------------------
# Large values — light cards
# --------------------------------------------------------

geom_text(
  data = metrics %>%
    filter(card_type == "light"),
  aes(
    x = x,
    y = y + 0.11,
    label = value
  ),
  size = 11,
  fontface = "bold",
  color = NU_PURPLE
) +
  
  # --------------------------------------------------------
# Descriptions — dark cards
# --------------------------------------------------------

geom_text(
  data = metrics %>%
    filter(card_type == "dark"),
  aes(
    x = x,
    y = y - 0.15,
    label = label
  ),
  size = 5.7,
  lineheight = 1.05,
  fontface = "bold",
  color = "white"
) +
  
  # --------------------------------------------------------
# Descriptions — light cards
# --------------------------------------------------------

geom_text(
  data = metrics %>%
    filter(card_type == "light"),
  aes(
    x = x,
    y = y - 0.15,
    label = label
  ),
  size = 5.7,
  lineheight = 1.05,
  fontface = "bold",
  color = DARK_TEXT
) +
  
  # --------------------------------------------------------
# Figure boundaries
# --------------------------------------------------------

coord_cartesian(
  xlim = c(0.48, 2.52),
  ylim = c(0.48, 2.52),
  expand = FALSE,
  clip = "off"
) +
  
  # --------------------------------------------------------
# Remove all normal chart components
# --------------------------------------------------------

theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )


# ==========================================================
# PART E — DISPLAY SUPPORTIVE CARE FIGURE
# ==========================================================

print(p_supportive)


# ==========================================================
# PART F — EXPORT SUPPORTIVE CARE FIGURE
# ==========================================================

# PNG for PowerPoint
ggsave(
  filename = PNG_OUTPUT,
  plot = p_supportive,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

# PDF for vector-quality printing
ggsave(
  filename = PDF_OUTPUT,
  plot = p_supportive,
  width = 10,
  height = 5.5,
  units = "in",
  bg = "white"
)


# ==========================================================
# PART G — COMMUNITY IMPACT DATA
# ==========================================================

# Central community outreach node
community_center <- tibble(
  x = 0,
  y = 0,
  value = "27",
  label = "Community Outreach &\nEducational Events"
)

# Community and partnership groups surrounding outreach
community_nodes <- tibble(
  x = c(
    0,      # Top
    -1.65,  # Left
    1.65,   # Right
    0       # Bottom
  ),
  
  y = c(
    1.15,
    0,
    0,
    -1.15
  ),
  
  label = c(
    "Patient Advocacy\nOrganizations",
    "Patients &\nFamilies",
    "Community\nPartners",
    "Industry\nPartners"
  )
)


# ==========================================================
# PART H — COMMUNITY IMPACT CONNECTIONS
# ==========================================================

# Create lines connecting each partnership group
# to the central community outreach node.

community_edges <- community_nodes %>%
  transmute(
    x = x,
    y = y,
    xend = 0,
    yend = 0
  )


# ==========================================================
# PART I — CREATE COMMUNITY IMPACT FIGURE
# ==========================================================

p_community <- ggplot() +
  
  # --------------------------------------------------------
# Connection lines
# --------------------------------------------------------

geom_segment(
  data = community_edges,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  ),
  linewidth = 1.4,
  color = NU_PURPLE,
  alpha = 0.65
) +
  
  # --------------------------------------------------------
# Outer partnership nodes
# --------------------------------------------------------

geom_label(
  data = community_nodes,
  aes(
    x = x,
    y = y,
    label = label
  ),
  size = 5.3,
  fontface = "bold",
  color = NU_PURPLE,
  fill = LIGHT_PURPLE,
  label.size = 0,
  label.padding = unit(
    0.45,
    "lines"
  ),
  lineheight = 1.05
) +
  
  # --------------------------------------------------------
# Central community impact node
# --------------------------------------------------------

geom_label(
  data = community_center,
  aes(
    x = x,
    y = y,
    label = value
  ),
  size = 12,
  fontface = "bold",
  color = "white",
  fill = NU_PURPLE,
  label.size = 0,
  label.padding = unit(
    0.55,
    "lines"
  )
) +
  
  # --------------------------------------------------------
# Central node description
# Positioned at approximately 4 o'clock from "27"
# --------------------------------------------------------

geom_text(
  data = community_center,
  aes(
    x = x + 0.55,
    y = y - 0.48,
    label = label
  ),
  size = 5.3,
  fontface = "bold",
  color = DARK_TEXT,
  lineheight = 1.05,
  hjust = 0
) +
  
  # --------------------------------------------------------
# Educational focus
# --------------------------------------------------------

annotate(
  "text",
  x = 0,
  y = -1.85,
  label = "Educational Focus",
  size = 5.3,
  fontface = "bold",
  color = NU_PURPLE
) +
  
  annotate(
    "text",
    x = 0,
    y = -2.12,
    label = paste(
      "GI Cancer  \u2022  Clinical Trials  \u2022",
      "Patient Navigation  \u2022  Health Literacy"
    ),
    size = 4.7,
    color = DARK_TEXT
  ) +
  
  # --------------------------------------------------------
# Figure boundaries
# --------------------------------------------------------

coord_cartesian(
  xlim = c(-2.5, 2.5),
  ylim = c(-2.4, 1.75),
  expand = FALSE,
  clip = "off"
) +
  
  # --------------------------------------------------------
# Remove normal chart components
# --------------------------------------------------------

theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(
      10,
      15,
      10,
      15
    )
  )


# ==========================================================
# PART J — DISPLAY COMMUNITY IMPACT FIGURE
# ==========================================================

print(p_community)


# ==========================================================
# PART K — EXPORT COMMUNITY IMPACT FIGURE
# ==========================================================

# PNG version for PowerPoint
ggsave(
  filename = COMMUNITY_PNG_OUTPUT,
  plot = p_community,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# PDF vector version for high-quality printing
ggsave(
  filename = COMMUNITY_PDF_OUTPUT,
  plot = p_community,
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)


# ==========================================================
# PART L — FINAL COMPLETION MESSAGE
# ==========================================================

cat(
  "\n==================================================",
  "\nHGICC POSTER FIGURES SUCCESSFULLY GENERATED",
  "\n==================================================",
  
  "\n\nSUPPORTIVE CARE INTEGRATION:",
  
  "\nPNG:",
  "\n", PNG_OUTPUT,
  
  "\n\nPDF:",
  "\n", PDF_OUTPUT,
  
  "\n\nCOMMUNITY IMPACT:",
  
  "\nPNG:",
  "\n", COMMUNITY_PNG_OUTPUT,
  
  "\n\nPDF:",
  "\n", COMMUNITY_PDF_OUTPUT,
  
  "\n\nAll figures saved to:",
  "\n", DOWNLOADS_DIR,
  
  "\n==================================================\n"
)


# ==========================================================
# END OF SCRIPT
# ==========================================================

