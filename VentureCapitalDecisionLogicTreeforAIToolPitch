
# ============================================================
# PHI GUARD — AI SKILL DECISION LOGIC v1.2
# Publication-Quality Flowchart
#
# Includes:
# - Organizational authentication + 2FA
# - PHI detection
# - Internal/external recipient assessment
# - Substitute, Mask, Delete, Block, or Ignore
# - Personal liability acknowledgment
# - Automatic compliance spreadsheet logging
# - Verification of successful audit recording
# - Final authorization before sharing
#
# Output: High-resolution PNG + vector SVG
# Location: User's Downloads folder
# ============================================================


# ============================================================
# 1. INSTALL AND LOAD PACKAGES
# ============================================================

packages <- c(
  "DiagrammeR",
  "DiagrammeRsvg",
  "rsvg"
)

missing_packages <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(DiagrammeR)


# ============================================================
# 2. LOCATE DOWNLOADS FOLDER
# ============================================================

if (.Platform$OS.type == "windows") {
  
  downloads_dir <- file.path(
    Sys.getenv("USERPROFILE"),
    "Downloads"
  )
  
} else {
  
  downloads_dir <- file.path(
    path.expand("~"),
    "Downloads"
  )
  
}

if (!dir.exists(downloads_dir)) {
  stop(
    "Downloads folder not found. ",
    "Please set downloads_dir manually."
  )
}

png_file <- file.path(
  downloads_dir,
  "PHI_Guard_Logic_v1.2.png"
)

svg_file <- file.path(
  downloads_dir,
  "PHI_Guard_Logic_v1.2.svg"
)


# ============================================================
# 3. BUILD PHI GUARD FLOWCHART
# ============================================================

flowchart <- grViz('

digraph PHI_Guard {

  # ----------------------------------------------------------
  # GRAPH SETTINGS
  # ----------------------------------------------------------

  graph [
    layout = dot,
    rankdir = TB,
    bgcolor = "white",
    pad = 0.55,
    nodesep = 0.65,
    ranksep = 0.85,
    splines = polyline,
    labelloc = t,
    label = "PHI GUARD | AI SKILL DECISION LOGIC v1.2",
    fontname = "Helvetica-Bold",
    fontsize = 25,
    fontcolor = "#17324D"
  ]

  node [
    shape = box,
    style = "rounded,filled",
    fontname = "Helvetica",
    fontsize = 15,
    fontcolor = "#183247",
    color = "#B8CAD8",
    fillcolor = "#F0F5FA",
    penwidth = 1.6,
    margin = "0.28,0.22",
    width = 4.7
  ]

  edge [
    color = "#74899C",
    penwidth = 1.8,
    arrowsize = 0.8,
    fontname = "Helvetica",
    fontsize = 12,
    fontcolor = "#475569"
  ]


  # ==========================================================
  # STEP 1 — USER AUTHENTICATION
  # ==========================================================

  auth [
    label = "1. USER AUTHENTICATION\\nOrganizational Login + 2FA",
    fillcolor = "#EAF0F7"
  ]


  # ==========================================================
  # STEP 2 — WORKFLOW ACTIVATION
  # ==========================================================

  trigger [
    label = "2. WORKFLOW ACTIVATION\\nSend / Forward / Share\\nMicrosoft 365 or Approved Website",
    fillcolor = "#EAF0F7"
  ]


  # ==========================================================
  # STEP 3 — AI CONTENT SCAN
  # ==========================================================

  scan [
    label = "3. AI CONTENT SCAN\\nName | DOB | MRN | Phone | Address\\nApproved Organizational Server",
    fillcolor = "#DDEBF8",
    color = "#8EB5D6"
  ]


  # ==========================================================
  # PHI DETECTION DECISION
  # ==========================================================

  detect [
    shape = diamond,
    label = "Potential PHI\\nIdentifiers Detected?",
    fillcolor = "#FFF0D5",
    color = "#DCA54F",
    width = 3.6,
    height = 1.45
  ]


  # No detected PHI
  no_phi [
    label = "NO IDENTIFIERS DETECTED\\nContinue Normal Sharing Checks",
    fillcolor = "#E7F4EB",
    color = "#A4CDB0"
  ]


  # ==========================================================
  # RECIPIENT ASSESSMENT
  # ==========================================================

  recipient [
    label = "RECIPIENT ASSESSMENT\\nInternal / External / Unknown\\nDisplay Contextual Risk Warning",
    fillcolor = "#FFF4E3",
    color = "#DEBB83"
  ]


  # ==========================================================
  # STEP 4 — USER DECISION
  # ==========================================================

  options [
    label = "4. USER DECISION\\n\\nSUBSTITUTE UNIQUE ID | MASK | DELETE\\nBLOCK | IGNORE",
    fillcolor = "#E6EFFB",
    color = "#98B5DF",
    penwidth = 2.2
  ]


  # ==========================================================
  # STANDARD PROTECTIVE ACTIONS
  # ==========================================================

  modify [
    label = "APPLY USER-APPROVED CHANGES\\n\\nSubstitute Unique Patient/Customer ID\\nMask or Delete Identifying Information",
    fillcolor = "#E6EFFB",
    color = "#98B5DF"
  ]


  # ==========================================================
  # STEP 5 — RE-SCAN AND VALIDATE
  # ==========================================================

  verify [
    label = "5. RE-SCAN AND VALIDATE\\n\\nCheck Modified Content\\nUser Reviews Final Version",
    fillcolor = "#E6EFFB",
    color = "#98B5DF"
  ]


  # ==========================================================
  # NEW OPTION — IGNORE WARNING
  # ==========================================================

  ignore [
    label = "IGNORE PHI WARNING\\n\\nUser Requests to Proceed\\nWithout Applying Suggested Changes",
    fillcolor = "#FBE7E7",
    color = "#C65C5C",
    penwidth = 2
  ]


  # ==========================================================
  # PERSONAL LIABILITY ACKNOWLEDGMENT
  # ==========================================================

  acknowledgment [
    label = "PERSONAL LIABILITY ACKNOWLEDGMENT\\n\\n[ ] I acknowledge the potential PHI risk.\\n\\nI accept personal responsibility and\\nany personal liability that may apply\\nunder law or institutional policy.\\n\\nMy decision will be recorded for\\norganizational compliance review.",
    fillcolor = "#FFF0F0",
    color = "#C65C5C",
    fontcolor = "#8B3030",
    penwidth = 2,
    width = 5.7
  ]


  # User acknowledgment confirmation
  confirmed [
    shape = diamond,
    label = "Acknowledgment\\nConfirmed?",
    fillcolor = "#FFF0D5",
    color = "#DCA54F",
    width = 3.5,
    height = 1.35
  ]


  # ==========================================================
  # AUTOMATED COMPLIANCE SPREADSHEET RECORD
  # ==========================================================

  audit [
    label = "6. AUTOMATED COMPLIANCE RECORD\\n\\nSave Acknowledgment to Secure Spreadsheet\\n\\nEvent ID | User ID | Date/Time\\nApplication | Recipient Type\\nPHI Categories | Ignore Decision\\nAcknowledgment | Authorization Status\\n\\nRestricted Organizational Access",
    fillcolor = "#E5F2E8",
    color = "#5D9D70",
    penwidth = 2.2,
    width = 5.8
  ]


  # ==========================================================
  # VERIFY RECORD WAS SAVED
  # ==========================================================

  logged [
    shape = diamond,
    label = "Compliance Record\\nSaved Successfully?",
    fillcolor = "#FFF0D5",
    color = "#DCA54F",
    width = 3.8,
    height = 1.45
  ]


  # ==========================================================
  # FINAL ORGANIZATIONAL AUTHORIZATION
  # ==========================================================

  policy [
    shape = diamond,
    label = "Sharing Permitted\\nby Organizational\\nPolicy?",
    fillcolor = "#FFF0D5",
    color = "#DCA54F",
    width = 3.8,
    height = 1.6
  ]


  # ==========================================================
  # FINAL OUTCOMES
  # ==========================================================

  proceed [
    label = "SHARE IF AUTHORIZED\\n\\nComplete Approved Workflow",
    fillcolor = "#E1F3E8",
    color = "#8DBFA0",
    penwidth = 2
  ]


  blocked [
    label = "BLOCK SHARING\\n\\nCancel Outgoing Action",
    fillcolor = "#FBE7E7",
    color = "#D7A0A0",
    penwidth = 2
  ]


  manual [
    label = "MANUAL REVIEW / ESCALATION\\n\\nUnresolved PHI Risk, Failed Scan,\\nor Unauthorized Sharing",
    fillcolor = "#FFF0D5",
    color = "#DCA54F"
  ]


  # ==========================================================
  # CONNECT WORKFLOW
  # ==========================================================

  # Authentication and scanning
  auth -> trigger
  trigger -> scan
  scan -> detect


  # ----------------------------------------------------------
  # PHI DETECTION PATH
  # ----------------------------------------------------------

  detect -> no_phi [
    label = "NO"
  ]

  detect -> recipient [
    label = "YES"
  ]

  no_phi -> policy


  # ----------------------------------------------------------
  # USER DECISION PATH
  # ----------------------------------------------------------

  recipient -> options


  # ----------------------------------------------------------
  # SUBSTITUTE / MASK / DELETE PATH
  # ----------------------------------------------------------

  options -> modify [
    label = "Substitute / Mask / Delete",
    color = "#5988BA"
  ]

  modify -> verify

  verify -> policy [
    label = "Validated"
  ]

  verify -> manual [
    label = "Unresolved",
    color = "#DCA54F"
  ]


  # ----------------------------------------------------------
  # BLOCK PATH
  # ----------------------------------------------------------

  options -> blocked [
    label = "BLOCK",
    color = "#C65C5C",
    penwidth = 2
  ]


  # ----------------------------------------------------------
  # IGNORE PATH
  # ----------------------------------------------------------

  options -> ignore [
    label = "IGNORE",
    color = "#C65C5C",
    penwidth = 2.2
  ]

  ignore -> acknowledgment

  acknowledgment -> confirmed


  # Acknowledgment declined
  confirmed -> blocked [
    label = "NO",
    color = "#C65C5C"
  ]


  # Acknowledgment accepted
  confirmed -> audit [
    label = "YES",
    color = "#5D9D70"
  ]


  # ----------------------------------------------------------
  # AUTOMATED SPREADSHEET LOGGING
  # ----------------------------------------------------------

  audit -> logged


  # Successful audit recording
  logged -> policy [
    label = "YES",
    color = "#5D9D70"
  ]


  # Failed audit recording
  logged -> blocked [
    label = "NO",
    color = "#C65C5C"
  ]


  # ----------------------------------------------------------
  # FINAL AUTHORIZATION
  # ----------------------------------------------------------

  policy -> proceed [
    label = "YES",
    color = "#5D9D70"
  ]

  policy -> manual [
    label = "NO / UNCLEAR",
    color = "#DCA54F"
  ]


  # ----------------------------------------------------------
  # FAILED OR UNCERTAIN SCANNING
  # ----------------------------------------------------------

  scan -> manual [
    label = "Scan Failure",
    style = dashed,
    constraint = false,
    color = "#DCA54F"
  ]

}

')


# ============================================================
# 4. DISPLAY FLOWCHART IN RSTUDIO VIEWER
# ============================================================

print(flowchart)


# ============================================================
# 5. EXPORT VECTOR SVG
# ============================================================

svg_text <- DiagrammeRsvg::export_svg(flowchart)

writeLines(
  svg_text,
  svg_file,
  useBytes = TRUE
)


# ============================================================
# 6. EXPORT HIGH-RESOLUTION PNG
# ============================================================

rsvg::rsvg_png(
  svg = charToRaw(svg_text),
  file = png_file,
  width = 4200
)


# ============================================================
# 7. CONFIRM SUCCESSFUL EXPORT
# ============================================================

if (file.exists(png_file) && file.exists(svg_file)) {
  
  cat("\n========================================\n")
  cat("PHI GUARD FLOWCHART GENERATED\n")
  cat("Version: 1.2\n")
  cat("========================================\n\n")
  
  cat("PNG saved to:\n")
  cat(png_file, "\n\n")
  
  cat("SVG saved to:\n")
  cat(svg_file, "\n\n")
  
  cat("Both files are ready for presentation use.\n")
  
} else {
  
  warning("One or more output files were not created.")
  
}

