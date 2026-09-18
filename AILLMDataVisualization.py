# ============================================================
# AI FOR DATA VISUALIZATION WORKSHOP
# PYTHON SCRIPT
# By Jorge Heneche
# Date:September 2026
# ============================================================


# ============================================================
# 1. PACKAGE SETUP
# ============================================================

import sys
import subprocess
import importlib.util
from pathlib import Path
import sqlite3


# Packages required for the workshop
required_packages = {
    "pandas": "pandas",
    "openpyxl": "openpyxl",
    "geopandas": "geopandas",
    "numpy": "numpy",
    "matplotlib": "matplotlib"
}


# Install packages only if they are missing
for import_name, package_name in required_packages.items():

    if importlib.util.find_spec(import_name) is None:

        print(f"Installing {package_name}...")

        subprocess.check_call([
            sys.executable,
            "-m",
            "pip",
            "install",
            package_name
        ])


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt


# ============================================================
# 3. DEFINE DATA LOCATION
# ============================================================

# Main workshop folder
folder = Path(
    "/Users/jorgeheneche/Downloads/AI-for-data-visualization-workshop-main"
)

# Workshop datasets are stored inside the data folder
data_folder = folder / "data"


# Make sure Python can find the folders
if not folder.exists():
    raise FileNotFoundError(
        f"Workshop folder was not found:\n{folder}"
    )

if not data_folder.exists():
    raise FileNotFoundError(
        f"Data folder was not found:\n{data_folder}"
    )


# ============================================================
# 4. LOAD CPS DATA
# ============================================================

CPS_merged = pd.read_csv(
    data_folder / "CPS_merged.csv"
)


# ============================================================
# 5. LOAD FRENCH STATES DATA
# ============================================================

data_on_french_states = pd.read_csv(
    data_folder / "data_on_french_states.csv"
)


# ============================================================
# 6. LOAD FINANCIAL SAMPLE DATA
# ============================================================

Financial_Sample = pd.read_excel(
    data_folder / "Financial Sample.xlsx"
)


# ============================================================
# 7. LOAD COMMUNES GEOJSON DATA
# ============================================================

communes = gpd.read_file(
    data_folder / "communes.geojson"
)


# ============================================================
# 8. CONNECT TO CHINOOK DATABASE
# ============================================================

chinook = sqlite3.connect(
    data_folder / "chinook.db"
)


# ============================================================
# 9. CHECK THAT DATASETS LOADED
# ============================================================

print("\n========================================")
print("DATA LOADED SUCCESSFULLY")
print("========================================")


print("\nCPS_merged")
print("Rows:", CPS_merged.shape[0])
print("Columns:", CPS_merged.shape[1])


print("\ndata_on_french_states")
print("Rows:", data_on_french_states.shape[0])
print("Columns:", data_on_french_states.shape[1])


print("\nFinancial_Sample")
print("Rows:", Financial_Sample.shape[0])
print("Columns:", Financial_Sample.shape[1])


print("\ncommunes")
print("Rows:", communes.shape[0])
print("Columns:", communes.shape[1])


print("\nchinook")
print("SQLite database connected successfully.")


# ============================================================
# 10. CHECK TABLES IN CHINOOK DATABASE
# ============================================================

chinook_tables = pd.read_sql_query(
    """
    SELECT name
    FROM sqlite_master
    WHERE type = 'table'
    ORDER BY name;
    """,
    chinook
)

print("\nChinook database tables:")
print(chinook_tables)


# ============================================================
# 11. FINAL DATA SETUP CONFIRMATION
# ============================================================

print("\n========================================")
print("SETUP COMPLETE")
print("========================================")

print("\nPython datasets/objects available:")
print("CPS_merged")
print("data_on_french_states")
print("Financial_Sample")
print("communes")
print("chinook")


# ============================================================
# 12. CPS SCATTER PLOT:
#     MOBILITY VS. COLLEGE ENROLLMENT
# ============================================================

# Variables selected from CPS_merged
#
# X = school mobility percentage
# Y = school college enrollment percentage for Year 2

x_column = "Mobility_Rate_Pct"
y_column = "College_Enrollment_School_Pct_Year_2"


# Keep only the variables needed for this analysis
CPS_plot_data = CPS_merged[
    [x_column, y_column]
].copy()


# Convert both variables to numeric
# Invalid/non-numeric values become NaN
CPS_plot_data[x_column] = pd.to_numeric(
    CPS_plot_data[x_column],
    errors="coerce"
)

CPS_plot_data[y_column] = pd.to_numeric(
    CPS_plot_data[y_column],
    errors="coerce"
)


# Remove schools missing either variable
CPS_plot_data = CPS_plot_data.dropna()


# Convert columns to NumPy arrays
x = CPS_plot_data[x_column].to_numpy()
y = CPS_plot_data[y_column].to_numpy()


print("\n========================================")
print("CPS VISUALIZATION")
print("========================================")

print("\nX variable:")
print(x_column)

print("\nY variable:")
print(y_column)

print("\nSchools included in plot:")
print(len(CPS_plot_data))


# ============================================================
# 13. FIT LINEAR REGRESSION
# ============================================================

# Construct regression matrix
#
# Model:
#
# college enrollment = intercept + slope * mobility

X = np.column_stack([
    np.ones(len(x)),
    x
])


# Estimate intercept and slope using ordinary least squares
beta = np.linalg.lstsq(
    X,
    y,
    rcond=None
)[0]


intercept = beta[0]
slope = beta[1]


print("\nLinear regression:")
print(f"Intercept = {intercept:.4f}")
print(f"Slope = {slope:.4f}")


# Predicted values for observed schools
y_predicted = X @ beta


# Calculate residuals
residuals = y - y_predicted


# ============================================================
# 14. CALCULATE 1-SIGMA CONFIDENCE INTERVAL
# ============================================================

n = len(x)


# Residual standard error
residual_standard_error = np.sqrt(
    np.sum(residuals ** 2) / (n - 2)
)


# Create smooth x values for regression line
x_line = np.linspace(
    x.min(),
    x.max(),
    300
)


# Predicted regression line
y_line = (
    intercept
    + slope * x_line
)


# Mean of x
x_mean = np.mean(x)


# Sum of squared deviations from x mean
Sxx = np.sum(
    (x - x_mean) ** 2
)


# Standard error of the fitted mean response
standard_error_line = (
    residual_standard_error
    * np.sqrt(
        (1 / n)
        + ((x_line - x_mean) ** 2 / Sxx)
    )
)


# 1-sigma confidence interval
lower_confidence = (
    y_line
    - standard_error_line
)

upper_confidence = (
    y_line
    + standard_error_line
)


# ============================================================
# 15. CREATE SCATTER PLOT
# ============================================================

fig, ax = plt.subplots(
    figsize=(7.2, 5.2)
)


# Scatter plot of individual schools
ax.scatter(
    x,
    y,
    s=24,
    alpha=0.55,
    edgecolors="none",
    label="Schools"
)


# Linear regression trendline
ax.plot(
    x_line,
    y_line,
    linewidth=2.0,
    label="Linear fit"
)


# 1-sigma confidence interval
ax.fill_between(
    x_line,
    lower_confidence,
    upper_confidence,
    alpha=0.22,
    label="1-sigma confidence interval"
)


# ============================================================
# 16. FORMAT FIGURE
# ============================================================

ax.set_xlabel(
    "Mobility percentage (%)"
)

ax.set_ylabel(
    "College enrollment percentage (%)"
)

ax.set_title(
    "Mobility vs. College Enrollment in Chicago Public Schools"
)


# Remove top and right borders
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)


# Add legend
ax.legend(
    frameon=False
)


# Adjust spacing
fig.tight_layout()


# ============================================================
# 17. SAVE FIGURE AS PDF
# ============================================================

output_pdf = folder / "CPS_mobility_vs_college_enrollment.pdf"


fig.savefig(
    output_pdf,
    format="pdf",
    bbox_inches="tight"
)


print("\n========================================")
print("FIGURE CREATED SUCCESSFULLY")
print("========================================")

print("\nPDF saved to:")
print(output_pdf)


# ============================================================
# 18. DISPLAY FIGURE
# ============================================================

plt.show()
