
"""AI for Data Visualization Workshop

By Jorge Heneche
September 2026
"""

import argparse
import importlib.util
import sqlite3
import subprocess
import sys
from contextlib import closing
from pathlib import Path


REQUIRED_PACKAGES = {
    "pandas": "pandas",
    "openpyxl": "openpyxl",
    "geopandas": "geopandas",
    "numpy": "numpy",
    "matplotlib": "matplotlib",
}

X_COLUMN = "Mobility_Rate_Pct"
Y_COLUMN = "College_Enrollment_School_Pct_Year_2"
OUTPUT_FILENAME = "CPS_mobility_vs_college_enrollment.pdf"

DEFAULT_WORKSHOP_FOLDER = Path(
    "/Users/jorgeheneche/Downloads/AI-for-data-visualization-workshop-main"
)


def install_missing_packages():
    """Install only missing packages."""
    for import_name, package_name in REQUIRED_PACKAGES.items():
        if importlib.util.find_spec(import_name) is None:
            print(f"Installing {package_name}...")
            subprocess.check_call(
                [sys.executable, "-m", "pip", "install", package_name]
            )


def print_shape(name, data):
    print(f"\n{name}")
    print("Rows:", data.shape[0])
    print("Columns:", data.shape[1])


def main():
    parser = argparse.ArgumentParser(
        description="Load workshop datasets and create the CPS scatter plot."
    )
    parser.add_argument(
        "--folder",
        type=Path,
        default=(
            DEFAULT_WORKSHOP_FOLDER
            if DEFAULT_WORKSHOP_FOLDER.is_dir()
            else Path(__file__).resolve().parent
        ),
        help="Workshop folder containing the data/ subfolder.",
    )
    parser.add_argument(
        "--no-show",
        action="store_true",
        help="Save the plot without displaying it.",
    )
    args = parser.parse_args()

    # 1. Package setup
    install_missing_packages()

    import geopandas as gpd
    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd

    # 2. Locate workshop data
    folder = args.folder.expanduser().resolve()
    data_folder = folder / "data"

    if not folder.is_dir():
        raise FileNotFoundError(
            f"Workshop folder was not found:\n{folder}"
        )

    if not data_folder.is_dir():
        raise FileNotFoundError(
            f"Data folder was not found:\n{data_folder}"
        )

    required_files = [
        "CPS_merged.csv",
        "data_on_french_states.csv",
        "Financial Sample.xlsx",
        "communes.geojson",
        "chinook.db",
    ]

    missing_files = [
        name for name in required_files
        if not (data_folder / name).is_file()
    ]

    if missing_files:
        raise FileNotFoundError(
            "Missing workshop data file(s): "
            + ", ".join(missing_files)
        )

    # 3. Load datasets
    CPS_merged = pd.read_csv(
        data_folder / "CPS_merged.csv"
    )

    data_on_french_states = pd.read_csv(
        data_folder / "data_on_french_states.csv"
    )

    Financial_Sample = pd.read_excel(
        data_folder / "Financial Sample.xlsx"
    )

    communes = gpd.read_file(
        data_folder / "communes.geojson"
    )

    print("\n========================================")
    print("DATA LOADED SUCCESSFULLY")
    print("========================================")

    print_shape("CPS_merged", CPS_merged)
    print_shape("data_on_french_states", data_on_french_states)
    print_shape("Financial_Sample", Financial_Sample)
    print_shape("communes", communes)

    # 4. Connect to Chinook database
    with closing(
        sqlite3.connect(data_folder / "chinook.db")
    ) as chinook:

        print("\nchinook")
        print("SQLite database connected successfully.")

        chinook_tables = pd.read_sql_query(
            """
            SELECT name
            FROM sqlite_master
            WHERE type = 'table'
            ORDER BY name;
            """,
            chinook,
        )

        print("\nChinook database tables:")
        print(chinook_tables.to_string(index=False))

    print("\n========================================")
    print("SETUP COMPLETE")
    print("========================================")

    # 5. Prepare CPS visualization data
    CPS_plot_data = CPS_merged[
        [X_COLUMN, Y_COLUMN]
    ].copy()

    CPS_plot_data[X_COLUMN] = pd.to_numeric(
        CPS_plot_data[X_COLUMN],
        errors="coerce"
    )

    CPS_plot_data[Y_COLUMN] = pd.to_numeric(
        CPS_plot_data[Y_COLUMN],
        errors="coerce"
    )

    CPS_plot_data = CPS_plot_data.replace(
        [np.inf, -np.inf], np.nan
    ).dropna()

    x = CPS_plot_data[X_COLUMN].to_numpy()
    y = CPS_plot_data[Y_COLUMN].to_numpy()

    n = len(x)

    print("\n========================================")
    print("CPS VISUALIZATION")
    print("========================================")

    print("\nX variable:", X_COLUMN)
    print("Y variable:", Y_COLUMN)
    print("Schools included in plot:", n)

    if n < 3:
        raise ValueError(
            "At least three complete school records are required."
        )

    x_mean = np.mean(x)

    Sxx = np.sum(
        (x - x_mean) ** 2
    )

    if Sxx == 0:
        raise ValueError(
            "Mobility values must vary to fit a regression line."
        )

    # 6. Fit linear regression
    X = np.column_stack([
        np.ones(n),
        x
    ])

    intercept, slope = np.linalg.lstsq(
        X,
        y,
        rcond=None
    )[0]

    y_predicted = intercept + slope * x
    residuals = y - y_predicted

    print("\nLinear regression:")
    print(f"Intercept = {intercept:.4f}")
    print(f"Slope = {slope:.4f}")

    # 7. Calculate 1-sigma confidence interval
    residual_standard_error = np.sqrt(
        np.sum(residuals ** 2) / (n - 2)
    )

    x_line = np.linspace(
        x.min(),
        x.max(),
        300
    )

    y_line = intercept + slope * x_line

    standard_error_line = (
        residual_standard_error
        * np.sqrt(
            (1 / n)
            + ((x_line - x_mean) ** 2 / Sxx)
        )
    )

    lower_confidence = (
        y_line - standard_error_line
    )

    upper_confidence = (
        y_line + standard_error_line
    )

    # 8. Create scatter plot
    fig, ax = plt.subplots(
        figsize=(7.2, 5.2)
    )

    ax.scatter(
        x,
        y,
        s=24,
        alpha=0.55,
        edgecolors="none",
        label="Schools"
    )

    ax.plot(
        x_line,
        y_line,
        linewidth=2.0,
        label="Linear fit"
    )

    ax.fill_between(
        x_line,
        lower_confidence,
        upper_confidence,
        alpha=0.22,
        label="1-sigma confidence interval"
    )

    # 9. Format figure
    ax.set_xlabel(
        "Mobility percentage (%)"
    )

    ax.set_ylabel(
        "College enrollment percentage (%)"
    )

    ax.set_title(
        "Mobility vs. College Enrollment in Chicago Public Schools"
    )

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.legend(frameon=False)

    fig.tight_layout()

    # 10. Save PDF
    output_pdf = folder / OUTPUT_FILENAME

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

    # 11. Display figure
    if not args.no_show:
        plt.show()

    plt.close(fig)


if __name__ == "__main__":
    main()
