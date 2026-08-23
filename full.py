import os
import subprocess
import pandas as pd
import matplotlib.pyplot as plt

# --- Config ---
FULLDATA = "fulldata.csv"
MAINDATA = "maindata.csv"
NAMES = "names.csv"
MATCHDATA = "matchdata.csv"
DATAANALYSIS_DIR = "dataAnalysis"
STATDATA = os.path.join(DATAANALYSIS_DIR, "statdata.csv")

os.makedirs(DATAANALYSIS_DIR, exist_ok=True)

# --- Read fulldata.csv ---
print("Loading", FULLDATA)
df = pd.read_csv(FULLDATA)

# Convert numeric whole-number columns to integers.
# Prevents values such as 5.0 from reaching the Haskell Int parser.
for col in df.columns:
    numeric = pd.to_numeric(df[col], errors="coerce")
    non_empty = numeric.dropna()

    if len(non_empty) > 0 and (non_empty % 1 == 0).all():
        df[col] = numeric.astype("Int64")

# --- Save first column ---
df.iloc[:, [0]].to_csv("time.csv", index=False)
print("Saved first column to time.csv")

# --- Create maindata.csv by dropping first, second and last columns ---
main_df = df.drop(df.columns[[0, 1, -1]], axis=1)
main_df.to_csv(MAINDATA, index=False)
print(f"Saved {MAINDATA}")

# --- Save first column of maindata.csv as names.csv ---
first_col = main_df.iloc[:, 0]
first_col.to_csv(NAMES, index=False, header=False)
print(f"Saved {NAMES} (first column from {MAINDATA} with no header)")

# --- Create matchdata.csv without header ---
# Use main_df directly so pandas does not re-infer the data types.
main_df.to_csv(MATCHDATA, index=False, header=False)
print(f"Saved {MATCHDATA} (no CSV header written — data rows preserved)")

# --- Create statdata.csv ---
stat_df = main_df.drop(main_df.columns[[0, 1]], axis=1)
stat_df.to_csv(STATDATA, index=False)
print(f"Saved {STATDATA} (first two columns removed, all rows preserved)")


# --- Optional verification (prints first few lines of files) ---
# print("\n--- Verification preview ---")
# print("maindata.csv head (first 3 rows):")
# print(pd.read_csv(MAINDATA, header=0).head(3).to_string(index=False))
# print("\nnames.csv (raw):")
# with open(NAMES, "r") as f:
#     print(f.read().splitlines()[:5])
# print("\nmatchdata.csv first 3 lines (raw):")
# with open(MATCHDATA, "r") as f:
#     for i, line in enumerate(f):
#         print(line.strip())
#         if i >= 2:
#             break
# print("\nstatdata.csv head (first 3 rows):")
# print(pd.read_csv(STATDATA).head(3).to_string(index=False))

# --- Run Haskell backend (cabal run) ---
print("\nRunning 'cabal run' ...")
subprocess.run(["cabal", "run"], check=True)
print("cabal run finished — expect matches.csv and top.csv to be created by your Haskell backend.")

# # --- Run gen.py ---
# print("Running gen.py ...")
# subprocess.run(["python", "gen.py"], check=True)
# print("gen.py finished.")

# # --- Run analysis scripts inside dataAnalysis ---
# print("Running dataAnalysis scripts (anomalies.py, correlation.py, gini.py) ...")
# cwd = os.getcwd()
# os.chdir(DATAANALYSIS_DIR)
# for script in ["anomalies.py", "correlation.py", "gini.py"]:
#     print(f" -> Running {script}")
#     subprocess.run(["python", script], check=True)
# os.chdir(cwd)
print("All analysis scripts completed.")
