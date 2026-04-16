import pandas as pd
import numpy as np

def earth_movers_distance(P, Q):
    """
    Compute the Earth Mover's Distance (EMD) between two discrete distributions P and Q.
    Both P and Q must be probability distributions (sum to 1).
    """
    E_prev = 0.0
    EMD = 0.0
    for i in range(len(P)):
        E_i = P[i] + E_prev - Q[i]
        EMD += abs(E_i)
        E_prev = E_i
    return EMD

def main():
    # --- Read CSV ---
    try:
        input_csv = "dataAnalysis/statdata.csv"
        df = pd.read_csv(input_csv)
    except FileNotFoundError:
        print(f"Error: Input file not found at {input_csv}")
        return
    except pd.errors.EmptyDataError:
        print(f"Error: Input file {input_csv} is empty.")
        return

    emd_dict = {}
    numeric_cols = df.select_dtypes(include=[np.number]).columns

    for col in numeric_cols:
        data = df[col].dropna().values

        if len(data) == 0:
            print(f"Skipping column '{col}' (no data).")
            continue

        # --- Convert data to frequency distribution ---
        # Assume values are categorical ratings like 1–10
        unique_vals = np.unique(data)
        bins = len(unique_vals)
        
        # Histogram normalized to sum to 1 (probability distribution)
        freq, _ = np.histogram(data, bins=bins, range=(min(unique_vals), max(unique_vals)), density=False)
        P = freq / np.sum(freq)

        # --- Uniform reference distribution ---
        Q = np.ones_like(P) / len(P)

        # --- Compute EMD ---
        emd_value = earth_movers_distance(P, Q)
        emd_dict[col] = emd_value

    if not emd_dict:
        print("No valid numeric data found to calculate EMD.")
        return

    # --- Sort descending (higher = more uneven distribution) ---
    emd_sorted = dict(sorted(emd_dict.items(), key=lambda item: item[1], reverse=True))

    # --- Write results to TXT ---
    output_txt = "dataAnalysis/emd_results.txt"
    try:
        with open(output_txt, "w") as f:
            f.write("Earth Mover's Distance (vs Uniform):\n")
            f.write("------------------------------------\n")
            for col, val in emd_sorted.items():
                f.write(f"{col}: {val:.4f}\n")
        
        print(f"EMD results saved to {output_txt}")
    except IOError as e:
        print(f"Error writing to output file {output_txt}: {e}")

if __name__ == "__main__":
    main()
