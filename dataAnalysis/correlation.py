import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import kendalltau

# -----------------------------
# Load and clean data
# -----------------------------
def load_data(filename, delimiter=","):
    """
    Reads the dataset from a file, ensuring proper conversion to numeric values.
    """
    df = pd.read_csv(filename, delimiter=delimiter, header=0)
    df = df.apply(pd.to_numeric, errors='coerce')
    return df


# -----------------------------
# Compute Kendall correlations and significance
# -----------------------------
def compute_kendall_significance(df):
    """
    Computes Kendall's tau correlation and significance (p-values) for all question pairs.

    Returns:
    - corr_df: DataFrame of Kendall's tau values
    - pval_df: DataFrame of corresponding p-values
    - results_list: List of tuples (q1, q2, tau, p)
    """
    cols = df.columns
    n = len(cols)
    corr_matrix = pd.DataFrame(index=cols, columns=cols, dtype=float)
    pval_matrix = pd.DataFrame(index=cols, columns=cols, dtype=float)
    results = []

    for i in range(n):
        for j in range(i, n):
            x = df[cols[i]]
            y = df[cols[j]]
            valid = x.notna() & y.notna()

            if valid.sum() > 2:
                tau, p = kendalltau(x[valid], y[valid], nan_policy="omit")
            else:
                tau, p = (float('nan'), float('nan'))

            corr_matrix.iloc[i, j] = corr_matrix.iloc[j, i] = tau
            pval_matrix.iloc[i, j] = pval_matrix.iloc[j, i] = p

            if i != j:
                results.append((cols[i], cols[j], tau, p))

    return corr_matrix, pval_matrix, results


# -----------------------------
# Filter strong and significant correlations
# -----------------------------
def filter_significant_correlations(results, corr_threshold=0.3, alpha=0.05):
    """
    Filters correlation results by strength and statistical significance.
    """
    filtered = [
        (q1, q2, tau, p)
        for q1, q2, tau, p in results
        if abs(tau) >= corr_threshold and p < alpha
    ]
    return filtered


# -----------------------------
# Save correlation results
# -----------------------------
def save_correlation_results(results, filename="dataAnalysis/significant_correlations.txt"):
    """
    Saves significant Kendall correlations to a text file, formatting small p-values clearly.
    """
    with open(filename, "w") as f:
        f.write("Significant Kendall Correlations (|τ| ≥ 0.3, p < 0.05)\n")
        f.write("=" * 70 + "\n\n")
        for q1, q2, tau, p in sorted(results, key=lambda x: -abs(x[2])):
            # Nicely format p-values
            if pd.isna(p):
                p_str = "NaN"
            elif p < 0.0001:
                p_str = "<0.0001"
            elif p < 0.001:
                p_str = f"{p:.2e}"
            else:
                p_str = f"{p:.4f}"
            f.write(f"{q1} <-> {q2}: τ = {tau:.3f}, p = {p_str}\n")
    print(f"Saved {len(results)} significant correlations to {filename}")


# -----------------------------
# Plot correlation heatmap (masking non-significant cells)
# -----------------------------
def plot_heatmap(corr_matrix, pval_matrix=None, title="Kendall Correlation Matrix", alpha=0.05):
    """
    Plots a heatmap of Kendall's correlations.
    Optionally masks non-significant cells.
    """
    plt.figure(figsize=(12, 8))
    if pval_matrix is not None:
        mask = pval_matrix >= alpha  # mask non-significant cells
        sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f", linewidths=0.5, mask=mask)
    else:
        sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f", linewidths=0.5)
    plt.title(title)
    plt.tight_layout()
    plt.show()


# -----------------------------
# Main function
# -----------------------------
def main():
    filename = "dataAnalysis/statdata.csv"  # Adjust as needed
    df = load_data(filename)

    # Compute Kendall correlations + significance
    kendall_corr, kendall_pval, kendall_results = compute_kendall_significance(df)

    # Save full correlation and p-value matrices
    kendall_corr.to_csv("dataAnalysis/kendall_correlation.csv")
    kendall_pval.to_csv("dataAnalysis/kendall_pvalues.csv")

    # Filter significant & strong pairs
    significant_pairs = filter_significant_correlations(kendall_results, corr_threshold=0.3, alpha=0.05)

    # Save formatted results
    save_correlation_results(significant_pairs, "dataAnalysis/significant_kendall_correlations.txt")

    # # Plot heatmap of significant Kendall correlations
    # plot_heatmap(kendall_corr, kendall_pval, "Kendall Correlation (Significant Only)")

    print("Kendall correlation analysis complete.")
    print(f"{len(significant_pairs)} significant correlations found (|τ| ≥ 0.3, p < 0.05).")


if __name__ == "__main__":
    main()
