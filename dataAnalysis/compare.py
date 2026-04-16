import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import numpy as np

# -----------------------------
# Load and clean data
# -----------------------------
def load_data(filename, delimiter=","):
    """
    Reads the dataset from a file, ensuring proper conversion to numeric values.
    """
    df = pd.read_csv(filename, delimiter=delimiter, header=0)
    df = df.apply(pd.to_numeric, errors="coerce")
    return df


# -----------------------------
# Compare two years with t-test + effect size
# -----------------------------
def compare_years_with_significance(df_current, df_previous, alpha=0.05):
    """
    Compares responses between two years for each question using Welch's t-test.
    Also computes Cohen's d effect size.

    Returns a DataFrame sorted by absolute change.
    """
    results = []
    all_questions = set(df_current.columns).union(set(df_previous.columns))

    for q in all_questions:
        x = df_previous[q].dropna() if q in df_previous else pd.Series(dtype=float)
        y = df_current[q].dropna() if q in df_current else pd.Series(dtype=float)

        # Skip if insufficient data
        if len(x) < 2 or len(y) < 2:
            continue

        mean_prev, mean_curr = x.mean(), y.mean()
        var_prev, var_curr = x.var(ddof=1), y.var(ddof=1)
        n_prev, n_curr = len(x), len(y)

        # Welch's t-test (unequal variances)
        t_stat, p_val = stats.ttest_ind(y, x, equal_var=False, nan_policy='omit')
        change = mean_curr - mean_prev

        # Compute pooled SD for Cohen's d
        pooled_sd = np.sqrt(((n_prev - 1) * var_prev + (n_curr - 1) * var_curr) / (n_prev + n_curr - 2))
        cohens_d = np.nan if pooled_sd == 0 else change / pooled_sd

        results.append({
            "Question": q,
            "Last Year Mean": mean_prev,
            "This Year Mean": mean_curr,
            "Change": change,
            "AbsChange": abs(change),
            "t-stat": t_stat,
            "p-value": p_val,
            "Significant (p<0.05)": p_val < alpha,
            "Cohen's d": cohens_d,
            "n_year1": n_prev,
            "n_year2": n_curr
        })

    results_df = pd.DataFrame(results)
    results_df.sort_values("AbsChange", ascending=False, inplace=True)

    return results_df


# -----------------------------
# Save and plot results
# -----------------------------
def save_significant_changes(results_df, filename="dataAnalysis/significant_changes.csv", alpha=0.05):
    """
    Saves significant changes to a CSV file.
    """
    sig_df = results_df[results_df["p-value"] < alpha]
    sig_df.to_csv(filename, index=False)
    print(f"Saved {len(sig_df)} significant changes to {filename}")
    return sig_df


def plot_top_changes(results_df, top_n=10):
    """
    Plots the top N questions with the largest absolute changes.
    """
    top_changes = results_df.head(top_n)
    plt.figure(figsize=(10, 6))
    sns.barplot(x="Change", y="Question", data=top_changes, palette="coolwarm")
    plt.title(f"Top {top_n} Questions with Most Change in Responses")
    plt.xlabel("Change in Mean (This Year - Last Year)")
    plt.ylabel("Question")
    plt.tight_layout()
    plt.show()


# -----------------------------
# Main
# -----------------------------
def main():
    # File paths (update as needed)
    current_year_file = "dataAnalysis/statdata_2025.csv"
    last_year_file = "dataAnalysis/statdata_2024.csv"

    # Load both datasets
    df_current = load_data(current_year_file)
    df_previous = load_data(last_year_file)

    # Compare with statistical tests
    results = compare_years_with_significance(df_current, df_previous)

    # Save significant results
    sig_df = save_significant_changes(results, "dataAnalysis/significant_changes.csv")

    # Plot top overall changes (regardless of significance)
    plot_top_changes(results, top_n=10)

    print("\nTop significant changes:")
    print(sig_df.head(10))


if __name__ == "__main__":
    main()
