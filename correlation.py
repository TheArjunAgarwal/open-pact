import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Load and clean data
def load_data(filename, delimiter=","):
    """
    Reads the dataset from a file, ensuring proper conversion to numeric values.

    Parameters:
    - filename (str): Path to the file
    - delimiter (str): Character used to separate values in the file (default: comma)

    Returns:
    - DataFrame: Pandas DataFrame containing numerical responses with column labels
    """
    df = pd.read_csv(filename, delimiter=delimiter, header=0)

    # Convert all responses to numeric, forcing errors to NaN if conversion fails
    df = df.apply(pd.to_numeric, errors='coerce')

    return df

# Compute correlation matrix
def compute_correlations(df, method="pearson"):
    """
    Computes correlation between questions.

    Parameters:
    - df (DataFrame): Pandas DataFrame containing question responses
    - method (str): Correlation method ("pearson", "spearman", "kendall")

    Returns:
    - DataFrame: Correlation matrix
    """
    return df.corr(method=method)

# Filter strong correlations
def filter_strong_correlations(corr_matrix, threshold=0.3):
    """
    Filters and extracts strongly correlated pairs (above threshold or below -threshold).

    Parameters:
    - corr_matrix (DataFrame): Correlation matrix
    - threshold (float): Absolute value threshold for strong correlations

    Returns:
    - List of tuples containing strong correlations
    """
    strong_pairs = []
    for i in range(len(corr_matrix.columns)):
        for j in range(i + 1, len(corr_matrix.columns)):  # Avoid duplicate pairs
            if abs(corr_matrix.iloc[i, j]) >= threshold:
                strong_pairs.append((corr_matrix.index[i], corr_matrix.columns[j], corr_matrix.iloc[i, j]))

    return strong_pairs

# Save strong correlations to a text file
def save_strong_correlations(strong_pairs, filename="strong_correlations.txt"):
    """
    Saves strongly correlated question pairs into a text file.

    Parameters:
    - strong_pairs (list of tuples): List of (question1, question2, correlation)
    - filename (str): Output file name
    """
    with open(filename, "w") as f:
        f.write("Strongly Correlated Question Pairs (Threshold ≥ 0.3 or ≤ -0.3)\n")
        f.write("=" * 60 + "\n\n")
        for q1, q2, corr in sorted(strong_pairs, key=lambda x: -abs(x[2])):  # Sort by absolute correlation value
            f.write(f"{q1} <-> {q2}: {corr:.2f}\n")

# Plot correlation heatmap
def plot_heatmap(corr_matrix, title="Correlation Matrix"):
    """
    Plots a heatmap of the correlation matrix.

    Parameters:
    - corr_matrix (DataFrame): Correlation matrix
    - title (str): Title of the heatmap
    """
    plt.figure(figsize=(12, 8))
    sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f", linewidths=0.5)
    plt.title(title)
    plt.show()

# Main function
def main():
    filename = "test.csv"  # Change to your actual file name
    df = load_data(filename)

    # Compute different correlation types
    pearson_corr = compute_correlations(df, method="pearson")
    spearman_corr = compute_correlations(df, method="spearman")
    kendall_corr = compute_correlations(df, method="kendall")

    # Save full correlation matrices
    pearson_corr.to_csv("pearson_correlation.csv")
    spearman_corr.to_csv("spearman_correlation.csv")
    kendall_corr.to_csv("kendall_correlation.csv")

    # Filter strong correlations
    strong_pairs = filter_strong_correlations(pearson_corr, threshold=0.3)

    # Save strong correlations to a text file
    save_strong_correlations(strong_pairs, "strong_correlations.txt")

    # Plot correlation heatmap
    plot_heatmap(pearson_corr, "Pearson Correlation Matrix")

    print("Correlation matrices saved to CSV files.")
    print(f"Strong correlations saved in 'strong_correlations.txt' ({len(strong_pairs)} pairs found).")

if __name__ == "__main__":
    main()
