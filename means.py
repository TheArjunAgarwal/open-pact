import pandas as pd
import numpy as np
from sklearn.cluster import KMeans

def perform_clustering_simple(
    file_path="maindata.csv",
    n_clusters=3,
    top_k_features=5
):
    """
    Loads data from maindata.csv (first column = name, rest numeric),
    runs standard K-means clustering,
    prints cluster members and top-k defining features per cluster.
    """

    # --- Load Data ---
    df = pd.read_csv(file_path)
    df.dropna(inplace=True)

    name_col = df.columns[0]
    numeric_cols = df.select_dtypes(include="number").columns.tolist()

    if not numeric_cols:
        raise ValueError("No numeric columns found for clustering!")

    X = df[numeric_cols].astype(float).to_numpy()

    # --- K-means clustering (simple version) ---
    km = KMeans(n_clusters=n_clusters, n_init="auto", random_state=42)
    labels = km.fit_predict(X)

    df["cluster"] = labels
    centroids = pd.DataFrame(km.cluster_centers_, columns=numeric_cols)

    # --- Print results ---
    print(f"\n========== K-MEANS CLUSTER RESULTS ({n_clusters} clusters) ==========\n")

    for c in range(n_clusters):
        print(f"\n==================== CLUSTER {c} ====================\n")

        # Members
        members = df[df["cluster"] == c][name_col].tolist()
        print("--- Members ---")
        for m in members:
            print(f"  {m}")

        # Top-k defining features
        print(f"\n--- Top {top_k_features} Defining Features ---")
        centroid = centroids.loc[c]
        top_features = centroid.nlargest(top_k_features)
        for feat, val in top_features.items():
            print(f"  {feat}: {val:.2f}")

    return df, centroids

if __name__ == "__main__":
    perform_clustering_simple(
    file_path="maindata.csv",
    n_clusters=6,       # <-- YOU CHOOSE
    top_k_features=10    # top 5 questions defining each cluster
    )