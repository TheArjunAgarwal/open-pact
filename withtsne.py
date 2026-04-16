import pandas as pd
import warnings
from sklearn.decomposition import PCA
from scipy.spatial import ConvexHull
import matplotlib.pyplot as plt
import numpy as np

warnings.filterwarnings('ignore')

def visualize_data(file_path='maindata.csv', pca_variance_ratio=False,
                   label_name='', top_n=10, top_avg_n=5):
    """
    Loads CSV with question headers, performs PCA on numeric columns,
    prints top contributing questions per principal component,
    draws convex hulls, highlights specific person and most average people.
    Handles label overlap near centroid gracefully.
    """
    try:
        df = pd.read_csv(file_path)
        df.dropna(inplace=True)

        # Identify numeric columns
        numeric_cols = df.select_dtypes(include='number').columns.tolist()
        if not numeric_cols:
            raise ValueError("No numeric columns found for PCA!")

        data_vectors = df[numeric_cols].values

        # --- PCA ---
        print("\nGenerating PCA projection visualization...")
        pca = PCA(n_components=2, random_state=42)
        pca_results = pca.fit_transform(data_vectors)
        df['pca-one'] = pca_results[:, 0]
        df['pca-two'] = pca_results[:, 1]

        if pca_variance_ratio:
            var_ratio = pca.explained_variance_ratio_
            print(f"\nExplained variance by PCA components:")
            print(f"  PC1: {var_ratio[0]*100:.2f}%")
            print(f"  PC2: {var_ratio[1]*100:.2f}%")

        # --- Top contributing questions per PC ---
        loadings = pd.DataFrame(pca.components_.T, index=numeric_cols, columns=['PC1', 'PC2'])
        print("\nTop contributing questions per principal component:")
        for pc in ['PC1', 'PC2']:
            print(f"\n{pc}:")
            top_features = loadings[pc].abs().sort_values(ascending=False).head(top_n)
            for q in top_features.index:
                coef = loadings.loc[q, pc]
                print(f"  {q}: {coef:.4f}")

        # --- Compute centroid and distances ---
        centroid = df[['pca-one', 'pca-two']].mean().values
        df['distance_from_center'] = np.sqrt(
            (df['pca-one'] - centroid[0]) ** 2 + (df['pca-two'] - centroid[1]) ** 2
        )

        # Identify most average respondents
        avg_people = df.nsmallest(top_avg_n, 'distance_from_center')
        print(f"\nMost average {top_avg_n} respondents (closest to data centroid):")
        for _, row in avg_people.iterrows():
            print(f"  {row.iloc[0]} - distance: {row['distance_from_center']:.4f}")

        # --- Plot PCA ---
        plt.figure(figsize=(10, 7))
        plt.scatter(df['pca-one'], df['pca-two'], s=40, alpha=0.6, label='All respondents')

        # Convex hull (outliers)
        # points = df[['pca-one', 'pca-two']].values
        # if len(points) > 2:
        #     hull = ConvexHull(points)
        #     hull_points = points[hull.vertices]
        #     plt.fill(hull_points[:, 0], hull_points[:, 1], alpha=0.15,
        #              color='lightblue', label='Convex Hull (Outliers)')
        #     for i in hull.vertices:
        #         plt.text(df['pca-one'].iloc[i] + 0.02, df['pca-two'].iloc[i] + 0.02,
        #                  df.iloc[i, 0], fontsize=8, alpha=0.8)

        # --- Highlight most average people ---
        plt.scatter(avg_people['pca-one'], avg_people['pca-two'],
                    color='green', label='CMI-est People')

        # Offset labels to prevent overlap
        # offset_angle = np.linspace(0, np.pi, len(avg_people))  # spread labels in a semicircle
        # offset_dist = 0.08  # how far to shift text

        # for (i, (_, row)) in enumerate(avg_people.iterrows()):
        #     x, y = row['pca-one'], row['pca-two']
        #     dx = np.cos(offset_angle[i]) * offset_dist
        #     dy = np.sin(offset_angle[i]) * offset_dist
        #     plt.annotate(
        #         row.iloc[0],
        #         xy=(x, y),
        #         xytext=(x + dx, y + dy),
        #         textcoords='data',
        #         fontsize=9,
        #         color='green',
        #         fontweight='bold',
        #         arrowprops=dict(arrowstyle='->', lw=0.8, color='gray', alpha=0.6)
        #     )

        # --- Highlight specific label (you/me) ---
        match = df[df.iloc[:, 0].str.lower() == label_name.lower()]
        if not match.empty:
            plt.scatter(match['pca-one'], match['pca-two'],
                        color='red', s=120, label=label_name)
            for _, row in match.iterrows():
                plt.text(row['pca-one'] + 0.02, row['pca-two'] + 0.02,
                         row.iloc[0], fontsize=10, fontweight='bold', color='red')

        # --- Centroid marker ---
        # plt.scatter(centroid[0], centroid[1], color='black', s=80, marker='x', label='Centroid')

        plt.title("PCA Projection")
        plt.xlabel("Principal Component 1")
        plt.ylabel("Principal Component 2")
        plt.grid(True, alpha=0.3)
        plt.legend()
        plt.tight_layout()
        plt.show()

    except FileNotFoundError:
        print(f"Error: The file '{file_path}' was not found.")
    except Exception as e:
        print(f"An error occurred: {e}")


# --- Main ---
if __name__ == "__main__":
    visualize_data(file_path='maindata.csv', pca_variance_ratio=True)
