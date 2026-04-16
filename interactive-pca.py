import pandas as pd
import warnings
from sklearn.decomposition import PCA
from scipy.spatial import ConvexHull
import matplotlib.pyplot as plt
import numpy as np
import mplcursors  # <--- Added this

warnings.filterwarnings('ignore')

def visualize_data(file_path='maindata.csv', pca_variance_ratio=False,
                    label_name='', top_n=10, top_avg_n=5):
    try:
        df = pd.read_csv(file_path)
        df.dropna(inplace=True)

        numeric_cols = df.select_dtypes(include='number').columns.tolist()
        if not numeric_cols:
            raise ValueError("No numeric columns found for PCA!")

        data_vectors = df[numeric_cols].values

        # --- PCA ---
        pca = PCA(n_components=2, random_state=42)
        pca_results = pca.fit_transform(data_vectors)
        df['pca-one'] = pca_results[:, 0]
        df['pca-two'] = pca_results[:, 1]

        # --- Plot PCA ---
        fig, ax = plt.subplots(figsize=(10, 7))
        
        # Capture the scatter object in a variable 'sc'
        sc = ax.scatter(df['pca-one'], df['pca-two'], s=40, alpha=0.6, label='All respondents', picker=True)

        # --- Convex hull ---
        points = df[['pca-one', 'pca-two']].values
        if len(points) > 2:
            hull = ConvexHull(points)
            hull_points = points[hull.vertices]
            ax.fill(hull_points[:, 0], hull_points[:, 1], alpha=0.1, color='lightblue', label='Convex Hull')

        # --- Highlight specific label ---
        match = df[df.iloc[:, 0].str.lower() == label_name.lower()]
        if not match.empty:
            ax.scatter(match['pca-one'], match['pca-two'], color='red', s=120, label=f"Target: {label_name}")

        # --- INTERACTIVE HOVER LOGIC ---
        # This tells mplcursors to show the name from the first column of the dataframe
        cursor = mplcursors.cursor(sc, hover=True)
        
        @cursor.connect("add")
        def _(sel):
            # sel.index is the index of the point hovered over
            name = df.iloc[sel.index, 0] 
            sel.annotation.set_text(name)
            sel.annotation.get_bbox_patch().set(fc="white", alpha=0.9)

        ax.set_title("PCA Projection (Hover over points to see names)")
        ax.set_xlabel("Principal Component 1")
        ax.set_ylabel("Principal Component 2")
        ax.grid(True, alpha=0.3)
        ax.legend()
        plt.tight_layout()
        plt.show()

    except FileNotFoundError:
        print(f"Error: The file '{file_path}' was not found.")
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    # Ensure you have a maindata.csv in the same directory
    visualize_data(file_path='maindata.csv', pca_variance_ratio=True)