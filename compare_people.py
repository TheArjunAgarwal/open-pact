import pandas as pd
import matplotlib.pyplot as plt

# -----------------------------
# CONFIG — choose two people
# -----------------------------
personA = ""
personB = ""
csv_file = "maindata.csv"

# -----------------------------
# Load & clean
# -----------------------------
df = pd.read_csv(csv_file)

# Normalize column names to handle "Name" / "Email"
df.columns = [c.strip() for c in df.columns]
colmap = {c.lower(): c for c in df.columns}

if "name" not in colmap:
    raise KeyError("❌ Could not find a 'Name' column in your CSV")

name_col = colmap["name"]
email_col = colmap["email"] if "email" in colmap else None

# Identify question columns
question_cols = [c for c in df.columns if c not in [name_col, email_col]]

# Convert responses to numeric safely
df[question_cols] = df[question_cols].apply(pd.to_numeric, errors="coerce")

# -----------------------------
# Fetch two specific people
# -----------------------------
rowA = df[df[name_col] == personA]
rowB = df[df[name_col] == personB]

if rowA.empty:
    raise ValueError(f"❌ Person '{personA}' not found in Name column")
if rowB.empty:
    raise ValueError(f"❌ Person '{personB}' not found in Name column")

pA = rowA.iloc[0][question_cols]
pB = rowB.iloc[0][question_cols]

# -----------------------------
# Compute differences
# -----------------------------
diffs = pA - pB  # positive = A scored higher, negative = B scored higher
absdiffs = diffs.abs().sort_values(ascending=False)

# Build results table
results = pd.DataFrame({
    "Question": absdiffs.index,
    "Difference": diffs[absdiffs.index].values,
    personA: pA[absdiffs.index].values,
    personB: pB[absdiffs.index].values
})

# Print nicely
print("\nTop 20 differences:\n")
print(results.head(20).to_string(index=False))

# -----------------------------
# Plot diverging bar chart
# -----------------------------
top = results.head(50)
top = top[top["Difference"].abs() > 4]

plt.figure(figsize=(12, 8))
bars = plt.barh(top["Question"], top["Difference"], color=["#4CAF50" if d > 0 else "#F44336" for d in top["Difference"]])
plt.gca().invert_yaxis()

plt.axvline(0, color="black", linewidth=1)
plt.title(f"{personB} vs {personA}")
plt.xlabel(f"{personB} higher  ←→  {personA} higher")

# labels on bars
for bar, diff in zip(bars, top["Difference"]):
    plt.text(bar.get_width() + (0.3 if diff > 0 else -0.3),
             bar.get_y() + bar.get_height()/2,
             f"{diff:+.1f}",
             va="center",
             ha="center",
             fontsize=9)

plt.tight_layout()
plt.show()