import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

# --- Load timestamps ---
df = pd.read_csv("time.csv")
df['Timestamp'] = pd.to_datetime(df['Timestamp'])
df = df.sort_values('Timestamp').reset_index(drop=True)

# --- Compute cumulative count ---
df['Cumulative'] = range(1, len(df) + 1)

# --- Plot ---
plt.figure(figsize=(14, 6))

# Line plot
plt.plot(df['Timestamp'], df['Cumulative'], color="#dd1c39", linewidth=2)

# Fill area under curve for star-history style
plt.fill_between(df['Timestamp'], df['Cumulative'], color="#ffffff", alpha=0.3)

# Format x-axis nicely
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d %H:%M'))
plt.xticks(rotation=45)

# Labels and title
plt.xlabel("Time")
plt.ylabel("Cumulative Responses")
plt.title("Star-History Style Worm Plot of Responses")

plt.tight_layout()
plt.savefig("worm_star_history.png", dpi=300)
plt.close()

print("Saved worm_star_history.png")
