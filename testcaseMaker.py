import random
import csv

# Generate CSV data
csv_filename = "/Users/deepthought/Desktop/Python/case.csv"

with open(csv_filename, mode="w", newline="") as file:
    writer = csv.writer(file)
    
    # Write header
    writer.writerow(["Name", "Email"] + [f"Number_{i}" for i in range(1, 51)])
    
    # Generate 100 rows
    for i in range(1, 1001):
        name = f"User{i}"
        email = f"user{i}@example.com"
        numbers = [random.randint(1, 10) for _ in range(50)]
        writer.writerow([name, email] + numbers)

csv_filename