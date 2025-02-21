import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import entropy


# Load the data
def load_data(filename, delimiter=","):
    """
    Reads the dataset from a file and converts responses to numeric.

    Parameters:
    - filename (str): Path to the file
    - delimiter (str): Character used to separate values (default: comma)

    Returns:
    - DataFrame: Pandas DataFrame containing numerical responses with column labels
    """
    df = pd.read_csv(filename, delimiter=delimiter, header=0)
    df = df.apply(pd.to_numeric,
                  errors='coerce')  # Convert responses to numeric
    return df


# Identify even distributions
def find_even_distributions(df, threshold=0.15):
    """
    Finds questions where responses are spread almost evenly.

    Parameters:
    - df (DataFrame): DataFrame of responses
    - threshold (float): Max difference in probabilities to qualify as 'even'

    Returns:
    - List of questions with even distributions
    """
    even_questions = []
    for col in df.columns:
        counts = df[col].value_counts(
            normalize=True)  # Get percentage distribution
        if max(counts) - min(
                counts) < threshold:  # Check if spread is fairly uniform
            even_questions.append(col)
    return even_questions


# Identify lopsided questions
def find_lopsided_questions(df, dominance_threshold=0.4):
    """
    Finds questions where one response dominates significantly.

    Parameters:
    - df (DataFrame): DataFrame of responses
    - dominance_threshold (float): Minimum percentage a single answer must have to be 'lopsided'

    Returns:
    - List of tuples (question, dominant answer, percentage)
    """
    lopsided_questions = []
    for col in df.columns:
        counts = df[col].value_counts(normalize=True)
        if counts.iloc[
                0] >= dominance_threshold:  # If the most common answer is too dominant
            lopsided_questions.append((col, counts.index[0], counts.iloc[0]))

    well_formatteded_lopsided_questions = []
    for (col, answer, percentage) in lopsided_questions:
        well_formatteded_lopsided_questions.append(
            f"{col} - {answer}: {100*percentage:.2f}%")
    return well_formatteded_lopsided_questions


# Identify polarized questions
def find_polarized_questions2(df, group_count=2, min_share=0.5):
    """
    Finds questions where responses are heavily clustered into 2-3 options.

    Parameters:
    - df (DataFrame): DataFrame of responses
    - group_count (int): Number of dominant groups to check
    - min_share (float): Minimum total share of responses for polarization

    Returns:
    - List of tuples (question, dominant answers, total share)
    """
    polarized_questions = []
    for col in df.columns:
        counts = df[col].value_counts(normalize=True)
        if counts.iloc[:group_count].sum(
        ) >= min_share:  # If top answers dominate
            polarized_questions.append((col, list(counts.index[:group_count]),
                                        counts.iloc[:group_count].sum()))

    well_formatteded_polarized_questions = []
    for (col, answers, total_share) in polarized_questions:
        well_formatteded_polarized_questions.append(
            f"{col} - {', '.join(map(str, answers))}: {100*total_share:.2f}%"
        )
    return well_formatteded_polarized_questions

def find_polarized_questions3(df, group_count=3, min_share=0.5):
    """
    Finds questions where responses are heavily clustered into 2-3 options.

    Parameters:
    - df (DataFrame): DataFrame of responses
    - group_count (int): Number of dominant groups to check
    - min_share (float): Minimum total share of responses for polarization

    Returns:
    - List of tuples (question, dominant answers, total share)
    """
    polarized_questions = []
    for col in df.columns:
        counts = df[col].value_counts(normalize=True)
        if counts.iloc[:group_count].sum(
        ) >= min_share:  # If top answers dominate
            polarized_questions.append((col, list(counts.index[:group_count]),
                                        counts.iloc[:group_count].sum()))
    well_formatted_polarized_questions = []
    for (col, answers, total_share) in polarized_questions:
        well_formatted_polarized_questions.append(
            f"{col} - {', '.join(map(str, answers))}: {100*total_share:.2f}%")
    return well_formatted_polarized_questions


# Identify fence-sitting questions
def find_fence_sitters(df, middle_range=(4, 6), min_share=0.4):
    """
    Finds questions where most responses fall into the middle range (e.g., 4-6 on a 1-10 scale).

    Parameters:
    - df (DataFrame): DataFrame of responses
    - middle_range (tuple): Range of "neutral" responses
    - min_share (float): Minimum percentage required to qualify as fence-sitting

    Returns:
    - List of tuples (question, middle response share)
    """
    fence_sitters = []
    for col in df.columns:
        middle_responses = df[col][df[col].between(middle_range[0],
                                                   middle_range[1])].count()
        total_responses = df[col].count()
        middle_share = middle_responses / total_responses
        if middle_share >= min_share:
            fence_sitters.append((col, middle_share))
    
    well_formatted_fence_sitters = []
    
    for (col, middle_share) in fence_sitters:
        well_formatted_fence_sitters.append(
            f"{col}")
    return well_formatted_fence_sitters
    
def find_bench_sitters(df, middle_range=(6, 8), min_share=0.4):
    """
    Finds questions where most responses fall into the just above middle or the psychological middle range (e.g., 6-8 on a 1-10 scale).

    Parameters:
    - df (DataFrame): DataFrame of responses
    - middle_range (tuple): Range of "neutral" responses
    - min_share (float): Minimum percentage required to qualify as fence-sitting

    Returns:
    - List of tuples (question, middle response share)
    """
    bench_sitters = []
    for col in df.columns:
        middle_responses = df[col][df[col].between(middle_range[0],
                                                   middle_range[1])].count()
        total_responses = df[col].count()
        middle_share = middle_responses / total_responses
        if middle_share >= min_share:
            bench_sitters.append((col, middle_share))


    well_formatted_bench_sitters = []

    for (col, middle_share) in bench_sitters:
        well_formatted_bench_sitters.append(
            f"{col}")
    return well_formatted_bench_sitters


# Identify low-variance questions (everyone answers similarly)
def find_low_variance(df, threshold=3.0):
    """
    Finds questions where responses have very low variance (little variation).

    Parameters:
    - df (DataFrame): DataFrame of responses
    - threshold (float): Variance threshold below which a question is considered 'low variance'

    Returns:
    - List of questions with low variance
    """
    return [col for col in df.columns if df[col].var() < threshold]


# Identify high-variance questions (responses are all over the place)
def find_high_variance(df, threshold=10.0):
    """
    Finds questions where responses have very high variance (big spread).

    Parameters:
    - df (DataFrame): DataFrame of responses
    - threshold (float): Variance threshold above which a question is considered 'high variance'

    Returns:
    - List of questions with high variance
    """
    return [col for col in df.columns if df[col].var() > threshold]


# Save results to a text file
def save_anomalies(filename, **kwargs):
    """
    Saves detected anomalies to a text file.

    Parameters:
    - filename (str): Output file name
    - kwargs: Named lists of anomalies (e.g., even_distributions=[...])
    """
    with open(filename, "w") as f:
        f.write("📊 Fun Statistical Anomalies Detected\n")
        f.write("=" * 60 + "\n\n")

        for key, values in kwargs.items():
            f.write(f"🔹 {key.replace('_', ' ').title()}:\n")
            if not values:
                f.write("  - None found\n")
            elif isinstance(values[0], tuple):
                for item in values:
                    f.write(f"  - {item}\n")
            else:
                for v in values:
                    f.write(f"  - {v}\n")
            f.write("\n")


# Main function
def main():
    filename = "test.csv"  # Change to your actual file
    df = load_data(filename)

    # Find anomalies
    even_distributions = find_even_distributions(df)
    lopsided_questions = find_lopsided_questions(df)
    polarized_questions_2 = find_polarized_questions2(df)
    polarized_questions_3 = find_polarized_questions3(df)
    fence_sitters = find_fence_sitters(df)
    bench_sitters = find_bench_sitters(df)
    low_variance = find_low_variance(df)
    high_variance = find_high_variance(df)

    # Save results
    save_anomalies("statistical_anomalies.txt",
                   even_distributions=even_distributions,
                   lopsided_questions=lopsided_questions,
                   polarized_questions2=polarized_questions_2,
                   polarized_questions3=polarized_questions_3,
                   fence_sitters=fence_sitters,
                   bench_sitters=bench_sitters,
                   low_variance=low_variance,
                   high_variance=high_variance)

    print("Statistical anomalies saved in 'statistical_anomalies.txt'.")


if __name__ == "__main__":
    main()
