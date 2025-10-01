# **OpenPact** 💍🔗  

**OpenPact** is an open-source **Haskell implementation** inspired by **Marriage Pact** ([marriagepact.com](https://www.marriagepact.com/)).  
It provides **algorithmic matchmaking** with modes for:  
✅ **Romantic matches(still to be configured)** (heterosexual & homosexual)  
✅ **Best friend matches** (platonic compatibility)  

Additionally, **data analysis tools** help uncover **patterns, anomalies, and correlations** in responses.  

---

## **📁 Project Structure**
```
app/
    ├── CsvDecoder.hs              # Parses CSV data into Haskell data types
    ├── CsvMaker.hs                # Generates match output in CSV format
    ├── Irving.hs                  # Irving's Stable Roommates Algorithm for matchmaking
    └── Main.hs                    # Main execution flow of OpenPact
dataAnalysis/
    ├── anomalies.py              # Detects statistical anomalies in response data
    ├── correlation_heatmap.png   # Visualization of question correlations
    ├── correlation.py            # Computes Pearson, Spearman, Kendall correlations
    ├── data.csv                  # Annonymized Data
    ├── kendall_correlation.csv   # Kendall correlation matrix
    ├── pearson_correlation.csv   # Pearson correlation matrix
    ├── spearman_correlation.csv  # Spearman correation matrix
    ├── statistical_anomalies.txt # List of questions with unusual response patterns
    └── strong_correlations.txt   # Questions with strong correlations
.gitignore                        # gitignore for project files
CHANGELOG.md                      # Changelog for project updates
form-questions.txt                # Questions asked on the google form
gsaAndScore.hs                    # Additional scoring and matching logic for romantic mathes(yet to add support csv)
LICENSE                           # License file
Marraige-Pact.cabal               # Cabal package configuration
README.md                         # YOU ARE HERE!
rosettatest.hs                    # Some test cases from Rosetta Code
send_emails.py                    # Sends match results via email
test.csv                          # A huge csv file to test stuff out
testcaseMaker.py                  # The python file which made the test csv.
```

---

## **🚀 How It Works**
1. **Matchmaking Algorithm (Haskell)**  
   - Uses **Irving's Stable Roommates Algorithm** (`Irving.hs`) to compute optimal pairings.  
   - Reads participant responses from **`realData.csv`** (or `test.csv` for testing).  
   - Outputs results in **`matches.csv`** and **`top.csv`** (highest compatibility pairs).  

2. **Email Delivery (Python)**  
   - The **`email.py`** script sends match results via email.  

3. **Data Analysis (Python)**  
   - **`correlation.py`**: Finds relationships between questions.  
   - **`anomalies.py`**: Detects statistical oddities (polarized questions, fence-sitting, etc.).  
   - **Outputs:**  
     - **`pearson_correlation.csv`**, **`spearman_correlation.csv`**, **`kendall_correlation.csv`**  
     - **`correlation_heatmap.png`** (visual correlation map)  
     - **`statistical_anomalies.txt`**, **`strong_correlations.txt`**  

---


## **📜 License**
This project is licensed under the **BSD-3-Clause License**.  

```
Copyright (c) 2025, Arjun Maneesh Agarwal

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

---

Feel free to open an **issue** or submit a **pull request**! 🚀
