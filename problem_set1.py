# Write a program to convert the last character of all the words in a sentence into capital case. It should not convert single letter, propositions, conjunctions. Clue: Use the title function and slicing techniques

def problem_1(sentence):
    # List of prepositions and conjunctions, courtasy GPT, to exclude
    exceptions = {"and", "or", "but", "nor", "for", "yet", "so", "a", "an", "the", 
                  "in", "on", "at", "to", "by", "with", "of", "up", "out", "about", "as", "off", "over"}
    
    words = sentence.split()
    ans = ""
    for word in words:
        if len(word) == 1 or word.lower() in exceptions:
            new_word = word
        else:
            new_word = word[:-1].lower() + word[-1].upper()  # Change the last character to uppercase
        ans += new_word + " "
    return ans


# Check whether string is a palindrome. Output True/False

def problem_2(str):
    return str.lower().replace(" ","") == str[::-1].lower().replace(" ","")

# Count the frequency of each character in a string and display the result.

def problem_3(str):
    ans = [0]*26
    for i in str:
        if not i.isalpha():
            continue
        if i.isupper():
            i = i.lower()
        count = ord(i) - ord("a")
        ans[count] += 1
    return ans

# Write a function to determine if two strings are anagrams of each other. Example -listen and silent are anagrams

def problem_4(str1,str2):
    return (problem_3(str1)) == (problem_3(str2))

# Count the frequency of each word in a sentence and output the result for each word

def problem_5(str):
    str = str.lower()
    words = str.split()
    ans = {}
    for word in words:
        if word in ans:
            ans[word] += 1
        else:
            ans[word] = 1
    return ans

# Remove duplicate characters from a string

def problem_6(str):
    seen = ""
    ans = ""
    for i in str:
        if i in seen:
            continue
        else:
            ans += i
    return ans



# Reverse the order of words in a sentence and reverse every word

def problem_7(str):
    words = str.split()
    rev_words = [word[::-1] for word in words]
    rev_sentence = rev_words[::-1]
    return " ".join(rev_sentence)

print(problem_7("This is a dumb sentence put here for the sake of testing"))

# Generate all permutations of a string

def problem_8(str):
    pass

# Write a function to encrypt a string using the following rules:
## Reverse the string
## Replace vowels with their ASCII value
## Append the length of the original string at the end
## Example: hello 111ll101h5
## Note: Use ord() function to get the ASCII value of a character

def problem_9(string):
    rev = string[::-1]
    length = len(string)
    vowels = "aeiouAEIOU"
    ans = ""
    for i in rev:
        if i in vowels:
            ans += str(ord(i))
        else:
            ans += i
    return ans + str(length)



# Write a function to decode the string encoded using the previous algorithm given in 9

def problem_10(string):
    pass


# Write a function to convert a simple Markdown string to HTML. Rules:
# # becomes <h1>, ## becomes <h2>
#  *text* becomes <em>text</em>
#  **text** becomes <strong>text</strong>.
#  Key Functions: startswith(), replace()

def problem_11(string):
    string = string.replace("##", "<h2>").replace("#", "<h1>")
    ans = ""
    holder = ""
    star_flag = False
    strong_flag = False
    i = 0

    while i < len(string):
        if string[i:i+2] == "**":  # Handle ** for <strong>
            if strong_flag:
                strong_flag = False
                ans += "<strong>" + holder + "</strong>"
                holder = ""
            else:
                strong_flag = True
            i += 1  # Skip the next '*' since it's part of '**'
        elif string[i] == "*":  # Handle * for <em>
            if star_flag:
                star_flag = False
                ans += "<em>" + holder + "</em>"
                holder = ""
            else:
                star_flag = True
        elif star_flag or strong_flag:
            holder += string[i]
        else:
            ans += string[i]
        i += 1
    return ans

            

# 12. Given a string and a maximum line width (say 80 characters), wrap the text to fit within the specified width. Break the text at word boundaries (do not split words)

def problem_12(delimitor,string):
    words = string.split()
    counter = 0
    line = ""
    ans = []
    for word in words:
        counter += len(word) + 1
        if counter > delimitor:
            ans.append(line.strip())
            line = ""
            line += word + " "
            counter = 0
        else:
            line += word + " "
    return ("\n".join(ans))

# 13. Find out whether an ISBN-10 string is valid or not. Example: ISBN-10 of 81-85015-96-1 is a valid string. It is verified as follows:
## sum = (8 × 10) + (1 × 9) + (8 × 8) + (5 × 7) + (0 × 6) + (1 × 5) + (5 × 4) + (9 × 3)+ (6 × 2) + (1 × 1)
## if sum mod 11 == 0, then it is a valid ISBN-10 string

def problem_13(string):
    isbn_number = ""
    for i in string:
        if i.isdigit():
            isbn_number += i
        else:
            continue
    
    verifier = 0
    i = 0
    k = len(isbn_number)
    while i < k:
        verifier += int(isbn_number[i]) * (k - i)
        i += 1
    return (verifier % 11 == 0)