import smtplib
import csv
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

def send_email(to_email, subject, body, sender_email, sender_password):
    """
    Sends an email using an SMTP SSL connection.

    Parameters:
    - to_email (str): Recipient email address.
    - subject (str): Email subject line.
    - body (str): Email content.
    - sender_email (str): Sender email address.
    - sender_password (str): Sender email password (should be stored securely).
    """
    try:
        # Create a multipart email message
        msg = MIMEMultipart()
        msg['From'] = sender_email
        msg['To'] = to_email
        msg['Subject'] = subject
        msg.attach(MIMEText(body, 'plain'))

        # Establish a secure connection to the SMTP server
        server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
        server.login(sender_email, sender_password)
        server.sendmail(sender_email, to_email, msg.as_string())
        server.quit()

        print(f"Email sent successfully to {to_email}")
    except Exception as e:
        print(f"Failed to send email to {to_email}: {e}")

def main():
    """
    Reads match data from a CSV file and sends personalized emails
    to participants with their match details.
    
    Expects a CSV file with columns: @name, @mail, @match, @matchmail.
    """
    sender_email = "your_email@gmail.com"  # Replace with actual email
    sender_password = "your_secure_app_password"  # Use environment variables or secure storage
    csv_file = "matches.csv"  # Path to the CSV file

    # Email subject
    subject = "You've Been Matched!"

    # Leaderboard (static text for top matches)
    leaderboard = """\n\nPS: The best matches leaderboard is:
    
1. A + N
2. TC + WS
3. AD + L
4. ADD + DM
5. AC + G
6. S + S
7. AG + SR
8. D + S
9. BP + NC
10. AB + H

And if we run the algorithm in reverse, the worst match is NS + S.
"""

    # Open the CSV file and read each row
    try:
        with open(csv_file, newline='') as file:
            reader = csv.DictReader(file)
            for row in reader:
                name = row['@name']
                email = row['@mail']
                match = row['@match']
                match_email = row['@matchmail']

                # Email body with personalized match details
                body = f"""
Hi {name},

Great news! Based on your responses, we’ve found a friendship match for you. Say hello to {match}!

📩 Email: {match_email}

I encourage you to reach out, introduce yourself, and give it a shot—regardless of any prior experiences with this person. You never know what a fresh start could bring!

Additionally, if you’d like, I can provide information about other people similar to you within the next 24 hours. This data will be completely wiped at 11:59 AM on February 9.

Just let me know if you’re interested.

I may also send a single follow-up in about three months to see how things are going. No pressure—just curious to hear how it worked out! Best of luck, and happy connecting!

Warm Regards,
Arjun Maneesh Agarwal
{leaderboard}
"""
                # Send the email
                send_email(email, subject, body, sender_email, sender_password)

    except FileNotFoundError:
        print(f"Error: CSV file '{csv_file}' not found.")
    except KeyError as e:
        print(f"Error: Missing expected column in CSV file: {e}")

if __name__ == "__main__":
    main()
