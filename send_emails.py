import smtplib
import csv
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
import dotenv
from dotenv import load_dotenv
import os

load_dotenv()

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
    sender_email = os.environ['email']
    sender_password = os.environ['password']
    csv_file = "/Users/deepthought/Desktop/Marraige Pact/open-pact/preMatches.csv"  # Path to the CSV file

    # Email subject
    subject = " Quick Followup on Your Friendship Finder Match"

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

A few months ago, I matched you with {match}({match_email}) as part of our friendship matching experiment. I hope you had a chance to connect and maybe even found a meaningful interaction!

As promised, I’m reaching out with a quick followup. I’d love to hear how things went:

- Did you end up reaching out to {match}? Did you two know each other before hand? Did you have a conversation or meet up?

- What was your overall expirence with this person? Do you feel they are a good friend, say on a scale of 1-10.

- How did it go overall?

- Any suggestions for how we could improve the matching process?

Your feedback, positive or constructive, would mean a lot and will help shape future experiments like this. Just hit "Reply" and share a few thoughts if you’re up for it. No pressure at all.

Thanks so much for being part of this!

Warm regards,
Arjun Maneesh Agarwal

PS: A few people recived two mails from me as I hit the run command by chance. I am really sorry.
"""
                # Send the email
                send_email(email, subject, body, sender_email, sender_password)

    except FileNotFoundError:
        print(f"Error: CSV file '{csv_file}' not found.")
    except KeyError as e:
        print(f"Error: Missing expected column in CSV file: {e}")

if __name__ == "__main__":
    main()
