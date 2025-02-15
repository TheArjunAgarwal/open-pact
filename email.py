import smtplib
import csv
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

def send_email(to_email, subject, body, sender_email, sender_password):
    try:
        msg = MIMEMultipart()
        msg['From'] = sender_email
        msg['To'] = to_email
        msg['Subject'] = subject
        msg.attach(MIMEText(body, 'plain'))

        # Ensure persistent SSL connection
        server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
        server.login(sender_email, sender_password)
        server.sendmail(sender_email, to_email, msg.as_string())
        server.quit()

        print(f"Email sent to {to_email}")
    except Exception as e:
        print(f"Failed to send email to {to_email}: {e}")

def main():
    sender_email = "anulick0@gmail.com"  # Replace with your email
    sender_password = "zepvxetearfedrma"  # Replace with your email password
    csv_file = "matches.csv"  # Path to the CSV file

    subject = "You've Been Matched!"
    leaderboard = """\n\nPS: The best matches leaderboard is (I am only giving out initials...)

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

And if we run my algorithm in reverse, the worst match on this campus is NS + S."""

    with open(csv_file, newline='') as file:
        reader = csv.DictReader(file)
        for row in reader:
            name = row['@name']
            email = row['@mail']
            match = row['@match']
            match_email = row['@matchmail']

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
            send_email(email, subject, body, sender_email, sender_password)

if __name__ == "__main__":
    main()
