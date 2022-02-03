from api.models.base import db  # ,Validation_Tuple
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pony.orm import db_session, commit
import random
import string
import smtplib


class Validation:

    def __init__(self):
        self.__verification_code = self.__get_random_string(5)

    def __get_random_string(self, length):
        letters = string.ascii_uppercase
        result_str = ''.join(random.choice(letters) for i in range(length))
        print(result_str)
        return result_str

    @db_session
    def send_mail(self, to):
        '''
        from_u = 'pytherinfamaf@gmail.com'
        passw = 'Pytherin123'

        msg = MIMEMultipart()
        msg['Subject'] = "Secret Voldemort account verification"
        msg['From'] = from_u
        msg['To'] = to

        body = "Hi, click this link to validate your secret voldemor account " + \
            "http://127.0.0.1:8000/validate?email=" + to + "&code=" + \
               self.__verification_code

        msg.attach(MIMEText(body, 'plain'))

        text = msg.as_string()

        s = smtplib.SMTP('smtp.gmail.com', 587)
        s.starttls()
        s.login(from_u, passw)
        s.sendmail(from_u, to, text)
        s.close()
        '''
        db.Validation_Tuple(email=to, code=self.__verification_code)
        # For some reason i can not do this and get the same result
        commit()
