from pony.orm import db_session, select
from api.models.user_models import User
from api.models.base import db  # ,DB_User


def check_username_in_database(u: User):
    with db_session:
        uname = u.username
        return db.exists("select * from DB_user where username = $uname")


def check_email_in_database(u: User):
    with db_session:
        mail = u.email
        return db.exists("select * from DB_user where email = $mail")
