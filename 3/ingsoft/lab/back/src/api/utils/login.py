# login.py
from fastapi import Depends, HTTPException, status
from pony.orm import db_session, select, commit

from api.models.base import db  # , DB_User
from api.handlers.authentication import valid_credentials
from api.handlers.pass_handler import verify_password


@db_session
def authenticate_user(mail: str, password: str):
    """
    Function that autenthicates the user by checking his password
    """
    keys = ('username', 'email', 'hashed_password',
            'email_confirmed', 'icon', 'creation_date')
    try:
        user_tuple = db.get("select * from DB_User where email = $mail")
    except:
        raise HTTPException(
            status_code=400, detail="Incorrect mail address")
    user = dict(zip(keys, user_tuple))
    if not verify_password(password, user['hashed_password']):
        return False
    return user


async def get_current_user(email: str = Depends(valid_credentials)):
    """
    Function that return a dict with all the users data from the database
    """
    if email is None:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED,
                            detail="Could not validate credentials",
                            headers={"WWW-Authenticate": "Bearer"})
    keys = ('username', 'email', 'hashed_password',
            'email_confirmed', 'icon', 'creation_date')
    with db_session:
        try:
            user_tuple = db.get(
                "select * from DB_User where email = $email")
        except:
            raise HTTPException(
                status_code=400, detail="Incorrect email or password")
        user = dict(zip(keys, user_tuple))
    return user
