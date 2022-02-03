# users.py
from fastapi import APIRouter, Depends, HTTPException
from pony.orm import db_session, commit, select, exists

from api.models.base import db  # , DB_User
from api.models.user_models import User, NewPassword, NewUsername
from api.utils.login import get_current_user
from api.handlers.param_check import *
from api.handlers.pass_handler import verify_password, get_password_hash


router = APIRouter()


@router.put("/users/change_password", status_code=200, tags=["Users"])
async def change_psw(new_password_request: NewPassword, user: User = Depends(get_current_user)):
    """
    Endpoint that allows a User to change his password
    It takes the old password and the new one, it checks that the
    new password satisfies the requirements.
    Returns:
    200 OK              message : You have changed your password succesfully,
    401 UNAUTHORIZED    detail : Could not validate credentials,
    401 UNAUTHORIZED    detail : Wrong old password,
    422 BAD ENTITY      detail : The new password doesen't satisfies the requirements,
    503 SERVICE UNAVAILABLE detail : Something went wrong on the database
    """

    if not verify_password(new_password_request.old_pwd, user['hashed_password']):
        raise HTTPException(status_code=401, detail="Wrong old password")
    email = user['email']
    try:
        new_hash = get_password_hash(new_password_request.new_pwd)
        with db_session:
            user = db.DB_User.get(email=email)
            user.set(hashed_password=new_hash)
            commit()
    except Exception as e:
        print(e)
        raise HTTPException(
            status_code=503, detail="Service unavailable, try again soon")
    return {"message": "You have changed your password succesfully"}


@router.put("/users/change_username", status_code=200, tags=["Users"])
async def change_username(new_username: NewUsername, user: User = Depends(get_current_user)):
    """
    Endpoint that allows a User to change his Username
    It takes a new username, it checks that the
    new username satisfies the requirements.
    Returns:
    200 OK              message : You have changed your username succesfully,
    401 UNAUTHORIZED    detail : Could not validate credentials,
    409 CONFLICT        detail : nickname not available
    422 BAD ENTITY      detail : The new username doesen't satisfies the requirements,
    503 SERVICE UNAVAILABLE detail : Something went wrong on the database
    """
    email = user['email']
    try:
        with db_session:
            uname = new_username.username
            exists = db.exists("select * from DB_user where username = $uname")
    except:
        raise HTTPException(
            status_code=503, detail="Service unavailable, try again soon")
    if exists:
        raise HTTPException(
            status_code=409, detail="Nickname already registered")
    else:
        with db_session:
            user = db.DB_User.get(email=email)
            user.set(username=new_username.username)
            commit()
    return {"message": "You have changed your nickname succesfully"}


@router.get("/users/me", status_code=200, tags=["Users"])
async def read_users(current_user: User = Depends(get_current_user)):
    '''
    API endpoint that serves for testing the token validation. Returns info about
    the user that logged in if validation went well
    '''
    return (current_user)
