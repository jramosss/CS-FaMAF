# users.py
from fastapi import APIRouter, Depends, HTTPException, status, BackgroundTasks
from fastapi.security import OAuth2PasswordRequestForm
from fastapi.responses import HTMLResponse
from datetime import datetime, timedelta
from pony.orm import db_session, commit


from api.models.base import db  # , DB_User, Validation_Tuple
from api.models.user_models import User, Token
from api.utils.login import authenticate_user
from api.handlers.pass_handler import *
from api.handlers.authentication import *
from api.handlers.param_check import *
from api.handlers.emailvalidation import *
from classes.game import Vote


router = APIRouter()


@router.post("/users/register", tags=["Users"], status_code=201)
async def register(user: User, background_t: BackgroundTasks):
    """
    User register endpoint
    Params: User data->
      * username : str
      * email : EmailStr
      * password : str
    """
    if not check_username_in_database(user) and not check_email_in_database(user):
        with db_session:
            db.DB_User(
                username=user.username,
                email=user.email,
                hashed_password=get_password_hash(user.password),
                email_confirmed=False,
                icon=user.icon,
                creation_date=datetime.today().strftime("%Y-%m-%d"),
            )

        validator = Validation()
        background_t.add_task(validator.send_mail, user.email)

        return {
            "message": user.username
            + ", a verification email has"
            + " been sent to "
            + user.email
        }
    else:
        msg = ""
        if check_username_in_database(user):
            msg += "Username already registered "
            raise HTTPException(
                status_code=409, detail="Username already registered ")
        elif check_email_in_database(user):
            msg += "Email already registered"
            raise HTTPException(
                status_code=409, detail="Email aready registered")
        return {msg}


# This is a get bc we want the user to be able to use this endpoint from sent link
@router.get("/validate/", tags=["Users"], status_code=200)
async def validate_user(email: str, code: str):
    try:
        with db_session:

            data = db.get(
                "select email,code from Validation_Tuple where email=$email")

            if data[1] != code:
                raise HTTPException(
                    status_code=409, detail="Invalid validation code")

            user = db.DB_User.get(email=email)
            user.set(email_confirmed=True)
            commit()

        html = """
    <!DOCTYPE html>
    <html>
        <head>
            <title>Secret voldemort</title>
        </head>
        <body style="background-color:black; text-align: center;">
            <h1 style="color: goldenrod; padding-top: 60px; text-shadow:1px 1px 2px darkgoldenrod;" >Email Verified!</h1>
            <h5 style="color: goldenrod;text-shadow:1px 1px 2px darkgoldenrod;" >
                You can start playing now!
            </h5>
            <div>
                <img src="https://images-ext-2.discordapp.net/external/TKE5N1VRYV4jDNc2EFBou31abWc9yuAi3J5zP3gztAc/https/1000logos.net/wp-content/uploads/2018/08/Hogwarts-Logo.jpg"; style="width: 800px;height: 400px;">
                </img>
            </div>
        </body>
    </html>
    """
        return HTMLResponse(html)
    except:
        raise HTTPException(status_code=404, detail="Email not found")


@router.post("/users", tags=["Login"], response_model=Token, status_code=200)
async def login(form_data: OAuth2PasswordRequestForm = Depends()):
    """
    LogIn endpoint, first, authenticates the user checking that the
    email and the password submitted by the user are correct.
    Then it creates a valid token for the user.
    """
    user = authenticate_user(form_data.username, form_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"email": user["email"], "username": user["username"]},
        expires_delta=access_token_expires,
    )
    return {"access_token": access_token, "token_type": "bearer"}


@router.put("/users/refresh", tags=["Login"], response_model=Token, status_code=201)
async def refresh_token(email: str = Depends(valid_credentials)):
    """
    Endpoint that creates a new web token.
    As the funciton "updates" creating a new token, it has the PUT method.
    Need to be logged in to use.
    """
    if not email:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect password",
            headers={"WWW-Authenticate": "Bearer"},
        )

    try:
        with db_session:
            username: str = db.get(
                "select username from DB_User where email=$email")

        access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
        access_token = create_access_token(
            data={"email": email, "username": username},
            expires_delta=access_token_expires,
        )
        return {"access_token": access_token, "token_type": "bearer"}
    except:
        raise HTTPException(
            status_code=405,
            detail="Something went wrong"
        )


@router.post("/token", response_model=Token, status_code=200)
async def login(form_data: OAuth2PasswordRequestForm = Depends()):
    user = authenticate_user(form_data.username, form_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(
        data={"email": user["email"], "username": user["username"]},
        expires_delta=access_token_expires,
    )
    return {"access_token": access_token, "token_type": "bearer"}
