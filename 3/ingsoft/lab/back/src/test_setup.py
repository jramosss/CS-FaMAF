# test_setup.py
from fastapi.testclient import TestClient
from pony.orm import db_session, commit
from api.models.base import db
from test_main import test_app as app
#from main import app

client = TestClient(app)


def register(email: str):
    response = client.post(
        "/users/register",
        json={
            "username": email.split('@')[0],
            "email": email,
            "password": "Heladera65",
            "icon": "string",
        })
    with db_session:
        try:
            user = db.DB_User.get(email=email)
            user.set(email_confirmed=True)
            commit()
        except Exception as e:
            print(e)
            pass
    return response


def login(email: str):
    response_login = client.post(
        "/users",
        data={
            "grant_type": "",
            "username": email,
            "password": "Heladera65",
            "scope": "",
            "client_id": "",
            "client_secret": "",
        },
    )
    assert response_login.status_code == 200
    rta: dict = response_login.json()
    token: str = rta["access_token"]
    token_type: str = "Bearer "
    head: str = token_type + token

    return {"accept": "application/json", "Authorization": head}


def create(header, room_name: str, nof_players: int):
    client.post(
        "/room/new",
        headers=header,
        json={"name": f"{room_name}", "max_players": f"{nof_players}"}
    )


def join(header, room_name: str):
    response = client.get(
        f"/room/join/{room_name}",
        headers=header,
    )
    return response


def start_game(owner, room_name: str):
    response = client.put(
        f"/{room_name}/start",
        headers=owner,
        json={"room_name": room_name}
    )
    return response


def vote(header: str, vote: str, room_name: str):
    response = client.put(
        f"/{room_name}/vote",
        headers=header,
        json={
            "vote": vote
        }
    )
    return response


response = client.post(
    "/users/register",
    json={
        "username": "unconfirmed",
        "email": "unconfirmed@example.com",
        "password": "Heladera65",
        "icon": "string",
    })


p = []
unames = []
for i in range(0, 12):
    unames.append(f"player{i}")
    register(f"player{i}@example.com")
    p.append(login(f"player{i}@example.com"))

assert register("player1@example.com").status_code == 409


create(p[0], "test-crucio", 7)
for i in range(0, 7):
    rta = join(p[i], "test-crucio")

create(p[0], "test-crucio-9", 9)
for i in range(0, 9):
    rta = join(p[i], "test-crucio-9")

create(p[0], "test-chaos", 5)
for i in range(0, 5):
    rta = join(p[i], "test-chaos")

create(p[0], "test-expelliarmus", 10)
for i in range(0, 10):
    rta = join(p[i], "test-expelliarmus")

create(p[0], "test-game-5", 5)
for i in range(0, 5):
    rta = join(p[i], "test-game-5")

create(p[0], "test-game-8", 8)
for i in range(0, 8):
    rta = join(p[i], "test-game-8")


create(p[0], "test-game-10", 10)
for i in range(0, 10):
    rta = join(p[i], "test-game-10")
