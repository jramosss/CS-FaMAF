# login_change_username.py
from fastapi.testclient import TestClient
from test_main import test_app
from test_setup import p


client = TestClient(test_app)


def test_change_nickname0():  # htest_appy path
    response = client.put(
        "/users/change_username",
        headers=p[0],
        json={"username": "Malfoy"}
    )
    assert response.status_code == 200


def test_see_changes():
    response = client.get(
        "/users/me",
        headers=p[0],
    )
    assert response.status_code == 200
    rta: dict = response.json()
    new_username: str = rta["username"]
    assert new_username == "Malfoy"


def test_unique_nickname1():  # htest_appy path
    response = client.put(
        "/users/change_username",
        headers=p[1],
        json={"username": "Malfoy"}
    )
    assert response.status_code == 409


def test_back_nickname0():  # htest_appy path
    response = client.put(
        "/users/change_username",
        headers=p[0],
        json={"username": "player0"}
    )
    assert response.status_code == 200


def test_back_nickname1():  # htest_appy path
    response = client.put(
        "/users/change_username",
        headers=p[1],
        json={"username": "player1"}
    )
    assert response.status_code == 409


def test_invalid_new_uname1():
    response = client.put(
        "/users/change_username",
        headers=p[2],
        json={"username": "as"}
    )
    assert response.status_code == 422


def test_invalid_new_uname2():
    response = client.put(
        "/users/change_username",
        headers=p[2],
        json={"username": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
    )
    assert response.status_code == 422
