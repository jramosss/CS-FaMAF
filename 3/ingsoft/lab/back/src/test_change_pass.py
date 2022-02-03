# login_change_password.py
from fastapi.testclient import TestClient
from test_main import test_app
from test_setup import p

client = TestClient(test_app)


def test_change_password1():  # htest_appy path
    response = client.put(
        "/users/change_password",
        headers=p[0],
        json={"old_pwd": "Heladera65",
              "new_pwd": "Refrigerador65"}
    )
    assert response.status_code == 200


def test_relogin_with_new_pass():
    response = client.post(
        "/users",
        data={
            "grant_type": "",
            "username": "player0@example.com",
            "password": "Refrigerador65",
            "scope": "",
            "client_id": "",
            "client_secret": "",
        },
    )
    assert response.status_code == 200


def test_change_password2():  # htest_appy path
    response = client.put(
        "/users/change_password",
        headers=p[0],
        json={"old_pwd": "Refrigerador65",
              "new_pwd": "Heladera65"}
    )
    assert response.status_code == 200


def test_wrong_psw():
    response = client.put(
        "/users/change_password",
        headers=p[1],
        json={"old_pwd": "ASDaSD77",
              "new_pwd": "Refrigerador65"}
    )
    assert response.status_code == 401


def test_invalid_new_psw1():
    response = client.put(
        "/users/change_password",
        headers=p[2],
        json={"old_pwd": "Heladera65",
              "new_pwd": "heladera"}
    )
    assert response.status_code == 422


def test_invalid_new_psw2():
    response = client.put(
        "/users/change_password",
        headers=p[2],
        json={"old_pwd": "Heladera65",
              "new_pwd": "Helado"}
    )
    assert response.status_code == 422


def test_invalid_new_psw3():
    response = client.put(
        "/users/change_password",
        headers=p[2],
        json={"old_pwd": "Heladera65",
              "new_pwd": "7aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
    )
    assert response.status_code == 422
