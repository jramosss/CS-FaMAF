
from fastapi.testclient import TestClient
from test_main import test_app
from pony.orm import db_session, commit
from test_setup import login

client = TestClient(test_app)

head_bad = login("unconfirmed@example.com")
head_good = login("player9@example.com")


# test no login
def test_no_login():
    response = client.post(
        "/room/new",
        json={"name": "foobar", "max_players": "5"}
    )
    assert response.status_code == 401
    assert response.json() == {
        "detail": "Not authenticated"}


# test login no mail
def test_unconfirmed_mail():
    response = client.post(
        "/room/new",
        headers=head_bad,
        json={"name": "foobar", "max_players": "5"}
    )
    assert response.status_code == 403
    assert response.json() == {
        "detail": "E-mail not confirmed"}


# test login and mail bad arguments
def test_bad_json():
    response1 = client.post(
        "/room/new",
        headers=head_good,
        json={"name": "foobar"}
    )

    response2 = client.post(
        "/room/new",
        headers=head_good,
        json={"name": "foobar", "max_players": "4"}
    )

    response3 = client.post(
        "/room/new",
        headers=head_good,
        json={"nam": "foo", "max_players": "5"}
    )

    assert response1.status_code == 422
    assert response2.status_code == 422
    assert response3.status_code == 422


# test login and mail good arguments (htest_appy path)
def test_test_happy_path():
    response = client.post(
        "/room/new",
        headers=head_good,
        json={"name": "foobar", "max_players": "5"}
    )
    assert response.status_code == 201
    assert response.json() == {
        "message": "Room created successfully"}


# test login and mail in use nam
def test_used_name():
    response = client.post(
        "/room/new",
        headers=head_good,
        json={"name": "foobar", "max_players": "6"}
    )
    assert response.status_code == 409
    assert response.json() == {
        "detail": "Room name already in use"}
