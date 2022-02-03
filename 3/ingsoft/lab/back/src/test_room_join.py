"""
Always erase database before running this test
"""
from fastapi.testclient import TestClient
from test_main import test_app
from test_setup import login


client = TestClient(test_app)

head_good = login("player9@example.com")

creation = client.post(
    "/room/new",
    headers=head_good,
    json={"name": "1stroom", "max_players": "5"},
)
assert creation.status_code == 201


head_bad = login("unconfirmed@example.com")


# test no login
def test_no_login():
    response = client.get("/room/join/testroom")
    assert response.status_code == 401
    assert response.json() == {"detail": "Not authenticated"}


# test login no mail
def test_unconfirmed_mail():
    response = client.get(
        "/room/join/testroom",
        headers=head_bad
    )
    assert response.json() == {"detail": "E-mail is not confirmed"}
    assert response.status_code == 403


# test login and mail bad path
def test_bad_json():
    response1 = client.get(
        "/room/join/{foo}",
        headers=head_good
    )

    response2 = client.get(
        "/room/join/{unodostrescuatrocincoseis}",
        headers=head_good
    )

    assert response1.status_code == 422
    assert response2.status_code == 422


# test login and mail good path (test_happy path)
def test_test_happy_path():

    response = client.get(
        "/room/join/1stroom",
        headers=head_good
    )
    assert response.status_code == 200
    assert response.json() == {"message": "Joined 1stroom"}


# test user already in room
def test_user_already_in():
    response = client.get(
        "/room/join/1stroom",
        headers=head_good
    )
    assert response.status_code == 200
    assert response.json() == {"message": "Joined 1stroom"}
