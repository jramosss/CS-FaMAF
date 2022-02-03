from fastapi.testclient import TestClient
from test_main import test_app

client = TestClient(test_app)


# def test_register_valid_user():
#    response = client.post("users/register",
#                           json={
#                               "username": "Chuls",
#                               "email": "thejipi999@gmail.com",
#                               "password": "Chacarita1",
#                               "icon": "string"
#                           })
#    assert response.status_code == 201


# def test_register_already_registered_username():
#    response = client.post("users/register",
#                           json={
#                               "username": "Chuls",
#                               "email": "jramosp@gmail.com",
#                               "password": "Chacarita1",
#                               "icon": "string"
#                           })
#    assert response.status_code == 409


# def test_register_already_registered_email():
#    response = client.post("users/register",
#                           json={
#                               "username": "Shulsss",
#                               "email": "thejipi999@gmail.com",
#                               "password": "Chacarita1",
#                               "icon": "string"
#                           })
#    assert response.status_code == 409


def test_post_invalid_username_user():
    response = client.post("users/register",
                           json={
                               "username": "ch",
                               "email": "thejipi999@gmail.com",
                               "password": "Chacarita1",
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_invalid_username_user2():
    response = client.post("users/register",
                           json={
                               "username": "chajsdhjashdjahsjdkhasjkfhajkshdkajshdkjahsdkhakjshdjashdkjashjhkjaskjhdjkash",
                               "email": "thejipi999@gmail.com",
                               "password": "Chacarita1",
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_invalid_email_user():
    response = client.post("users/register",
                           json={
                               "username": "Faraon Love Shady",
                               "email": "mailquenoesmailgmail.com",
                               "password": "Chacarita1",
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_invalid_password_user():
    response = client.post("users/register",
                           json={
                               "username": "Daniel Penazzi",
                               "email": "dpenazzi@gmail.com",
                               "password": "covid19",  # No capital letters
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_invalid_password_user2():
    response = client.post("users/register",
                           json={
                               "username": "Kobe Bryant",
                               "email": "ripkobe@gmail.com",
                               "password": "Kooooobee",  # No number
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_invalid_password_user3():
    response = client.post("users/register",
                           json={
                               "username": "Selena Gomez",  # Bc obviously selena gomez is gonna play our game
                               "email": "selena@gmail.com",
                               "password": "selena",  # length < 8
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_user_no_username():
    response = client.post("users/register",
                           json={
                               "username": "",
                               "email": "selena@gmail.com",
                               "password": "SelenaG10",
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_user_no_email():
    response = client.post("users/register",
                           json={
                               "username": "Selena Gomez",  # Bc obviously selena gomez is gonna play our game
                               "email": "@gmail.com",
                               "password": "SelenaG10",  # length < 8
                               "icon": "string"
                           })
    assert response.status_code == 422


def test_post_user_no_password():
    response = client.post("users/register",
                           json={
                               "username": "Selena Gomez",  # Bc obviously selena gomez is gonna play our game
                               "email": "selena@gmail.com",
                               "password": "",  # length < 8
                               "icon": "string"
                           })
    assert response.status_code == 422
