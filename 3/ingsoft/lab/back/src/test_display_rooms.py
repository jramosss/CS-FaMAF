from fastapi.testclient import TestClient
from test_main import test_app
from random import randint
from test_setup import join, create as create_room, start_game, p

client = TestClient(test_app)

#p = register_and_login()

owner = p[0]
p1 = p[1]
p2 = p[2]
p3 = p[3]
p4 = p[4]


def join_several_players(room, n):
    if n > len(p):
        return
    count = 0
    for player in p:
        if count == n:
            break
        join(player, room)
        count += 1


def create_several_rooms(n):
    for i in range(n):
        max_p = randint(5, 10)
        room_name = "Pytherin" + str(i)
        # TODO max players
        create_room(owner, room_name, 5)


def test_get_rooms_empty():
    response = client.get(
        "/rooms",
        headers=owner
    )
    print(response.json().__str__())
    assert response.status_code == 200
    #assert response.json() == {"rooms_list" : []}


N_ROOMS = 5


def test_get_rooms_plenty():
    create_several_rooms(N_ROOMS)
    response = client.get(
        "/rooms",
        headers=owner
    )
    print(response.json().__str__())
    assert response.status_code == 200
    assert response.json != {"rooms_list": []}


def test_get_rooms_non_empty():
    rooms = client.get(
        "/rooms",
        headers=owner
    )
    real_rooms = rooms.json().get("room_list")
    room_names = []
    for room in real_rooms:
        room_names.append(room.get("name"))

    print(room_names)

    for i in range(0, N_ROOMS):
        n_players = randint(1, 4)
        join_several_players(room_names[i], n_players)

    rooms = client.get(
        "/rooms",
        headers=owner
    )

    print(rooms.json().__str__())
    assert rooms.status_code == 200


def test_start_game():
    rooms = client.get(
        "/rooms",
        headers=owner
    )
    real_rooms = rooms.json().get("room_list")
    room_names = []
    for room in real_rooms:
        room_names.append(room.get("name"))

    start_game(owner, room_names[0])
    rooms = client.get(
        "/rooms",
        headers=owner
    )
    rooms_json = rooms.json()
    print("\n\nStarted first room: " + rooms_json.__str__())
    #assert rooms_json


test_get_rooms_empty()
test_get_rooms_plenty()
test_get_rooms_non_empty()
test_start_game()
