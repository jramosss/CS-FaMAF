import sys
from random import randint, choice
from time import sleep
from classes.game_status_enum import GamePhase
import requests


def login(email):
    response_login = requests.post(
        "http://127.0.0.1:8000/users",
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


messages: list = [
    "Dobby is free!",
    "I solemnly swear I am up to no good",
    "Don’t let the muggles get you down",
    "Training for the ballet, Potter?",
    "There's Always A Bigger Fish",
    "Anyone can speak Troll. All you have to do is point and grunt",
    "I mean, it's sort of exciting, isn't it, breaking the rules?",
    "I am a wizard, not a baboon brandishing a stick.",
    "It is the quality of one’s convictions that determines success, not the number of followers"
]

room_name = sys.argv[1]
index_in = sys.argv[2]
nof_players = int(sys.argv[3])
print(f"Room Name: {room_name} ")
print(f"Player index: {index_in}")
print(f"Number of player: {nof_players}")


index = int(index_in)
print("prelogin")
header = login(f"player{index}@example.com")  # players[index]
print("postlogin")
nick = (f"player{index}")

url = "http://127.0.0.1:8000"

print("prejoin")
join = requests.get(f"{url}/room/join/{room_name}", headers=header)
print("post join")
chat = requests.put(f"{url}/{room_name}/chat", json={"msg": "Hello There!"},
                    headers=header)
game_not_begun = True
while game_not_begun:
    sleep(3)
    r = requests.get(f"{url}/{room_name}/game_state", headers=header)
    state = r.json()
    if (state['room_status'] == "In game"):
        prob = randint(1, 4)
        sleep(prob)
        chat = requests.put(f"{url}/{room_name}/chat",
                            json={"msg": choice(messages)},
                            headers=header)
        game_not_begun = False
        break

while True:

    sleep(3)
    r = requests.get(f"{url}/{room_name}/game_state", headers=header)
    state = r.json()
    phase = state["phase"]
    minister = state["minister"]
    director = state["director"]
    my_role = state["my_role"]
    de_score = state["de_procs"]

    prob = randint(1, 4)
    if prob == 1:
        vote = 'Nox'
    else:
        vote = 'Lumos'

    print(f"\nMy name is: {nick}, minister : {minister}, phase: {phase}")

    if (str(phase) == str(GamePhase.PROPOSE_DIRECTOR.value) and nick == minister):
        requests.put(f"{url}/{room_name}/director",
                     json={
                         "director_uname": f"player{randint(0, nof_players)}"},
                     headers=header)

    elif (str(phase) == str(GamePhase.VOTE_DIRECTOR.value)):
        requests.put(f"{url}/{room_name}/vote",
                     json={"vote": vote},
                     headers=header)

    elif (str(phase) == str(GamePhase.MINISTER_DISCARD.value) and nick == minister):
        requests.put(f"{url}/{room_name}/discard",
                     json={"card_index": "1"},
                     headers=header)

    elif (str(phase) == str(GamePhase.DIRECTOR_DISCARD.value) and nick == director):
        if de_score == 5:
            requests.put(f"{url}/{room_name}/discard",
                         json={"card_index": "3"},
                         headers=header)
        else:
            requests.put(f"{url}/{room_name}/discard",
                         json={"card_index": "1"},
                         headers=header)

    elif (str(phase) == str(GamePhase.REJECTED_EXPELLIARMUS.value) and nick == director):
        requests.put(f"{url}/{room_name}/discard",
                     json={"card_index": "1"},
                     headers=header)

    elif (str(phase) == str(GamePhase.CAST_CRUCIO.value) and nick == minister):
        requests.put(f"{url}/{room_name}/cast/crucio",
                     json={
                         "target_uname": f"player{randint(0, nof_players)}"},
                     headers=header)
        requests.put(f"{url}/{room_name}/cast/confirm-crucio",
                     headers=header)

    elif (str(phase) == str(GamePhase.CAST_DIVINATION.value) and nick == minister):
        requests.get(f"{url}/{room_name}/cast/divination",
                     headers=header)
        requests.put(f"{url}/{room_name}/cast/confirm_divination",
                     headers=header)

    elif (str(phase) == str(GamePhase.CAST_IMPERIUS.value) and nick == minister):
        requests.put(f"{url}/{room_name}/cast/imperius",
                     json={
                         "target_uname": f"player{randint(0, nof_players)}"},
                     headers=header)

    elif (str(phase) == str(GamePhase.CAST_AVADA_KEDAVRA.value) and nick == minister):
        victim = f"player{randint(1, nof_players)}"
        requests.put(f"{url}/{room_name}/chat",
                     json={
                         "msg": f"You’re a fool {victim}, and you will lose everything"},
                     headers=header)
        requests.put(f"{url}/{room_name}/cast/avada-kedavra",
                     json={"target_uname": victim},
                     headers=header)

    elif (str(phase) == str(GamePhase.CONFIRM_EXPELLIARMUS.value) and nick == minister):
        requests.put(f"{url}/{room_name}/expelliarmus",
                     json={"vote": "Lumos"},
                     headers=header)

    elif (str(phase) == str(GamePhase.DE_WON.value) or str(phase) == str(GamePhase.FO_WON.value)):
        print("Game has ended")
        break

    else:
        sleep(3)
        pass
