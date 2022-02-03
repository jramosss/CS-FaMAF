from random import randint
from fastapi.testclient import TestClient
from test_main import test_app
from test_setup import p, unames, start_game, vote
from api.handlers.authentication import get_username_from_token


client = TestClient(test_app)


owner = p[0]

FO_WIN = "FO WIN"
DE_WIN = "DE WIN"
VOL_WIN = "VOL WIN"
TC: str = "test-crucio"
TC9: str = "test-crucio-9"


def get_game_state(header=owner, room_name=TC):
    response_get_game = client.get(
        "/" + room_name + "/game_state",
        headers=header
    )
    assert response_get_game.status_code == 200
    return response_get_game


def propose_director(proposed, minister_index, room_name=TC):
    response = client.put(
        "/" + room_name + "/director",
        json={"director_uname": proposed},
        headers=p[minister_index]
    )
    return response


def get_cards(index, room_name=TC):
    response = client.get(
        "/" + room_name + "/cards",
        headers=p[index]
    )
    return response


def random_vote(alive_lads, room_n=TC, n_of_players=7):
    response = None
    for i in range(0, n_of_players):
        if unames[i] in alive_lads:
            response = vote(
                header=p[i], vote="Lumos", room_name=room_n)
            assert response.status_code == 200
        else:
            response = vote(header=p[i], vote="Nox",
                            room_name=room_n)
            assert response.status_code == 409
    return response


def approve_formula(alive_lads, room_n=TC, n_of_players=7):
    response = None
    for i in range(0, n_of_players):
        if unames[i] in alive_lads:
            response = vote(
                header=p[i], vote="Lumos", room_name=room_n)
            assert response.status_code == 200
    return response


def discard(minister_index, card_index=0, room_name=TC):
    response = client.put(
        room_name + "/discard",
        json={"card_index": card_index},
        headers=p[minister_index]
    )
    return response


def cast_crucio(minister_index, victim_index, room_name=TC):
    response = client.put(
        "/" + room_name + "/cast/crucio",
        headers=p[minister_index],
        json={"target_uname": unames[victim_index]}
    )
    return response


def confirm_crucio(index, room_name=TC):
    response = client.put(
        room_name + "/cast/confirm-crucio",
        headers=p[index]
    )
    return response


def cast_divination(index, room_name=TC):
    response = client.get(
        "/" + room_name + "/cast/divination",
        headers=p[index]
    )
    return response


def check_win_conditions(de_score=0, fo_score=0, director="", voldemort="N"):
    if de_score == 5:
        return DE_WIN
    elif fo_score == 6:
        return FO_WIN
    elif voldemort == director:
        return VOL_WIN


def get_voldi(room_name=TC):
    for k in range(0, 7):
        response_get_game = get_game_state(p[k], room_name)
        assert response_get_game.status_code == 200
        rta: dict = response_get_game.json()

        if rta['my_role'] == "Voldemort":
            return unames[k]
    return None


def test_kys_5():
    first = True
    response_get_pregame1 = get_game_state()
    assert response_get_pregame1.status_code == 200

    response_start = start_game(owner, TC)
    print(response_start.json())
    #assert response_start.status_code == 201
    voldemort_uname = get_voldi()

    round_count = 0
    de_score = 0

    while de_score <= 2:
        round_count += 1
        # print("Round count " + str(round_count))
        response_get_ingame = get_game_state(room_name=TC)
        assert response_get_ingame.status_code == 200

        rta: dict = response_get_ingame.json()
        minister_uname: str = rta["minister"]
        minister_index = unames.index(minister_uname)
        director_index = (minister_index + 1) % 7
        director_uname: str = unames[director_index]
        alive_lads = rta["player_list"]
        de_procs = rta['de_procs']

        response_propose = propose_director(director_uname, minister_index)
        if (response_propose.status_code != 201):
            print(response_propose.json())
        assert response_propose.status_code == 201

        if de_procs == 1:
            approve_formula(alive_lads)
        else:
            random_vote(alive_lads)

        response_get_ingame2 = get_game_state()
        assert response_get_ingame2.status_code == 200
        # # print("\nAfter the voting")
        # # print(response_get_ingame2.json())

        if de_score > 2 and voldemort_uname == director_uname:
            # print("Death eaters won, voldi runs hogwarts")
            break

        response_get_cards1 = get_cards(minister_index)
        assert response_get_cards1.status_code == 200

        response_discard1 = discard(minister_index)
        assert response_discard1.status_code == 201

        response_get_cards2 = get_cards(director_index)
        assert response_get_cards2.status_code == 200

        response_discard2 = discard(director_index)
        assert response_discard2.status_code == 201

        response_post_proclamation = get_game_state()

        assert response_post_proclamation.status_code == 200
        scores_state: dict = response_post_proclamation.json()

        response_cast_crucio = cast_crucio(minister_index, minister_index)

        de_score = scores_state["de_procs"]

        if (de_score == 2 and first):
            # print(response_cast_crucio.status_code)
            assert response_cast_crucio.status_code == 406
            first = False

        else:
            # print(response_cast_crucio.status_code)
            assert (response_cast_crucio.status_code == 400 or
                    response_cast_crucio.status_code == 405)

        confirm = confirm_crucio(minister_index)


def test_happy_path_9():
    response_get_pregame1 = get_game_state(room_name=TC9)
    assert response_get_pregame1.status_code == 200

    response_start = start_game(owner, TC9)
    #assert response_start.status_code == 201
    voldemort_uname = get_voldi(TC9)

    round_count = 0
    de_score = 0
    crucios = 2

    while de_score <= 2:
        round_count += 1
        # print("-------------------------------------\n")
        # print("Round count " + str(round_count))
        response_get_ingame = get_game_state(room_name=TC9)
        assert response_get_ingame.status_code == 200

        rta: dict = response_get_ingame.json()
        minister_uname: str = rta["minister"]
        minister_index = unames.index(minister_uname)
        director_index = (minister_index + 1) % 9
        director_uname: str = unames[director_index]
        alive_lads = rta["player_list"]
        de_procs = rta['de_procs']

        respone_propose = propose_director(
            director_uname, minister_index, room_name=TC9)
        assert respone_propose.status_code == 201

        if de_procs == 0 or de_procs == 1:
            approve_formula(alive_lads, TC9, 9)
        else:
            random_vote(alive_lads, TC9, 9)

        response_get_ingame2 = get_game_state(room_name=TC9)
        assert response_get_ingame2.status_code == 200
        # # print("\nAfter the voting")
        # # print(response_get_ingame2.json())

        if de_score > 2 and voldemort_uname == director_uname:
            # print("Death eaters won, voldi runs hogwarts")
            break

        response_get_cards1 = get_cards(minister_index, room_name=TC9)
        assert response_get_cards1.status_code == 200

        response_discard1 = discard(minister_index, room_name=TC9)
        assert response_discard1.status_code == 201

        response_get_cards2 = get_cards(director_index, room_name=TC9)
        assert response_get_cards2.status_code == 200

        response_discard2 = discard(director_index, room_name=TC9)
        assert response_discard2.status_code == 201

        response_post_proclamation = get_game_state(room_name=TC9)

        assert response_post_proclamation.status_code == 200
        scores_state: dict = response_post_proclamation.json()
        # print(f"SCORES_STATE {str(scores_state)}")

        poor_guy = minister_index
        while poor_guy == minister_index:
            poor_guy = randint(0, 8)

        de_score = scores_state["de_procs"]

        if (de_score == 1 and crucios > 1):
            response_cast_crucio = cast_crucio(minister_index, poor_guy, TC9)
            # print(response_cast_crucio.status_code)
            # print("LOYALTY: " + str(response_cast_crucio.json()))
            crucios -= 1
            # EDGE CASE, user was already investigated
            assert (response_cast_crucio.status_code == 200 or
                    response_cast_crucio.status_code == 409)
        elif (de_score == 2 and crucios > 0):
            response_cast_crucio = cast_crucio(minister_index, poor_guy, TC9)
            # print(response_cast_crucio.status_code)
            # print("LOYALTY: " + str(response_cast_crucio.json()))
            crucios -= 1
            # EDGE CASE, user was already investigated
            assert (response_cast_crucio.status_code == 200 or
                    response_cast_crucio.status_code == 409)

        confirm = confirm_crucio(minister_index, TC9)


# print("TEST_KYS_5")
# test_kys_5()
# print("TEST_HAPPY_PATH_9")
# test_happy_path_9()
# hola
