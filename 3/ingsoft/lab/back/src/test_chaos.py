from fastapi.testclient import TestClient
from test_main import test_app
from test_setup import p, start_game, unames, vote

client = TestClient(test_app)


def test_chaos():
    response_get_pregame1 = client.get(
        "/test-chaos/game_state",
        headers=p[0]
    )
    assert response_get_pregame1.status_code == 200
    # print(response_get_pregame1.json())

    response_start = start_game(p[0], "test-chaos")
    print(response_start.json())
    # assert response_start.status_code == 201

    round_count = 0
    game_is_not_over = True
    de_score = 0
    fo_score = 0
    avadas_avaliables = 2

    while game_is_not_over:
        round_count += 1
        response_get_ingame = client.get(
            "/test-chaos/game_state",
            headers=p[0]
        )
        assert response_get_ingame.status_code == 200

        rta: dict = response_get_ingame.json()
        #print(f"\nStart of round {round_count}")
        # print(rta)
        minister_uname: str = rta["minister"]
        minister_index = unames.index(minister_uname)
        director_index = (minister_index + 1) % 5
        director_uname: str = unames[director_index]
        alive_lads = rta["player_list"]

        respone_propose = client.put(
            "/test-chaos/director",
            json={"director_uname": director_uname},
            headers=p[minister_index]
        )
        assert respone_propose.status_code == 201

        de_score = rta["de_procs"]
        fo_score = rta["fo_procs"]
        if round_count == 2:
            for i in range(0, 5):
                if unames[i] in alive_lads:
                    vote(header=p[i], vote="Lumos", room_name="test-chaos")
            response_get_cards1 = client.get(
                "test-chaos/cards",
                headers=p[minister_index]
            )
            assert response_get_cards1.status_code == 200

            response_discard1 = client.put(
                "test-chaos/discard",
                json={"card_index": 0},
                headers=p[minister_index]
            )
            assert response_discard1.status_code == 201

            response_get_cards2 = client.get(
                "test-chaos/cards",
                headers=p[director_index]
            )
            assert response_get_cards2.status_code == 200

            response_discard2 = client.put(
                "test-chaos/discard",
                json={"card_index": 0},
                headers=p[director_index]
            )
            assert response_discard2.status_code == 201
        else:
            for i in range(0, 5):
                if unames[i] in alive_lads:
                    vote(header=p[i], vote="Nox", room_name="test-chaos")

        response_get_ingame2 = client.get(
            "/test-chaos/game_state",
            headers=p[0]
        )
        assert response_get_ingame2.status_code == 200
        #print("\nAfter the voting")
        # print(response_get_ingame2.json())

        response_post_proclamation = client.get(
            "/test-chaos/game_state",
            headers=p[0]
        )
        assert response_post_proclamation.status_code == 200
        scores_state: dict = response_post_proclamation.json()
        de_score = scores_state["de_procs"]
        fo_score = scores_state["fo_procs"]

        if de_score == 6:
            #print("Death eaters won")
            game_is_not_over = False
            break
        elif fo_score == 5:
            #print("Phoenix order won")
            break
        else:
            pass

        if de_score == 3:
            response_cast_divination = client.get(
                "/test-chaos/cast/divination",
                headers=p[minister_index]
            )
            assert response_cast_divination.status_code == 405

            response_confirm_divination = client.put(
                "/test-chaos/cast/confirm_divination",
                headers=p[minister_index]
            )
            assert response_confirm_divination.status_code == 405

        if (de_score == 4 or de_score == 5) and avadas_avaliables >= 1:
            victim_index = (minister_index - 1) % 5
            victim_uname = unames[victim_index]
            response_cast_avada = client.put(
                "/test-chaos/cast/avada-kedavra",
                headers=p[minister_index],
                json={"target_uname": victim_uname}
            )
            assert response_cast_avada.status_code == 405
