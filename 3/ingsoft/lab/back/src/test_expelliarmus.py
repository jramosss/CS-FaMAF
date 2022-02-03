from fastapi.testclient import TestClient
from pony.orm import db_session, commit
from api.models.base import db
from test_main import test_app
from test_setup import p, start_game, unames, vote

client = TestClient(test_app)


def test_expelliarmus():
    response_get_pregame1 = client.get(
        "/test-expelliarmus/game_state",
        headers=p[0]
    )
    # print(response_get_pregame1.json())
    assert response_get_pregame1.status_code == 200

    response_start = start_game(p[0], "test-expelliarmus")
    # print(response_start.json())
    #assert response_start.status_code == 201

    voldemort_uname = ""
    for k in range(0, 10):
        response_get_game = client.get(
            "/test-expelliarmus/game_state",
            headers=p[k]
        )
        assert response_get_game.status_code == 200
        rta: dict = response_get_game.json()

        if rta['my_role'] == "Voldemort":
            voldemort_uname = unames[k]
        else:
            pass
    # print(f"Voldemort is: {voldemort_uname}")

    round_count = 0
    game_is_not_over = True
    de_score = 0
    fo_score = 0
    imperio_casted = False
    expelliarmus_cast = False
    crucio_availables = 2
    avadas_avaliables = 2

    while game_is_not_over:
        round_count += 1
        expelliarmus_cast = False
        response_get_ingame = client.get(
            "/test-expelliarmus/game_state",
            headers=p[0]
        )
        assert response_get_ingame.status_code == 200

        rta: dict = response_get_ingame.json()
        # print(f"\nStart of round {round_count}")
        # print(rta)
        minister_uname: str = rta["minister"]
        minister_index = unames.index(minister_uname)
        alive_lads = rta["player_list"]

        director_index = (minister_index + 1) % 10
        director_uname: str = unames[director_index]
        while (director_uname not in alive_lads):
            director_index = (director_index + 1) % 10
            director_uname: str = unames[director_index]

        response_propose = client.put(
            "/test-expelliarmus/director",
            json={"director_uname": director_uname},
            headers=p[minister_index]
        )
        if (response_propose.status_code != 201):
            print(response_propose.json())
        assert response_propose.status_code == 201

        for i in range(0, 10):
            if unames[i] in alive_lads:
                if (voldemort_uname != director_uname):
                    response = vote(
                        header=p[i], vote="Lumos", room_name="test-expelliarmus")
                    assert response.status_code == 200
                else:
                    response = vote(
                        header=p[i], vote="Nox", room_name="test-expelliarmus")
                    assert response.status_code == 200
        if (voldemort_uname == director_uname):
            continue

        response_get_ingame2 = client.get(
            "/test-expelliarmus/game_state",
            headers=p[0]
        )
        assert response_get_ingame2.status_code == 200

        if de_score > 2 and voldemort_uname == director_uname:
            print("Death eaters won, voldi runs hogwarts")
            game_is_not_over = False
            break

        # ---------------------------------
        if (round_count == 2):
            assert (client.put(
                "test-expelliarmus/discard",
                json={"card_index": 3},
                headers=p[director_index])
            ).status_code == 405
        # ---------------------------------

        response_get_cards1 = client.get(
            "test-expelliarmus/cards",
            headers=p[minister_index]
        )
        assert response_get_cards1.status_code == 200

        response_discard1 = client.put(
            "test-expelliarmus/discard",
            json={"card_index": 0},
            headers=p[minister_index]
        )
        assert response_discard1.status_code == 201

        response_get_cards2 = client.get(
            "test-expelliarmus/cards",
            headers=p[director_index]
        )
        assert response_get_cards2.status_code == 200

        # ---------------------- EXPELLIARMUS TESTING
        if (round_count == 2):
            assert (client.put(
                "test-expelliarmus/discard",
                json={"card_index": 3},
                headers=p[director_index])
            ).status_code == 403

        if (de_score == 5 and not expelliarmus_cast):
            response_expelliarmus_director = client.put(
                "test-expelliarmus/discard",
                json={"card_index": 3},
                headers=p[director_index]
            )
            # print(response_expelliarmus_director.json())
            assert response_expelliarmus_director.status_code == 201

            expelliarmus_cast = True

            if (round_count % 2):
                response_expelliarmus_minister = client.put(
                    f"/test-expelliarmus/expelliarmus",
                    headers=p[minister_index],
                    json={"vote": "Lumos"}
                )
                assert response_expelliarmus_minister.status_code == 200
                # print(response_expelliarmus_minister.json())
                continue  # With this it should come back to the while statement
            else:
                response_expelliarmus_minister = client.put(
                    f"/test-expelliarmus/expelliarmus",
                    headers=p[minister_index],
                    json={"vote": "Nox"}
                )
                # print(response_expelliarmus_minister.json())
                assert response_expelliarmus_minister.status_code == 200
        # ---------------EXPELLIARMUS TESTING END------------------

        response_discard2 = client.put(
            "test-expelliarmus/discard",
            json={"card_index": 0},
            headers=p[director_index]
        )
        assert response_discard2.status_code == 201

        response_post_proclamation = client.get(
            "/test-expelliarmus/game_state",
            headers=p[0]
        )
        assert response_post_proclamation.status_code == 200
        scores_state: dict = response_post_proclamation.json()
        de_score = scores_state["de_procs"]
        fo_score = scores_state["fo_procs"]

        if de_score == 6:
            print("Death eaters won")
            game_is_not_over = False
            break
        elif fo_score == 5:
            print("Phoenix order won")
            break
        else:
            pass
            # print(f"Death Eaters: {de_score} , Phoenix Order: {fo_score}")

        if de_score == 1 and crucio_availables > 1:
            response_cast_crucio = client.put(
                "/test-expelliarmus/cast/crucio",
                headers=p[minister_index],
                json={"target_uname": unames[(minister_index - 2) % 10]}
            )
            assert response_cast_crucio.status_code == 200
        elif de_score == 2 and crucio_availables > 0:
            response_cast_crucio = client.put(
                "/test-expelliarmus/cast/crucio",
                headers=p[minister_index],
                json={"target_uname": unames[(minister_index - 2) % 10]}
            )
            assert response_cast_crucio.status_code == 200
        else:
            pass

        if de_score == 1 and crucio_availables > 1:
            response_confirm_crucio = client.put(
                "/test-expelliarmus/cast/confirm-crucio",
                headers=p[minister_index]
            )
            assert response_confirm_crucio.status_code == 200
            crucio_availables -= 1
        elif de_score == 2 and crucio_availables > 0:
            response_confirm_crucio = client.put(
                "/test-expelliarmus/cast/confirm-crucio",
                headers=p[minister_index]
            )
            assert response_confirm_crucio.status_code == 200
            crucio_availables -= 1
        else:
            pass

        if de_score == 3 and not imperio_casted:
            response_cast_imperio_good = client.put(
                "/test-expelliarmus/cast/imperius",
                headers=p[minister_index],
                json={"target_uname": unames[(minister_index - 2) % 10]}
            )
            # print(response_cast_imperio_good.json())
            assert response_cast_imperio_good.status_code == 200

            imperio_casted = True

        if ((de_score == 4 and avadas_avaliables == 2) or
                (de_score == 5 and avadas_avaliables == 1)):
            victim_index = (minister_index - 1) % 10
            victim_uname = unames[victim_index]
            response_cast_avada = client.put(
                "/test-expelliarmus/cast/avada-kedavra",
                headers=p[minister_index],
                json={"target_uname": victim_uname}
            )
            # print(response_cast_avada.json())
            assert response_cast_avada.status_code == 200
            avadas_avaliables -= 1
            if victim_uname == voldemort_uname:
                print("Voldemort died, F")
                game_is_not_over = False

        # print("--------------------------------------------------")


# test_expelliarmus()
