from fastapi import APIRouter, HTTPException, status, Depends, Path

from api.models.room_models import VoteRequest, ProposeDirectorRequest, DiscardRequest
from api.handlers.authentication import valid_credentials, get_username_from_token
from api.handlers.game_checks import check_game_preconditions
from api.utils.room_utils import check_email_status, votes_to_json
from api.routers.room_endpoints import hub

from classes.room import Room, RoomStatus
from classes.room_hub import RoomHub
from classes.role_enum import Role
from classes.game_status_enum import GamePhase
from classes.game import Vote, Game
from api.models.base import save_game_on_database

router = APIRouter()


@ router.get("/{room_name}/game_state", tags=["Game"], status_code=status.HTTP_200_OK)
async def get_game_state(
        room_name: str = Path(
            ...,
            min_length=6,
            max_length=20,
            description="The room wich you want to get the game state of",
        ),
        username: str = Depends(get_username_from_token)):

    """
    Endpoint for getting the state of the game at every moment

    Will return the list of users in the room, and the room owner if the game
    hasn't started yet.

    A json containing the following game info if is has already started:
            "my_role": the role of the player who requested the game sate,
            "death_eaters": if my role is DE, this contains a list of the other deatheaters,
            "voldemort": if you are DE, who is voldemort,
            "minister": this turn's minister,
            "director": the proposed director,
            "last_minister": the previous minister,
            "last_director": the previous director,
            "de_procs": DeathEater proclamations,
            "fo_procs": FenixOrder proclamations,
            "phase": the phase the game is in propose director, vote,
                     minister discard ,director discard, (1,2,3,4) respectively,
            "player_list": the players in the game,
            "votes": the player votes for this turn,
            "chaos": the chaos counter,
            "messages" : The chat.

    Or it will return the winner if the game is over
    """
    room = hub.get_room_by_name(room_name)
    if room is None:
        raise HTTPException(status_code=404, detail="Room not found")
    elif not username in room.get_user_list():
        raise HTTPException(status_code=403,
                            detail="You're not in this room")
    elif room.status == RoomStatus.PREGAME:
        return {"room_status": room.status, "users": room.users, "owner": room.owner, "messages": room.get_messages()}
    else:
        room.update_status()
        game = room.get_game()
        my_role = game.get_player_role(username)

        if ((game.get_nof_players() < 7 and
             (my_role == Role.DEATH_EATER or my_role == Role.VOLDEMORT))
                or room.status == RoomStatus.FINISHED):
            de_list = game.get_de_list()
            voldemort = game.get_voldemort()
        elif ((game.get_nof_players() <= 10 and
               (my_role == Role.DEATH_EATER)) or room.status == RoomStatus.FINISHED):
            de_list = game.get_de_list()
            voldemort = game.get_voldemort()
        else:
            de_list = []
            voldemort = ""

        json_r = {
            "room_status": room.status,
            "my_role": my_role,
            "death_eaters": de_list,
            "voldemort": voldemort,
            "n_of_players": room.get_user_count(),
            "minister": game.get_minister_user(),
            "director": game.get_director_user(),
            "last_minister": game.get_last_minister_user(),
            "last_director": game.get_last_director_user(),
            "de_procs": game.get_de_procs(),
            "fo_procs": game.get_fo_procs(),
            "phase": game.get_phase(),
            "player_list": game.get_alive_players(),
            "votes": votes_to_json(game.get_votes()),
            "chaos": game.get_chaos(),
            "messages": room.get_messages()
        }
        return json_r


@ router.put("/{room_name}/director", tags=["Game"], status_code=status.HTTP_201_CREATED)
async def propose_director(body: ProposeDirectorRequest,
                           room_name: str = Path(
                               ...,
                               min_length=6,
                               max_length=20,
                           ),
                           username: str = Depends(get_username_from_token)):

    """
    Endpoint used by the minister to propose the director
    """
    room = check_game_preconditions(username, room_name, hub)
    game = room.get_game()
    phase = game.get_phase()
    minister = game.get_minister_user()
    if (phase == GamePhase.PROPOSE_DIRECTOR and minister == username):
        if body.director_uname not in game.get_alive_players():
            raise HTTPException(
                status_code=404, detail="Player dead or not found")
        elif (body.director_uname == game.get_last_director_user()
              or minister == body.director_uname):
            raise HTTPException(
                status_code=403, detail="That player cannot be the director this round")
        else:
            game.set_director(body.director_uname)
            game.set_phase(GamePhase.VOTE_DIRECTOR)
            await save_game_on_database(room)
            return {"message": "Director proposed successfully"}

    else:
        raise HTTPException(
            detail="You're not allowed to do this", status_code=405)


@ router.put("/{room_name}/vote", tags=["Game"], status_code=status.HTTP_200_OK)
async def vote(
        vote_req: VoteRequest,
        username: str = Depends(get_username_from_token),
        room_name: str = Path(
            ...,
            min_length=6,
            max_length=20,
        )):
    """
    This endpoint registers a vote and who`s voting.
    Throws 409 if the game if you already voted
    Throws 405 if the game is not in phase of voting
    Throws 404 if the game cannot be found in Hub
    Throws 400 if the vote is not Lumos or Nox
    """

    room = check_game_preconditions(username, room_name, hub)
    game = room.get_game()

    if game.phase == GamePhase.VOTE_DIRECTOR:

        if vote_req.vote not in [Vote.LUMOS.value, Vote.NOX.value]:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid vote")
        elif username in game.get_votes(True).keys():
            raise HTTPException(status_code=status.HTTP_409_CONFLICT,
                                detail="You already voted")
        else:
            game.register_vote(vote_req.vote, username)

    else:
        raise HTTPException(
            status_code=status.HTTP_405_METHOD_NOT_ALLOWED, detail="Game is not in voting phase")

    if len(game.get_alive_players()) == len(game.votes):
        await game.compute_votes()

    return {"message": "Succesfully voted!"}


@ router.get("/{room_name}/cards", tags=["Game"], status_code=status.HTTP_200_OK)
async def get_cards(
    room_name: str = Path(
        ...,
        min_length=6,
        max_length=20,
    ),
        username: str = Depends(get_username_from_token)):

    """
    Endpoint for getting the cards, used both by minister and director, but at their respective times.
    Will return a list similar to this:

    ['Order of the Fenix proclamation',
        'Order of the Fenix proclamation', 'Death Eater proclamation']
    """
    room = check_game_preconditions(username, room_name, hub)

    game = room.get_game()
    phase = game.get_phase()
    minister = game.get_minister_user()
    director = game.get_director_user()
    if ((phase == GamePhase.MINISTER_DISCARD and minister == username) or
            (phase == GamePhase.DIRECTOR_DISCARD and director == username) or
            (phase == GamePhase.REJECTED_EXPELLIARMUS and director == username)):
        return {"cards": game.get_cards()}
    else:
        raise HTTPException(
            detail="You're not allowed to do this", status_code=405)


@ router.put("/{room_name}/discard", tags=["Game"], status_code=status.HTTP_201_CREATED)
async def discard(body: DiscardRequest,
                  room_name: str = Path(
                      ...,
                      min_length=6,
                      max_length=20,
                  ),
                  username: str = Depends(get_username_from_token)):

    """
    Endpoint used for discarding a card, both by minister and director.

    Expects a body containing 1 field ("card_index") that represents the index of the card
    you want to discard (as returned by the /cards endpoint)
    """

    room = check_game_preconditions(username, room_name, hub)

    game = room.get_game()
    phase = game.get_phase()
    if (phase == GamePhase.MINISTER_DISCARD and game.get_minister_user() == username):
        if (body.card_index not in [0, 1, 2]):
            raise HTTPException(
                detail="Index out of bounds", status_code=400)

        game.discard(body.card_index)
        game.set_phase(GamePhase.DIRECTOR_DISCARD)
        return {"message": "Successfully discarded"}

    elif (phase == GamePhase.DIRECTOR_DISCARD and
          game.get_director_user() == username):
        if (body.card_index not in [0, 1, 3]):
            raise HTTPException(
                detail="Index out of bounds", status_code=400)
        elif (body.card_index == 3):
            if (game.get_de_procs() >= 5):
                if (not game.is_expelliarmus_casted()):
                    game.cast_expelliarmus()
                    game.set_phase(GamePhase.CONFIRM_EXPELLIARMUS)
                    return {"message": "Successfully casted Expelliarmus!"}
                else:
                    raise HTTPException(
                        detail="You cant cast expelliarmus twice in the round",
                        status_code=status.HTTP_403_FORBIDDEN)
            else:
                raise HTTPException(
                    detail="You can't cast Expelliarmus! yet", status_code=403)
        else:
            game.discard(body.card_index)
            game.proc_leftover_card()
            return {"message": "Successfully discarded"}

    elif (phase == GamePhase.REJECTED_EXPELLIARMUS and
          game.get_director_user() == username):
        if (body.card_index not in [0, 1]):
            raise HTTPException(
                detail="Index out of bounds", status_code=400)
        else:
            game.discard(body.card_index)
            game.proc_leftover_card()
            return {"message": "Successfully discarded"}
    else:
        raise HTTPException(
            detail="You're not allowed to do this", status_code=405)
