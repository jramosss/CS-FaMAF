from fastapi import APIRouter, HTTPException, status, Depends, Path
from fastapi_utils.tasks import repeat_every
from fastapi.responses import HTMLResponse
from datetime import datetime, timedelta
from typing import List

from api.models.base import db, save_game_on_database, load_from_database, remove_room_from_database
from api.models.room_models import RoomCreationRequest, ChatRequest
from api.handlers.authentication import valid_credentials, get_username_from_token
from api.handlers.game_checks import check_game_preconditions
from api.utils.room_utils import check_email_status

from classes.room import Room, RoomStatus
from classes.room_hub import RoomHub
from classes.role_enum import Role
from classes.game_status_enum import GamePhase
from classes.game import Vote, Game

router = APIRouter()
hub = RoomHub()


@router.on_event("startup")
def load_hub():
    prev_rooms = load_from_database()
    for room in prev_rooms:
        hub.add_room(room)


@router.on_event("startup")
@repeat_every(seconds=60, wait_first=True)
async def clean_hub_and_db():
    for room in hub.rooms:
        owner = room.get_owner()
        count = room.get_user_count()
        last_update_delta = datetime.now() - room.get_last_update()
        if ((owner is None and count <= 0)
                or (last_update_delta > timedelta(seconds=600))):
            hub.remove_room(room)
            await remove_room_from_database(room)


@router.post("/room/new", tags=["Room"], status_code=status.HTTP_201_CREATED)
async def create_room(
        room_info: RoomCreationRequest,
        email: str = Depends(valid_credentials),
        username: str = Depends(get_username_from_token)):

    """
    Endpoint for creating a new room.

    Possible respones:\n
            201 when succesfully created.
            401 when not logged in.
            403 when email not confirmed.
            409 when the room name is already in use.
            500 when there's an internal error in the database.
    """

    room_name = room_info.name
    max_players = room_info.max_players
    email_confirmed = await check_email_status(email)

    if not email_confirmed:
        raise HTTPException(status_code=403, detail="E-mail not confirmed")
    elif room_name in (hub.all_rooms()):
        raise HTTPException(status_code=409,
                            detail="Room name already in use")
    else:
        new_room = Room(room_name, max_players, username)
        await save_game_on_database(new_room)
        hub.add_room(new_room)
        return {"message": "Room created successfully"}


@ router.get("/room/join/{room_name}", tags=["Room"], status_code=status.HTTP_200_OK)
async def join_room(
        room_name: str = Path(
            ...,
            min_length=6,
            max_length=20,
            description="The name of the room you want to join",
        ),
        email: str = Depends(valid_credentials),
        username: str = Depends(get_username_from_token)):
    """
    Endpoint to join a room, it takes the room name as a parameter in the URL,
    and the access_token in the request headers.

    Possible respones:\n
            200 when succesfully joined.
            401 when not logged in.
            403 when email not confirmed or room is full.
            404 when the room doesn't exist.
            409 when the user is already in the room.
            403 when the room is full or in-game.
    """

    room = hub.get_room_by_name(room_name)
    if not await check_email_status(email):
        raise HTTPException(status_code=403,
                            detail="E-mail is not confirmed")
    elif not room:
        raise HTTPException(status_code=404, detail="Room not found")
    elif email in room.get_emails_list():
        return {"message": f"Joined {room_name}"}
    elif not room.is_open():
        raise HTTPException(status_code=403,
                            detail="Room is full or in-game")
    else:

        await room.user_join(username, email)
        await save_game_on_database(room)
        return {"message": f"Joined {room_name}"}


@ router.get("/room/leave/{room_name}", tags=["Room"], status_code=status.HTTP_200_OK)
async def leave_room(
        room_name: str = Path(
            ...,
            min_length=6,
            max_length=20,
            description="The name of the room you want to leave",
        ),
        email: str = Depends(valid_credentials),
        username: str = Depends(get_username_from_token)):
    """
    Endpoint to leave a room, it takes the room name as a parameter in the URL,
    and the access_token in the request headers.

    Possible respones:\n
            200 when succesfully left.
            401 when not logged in.
            403 when email not confirmed.
            404 when the room doesn't exist.
            409 when the user is not in the room.
            403 when the room is full or in-game.
    """

    room = hub.get_room_by_name(room_name)
    if not await check_email_status(email):
        raise HTTPException(status_code=403,
                            detail="E-mail is not confirmed")
    elif not room:
        raise HTTPException(status_code=404, detail="Room not found")
    elif username not in room.get_user_list():
        raise HTTPException(status_code=409,
                            detail="You're not in this room")
    elif room.status == RoomStatus.IN_GAME:
        raise HTTPException(status_code=403, detail="Room is in-game")
    else:
        await room.user_leave(username, email)
        return {"message": f"Left {room_name}"}


@ router.put("/{room_name}/start", tags=["Game"], status_code=status.HTTP_201_CREATED)
async def start_game(
        room_name: str = Path(
            ...,
            min_length=6,
            max_length=20,
        ),
        email: str = Depends(valid_credentials),
        username: str = Depends(get_username_from_token)):
    """
    Endpoint for starting the game, should only be used by the room owner.

    Will only work if there are 5 players or more.
    """
    room = hub.get_room_by_name(room_name)
    if (room.owner != username):
        raise HTTPException(
            status_code=403, detail="You're not the owner of the room")
    elif (len(room.get_user_list()) < 5):
        raise HTTPException(
            status_code=409, detail="Not enough players")
    elif (room.get_status() != RoomStatus.PREGAME):
        raise HTTPException(
            status_code=409, detail="Game on room has already started")
    else:
        room.start_game()
        await save_game_on_database(room)
        return {"message": "Succesfully started"}


@router.put("/{room_name}/chat", tags=["Room"], status_code=status.HTTP_201_CREATED)
async def send_message(body: ChatRequest,
                       room_name: str = Path(...,
                                             min_length=6,
                                             max_length=20,
                                             description="The name of the room you want to post a message to the chat"),
                       username: str = Depends(get_username_from_token)):

    room = hub.get_room_by_name(room_name)
    if room is None:
        raise HTTPException(detail="Room not found",
                            status_code=status.HTTP_404_NOT_FOUND)
    elif room.can_user_chat(username):
        room.post_message(username + ": " + body.msg)
        return {"message": "Message sent succesfully"}
    else:
        raise HTTPException(detail="You can't chat right now",
                            status_code=status.HTTP_405_METHOD_NOT_ALLOWED)
