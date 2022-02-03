from fastapi import *
from api.routers.room_endpoints import hub
from classes.room import RoomStatus

router = APIRouter()


@router.get("/rooms", status_code=status.HTTP_200_OK, tags=["Hub"])
def get_rooms():
    room_names = hub.all_rooms()
    rooms = []
    ret_json = []

    for room_name in room_names:
        room = hub.get_room_by_name(room_name)
        if room.status == RoomStatus.PREGAME:
            rooms.append(room)

    for room in rooms:
        aux = {"name": room.name,
               "max_players": room.max_players,
               "active_users": room.get_user_count()}
        ret_json.append(aux)

    # return {"message" : ret_json.__str__()}
    return {"room_list": ret_json}
