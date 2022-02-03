# base.py
from pony.orm import *
from datetime import date
from pydantic.networks import EmailStr
from classes.room import Room, RoomStatus
from classes.game import Game
db = Database()


def define_database_and_entities(**db_params):
    global db

    class DB_User(db.Entity):
        '''
        Entity for the database, the password is stored hashed, and
        the db uses the user email as PK
        '''
        username = Required(str)
        email = PrimaryKey(str)
        hashed_password = Required(str)
        email_confirmed = Required(bool)
        icon = Optional(str)
        creation_date = Required(date)

    class DB_Room(db.Entity):
        """
        Entity for the database, where we keep the status of the room
        and the game_state if begun.
        """
        name = PrimaryKey(str)
        max_players = Required(int)
        owner = Required(str)
        status = Required(str)
        users = Required(Json)
        emails = Required(Json)
        game = Required(Json)

    class Validation_Tuple (db.Entity):
        """
        Database table used in storing the validation codes
        corresponding to each email registered.
        """
        email = PrimaryKey(EmailStr)
        code = Required(str)

    db.bind(**db_params)
    db.generate_mapping(create_tables=True)

    pass


def load_from_database():
    db_rooms = []
    try:
        with db_session:
            db_rooms += db.DB_Room.select()
    except Exception as e:
        print(e)

    rooms = []
    for room in db_rooms:
        new_room = Room(room.name, room.max_players, room.owner)
        new_room.users = room.users["users"]
        new_room.emails = room.emails["emails"]
        if room.status == RoomStatus.PREGAME.value:
            new_room.set_status(RoomStatus.PREGAME)
        elif room.status == RoomStatus.IN_GAME.value:
            new_room.set_status(RoomStatus.IN_GAME)
        else:
            new_room.set_status(RoomStatus.FINISHED)

        if (new_room.get_status() != RoomStatus.PREGAME and room.game is not {}):
            new_room.game = Game(new_room.get_user_list())
            new_room.game.build_from_json(room.game)

        rooms.append(new_room)

    return rooms


async def save_game_on_database(room: Room):
    json_r = room.dump_game_json()
    room_name = room.get_name()
    try:
        with db_session:
            if (db.exists("select * from DB_Room where name = $room_name")):
                db_room = db.DB_Room.get(name=room_name)
                db_room.set(status=room.get_status().value)
                db_room.set(users={"users": room.get_user_list()})
                db_room.set(emails={"emails": room.get_emails_list()})
                db_room.set(game=json_r)
                db_room.set(owner=room.get_owner())
            else:
                db.DB_Room(
                    name=room.get_name(),
                    max_players=room.get_max_players(),
                    owner=room.get_owner(),
                    status=room.get_status().value,  # MEJORAR
                    users={"users": []},
                    emails={"emails": []},
                    game={}
                )
    except Exception as e:
        print(f"Something went wrong on the db, {e}")


async def remove_room_from_database(room: Room):
    try:
        room_name = room.get_name()
        with db_session:
            delete(r for r in db.DB_Room if r.name == room_name)
    except Exception as e:
        print(e)
