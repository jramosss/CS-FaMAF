from typing import List, Dict
from enum import Enum, unique
from datetime import datetime

from classes.game import Game
from classes.game_status_enum import GamePhase


@unique
class RoomStatus(Enum):
    """Enum for Room class status"""

    PREGAME = "Pregame"
    IN_GAME = "In game"
    FINISHED = "Finished"


class Room:
    """
    This class represents the room, not the game itself.
    It should support the chat, joining, starting a game, etc

    Currently identifying users by their e-mail(str). Maybe not the most optimal approach.
    Using type decorators whenever possible.
    """

    # game: Game -> when game starts

    def __init__(self, name, max_players, owner):
        self.name: str = name
        self.max_players: int = max_players
        self.owner: str = owner
        self.users: List[str] = []
        self.emails: List[str] = []
        #self.sockets = {}
        self.status: RoomStatus = RoomStatus.PREGAME
        self.game: Game = None
        self.messages: List[str] = []

    async def user_join(self, user: str, email: str):
        """
        Add  'user' to the current users list
        user_connect shouldn't be called when is_open == false, but doublecheck anyway.

        Maybe it should return something (confirmation? error?)
        """
        if self.is_open():
            self.users.append(user)
            self.emails.append(email)

        if self.owner is None:
            self.owner = user

    def is_open(self):
        """
        Returns true when a user can join, false otherwise
        """
        return len(self.users) < self.max_players and self.status == RoomStatus.PREGAME

    async def user_leave(self, user: str, email: str):
        """
        Removes a user from the current users list.
        Then passes ownership or removes the room from the hub if necessary
        """
        self.users.remove(user)
        self.emails.remove(email)
        if self.users == []:
            # WIP
            # server.remove_room(self) -> remove myself from hub
            # del self -> destroy myself
            self.owner = None
        elif self.owner == user:
            self.owner = self.users[0]

    def get_name(self):
        """ Room name getter"""
        return self.name

    def get_user_count(self):
        """User count getter"""
        return len(self.users)

    def get_owner(self):
        return self.owner

    def get_user_list(self):
        """User list getter"""
        return self.users

    def get_emails_list(self):
        return self.emails

    def start_game(self):
        self.game = Game(self.users)
        self.status = RoomStatus.IN_GAME
        self.messages.clear()
        # Obviously is not finished.

    def get_game(self):
        return self.game

    def get_status(self):
        return self.status

    def get_max_players(self):
        return self.max_players

    def set_status(self, status: RoomStatus):
        self.status = status

    def update_status(self):
        game = self.get_game()
        phase = game.get_phase()
        if (phase in [GamePhase.FO_WON, GamePhase.DE_WON]):
            self.set_status(RoomStatus.FINISHED)

    def dump_game_json(self):
        game = self.get_game()
        if self.get_game() is not None:
            json = {"death_eaters": game.get_de_list(),
                    "voldemort": game.get_voldemort(),
                    "minister": game.get_minister_user(),
                    "director": game.get_director_user(),
                    "last_minister": game.get_last_minister_user(),
                    "last_director": game.get_last_minister_user(),
                    "de_procs": game.get_de_procs(),
                    "fo_procs": game.get_fo_procs(),
                    "player_list": game.get_alive_players(),
                    "deck_cards": game.get_deck(),
                    "game_cards": list(map(lambda c: c.value, game.get_cards())),
                    "spells": game.get_board_spells(),
                    "phase": game.get_phase().value,
                    "chaos": game.get_chaos()}
        else:
            json = {}
        return json

    def get_last_update(self):
        time = datetime.now()
        if self.game is not None:
            time = self.game.last_update

        return time

    def can_user_chat(self, username: str):
        in_room = username in self.users
        #socket_exists = username in self.sockets.keys()
        if self.get_game() is None:
            can_chat = True
        elif in_room:
            can_chat = self.get_game().player_can_speak(username)
        else:
            can_chat = False

        return (in_room and can_chat)

    def post_message(self, msg):
        self.messages.append(msg)
        if (len(self.messages) > 32):
            self.messages.pop(0)

    def get_messages(self):
        return self.messages
