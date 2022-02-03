from typing import List
from classes.room import Room


class RoomHub:

    """
    This class contains all the created Rooms (see Room.py), and provides functions
    related to listing and finding rooms.

    Maybe it shouldn't be a class as it contains minimal state, and some methods,
    also, it will only be instanciated once.
    """

    def __init__(self):
        self.rooms: List[Room] = []

    def add_room(self, room):
        """
        Adds a room to the hub, doesn't check for name availability
        """
        self.rooms.append(room)

    def remove_room(self, room):
        """Removes a room to the hub"""
        self.rooms.remove(room)

    def get_room_by_name(self, name):
        """
        Returns the first room with the corresponding name or None if it doesn't exist
        """
        room = next((r for r in self.rooms if r.get_name() == name), None)
        return room

    def all_rooms(self):
        """
        Returns a list of all the room names in use
        """
        names = map(lambda r: r.get_name(), self.rooms)
        return list(names)

    def open_rooms(self):
        """
        Returns a list of all the rooms that a user can join
        """
        names = filter(lambda r: r.is_open(), self.rooms)
        return list(names)

    def number_of_rooms(self):
        """
        Returns the number of rooms in the hub
        """
        return len(self.rooms)
