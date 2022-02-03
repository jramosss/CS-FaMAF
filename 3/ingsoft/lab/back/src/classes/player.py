from classes.role_enum import Role
from classes.loyalty_enum import Loyalty


class Player:
    def __init__(self, user: str):
        self.user: str = user
        self.role: Role = Role.TBD
        self.loyalty: Loyalty = Loyalty.TBD
        self.is_alive: bool = True
        self.investigated : bool = False

    def get_user(self):
        return self.user

    def get_role(self):
        return self.role

    def get_loyalty(self):
        return self.loyalty

    def kill(self):
        self.is_alive = False

    def make_voldemort(self):
        self.role = Role.VOLDEMORT

    def is_player_alive(self):
        return self.is_alive

    def is_voldemort(self):
        return self.role == Role.VOLDEMORT

    def set_role(self, role: Role):
        self.role = role

    def set_loyalty(self, loyalty: Loyalty):
        self.loyalty = loyalty