from enum import Enum, unique, auto


@unique
class Loyalty(Enum):
    """Enum for Player loyaties"""

    DEATH_EATER = 'Death eater'
    FENIX_ORDER = 'Member of the Fenix Order'
    TBD = auto()

    def __str__(self):
        return self.name
