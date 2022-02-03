from enum import Enum, unique, auto


@unique
class Role(Enum):
    """Enum for Player roles"""

    DEATH_EATER = 'Death eater'
    FENIX_ORDER = 'Member of the Fenix Order'
    VOLDEMORT = 'Voldemort'
    TBD = auto()

    def __str__(self):
        return self.name
