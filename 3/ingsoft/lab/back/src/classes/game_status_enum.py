from enum import Enum, unique, auto


@unique
class GamePhase(Enum):
    """Enum for Game phase"""

    PROPOSE_DIRECTOR = auto()       # 1
    VOTE_DIRECTOR = auto()          # 2
    MINISTER_DISCARD = auto()       # 3
    DIRECTOR_DISCARD = auto()       # 4
    DE_WON = auto()                 # 5
    FO_WON = auto()                 # 6
    CAST_DIVINATION = auto()        # 7
    CAST_AVADA_KEDAVRA = auto()     # 8
    CAST_IMPERIUS = auto()          # 9
    CAST_CRUCIO = auto()            # 10
    CONFIRM_EXPELLIARMUS = auto()   # 11
    REJECTED_EXPELLIARMUS = auto()  # 12

    def __str__(self):
        return self.name
