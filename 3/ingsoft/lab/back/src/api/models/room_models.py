from pydantic import BaseModel, Field


class RoomCreationRequest(BaseModel):
    """
    Body of the request used to create a new room,
    Identifying owner by email isn't optimal(?)
    """
    name: str = Field(..., min_length=6, max_length=20,
                      description="The room's name")
    max_players: int = Field(
        ..., ge=5, le=10, description="Max allowed players in the room")


class VoteRequest(BaseModel):
    """
    Body of the request used to vote
    """
    vote: str = Field(..., description="Vote = <lumos/nox>")


class DiscardRequest(BaseModel):
    """
    Body of the request used for discarding a card.
    Used by minister and director only.
    If has te value 3, it means the director choose to cast expelliarmus.
    """
    card_index: int = Field(..., ge=0, le=3,
                            description="The index of the card to be discarded")


class ProposeDirectorRequest(BaseModel):
    """
    Body of the request used for proposing a director.
    Used by minister only.
    """
    director_uname: str = Field(...,
                                description="The email of the user to be proposed as director")


class TargetedSpellRequest(BaseModel):
    """
    Body of the request used for spells that are casted onto another player
    """
    target_uname: str = Field(...,
                              description="The email of the user to receive the spell")


class ChatRequest(BaseModel):
    """
    Body of the request used for sending messages to the chat.
    """
    msg: str = Field(..., max_length=256, min_length=1,
                     description="The message you want to send")
