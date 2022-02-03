from enum import Enum, unique
from random import shuffle
from typing import List


@unique
class Card(Enum):
    FO = 'Order of the Fenix proclamation'
    DE = 'Death Eater proclamation'

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name


class Deck:
    def __init__(self):
        self.cards: List[Card] = []
        self.shuffle_deck()

    def shuffle_deck(self):
        tail: List[Card] = []
        for _ in range(11):
            tail.append(Card.DE)

        for _ in range(6):
            tail.append(Card.FO)

        shuffle(tail)
        self.cards += tail

    def take_card(self):
        if (self.cards == []):
            self.shuffle_deck()

        return self.cards.pop(0)

    def take_3_cards(self):
        top3: List[Card] = []
        for _ in range(3):
            top3.append(self.take_card())

        return top3

    def see_top_cards(self):
        if (len(self.cards) < 3):
            self.shuffle_deck()
        return [self.cards[0], self.cards[1], self.cards[2]]

    def load_deck(self, card_list):
        cards = []
        for card in card_list:
            if card == Card.FO.value:
                cards.append(Card.FO)
            else:
                cards.append(Card.DE)
        self.cards = cards
