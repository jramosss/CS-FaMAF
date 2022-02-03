import sys
import os
room_name = sys.argv[1]
nof_players = int(sys.argv[2])

for p in range(1, nof_players):
    os.system(
        f"gnome-terminal -e 'bash -c \"python3 player_bot.py {room_name} {p} {nof_players}\"'")
