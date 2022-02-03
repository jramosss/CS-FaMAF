# Secret Voldemort by Pyhterin, Backend

Secret Voldemort is a card game, now in pyhton.

## Installation

To be able to run our backend you will need to execute

```
pip3 install -r requirements.txt
```

## Usage

To start the server, go to `src/` and run:

```
uvicorn main:app
```

You can interact with the server using the [OPENapi](127.0.0.1/8000/docs)

If you wish to test the software, on the `/src` folder, do:

```
$ python3 test_setup.py
$ pytest --cov=. test_*
```

If you want to see how a match of 10 players is played, you can delete all the
'#' on the test_game.py file, and run:

```
$ python3 test_setup.py
$ python3 test_game_10.py
```

If you wish to play a game with bots, on your `/src` folder run:

```
$ python3 test_setup.py (only if you haven't done it before!)
$ uvicorn test_main:test_app --reload
```

Then, running the [FrontEnd](https://github.com/ibaldoncini/Front-Pytherin) do:

-> Login with the user "player0@example.com" with the password: "Heladera65"

-> Create a new room for the amount of player that you want.

Then, on another terminal, run the command:

```
python3 deploy_n_bots.py <room_name> <n° players>
```

With this command, the bots will join the match, and on the browser you can
start the game.
Enjoy!

## License

[MIT](https://choosealicense.com/licenses/mit/)
