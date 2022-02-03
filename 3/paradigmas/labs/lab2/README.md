# freelanceando.com #

## Build & Run ##

```sh
$ cd freelanceando
$ sbt
> jetty:start
> browse
```

If `browse` doesn't launch your browser, manually open [http://localhost:8080/](http://localhost:8080/) in your browser.


### Run with a different database

You can set the environment variable `FL_DATABASE` to read the database from
a different directory when calling sbt. By deafult, the app will look for the
database on a directory `./database`

```sh
$ cd freelanceando
$ FL_DATABASE="new_directory_path" sbt
> jetty:start
> browse
```

When compiling, sbt will generate two new directories `target` and `project` that you should NEVER commit to your git repository.

### Re-starting

The server won't re-start after every change, you can stop it manually with

```sh
> jetty:stop
```
or even chain both commands with

```sh
> jetty:stop;jetty:start
```

## How to run the test

First, call the following script:

```sh
$ bash tests/clean_database.sh
```

This will create a directory `test_database` with the following files:

 - categories.json
 - utils.json

*utils.json* will be only used as needed objects for all the test cases.
You do not need to include them in your database.

Second, start your server with the correct directory

```sh
$ FL_DATABASE=tests/test_database sbt
> jetty:start
```

Finally, run the test files located in the directory *tests* by means of the following script:
```sh
$ python3 runner.py
```

In order to run test cases you need Python 3.6 or higher.

VERY IMPORTANT: between test runs, restart the server to clean possibly stored
instances.

### How to run test individually

If you want to run a single test suite, you can, by doing

```sh
$ python3 test_client.py
```

Or you can even run a specific test by adding the class name and test name as parameters to the python call.

```sh
$ python3 test_client.py PayAPITest.test_post_pay
```

