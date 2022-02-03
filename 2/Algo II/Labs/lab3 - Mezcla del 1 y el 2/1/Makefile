TARGET = weather
CFLAGS = -Wall -Wextra -Werror -pedantic -std=c99 -g
CC = gcc
SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

$(TARGET):$(OBJECTS) 
	$(CC) $(CFLAGS)  -o $@ $^

clean:
	rm -f $(OBJECTS) $(TARGET)

