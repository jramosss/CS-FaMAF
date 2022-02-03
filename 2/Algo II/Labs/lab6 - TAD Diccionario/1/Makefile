CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g
SOURCES = map.c string.c testing.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = map

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LFLAGS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	$(RM) $(OBJECTS) $(TARGET)
