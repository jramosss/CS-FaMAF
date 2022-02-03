CC := gcc

# To eliminate debugging messages use -DNDEBUG
CFLAGS := -O0 -std=gnu11 -Wall -Werror -Wno-unused-parameter -Werror=vla -g \
	  -D_FILE_OFFSET_BITS=64 -DFUSE_USE_VERSION=26 -D_GNU_SOURCE
CPPFLAGS := `pkg-config --cflags glib-2.0`
LDFLAGS=`pkg-config --libs glib-2.0` -lfuse

export CC
export CFLAGS
export CPPFLAGS
export LDFLAGS

HEADERS := $(wildcard *.h)
SOURCES := $(wildcard *.c)
TARGET := fat-fuse

OBJECTS=$(SOURCES:.c=.o)

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test-ht: hierarchy_tree.o
	make -C tests test_ht

test-ft:
	make -C tests test_ft

clean:
	rm -f $(TARGET) $(OBJECTS) tags cscope*
	make -C tests clean

#mount and run
mandrun:
	make mount
	make run

runf:
	make 
	./fat-fuse -f resources/hormigas.img ./mnt 
	fusermount -u ./mnt


mount:
	dd if=/dev/zero of=hormigas.img count=35 bs=1M 
	mkfs.vfat -F 32 -v ./hormigas.img 
	sudo mount -t vfat hormigas.img mnt/ -o umask=000 

umount:
	make clean
	sudo umount mnt

refresh:
	make mount
	mv hormigas.img resources/

refrun:
	make refresh
	make runf

.PHONY: clean
