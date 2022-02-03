import socket
from constants import *
from time import sleep

def test_single_command (s):
    s.send('get_file_listing\r\n'.encode("ascii"))

    print(s.recv(1024).decode(UTF8))

def test_uno(s):
    s.send('get'.encode("ascii"))
    s.send('_file_listing\r\n'.encode("ascii"))
    s.send('get_metadata README.md\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))


def test_dos (s):
    s.send('get_file_listing\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))
    s.send('get_metadata README.md\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))
    s.send('get_file_listing\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))
    s.send('quit\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))


def quit_many_args (s):
    s.send('quit por favor\r\n'.encode("ascii"))
    print(s.recv(1024).decode(UTF8))


if __name__ == '__main__':
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        s.connect((DEFAULT_ADDR,DEFAULT_PORT))
    except socket.error:
        print("No se pudo establecer conexión al server")
    
    print("-------Test single command")
    test_single_command(s)
    print("-------Test uno")
    test_uno(s)
    print("-------Test quit many args")
    quit_many_args(s)
    print("--------Test dos")
    test_dos(s)

    #s.close()