# encoding: utf-8
# Revisión 2019 (a Python 3 y base64): Pablo Ventura
# Copyright 2014 Carlos Bederián
# $Id: connection.py 455 2011-05-01 00:32:09Z carlos $

import socket
from constants import *
from base64 import b64encode
import os
import time

class Connection(object):
    """
    Conexión punto a punto entre el servidor y un cliente.
    Se encarga de satisfacer los pedidos del cliente hasta
    que termina la conexión.
    """

    def __init__(self, socket, directory):
        self.socket     = socket
        self.directory  = directory
        self.buffer     = ''


    def __list_files(self, comm, dir):
        if len(comm) != 1:
            self.socket.sendall(generate_response(INVALID_ARGUMENTS))

        else:
            files = os.listdir(dir)
            res = ""
            for file in files:
                res += file + EOL
            self.socket.sendall(generate_response(CODE_OK, res))


    def __send_metadata(self, comm):
        if len(comm) != 2:
            self.socket.sendall(generate_response(INVALID_ARGUMENTS))
        else:
            try:
                FILENAME = comm[1]
                if len(FILENAME) > 500:
                    response = generate_response(FILE_NOT_FOUND)
                else:
                    FILENAME = os.path.join(self.directory, FILENAME)
                    response = ""
                    size = os.path.getsize(FILENAME)
                    response = generate_response(CODE_OK, size)
            except OSError as o:
                # Si el archivo no existe
                response = generate_response(FILE_NOT_FOUND)
            except IndexError as i:
                # Si no nos pasaron archivo
                response = generate_response(INVALID_ARGUMENTS)
            finally:
                self.socket.sendall(response)


    def __send_slice(self, comm):
        response = ""
        if len(comm) != 4:
            self.socket.sendall(generate_response(INVALID_ARGUMENTS))

        else:
            try:
                FILENAME = comm[1]
                offset = int(comm[2])
                size = int(comm[3])
                if offset < 0 or size < 0:
                    self.socket.sendall(generate_response(INVALID_ARGUMENTS))
    
                FILENAME = os.path.join(self.directory, FILENAME)
                file = open(FILENAME, "rb")
                if offset + size > os.path.getsize(FILENAME):
                    # Si el pedido se refiere a una posición inexistenteen un archivo
                    response = generate_response(BAD_OFFSET)
                else:
                    file.seek(offset)
                    resaux = b64encode(file.read(size))
                    response = generate_response(CODE_OK, resaux.decode('ascii'))

            except OSError:
                # Si el archivo no existe
                response = generate_response(FILE_NOT_FOUND)

            except (ValueError, IndexError) as v:
                print("Excc2: ",v.__str__())
                response = generate_response(INVALID_ARGUMENTS)

            finally:
                self.socket.sendall(response)


    def __quit_conn(self, comm):
        if len(comm) != 1:
            self.socket.sendall(generate_response(INVALID_ARGUMENTS))
            return False

        else:
            self.socket.sendall(generate_response(CODE_OK))
            return True

    
    def __get_server_time (self,comm):
        if len(comm) != 1:
            self.socket.sendall(generate_response(INVALID_ARGUMENTS))
        else:
            TIME = time.strftime("%c")
            try:
                self.socket.sendall(generate_response(CODE_OK,TIME))
            except UnicodeDecodeError:
                pass



    def __exec_comm(self, comm):

        if comm[0] == 'get_file_listing':
            self.__list_files(comm,self.directory)

        elif comm[0] == "get_metadata":
            self.__send_metadata(comm)

        elif comm[0] == 'get_slice':
            self.__send_slice(comm)

        elif comm[0] == 'get_server_time':
            self.__get_server_time(comm)

        elif comm[0] == 'quit':
            return self.__quit_conn(comm)

        else:
            # Comando invalido
            self.socket.sendall(generate_response(INVALID_COMMAND))


    #Atiende eventos de la conexión hasta que termina.
    def handle(self):
        while True:
            #Lo convertimos a string
            req = self.socket.recv(4096).decode(UTF8)

            if req != "":
                self.buffer += req
                if len(req) < 100:
                    print("Input: ",req) 
            else:
                break

            """
                En la consigna dice 
                Por ejemplo, un comando malintencionado, de gran longitud, 
                podría provocarun DoSo disminución de performance en el server
                y podría ser intervenido por un error fatal de este tipo.

                Es por eso que habiamos puesto esta guarda, pero despues vemos 
                que en el test big_filename necesita que aceptemos una request
                con un tamaño de 5*2^20, por eso lo comentamos y lo dejamos 
                en la duda

                if len(self.buffer) > 4096 or len(req) > 4096:
                    self.socket.sendall(generate_response(BAD_REQUEST))
                    break
            """

            if EOL in self.buffer:
                content = self.buffer.split(EOL)
                #Obtenemos el primer comando de la cadena y lo decolamos
                command         = content.pop(0)

                #Volvemos a setear el buffer como un string
                self.buffer = ""
                for s in content:
                    if s != "":
                        self.buffer += s + EOL

                #Si en nuestro comando hay un \n prematuro tiramos un error 100 fatal
                if '\n' in command:
                    self.socket.sendall(generate_response(BAD_EOL))
                    break
                
                command = command.strip()

                splitted = command.split(' ')

                for comm in splitted:
                    comm = comm.strip()

                if self.__exec_comm(splitted):
                    break


