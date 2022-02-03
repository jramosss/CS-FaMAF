#!/usr/bin/env python
# encoding: utf-8
# Revisión 2019 (a Python 3 y base64): Pablo Ventura
# Revisión 2011 Nicolás Wolovick
# Copyright 2008-2010 Natalia Bidart y Daniel Moisset
# $Id: server-test.py 388 2011-03-22 14:20:06Z nicolasw $

import unittest
import client
import constants
import select
import time
import socket
import os
import os.path
import logging
import sys

DATADIR = 'testdata'
TIMEOUT = 3  # Una cantidad razonable de segundos para esperar respuestas


class TestBase(unittest.TestCase):

    # Entorno de testing ...
    def setUp(self):
        print("\nIn method %s:" % self._testMethodName)
        os.system('rm -rf %s' % DATADIR)
        os.mkdir(DATADIR)

    def tearDown(self):
        os.system('rm -rf %s' % DATADIR)
        if hasattr(self, 'client'):
            if self.client.connected:
                # Deshabilitar el logging al desconectar
                # Dado que en algunos casos de prueba forzamos a que
                # nos desconecten de mala manera
                logging.getLogger().setLevel('CRITICAL')
                try:
                    self.client.close()
                except socket.error:
                    pass  # Seguramente ya se desconecto del otro lado
                logging.getLogger().setLevel('WARNING')
            del self.client
        if hasattr(self, 'output_file'):
            if os.path.exists(self.output_file):
                os.remove(self.output_file)
            del self.output_file

    # Funciones auxiliares:
    def new_client(self):
        assert not hasattr(self, 'client')
        try:
            self.client = client.Client()
        except socket.error:
            self.fail("No se pudo establecer conexión al server")
        return self.client


class TestHFTPServer(TestBase):

    # Tests
    def test_connect_and_quit(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            s.connect((constants.DEFAULT_ADDR, constants.DEFAULT_PORT))
        except socket.error:
            self.fail("No se pudo establecer conexión al server")
        s.send('quit\r\n'.encode("ascii"))
        # Le damos TIMEOUT segundos para responder _algo_ y desconectar
        w, _, __ = select.select([s], [], [], TIMEOUT)
        self.assertEqual(w, [s],
                         "Se envió quit, no hubo respuesta en %0.1f segundos" % TIMEOUT)
        # Medio segundo más par
        start = time.process_time()
        got = s.recv(1024)
        while got and time.process_time() - start <= 0.5:
            r, w, e = select.select([s], [], [], 0.5)
            self.assertEqual(r, [s], "Luego de la respuesta de quit, la "
                             "conexión se mantuvo activa por más "
                             "de 0.5 segundos")
            got = s.recv(1024)
        # Se desconectó?
        self.assertTrue(not got)
        s.close()

    def test_quit_answers_ok(self):
        c = self.new_client()
        c.close()
        self.assertEqual(c.status, constants.CODE_OK)

    def test_lookup(self):
        # Preparar el directorio con datos
        f = open(os.path.join(DATADIR, 'bar'), 'w').close()
        f = open(os.path.join(DATADIR, 'foo'), 'w').close()
        f = open(os.path.join(DATADIR, 'x'), 'w').close()
        c = self.new_client()
        files = sorted(c.file_lookup())
        self.assertEqual(c.status, constants.CODE_OK)
        # La lista de archivos es la correcta?
        self.assertEqual(files, ['bar', 'foo', 'x'])
        c.close()

    def test_get_metadata(self):
        test_size = 123459
        f = open(os.path.join(DATADIR, 'bar'), 'w')
        f.write('x' * test_size)
        f.close()
        c = self.new_client()
        m = c.get_metadata('bar')
        self.assertEqual(c.status, constants.CODE_OK)
        self.assertEqual(m, test_size,
                         "El tamaño reportado para el archivo no es el correcto")
        c.close()

    def test_get_metadata_empty(self):
        f = open(os.path.join(DATADIR, 'bar'), 'w').close()
        c = self.new_client()
        m = c.get_metadata('bar')
        self.assertEqual(c.status, constants.CODE_OK)
        self.assertEqual(m, 0,
                         "El tamaño reportado para el archivo no es el correcto")
        c.close()

    def test_get_full_slice(self):
        self.output_file = 'bar'
        test_data = 'The quick brown fox jumped over the lazy dog'
        f = open(os.path.join(DATADIR, self.output_file), 'w')
        f.write(test_data)
        f.close()
        c = self.new_client()
        c.get_slice(self.output_file, 0, len(test_data))
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(), test_data,
                         "El contenido del archivo no es el correcto")
        f.close()
        c.close()

    def test_partial_slices(self):
        self.output_file = 'bar'
        test_data = 'a' * 100 + 'b' * 200 + 'c' * 300
        f = open(os.path.join(DATADIR, self.output_file), 'w')
        f.write(test_data)
        f.close()
        c = self.new_client()
        c.get_slice(self.output_file, 0, 100)
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(), 'a' * 100,
                         "El contenido del archivo no es el correcto")
        f.close()
        c.get_slice(self.output_file, 100, 200)
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(),
                         'b' * 200, "El contenido del archivo no es el correcto")
        f.close()
        c.get_slice(self.output_file, 200, 200)
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(),
                         'b' * 100 + 'c' * 100,
                         "El contenido del archivo no es el correcto")
        f.close()
        c.get_slice(self.output_file, 500, 100)
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(),
                         'c' * 100, "El contenido del archivo no es el correcto")
        f.close()
        c.close()


class TestHFTPErrors(TestBase):

    def test_bad_eol(self):
        c = self.new_client()
        c.send('qui\nt\n')
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.BAD_EOL,
                         "El servidor no contestó 100 ante un fin de línea erróneo")

    def test_bad_command(self):
        c = self.new_client()
        c.send('verdura')
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.INVALID_COMMAND,
                         "El servidor no contestó 200 ante un comando inválido")
        c.close()

    def test_bad_argument_count(self):
        c = self.new_client()
        c.send('quit passing extra arguments!')
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.INVALID_ARGUMENTS,
                         "El servidor no contestó 201 ante una lista de argumentos "
                         "muy larga")
        c.close()

    def test_bad_argument_count_2(self):
        c = self.new_client()
        c.send('get_metadata')  # Sin argumentos
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.INVALID_ARGUMENTS,
                         "El servidor no contestó 201 ante una lista de argumentos "
                         "muy corta")
        c.close()

    def test_bad_argument_type(self):
        f = open(os.path.join(DATADIR, 'bar'), 'w')
        f.write('data')
        f.close()
        c = self.new_client()
        c.send('get_slice bar x x')  # Los argumentos deberían ser enteros
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.INVALID_ARGUMENTS,
                         "El servidor no contestó 201 ante una lista de argumentos "
                         "mal tipada (status=%d)" % status)
        c.close()

    def test_file_not_found(self):
        c = self.new_client()
        c.send('get_metadata does_not_exist')
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.FILE_NOT_FOUND,
                         "El servidor no contestó 202 ante un archivo inexistente")
        c.close()


class TestHFTPHard(TestBase):

    def test_command_in_pieces(self):
        c = self.new_client()
        for ch in 'quit\r\n':
            c.s.send(ch.encode("ascii"))
            os.system('sleep 1')  # Despaciiiiiiiiiiito
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.CODE_OK,
                         "El servidor no entendio un quit enviado de a un caracter por vez")

    def test_multiple_commands(self):
        c = self.new_client()
        l = c.s.send(
            'get_file_listing\r\nget_file_listing\r\n'.encode("ascii"))
        assert l == len(
            'get_file_listing\r\nget_file_listing\r\n'.encode("ascii"))
        status, message = c.read_response_line(TIMEOUT)
        self.assertEqual(status, constants.CODE_OK,
                         "El servidor no entendio muchos mensajes correctos "
                         "enviados juntos")
        c.connected = False
        c.s.close()

    def test_big_file(self):
        self.output_file = 'bar'
        f = open(os.path.join(DATADIR, self.output_file), 'wb')
        for i in range(1, 255):
            f.write(bytes([i]) * (2 ** 17))  # 128KB
        f.close()

        c = self.new_client()
        size = c.get_metadata(self.output_file)
        self.assertEqual(c.status, constants.CODE_OK)
        c.get_slice(self.output_file, 0, size)
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file, "rb")
        for i in range(1, 255):
            s = f.read(2 ** 17)  # 128 KB
            self.assertEqual(
                s, bytes([i]) * (2 ** 17), "El contenido del archivo no es el correcto")
        f.close()
        c.close()

    def test_big_filename(self):
        c = self.new_client()
        c.send('get_metadata ' + 'x' * (5 * 2 ** 20), timeout=120)
        # Le damos 4 minutos a esto
        status, message = c.read_response_line(TIMEOUT * 6)
        # Le damos un rato mas
        self.assertEqual(status, constants.FILE_NOT_FOUND,
                         "El servidor no contestó 202 ante un archivo inexistente con "
                         "nombre muy largo (status=%d)" % status)
        c.close()

    def test_data_with_nulls(self):
        self.output_file = 'bar'
        test_data = 'x' * 100 + '\0' * 100 + 'y' * 100
        f = open(os.path.join(DATADIR, self.output_file), 'w')
        f.write(test_data)
        f.close()
        c = self.new_client()
        c.get_slice(self.output_file, 0, len(test_data))
        self.assertEqual(c.status, constants.CODE_OK)
        f = open(self.output_file)
        self.assertEqual(f.read(), test_data,
                         "El contenido del archivo con NULs no es el correcto")
        f.close()
        c.close()

    def test_long_file_listing(self):
        # Preparar el directorio de datos
        correct_list = []
        for i in range(1000):
            filename = 'test_file%04d' % i
            f = open(os.path.join(DATADIR, filename), 'w').close()
            correct_list.append(filename)
        c = self.new_client()
        files = sorted(c.file_lookup())
        self.assertEqual(c.status, constants.CODE_OK)
        self.assertEqual(files, correct_list,
                         "La lista de 1000 archivos no es la correcta")
        c.close()


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TestHFTPServer))
    suite.addTest(unittest.makeSuite(TestHFTPErrors))
    suite.addTest(unittest.makeSuite(TestHFTPHard))
    return suite


def main():
    import optparse
    global DATADIR
    parser = optparse.OptionParser()
    parser.set_usage("%prog [opciones] [clases de tests]")
    parser.add_option('-d', '--datadir',
                      help="Directorio donde genera los datos; "
                      "CUIDADO: CORRER LOS TESTS *BORRA* LOS DATOS EN ESTE DIRECTORIO",
                      default=DATADIR)
    options, args = parser.parse_args()
    DATADIR = options.datadir
    # Correr tests
    unittest.main(argv=sys.argv[0:1] + args)


if __name__ == '__main__':
    main()
