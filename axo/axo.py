#!/usr/bin/env python3
import serial
import sys



class axo:
    def __init__(self, port="/dev/ttyACM0"):
        self._ser = serial.Serial(port = port, timeout = 0.1)
        self._ser.write(b'Axop')
        self._sizes = { 'P' : 8,
                        'A' : 24,
                        'D' : 8,
                        'T' : 255,
                        '0' : 128,
                        '1' : 128,
                        '2' : 128,
                        '3' : 128,
                        '4' : 128,
                        '5' : 128,
                        '6' : 128,
                        '7' : 128,
                        '8' : 128,
                        'd' : 12,
                        'f' : 4, }
        while True:
            (hdr,data) = self.read_packet()
            print(hdr,len(data))

    def read(self,n):
        return self._ser.read(n)

    def read_packet(self):
        hdr = self.read(4)
        if hdr[0:3] != b'Axo':
            raise NameError("hdr = %s" % hdr)
        size = self._sizes[chr(hdr[3])]
        payload = self.read(size)
        return (hdr,payload)
        

if __name__ == '__main__':
    axo()
