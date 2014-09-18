#!/usr/bin/env python3
import serial
import sys



class axo:
    def __init__(self, port="/dev/ttyACM0"):
        self._ser = serial.Serial(port = port, timeout = 0.1)
        self.ping()
        self._sizes = { 'P' : 8,   # paramchange
                        'A' : 24,  # ack
                        'D' : 8,   # display
                        'T' : 255, # text
                        '0' : 128, # LCD
                        '1' : 128,
                        '2' : 128,
                        '3' : 128,
                        '4' : 128,
                        '5' : 128,
                        '6' : 128,
                        '7' : 128,
                        '8' : 128,
                        'd' : 12,  # sdinfo
                        'f' : 4, } # fileinfo
        self.dump()

    def dump(self):
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
        
    def ping(self):
        self._ser.write(b'Axop')

if __name__ == '__main__':
    axo()
