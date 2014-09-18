#!/usr/bin/env python3
import serial
import sys

def bytes2words(seq, byte_index_list):
    s = (b for b in seq)  # need sequence api
    while 1:
        acc = 0
        for i in byte_index_list:
            acc = acc | ((0xFF & s.__next__()) << (8 * i))
        yield (acc)

def bytes2words_le(seq, bytes_per_word=2):
    """Convert little endian byte stream to word stream."""
    return bytes2words(seq, list(range(bytes_per_word)))

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
        self._decode = { 'A' : self.decode_ack }
        self.dump()

    def dump(self):
        while True:
            rv = self.read_packet()
            if rv:
                print(rv)

    def read(self,n):
        return self._ser.read(n)

    def read_packet(self):
        hdr = self.read(4)
        if hdr[0:3] != b'Axo':
            raise NameError("hdr = %s" % hdr)
        tag = chr(hdr[3])
        size = self._sizes[tag]
        payload = self.read(size)
        try:
            rv = self._decode[tag](payload)
        except Exception as e:
            rv = False
        return rv
        
    def ping(self):
        self._ser.write(b'Axop')

    def decode_ack(self, data):
        [FirmwareId, DSPLoad, PatchID] = bytes2words_le(data[:12], 4)
        [CpuId] = bytes2words_le(data[12:],12)
        return { 'FirmwareId' : FirmwareId,
                 'DSPLoad'    : DSPLoad,
                 'CpuId'      : CpuId, }
                 

if __name__ == '__main__':
    axo()
