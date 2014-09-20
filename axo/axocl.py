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

def words2bytes_le(word_seq, bytes_per_word=2):
    for word in word_seq:
        for i in range(bytes_per_word):
            yield(0xFF & (word >> (i * 8)))

class axo:
    def __init__(self, port="/dev/ttyACM0", verbose=False):
        self.verbose = verbose
        self.ser = serial.Serial(port = port, timeout = 0.1)
        self.sizes = { 
            'P' : 8,   # paramchange
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
        self.decode = {
            'A' : self.decode_ack }
        self.stats = False

    def poll(self, condition = lambda: True):
        while condition(rv):
            self.read_packet()

    def read(self,n):
        return self.ser.read(n)

    def read_raw_packet(self):
        hdr = self.read(4)
        if hdr[0:3] != b'Axo':
            raise NameError("hdr = %s" % hdr)
        tag = chr(hdr[3])
        size = self.sizes[tag]
        payload = self.read(size)
        return (tag, payload)
        
    def read_packet(self):
        (tag,payload) = self.read_raw_packet()
        try:
            rv = self.decode[tag](payload)
        except Exception as e:
            rv = False
        if self.verbose:
            print(tag,len(payload))
        return rv
        
    def ping(self):
        self.ser.write(b'Axop')
        self.wait_ack()
        print(self.stats)

    def start(self):
        self.ser.write(b'Axos')

    def stop(self):
        self.ser.write(b'AxoS')

    def dump(self):
        while True:
            (tag,payload) = self.read_raw_packet()
            print("Axo%s: %s" % (tag, " ".join(["%02X" % p for p in payload])))

    def wait_ack(self):
        self.stats = False
        self.poll(condition = lambda: not self.stats)

    def load(self, filename, addr):
        with open(filename, "rb") as f:
            data = f.read(64*1024)
        print("Loading %d bytes from %s at 0x%08X" % (len(data), filename, addr))
        self.ser.write(b'AxoW')
        self.ser.write(bytes(words2bytes_le([addr, len(data)], 4)))
        self.ser.write(data)
        self.wait_ack()

    def decode_ack(self, data):
        [FirmwareId, DSPLoad, PatchID] = bytes2words_le(data[:12], 4)
        [CpuId] = bytes2words_le(data[12:],12)
        self.stats = { 'FirmwareId' : "%X" % FirmwareId,
                       'DSPLoad'    : DSPLoad,
                       'CpuId'      : "%X" % CpuId, }
        return self.stats

                 
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Axoloti Command Line Tool')
    parser.add_argument('--dev', nargs=1, default=['/dev/ttyACM0'], help='serial device')
    parser.add_argument('--addr', nargs=1, default=['0x20010000'], help='patch load address')
    parser.add_argument('--verbose', action='store_true', help='verbose output')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--ping', action='store_true', help='ping & print ack stats')
    group.add_argument('--start', action='store_true', help='start patch')
    group.add_argument('--stop', action='store_true', help='stop patch')
    group.add_argument('--dump', action='store_true', help='dump messages')
    group.add_argument('--load', nargs=1, help='upload patch binary')
    args = parser.parse_args()
    a = axo(port=args.dev[0], verbose=args.verbose)
    if args.ping:
        a.ping()
    elif args.start:
        a.start()
    elif args.stop:
        a.stop()
    elif args.dump:
        a.dump()
    elif args.load:
        a.load(args.load[0], addr=int(args.addr[0], 0))

