import argparse
import select
import sys

import bluetooth as bt


def bind_io(sock):
    while True:
        ready = select.select([sys.stdin, sock], [], [])
        if not ready[0]:
            sock.close()
            print("\nExit\n")
            sys.exit(0)

        for s in ready[0]:
            if sys.stdin == s:
                d = s.readline()
            else:
                d = s.recv(1024)

            if s == sock:
                print(d)
            else:
                sock.send(d)


def _rfcomm_client(addr, port):
    sock = bt.BluetoothSocket(bt.RFCOMM)
    sock.connect((addr, int(port)))

    bind_io(sock)


def _rfcomm_server(addr, port=bt.PORT_ANY):
    sock = bt.BluetoothSocket(bt.RFCOMM)
    sock.bind(("", bt.PORT_ANY))
    sock.listen(1)

    print("before accept")
    client_sock, client_info = sock.accept()
    print("Accepted connection from", client_info)

    bind_io(client_sock)


def _inquiry():
    print("Performing inquiry...")

    nearby_devices = bt.discover_devices(
        duration=8, lookup_names=True, flush_cache=True, lookup_class=False
    )

    print("Found {} devices".format(len(nearby_devices)))

    for addr, name in nearby_devices:
        try:
            print("   {} - {}".format(addr, name))
        except UnicodeEncodeError:
            print("   {} - {}".format(addr, name.encode("utf-8", "replace")))


def _browse(addr):
    if addr is None:
        addr = "all"
    services = bt.find_service(address=addr)
    last_host = None

    for svc in services:
        if svc["host"] != last_host:
            last_host = svc["host"]
            print("Host: ", last_host)

        print("\n  Service Name:", svc["name"])
        if svc["description"] is not None:
            print("    Description:", svc["description"])
        if svc["provider"] is not None:
            print("    Provided By:", svc["provider"])
        if svc["protocol"] is not None:
            print("    Protocol:   ", svc["protocol"])
        print("    channel/PSM:", svc["port"])
        if svc["service-classes"] != []:
            print("    svc classes:", svc["service-classes"])
        if svc["profiles"] != []:
            print("    profiles:   ", svc["profiles"])
        if svc["service-id"] is not None:
            print("    service id: ", svc["service-id"])


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-a", dest="addr", default=None, help="target bt address")
    parser.add_argument("-p", dest="port", default=1, help="rfcomm port")
    parser.add_argument("-brow", dest="browse", action="store_true", help="browse SDP")
    parser.add_argument(
        "-inq", dest="inquiry", action="store_true", help="inquiry mode"
    )
    parser.add_argument(
        "-rfc",
        dest="rfcomm_client",
        action="store_true",
        help="rfcomm client mode",
    )
    parser.add_argument(
        "-rfs",
        dest="rfcomm_server",
        action="store_true",
        help="rfcomm server mode",
    )

    args = parser.parse_args()

    if args.rfcomm_client:
        _rfcomm_client(args.addr, args.port)
    elif args.rfcomm_server:
        _rfcomm_server(args.addr)
    elif args.inquiry:
        _inquiry()
    elif args.browse:
        _browse(args.addr)
    else:
        parser.print_help()
