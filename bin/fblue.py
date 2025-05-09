#!/usr/bin/env python3

import os
import sys
from cmd import Cmd

import bluetooth
import bluetooth.ble
import fc


class FBlueToolbox(Cmd):
    intro = "\nWelcome to Bluetooth Toolbox.\n"
    prompt = "bt > "
    devices = []

    def precmd(self, line):
        if line == "EOF":
            return "exit"

        return line

    def do_scan(self, inp):
        print("Scanning ...")

        nearby_devices = bluetooth.discover_devices(duration=5, lookup_names=True)
        print("found %d devices" % len(nearby_devices))

        self.devices = []
        for addr, name in nearby_devices:
            print("  %s - %s" % (addr, name))
            self.devices.append("%s %s" % (addr, name))

    def do_discovery(self, inp):
        if inp is None or inp == "":

            r = fc.user_select(self.devices)
            if r is None:
                print("No device specified.")
                return
            inp = r.split()[0]

        print("Discovering " + inp + " ...")

        services = bluetooth.find_service(address=inp)

        if len(services) > 0:
            print("found %d services on %s" % (len(services), inp))
            print("")
        else:
            print("no services found")

        for svc in services:
            print("──────────────────────────────────")
            print("Service Name: %s" % svc["name"])
            print("    Host:        %s" % svc["host"])
            print("    Description: %s" % svc["description"])
            print("    Provided By: %s" % svc["provider"])
            print("    Protocol:    %s" % svc["protocol"])
            print("    channel/PSM: %s" % svc["port"])
            print("    svc classes: %s " % svc["service-classes"])
            print("    profiles:    %s " % svc["profiles"])
            print("    service id:  %s " % svc["service-id"])
            print("")

    def do_lescan(self, inp):
        service = bluetooth.ble.DiscoveryService()
        devices = service.discover(4)

        for address, name in devices.items():
            print("name: {}, address: {}".format(name, address))

    def do_exit(self, inp):
        print("\nByebye !")
        return True


FBlueToolbox().cmdloop()
