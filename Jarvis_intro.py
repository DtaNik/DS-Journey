# -*- coding: utf-8 -*-
"""
Created on Fri Jun 22 13:53:05 2018

@author: A0744957
"""
import getpass
import subprocess
import re
import sys

def userdetails():
    username = getpass.getuser()
    p = subprocess.Popen(
            'net user %s /domain' % username,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
    info , err= p.stdout.read(),p.stderr.read()
    full_name = re.findall(r'Full Name\s+(.*\S)', info)
    full_name=full_name[0]
    return(full_name[0:full_name.find(' ')])


Fst_name=userdetails()

converse=str(raw_input())

if('jarvis' in converse.lower()):
    print( "Hi "+Fst_name+",\nHow can i help you?")
    give_instruction=str(raw_input())
    if('fuck' in give_instruction.lower() and 'PACE' in give_instruction.upper()):
        execfile("\\hewitt.com\apfs\India\Gurgaon\GURDAT13\BPO\BEACONS\Team Folder\Team IEG GGN\Nikhil Chauhan\Python\prog.py"")
    else:
        print('Not updated yet')
        sys.exit()
else:
    print("Say my name correctly!")