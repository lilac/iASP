#! /usr/bin/python

import os
import re
import zipfile

TPTP_PATH = "/mnt/data/TPTP-v3.1.0/TPTP2X/protein/"
OUT_PATH = "/mnt/data/TPTP-v3.1.0-zipped/"
SUFFIX = ".tme"

def zip_file (_, dir, files):
    rel_path = dir[ len (TPTP_PATH): ]
    print rel_path

    for file_name in files:
        file_path = os.path.join (dir, file_name)
        print file_path

        if os.path.isdir (file_path):
            out_path = os.path.join (OUT_PATH, rel_path, file_name)
            print out_path

            os.mkdir (out_path)

        elif (file_name[-len(SUFFIX):] == SUFFIX):
            zip_file_name = file_name[:-len(SUFFIX)] + ".zip"
            print (zip_file_name)
            out_path = os.path.join (OUT_PATH, rel_path, zip_file_name)
            print out_path

            #file = open (file_path)
            zip_file = zipfile.ZipFile (out_path, 'w', zipfile.ZIP_DEFLATED)
            zip_file.write (file_path, file_name)
            zip_file.close ()

        else:
            print ("Ignoring: " + file_path)


# get all wanted problems and print them line by line
os.mkdir (OUT_PATH)
os.path.walk (TPTP_PATH, zip_file, ())
#os.walk (TPTP_PATH)
