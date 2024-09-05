#!/usr/bin/python 
#-*- coding: utf-8 -*-
"""
This module checks user authorization
"""

import MySQLdb
import cgi
import glob
import os
import shutil
import string
import sys
import tempfile
import time
import traceback
from pywps import carto
from pywps.carto import StatusChecker

def errorOut(e):
   print "Error %d: %s" % (e.args[0], e.args[1])
   sys.exit(1)

def connectDB():
   try:
      conn = MySQLdb.connect(host = "localhost", user = "afidsuser", passwd="afidspwd", db = "afidsdb")
   except MySQLdb.Error, e:
      errorOut(e)

   return conn

def isValidUser(conn,usr,pwd):
    try:
       cursor = conn.cursor()
       cursor.execute("""SELECT id,username FROM afids_users""")
       for record in cursor.fetchall():
          id = record[0]
          user = record[1]
          if user == usr:
             cursor.execute("select password from afids_users where id = %s" %(id))
             actual = (cursor.fetchone())[0]
             if actual == pwd:
                return 1
             else:
                return -1

       cursor.close
    except MySQLdb.Error, e:
       errorOut(e)

    return 0

def getDir(conn,usr,pwd):
   try:
      cursor = conn.cursor()
      cursor.execute("""SELECT dir FROM afids_users WHERE username = %s and password = %s""", (usr, pwd))
      r = cursor.fetchone()
      if r == None:
          userDir = ""
      else:
          userDir = r[0]
      cursor.close
   except MySQLdb.Error, e:
      errorOut(e)

   return userDir

def main():
    """
    This module checks user authorization
    """

    found = 0
    form = cgi.FieldStorage()
    try:
       trans = string.maketrans("/\"\'\\","____")
       usr = form[ 'username' ].value.translate(trans)
       pwd = form[ 'password' ].value.translate(trans)
       userDir = form[ 'userdir' ].value.translate(trans)
       if userDir == "-":
          conn = connectDB()
          found = isValidUser(conn,usr,pwd)
          if found == 1:
             userDir = getDir(conn,usr,pwd)
             conn.close()
          else:
             found = 0
       else:
          userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"
          pathList = glob.glob(userData)
          if len(pathList) == 1:
             found = 1
              
    except KeyError, e:
        found = 0
       
    print "Content-type: text/html\n\n"
    print "<html><head>"
    print "<title>AFIDS OGC WPS</title>"

    if found == 0 or found == -1:       
        print "<meta http-equiv=\"refresh\" content=\"7; url=/\" />\n"
        print "</head><body>"
        print "Authentication failed."
    else:
        print "<script type=\"text/javascript\">"

        print "function setSensorType(div_id)"
        print "{"
        print "hideAllSensors();"
        print "changeObjectVisibility(div_id,\"inline\");"
        print "if(div_id==\"ntm\")"
        print "  changeObjectVisibility(\"ntmmenu\",\"inline\");"
        print "else"
        print "  changeObjectVisibility(\"nonntmmenu\",\"inline\");"
        print "}"

        print "function hideAllSensors()"
        print "{"
        print "changeObjectVisibility(\"landsat\",\"none\");"
        print "changeObjectVisibility(\"aster\",\"none\");"
        print "changeObjectVisibility(\"quickbird\",\"none\");"
        print "changeObjectVisibility(\"ntm\",\"none\");"
        print "changeObjectVisibility(\"nonntmmenu\",\"none\");"
        print "changeObjectVisibility(\"ntmmenu\",\"none\");"
        print "}"

        print "function setCoregType(div_id)"
        print "{"
        print "hideAllCoregTypes();"

        print "changeObjectVisibility(\"landsat\" + div_id,\"inline\");"
        print "changeObjectVisibility(\"quickbird\" + div_id,\"inline\");"
        print "changeObjectVisibility(\"aster\" + div_id,\"inline\");"
        print "}"

        print "function setNtmProcessingType(div_id)"
        print "{"
        print "if (div_id==\"concept\") {"
        print "  changeObjectVisibility(\"ntmconcept\",\"inline\");"
        print "  changeObjectVisibility(\"ntmprocess\",\"none\");"
        print "} else {"
        print "  changeObjectVisibility(\"ntmconcept\",\"none\");"
        print "  changeObjectVisibility(\"ntmprocess\",\"inline\");"
        print "}"
        print "}"

        print "function hideAllCoregTypes()"
        print "{"
        print "changeObjectVisibility(\"landsatconcept\",\"none\");"
        print "changeObjectVisibility(\"landsatmaster\",\"none\");"
        print "changeObjectVisibility(\"landsatsecondary\",\"none\");"
        print "changeObjectVisibility(\"landsatadditional\",\"none\");"
        print "changeObjectVisibility(\"quickbirdconcept\",\"none\");"
        print "changeObjectVisibility(\"quickbirdmaster\",\"none\");"
        print "changeObjectVisibility(\"quickbirdsecondary\",\"none\");"
        print "changeObjectVisibility(\"quickbirdadditional\",\"none\");"
        print "changeObjectVisibility(\"asterconcept\",\"none\");"
        print "changeObjectVisibility(\"astermaster\",\"none\");"
        print "changeObjectVisibility(\"astersecondary\",\"none\");"
        print "changeObjectVisibility(\"asteradditional\",\"none\");"
        print "}"

        print "function getStyleObject(objectId) {"
        print "if(document.getElementById && document.getElementById(objectId)) {"
        print "return document.getElementById(objectId).style;"
        print "}"
        print "else if (document.all && document.all(objectId)) {"
        print "return document.all(objectId).style;"
        print "}"
        print "else if (document.layers && document.layers[objectId]) {"
        print "return document.layers[objectId];"
        print "} else {"
        print "return false;"
        print "}"
        print "}"

        print "function changeObjectVisibility(objectId, newVisibility) {"
        print "var styleObject = getStyleObject(objectId);"
        print "if (styleObject) {"
        print "styleObject.display = newVisibility;"
        print "}"
        print "}"

        print "</script>"

        print "</head><body>"

        print "<h2>AFIDS Image Coregistration Service</h2>"

        print "<select name=\"sensortypemenu\" onChange=\"setSensorType(this.value)\">"
        print "<option value =\"aster\" selected=\"selected\">ASTER</option>"
        print "<option value =\"landsat\">Landsat</option>"
        print "<option value =\"ntm\">NTM</option>"
        print "<option value =\"quickbird\">Quickbird</option>"
        print "</select>"

        print "<span id=\"nonntmmenu\">"
        print "<select name=\"coregtypemenu\" onChange=\"setCoregType(this.value)\">"
        print "<option value =\"concept\" selected=\"selected\">Concept of Operation</option>"
        print "<option value =\"master\">Master</option>"
        print "<option value =\"secondary\">Secondary</option>"
        print "<option value =\"additional\">Additional Band</option>"
        print "</select>"
        print "</span>"

        print "<span id=\"ntmmenu\" style=\"display:none\">"
        print "<select name=\"ntmprocessingmenu\" onChange=\"setNtmProcessingType(this.value)\">"
        print "<option value =\"concept\" selected=\"selected\">Concept of Operation</option>"
        print "<option value =\"process\">Process</option>"
        print "</select>"
        print "</span>"

        print "<a href=\"/\">Logout</a>"

        # Landsat
        print "<div id=\"landsat\" style=\"display:none\">"
        print "<div id=\"landsatconcept\">"
        print "<h2>Landsat Coregistration Concept of Operation</h2>"

        print "<p>The AFIDS WPS registers your Landsat image (band3) to an"
        print "orthorectified base image, and returns to you the registered image in"
        print "GeoTIFF format, plus registration accuracy information and a log"
        print "file. You may then optionally upload the remaining Landsat bands to"
        print "have them registered as well (using the same grid so everything"
        print "matches exactly). After the first registration, a second date Landsat"
        print "image can be optionally sent to the AFIDS WPS for coregistration with"
        print "your first image. These First and Second Landsat images are referred"
        print "to as Master and Secondary. If desired, multiple Secondary images can"
        print "be registered to the Master to develop a time-series of registered"
        print "Landsat imagery.</p>"

        print "<p>Master - Register your band3 Landsat to an orthorectified base image.</p>"
        print "<p>Secondary - Register a second date band3 Landsat to the master band3 Landsat.</p>"
        print "<p>Additional Band - Apply the registration (grid) from the master or"
        print "secondary Landsat to its other bands.</p>"

        print "</div>"

        # Landsat Master
        print "<div id=\"landsatmaster\" style=\"display:none\">"
        print "<h2>Landsat Master Coregistration</h2>"
        print "<p>This process coregisters band 3 of a Landsat image to a precalculated"
        print "orthorectified base image.</p>"

        print "<p>Supported Landsat format types:<br>"
        print "Landsat uncorrected (L0R \"raw\" or Level 1G) Landsat MSS, TM, ETM, or"
        print "ETM+ Imagery in TIFF, HDF, or USGS \"NLAPS\" format (.b3, .b30,"
        print "*nn3.tif, .I3 or .FST) with their metadata (.met, .ip3, .H1, WO.tif,"
        print "or .MTL) file. Other formats and metadata may be acceptable. The 80m"
        print "Landsat MSS contains 4 files. The 30m TM and ETM Landsats contain 7 or"
        print "8 spectral channels (blu, grn, red, nir, fir, tir, Geology Shortwave,"
        print "and 15m Panchromatic). See sample <a href=\"/sampleData\" target=\"sample\">data</a>.</p>"

        print "<form action=\"coregisterLandsatMasterForm.py\" method=\"post\" enctype=\"multipart/form-data\">"

        print "<table>"
        print "<tr>"
        print "<td>Input band-3 image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input metadata text file</td> <td><input type=\"file\" name=\"rawmeta\" size=\"75\" /></td>"
        print "</tr>"

        # previous metadata menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousmeta\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"

        # output prefix
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. lsat3_master_b3"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"
        print "</div>"

        # Landsat Secondary
        print "<div id=\"landsatsecondary\" style=\"display:none\">"
        print "<h2>Landsat Secondary Coregistration</h2>"

        print "<p>This process coregisters band 3 of a Landsat image to the"
        print "precalculated orthorectified Master Landsat image.</p>"

        print "<form action=\"coregisterLandsatSecondaryForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input secondary band-3 image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"

        print "<tr>"
        print "<td>Input secondary metadata text file</td> <td><input type=\"file\" name=\"rawmeta\" size=\"75\" /></td>"
        print "</tr>"


        # previous metadata menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousmeta\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input master image (.img) file</td> <td>"

        print "<select name=\"refimg\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. lsat3_second_b3"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"

        # Landsat Additional Band
        print "<div id=\"landsatadditional\" style=\"display:none\">"
        print "<h2>Landsat Additional Band Coregistration</h2>"

        print "<p>This process coregisters a Master or Secondary band (bands 1-2, 4-8)"
        print "with the previously coregistered band 3 of the same Landsat image (in"
        print "VICAR format).</p>"

        print "<form action=\"coregisterLandsatAdditionalForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input band-3 image (.img) file</td> <td>"

        print "<select name=\"fnamein\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Input band-3 grid (.grid) file</td> <td>"

        print "<select name=\"gridtar\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.grid")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. lsat3_master_b1 or lsat3_second_b2"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"
        print "</div>"

        # Quickbird
        print "<div id=\"quickbird\" style=\"display:none\">"
        print "<div id=\"quickbirdconcept\">"
        print "<h2>Quickbird Coregistration Concept of Operation</h2>"

        print "<p>The AFIDS WPS registers your DigitalGlobe(c) Quickbird panchromatic"
        print "image to an orthorectified base image, and returns to you the"
        print "registered image in NITF format plus a log file. You may then"
        print "optionally upload the Quickbird multispectral image file to have the multispectral bands"
        print "co-registered (using the same grid plus a rescaling, so everything"
        print "matches exactly). After the first registration, a second date"
        print "Quickbird panchromatic image can be sent to the AFIDS WPS for"
        print "coregistration with your first image. These First and Second Quickbird"
        print "images are referred to as Master and Secondary. If desired, multiple"
        print "Secondary images can be registered to the Master to develop a"
        print "time-series of registered Quickbird imagery.</p>"

        print "<p>Master - Register your Quickbird Panchromatic image to an"
        print "orthorectified base image.</p>"
        print "<p>Secondary - Register a second date Quickbird Panchromatic image to the"
        print "Master Quickbird.</p>"
        print "<p>Additional Band - Apply the registration (grid) from the Master or"
        print "Secondary Quickbird to its Multispectral images, and resize the"
        print "Multispectral bands to match the Panchromatic pixel scale.</p>"

        print "</div>"

        # Quickbird Master
        print "<div id=\"quickbirdmaster\" style=\"display:none\">"
        print "<h2>Quickbird Master Coregistration</h2>"

        print "<p>This process coregisters a DigitalGlobe(c) Quickbird panchromatic"
        print "image to a precalculated orthorectified base image.</p>"

        print "<p>Supported Quickbird format types:<br>"
        print "Quickbird \"Basic\" Panchromatic and Multispectral Imagery in "
        print "NITF (.NTF) format (not TIFF). The"
        print "NITF version uses metadata and rpc parameters that are stored in the"
        print "NITF file, but a dummy metadata file must be provided to the metadata dialog box."
        print "All outputs are in NITF format. The Quickbird Pan channel is 0.61m resolution, and the"
        print "Multispectral bands (blu, grn, red, nir) are 2.44m resolution. The Pan"
        print "channel is used as the \"Master\" for both Panchromatic AND"
        print "Multispectral data that are being registered.</p>"

        print "<form action=\"coregisterQuickbirdMasterForm.py\" method=\"post\" enctype=\"multipart/form-data\">"

        print "<table>"
        print "<tr>"
        print "<td>Input Pan image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input metadata text file</td> <td><input type=\"file\" name=\"rawmeta\" size=\"75\" /></td>"
        print "</tr>"

        # previous metadata
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousmeta\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. QB1_pan_master"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"
        print "</div>"

        # Quickbird Secondary
        print "<div id=\"quickbirdsecondary\" style=\"display:none\">"
        print "<h2>Quickbird Secondary Coregistration</h2>"

        print "<p>This process coregisters a second DigitalGlobe(c) Quickbird"
        print "panchromatic image to the precalculated orthorectified Master"
        print "Quickbird image.</p>"

        print "<form action=\"coregisterQuickbirdSecondaryForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input secondary QB image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input secondary metadata text file</td> <td><input type=\"file\" name=\"rawmeta\" size=\"75\" /></td>"
        print "</tr>"

        # previous metadata
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousmeta\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input master QB image (.img) file</td> <td>"

        print "<select name=\"refimg\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. QB2_pan_second"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"

        # Quickbird Additional Band
        print "<div id=\"quickbirdadditional\" style=\"display:none\">"
        print "<h2>Quickbird Additional Band Coregistration</h2>"

        print "<p>This process coregisters the Quickbird Multispectral bands with the"
        print "previously coregistered Master or Secondary Panchromatic image (in"
        print "VICAR format).</p>"

        print "<form action=\"coregisterQuickbirdAdditionalForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input MSI file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input metadata text file</td> <td><input type=\"file\" name=\"rawmeta\" size=\"75\" /></td>"
        print "</tr>"

        # previous metadata
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousmeta\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input band</td> <td>"
        print "<select name=\"band\">"
        print "<option value =\"1\" selected=\"selected\">1</option>"
        print "<option value =\"2\">2</option>"
        print "<option value =\"3\">3</option>"
        print "<option value =\"4\">4</option>"
        print "</select>"
        print "</td>"
        print "</tr>"

        print "<tr>"
        print "<td>Input Pan image (.img) file</td> <td>"

        print "<select name=\"fnamein\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Input Pan grid (.grid) file</td> <td>"

        print "<select name=\"gridtar\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.grid")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. QB1_master_b1 or QB2_second_b1"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"
        print "</div>"
        
        # NTM
        print "<div id=\"ntm\" style=\"display:none\">"
        print "<div id=\"ntmconcept\">"
        print "<h2>NTM Coregistration Concept of Operation</h2>"

        print "<p>The AFIDS WPS registers your NTM/NITF image to an"
        print "orthorectified base image with elevation corrections, and"
        print "returns to you: 1) Your original full-scene input NITF image"
        print "with improved RPC georeference values; 2) An"
        print "orthorectified full-scene NITF image in Platte Caree projection"
        print "with new RPC georeference parameters; and 3) A log file. Your"
        print "original/raw image with the updated RPCs will have the suffix"
        print "\"_updatedRPC.\" Your new orthorectified image will have the"
        print "suffix \"_master,\" which continues the AFIDS concept that the"
        print "first orthorectified image bears the reference name \"master.\"</p>"
        print "</div>"

        # NTM Process
        print "<div id=\"ntmprocess\" style=\"display:none\">"
        print "<h2>NTM Process</h2>"

        print "<form action=\"coregisterNtmMasterForm.py\" method=\"post\" enctype=\"multipart/form-data\">"

        print "<table>"
        print "<tr>"
        print "<td>Input NTM image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Output pixel size (meters)</td> <td><input type=\"text\" name=\"pixelsize\" size=\"30\" /> e.g. 1.0</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /> e.g. NTM1</td>"
        print "</tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"
        print "</div>"
        print "</div>"
        
        # ASTER
        print "<div id=\"aster\">"
        print "<div id=\"asterconcept\">"
        print "<h2>ASTER Coregistration Concept of Operation</h2>"

        print "<p>"
        print "The AFIDS WPS registers your ASTER VNIR (15-meter) band 2 image "
        print "to an orthorectified base image, and returns to you "
        print "the registered image in GeoTIFF format, plus registration "
        print "accuracy information and a log file. You may then optionally "
        print "upload the remaining ASTER bands to have them registered as "
        print "well (using the same grid so everything matches "
        print "exactly). After the first registration, a second date ASTER "
        print "image can be optionally sent to the AFIDS WPS for "
        print "coregistration with your first image. These First and Second "
        print "ASTER images are referred to as Master and Secondary. If "
        print "desired, multiple Secondary images can be registered to the "
        print "Master to develop a time-series of registered ASTER imagery. "
        print "</p>"

        print "<p>"
        print "Master - Register your band2 ASTER to an orthorectified base image. "
        print "</p>"

        print "<p>"
        print "Secondary - Register a second date band2 ASTER to the master band2 ASTER. "
        print "</p>"

        print "<p>"
        print "Additional Band - Apply the registration (grid) from the "
        print "master or secondary ASTER to its' other bands. ASTER SWIR and "
        print "TIR images are rescaled to match the VNIR 15-meter pixel size. "
        print "</p>"
        print "</div>"

        # ASTER Master
        print "<div id=\"astermaster\" style=\"display:none\">"
        print "<h2>ASTER Master Coregistration</h2>"

        print "<p>This process coregisters band 2 of an ASTER image set to a precalculated orthorectified base image. </p>"

        print "<form action=\"coregisterAsterMasterForm.py\" method=\"post\" enctype=\"multipart/form-data\">"

        print "<table>"
        print "<tr>"
        print "<td>Input image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. ASTER_master"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"
        print "</div>"

        # ASTER Secondary
        print "<div id=\"astersecondary\" style=\"display:none\">"
        print "<h2>ASTER Secondary Coregistration</h2>"

        print "<p>This process coregisters band 2 of an ASTER image set to the precalculated orthorectified Master ASTER band 2 image. </p>"

        print "<form action=\"coregisterAsterSecondaryForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input secondary ASTER image file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"


        print "<tr>"
        print "<td>Input master ASTER image (.img) file</td> <td>"

        print "<select name=\"refimg\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. ASTER_second"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"

        # ASTER Additional Band
        print "<div id=\"asteradditional\" style=\"display:none\">"
        print "<h2>ASTER Additional Band Coregistration</h2>"

        print "<p>This process coregisters the Master's or Secondary's"
        print "additional bands (bands 1,3-14) with the previously "
        print "coregistered band 2 of the same ASTER image. All additional "
        print "bands are rescaled to match the VNIR 15-meter pixel size. TIR "
        print "images have 16-bit pixels, which may not be displayable by some browsers or image viewers.</p>"
        
        print "<form action=\"coregisterAsterAdditionalForm.py\" method=\"post\" enctype=\"multipart/form-data\">"
        print "<table>"
        print "<tr>"
        print "<td>Input file</td> <td><input type=\"file\" name=\"rawimg\" size=\"75\" /></td>"
        print "</tr>"

        # previous image menu
        print "<tr>"
        print "<td></td> <td>"

        print "<select name=\"previousimage\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/upload"

        fileList = glob.glob(userData + "/*")

        print "<option value =\"-\" selected=\"selected\">Use entry above</option>"

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:
              fileName = os.path.basename(path[1])
              print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        print "</select>"

        print "</tr>"
        print "<tr>"



        print "<tr>"
        print "<td>Input band</td> <td>"
        print "<select name=\"band\">"
        print "<option value =\"1\" selected=\"selected\">1 (VNIR)</option>"
        print "<option value =\"3N\">3N (VNIR)</option>"
        print "<option value =\"4\">4 (SWIR)</option>"
        print "<option value =\"5\">5 (SWIR)</option>"
        print "<option value =\"6\">6 (SWIR)</option>"
        print "<option value =\"7\">7 (SWIR)</option>"
        print "<option value =\"8\">8 (SWIR)</option>"
        print "<option value =\"9\">9 (SWIR)</option>"
        print "<option value =\"10\">10 (TIR)</option>"
        print "<option value =\"11\">11 (TIR)</option>"
        print "<option value =\"12\">12 (TIR)</option>"
        print "<option value =\"13\">13 (TIR)</option>"
        print "<option value =\"14\">14 (TIR)</option>"
        print "</select>"
        print "</td>"
        print "</tr>"

        print "<tr>"
        print "<td>Input band 2 image (.img) file</td> <td>"

        print "<select name=\"fnamein\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.img")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Input band 2 grid (.grid) file</td> <td>"

        print "<select name=\"gridtar\">"
        userData = "/usr/share/tomcat5/webapps/ROOT/users/" + userDir + "/wpsoutput"

        fileList = glob.glob(userData + "/*.grid")

        if len(fileList):
           timeFileList = []
           
           for path in fileList:

              stats = os.stat(path)
              lastAccTime = time.localtime(stats[7])
              timeFileTuple = lastAccTime, path
              timeFileList.append(timeFileTuple)

           timeFileList.sort()
           timeFileList.reverse()

           for path in timeFileList:

              fileName = os.path.basename(path[1])
              if path == timeFileList[0]:
                 print "<option value =\"" + fileName + "\" selected=\"selected\">" + fileName + "</option>"
              else:
                 print "<option value =\"" + fileName + "\">" + fileName + "</option>"
              
        else:
           print "<option value =\"-\">None Available</option>"
        print "</select>"

        print "</td>"
        print "</tr>"
        print "<tr>"
        print "<td>Output file prefix</td> <td><input type=\"text\" name=\"prefix\" size=\"30\" /></td>"
        print "</tr>"
        print "<tr><td></td><td>"
        print "e.g. ASTER_master_b1 or ASTER_second_b1"
        print "</td></tr>"
        print "</table>"
        print "<input type=\"submit\" value=\"Execute\" />"
        print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\" />"
        print "</form>"

        print "</div>"
        print "</div>"
        
        # User Files
        print "<h2>User Files</h2> (Mouse Right-Click to View or Copy) <br />\n"

        sc = StatusChecker.StatusChecker()

        userData = sc.usersDir + "/" + userDir + "/upload"
        fileList = glob.glob(userData + "/*")
        if len(fileList):
           print "<p>Previous Input: <a href=\"/cgi-bin/deleteAllIn.py?dir=" + userDir + "\">[Delete All]</a><br />\n"
           for path in fileList:
              fileName = os.path.basename(path)
              url = sc.usersUrl + "/" + userDir + "/upload/" + fileName
              print "<a href=\"/cgi-bin/deleteOneIn.py?target=" + fileName + "&dir=" + userDir + "\">[Delete]</a> <a href=\"" + url + "\">" + fileName + "</a><br />\n"
           print "</p>\n"
        else:
           print "<p>Previous Input: None</p>\n"

        userData = sc.usersDir + "/" + userDir + "/wpsoutput"
        fileList = glob.glob(userData + "/*.txt" )
        fileList.extend( glob.glob(userData + "/*.nitf" ) )
        fileList.extend( glob.glob(userData + "/*.tiff" ) )
        fileList.extend( glob.glob(userData + "/*.tif" ) )
        fileList.extend( glob.glob(userData + "/*.log" ) )
        fileList.sort()
        
        if len(fileList):
           print "<p>Previous Output: <a href=\"/cgi-bin/deleteAllOut.py?dir=" + userDir + "\">[Delete All]</a><br />\n"
           for path in fileList:
              fileName = os.path.basename(path)
              url = sc.usersUrl + "/" + userDir + "/wpsoutput/" + fileName
              print "<a href=\"/cgi-bin/deleteOne.py?target=" + fileName + "&dir=" + userDir + "\">[Delete]</a> <a href=\"" + url + "\">" + fileName + "</a><br />\n"
           print "</p>\n"
        else:
           print "<p>Previous Output: None</p>\n"

    print "</body></html>"

    return

if __name__ == "__main__":
   try:
      main()
   except:
      print "Content-type: text/html\n\n"
      print "<html><head>"
      print "<title>Exception Traceback</title>"
      print "</head><body>"
      print "An unhandled exception occurred, traceback:"
      print "<pre>"
      traceback.print_exc(file=sys.stdout)
      print "</pre></body></html>"
