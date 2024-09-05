#!/usr/bin/python 
#-*- coding: utf-8 -*-
"""
This program generates an OGS WPS query for NTM master image coregistration.
"""

from httplib import HTTPConnection
from xml.dom import minidom
import cgi
import glob
import os
import shutil
import string
import sys
import tempfile
import time
import traceback
import pywps
from pywps import carto
from pywps.carto import StatusChecker

def main():
    """
    This main function generates and returns the OGS WPS query link.
    """

    print "Content-type: text/html\n"
    print "<html><head><title>NTM Master Coregistration</title>"
    print "</head><body>"

    sc = StatusChecker.StatusChecker()

    form = cgi.FieldStorage()

    rawimg = form[ 'rawimg' ]
    previmg = form[ 'previousimage' ].value
    prefix = form[ 'prefix' ].value.translate(sc.pathSafeTrans)
    pixelsize = form[ 'pixelsize' ].value
    userDir = os.path.basename(form[ 'userdir' ].value).translate(sc.pathSafeTrans)
    uploadDir = sc.usersDir + "/" + userDir + "/upload"
    uploadDirUrl = sc.usersUrl + "/" + userDir + "/upload"

    if previmg == "-":
        if rawimg.file:
            rawimgname = rawimg.filename
            rawimgPath = uploadDir + "/" + rawimgname
            data = rawimg.file.read()
            uploadFile = open( rawimgPath, "w" )
            uploadFile.write( data )
            uploadFile.close()
        else:
            rawimgname = "No_Input_Image_Name"
    else:
        rawimgname = os.path.basename(previmg).translate(sc.pathSafeTrans)

    url = sc.schemeHostPort + "/cgi-bin/wps.py?service=Wps&version=0.4.0&request=execute&identifier=coregisterntmmaster&datainputs=prefix," + prefix + \
          ",rawimg," + uploadDirUrl + "/" + rawimgname + \
          ",pixelsize," + pixelsize + \
          ",userdir," + userDir + \
          "&store=true&status=true"

    conn = HTTPConnection(sc.hostPort)
    conn.request("GET", url)

    r1 = conn.getresponse()

    data1 = r1.read()

    if data1 == "":
       print "No data from WPS"
       print "</body></html>"
       return

    if data1.find("ServerBusy") != -1:
        print "Server Busy:"
        print "<xmp>" + data1 + "</xmp>"
        print "</body></html>"
        return

    if data1.find("ExceptionReport") != -1:
        print "Unexpected exception:"
        print "<xmp>" + data1 + "</xmp>"
        print "</body></html>"
        return        

    statusLoc = sc.getStatusLocation(data1)
    oldStatus = ""
    status = sc.getStatus(data1)

    while(1):
        time.sleep( 2 )
        if status != oldStatus:
           print "<br> ", status
           sys.stdout.flush()
           oldStatus = status
        conn.request("GET", statusLoc)
        r1 = conn.getresponse()
        data1 = r1.read()

        if data1 == "":
           print "No data from WPS with URL<br>\"" + statusLoc + "\""
           print "</body></html>"
           return

        status = sc.getStatus(data1)

        if status == "Process succeeded":
           break

    conn.request("GET", statusLoc)
    r1 = conn.getresponse()
    data1 = r1.read()

    outprodUrl = sc.getURL(data1, "outprod")
    inpupdatedUrl = sc.getURL(data1, "inpupdated")
    logUrl = sc.getURL(data1, "log")

    wpsoutput = sc.usersDir + "/" + userDir + "/wpsoutput"
    file = open(wpsoutput + "/" + prefix + "_query.txt", "w")

    file.write("WPS query: " + url + "\n\n")
    file.write(data1)
    file.close()

    conn.close()

    badFile = 0

    print "<h2>Orthorectification output products</h2>"
    print "Click on a product to download it to your Desktop. Some products may require a Right-Click \"Save link As\"."
    print "<table border=\"1\">"
    print "<tr><td>"

    statinfo = os.stat( wpsoutput + "/" + os.path.basename(outprodUrl) )
    if ( statinfo.st_size == 0 ):
        print "<font color=\"red\">Processing Failure</font>"
        badFile = 1
    else:
        print "<a href=\"" + outprodUrl + "\">" + os.path.basename(outprodUrl) + "</a>"

    print "</td><td>Orthorectified output image"
    print "</td></tr>"

    print "<tr><td>"

    statinfo = os.stat( wpsoutput + "/" + os.path.basename(inpupdatedUrl) )
    if ( statinfo.st_size == 0 ):
        print "<font color=\"red\">Processing Failure</font>"
        badFile = 1
    else:
        print "<a href=\"" + inpupdatedUrl + "\">" + os.path.basename(inpupdatedUrl) + "</a>"

    print "</td><td>Input image with updated RPCs"
    print "</td></tr>"

    print "<tr><td>"

    statinfo = os.stat( wpsoutput + "/" + os.path.basename(logUrl) )
    if ( statinfo.st_size == 0 ):
        print "<font color=\"red\">Processing Failure</font>"
        badFile = 1
    else:
        print "<a href=\"" + logUrl + "\">" + os.path.basename(logUrl) + "</a>"

    print "</td><td>Processing log"
    print "</td></tr>"
    print "<tr><td>"

    statinfo = os.stat( wpsoutput + "/" + prefix + "_query.txt" )
    if ( statinfo.st_size == 0 ):
        print "<font color=\"red\">Processing Failure</font>"
        badFile = 1
    else:
        print "<a href=\"" + sc.usersUrl + "/" + userDir + "/wpsoutput/" + prefix + "_query.txt\">" + prefix + \
              "_query.txt" + "</a>"

    print "</td><td>OGC WPS query response"
    print "</td></tr>"
    print "</table>"

    if ( badFile == 1 ):
        print "<font color=\"red\">See " + \
              "<a href=\"" + logUrl + "\">" + os.path.basename(logUrl) + "</a>" + \
              " for explanation of processing failures.</font>"

    print "<br />"
    print "<form action=\"coregister.py\" method=\"post\">"
    print "<input type=\"hidden\" name=\"username\" value=\"-\">"
    print "<input type=\"hidden\" name=\"password\" value=\"-\">"
    print "<input type=\"hidden\" name=\"userdir\" value=\"" + userDir + "\">"
    print "<input type=\"submit\" value=\"Back\" />"
    print "</form>"

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
