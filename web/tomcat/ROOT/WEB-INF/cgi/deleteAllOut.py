#!/usr/bin/python
"""
Utility to delete all files from user input directory
"""

import cgi,os,traceback,glob

def getFileName(path):
   parts = path.split("/")
   return parts[len(parts)-1]

def main():
    """
    This module deletes a user file
    """

    form = cgi.FieldStorage()
    dir = getFileName(form[ 'dir' ].value)

    path = "/usr/share/tomcat5/webapps/ROOT/users/" + dir + "/wpsoutput/*"

    print "Content-type: text/html\n\n"
    print "<html><head>"
    print "<title>File Deleted</title>"
    print "<meta http-equiv=\"refresh\" content=\"1; url=/cgi-bin/coregister.py?userdir=" + dir + "&username=-&password=-\">"
    print "</head><body>"

    try:
       for file in glob.glob(path):
          os.remove(file)
    except:
        print "No such file"

    print "</body></html>"

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
      traceback.print_exc()
      print "</pre></body></html>"
