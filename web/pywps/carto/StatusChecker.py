#!/usr/bin/python

from xml.dom import minidom
import string

class StatusChecker:

   def __init__(self):
      pass

   scheme = "http"
   host = "cart01"
   port = "8080"
   afidsDir = "/usr/local/afids"
   usersDir = "/usr/share/tomcat5/webapps/ROOT/users"
   hostPort = host + ":" + port
   schemeHostPort = scheme + "://" + hostPort
   usersUrl = schemeHostPort + "/users"
   sqlSafeTrans = string.maketrans("\"\'\\","___")
   pathSafeTrans = string.maketrans("/ \"\'\\","_____")

   ###########################################################
   #
   # finds a URL based on its identifier
   #
   ###########################################################
   def getURL(self, xmlString, identifier):
      doc = minidom.parseString(xmlString)
      outputNodes = doc.getElementsByTagName('Output')

      s = identifier + "not found"
      for nodes in outputNodes:
         node = nodes.getElementsByTagName('ows:Identifier')
         if(node[0].firstChild.nodeValue.find(identifier) != -1):
            node = nodes.getElementsByTagName('ComplexValueReference')
            for subnode in node:
               s =  subnode.getAttribute("ows:reference")
               return s
      return s

   ###########################################################
   #
   # returns the status location from the given xml string
   #
   ###########################################################
   def getStatusLocation(self, xmlString):
      s = "no ExecuteResponse node in " + xmlString
      doc = minidom.parseString(xmlString)
      execNode = doc.getElementsByTagName('ExecuteResponse')
      for subNode in execNode:
         s = subNode.getAttribute('statusLocation')
      return s

   ###########################################################
   #
   # returns the status from the given xml string
   #
   ###########################################################
   def getStatus(self, xmlString):
      doc = minidom.parseString(xmlString)

      processStatusNode = doc.getElementsByTagName('ProcessStarted')
      if(len(processStatusNode) > 0):
         for node in processStatusNode:
            percentComplete = node.getAttribute("percentCompleted")
            if percentComplete == "0":
               s = "Process started"
            else:
               s = node.getAttribute("message") + " " + percentComplete + "%"
            return s

      processAcceptedNode = doc.getElementsByTagName('ProcessAccepted')
      if(len(processAcceptedNode) == 1):
         s = "Process accepted"
         return s

      processSucceededNode = doc.getElementsByTagName('ProcessSucceeded')
      if(len(processSucceededNode) == 1):
         s = "Process succeeded"
         return s

      return "running \"" + xmlString + "\""

if __name__=="__main__":
   print "\n--- Beginning testing of StatusChecker ---"
   testinp = open("testxml", 'r')
   xmlstring = ""
   for line in testinp:
      xmlstring = xmlstring + line

   #   print xmlstring
   
   statuschecker = StatusChecker()
   print statuschecker.getStatusLocation(xmlstring)
   print statuschecker.getStatus(xmlstring)
   print "--- Ending testing of StatusChecker ---\n"
