#!/usr/bin/python
"""
"""

import os,time,string,sys,glob
from stat import *
import pywps
from pywps.etc import settings
from pywps.carto import StatusChecker
from pywps.Wps import wpsexceptions

class Process:

    def __init__(self):
        # Identifier - name of this process
        self.Identifier = "coregisterquickbirdadditional"

        # processVersion - version of this process
        self.processVersion = "0.1"

        # Title - title for this process
        self.Title="Quickbird coregistration (additional band)"
        self.Abstract="This process coregisters a band with a previously coregistered band of the same Quickbird image."

        self.Inputs = [
                    {
                        'Identifier':'rawimg',
                        'Title': 'Input Quickbird NITF image',
                        'ComplexValueReference': {
                            'Formats': [ "application/octet-stream" ],
                        },
                    },
                    {
                        'Identifier':'rawmeta',
                        'Title': 'Input Quickbird image metadata text file',
                        'ComplexValueReference': {
                            'Formats': [ "text/plain" ]
                        },
                    },
                    {
                        'Identifier':'fnamein',
                        'Title': 'Input Quickbird Master/Secondary VICAR image',
                        'ComplexValueReference': {
                            'Formats': [ "application/octet-stream" ]
                        },
                    },
                    {
                         'Identifier': 'band',
                         'Title': 'Input image band number',
                         'LiteralValue': {'values':["*"]},
                         'value':"1",
                    },
                    {
                        'Identifier':'gridtar',
                        'Title': 'Input Quickbird Master/Secondary VICAR image grid',
                        'ComplexValueReference': {
                            'Formats': [ "application/octet-stream" ]
                        },
                    },
                    {
                         'Identifier': 'prefix',
                         'Title': 'Output filename prefix',
                         'LiteralValue': {'values':["*"]},
                         'value':"QuickbirdMaster",
                    },
                    {
                         'Identifier': 'userdir',
                         'Title': 'User directory',
                         'LiteralValue': {'values':["*"]},
                         'value':"UserDirectory",
                     },

                ]
        
        #
        # Output
        #
        self.Outputs = [
            {
                'Identifier': 'outnitf',
                'Title': 'Output coregistered image (NITF)',
                'ComplexValueReference': {
                    'Formats': [ "application/octet-stream" ],
                },
            },

            {
                'Identifier': 'log',
                'Title': 'Processing output log',
                'ComplexValueReference': {
                    'Formats':["text/plain"],
                },
            },

        ]
        
        self.storeSupported = "true"
        self.statusSupported = "true"
        
    #####################################################################
    #
    # Execute part of the process
    #
    #####################################################################
    def execute(self):
        """
        """

        sc = StatusChecker.StatusChecker()
        userDir = self.DataInputs['userdir']
        settings.ServerSettings['outputPath'] = sc.usersDir + "/" + userDir + "/wpsoutput"
        settings.ServerSettings['outputUrl'] = sc.usersUrl + "/" + userDir + "/wpsoutput"

        # if anything in the four lines above dies, the error will go to /usr/share/tomcat5/webapps/ROOT/wpsoutput/, if it exists

        pdfScript = "process.pdf"
        rawimg = self.DataInputs['rawimg']
        rawmeta = self.DataInputs['rawmeta']
        fnamein = self.DataInputs['fnamein']
        band = self.DataInputs['band']
        gridtar = self.DataInputs['gridtar']
        prefix = self.DataInputs['prefix']

        imgOut = prefix + ".img"
        #tiffOut = prefix + ".tiff"
        nitfOut = prefix + ".nitf"
        logOut = prefix + ".log"

        self.status=["Creating first pdf file.", 5]
        pdfFile = open( pdfScript, "w" )
        pdfFile.write( "procedure\n" )
        pdfFile.write( "body\n" )
        pdfFile.write( "ush cat process.pdf\n" )
        pdfFile.write( "ikqbwarpad2 key=tst +\n" )
        pdfFile.write( "rawimg=" + rawimg + " +\n" )
        pdfFile.write( "rawmeta=" + rawmeta + " +\n" )
        pdfFile.write( "band=" + band + " nitfbands=1 +\n" )
        pdfFile.write( "senstype=q +\n" )
        pdfFile.write( "fnamein=" + fnamein + " +\n" )
        pdfFile.write( "outimg=" + imgOut + "\n" )

        #pdfFile.write( "vtiff3-from INP=" + imgOut + " OUT=" + tiffOut + "\n" )
        pdfFile.write( "vicar2ntf INP=" + imgOut + " OUT=" + nitfOut + "\n" )
        pdfFile.write( "end-proc\n" )
        pdfFile.close()

        self.status=["Orthorectifying image ...", 15]

        self.Outputs[0]['value'] = nitfOut
        self.Outputs[1]['value'] = logOut

        os.system( "mkdir rawtst" )
        os.system( "mkdir scratch" )
        os.system( "mkdir finaltst" )

        os.system( "tar xf " + gridtar )

        os.system( "touch " + nitfOut + "; touch " + logOut )

        os.system( "( ( . " + sc.afidsDir + "/setup_afids_env.sh ; vicarb " + pdfScript + " ) > " + logOut + " ; touch doneFile ) &" )

        while len(glob.glob("doneFile")) == 0:
            time.sleep( 2 )
            os.system( "wc " + logOut + " > msgcnt" )
            msgFile = open( "msgcnt", "r" )
            msgCnt = msgFile.read()
            msgFile.close()
            split = msgCnt.split()
            cnt = int(split[0])
            self.status=["Coregistration progress", int(cnt * 1000.0 / 3041)/10.0]
        
        os.system( "grep TAE- " + logOut + " | wc > msgcnt" )
        msgFile = open( "msgcnt", "r" )
        msgCnt = msgFile.read()
        msgFile.close()
        split = msgCnt.split()
        cnt = int(split[0])

        if cnt > 0:
            self.status=["Failed: TAE Exception", 100]
        else:
            self.status=["Finished", 100]
        
        return
