#!/usr/bin/python
"""
NTM master coregistration
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
        self.Identifier = "coregisterntmmaster"

        # processVersion - version of this process
        self.processVersion = "0.1"

        # Title - title for this process
        self.Title="NTM coregistration (master)"
        self.Abstract="This process coregisters a NTM image."

        self.Inputs = [
                     {
                        'Identifier':'rawimg',
                        'Title': 'Input NTM image',
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
                         'Identifier': 'pixelsize',
                         'Title': 'Processing pixel size (m)',
                         'LiteralValue': {'values':["*"]},
                         'value':"1.0",
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
                'Identifier': 'outprod',
                'Title': 'Output orthorectified image (NITF)',
                'ComplexValueReference': {
                    'Formats':["application/octet-stream"],
                },
            },

            {
                'Identifier': 'inpupdated',
                'Title': 'Input image with updated RPCs (NITF)',
                'ComplexValueReference': {
                    'Formats':["application/octet-stream"],
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
        This function
            1) Creates a pdf to run the master coregistration
            2) Runs the pdf
        """

        sc = StatusChecker.StatusChecker()
        userDir = self.DataInputs['userdir']
        settings.ServerSettings['outputPath'] = sc.usersDir + "/" + userDir + "/wpsoutput"
        settings.ServerSettings['outputUrl'] = sc.schemeHostPort + "/users/" + userDir + "/wpsoutput"

        # if anything in the four lines above dies, the error will go to /usr/share/tomcat5/webapps/ROOT/wpsoutput/, if it exists

        pdfScript = "process.pdf"
        rawimg = self.DataInputs['rawimg']
        prefix = self.DataInputs['prefix']
        pixelSize = self.DataInputs['pixelsize']
        imgOut = prefix + ".img"
        nitfOut = prefix + "_master.nitf"
        logOut = prefix + ".log"
        inpUpdated = prefix + "_updatedRPC.nitf"

        self.status=["Creating pdf file.", 5]
        pdfFile = open( pdfScript, "w" )
        pdfFile.write( "procedure\n" )
        pdfFile.write( "body\n" )
        pdfFile.write( "ush cat process.pdf\n" )
        pdfFile.write( "ikqbcall2 key=tst +\n" )
        
        pdfFile.write( "rawimg=" + rawimg + " +\n" )
        pdfFile.write( "rawmeta=\"\" +\n" )
        pdfFile.write( "rawhdr=\"\" +\n" )
        pdfFile.write( "outimg=" + imgOut + " +\n" )
        pdfFile.write( "nah=950 nav=950 rtype=master +\n" )
        pdfFile.write( "dted=\"\" +\n" )
        pdfFile.write( "base=\"\" rawtype=nitf senstype=n +\n" )
        pdfFile.write( "mpix=" + pixelSize + " +\n" )
        pdfFile.write( "maptype=pc xvdonly=n +\n" )
        pdfFile.write( "outrpc=\"" + inpUpdated + "\" outrpctp=\"ntf\" +\n" )
        pdfFile.write( "usermapref=\"\" interp=bilin rastype=area +\n" )
        pdfFile.write( "siteref=\"\" siteout=\"\" +\n" )
        pdfFile.write( "line_upper=\"\" line_lower=\"\" samp_left=\"\" samp_right=\"\"\n" )

        pdfFile.write( "vicar2ntf INP=" + imgOut + " OUT=" + nitfOut + "\n" )
        pdfFile.write( "end-proc\n" )
        pdfFile.close()

        self.status=["Orthorectifying image ...", 0]
        #os.system( "cp " + pdfScript + " /tmp" )

        self.Outputs[0]['value'] = nitfOut
        self.Outputs[1]['value'] = inpUpdated
        self.Outputs[2]['value'] = logOut

        os.system( "mkdir basemos" )
        os.system( "mkdir dtedmos" )
        os.system( "mkdir rawtst" )
        os.system( "mkdir scratch" )
        os.system( "mkdir finaltst" )

        os.system( "touch " + nitfOut + "; touch " + logOut + "; touch " + inpUpdated + "; touch " + imgOut )

        os.system( "( ( . " + sc.afidsDir + "/setup_afids_env.sh ; vicarb " + pdfScript + " ) > " + logOut + " ; touch doneFile ) &" )

        while len(glob.glob("doneFile")) == 0:
            time.sleep( 5 )
            os.system( "wc " + logOut + " > msgcnt" )
            msgFile = open( "msgcnt", "r" )
            msgCnt = msgFile.read()
            msgFile.close()
            split = msgCnt.split()
            cnt = int(split[0])
            self.status=["Coregistration progress", int(cnt * 100000.0 / 7632)/1000.0]

        # check for errors in case nobody cares
        os.system( "grep TAE- " + logOut + " | wc > msgcnt" )
        msgFile = open( "msgcnt", "r" )
        msgCnt = msgFile.read()
        msgFile.close()
        split = msgCnt.split()
        cnt = int(split[0])

        if cnt > 0:
            self.status=["Finished with TAE Exception", 100]
        else:
            self.status=["Finished", 100]
        
        return
