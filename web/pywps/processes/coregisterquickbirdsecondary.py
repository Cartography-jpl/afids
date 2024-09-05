#!/usr/bin/python
"""
Quickbird secondary coregistration
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
        self.Identifier = "coregisterquickbirdsecondary"

        # processVersion - version of this process
        self.processVersion = "0.1"

        # Title - title for this process
        self.Title="Quickbird coregistration (secondary)"
        self.Abstract="This process coregisters a Quickbird image to a previously orthorectified master image."

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
                         'Identifier': 'prefix',
                         'Title': 'Output filename prefix',
                         'LiteralValue': {'values':["*"]},
                         'value':"QuickbirdMaster",
                     },
                    {
                        'Identifier':'refimg',
                        'Title': 'Input Quickbird Master VICAR image',
                        'ComplexValueReference': {
                            'Formats': [ "application/octet-stream" ],
                        },
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

            {
                'Identifier': 'outgrid',
                'Title': 'Output image grid',
                'Abstract': 'Used for additional band registration',
                'ComplexValueReference': {
                    'Formats':["image/tiff"],
                },
            },

            {
                'Identifier': 'outvicar',
                'Title': 'Output coregistered image (VICAR)',
                'Abstract': 'Used to coregister band 3 of another image to this image',
                'ComplexValueReference': {
                    'Formats': [ "application/octet-stream" ],
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
            1) Creates a pdf to run the secondary coregistration
            2) Runs the pdf
        """

        sc = StatusChecker.StatusChecker()
        userDir = self.DataInputs['userdir']
        settings.ServerSettings['outputPath'] = sc.usersDir + "/" + userDir + "/wpsoutput"
        settings.ServerSettings['outputUrl'] = sc.usersUrl + "/" + userDir + "/wpsoutput"

        # if anything in the four lines above dies, the error will go to /usr/share/tomcat5/webapps/ROOT/wpsoutput/, if it exists

        pdfScript = "process.pdf"
        rawimg = self.DataInputs['rawimg']
        rawmeta = self.DataInputs['rawmeta']
        prefix = self.DataInputs['prefix']
        refimg = self.DataInputs['refimg']

        imgOut = prefix + ".img"
        #tiffOut = prefix + ".tiff"
        nitfOut = prefix + ".nitf"
        logOut = prefix + ".log"
        gridOut = prefix + ".grid"

        self.status=["Creating first pdf file.", 5]
        pdfFile = open( pdfScript, "w" )
        pdfFile.write( "procedure\n" )
        pdfFile.write( "local ftypi integer\n" )
        pdfFile.write( "local ftyps string\n" )
        pdfFile.write( "body\n" )
        pdfFile.write( "ush cat process.pdf\n" )
        pdfFile.write( "file2tcl inp=" + rawimg + " ftype=ftypi\n" )
        pdfFile.write( "if (ftypi=2)\n" )
        pdfFile.write( "let ftyps = \"tif\"\n" )
        pdfFile.write( "else\n" )
        pdfFile.write( "let ftyps = \"nitf\"\n" )
        pdfFile.write( "end-if\n" )
        pdfFile.write( "ikqbcall2 key=tst +\n" )
        pdfFile.write( "rawimg=" + rawimg + " +\n" )
        pdfFile.write( "rawmeta=" + rawmeta + " +\n" )
        pdfFile.write( "rawhdr=\"\" +\n" )
        pdfFile.write( "outimg=" + imgOut + " +\n" )
        pdfFile.write( "nah=0 nav=0 rtype=secondary +\n" )
        pdfFile.write( "dted=\"\" +\n" )
        pdfFile.write( "base=\"\" rawtype=&ftyps senstype=q +\n" )
        pdfFile.write( "mpix=0.0 mpixf=0.0 +\n" )
        pdfFile.write( "maptype=pc +\n" )
        pdfFile.write( "xvdonly=n +\n" )
        pdfFile.write( "outrpc=\"\" outrpctp=\"ntf\" +\n" )
        pdfFile.write( "refimg=" + refimg + " +\n" )
        pdfFile.write( "usermapref=\"ignore\" interp=noin rastype=area +\n" )
        pdfFile.write( "siteref=\"\" siteout=\"\" +\n" )
        pdfFile.write( "line_upper=\"\" line_lower=\"\" samp_left=\"\" samp_right=\"\"\n" )

        #pdfFile.write( "vtiff3-from INP=" + imgOut + " OUT=" + tiffOut + "\n" )
        pdfFile.write( "vicar2ntf INP=" + imgOut + " OUT=" + nitfOut + "\n" )
        pdfFile.write( "end-proc\n" )
        pdfFile.close()

        self.status=["Orthorectifying image ...", 0]
        #os.system( "cp " + pdfScript + " /tmp" )

        self.Outputs[0]['value'] = nitfOut
        self.Outputs[1]['value'] = logOut
        self.Outputs[2]['value'] = gridOut
        self.Outputs[3]['value'] = imgOut

        os.system( "mkdir basemos" )
        os.system( "mkdir dtedmos" )
        os.system( "mkdir rawtst" )
        os.system( "mkdir scratch" )
        os.system( "mkdir finaltst" )

        os.system( "touch " + nitfOut + "; touch " + logOut + "; touch " + gridOut + "; touch " + imgOut )

        os.system( "( ( . " + sc.afidsDir + "/setup_afids_env.sh ; vicarb " + pdfScript + " ) > " + logOut + " ; touch doneFile ) &" )

        while len(glob.glob("doneFile")) == 0:
            time.sleep( 5 )
            os.system( "wc " + logOut + " > msgcnt" )
            msgFile = open( "msgcnt", "r" )
            msgCnt = msgFile.read()
            msgFile.close()
            split = msgCnt.split()
            cnt = int(split[0])
            self.status=["Coregistration progress", int(cnt * 100000.0 / 7796)/1000.0]
        
        os.system( "tar cf " + gridOut + " finaltst" );

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
