#!/usr/bin/python
"""
Landsat master coregistration
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
        self.Identifier = "coregisterlandsatmaster"

        # processVersion - version of this process
        self.processVersion = "0.1"

        # Title - title for this process
        self.Title="Landsat coregistration (master)"
        self.Abstract="This process coregisters band 3 of a Landsat image (in GeoTIFF format) to a precalculated \
        orthorectified base image. The generated image can be used to calculate the coregistration of additional \
        bands of the same image, or any band of another Landsat image."

        self.Inputs = [
                    {
                        'Identifier':'rawimg',
                        'Title': 'Input Landsat GeoTIFF image',
                        'ComplexValueReference': {
                            'Formats': [ "image/tiff" ]
                        },
                    },
                    {
                        'Identifier':'rawmeta',
                        'Title': 'Input Landsat image metadata text file',
                        'ComplexValueReference': {
                            'Formats': [ "text/plain" ]
                        },
                    },
                     {
                         'Identifier': 'prefix',
                         'Title': 'Output filename prefix',
                         'LiteralValue': {'values':["*"]},
                         'value':"LandsatMaster",
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
                'Identifier': 'outtiff',
                'Title': 'Output coregistered image (GeoTIFF)',
                'ComplexValueReference': {
                    'Formats':["image/tiff"],
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
                    'Formats':["application/octet-stream"],
                },
            },

            {
                'Identifier': 'geoacc',
                'Title': 'Output geo accuracy statistics',
                'Abstract': 'Output geo accuracy statistics',
                'ComplexValueReference': {
                    'Formats':["application/octet-stream"],
                },
            },

            {
                'Identifier': 'outplot',
                'Title': 'Output accuracy plot',
                'Abstract': 'Output accuracy plot',
                'ComplexValueReference': {
                    'Formats':["application/octet-stream"],
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
        rawmeta = self.DataInputs['rawmeta']
        prefix = self.DataInputs['prefix']
        imgOut = prefix + ".img"
        tiffOut = prefix + ".tiff"
        logOut = prefix + ".log"
        gridOut = prefix + ".grid"
        geoacc = prefix + "_GeoAcc.txt"
        outplot = prefix + "_OUTPLOT.tiff"

        self.status=["Creating first pdf file.", 5]
        pdfFile = open( pdfScript, "w" )
        pdfFile.write( "procedure\n" )
        pdfFile.write( "body\n" )
        pdfFile.write( "ush cat process.pdf\n" )
        pdfFile.write( "lscall2 key=tst +\n" )
        
        pdfFile.write( "rawimg=" + rawimg + " +\n" )
        pdfFile.write( "rawmeta=" + rawmeta + " +\n" )
        pdfFile.write( "outimg=" + imgOut + " +\n" )
        pdfFile.write( "nah=500 nav=500 rtype=master +\n" )
        pdfFile.write( "dted=\"\" +\n" )
        pdfFile.write( "base=\"\" +\n" )
        pdfFile.write( "maptype=pc xvdonly=n +\n" )
        pdfFile.write( "usermapref=\"\" interp=bilin rastype=area +\n" )
        pdfFile.write( "siteref=\"\" siteout=\"\" +\n" )
        pdfFile.write( "line_upper=\"\" line_lower=\"\" samp_left=\"\" samp_right=\"\"\n" )

        pdfFile.write( "vtiff3-from INP=" + imgOut + " OUT=" + tiffOut + "\n" )
        pdfFile.write( "end-proc\n" )
        pdfFile.close()

        self.status=["Orthorectifying image ...", 0]
        #os.system( "cp " + pdfScript + " /tmp" )

        self.Outputs[0]['value'] = tiffOut
        self.Outputs[1]['value'] = logOut
        self.Outputs[2]['value'] = gridOut
        self.Outputs[3]['value'] = imgOut
        self.Outputs[4]['value'] = geoacc
        self.Outputs[5]['value'] = outplot

        os.system( "mkdir basemos" )
        os.system( "mkdir dtedmos" )
        os.system( "mkdir rawtst" )
        os.system( "mkdir scratch" )
        os.system( "mkdir finaltst" )

        os.system( "touch " + tiffOut + "; touch " + logOut + "; touch " + gridOut + "; touch " + imgOut )

        os.system( "( ( . " + sc.afidsDir + "/setup_afids_env.sh ; vicarb " + pdfScript + " ) > " + logOut + " ; touch doneFile ) &" )

        while len(glob.glob("doneFile")) == 0:
            time.sleep( 2 )
            os.system( "wc " + logOut + " > msgcnt" )
            msgFile = open( "msgcnt", "r" )
            msgCnt = msgFile.read()
            msgFile.close()
            split = msgCnt.split()
            cnt = int(split[0])
            self.status=["Coregistration progress", int(cnt * 1000.0 / 19112)/10.0]

        os.system( "tar cf " + gridOut + " finaltst" )

        # get dtedmos name
        os.system( "ls dtedmos > dtedmos.lst" )
        dtedFile = open( "dtedmos.lst", "r" )
        dtedName = dtedFile.read()
        dtedName = dtedName.rstrip()
        dtedFile.close()

        # get basemos name
        os.system( "ls basemos > basemos.lst" )
        baseFile = open( "basemos.lst", "r" )
        baseName = baseFile.read()
        baseName = baseName.rstrip()
        baseFile.close()

        self.status=["Creating second pdf file.", 45]
        
        pdfScript = "process2.pdf"

        pdfFile = open( pdfScript, "w" )
        pdfFile.write( "procedure\n" )
        pdfFile.write( "body\n" )
        pdfFile.write( "ush cat process2.pdf\n" )
        pdfFile.write( "lsat12_geo dir1=./  inp1=" + imgOut + " +\n" )
        pdfFile.write( "dir2=basemos/ inp2=" + baseName + " +\n" )
        pdfFile.write( "elev=dtedmos/" + dtedName + " +\n" )
        pdfFile.write( "prefx=\"" + prefix + "\" +\n" )
        pdfFile.write( "wsize=2048 ffts=128 magn=1.0 redow=\"n\"\n" )
        #         pdfFile.write( "lsat12_geo dir1=./  inp1=" + imgOut + " +\n" )
        #         pdfFile.write( "dir2=basemos/ inp2=" + baseName + " +\n" )
        #         pdfFile.write( "elev=dtedmos/" + dtedName + " +\n" )
        #         pdfFile.write( "prefx=\"" + prefix + "\" +\n" )
        #         pdfFile.write( "wsize=2048 ffts=128 magn=1.0 redop=\"y\"\n" )
        pdfFile.write( "end-proc\n" )
        pdfFile.close()

        self.status=["Calculating accuracy statistics and plot", 90]
        
        #os.system( "cp " + pdfScript + " /tmp" )

        os.system( "( . " + sc.afidsDir + "/setup_afids_env.sh ; vicarb " + pdfScript + " ) >> " + logOut )
        os.system( "touch " + geoacc + "; touch " + outplot )

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
