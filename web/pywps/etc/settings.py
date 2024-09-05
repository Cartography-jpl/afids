"""
This is main configuration file for pywps. 

The most importand parts are
WPS[ServiceIdenteification]
ServerSettings
"""
# Author:	Jachym Cepicky
#        	http://les-ejk.cz
# Lince: 
# 
# Web Processing Service implementation (conf. file)
# Copyright (C) 2006 Jachym Cepicky
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA



###########################################################
#
# In this WPS structure the main configuration is stored
#
###########################################################
WPS = {
    # version of supported WPS
    # 'version':"0.4.0"
    'version': "0.4.0",

    #
    # This are mandatory and optional conf. parameters
    #
    'ServiceIdentification': {

        ####################
        # Mandatory options
        ####################
        #
        # 'Title':"This WPS Title",
        #
        'Title':"WpsAfids 0.1",
        #
        # 'ServiceType":"WPS",
        #
        'ServiceType':"WPS",
        #
        # 'ServiceTypeVersion':"0.1.0",
        #
        'ServiceTypeVersion':"0.1.0",

    },
    
    #
    # Service provider identification
    #
    'ServiceProvider': {
            'ProviderName' : "JPL",
            'IndividualName':"Walt Bunch",
            'PositionName':"Engineer",
            'Role':"Author",
            'DeliveryPoint': "4800 Oak Grove",
            'City': "Pasadena",
            'AdministrativeArea': "CA",
            'PostalCode':"91109",
            'Country': "USA",
            'ElectronicMailAddress':"Walter.L.Bunch@jpl.nasa.gov",
    },

    # 
    # OperationsMetadata options
    #
    'OperationsMetadata': {
        #
        # ServerAddress - URL address to your pywps server
        'ServerAddress' : "http://localhost/cgi-bin/wps.py",
    },

    #
    # Server Keywords array
    # 
    # 'Keywords': ['GRASS','GIS','WPS'],
    'Keywords' : ['GIS','WPS','AFIDS'],

}

###########################################################
#
# In this ServerSettings structure, the most importand server settings are
# stored
#
###########################################################
ServerSettings = {
    #
    # outputPath - directory, where your files will be stored, if
    # storeSupported is set to "true"
    # NOTE: You have to create this directory manualy and set rights, so
    #       the program is able to store data in there 
    # 'outputPath':'/var/www/wpsoutputs',
    'outputPath': '/usr/share/tomcat5/webapps/ROOT/wpsoutput',
    
    #
    # 'outputUrl' - URL of the directory, where the outputs will be stored
    # 'outputUrl': 'http://localhost/wpsoutputs',
    'outputUrl':  'http://cart01:8080/wpsoutput',

    #
    # tempPath - path to directory, where temporary data will be stored.
    # NOTE: the pywps has to have rights, to create directories and files
    #       in this directory
    # 'tempPath':'/tmp',
    'tempPath': '/tmp',

    #
    # maxOperations - maximum number of operations, which is allowed to low
    # on this server at ones 
    # default = 1
    # 'maxOperations':1,
    'maxOperations':16,
    
    #
    # maxSize: maximum input file size in bytes
    # NOTE: maximum file size is 5MB, no care, if this number is heigher
    # 'maxSize':5242880, # 5 MB
    'maxSize':100000000000, # 100G

    #
    # maxInputParamLength: maximal length of input values
    # NOTE: maximal length of input parameters is 256, no mather, how height
    #       is this number
    # 'maxInputParamLength':256,
    'maxInputParamLength':1024,
}
