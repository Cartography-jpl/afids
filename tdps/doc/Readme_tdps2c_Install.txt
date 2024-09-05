1. TDPS INSTALLATION AND LAUNCHING 

The TDPS (Terrain Database Preparation System) has been packaged into a
set of procedures called VICAR. To install TDPS/VICAR, follow the
installation instructions below. The TDPS now runs with Red Hat Linux
Enterprise v3 or v4, and runs on either 32 or 64 bit PC hardware. A
System Administrator should perform the installation. 


1.1 TDPS/VICAR Distribution Files 

TDPS/VICAR is nominally distributed on a CD-ROM with the following files: 
- tdps2c_date.tar.Z    	  (Complete TDPS/VICAR software package) 
- Readme_TDPS_Install.txt   (Latest Installation Information)
Note: TDPS data files are distributed separately


1.2 Replacing an Older Version of TDPS/VICAR

If this installation is to replace a pre-existing version, first delete
the entire previous installation and it's directory:

rm -rf /opt/tdps2b


1.3 File Locations and Installation
 
The recommended location for the TDPS is /opt. In the /opt directory
(or wherever you actually install TDPS/VICAR), uncompress and untar the 
"tdps2c_date.tar.Z" file. 

uncompress tdps2c_date.tar.Z 	(if unavailable, use gzip -d)
tar xvpf tdps2c_date.tar 		(be sure to specify the "p")

This creates a directory (about 1.3GB in size) called:
 
tdps2c

NOTE: If you choose an alternate path to tdps2c, be sure 
there are NO CAPITAL LETTERS or hyphens in the path name! 

Change your location to /opt/tdps2c (or wherever), and from within 
/opt/tdps2c, install the software by typing: 

./install 

When prompted, enter the path name to your tdps data files,
for example: /data

Enter absolute path to directory containing TDPS data> /data

After a few messages are displayed, the installation is complete.


1.4 Shell and Path Suggestions 

VICAR uses the "tcsh" shell. However, the TDPS installation 
internalizes the tcsh shell such that users can operate TDPS/VICAR 
regardless of the shell they prefer. In order to run TDPS/VICAR 
outside the installation directory, the following Path information 
should be entered (Note, replace /opt with your actual path): 

cshell or tcshell - Add this to the end of your ~/.cshrc file
setenv PATH "/opt/tdps2c:${PATH}" 

bourne/bash shell - Add this to the end of your ~/.bash_profile: 
PATH=/opt/tdps2c:${PATH} 
export PATH 

sh shell - Add this to the end of your ~/.profile: 
PATH=/opt/tdps2c:${PATH} 
export PATH 

Note that these changes will not take effect until a new shell is
started, i.e., Log out then log back in.


1.5 Activate TDPS

To launch the TDPS, move to your working directory (e.g., /tdps2c).
Open an "X" window (if not already the default) and type:

	tdps or ./tdps

The "CBS TDPS Main Menu" gui will appear. Click the "?" help buttons
for information about how to run TDPS, or review the User Guide.

