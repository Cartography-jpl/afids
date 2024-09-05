1. VICAR/AFIDS INSTALLATION AND LAUNCHING 

VICAR has been packaged into a set of procedures called AFIDS. To install 
VICAR, follow the AFIDS installation instructions below. 

AFIDS runs on both Red Hat Linux Enterprise v3 or v4, and Sun Solaris v8,
v9, and v10 operating systems. The Red Hat Linux version of AFIDS was
designed for both 32 or 64 bit PC hardware, whereas the Sun platforms are
always 64 bits. A System Administrator should perform the installation.


1.1 AFIDS/VICAR Distribution Files 

AFIDS/VICAR is nominally distributed on a CD-ROM with the following files: 
- afids_4c_dateOS.tar.Z      (Complete AFIDS/VICAR software package) 
- Readme_VICAR_Install.txt   (Installation Information)


1.2 Installing Over an Older Version of AFIDS/VICAR

If this installation is to replace a pre-existing version, first delete
the entire previous installation and itÕs directory:

rm Ðrf /opt/afids_4.0

Alternatively, you can rename (mv) the old directory, e.g., afids_4.0_old


1.3 File Locations
 
The recommended location for AFIDS is /opt. In the /opt directory
(or wherever you want to install AFIDS/VICAR),
uncompress and untar the "afids_4c_dateOS.tar.Z" file. 

uncompress afids_4c_dateOS.tar.Z 
tar xvf afids_4c_dateOS.tar 

This creates a directory (about 2.3GB in size) called:
 
afids_4.0

NOTE: If you choose an alternate path to afids_4.0, be sure 
there are NO CAPITAL LETTERS or hyphens in the path name! 

Change your location to /opt/afids_4.0 (or wherever), and from within 
/opt/afids_4.0, install the software by typing: 

./install 


1.4 Shell and Path Suggestions 

VICAR uses the "tcsh" shell. However, the AFIDS installation 
internalizes the tcsh shell such that users can operate AFIDS/VICAR 
regardless of the shell they prefer. In order to run AFIDS/VICAR 
outside the installation directory, the following Path information 
should be entered (Note, replace /opt with your actual path): 

cshell or tcshell - Add this to the end of your ~/.cshrc file
setenv PATH "/opt/afids_4.0:${PATH}" 

bourne/bash shell - Add this to the end of your ~/.bash_profile: 
PATH=/opt/afids_4.0:${PATH} 
export PATH 

sh shell - Add this to the end of your ~/.profile: 
PATH=/opt/afids_4.0:${PATH} 
export PATH 

Note that these changes will not take effect until a new shell is started,
or type:  rehash


1.5 Activate VICAR
 
VICAR is now automatically available through AFIDS or the AMT Perl script.

Launch VICAR by typing vicar at the Unix prompt. The system will respond 
with a Welcome Banner, several messages, and the vicar prompt: 

%VICAR> 

To leave/quit VICAR, type exit 

%VICAR>exit 

