dnl Process this file with autoconf to produce a configure script.

AC_INIT(afids, 1.26)
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config/m4])
AFIDS_COMMON
ACX_PTHREAD
MP_WITH_CURSES
AC_CONFIG_FILES([afids/script/setup_afids_env.csh afids/script/setup_afids_env.sh afids/script/run_me_if_moved.sh])
AC_CONFIG_FILES([afids/script/vicar afids/script/vicarb afids/script/taetm afids/script/vifclient afids/script/vextract afids/script/afids afids/script/ctv2 afids/vdev/AfidsMakefile.in])
AC_CONFIG_FILES([afids/tdps/src/tdps afids/tma/bin/tma afids/tma/tcllib/maptcl/map_srv afids/tma/tcllib/tma/regionCreate.tcl])

AC_ARG_VAR([THIRDPARTY], [
It is common that either 1) you want to build a local copy of all
thirdparty software or 2) you have a central location not already
searched for that you want to be searched or 3) you want to build a local
copy of any thirdparty software not found on the system. You can specify
THIRDPARTY=build, THIRDPARTY=build_needed or THIRDPARTY=<directory>.
This gets added to all the third party library checks.
])

#=================================================================
# Need to find VICAR RTL library
AC_VICAR_RTL(required, cannot_build, default_search)

#=================================================================
# Need to find VICAR programs
AC_VICAR(required, cannot_build, default_search)

#=================================================================
# Usually want xvd, although it isn't strictly required
AC_AFIDS_XVD(not_required, cannot_build, default_search)

#=================================================================
# Need to find carto library.
AC_GSL(required, cannot_build, default_search)
AC_CARTO(required, cannot_build, default_search)

#=================================================================
# Need to have AFIDS data
AFIDS_DATA_OPTIONS
AC_AFIDS_DATA(required, cannot_build, default_search)

#=================================================================
# Various thirdparty libraries.

AC_SPICE(required, cannot_build, default_search)
AC_HDFEOS(required, cannot_build, default_search)
AC_GNUPLOT(required, cannot_build, default_search)
AC_FFTW(required, cannot_build, default_search)
AC_PNG(required, cannot_build, default_search)
AC_TIFF(required, cannot_build, default_search)
AC_GEOTIFF(required, cannot_build, default_search)
AC_JPEG(required, cannot_build, default_search)
AC_FREETYPE(required, cannot_build, default_search)
AC_OPENSSL(required, cannot_build, default_search)
AC_TCL(required, cannot_build, default_search)
# Temporary, allow itcl to be missing. There is stuff we can't build,
# but we don't have this in conda yet and want to build everything
# that doesn't depend on this.
AC_ITCL(no_required)
# Not required for build, but we do set up a pointer to this in the
# setup file. So look for this, but don't fail if we don't find it
AC_GDAL(no_required, cannot_build, default_search)
AC_VICAR_GDALPLUGIN(no_required, cannot_build, default_search)

#=================================================================
# We have a small amount of code that gets different flags depending on
# if we are using g77 or gfortran, so pass this to the Makefile.
#=================================================================

AM_CONDITIONAL([HAVE_G77], [test `expr "${F77}" : '.*g77'` != "0"])
AM_CONDITIONAL([HAVE_GFORTRAN], [test `expr "${F77}" : '.*gfortran'` != "0"])

#=================================================================
# We have a few Make rules that need to be different on the Mac. This
# only affects external library builds - all of our code builds the 
# same on a Mac or Linux machine.
#=================================================================

AC_IS_MAC

#=================================================================
# Redhat 8 changed the location of rpc.h, add handling for this
have_tirpc="no"
RPC_CPPFLAGS=""
AC_CHECK_HEADER([rpc/rpc.h],,
[
saved_CPPFLAGS=$CPPFLAGS
CPPFLAGS="$saved_CPPFLAGS -I/usr/include/tirpc"
AC_CHECK_HEADER([tirpc/rpc/rpc.h],
		[have_tirpc="yes"; RPC_CPPFLAGS="-I/usr/include/tirpc"])
CPPFLAGS="$saved_CPPFLAGS"		
])
AM_CONDITIONAL([HAVE_NEW_TIRPC],[test "$have_tirpc" = "yes"])
AC_SUBST(RPC_CPPFLAGS)

#=================================================================
# Option to include "AFIDS configuration A" stuff.
AFIDS_A_OPTION
if test x$with_afids_a = xyes; then
  AC_DEFINE(INCLUDE_AFIDS_A, [], [Include AFIDS A stuff])
fi

#=================================================================
# Include tests with a random element.

AC_ARG_WITH([random-test],
            [AS_HELP_STRING([--with-random-test],
              [Some tests include a random element. These are not ideal for
automated testing, so these aren't run by default. But you may want to 
optionally turn this on in your own testing area.])],
            [with_random_test=yes],
            [with_random_test=no])
AM_CONDITIONAL([WITH_RANDOM_TEST], [test x$with_random_test = xyes])

# This set up all the source directories. Source for this is found in
# config/m4/afids_source_directory.m4 so we can include this in multiple
# configuration files.
AFIDS_SOURCE_DIRECTORY


AM_CONDITIONAL([BUILD_AFIDS], [true])
AM_CONDITIONAL([HAVE_AFIDS], [true])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT


if test "$have_swig" = "yes" -a "$USE_MAINTAINER_MODE" != "no"; then
  generate_swig="yes"
else
  generate_swig="no" 
fi

#======================================================================
# Warning about no XVD
#======================================================================

if test "$afids_xvd_build_error" = "yes"; then
   AC_MSG_WARN([$afids_xvd_build_error_message])
fi
if test "$have_afids_xvd" = "no"; then
  AC_MSG_WARN([
=============================================================================
=== The AFIDS XVD program is not found on the system. Normally you want   ===
=== this, although it isn't strictly required. Try specifying location    ===
=== using --with-afids-xvd if configure couldn't find the library. You    ===
=== can specify --with-afids-xvd=build if you want to build your own      ===
=== local copy of AFIDS XVD.                                              ===
===                                                                       ===
=== You can also specify THIRDPARTY=build if you want to build a local    ===
=== copy of all the libraries/programs.                                   ===
===                                                                       ===
=== If you are trying to build AFIDS XVD but getting this message, look   ===
=== above for possible information about missing dependencies.            ===
=============================================================================
])
fi

#======================================================================
# Give a nice log message describing what has been configured, and which
# optional libraries are going to be used.
#======================================================================

LOC_MSG()
LOC_MSG([AFIDS is now configured])
LOC_MSG()
LOC_MSG([  Installation directory:        ${prefix}])
LOC_MSG([  Build debug version:           ${enable_debug}])
LOC_MSG([  Fortran compiler:              ${F77} ${AM_FFLAGS}])
LOC_MSG([  C compiler:                    ${CC} ${CFLAGS}])
LOC_MSG([  CXX compiler:                  ${CXX} ${CXXFLAGS}])
LOC_MSG([  AFIDS test data:               ${ac_afids_test_data_dir_default}])
LOC_MSG([  Have massive test datasets:    ${have_massive_dataset}])
LOC_MSG()
LOC_MSG([  Include AFIDS A configuration: ${with_afids_a}])
