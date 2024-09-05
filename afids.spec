Summary: This is AFIDS
Name: afids
Version: 1.26
Release: 1.el%{rhel}
License: Copyright 2021 California Institute of Technology ALL RIGHTS RESERVED
Group: Applications/Engineering
Vendor: California Institute of Technology
URL: http://www-mipl.jpl.nasa.gov/cartlab/cartlab.html
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Requires: vicar afids-data tk itcl-afids tix gnuplot-afids
BuildRequires: vicar-rtl vicar carto afids-data spice gsl-devel hdfeos gnuplot-afids fftw-devel libgeotiff-afids libtiff-devel freetype-devel libjpeg-turbo-devel openssl-devel vicar-gdalplugin tk-devel afids-xvd
Prefix: /opt/afids

# And turn off brp-mangle-shebangs. We actually do not want the RPM to
# update these, we already handle these correctly
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-mangle-shebangs[[:space:]].*$!!g')

%description

This is AFIDS.

%prep
%setup -q

%build
./configure --prefix=/opt/afids --with-afids-a 
make %_smp_mflags 

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install
rm -f $RPM_BUILD_ROOT/opt/afids/lib/libfilenotify.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/libgeotrans.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/libvif.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/notifyTcl.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/shapeUtils.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/tiffTcl.la
rm -f $RPM_BUILD_ROOT/opt/afids/lib/tmaSrv.la

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc
/opt/afids/afids/AfidsMakefile.in
/opt/afids/afids/doc/AFIDS_Server_Rack.pdf
/opt/afids/afids/doc/AFIDS_User_Guide_v4p.pdf
/opt/afids/afids/doc/NLC_User_Guide_v12.doc
/opt/afids/afids/doc/NLC_User_Guide_v12.pdf
/opt/afids/afids/doc/Readme_VICAR_Install_copy.txt
/opt/afids/afids/doc/Software_Release_Update_4d.pdf
/opt/afids/afids/doc/help.html
/opt/afids/afids/doc/help_nlc.htm
/opt/afids/afids/doc/vug.tar
/opt/afids/afids/tcl/afids.tcl
/opt/afids/afids/tcl/api.tcl
/opt/afids/afids/tcl/asc2fade.tcl
/opt/afids/afids/tcl/asc2shp.tcl
/opt/afids/afids/tcl/cbSupport.tcl
/opt/afids/afids/tcl/cibaoi.tcl
/opt/afids/afids/tcl/cibmos.tcl
/opt/afids/afids/tcl/classes.tcl
/opt/afids/afids/tcl/ctv2.tcl
/opt/afids/afids/tcl/decodeIgeoloDms.tcl
/opt/afids/afids/tcl/getNitfImageHeaderField.tcl
/opt/afids/afids/tcl/getnitfcorners.tcl
/opt/afids/afids/tcl/gui.tcl
/opt/afids/afids/tcl/html_library.tcl
/opt/afids/afids/tcl/ngclasses.tcl
/opt/afids/afids/tcl/nlc.tcl
/opt/afids/afids/tcl/taehelp.tcl
/opt/afids/afids/tcl/wv2asc2fade.tcl
/opt/afids/afids/tdps_tcl/aoi.tcl
/opt/afids/afids/tdps_tcl/attribute.tcl
/opt/afids/afids/tdps_tcl/canvasMouse.tcl
/opt/afids/afids/tdps_tcl/colors.tcl
/opt/afids/afids/tdps_tcl/database.tcl
/opt/afids/afids/tdps_tcl/debug.tcl
/opt/afids/afids/tdps_tcl/filters.tcl
/opt/afids/afids/tdps_tcl/globals.tcl
/opt/afids/afids/tdps_tcl/html_library.tcl
/opt/afids/afids/tdps_tcl/loadUnload.tcl
/opt/afids/afids/tdps_tcl/misc.tcl
/opt/afids/afids/tdps_tcl/options.tcl
/opt/afids/afids/tdps_tcl/propEd.tcl
/opt/afids/afids/tdps_tcl/selection.tcl
/opt/afids/afids/tdps_tcl/statusWin.tcl
/opt/afids/afids/tdps_tcl/vecEdit.tcl
/opt/afids/afids/tdps_tcl/vectorEditor.tcl
/opt/afids/afids/tdps_tcl/vectorProcessing.tcl
/opt/afids/afids/tdps_tcl/view.tcl
/opt/afids/afids/tdps_tcl/vmap.tcl
/opt/afids/afids/tdps_tcl/vmap1_vmap2shp.tcl
/opt/afids/afids/tma_tcl/dbtcl/dbtcl.tcl
/opt/afids/afids/tma_tcl/dbtcl/pkgIndex.tcl
/opt/afids/afids/tma_tcl/maptcl/map.tcl
/opt/afids/afids/tma_tcl/maptcl/mapSrv.tcl
/opt/afids/afids/tma_tcl/maptcl/pkgIndex.tcl
/opt/afids/afids/tma_tcl/tma/MUSESRV.tcl
/opt/afids/afids/tma_tcl/tma/analysisHistory.tcl
/opt/afids/afids/tma_tcl/tma/analyst.tcl
/opt/afids/afids/tma_tcl/tma/colorwheel.tcl
/opt/afids/afids/tma_tcl/tma/contours.tcl
/opt/afids/afids/tma_tcl/tma/editMover.tcl
/opt/afids/afids/tma_tcl/tma/genMover.tcl
/opt/afids/afids/tma_tcl/tma/minpath.tcl
/opt/afids/afids/tma_tcl/tma/mover.tcl
/opt/afids/afids/tma_tcl/tma/obstacles.tcl
/opt/afids/afids/tma_tcl/tma/pkgIndex.tcl
/opt/afids/afids/tma_tcl/tma/polygon.tcl
/opt/afids/afids/tma_tcl/tma/queryTerrain.tcl
/opt/afids/afids/tma_tcl/tma/rainbow.tcl
/opt/afids/afids/tma_tcl/tma/regionCreate.tcl
/opt/afids/afids/tma_tcl/tma/reportCorrelation.tcl
/opt/afids/afids/tma_tcl/tma/tma.tcl
/opt/afids/afids/tma_tcl/tma/tmasrv.tcl
/opt/afids/afids/tma_tcl/tma/util.tcl
/opt/afids/afids/tma_tcl/tma/vpf2vec.tcl
/opt/afids/afids/tma_tcl/tma/weather.tcl
/opt/afids/afids/vdev/*
/opt/afids/bin/Notify_App
/opt/afids/bin/acp
/opt/afids/bin/afids
/opt/afids/bin/ctv2
/opt/afids/bin/libtool
/opt/afids/bin/taetm
/opt/afids/bin/tailor
/opt/afids/bin/vextract
/opt/afids/bin/vicar
/opt/afids/bin/vicarb
/opt/afids/bin/vif
/opt/afids/bin/vifclient
/opt/afids/data/gui/filledTriangle.gif
/opt/afids/data/gui/helpicon.gif
/opt/afids/data/tdps/graphics/add.gif
/opt/afids/data/tdps/graphics/cbs_logo_large.tif
/opt/afids/data/tdps/graphics/cbs_logo_small.gif
/opt/afids/data/tdps/graphics/hand.gif
/opt/afids/data/tdps/graphics/hand.pbm
/opt/afids/data/tdps/graphics/hand.xbm
/opt/afids/data/tdps/graphics/helpicon.gif
/opt/afids/data/tdps/graphics/line.gif
/opt/afids/data/tdps/graphics/line.xbm
/opt/afids/data/tdps/graphics/lineCursor.pbm
/opt/afids/data/tdps/graphics/lineCursor.xbm
/opt/afids/data/tdps/graphics/map.gif
/opt/afids/data/tdps/graphics/map.pbm
/opt/afids/data/tdps/graphics/map.xbm
/opt/afids/data/tdps/graphics/move.gif
/opt/afids/data/tdps/graphics/polygon.gif
/opt/afids/data/tdps/graphics/polygon.xbm
/opt/afids/data/tdps/graphics/polygonCursor.pbm
/opt/afids/data/tdps/graphics/polygonCursor.xbm
/opt/afids/data/tdps/graphics/remove.gif
/opt/afids/data/tdps/graphics/select.gif
/opt/afids/data/tdps/graphics/zoomfit.gif
/opt/afids/data/tdps/graphics/zoomin.gif
/opt/afids/data/tdps/graphics/zoomin.pbm
/opt/afids/data/tdps/graphics/zoomin.xbm
/opt/afids/data/tdps/graphics/zoomout.gif
/opt/afids/data/tdps/graphics/zoomout.pbm
/opt/afids/data/tdps/graphics/zoomout.xbm
/opt/afids/data/tdps/traf_codes.asc
/opt/afids/data/tma/AMD_COORDINATE.dat
/opt/afids/data/tma/AMD_COORDINATE.fmt
/opt/afids/data/tma/AMD_MUSEAOI.dat
/opt/afids/data/tma/AMD_MUSEAOI.fmt
/opt/afids/data/tma/tma_aoi.dat
/opt/afids/data/tma/tma_aoi.fmt
/opt/afids/data/tma/tma_aoiData.dat
/opt/afids/data/tma/tma_aoiData.fmt
/opt/afids/data/tma/tma_dnet.fmt
/opt/afids/data/tma/tma_globalCosts.fmt
/opt/afids/data/tma/tma_mapRegion.dat
/opt/afids/data/tma/tma_mapRegion.fmt
/opt/afids/data/tma/tma_mover.dat
/opt/afids/data/tma/tma_mover.fmt
/opt/afids/data/tma/tma_moverName.dat
/opt/afids/data/tma/tma_moverName.fmt
/opt/afids/data/tma/tma_polygon.dat
/opt/afids/data/tma/tma_polygon.fmt
/opt/afids/data/tma/tma_regionThemes.dat
/opt/afids/data/tma/tma_regionThemes.fmt
/opt/afids/data/tma/tma_slope.dat
/opt/afids/data/tma/tma_slope.fmt
/opt/afids/data/tma/tma_terrain.dat
/opt/afids/data/tma/tma_terrain.fmt
/opt/afids/data/tma/tma_terrainMapping.dat
/opt/afids/data/tma/tma_terrainMapping.fmt
/opt/afids/etc/afids/setup_afids_env.csh
/opt/afids/run_me_if_moved.sh
/opt/afids/etc/afids/setup_afids_env.sh
/opt/afids/lib/libfilenotify.a
/opt/afids/lib/libfilenotify.so
/opt/afids/lib/libfilenotify.so.0
/opt/afids/lib/libfilenotify.so.0.0.0
/opt/afids/lib/libvif.a
/opt/afids/lib/libvif.so
/opt/afids/lib/libvif.so.0
/opt/afids/lib/libvif.so.0.0.0
/opt/afids/lib/notifyTcl.a
/opt/afids/lib/notifyTcl.so
/opt/afids/lib/shapeUtils.a
/opt/afids/lib/shapeUtils.so
/opt/afids/lib/tiffTcl.a
/opt/afids/lib/tiffTcl.so
/opt/afids/lib/tmaSrv.a
/opt/afids/lib/tmaSrv.so
/opt/afids/setup_afids_env.csh
/opt/afids/setup_afids_env.sh
/opt/afids/share/doc/tdps/CBS_TDPS_UsersGuide_v1_1a.pdf
/opt/afids/share/doc/tdps/Readme_tdps2c_Install.txt
/opt/afids/share/doc/tdps/help.html
/opt/afids/share/doc/tma/TMA-TDPS_Overview.docx
/opt/afids/share/geotrans/3_param.dat
/opt/afids/share/geotrans/7_param.dat
/opt/afids/share/geotrans/ellips.dat

%changelog
* Wed Sep 25 2019 Smyth <smyth@macsmyth> - 1.23-1.el%{rhel}
- Add nofill option to shp2rast. Bug fixes for size, deriv, hist. Add
  support in rpc2grid for RSM as well as RPC

* Thu Oct 18 2018 Smyth <smyth@macsmyth> - 1.22-1.el%{rhel}
- Change geomv to work with float type

* Fri Jul 20 2018 Smyth <smyth@macsmyth> - 1.21-1.el%{rhel}
- Add sdsems program

* Tue Jun  5 2018 Smyth <smyth@macsmyth> - 1.20-1.el%{rhel}
- Fix std deviation calculation in imgstat to use N-1 rather than N.

* Wed Oct 18 2017 Smyth <smyth@macsmyth> - 1.19-1.el%{rhel}
- Update fitg, ibisclst3, rpc2sc and shp2rast. This adds support for
  larger files (fitg), adds new margin and pixperdeg parm (shp2rast),
  and cleans up some code (ibisclst3 and rpc2sc).

* Fri Jun 23 2017 Smyth <smyth@macsmyth> - 1.18-1.el%{rhel}
- Update spice library to N0066 version.

* Thu May 25 2017 Smyth <smyth@macsmyth> - 1.17-1.el%{rhel}
- Add dig2refl proc, include numerous changes from Ray, and update lave
  to support 'QUIET' option.

* Fri Oct 7 2016 Mike M Smyth <smyth@pistol> - 1.16-1.el%{rhel}
- Much improved version of mars_correct_image script

* Tue May 17 2016 Mike M Smyth <smyth@pistol> - 1.15-1.el%{rhel}
- Change to use new gtproj2, which support Mars coordinates. Also add 
  wmask=n option to accck to suppress the use of a water mask (which
  obviously doesn't work on Mars). Add use of new proc picmtch5.

* Wed Feb  3 2016 Mike M Smyth <smyth@pistol> - 1.14-1.el%{rhel}
- Fix a bug in imcorners

* Thu Jan 21 2016 Mike M Smyth <smyth@pistol> - 1.13-2.el%{rhel}
- Rebuild after rebuild of vicar-gdalplugin

* Thu Dec 17 2015 Mike M Smyth <smyth@pistol> - 1.13-1
- Rebuild with updated dependencies

* Wed Nov 25 2015 Mike M Smyth <smyth@pistol> - 1.12-1
- Update setup scripts, and vicar2ntf2

* Wed Nov 12 2014 Mike M Smyth <smyth@pistol> - 1.11-1
- Remove use of afids_b option. Not a functional change.

* Tue Oct  7 2014 Mike M Smyth <smyth@pistol> - 1.10-2
- Include change to vextract2 that should have been in 1.10

* Mon Sep 29 2014 Mike M Smyth <smyth@pistol> - 1.10-1
- Add rcpslant, and clean up interface between afids and carto library.

* Thu Jul 24 2014 Mike M Smyth <smyth@pistol> - 1.09-2
- Rebuild against updated version of hdfeos

* Mon Jul  7 2014 Mike M Smyth <smyth@pistol> - 1.09-1
- This adds changes from Walt to support abachd. Updates comptab2 and concomp,
  and adds the new PDFs concomp_module.pdf and pattern_module.pdf

* Thu May 29 2014 Mike M Smyth <smyth@pistol> - 1.08-1
- Clean up a number of TCL/TK scripts

* Fri Feb 21 2014 Mike M Smyth <smyth@pistol> - 1.07-1
- Add ibis2img, and gtpswarp (previously in the source tree, but mistakenly not installed).

* Thu Oct 10 2013 Mike M Smyth <smyth@pistol> - 1.06-1
- Ray's updates for asc2tcl, ascnum, file2tcl, ftype2tcl, gt2ticl,
  ldir2tcl, str2tcl, and substr2tcl. Also Peter's updates to pldsrf
  and spotref.

* Tue Sep 24 2013 Mike M Smyth <smyth@pistol> - 1.05-1
- Ray's updates for f2, fft2005, fftflip, filter, ibis2asc, ibislsq4,
  ifthen, img2ascii, imgsum, indexmerge, interlv, lab2tcl, norangle,
  pixstat, plotint, randpixel, starcar,tclmath, wedge

* Wed Sep  4 2013 Mike M Smyth <smyth@pistol> - 1.04-1
- Update a number of programs to use gnuplot, and to support large files

* Fri Aug  9 2013 Mike M Smyth <smyth@pistol> - 1.03-1
- Add ccdnoise and pldsref

* Tue Jun 25 2013 Mike M Smyth <smyth@pistol> - 1.02-1
- Add sourcing of new setup_afids_xvd.csh file

* Thu Jun  6 2013 Mike M Smyth <smyth@pistol> - 1.01-1
- Update a number of programs, including use of new stacka_big

* Wed Dec  5 2012 Mike M Smyth <smyth@pistol> - 
- Initial build.

