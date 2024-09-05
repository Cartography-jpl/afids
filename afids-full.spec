Summary: This installs the full AFIDS package
Name: afids-full
# Note, if you update this version, make sure to update configure.ac also
Version: 1.32
Release: 1.el%{rhel}
License: Copyright 2022 California Institute of Technology ALL RIGHTS RESERVED
Group: Applications/Engineering
Vendor: California Institute of Technology
URL: http://www-mipl.jpl.nasa.gov/cartlab/cartlab.html
# We list all the version and items we have in a afids install
Requires: isis >= 6.0.0
Requires: pommos >= 1.1
Requires: pomm-ui >= 1.0
Requires: afids >= 1.26
Requires: afids-data >= 1.11
Requires: afids-python >= 1.28
Requires: pynitf >= 1.11
Requires: afids-xvd >= 1.05
Requires: blitz-afids >= 1.0.1
Requires: boost-afids >= 1.75.0
Requires: carto >= 1.05
Requires: cmake28 >= 3.22.3
Requires: gd-afids >= 2.1.1
Requires: gdal-afids >= 3.2.1
Requires: geos-afids >= 3.9.0
Requires: gnuplot-afids >= 5.4.1
Requires: hdf5-afids >= 1.10.7
Requires: hdf4-afids >= 4.2.15
Requires: hdfeos >= 2.19.1
Requires: itcl-afids >= 4.0.4
Requires: joe-afids >= 4.1
Requires: libgeotiff-afids >= 1.6.0
Requires: ogdi-afids >= 4.1.0
Requires: opencv-afids >= 4.1.0
Requires: openjpeg20 >= 2.4.0
Requires: proj-afids >= 7.2.1
Requires: python3-afids >= 3.9.1
Requires: python-package >= 2.6
Requires: spice >= 66.0
Requires: vicar >= 1.13.1
Requires: vicar-gdalplugin >= 1.12
Requires: vicar-rtl >= 1.09
Requires: zeromq4 >= 4.3.4
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
This is a convenience package that just installs all of the afids packages
at once.

%prep

%build

%install

%clean

%files
%defattr(-,root,root,-)
%doc


%changelog
* Wed Feb 17 2021 Smyth <smyth@macsmyth> - 1.29-1.el%{rhel}
- Update stuff

* Wed Sep 25 2019 Smyth <smyth@macsmyth> - 1.28-1.el%{rhel}
- Add HiRISE support to generate_rsm and rsm_project. Add tpcol_to_table
  for writing tie-points out as an ASCII file. Add float support to geomv.
  Add nofill option to shp2rast. Bug fixes for size, deriv, hist. Add
  support in rpc2grid for RSM as well as RPC  

* Thu Oct 18 2018 Smyth <smyth@macsmyth> - 1.27-1.el%{rhel}
- Add sdsems, change geomv to work with float type, adds hidra software,
  rsm_tiepoint, rsm_improve programs. Changes mars_nest to work with
  multiple bands.

* Tue Jun  5 2018 Smyth <smyth@macsmyth> - 1.26-1.el%{rhel}
- Improves mars_rsm to include camera nonlinearities and fix "missing grid"
  point errors. Numerous changes to support reading/writing NITF files
  and working with ecostress. Fix std deviation calculation in imgstat to
  use N-1 rather than N. Remove use of relative paths in geocal.ker,
  this causes problem when reloading kernels. Also fix long path problem
  with cspice. Add proper support for pixel as point vs pixel as area in
  handling VICAR files (vicar gdal plugin)
  
* Wed Oct 18 2017 Smyth <smyth@macsmyth> - 1.25-1.el%{rhel}
- Add mars_generate_rsm and rsm_project for mars nest

* Fri Jun 23 2017 Smyth <smyth@macsmyth> - 1.24-2.el%{rhel}
- Update spice library to N0066 version.

* Thu May 25 2017 Smyth <smyth@macsmyth> - 1.24-1.el%{rhel}
- Add support for doing Mars processing in GeoCal

* Fri Oct 7 2016 Mike M Smyth <smyth@pistol> - 1.23-1.el%{rhel}
- Much improved version of mars_correct_image script

* Tue May 17 2016 Mike M Smyth <smyth@pistol> - 1.22-1.el%{rhel}
- Add support for Mars coordinate systems. Add mars_correct_image
  script

* Wed Feb  3 2016 Mike M Smyth <smyth@pistol> - 1.21-1.el%{rhel}
- Fix a bug in imcorners. Fix some things broken in abachd script for
  python 3 upgrade, as well as allowing DEM specification to be a
  relative path (previously this file only had to be an absolute
  path.)

* Wed Jan 20 2016 Mike M Smyth <smyth@pistol> - 1.20-1.el%{rhel}
- Remove python 2.7, and only have python 3.

* Thu Jan 14 2016 Mike M Smyth <smyth@pistol> - 1.19-1
- Have python 3 working, although this still isn't the default.

* Mon Dec 14 2015 Mike M Smyth <smyth@pistol> - 1.18-1
- Add python 3

* Wed Nov 25 2015 Mike M Smyth <smyth@pistol> - 1.17-1
- Update version of boost to 1.59.0 and update to latest version of geocal.

* Wed Jul  1 2015 Mike M Smyth <smyth@pistol> - 1.16-1
- Add scikit-umfpack

* Wed Jun 10 2015 Mike M Smyth <smyth@pistol> - 1.15-1
- Add seaborn and panda

* Tue Jun  9 2015 Mike M Smyth <smyth@pistol> - 1.14-1
- Update version of matplotlib, and include new dependencies

* Tue Jun  2 2015 Mike M Smyth <smyth@pistol> - 1.13-1
- Update ipython to 3.1.0, including a few new dependencies of ipython

* Fri Jan 23 2015 Mike M Smyth <smyth@pistol> - 1.12-1
- Explicitly list versions needed, so we can update the whole system by 
  just doing yum install afids-full

* Fri Nov 14 2014 Mike M Smyth <smyth@pistol> - full-1
- Initial build.

