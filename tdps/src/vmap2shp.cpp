#include <ossimGeoAnnotationMultiPolyLineObject.h>
#include <ossimGeoAnnotationMultiPolyObject.h>
#include <ossimVpfAnnotationLibraryInfo.h>
#include <ossimVpfAnnotationSource.h>
#include <ossimVpfTable.h>
#include <shapefil.h>

class exportableOssimVpfAnnotationFeatureInfo : public ossimVpfAnnotationFeatureInfo {
public:

  int dump (char * featureFile, char * outname, unsigned int dbfFieldWidth) {
    std::vector< ossimRefPtr<ossimGeoAnnotationObject> > v = theAnnotationArray;
    int nShapeType;
    int lastShapeId = -1;

    if      (! strcmp( v[0] -> getType() . getname(), "ossimGeoAnnotationMultiPolyLineObject"))
      nShapeType = SHPT_ARC;
    else if (! strcmp( v[0] -> getType() . getname(), "ossimGeoAnnotationMultiPolyObject"))
      nShapeType = SHPT_POLYGON;
    else {
      cerr << "attempted dump of object other than polyline or polygon" << endl;
      exit (1);
    }

    SHPHandle shp = SHPCreate( outname, nShapeType );
    DBFHandle dbf = DBFCreate( outname );

    if (!shp || !dbf) {
      cerr << "failed creating outname" << endl;
      exit (1);
    }

    ossimFilename tableFileName = theCoverage.getPath().dirCat(featureFile);
    ossimVpfTable table;
  
    if(table.openTable(tableFileName)) {
      int numCols = table.getNumberOfColumns();
      for (int i = 0; i < numCols; i ++)
	DBFAddField( dbf, table.getColumnName( i ), FTString, dbfFieldWidth, 0 );
    } else {
      cerr << "failed opening " << tableFileName;
      exit (1);
    }

    int id = 1;

    for (vector< ossimRefPtr< ossimGeoAnnotationObject > >::iterator i = v.begin(); i != v.end(); i++) {
      ossimRefPtr< ossimGeoAnnotationObject > obj = * i;
      ossimGeoAnnotationMultiPolyLineObject * objp = (ossimGeoAnnotationMultiPolyLineObject *) &obj;

      if (! strcmp(obj -> getType() . getname(), "ossimGeoAnnotationMultiPolyLineObject")) {
	
	ossimRefPtr< ossimGeoAnnotationMultiPolyLineObject > mpl = objp;

	std::vector< ossimPolyLine > & plv = mpl -> getMultiPolyLine();

 	for (vector< ossimPolyLine >::iterator i = plv.begin(); i != plv.end(); i++) {
 	  vector< ossimDpt > & vl = i -> getVertexList();

	  double * padfX = new double [vl.size()];
	  double * padfY = new double [vl.size()];
	  int padI;

	  if (! padfX || ! padfY ) {
	    cerr << "new failed on id " << id << " with size " << vl.size() << endl;
	    exit (1);
	  }

	  padI = 0;
	  for (vector< ossimDpt >::iterator j = vl.begin(); j != vl.end(); j++) {
	    padfX [padI] = j -> lon;
	    padfY [padI] = j -> lat;
	    padI ++;
	  }

	  SHPObject * shpObj = SHPCreateSimpleObject( nShapeType, vl.size(), padfX, padfY, 0 );
	  int newShpId = SHPWriteObject( shp, -1, shpObj );
	  lastShapeId = newShpId;
	  SHPDestroyObject( shpObj );

	  delete[] padfX;
	  delete[] padfY;

	  vector< ossimString >& al = i -> getAttributeList();
	  if (al.size() > 0) {
	    for (unsigned int fieldIndex = 0; fieldIndex < al.size(); fieldIndex ++) {
	      if ( strlen( al[fieldIndex] ) > dbfFieldWidth ) {
		char fieldBuf [dbfFieldWidth + 1];
		strncpy( fieldBuf, al[fieldIndex], dbfFieldWidth );
		fieldBuf [dbfFieldWidth] = '\0';

		if (! DBFWriteStringAttribute( dbf, newShpId, fieldIndex, fieldBuf )) {
		  cerr << "DBFWriteStringAttribute failed on " << fieldBuf << " at index " << fieldIndex << " with shape id " << newShpId << endl;
		  exit (1);
		}
		cout << "truncated shape " << newShpId << " field \"" << al[fieldIndex] << "\" index " << fieldIndex << " to " << dbfFieldWidth << " characters" << endl;
	      } else {
		if (! DBFWriteStringAttribute( dbf, newShpId, fieldIndex, al[fieldIndex] )) {
		  cerr << "DBFWriteStringAttribute failed on " << al[fieldIndex] << " at index " << fieldIndex << " with shape id " << newShpId << endl;
		  exit (1);
		}
	      }
	    }
	  }

	  id ++;
 	}
      } else if (! strcmp(obj -> getType() . getname(), "ossimGeoAnnotationMultiPolyObject")) {
	ossimGeoAnnotationMultiPolyObject * objp = (ossimGeoAnnotationMultiPolyObject *) &obj;

	ossimRefPtr< ossimGeoAnnotationMultiPolyObject > mp = objp;

	vector< ossimGeoPolygon > & pv = mp -> getMultiPolygon();

 	for (vector< ossimGeoPolygon >::iterator i = pv.begin(); i != pv.end(); i++) {
 	  const vector< ossimGpt > & vl = i -> getVertexList();
	  const vector< ossimGeoPolygon > & holes = i ->  getHoleList();

	  int shapeSize = vl.size();
	  int holeCount = holes.size();
	  int * panPartStart = new int [holeCount + 1];
	  panPartStart[0] = 0;
	  for (int hole = 0; hole < holeCount; hole ++) {
	    panPartStart[hole + 1] = shapeSize;
	    shapeSize += holes[hole].getVertexList().size();
	  }

	  double * padfX = new double [shapeSize];
	  double * padfY = new double [shapeSize];

	  if (! padfX || ! padfY ) {
	    cerr << "new failed on id " << id << " with size " << vl.size() << endl;
	    exit (1);
	  }

	  int vlLen = vl.size();
	  int padI = 0;
	  for (int vlIndex = 0; vlIndex < vlLen; vlIndex ++) {
	    padfX [padI] = vl[vlIndex].lon;
	    padfY [padI] = vl[vlIndex].lat;
	    padI ++;
	  }

	  for (int hole = 0; hole < holeCount; hole ++) {
	    const vector< ossimGpt > & hvl = holes[hole] . getVertexList();
	    int size = hvl.size();
	    for (int vlIndex = 0; vlIndex < size; vlIndex ++) {
	      padfX [padI] = hvl[vlIndex].lon;
	      padfY [padI] = hvl[vlIndex].lat;

	      padI ++;
	    }
	  }

	  SHPObject * shpObj = SHPCreateObject( nShapeType, -1, holeCount + 1, panPartStart, NULL, padI, padfX, padfY, NULL, NULL );
	  int newShpId = SHPWriteObject( shp, -1, shpObj );
	  lastShapeId = newShpId;
	  SHPDestroyObject( shpObj );

	  delete[] padfX;
	  delete[] padfY;

	  vector< ossimString >& al = i -> getAttributeList();
	  if (al.size() > 0) {
	    for (unsigned int fieldIndex = 0; fieldIndex < al.size(); fieldIndex ++) {
	      if ( strlen( al[fieldIndex] ) > dbfFieldWidth ) {
		char fieldBuf [dbfFieldWidth + 1];
		strncpy( fieldBuf, al[fieldIndex], dbfFieldWidth );
		fieldBuf [dbfFieldWidth] = '\0';

		if (! DBFWriteStringAttribute( dbf, newShpId, fieldIndex, fieldBuf )) {
		  cerr << "DBFWriteStringAttribute failed on " << fieldBuf << " at index " << fieldIndex << " with shape id " << newShpId << endl;
		  exit (1);
		}
		cout << "truncated shape " << newShpId << " field \"" << al[fieldIndex] << "\" index " << fieldIndex << " to " << dbfFieldWidth << " characters" << endl;
	      } else {
		if (! DBFWriteStringAttribute( dbf, newShpId, fieldIndex, al[fieldIndex] )) {
		  cerr << "DBFWriteStringAttribute failed on " << al[fieldIndex] << " at index " << fieldIndex << " with shape id " << newShpId << endl;
		  exit (1);
		}
	      }
	    }
	  }

	  id ++;
 	}

      }
    }

    DBFClose( dbf );
    SHPClose( shp );

    return lastShapeId;
  }
};

int main( int argc, char * argv [] ) {
  if (argc < 7) {
    cerr << "usage: " << argv[0] << " dhtPath lib coverage feature outname dbfFieldWidth [type]" << endl;
    cerr << "  e.g. " << argv[0] << " cd/eurnasia/dht lib_001 hydro watrcrsa watrcrsa 25 line" << endl;
    exit (1);
  }

  char * dhtPath = argv[1];
  char * lib = argv[2];
  char * coverage = argv[3];
  char * feature = argv[4];
  char * featureFile = new char[ strlen( argv[4] ) + 5 ]; // null plus ".xft"
  char * outname = argv[5];
  unsigned int dbfFieldWidth;
  char * shapeType = 0;

  if ( argc == 8 )
    shapeType = argv[ 7 ];

  if (sscanf ( argv [6], "%d", & dbfFieldWidth ) != 1) {
    cerr << "failure parsing dbfFieldWidth from " << argv [6] << endl;
    exit (1);
  }

  strcpy( featureFile, argv[4]);

  if      ( (shapeType && !strcmp(shapeType, "line")) || (argv[4][strlen( argv[4] ) - 1] | ' ') == 'l')
    strcat( featureFile, ".lft" );
  else if ( (shapeType && !strcmp(shapeType, "area")) || (argv[4][strlen( argv[4] ) - 1] | ' ') == 'a')
    strcat( featureFile, ".aft");
  else {
    cerr << "feature must be either a line or an area feature" << endl;
    exit (1);
  }

  ossimVpfDatabase vd;

  if( ! vd.openDatabase( dhtPath ) ) {
    cout << "failed to open a dht " << dhtPath << endl;
    exit (1);
  }

  vector< ossimString > v = vd.getLibraryNames();
  int libIndex = 0, theLibIndex = -1;

  for (vector< ossimString >::iterator i = v.begin(); i != v.end(); i++) {
    if (! strcasecmp( i->c_str(), lib ))
      theLibIndex = libIndex;
    libIndex ++;
  }

  if (theLibIndex < 0) {
    cout << "didn't find lib " << lib << " while comparing to the following:" << endl;
    for (vector< ossimString >::iterator i = v.begin(); i != v.end(); i++)
      cout << *i << endl;
    exit (1);
  }

  ossimVpfAnnotationLibraryInfo vali;
  vali.setName( v[theLibIndex] );
  vali.setDatabase( & vd );
  vali.buildLibrary(coverage, feature);

  std::vector< ossimVpfAnnotationFeatureInfo * > featureVec;
  vali.getAllFeatures( featureVec );

  if (featureVec.size() > 0)
    cout << 1 + ((exportableOssimVpfAnnotationFeatureInfo *) featureVec[0]) -> dump( featureFile, outname, dbfFieldWidth ) << " shapes written to " << outname << endl;
  else
    cout << "No shapes for " << outname << endl;

  exit (0);
}
