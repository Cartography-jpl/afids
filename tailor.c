#include <stdio.h>

#define LINE_LENGTH 80
#define FIRST_COLUMN 10
#define FIELD_LENGTH 20

int fixLine( char * path, FILE * f, int line ) {
  char buf[ FIELD_LENGTH + 1 ];
  int fieldValue;
  char format[ 5 ];

  buf[ FIELD_LENGTH ] = '\0';

  sprintf( format, "%%%dd", FIELD_LENGTH );

  if ( fseek( f, line * LINE_LENGTH + FIRST_COLUMN, SEEK_SET ) ) {
    fprintf( stderr, "tailor: error seeking to field in %s\n", path );
    return 1;
  }

  if ( fread( buf, FIELD_LENGTH, 1, f ) != 1 ) {
    fprintf( stderr, "tailor: error reading field in %s\n", path );
    return 1;
  }

  if ( sscanf( buf, "%d", &fieldValue ) != 1 ) {
    fprintf( stderr, "tailor: error parsing integer from field value \"%s\"\n", buf );
    return 1;
  }

  sprintf( buf, format, fieldValue );

  if ( fseek( f, line * LINE_LENGTH + FIRST_COLUMN, SEEK_SET ) ) {
    fprintf( stderr, "tailor: error seeking to field in %s\n", path );
    return 1;
  }

  if ( fwrite( buf, FIELD_LENGTH, 1, f ) != 1 ) {
    fprintf( stderr, "tailor: error writing field in %s\n", path );
    return 1;
  }

  return 0;
}

int main( int argc, char* argv[] ) {
  FILE * f;
  int line;

  if ( argc != 2 ) {
    fprintf( stderr, "usage: tailor file\n" );
    return 1;
  }

  f = fopen( argv[ 1 ], "r+b" );

  if ( ! f ) {
    fprintf( stderr, "tailor: error opening %s for read/write\n", argv[ 1 ] );
    return 1;
  }

  for ( line = 3; line < 5; line ++ )
    if ( fixLine( argv[ 1 ], f, line ) )
      return 1;

  if ( fclose ( f ) ) {
    fprintf( stderr, "tailor: error closing %s\n", argv[ 1 ] );
    return 1;
  }

  return 0;
}
