#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "vif.h"

// This is the "VICAR Interface" application. It runs either as a
// server or as a client. In server mode, it opens two server
// sockets. One is opened using FN_Sender. It listens for clients who
// want to receive file notifications. The other server socket waits
// for messages from VICAR clients producing notification messages,
// which are forwarded to FN_Sender's clients. In client mode, this
// application connects to an instance of this application running in
// server mode. It sends a single file name and disconnects and
// terminates.

// This application was built with the following command:
// gcc vifMain.cpp vif.cpp ../sample/src/FN_Util.cpp -I../inc -I../sample/inc -lfile-notify -L../lib -o vif

static void usage( char* name ) {
  printf( "Possible Usages:\n" );
  printf( "%s server <VICAR client port number> <FN_Sender port number>\n", name );
  printf( "%s client <server ip address> <server port> fileName\n", name );

  exit( 1 );
}

int main(int argc, char *argv[]) {
	
  if ( argc == 4 && strcmp( argv[ 1 ], "server" ) == 0 )
    exit( vifStartServer( argv[ 2 ], argv[ 3 ] ) );

  else if ( argc == 5 && ( strcmp( argv[ 1 ], "client" ) == 0 ) )
    exit( vifNotifyServer( argv[ 2 ], argv[ 3 ], argv[ 4 ] ) );

  else
    usage( argv[ 0 ] );
	
  exit( 1 );
}
