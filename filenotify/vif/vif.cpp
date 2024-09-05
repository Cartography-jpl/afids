#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>

#include "FN_Sender.h"
#include "Simple_Sender_Handler.h" 
#include "FN_Defs.h"
#include "Standard_Socket.h"
#include "vif.h"

static void notify_loop( Standard_Socket& sock ) {
  char buffer[501];
  bool ask_again = true;
  FN_Sender* fns = FN_Sender::instance();
  int ret;

  do {
    //acquire file path from notifying client
    memset(buffer, 0, sizeof(buffer));

    int size;

    // wait for client
    FN_Socket* fnSock = sock.accept();

    fnSock->read_n( &size, sizeof( size ) );
    if ( size > 500 ) {
      size = 500;
      printf( "file name truncated to 500 characters\n" );
    }
    fnSock->read_n( buffer, size );
    buffer[ size ] = '\0';

    fnSock->close();

    delete fnSock;

    //check if server should be closed
    if(strcmp(buffer, "this cannot possibly be a file name") != 0 &&
       strcmp(buffer, "shutdown") != 0) {
      ret = fns->file_notify(buffer);
      printf("Notified %d Receivers of File Path: %s\n", ret, buffer);
    }
    else
      ask_again = false;

  } while(ask_again);

  printf("Closing Server\n");
  fns->close(true);
}

int vifStartServer( char* vicarPort, char* fnSenderPort ) {
  // Open socket to listen for clients receiving file notification messages
  Simple_Sender_Handler fnsh;
  FN_Sender* fns = FN_Sender::init( &fnsh );
  int ret;

  ret = fns->start( atoi( fnSenderPort ) );

  if ( ret == FN_RETURN_SUCCESS ) {
    printf( "Listening for notification recipients on port %s\n", fnSenderPort );
  } else {
    FN_Util::print_err( ret );
    return -1;
  }

  // Open socket to listen for clients sending file notification messages
  Standard_Socket sock;

  printf( "Attempting to start server listening for notification from VICAR clients on port %s\n", vicarPort );

  if ( ( ret = sock.listen( atoi( vicarPort ) ) ) == FN_RETURN_SUCCESS )
    printf( "Server waiting for notification messages\n" );
  else {
    FN_Util::print_err( ret );
    return -1;
  }

  // Wait for notification messages
  notify_loop( sock );

  return 0;
}

int vifNotifyServer( char* ip, char* port, char* fileName ) {
  Standard_Socket sock;
  int ret;

  printf( "Attempting to connect to %s:%s\n", ip, port );

  if ( ( ret = sock.connect( ip, atoi( port ) ) ) == FN_RETURN_SUCCESS )
    printf( "Connection to server established\n" );
  else {
    FN_Util::print_err( ret );
    return 0;
  }

  int size = strlen( fileName );

  sock.write_n( &size, sizeof( size ) );
  sock.write_n( fileName, size );
  
  sock.close();

  return 0;
}
