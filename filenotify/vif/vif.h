#ifndef _VIF_H_
#define _VIF_H_

int vifStartServer( char* vicarPort, char* fnSenderPort );
int vifNotifyServer( char* ip, char* port, char* fileName );

#endif

