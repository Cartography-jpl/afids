/* FN_Receiver.h
 *
 * Receives notifcations about the availability of files and directories
 * from another process via a TCP/IP socket connection.  The FN_Receiver
 * object is a client side implementation of the 'File Notification Protocol.'
 */

#ifndef __FN_RECEIVER_H__
#define __FN_RECEIVER_H__

#include "FN_Receiver_Handler.h"
#include "FN_Socket.h"

class FN_Receiver
{
    public:
	//File Notification Protocol Version Implemented by this Receiver
	static const char version = 1;

	/*
	 * Constructor for Standard Receiver
	 */
        FN_Receiver(FN_Receiver_Handler* fnrh);

	/*
	 * Constructor for SSL Version 
         *
	 */
	
	FN_Receiver(FN_Receiver_Handler* fnrh, const char* certfile, 
		    const char* key, const char* ca, char* pw = 0);
	
        ~FN_Receiver();

	/*
	 * Connects to server and waits to receive notifications.  
	 * 
	 * This method will block until a shutdown is received from the server.  
	 * It will issue callbacks whenever a notification is received from 
	 * the server.
	 *
	 * @param ip - The ip address of the server you wish to connect to
	 * @param port - The port of the server you wish to connect to
	 *
	 * @return FN_RETURN_SUCCESS - There are no errors.  
	 *	   FN_SOCK_OPEN_ERR  - The socket used to create the connection
	 *	   cannot be opened.
	 *	   FN_SOCK_CONN_ERR - The socket connection fails.
	 *	   FN_SOCK_WRITE_ERR - The was an error while attempted to 
	 * 	   write the version number to the server
	 *	   FN_SOCK_READ_ERR - There was an error reading a notification
	 *	   from the server.
	 *	   FN_VERION_ERR - There server did not accept the version of 
	 *	   the 'File Notification Protocol' implemented by this client 
	 *
	 */
        int connect(const char* ip, int port);

    private:
	FN_Receiver_Handler* fnrh_;
	FN_Socket* sock_;

	int recv_notifications();
	int recv_notification();
};

#endif

