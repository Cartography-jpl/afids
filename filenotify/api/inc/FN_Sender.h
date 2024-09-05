/* FN_Sender.h
 *
 * The FN_Sender is used to send notifications about the availability of files
 * and directories to other processes via a TCP/IP socket connection.  The
 * FN_Sender is a server side implementation of the 'File Notification Protocol.'
 * The FN_Sender is a thread safe singleton object.
 */


#ifndef __FN_SENDER_H__
#define __FN_SENDER_H__

#include <pthread.h>
#include <string>
#include <vector>

#include "FN_Sender_Handler.h"
#include "FN_Socket.h"
#include "Frame_Headers.h"

class FN_Sender
{

	//Used to destroy the instance of this singleton at program termination
	friend class FN_Sender_Destroyer;

    public:

	//File Notification Protocol Version Implemented by this Sender
	static const char version = 1;

        /*
         * Create a standard instance of the Sender
         *
         */
	static FN_Sender* init(FN_Sender_Handler* fnsh = 0);

        /*
         * Create an SSL instance of the Sender
         *
         */

	static FN_Sender* init(const char* certfile, 
			       const char* key, 
			       const char* ca,
			       FN_Sender_Handler* fnsh = 0);


        static FN_Sender* init(const char* certfile,
                               const char* key,
                               const char* ca,
			       char* pw,
                               FN_Sender_Handler* fnsh = 0);


	/*
	 * Return an instance of the Sender
	 */
	static FN_Sender* instance();

	/*
	 * Start the file notification server.
	 *
	 * This method will spawn a thread that will listen on the
         * given port for incoming connections and add new connections
	 * to the list of receivers that will receive notifications.
	 *
	 * @param port - The port for the server to listen on.
	 * @return FN_RETURN_SUCCESS - There are no errors
	 *	   FN_SERVER_ALREADY_STARTED - The server has already
	 *	   been started.
	 *	   FN_SOCK_BIND_ERR - There is a problem binding the
	 *	   socket to the given port.
	 */
	int start(int port);

	/*
	 * Notifies the list of connections of the availability of
	 * a file.
	 *
	 * @param file - The full path to the available file.
         * @return - The number of clients that were successfully sent  
	 * 	     a notification.
	 */
	int file_notify(const char* file);

        /*
         * Notifies the list of connections of the availability of 
	 * a directory.
         *
         * @param dir - The path to the available directory.
         * @return - The number of clients that were successfully sent
	 *	     a notification.
         */

	int dir_notify(const char* dir);

        /*
         * Notify the list of clients that the server is shutting down and 
         * then shutdown the server.
         *
         * @return - The number of clients successfully notified of the shutdown.
         */
	int close(bool wait);

    private:
	std::vector<FN_Socket*> socks_;
	pthread_mutex_t mutex_;
	FN_Sender_Handler* fnsh_;
	FN_Socket* sock_;
	bool server_started_;
	pthread_t thread_;
	static FN_Sender* instance_;
	static pthread_mutex_t imutex_;	


        FN_Sender(FN_Sender_Handler* fnsh);
	FN_Sender(FN_Sender_Handler* fnsh, const char* certfile, 
		  const char* key,  const char* ca, char* pw); 
        ~FN_Sender();
        int notify(const Generic_Header* gh, const char* data);
	int notify_sock(const Generic_Header* gh, const char* data, FN_Socket*);
	int close_wait();
	int close_immediate();
	int close_sock(FN_Socket*);
	int close_sock(FN_Socket*, pthread_t* thread);
	void incoming_connection(std::string ip, int port);
	void error_occured(int error_num);

	static void* start_server(void* arg);
	static void get_connection(FN_Sender* t_this);
	static void* get_close_reply(void* arg);
};

#endif

