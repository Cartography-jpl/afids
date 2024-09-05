/* FN_Receiver_Handler.h
 *
 * The FN_Receiver Handler is an abstract class used as an
 * interface for a callback object which will receive events 
 * from the FN_Receiver class.
 *
 */

#ifndef __FN_RECEIVER_HANDLER_H__
#define __FN_RECEIVER_HANDLER_H__

#include <string>

class FN_Receiver_Handler
{
    public:
  virtual ~FN_Receiver_Handler() {}

	/* This method will be called by the FN_Receiver after
         * it has successfully connected to the server.
	 *
  	 * @param ip - The ip address of the server
	 * @param port - The server port to connect to
	 */
	virtual void handle_connection(std::string ip, int port) = 0;

	/* This method will be called by the FN_Receiver when a
	 * file notification is received from the server
	 *
	 * @param file - The full path to the available file
	 */
	virtual void handle_file_notification(std::string file) = 0; 

	/* This method will be called by the FN_Receiver when a
         * directory notification is received from the server
         *
         * @param dir - The  path to the available directory
         */
	virtual void handle_dir_notification(std::string dir) = 0;

	/* This method will be called by the FN_Receiver when 
	 * the server notifies that FN_Receiver that it wishes 
	 * to shutdown.
	 *
	 * @param response_requested - If this value is true then the
	 * server indicated that it wants a response from the FN_Receiver
	 * before it shutsdown.  If this value is true the reponse will be
	 * sent to the server immediatly after this method returns.
	 */
	virtual void handle_close_notification(bool resposne_requested) = 0;
};

#endif

