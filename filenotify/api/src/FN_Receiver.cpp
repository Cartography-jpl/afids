#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "FN_Receiver.h"
#include "Frame_Headers.h"
#include "FN_Defs.h"
#include "Standard_Socket.h"
#include "Open_SSL_Socket.h"

//constructor
FN_Receiver::FN_Receiver(FN_Receiver_Handler* fnrh) : fnrh_(fnrh)  {
	sock_ = new Standard_Socket;	
}


FN_Receiver::FN_Receiver(FN_Receiver_Handler* fnrh, const char* certfile, 
			 const char* key, const char* ca, char* pw) : fnrh_(fnrh)  {
        sock_ = new Open_SSL_Socket(certfile, key, ca, pw);
}


//deconstructor
FN_Receiver::~FN_Receiver() {
	if(sock_)
		delete sock_;
}

//making an out going connection
int FN_Receiver::connect(const char* ip, int port) {
        Conn_Req_Header req;
	Conn_Resp_Header resp;
	int ret;

	//connect to the given address
	ret = sock_->connect(ip, port); 
	if(ret != FN_RETURN_SUCCESS) {
		return ret;
	}

	req.op_code = Conn_Req_Header::OP_CODE;
	req.version =  version;
	req.data_size = 0;

	//send version number
        if(sock_->write_n(&req, sizeof(req)) != sizeof(req)) {
                //printf("Error Writing to Socket\n");
		sock_->close();
		return FN_SOCK_WRITE_ERR;
        }

	//get version response
        if(sock_->read_n( &resp, sizeof(resp)) != sizeof(resp)) {
                //printf("Error Reading from Socket\n");
		sock_->close();
		return FN_SOCK_READ_ERR;
        } 

	//check for correct op code
	if(resp.op_code != Conn_Resp_Header::OP_CODE) {
		//printf("Unexpected Op Code Received\n");
		sock_->close();
		return FN_UNEXPECTED_OP_CODE_ERR;
	}

	//check that server recognized our version 
        if(resp.version_resp == Conn_Resp_Header::INVALID_VERSION) {
		//printf("Client Did not Accept Version\n");
		sock_->close();
		return FN_VERSION_ERR;
	}

	//tell event handler a connection was successfully made
	fnrh_->handle_connection(ip, port);

        return recv_notifications();
}


int FN_Receiver::recv_notifications() {
	int ret;

	//continue receiving messages until shutdown received or error occurs
	do {
		ret = recv_notification();
	} while(ret == FN_RETURN_SUCCESS);

	//return SUCCESS if shutdown was received otherwise return the error
	if(ret == FN_SHUTDOWN_RECEIVED)
		return FN_RETURN_SUCCESS;
	else
		return ret;
}

int FN_Receiver::recv_notification() {

        char* buffer = NULL;
        Generic_Header gh;
	int ret_val = FN_RETURN_SUCCESS;

	//read new msg from server
        if(sock_->read_n(&gh, sizeof(gh)) != sizeof(gh)) {
                //fnrh_->handle_error(FN_SOCK_READ_ERR);
        	ret_val = FN_SOCK_READ_ERR;
	}
	//check if server is shutting down
        else if(gh.op_code == Close_Req::OP_CODE) {
        	ret_val = FN_SHUTDOWN_RECEIVED;	
			
		//check if the server wants a response
		if( ((Close_Req*) &gh)->response) {
			fnrh_->handle_close_notification(true);

			Close_Resp resp;
			resp.op_code = Close_Resp::OP_CODE;
			resp.data_size = 0;
			sock_->write_n(&resp, sizeof(resp));
		}
		else {
			fnrh_->handle_close_notification(false);
		}
        }
	//check if there is a new dir notification
        else if (gh.op_code == File_Notify_Header::OP_CODE ||
		 gh.op_code == Dir_Notify_Header::OP_CODE) {

	        buffer = new char[gh.data_size + 1];
                memset(buffer, 0, gh.data_size + 1);

		//read new file location
                if(sock_->read_n(buffer, gh.data_size) != gh.data_size) {
			//fnrh_->handle_error(FN_SOCKET_READ_ERR);
                        ret_val =  FN_SOCK_READ_ERR;
                }
		else {
			if(gh.op_code == File_Notify_Header::OP_CODE) {
                    		fnrh_->handle_file_notification(buffer);
			}
			else { //must be Dir_Notify_Header::OP_CODE
				fnrh_->handle_dir_notification(buffer);
			}
		}
	}
        else {
		//fnrh_->handle_error(FN_UNEXPECTED_OP_CODE_ERR);
		ret_val =  FN_UNEXPECTED_OP_CODE_ERR;			
        }

        if(buffer)
                delete []  buffer;

        return ret_val;
}

