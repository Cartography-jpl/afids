#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "FN_Sender.h"
#include "Mutex_Guard.h"
#include "Frame_Headers.h"
#include "FN_Defs.h"
#include "Standard_Socket.h"
#include "Open_SSL_Socket.h"

//------Used to Deallocate Singleton at Program Termination-----------

//declare this class in cpp file in order to keep it hidden
class FN_Sender_Destroyer {

        public:
        FN_Sender_Destroyer() { /*FN_Sender::instance();*/ }
        ~FN_Sender_Destroyer() { if(FN_Sender::instance_) delete FN_Sender::instance_; }
};

//delcare static destroyer at global scope to ensure deconstruction
static FN_Sender_Destroyer fn_sender_destroyer;

//--------------------------------------------------------------------


//initialize static member variables
FN_Sender* FN_Sender::instance_ = 0;
pthread_mutex_t FN_Sender::imutex_ = PTHREAD_MUTEX_INITIALIZER;

//standard constructor
FN_Sender::FN_Sender(FN_Sender_Handler* fnsh) : fnsh_(fnsh) {
	sock_ = new Standard_Socket;
	pthread_mutex_init(&mutex_, NULL);
	server_started_ = false;
}

//openssl constructor
FN_Sender::FN_Sender(FN_Sender_Handler* fnsh, const char* certfile, 
		     const char* key, const char* ca, char* pw) : fnsh_(fnsh) {
        sock_ = new Open_SSL_Socket(certfile, key, ca, pw);
	pthread_mutex_init(&mutex_, NULL);
}


FN_Sender::~FN_Sender() {
	close(false);

	if(sock_)
		delete sock_;

	pthread_mutex_destroy(&mutex_);
}

//initialize singleton
FN_Sender* FN_Sender::init(FN_Sender_Handler* fnsh) {

	//acquire lock
	pthread_mutex_lock(&imutex_);

	//make sure instance does not already exist 
	if(instance_) 
		return NULL;

	instance_ = new FN_Sender(fnsh);
		
	//release lock
	pthread_mutex_unlock(&imutex_);

	return instance_;
}


//initiailize ssl singleton
FN_Sender* FN_Sender::init(const char* certfile, const char* key, 
			   const char* ca, FN_Sender_Handler* fnsh) {
	return init(certfile, key, ca, NULL, fnsh);
}

FN_Sender* FN_Sender::init(const char* certfile, const char* key, const char* ca, 
			   char* pw, FN_Sender_Handler* fnsh) {
	//acquire lock
        pthread_mutex_lock(&imutex_);

        //make sure instance does not already exist
        if(instance_) 
		return NULL;

        instance_ = new FN_Sender(fnsh, certfile, key, ca, pw);

        //release lock
        pthread_mutex_unlock(&imutex_);

        return instance_;
}


FN_Sender* FN_Sender::instance() {
	return instance_;
}

int FN_Sender::start(int port) {
	int ret;

	Mutex_Guard g(&mutex_);

	//check to see if server is already started
	if(server_started_)
		return FN_SERVER_ALREADY_STARTED_ERR;

	//ignore the broken pipe signal
	signal(SIGPIPE, SIG_IGN);

	//listen on the given port
	ret = sock_->listen(port);
	if(ret != FN_RETURN_SUCCESS) {
		return ret;
	}

	server_started_ = true;

        //start server thread
        pthread_create(&thread_, 0, start_server, this);

        return FN_RETURN_SUCCESS;

}

int FN_Sender::file_notify(const char* file) {
        File_Notify_Header fnh;
        fnh.op_code = File_Notify_Header::OP_CODE;
        fnh.data_size = (unsigned short) strlen(file);

	return (notify( (Generic_Header*) &fnh, file));
}

int FN_Sender::dir_notify(const char* dir) {
        Dir_Notify_Header dnh;
        dnh.op_code = Dir_Notify_Header::OP_CODE;
        dnh.data_size = (unsigned short) strlen(dir);

        return (notify( (Generic_Header*) &dnh, dir  ));
}

//returns number of receiver's successfully notified
int FN_Sender::notify(const Generic_Header* gh, const char* data) {
	int num = 0;
	int ret;
	std::vector<std::vector<FN_Socket*>::iterator> bad_socks;

	//lock until method returns
	Mutex_Guard g(&mutex_);

	//check to see if any connections exist
	if(socks_.size() == 0) {
		error_occured(FN_NO_CONN_ERR);
		return 0;
	}

	//notify all connections
        for (std::vector<FN_Socket*>::iterator i = socks_.begin();
                 i != socks_.end(); i++) {

		ret = notify_sock(gh, data, *i);
                if(ret != FN_RETURN_SUCCESS) {
			error_occured(ret);			
		
			//delete all the dead sockets & store their iterator position
			if(ret == FN_SOCK_PIPE_ERR ||
			   ret == FN_SOCK_NOT_OPEN_ERR) {
				delete *i;
				bad_socks.push_back(i);
			}
		} 
		else {
			num++;
		}
        }

	//remove all dead sockets from vector
        for (std::vector<std::vector<FN_Socket*>::iterator>::iterator x =
		 bad_socks.begin(); x != bad_socks.end(); x++) {

		socks_.erase(*x);
	}

	return num;
}

//close all connections
int FN_Sender::close(bool wait) {
	int num = 0;

	//lock until method returns
	Mutex_Guard g(&mutex_);
	server_started_ = false;

	//interupt the accept() call that is blocking in the other thread
	pthread_kill(thread_, SIGPIPE);

	if(!wait) {
		num = close_immediate();
	}
	else {
		num = close_wait();
	} 	

	sock_->close();
	socks_.clear();

        return num;
}

int FN_Sender::close_wait() {
	int num = 0;
	int ret;

	std::vector<pthread_t*> threads;
	pthread_t* thread;

        for (std::vector<FN_Socket*>::iterator i = socks_.begin();
                 i != socks_.end(); i++) {

		thread = new pthread_t;

                if((ret = close_sock(*i, thread)) != FN_RETURN_SUCCESS) {
                        error_occured(ret);
			delete thread;
                }
                else {
                        num++;
			threads.push_back(thread);	
                }
        }

        for (std::vector<pthread_t*>::iterator i = threads.begin();
                 i != threads.end(); i++) {

		pthread_join(**i, NULL);
		delete *i;
	}

        for (std::vector<FN_Socket*>::iterator i = socks_.begin();
                 i != socks_.end(); i++) {
		(*i)->close();
		delete *i;
        }

        return num;
}


int FN_Sender::close_immediate() {
	int num = 0;
	int ret;

        for (std::vector<FN_Socket*>::iterator i = socks_.begin();
                 i != socks_.end(); i++) {

                if((ret = close_sock(*i)) != FN_RETURN_SUCCESS) {
                        error_occured(ret);
                }
                else {
                        num++;
                }

                delete *i;
        }

	return num;
}

void* FN_Sender::start_server(void* arg) {
        FN_Sender* t_this = (FN_Sender*) arg;

	//lock access to server_started_
	//continue to accept connections as long as server_started is true
	pthread_mutex_lock( &(t_this->mutex_));
	while(t_this->server_started_) {
		pthread_mutex_unlock( &(t_this->mutex_));
		get_connection(t_this);
		pthread_mutex_lock( &(t_this->mutex_));
	}
	 pthread_mutex_unlock( &(t_this->mutex_));

	return arg;
}

void FN_Sender::get_connection(FN_Sender* t_this) {
        Conn_Req_Header req;
        Conn_Resp_Header resp;

	//accept a connection
	FN_Socket* conn_sock = t_this->sock_->accept();

	//make sure connection was accepted successfully
        if(!conn_sock) {
                //printf("Error Accepting Connection\n");
		if(t_this->server_started_) {
			t_this->error_occured(FN_SOCK_CONN_ERR);
		}
		return;
        }

        //check version of client
        if(conn_sock->read_n(&req, sizeof(req)) != sizeof(req)) {
                //printf("Error Reading From Socket\n");
                conn_sock->close();
		delete conn_sock;
		t_this->error_occured(FN_SOCK_READ_ERR);
		return;
        }

        if(req.op_code != Conn_Req_Header::OP_CODE) {
                //printf("Unexpected Op Code Received\n");
                conn_sock->close();
		delete conn_sock;
		t_this->error_occured(FN_UNEXPECTED_OP_CODE_ERR);
		return;
        }

        resp.op_code = Conn_Resp_Header::OP_CODE;
        resp.data_size = 0;

        //shutdown if version does not match
        if(req.version != version) {
                resp.version_resp = Conn_Resp_Header::INVALID_VERSION;
                conn_sock->write_n(&resp, sizeof(resp));
                conn_sock->close();
		delete conn_sock;
		t_this->error_occured(FN_VERSION_ERR);
		return;
        }

        //tell client the connection was success
        resp.version_resp = Conn_Resp_Header::VALID_VERSION;
        if(conn_sock->write_n(&resp, sizeof(resp)) != sizeof(resp)) {
                //printf("Error Writing to Socket\n");
                conn_sock->close();
		delete conn_sock;
		t_this->error_occured(FN_SOCK_WRITE_ERR);
		return;
        }

	pthread_mutex_lock( &(t_this->mutex_) );
	//make sure server is still running
	if(t_this->server_started_) {
		t_this->incoming_connection(conn_sock->get_ip(), 
					    conn_sock->get_port());
		t_this->socks_.push_back(conn_sock);
	}
	else {
		t_this->close_sock(conn_sock);
		delete conn_sock;
	}
	pthread_mutex_unlock( &(t_this->mutex_) );
}

//only accessed by thread safe methods
int FN_Sender::close_sock(FN_Socket* sock) {

	Close_Req cr;
	cr.op_code = Close_Req::OP_CODE;
	cr.response = 0;
	cr.data_size = 0;

	//send shutdown msg to the receiver
        if (sock->write_n(&cr, sizeof(cr)) != sizeof(cr)) {
                //printf("Error Reading from Socket\n");
		sock->close();
                return FN_SOCK_WRITE_ERR;
        }

	//close socket
	sock->close();
	return FN_RETURN_SUCCESS;
}

//only accessed by thread safe methods
int FN_Sender::close_sock(FN_Socket* sock, pthread_t* thread) {

        Close_Req cr;
        cr.op_code = Close_Req::OP_CODE;
        cr.response = 1;
        cr.data_size = 0;

        //send shutdown msg to the receiver
        if (sock->write_n(&cr, sizeof(cr)) != sizeof(cr)) {
                //printf("Error Reading from Socket\n");
                return FN_SOCK_WRITE_ERR;
        }

	pthread_create(thread, NULL, get_close_reply, (void*) sock);

        //close socket
        return FN_RETURN_SUCCESS;
}

void* FN_Sender::get_close_reply(void* arg) {
	FN_Socket* sock = (FN_Socket*) arg;
	
	Close_Resp resp;

	//read call should block until the close response is received
	//there is no reason to check for opcode or error conditions
	//because an error should still cause this method to return
	//immediatly.
	sock->read_n(&resp, sizeof(resp));

	return arg;	
}

//only accessed by thread safe methods
int FN_Sender::notify_sock(const Generic_Header* gh, 
			   const char* data, 
			   FN_Socket* sock) {
	int ret;
	
	//write file notification header
	ret = sock->write_n(gh, sizeof(*gh));
	if(ret < 0)
		return ret;

	//read file notification data
	ret = sock->write_n(data, gh->data_size);
        if(ret < 0)
                return ret;

	
        return FN_RETURN_SUCCESS;
}

//send incoming connection address to the callback handler
void FN_Sender::incoming_connection(std::string ip, int port) {
	if(fnsh_)// && server_started_)
		fnsh_->handle_connection(ip, port);
}

//send error to the callback handler
void FN_Sender::error_occured(int error_num) {
	if(fnsh_)// && server_started_) 
		fnsh_->handle_error(error_num);
}

