#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <signal.h>
#include <vector>
#include <errno.h>

#include "Standard_Socket.h"
#include "FN_Defs.h"

//set default values
Standard_Socket::Standard_Socket() : sock_fd_(-1), ip_(""), port_(0) {
}

//private constructor used for newly accept connections
Standard_Socket::Standard_Socket(const int sock_fd,
				 const std::string ip,
				 const int port) : 
	sock_fd_(sock_fd),
	ip_(ip),
	port_(port) {
}

//empty destructor
Standard_Socket::~Standard_Socket() {
}

//have socket listen on the give port
int Standard_Socket::listen(const int port) {

	//check to see if socket is already open
	if(sock_fd_ >= 0) {
		return FN_SOCK_ALREADY_OPEN_ERR;
	}

        struct sockaddr_in serv_addr;
	port_ = port;
	
        //begin to setup server
        sock_fd_ = socket(AF_INET, SOCK_STREAM, 0);
        if(sock_fd_ < 0) {
                return FN_SOCK_OPEN_ERR;
        }

	int val = 1;
	if ( setsockopt( sock_fd_, SOL_SOCKET, SO_REUSEADDR, &val, sizeof( val ) ) < 0 )
	  return FN_SOCK_OPEN_ERR;

        memset( (char*) &serv_addr, 0, sizeof(serv_addr));
        serv_addr.sin_family = AF_INET;
        serv_addr.sin_addr.s_addr = INADDR_ANY;
        serv_addr.sin_port = htons(port);

        //bind socket
        if(bind(sock_fd_, (struct sockaddr*) &serv_addr, sizeof(serv_addr)) < 0) {
		close();
                return FN_SOCK_BIND_ERR;
        }

        //listen on port for a connection
        ::listen(sock_fd_, 5);

        return FN_RETURN_SUCCESS;
}

//accept an incoming connection
FN_Socket* Standard_Socket::accept() {

	//make sure socket is already opened
	if(sock_fd_ < 0)
		return NULL;

        int sock_fd, clilen;
        struct sockaddr_in cli_addr;

	//accept new connection
        clilen = sizeof(cli_addr);
        sock_fd = ::accept(sock_fd_,
                       (struct sockaddr*) &cli_addr,
                       (socklen_t*) &clilen);

	//make sure connection was accepted successfully
        if(sock_fd < 0) {
                return NULL;
        }

        char c_ip[46];
        int c_port;
        inet_ntop(AF_INET, &cli_addr.sin_addr, c_ip, sizeof(c_ip));
        c_port = ntohs(cli_addr.sin_port);

	//return a new socket object for the accepted connection
	return ( new Standard_Socket(sock_fd, c_ip, c_port) ); 
}

//connect to a give address
int Standard_Socket::connect(const char* ip, const int port) {

	//make sure socket is not already open
	if(sock_fd_ >= 0)
		return FN_SOCK_ALREADY_OPEN_ERR;

        struct sockaddr_in serv_addr;

        sock_fd_ = socket(AF_INET, SOCK_STREAM, 0);
        if(sock_fd_ < 0) {
                //printf("Error Opening Socket\n");
                return FN_SOCK_OPEN_ERR;
        }

        memset( (char*) &serv_addr, 0, sizeof(serv_addr));

        serv_addr.sin_family = AF_INET;
        inet_pton(AF_INET, ip, &serv_addr.sin_addr);
        serv_addr.sin_port = htons(port);

	//make connection
        if(::connect(sock_fd_, (struct sockaddr*) &serv_addr, sizeof(serv_addr)) < 0) {
               //printf("Error Connecting\n");
		close();
                return FN_SOCK_CONN_ERR;
        }

	ip_ = ip;
	port_ = port;
	
	return FN_RETURN_SUCCESS;
}            

//read data of the socket
int Standard_Socket::read_n(void* vdata, const int n) {

	//make sure the socket is open
	if(sock_fd_ < 0)
		return FN_SOCK_NOT_OPEN_ERR;

        int nleft = n;
        int nread = 0;
        char* data = (char*) vdata;

	//read the requested number of bytes off the socket
        while(nleft > 0) {
                if( (nread = read(sock_fd_, data, nleft)) < 0) {
                        if(errno == EINTR) {
                                nread = 0;
                        }
                        else {
                                return FN_SOCK_READ_ERR;
                        }
                }
                else if(nread == 0) {
                        break;
                }

                nleft -= nread;
                data += nread;
        }

        return (n - nleft);
}

//write data to the socket
int Standard_Socket::write_n(const void* vdata, const int n) {

	//make sure socket is open
	if(sock_fd_ < 0)
		return FN_SOCK_NOT_OPEN_ERR;

        int nleft = n;
        int nwritten = 0;
        const char* data = (char*) vdata;

	//write the requested number ob bytes to the socket
        while(nleft > 0) {
                if( (nwritten = write(sock_fd_, data, nleft)) <= 0) {
                        if(errno == EINTR) {
                                nwritten = 0;
                        }
                        else if(errno == EPIPE) {
				close();
                                return FN_SOCK_PIPE_ERR;
                        }
                        else {
                                return FN_SOCK_WRITE_ERR;
                        }
                }

                nleft -= nwritten;
                data += nwritten;
        }

        return n;
}

//close the socket
int Standard_Socket::close() {

	//make sure the socket is open
	if(sock_fd_ < 0)
		return FN_SOCK_NOT_OPEN_ERR;

	
	::close(sock_fd_);
	sock_fd_ = -1;
	ip_ = "";
	port_ = 0;

        return FN_RETURN_SUCCESS;
}

