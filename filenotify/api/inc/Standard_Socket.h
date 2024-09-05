#ifndef __STANDARD_SOCKET_H__
#define __STANDARD_SOCKET_H__

#include <string>

#include "FN_Socket.h"

class Standard_Socket : public FN_Socket
{
    public:

        Standard_Socket();
	//Standard_Socket(const int sock_fd, const std::string ip, const int port);
        virtual ~Standard_Socket();

	virtual int listen(const int port);
	virtual FN_Socket* accept();
	virtual int connect(const char* ip, const int port);
	virtual int close();
	virtual int read_n(void* vdata, const int n);
	virtual int write_n(const void* vdata, const int n);

	virtual std::string get_ip() { return ip_; }
	virtual int get_port() { return port_; }

	virtual bool is_open() { return sock_fd_; }
    private:
	int sock_fd_;
	std::string ip_;
	int port_;

	Standard_Socket(const int sock_fd, const std::string ip, const int port);
};

#endif

