#ifndef __FN_SOCKET_H__
#define __FN_SOCKET_H__

class FN_Socket
{
    public:

        //FN_Socket();
        virtual ~FN_Socket() { }

	virtual int listen(int port) = 0;
	virtual FN_Socket* accept() = 0;
	virtual int connect(const char* ip, const int port) = 0;
	virtual int close() = 0;
	virtual int read_n(void* vdata, const int n) = 0;
        virtual int write_n(const void* vdata, const int n) = 0;

	virtual std::string get_ip() = 0;
	virtual int get_port() = 0;
};

#endif

