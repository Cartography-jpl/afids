#ifndef __OPEN_SSL_SOCKET_H__
#define __OPEN_SSL_SOCKET_H__

#include <string>

#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/x509v3.h>

#include <pthread.h>

#include "FN_Socket.h"


struct CRYPTO_dynlock_value {
	pthread_mutex_t mutex;
};

class Open_SSL_Socket : public FN_Socket
{
    public:

        Open_SSL_Socket(const char* certfile, const char* key, 
			const char* ca, char* pw = 0);
        virtual ~Open_SSL_Socket();

	virtual int listen(const int port);
	virtual FN_Socket* accept();
	virtual int connect(const char* ip, const int port);
	virtual int close();
	virtual int read_n(void* vdata, const int n);
	virtual int write_n(const void* vdata, const int n);

	virtual std::string get_ip() { return ip_; }
	virtual int get_port() { return port_; }

    private:
	std::string ip_;
	int port_;
	bool init_;
	BIO* acc_;
	SSL_CTX* ctx_;
	SSL* ssl_;

	static pthread_mutex_t* mutex_buf;

	Open_SSL_Socket(SSL* ssl, const char* ip, const int port);
	bool init_OpenSSL(const char* certfile, const char* key, 
			  const char* ca, char* pw);
	int thread_setup();
	int thread_cleanup();	

	static void locking_function(int mode, int n, const char*, int);
	static unsigned long id_function();
	static struct CRYPTO_dynlock_value* dyn_create_function(const char*, int);
	static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* mutex,
				      const char*, int);
	static void dyn_destroy_function(struct CRYPTO_dynlock_value* mutex,
					 const char*, int);	
	static int verify_callback(int ok, X509_STORE_CTX* store);
	static int pem_passwd_cb(char* buf, int size, int, void* pw);
};

#endif

