#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "Open_SSL_Socket.h"
#include "FN_Defs.h"

pthread_mutex_t* Open_SSL_Socket::mutex_buf = NULL;

Open_SSL_Socket::Open_SSL_Socket(const char* certfile, const char* key, 
				 const char* ca, char* pw) :  
	ip_(""), 
	port_(0),
	acc_(0),
	ctx_(0),
	ssl_(0)
{
	init_ = init_OpenSSL(certfile, key, ca, pw);	
}

Open_SSL_Socket::Open_SSL_Socket(SSL* ssl, const char* ip, const int port) :
        ip_(ip),
        port_(port),
        acc_(0),
        ctx_(0),
        ssl_(ssl)
{
}


//empty destructor
Open_SSL_Socket::~Open_SSL_Socket() {
	close();
}

int Open_SSL_Socket::listen(const int port) {

        if(!init_)
                return FN_SSL_INIT_FAILURE_ERR;

	char char_port[6];
	memset(char_port, 0, 6);
	sprintf(char_port, "%d", port);
	
	//create server socket
	acc_ = BIO_new_accept(char_port);
	if(!acc_) {
		return FN_SOCK_OPEN_ERR;
	}

	//bind server socket
	if(BIO_do_accept(acc_) <= 0) {
		return FN_SOCK_BIND_ERR;
	}
	
	port_ = port;

        return FN_RETURN_SUCCESS;
}

FN_Socket* Open_SSL_Socket::accept() {

	if(!init_) {
//		printf("::accept init error\n");
		return NULL;
	}

        if(!acc_) {
//              printf("::accept acc error\n");
		return NULL;
	}

	if(BIO_do_accept(acc_) <= 0) {
//                printf("::accept BIO_do_accept error\n");
		return NULL;
	}

	BIO* client = BIO_pop(acc_);
	SSL* ssl;
	if(!(ssl = SSL_new(ctx_))) {
//		printf("::accept SSL_new error\n");
		return NULL;
	}

	SSL_set_bio(ssl, client, client);

	if(SSL_accept(ssl) <= 0) {
//              printf("::accept SSL_accept error\n");
		SSL_free(ssl);
		return NULL;
	}
	
        if(SSL_get_verify_result(ssl) != X509_V_OK) {
//                printf("::accept SSL_get_verify_results error\n");
		SSL_free(ssl);
		return NULL;
        }
	
	//connection established at this point

	return (new Open_SSL_Socket(ssl, "temp", 111));
}

//TODO: ADD MORE SPECIFIC ERROR RETURN CODES
int Open_SSL_Socket::connect(const char* ip, const int port) {
	
	if(!init_)
		return FN_SSL_INIT_FAILURE_ERR;

	if(ssl_)	
		return FN_SOCK_ALREADY_OPEN_ERR;

	char addr[25];
	sprintf(addr, "%s:%d", ip, port);
	BIO* conn = BIO_new_connect(addr);	 

	if(!conn) {
//		printf("Error creating bio connection\n");
		return FN_SOCK_CONN_ERR;
	}

	if(BIO_do_connect(conn) <= 0) {
//		printf("Error connecting to remote machine\n");
		return FN_SOCK_CONN_ERR;
	}

	if(!(ssl_ = SSL_new(ctx_))) {
//		printf("Error creating an SSL context\n");
		return FN_SOCK_CONN_ERR;
	}

	SSL_set_bio(ssl_, conn, conn);

	if(SSL_connect(ssl_) <= 0) {
//		printf("Error connecting SSL object\n");
		return FN_SOCK_CONN_ERR;
	}

	if(SSL_get_verify_result(ssl_) != X509_V_OK) {
//                printf("Error verifying SSL connection\n");
		return FN_SOCK_CONN_ERR;
	}

	return FN_RETURN_SUCCESS;
}            

//TODO: modify read method
int Open_SSL_Socket::read_n(void* vdata, const int n) {

        //make sure the socket is open
        if(!ssl_)
                return FN_SOCK_NOT_OPEN_ERR;

        int nleft = n;
        int nread = 0;
        char* data = (char*) vdata;

        //read the requested number of bytes off the socket
        while(nleft > 0) {
                if( (nread = SSL_read(ssl_, data, nleft)) < 0) {
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

//TODO modify write method
int Open_SSL_Socket::write_n(const void* vdata, const int n) {


        //make sure socket is open
        if(!ssl_)
                return FN_SOCK_NOT_OPEN_ERR;

        int nleft = n;
        int nwritten = 0;
        const char* data = (char*) vdata;

        //write the requested number ob bytes to the socket
        while(nleft > 0) {
                if( (nwritten = SSL_write(ssl_, data, nleft)) <= 0) {
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

	return 0;
}

int Open_SSL_Socket::close() {

	//TODO: LEARN MORE ABOUT SHUTDOWN AND CLEAR
	if(ssl_) {
		if(SSL_get_shutdown(ssl_) & SSL_RECEIVED_SHUTDOWN) {		
			SSL_shutdown(ssl_);
		} 
		else {
			SSL_clear(ssl_);
		}	

		SSL_free(ssl_);
		ssl_ = NULL;
	}

	if(acc_) {
		BIO_free(acc_);
		acc_ = NULL;
	}

	if(ctx_) {
		SSL_CTX_free(ctx_);
		ctx_ = NULL;
	}

        return FN_RETURN_SUCCESS;
}

//TODO MAKE ERROR RETURN MORE EXPLICIT 
bool Open_SSL_Socket::init_OpenSSL(const char* certfile, const char* key, 
				   const char* ca,  char* pw) {

	//init library
        if(!thread_setup() ||  !SSL_library_init()) {
                return false;
        }

	//load error strings
        SSL_load_error_strings();

	//seed prng
	if(!RAND_load_file("/dev/urandom", 1024))
		return false;

	//setup ctx
        ctx_ = SSL_CTX_new(SSLv23_method());

	if(pw) {
		SSL_CTX_set_default_passwd_cb_userdata(ctx_, pw);
		SSL_CTX_set_default_passwd_cb(ctx_, pem_passwd_cb);
	}

	if(SSL_CTX_load_verify_locations(ctx_, ca, NULL) != 1) {
		return false;
	}

	if(SSL_CTX_set_default_verify_paths(ctx_) != 1) {
		return false;
	}

        if(SSL_CTX_use_certificate_chain_file(ctx_, certfile) != 1) {
                return false;
        }

        if(SSL_CTX_use_PrivateKey_file(ctx_, key, SSL_FILETYPE_PEM) != 1) {
                return false;
        }

	SSL_CTX_set_mode(ctx_, SSL_MODE_AUTO_RETRY);

	SSL_CTX_set_verify(ctx_, SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT,
			   verify_callback);

	SSL_CTX_set_verify_depth(ctx_, 5);

        return true;
}

void Open_SSL_Socket::locking_function(int mode, int n, 
				       const char*, int ) {
	if(mode & CRYPTO_LOCK) 
		pthread_mutex_lock(&(mutex_buf[n]));
	else
                pthread_mutex_unlock(&(mutex_buf[n]));
}

unsigned long Open_SSL_Socket::id_function() {
	return ( (unsigned long) pthread_self()  );
}

int Open_SSL_Socket::thread_setup() {
	int i;

	mutex_buf = new pthread_mutex_t[CRYPTO_num_locks()];
	if (!mutex_buf)
		return 0;

	for(i = 0; i < CRYPTO_num_locks(); i++) {
		pthread_mutex_init(&(mutex_buf[i]), NULL);
	}

	CRYPTO_set_id_callback(id_function);
	CRYPTO_set_locking_callback(locking_function);

	CRYPTO_set_dynlock_create_callback(dyn_create_function);
	CRYPTO_set_dynlock_lock_callback(dyn_lock_function);
	CRYPTO_set_dynlock_destroy_callback(dyn_destroy_function);

	return 1;
}

int Open_SSL_Socket::thread_cleanup() {

	int i;
	
	if(!mutex_buf)	
		return 0;

	CRYPTO_set_id_callback(NULL);
	CRYPTO_set_locking_callback(NULL);
        CRYPTO_set_dynlock_create_callback(NULL);
        CRYPTO_set_dynlock_lock_callback(NULL);
        CRYPTO_set_dynlock_destroy_callback(NULL);


	for(i = 0; i < CRYPTO_num_locks(); i++) {
		pthread_mutex_destroy(&(mutex_buf[i]));	
	}

	delete [] mutex_buf;

	mutex_buf = NULL;

	return 1;
}

struct CRYPTO_dynlock_value* Open_SSL_Socket::dyn_create_function(const char*, int) {

	struct CRYPTO_dynlock_value* value;

	value = new (struct CRYPTO_dynlock_value);

	if(!value)
		return NULL;

	pthread_mutex_init(&(value->mutex), NULL);
	return value;
}

void Open_SSL_Socket::dyn_lock_function(int mode, struct CRYPTO_dynlock_value* mutex,
                                        const char*, int) {
	if(mode & CRYPTO_LOCK)
		pthread_mutex_lock(&(mutex->mutex));
	else
		pthread_mutex_unlock(&(mutex->mutex));
}

void Open_SSL_Socket::dyn_destroy_function(struct CRYPTO_dynlock_value* mutex,
                                           const char*, int) {
	pthread_mutex_destroy(&(mutex->mutex));
	delete mutex;
}

int Open_SSL_Socket::verify_callback(int ok, X509_STORE_CTX* store)
{
	char data[256];

	//********** Comment Out When Not Debugging ***********
	if(!ok) {
		X509* cert = X509_STORE_CTX_get_current_cert(store);
		int depth = X509_STORE_CTX_get_error_depth(store);
		int err = X509_STORE_CTX_get_error(store);

		printf("Error with certificate at depth %d\n", depth);
		X509_NAME_oneline(X509_get_issuer_name(cert), data, 256);
		printf("  issuer    = %s\n", data);
		X509_NAME_oneline(X509_get_subject_name(cert), data, 256);
		printf("  subject   = %s\n", data);
		printf("  err %d:%s\n", err, X509_verify_cert_error_string(err));
	}
	//****************************************************/

	return ok;
}

int Open_SSL_Socket::pem_passwd_cb(char* buf, int size, int, void* pw) {
	strncpy(buf, (char*) pw, size);
	buf[size - 1] = '\0';
	return (strlen(buf));
}
