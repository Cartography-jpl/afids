#ifndef __SIMPLE_RECEIVER_HANDLER_H__
#define __SIMPLE_RECEIVER_HANDLER_H__

#include <stdio.h>
#include <string>
#include <stdlib.h>
#include "FN_Receiver_Handler.h"
#include "FN_Util.h"
#include <sys/time.h>

class Simple_Receiver_Handler : public FN_Receiver_Handler
{
    public:

/*
	virtual void handle_error(int error) {
		FN_Util::print_err(error);
	}
*/

	virtual void handle_connection(std::string ip, int port) {
		printf("Connected to %s:%d...\n", ip.c_str(), port);
	}

	virtual void handle_file_notification(std::string filename) {
		printf("Notified of File: %s\n", filename.c_str());
	}

	virtual void handle_dir_notification(std::string dirname) {
		printf("Notified of Dir: %s\n", dirname.c_str());
	}

	virtual void handle_close_notification(bool) {
		timeval t;
		gettimeofday(&t, NULL);
		srand(t.tv_usec);
		int sleep_time = rand() % 5;
		printf("Close request received from server\n");
		printf("Sleeping for %d seconds\n", sleep_time);
		sleep(sleep_time);
		printf("Done Sleeping\n");
	}
};

#endif

