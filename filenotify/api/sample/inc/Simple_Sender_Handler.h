#ifndef __SIMPLE_SENDER_HANDLER_H__
#define __SIMPLE_SENDER_HANDLER_H__

#include <stdio.h>
#include <string>
#include "FN_Sender_Handler.h"
#include "FN_Util.h"

class Simple_Sender_Handler : public FN_Sender_Handler
{
    public:
  virtual ~Simple_Sender_Handler() {}

	virtual void handle_error(int error) {
		FN_Util::print_err(error);
	}

	virtual void handle_connection(std::string ip, int port) {
		printf("Connected to %s:%d...\n", ip.c_str(), port);
	}
};

#endif

