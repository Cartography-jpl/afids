#ifndef __FN_SENDER_HANDLER_H__
#define __FN_SENDER_HANDLER_H__

#include <string>

class FN_Sender_Handler
{
    public:
  virtual ~FN_Sender_Handler() {}
	virtual void handle_error(int error_num) = 0;
	virtual void handle_connection(std::string ip, int port) = 0;
};

#endif

