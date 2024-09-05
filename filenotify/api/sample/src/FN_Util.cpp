#include "FN_Util.h"
#include "FN_Defs.h"
#include <stdio.h>
#include <unistd.h>

void FN_Util::print_err(int err) {
        switch (err) {
                case FN_SOCK_OPEN_ERR  :
                        printf("Error Opening Socket\n");
                        break;
                case FN_SOCK_BIND_ERR  :
                        printf("Error Binding to Socket\n");
                        break;
                case FN_SOCK_CONN_ERR  :
                        printf("Error Opening Connection\n");
                        break;
                case FN_SOCK_READ_ERR  :
                        printf("Error Reading From Socket\n");
                        break;
                case FN_SOCK_WRITE_ERR :
                        printf("Error Writing to Socket\n");
                        break;
                case FN_VERSION_ERR    :
                        printf("Receiver With Invalid Version Attempted to Connect");
                        break;
                case FN_NO_CONN_ERR    :
                        printf("Error, Currently Not Connected to Any Receivers\n");
                        break;
                case FN_CONN_NOT_FOUND_ERR    :
                        printf("Error, Connection Could Not Be Found\n");
                        break;
                case FN_CONN_EXISTS_ERR    :
                        printf("Error, Conection Already Exists\n");
                        break;
                case FN_UNEXPECTED_OP_CODE_ERR    :
                        printf("Error, An Unexpected Op Code was Received\n");
                        break;
                case FN_SOCK_PIPE_ERR       :
                        printf("Error, Broken Pipe\n");
                        break;
                case FN_SERVER_ALREADY_STARTED_ERR       :
                        printf("Error, The Server Has Already Been Started\n");
                        break;
                default:
                        printf("Unknown Error Occured, Code: %d\n", err);
        }

}


