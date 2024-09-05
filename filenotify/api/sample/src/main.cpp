#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "FN_Sender.h"
#include "FN_Receiver.h"
#include "Simple_Receiver_Handler.h"
#include "Simple_Sender_Handler.h" 
#include "FN_Defs.h"
#include "FN_Util.h"

//print usage statment and exit
void usage(char* name) {
	printf("Possible Usages:\n");
	printf("%s sender <port number>\n", name);
	printf("%s receiver <ip address> <port>\n", name);

	exit(0);
}

int notify_loop() {
	char buffer[501];
	bool ask_again = true;
	FN_Sender* fns = FN_Sender::instance();
	int ret;

        printf("Begin Entering File Path's to Send to the Receiver\n");
        printf("Enter \"quit\" to Shutdown\n");

        do {
                //acquire file path from stdin
                memset(buffer, 0, sizeof(buffer));
                printf("Please Enter Fake File Path: ");
                fgets(buffer, sizeof(buffer), stdin);

                //get rid of newline character
                if(buffer[strlen(buffer) - 1] == '\n')
                         buffer[strlen(buffer) - 1] = 0;

                //check if server should be closed
                if(strcmp(buffer, "quit") != 0) {
                        ret = fns->file_notify(buffer);
			//ret = fns->dir_notify(buffer);
                        printf("Notified %d Receivers of File Path: %s\n", ret, buffer);
                }
		else {
			ask_again = false;
		}

        } while(ask_again);

        printf("Closing Sender...\n");
        fns->close(true);

        return 0;

}

//run the server
int run_sender(char* port) {
        Simple_Sender_Handler fnsh;
//        FN_Sender* fns = FN_Sender::init(SERVER_CERT, SERVER_KEY, SERVER_CA, 
//					 "pass123", &fnsh);
	FN_Sender* fns = FN_Sender::init(&fnsh);
        int ret;

        //start the server
        ret = fns->start(atoi(port));

        if(ret == FN_RETURN_SUCCESS) {
                printf("Sender Listening on Port %s\n", port);
        } else {
                FN_Util::print_err(ret);
		return -1;
        }

	notify_loop();

        return 0;
}

int run_receiver(char* ip, char* port) {
        Simple_Receiver_Handler srh;
	//FN_Receiver fnr(&srh, CLIENT_CERT, CLIENT_KEY, CLIENT_CA, "pass123");
	FN_Receiver fnr(&srh);
	int ret;

        printf("Attempting to Connect to %s:%s\n", ip, port);
	if((ret = fnr.connect(ip, atoi(port))) == FN_RETURN_SUCCESS) { 
        	printf("Connection to Sender Closed Successfully...\n\n");
        }
	else {
		FN_Util::print_err(ret);
		return -1;
	}

	return 0;
}

int main(int argc, char *argv[]) {
	
	//ensure the minimum number of arguments are given
	if(argc < 3) {
		usage(argv[0]);
	}

	if(strcmp(argv[1], "sender") == 0) {
		run_sender(argv[2]); //run sender code
	}
	else if((strcmp(argv[1], "receiver") == 0) && (argc >= 4)) {
		run_receiver(argv[2], argv[3]); //run receiver code
	} else {
		usage(argv[0]); //print usage statement and exit
	}
	
	return 0;
}


