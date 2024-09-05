#ifndef __FRAME_HEADERS_H__
#define __FRAME_HEADERS_H__

struct Generic_Header {
	unsigned char op_code;
	unsigned char reserve[13];
	unsigned short data_size;
};

//op code 0
struct Conn_Req_Header {
	static const unsigned char OP_CODE = 0;

        unsigned char op_code;
	unsigned char version;
        unsigned char reserve[12];
        unsigned short data_size;
};

//op code 1
struct Conn_Resp_Header {
	static const char OP_CODE = 1;
	static const char VALID_VERSION = 1;
	static const char INVALID_VERSION = 0;

        unsigned char op_code;
	unsigned char version_resp;
        unsigned char reserve[12];
        unsigned short data_size;
};

//op code 2
struct File_Notify_Header {
	static const unsigned char OP_CODE = 2;

	unsigned char op_code;
	unsigned char reserve[13];
	unsigned short data_size;
};

//opcode 4
struct Dir_Notify_Header {
	static const unsigned char OP_CODE = 4;

	unsigned char op_code;
	unsigned char reserve[13];
	unsigned short data_size;
};

//op code 127
struct Close_Req {
	static const unsigned char OP_CODE = 127;

        unsigned char op_code;
	unsigned char response;
        unsigned char reserve[12];
        unsigned short data_size;
};

//op code 128
struct Close_Resp {
	static const unsigned char OP_CODE = 128;

	unsigned char op_code;
	unsigned char reserve[13];
	unsigned short data_size;
};

#endif

