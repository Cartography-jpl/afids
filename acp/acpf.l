/* scanner for afidsexec parameter file */
%option noyywrap

%{
#include <stdlib.h>
#include "acpb.h"

#if 0
char* preLastToken = (char*) 0;
char* lastToken = (char*) 0;

void saveToken( char* token ) {
    printf( "saving %s\n", token );
    if ( lastToken ) {
	free( preLastToken );
	preLastToken = lastToken;
    }
    lastToken = strdup( token );
}
#endif

/*char* nameBuf = (char*) 0;*/
char** tokBuf = (char**) 0;
int tokCnt = 0;

void resetTokenList() {
    int i;

    for ( i = 0; i < tokCnt; ++i )
	free( tokBuf[ i ] );
    tokCnt = 0;
    free( tokBuf );
    tokBuf = (char**) 0;
}

void addToken( char* token ) {
    int i;
    char** newBuf = (char**) 0;

    ++ tokCnt;
    newBuf = (char**) malloc( tokCnt * sizeof( char* ) );
    for ( i = 0; i < tokCnt - 1; ++ i )
        newBuf[ i ] = tokBuf[ i ];
    newBuf[ tokCnt - 1 ] = strdup( token );
    free( tokBuf );
    tokBuf = newBuf;
}

%}

COM	^#[^\n]*\n
TOK	[ \t]*(\\\{|\\\}|\\\n|\\,|\\|\\=|[^=,\\\n \t\{\}])+(\\\{|\\\}|\\\n|\\,|\\|\\=|[^=,\\\n\{\}])*

%%

{COM}	return COMMENT;
{TOK}	{ addToken( yytext ); return TOKEN; }
"="	return EQUALS;
","	return COMMA;
"\n"	return EOL;
"{"	return LBRACE;
"}"	return RBRACE;
[ \t]+ /* eat whitespace */

%%
