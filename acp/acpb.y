%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
extern char* yytext;
extern char* preLastToken;
void
yyerror (const char *s)  /* Called by yyparse on error */
{
  printf ("%s\n", s);
}
#define YYDEBUG 0
int yydebug=0;

#if 0
char* nameBuf = (char *) 0;
char** valsBuf = (char**) 0;
int valCnt = 0;

void name( char* token ) {
    int i;

    free( nameBuf );
    nameBuf = strdup( token );
    for ( i = 0; i < valCnt; ++i )
	free( valsBuf[ i ] );
    valCnt = 0;
    free( valsBuf );
    valsBuf = (char**) 0;
}

void multiVal( char* token ) {
    int i;
    char** newBuf = (char**) 0;

    printf( "multiVal %s\n", token );

    ++ valCnt;
    newBuf = (char**) malloc( valCnt * sizeof( char* ) );
    for ( i = 0; i < valCnt - 1; ++ i )
        newBuf[ i ] = valsBuf[ i ];
    newBuf[ valCnt - 1 ] = strdup( token );
    free( valsBuf );
    valsBuf = newBuf;
}
#endif

extern char** tokBuf;
extern int tokCnt;
extern void resetTokenList();

char *trim(char *str) {
   char *out = str;
   int len = strlen(str);
   int i;
   for (i=0; i<len && isspace(str[i]); i++, out++);
   for (i=len-1; i>=0 && isspace(str[i]); str[i]=0, i--);
   return strdup(out);
}

void printRule() {
    int i;
    char* name = trim( tokBuf[ 0 ] );

    printf( "{%s %d ", name, tokCnt - 1 );
    free( name );
    for ( i = 1; i < tokCnt; ++i ) {
	if (i > 1) printf( " " );
	printf( "{%s}", tokBuf[ i ] );
    }
    printf( "} " );
}

%}

%defines
%token COMMENT
%token TOKEN
%token EQUALS
%token COMMA
%token EOL
%token LBRACE
%token RBRACE

%%

rules:
	|
	rules rule

rule:	EOL /* skip empty lines */
	| COMMENT /* skip */
	| TOKEN EQUALS TOKEN EOL { printRule(); resetTokenList(); }
	| TOKEN EQUALS LBRACE values RBRACE EOL { printRule(); resetTokenList(); }
;

values:	TOKEN
	| values COMMA TOKEN
;

%%

int
main (void)
{
    return yyparse();
}
