/*
|-------------------------------------------------------------------------------
| Scheme Interpreter
|-------------------------------------------------------------------------------
| Copyright (C) 2020 by Daniel Fors - All Rights Reserved
|
| This is an interpreter for a Scheme dialect inspired by 'The Little Schemer'.
| There are some differences in syntax to the dialect used in the book.
| Syntax rules:
| Atom	 : .+
| Number : [0-9]+
| List	 : '{' Expr* '}'
| Call	 : '(' Atom Expr* ')'
| Expr	 : Atom | Number | List | Call
| 
| Lists are constructed with '{ }' instead of '( )'. Only calls to functions use
| parenthesis. List = {this {is} a list}. (number? 10) calls function 'number?' 
| with argument '10'.
|
| Cond takes a list of tuples. Example:
|	(cond 
|		{(null? l) #t}
|		{(atom? (car l)) (lat? (crd l))}
|		{else #f}
|	)
|
| TODO(Fors):
| - Add command line options for file/repl.
| - File reading was a fast job, needs a look.
| - Trim/scan trailing whitespace for a '\' after an input lite.
|
*/

#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stdarg.h>

#include "internals.c"
#include "lexer.c"
#include "parser.c"

char *read_line(char *prompt) {
	static char buffer[2048];
	printf(prompt);
	fgets(buffer, 2048, stdin);
	char *str = xmalloc(strlen(buffer) + 1);
	strcpy(str, buffer);
	str[strlen(str) - 1] = '\0';
	return str;
}

char *read(char *prompt) {
	char *str = read_line(prompt);
	while (str[strlen(str) - 1] == '\\') {
		str[strlen(str) - 1] = '\n';
		char *temp = read_line(prompt);
		str = xrealloc(str, strlen(str) + strlen(temp) + 1);
		strcat(str, temp);
		xfree(temp);
	}
	return str;
}

void read_eval_print(Env *env, char *input) {
	init_lexer(input);
	while (!is_token(TOKEN_EOF)) {
		Expr *expr = parse_expr();
		Expr *e = eval_expr(env, expr);
		print_expr(e);
		printf("\n");
		delete_expr(e);
	}
}

void repl(Env *env) {
	for (;;) {
		// Output our prompt
		char *input = read(">");
		init_lexer(input);
		//while (!is_token(TOKEN_EOF)) {
			Expr *expr = parse_expr();
			Expr *e = eval_expr(env, expr);
			for (int i = 0; i < 0; i++) { // TODO: Remove
				init_lexer(input);
				Expr *ex = parse_expr();
				Expr *ev = eval_expr(env, ex);
				delete_expr(ev);
			}
			print_expr(e);
			printf("\n");
			delete_expr(e);
		//}
		xfree(input);
	}
}

int main(char argc, char **argv) {
	Env *global_scope = new_env(NULL);
	new_intern();
	init_builtins(global_scope);

	if (argc > 1) {
		FILE *file = fopen(argv[1], "r");
		if (!file) {
			printf("Could not open file: %s", argv[1]);
			exit(1);
		}
		if (fseek(file, 0, SEEK_END) != 0) {
			printf("Failed to set the file position of the stream to end of file.\n");
			fclose(file);
			exit(1);
		}
		size_t size = ftell(file);
		if (size == -1) {
			printf("Failed to set the file position of the stream to end of file.\n");
			fclose(file);
			exit(1);
		}
		if (fseek(file, 0, SEEK_SET) != 0) {
			printf("Failed to set the file position of the stream to the start.\n");
			fclose(file);
			exit(1);
		}

		char *text = xmalloc(size + 1);
		size_t read = fread(text, 1, size, file);
		if (fclose(file) != 0) {
			printf("Failed to close the file.\n");
			exit(1);
		}
		text[read] = 0;

		read_eval_print(global_scope, text);
		xfree(text);
	}

	repl(global_scope);
	return 0;
}