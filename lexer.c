/*
|-------------------------------------------------------------------------------
| Lexer
|-------------------------------------------------------------------------------
| Copyright (C) 2020 by Daniel Fors - All Rights Reserved
|
| This is the lexer/tokenizer for the Scheme Interpreter.
| Tokens can be:
| Atom		 : .+
| Number	 : [0-9]+
| Expr-token : '(' | ')' | '{' | '}'
|
| Exception to Atoms: can't begin with a Number or contain an Expr-token.
|
| TODO(Fors):
| - Add memory arena?
| - Add stretch buffers?
|
*/

typedef enum TokenKind {
	TOKEN_EOF,
	TOKEN_ERROR,
	TOKEN_ATOM,
	TOKEN_NUMBER,
	TOKEN_LPAREN,
	TOKEN_RPAREN,
	TOKEN_LBRACE,
	TOKEN_RBRACE,
} TokenKind;

typedef struct Token {
	TokenKind kind;
	union {
		char *atom;
		uint64_t number;
		char *error;
	};
} Token;

Token token;
char *stream;
uint64_t lexer_line;

bool non_allowed_char(void) {
	return *stream == '('
		|| *stream == ')'
		|| *stream == '{'
		|| *stream == '}';
}

void scan_number(void) {
	uint64_t val = 0;
	while (isdigit(*stream)) {
		val *= 10;
		val += (uint64_t)*stream - '0';
		stream++;
	}
	if (*stream && !isspace(*stream) && !non_allowed_char()) {
		token.kind = TOKEN_ERROR;
		token.error = "Atoms can't start with a number.";
		return;
	}

	token.kind = TOKEN_NUMBER;
	token.number = val;
}

void next_token(void) {
	// Remove whitespace
	while (isspace(*stream)) {
		if (*stream == '\n') {
			lexer_line++;
		}
		stream++;
	}
	switch (*stream) {
	case '0': case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
		scan_number();
		break;
	case '(':
		stream++;
		token.kind = TOKEN_LPAREN;
		break;
	case ')':
		stream++;
		token.kind = TOKEN_RPAREN;
		break;
	case '{':
		stream++;
		token.kind = TOKEN_LBRACE;
		break;
	case '}':
		stream++;
		token.kind = TOKEN_RBRACE;
		break;
	case '\0':
		token.kind = TOKEN_EOF;
		break;
	default:
	{ 
		// NOTE(Fors): If it's not a number, parenthesis, or a brace, it's an atom.
		char *begin = stream;
		while (*stream && !isspace(*stream) && !non_allowed_char()) {
			stream++;
		}
		size_t len = stream - begin;
		// NOTE(Fors): Max 256 characters allowed in an atom.
		static char str[256];
		strcpy(str, begin);
		str[len] = 0;
		token.kind = TOKEN_ATOM;
		token.atom = intern_str(str);
		break;
	}
	}
}

void init_lexer(char *str) {
	stream = str;
	lexer_line = 1;
	next_token();
}

void expect_token(TokenKind kind) {
	if (token.kind != kind) {
		printf("Expected %d got %d", kind, token.kind);
		exit(1);
	}
	next_token();
}

bool match_token(TokenKind kind) {
	if (token.kind == kind) {
		next_token();
		return true;
	}
	return false;
}

bool is_token(TokenKind kind) {
	return token.kind == kind;
}