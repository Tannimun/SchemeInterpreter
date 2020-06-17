/*
|-------------------------------------------------------------------------------
| Parser
|-------------------------------------------------------------------------------
| Copyright (C) 2020 by Daniel Fors - All Rights Reserved
|
| This is the Parser for the Scheme Interpreter. This is where the majority of 
| the work is done.
| Syntax rules:
| Atom	 : .+
| Number : [0-9]+
| List	 : '{' Expr* '}'
| Call	 : '(' Atom Expr* ')'
| Expr	 : Atom | Number | List | Call
|
| TODO(Fors):
| - There are a number of places where, instead of cloning, ownership can be 
|   moved. As long as pointer to the moved value is set to NULL, delete_expr 
|	will not free it. Example in 'cdr':
|		for (int i = 1; i < len; i++) {
|			exprs[i-1] = expr->call.args[0]->list.exprs[i];
|			expr->call.args[0]->list.exprs[i] = NULL;
|		}
| - Move the AST up a level, so atoms can be used in error messages for lamdda
|	expressions? Or alternatively, inject the atom into the call when getting
|	it from the environment?
| - Double check any missed error cases.
|
*/

typedef struct Env Env;
typedef struct Expr Expr;

Expr *clone_expr(Expr *);
void delete_expr(Expr *);
Expr *parse_expr();
Expr *eval_expr(Env *, Expr *);
void print_expr(Expr *);

#define err_str(...) string_format(512, __VA_ARGS__)

// ------------------------------------------ KEYWORDS ---------------------------------------------

char *builtin_define;
char *builtin_lambda;
char *builtin_car;
char *builtin_cdr;
char *builtin_cons;
char *builtin_cond;
char *builtin_else;
char *builtin_and;
char *builtin_or;
char *builtin_quote;
char *builtin_add;
char *builtin_sub;
char *builtin_mul;
char *builtin_div;
char *builtin_null_;
char *builtin_atom_;
char *builtin_number_;
char *builtin_zero_;
char *builtin_eq_;
char *builtin_add1;
char *builtin_sub1;

char *builtin_true;
char *builtin_false;

void init_builtins(Env *env) {
	builtin_define = intern_str("define");
	builtin_lambda = intern_str("lambda");
	builtin_car = intern_str("car");
	builtin_cdr = intern_str("cdr");
	builtin_cons = intern_str("cons");
	builtin_cond = intern_str("cond");
	builtin_else = intern_str("else");
	builtin_and = intern_str("and");
	builtin_or = intern_str("or");
	builtin_quote = intern_str("quote");
	builtin_add = intern_str("+");
	builtin_sub = intern_str("-");
	builtin_mul = intern_str("*");
	builtin_div = intern_str("/");
	builtin_null_ = intern_str("null?");
	builtin_atom_ = intern_str("atom?");
	builtin_number_ = intern_str("number?");
	builtin_zero_ = intern_str("zero?");
	builtin_eq_ = intern_str("eq?");
	builtin_add1 = intern_str("add1");
	builtin_sub1 = intern_str("sub1");
	builtin_true = intern_str("#t");
	builtin_false = intern_str("#f");
}

bool is_builtin(char *atom) {
	return atom == builtin_define
		|| atom == builtin_lambda
		|| atom == builtin_car
		|| atom == builtin_cdr
		|| atom == builtin_cons
		|| atom == builtin_cond
		|| atom == builtin_else
		|| atom == builtin_and
		|| atom == builtin_or
		|| atom == builtin_quote
		|| atom == builtin_add
		|| atom == builtin_sub
		|| atom == builtin_mul
		|| atom == builtin_div
		|| atom == builtin_null_
		|| atom == builtin_atom_
		|| atom == builtin_number_
		|| atom == builtin_zero_
		|| atom == builtin_eq_
		|| atom == builtin_add1
		|| atom == builtin_sub1;
}

// ----------------------------------------- ENVIRONMENT -------------------------------------------

struct Env {
	Env *parent;
	char **identifiers;
	Expr **values;
	size_t len;
	size_t cap;
};

Env *new_env(Env *parent) {
	Env *env = xcalloc(1, sizeof(Env));
	env->parent = parent;
	env->cap = 16;
	env->identifiers = xmalloc(env->cap * sizeof(char *));
	env->values = xmalloc(env->cap * sizeof(Expr *));
	return env;
}

void delete_env(Env *env) {
	for (int i = 0; i < env->len; i++) {
		delete_expr(env->values[i]);
	}
	xfree(env->values);
	xfree(env->identifiers);
	xfree(env);
}

void env_grow(Env *env) {
	env->cap *= 2;
	env->identifiers = xrealloc(env->identifiers, env->cap * sizeof(char *));
	env->values = xrealloc(env->values, env->cap * sizeof(Expr *));
}

void env_add(Env *env, char *id, Expr *expr) {
	for (int i = 0; i < env->len; i++) {
		if (id == env->identifiers[i]) {
			delete_expr(env->values[i]);
			env->values[i] = expr;
			return;
		}
	}

	if (env->len == env->cap) {
		env_grow(env);
	}
	env->identifiers[env->len] = id;
	env->values[env->len] = expr;
	env->len++;
}

Expr *env_get(Env *env, char *id) {
	for (int i = 0; i < env->len; i++) {
		if (id == env->identifiers[i]) {
			// NOTE(Fors): The environment never gives up ownership.
			return clone_expr(env->values[i]);
		}
	}
	if (env->parent) {
		return env_get(env->parent, id);
	} else {
		return NULL;
	}
}

// ----------------------------------------- EXPRESSIONS -------------------------------------------

typedef enum ExprKind {
	EXPR_ERROR,
	EXPR_ATOM,
	EXPR_NUMBER,
	EXPR_LIST,
	EXPR_CALL,
	EXPR_LAMBDA,
} ExprKind;

struct Expr {
	ExprKind kind;
	uint64_t line_number;
	char *error;
	union {
		char *atom;
		uint64_t number;
		struct {
			Expr **exprs;
			size_t num_exprs;
		} list;
		struct {
			Expr *func;
			Expr **args;
			size_t num_args;
		} call;
		struct {
			Expr **params;
			size_t num_params;
			Expr *body;
		} lambda;
	};
};

Expr *new_expr(ExprKind kind, uint64_t line_number) {
	Expr *expr = xcalloc(1, sizeof(Expr));
	expr->kind = kind;
	expr->line_number = line_number;
	return expr;
}

void delete_expr(Expr *expr) {
	if (!expr) {
		return;
	}

	switch (expr->kind) {
	case EXPR_LIST:
		for (int i = 0; i < expr->list.num_exprs; i++) {
			delete_expr(expr->list.exprs[i]);
		}
		xfree(expr->list.exprs);
		break;
	case EXPR_CALL:
		delete_expr(expr->call.func);
		for (int i = 0; i < expr->call.num_args; i++) {
			delete_expr(expr->call.args[i]);
		}
		xfree(expr->call.args);
		break;
	case EXPR_LAMBDA:
		delete_expr(expr->lambda.body);
		for (int i = 0; i < expr->lambda.num_params; i++) {
			delete_expr(expr->lambda.params[i]);
		}
		xfree(expr->lambda.params);
		break;
	case EXPR_ERROR:
		xfree(expr->error);
		break;
	}
	xfree(expr);
}

Expr *atom_expr(char *atom, uint64_t line_number) {
	Expr *expr = new_expr(EXPR_ATOM, line_number);
	expr->atom = atom;
	return expr;
}

Expr *number_expr(uint64_t number, uint64_t line_number) {
	Expr *expr = new_expr(EXPR_NUMBER, line_number);
	expr->number = number;
	return expr;
}

// NOTE(Fors): Assumes ownership of exprs
Expr *list_expr(Expr **exprs, size_t num_exprs, uint64_t line_number) {
	Expr *expr = new_expr(EXPR_LIST, line_number);
	expr->list.exprs = exprs;
	expr->list.num_exprs = num_exprs;
	return expr;
}

// NOTE(Fors): Assumes ownership of func & args
Expr *call_expr(Expr *func, Expr **args, size_t num_args, uint64_t line_number) {
	Expr *expr = new_expr(EXPR_CALL, line_number);
	expr->call.func = func;
	expr->call.args = args;
	expr->call.num_args = num_args;
	return expr;
}

// NOTE(Fors): Assumes ownership of params & body
Expr *lambda_expr(Expr **params, size_t num_params, Expr *body, uint64_t line_number) {
	Expr *expr = new_expr(EXPR_LAMBDA, line_number);
	expr->lambda.params = params;
	expr->lambda.num_params = num_params;
	expr->lambda.body = body;
	return expr;
}

// NOTE(Fors): Assumes ownership of error
Expr *error_expr(uint64_t line_number, char *error) {
	Expr *expr = new_expr(EXPR_ERROR, line_number);
	expr->error = error;
	return expr;
}

Expr *clone_expr(Expr *expr) {
	switch (expr->kind) {
	case EXPR_ATOM:
		return atom_expr(expr->atom, expr->line_number);
	case EXPR_NUMBER:
		return number_expr(expr->number, expr->line_number);
	case EXPR_LIST:
	{
		size_t len = expr->list.num_exprs;
		Expr **exprs = xmalloc(len * sizeof(Expr *));
		for (int i = 0; i < len; i++) {
			exprs[i] = clone_expr(expr->list.exprs[i]);
		}
		return list_expr(exprs, len, expr->line_number);
	}
	case EXPR_CALL:
	{
		size_t len = expr->call.num_args;
		Expr **args = xmalloc(len * sizeof(Expr *));
		for (int i = 0; i < len; i++) {
			args[i] = clone_expr(expr->call.args[i]);
		}
		return call_expr(clone_expr(expr->call.func), args, len, expr->line_number);
	}
	case EXPR_LAMBDA:
	{
		size_t len = expr->lambda.num_params;
		Expr **params = xmalloc(len * sizeof(Expr *));
		for (int i = 0; i < len; i++) {
			params[i] = clone_expr(expr->lambda.params[i]);
		}
		return lambda_expr(params, len, clone_expr(expr->lambda.body), expr->line_number);
	}
	case EXPR_ERROR:
	{
		size_t len = strlen(expr->error);
		char *error = xmalloc(len + 1);
		strcpy(error, expr->error);
		error[len] = 0;
		return error_expr(expr->line_number, error);
	}
	}
	printf("Couldn't clone unknown expression.\n");
	exit(1);
}

// ------------------------------------------- PARSING ---------------------------------------------

bool is_flat_list(Expr *expr, ExprKind kind) {
	if (expr->kind != EXPR_LIST) {
		return false;
	}
	for (int i = 0; i < expr->list.num_exprs; i++) {
		if (kind == EXPR_ATOM && expr->list.exprs[i]->kind != EXPR_ATOM) {
			return false;
		} else if (kind == EXPR_NUMBER && expr->list.exprs[i]->kind != EXPR_NUMBER) {
			return false;
		} else if (expr->list.exprs[i]->kind == EXPR_LIST) {
			return false;
		}
	}
	return true;
}

#define MAX_LIST 256
Expr *parse_list(void) {
	expect_token(TOKEN_LBRACE);
	uint64_t line_number = lexer_line;
	// NOTE(Fors): Max 256 items allowed in a list.
	Expr *list[MAX_LIST] = {0};
	size_t i = 0;
	while (!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
		if (i == MAX_LIST - 1) {
			for (int e = 0; e < i; e++) {
				delete_expr(list[e]);
			}
			return error_expr(lexer_line, err_str("No more than %i expressions allowed in a list.", MAX_LIST));
		}
		Expr *item = parse_expr();
		if (item->kind == EXPR_ERROR) {
			for (int e = 0; e < i; e++) {
				delete_expr(list[e]);
			}
			return item;
		}
		list[i++] = item;
	}
	if (is_token(TOKEN_EOF)) {
		for (int e = 0; e < i; e++) {
			delete_expr(list[e]);
		}
		return error_expr(lexer_line, err_str("Expexted closing brace in list."));
	}
	Expr **exprs = xmalloc(i * sizeof(Expr *));
	memcpy(exprs, list, i * sizeof(Expr *));
	expect_token(TOKEN_RBRACE);
	return list_expr(exprs, i, line_number);
}
#undef MAX_LIST

#define MAX_ARGS 16
Expr *parse_call(void) {
	expect_token(TOKEN_LPAREN);
	uint64_t line_number = lexer_line;
	// NOTE(Fors): Any expression can be a function call.
	Expr *func = parse_expr();
	if (func->kind == EXPR_ERROR) {
		return func;
	}
	// NOTE(Fors): Max 16 arguments allowed in a function.
	Expr *list[MAX_ARGS] = { 0 };
	size_t i = 0;
	while (!is_token(TOKEN_RPAREN) && !is_token(TOKEN_EOF)) {
		if (i == MAX_ARGS - 1) {
			for (int e = 0; e < i; e++) {
				delete_expr(list[e]);
			}
			delete_expr(func);
			return error_expr(lexer_line, err_str("No more than %i arguments allowed in a function.", MAX_ARGS));
		}
		Expr *item = parse_expr();
		if (item->kind == EXPR_ERROR) {
			for (int e = 0; e < i; e++) {
				delete_expr(list[e]);
			}
			delete_expr(func);
			return item;
		}
		list[i++] = item;
	}
	if (is_token(TOKEN_EOF)) {
		for (int e = 0; e < i; e++) {
			delete_expr(list[e]);
		}
		delete_expr(func);
		return error_expr(lexer_line, err_str("Expexted closing parenthesis in call."));
	}
	Expr **args = xmalloc(i * sizeof(Expr *));
	memcpy(args, list, i * sizeof(Expr *));
	expect_token(TOKEN_RPAREN);
	return call_expr(func, args, i, line_number);
}
#undef MAX_ARGS

Expr *parse_expr() {
	Expr *expr;
	switch (token.kind) {
	case TOKEN_ATOM:
		expr = atom_expr(token.atom, lexer_line);
		next_token();
		break;
	case TOKEN_NUMBER:
		expr = number_expr(token.number, lexer_line);
		next_token();
		break;
	case TOKEN_LBRACE:
		expr = parse_list();
		break;
	case TOKEN_LPAREN:
		expr = parse_call();
		break;
	case TOKEN_ERROR:
		expr = error_expr(lexer_line, err_str(token.error));
		break;
	default:
		printf("Unexpected lexing error occured.\n");
		exit(1);
	}
	return expr;
}

// ----------------------------------------- EVALUATION --------------------------------------------

#define transform_error(expr, ...) {\
	uint64_t line = (expr)->line_number;\
	delete_expr(expr);\
	(expr) = error_expr(line, err_str(__VA_ARGS__));}

#define error_match_pred(expr, pred, ...) if (!(pred)) { transform_error(expr, __VA_ARGS__) return (expr); }

#define error_define(expr, val, place) error_match_pred(expr, val, "Inside '%s', unexpected call to function 'define'", (place))
//#define error_atom(expr, atom, ...) error_pred(expr, (atom)->kind == EXPR_ATOM, __VA_ARGS__)

#define error_match_args(expr, num, place) \
	uint64_t args = (expr)->call.num_args;\
	error_match_pred(expr, args == (num), \
	"Function '%s' expected %llu arguments, was passed %llu arguments.", place, num, args)

#define error_prop(expr, val)\
	if ((val)->kind == EXPR_ERROR) {\
	Expr *err = clone_expr(val);\
	delete_expr(expr);\
	return err; }

Expr *eval_define(Env *env, Expr *expr) {
	error_match_args(expr, 2, "define");
	// NOTE(Fors): Weird situations can arrise if we allow expressions as identifier.
	// TODO(Fors): Might allow expressions that evaluates to an atom?
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_ATOM, "First argument in function 'define' must be an atom.");

	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], "define");
	error_prop(expr, expr->call.args[1]);

	env_add(env, expr->call.args[0]->atom, clone_expr(expr->call.args[1]));
	delete_expr(expr);
	return NULL;
}

Expr *eval_lambda(Env *env, Expr *expr) {
	error_match_args(expr, 2, "lambda");
	// TODO(Fors): Should we bother to check every item for an error?
	error_match_pred(expr, is_flat_list(expr->call.args[0], EXPR_ATOM), "Function 'lambda' expected first argument to be a flat list of atoms.");

	// Copy body and argument list.
	Expr *body = clone_expr(expr->call.args[1]);
	size_t len = expr->call.args[0]->list.num_exprs;
	Expr **params = xmalloc(len * sizeof(Expr *));
	for (int i = 0; i < len; i++) {
		params[i] = clone_expr(expr->call.args[0]->list.exprs[i]);
	}
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return lambda_expr(params, len, body, line);
}

Expr *eval_car(Env *env, Expr *expr) {
	error_match_args(expr, 1, "car");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "car");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_LIST && expr->call.args[0]->list.num_exprs > 0, 
		"Function 'car' is defined only for non-empty lists.");

	Expr *car = clone_expr(expr->call.args[0]->list.exprs[0]);
	delete_expr(expr);
	return car;
}

Expr *eval_cdr(Env *env, Expr *expr) {
	error_match_args(expr, 1, "cdr");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "cdr");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_LIST, "Function 'cdr' is defined only for non-empty lists.");

	size_t len = expr->call.args[0]->list.num_exprs;
	error_match_pred(expr, len > 0, "Function 'cdr' is defined only for non-empty lists.");

	// Skip first expr.
	Expr **exprs = xmalloc((len - 1) * sizeof(Expr *));
	for (int i = 1; i < len; i++) {
		exprs[i - 1] = clone_expr(expr->call.args[0]->list.exprs[i]);
	}
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return list_expr(exprs, len - 1, line);
}

Expr *eval_cons(Env *env, Expr *expr) {
	error_match_args(expr, 2, "cons");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "cons");
	error_prop(expr, expr->call.args[0]);
	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], "cons");
	error_prop(expr, expr->call.args[1]);
	error_match_pred(expr, expr->call.args[1]->kind == EXPR_LIST, "Function 'cons' expected the second argument to be a list.");

	size_t len = expr->call.args[1]->list.num_exprs;
	Expr **exprs = xmalloc((len + 1) * sizeof(Expr *));
	exprs[0] = clone_expr(expr->call.args[0]);
	for (int i = 0; i < len; i++) {
		exprs[i + 1] = clone_expr(expr->call.args[1]->list.exprs[i]);
	}
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return list_expr(exprs, len + 1, line);
}

Expr *eval_cond(Env *env, Expr *expr) {
	size_t args = expr->call.num_args;
	error_match_pred(expr, expr->call.num_args > 1, 
		"Function 'cond' expected at least 1 argument plus an 'else' clause, was passed %llu arguments.", args);
	for (int i = 0; i < expr->call.num_args; i++) {
		error_match_pred(expr, expr->call.args[i]->kind == EXPR_LIST, "Function 'cond' expexted argument %i to be a list.", i);
		error_match_pred(expr, expr->call.args[i]->list.num_exprs == 2, "Function 'cond' expected argument %i to be a pair.", i);

		expr->call.args[i]->list.exprs[0] = eval_expr(env, expr->call.args[i]->list.exprs[0]);
		error_define(expr, expr->call.args[i]->list.exprs[0], "cond");
		error_prop(expr, expr->call.args[i]->list.exprs[0]);
		error_match_pred(expr, expr->call.args[i]->list.exprs[0]->kind == EXPR_ATOM, 
			"In function 'cond', in argument %i, expected the first value in the pair to be an atom.", i);

		char *name = expr->call.args[i]->list.exprs[0]->atom;
		if (name == builtin_true || name == builtin_else) {
			Expr *val = eval_expr(env, clone_expr(expr->call.args[i]->list.exprs[1]));
			delete_expr(expr);
			return val;
		}
	}
	transform_error(expr, "Function 'cond' expected an 'else' clause.");
	return expr;
}

Expr *eval_and(Env *env, Expr *expr) {
	error_match_args(expr, 2, "and");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "and");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_ATOM, "Function 'and' expected argument 0 to be an atom.");
	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], "and");
	error_prop(expr, expr->call.args[1]);
	error_match_pred(expr, expr->call.args[1]->kind == EXPR_ATOM, "Function 'and' expected argument 1 to be an atom.");

	bool is_true = expr->call.args[0]->atom == builtin_true && expr->call.args[1]->atom == builtin_true;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_true ? builtin_true : builtin_false, line);
}

Expr *eval_or(Env *env, Expr *expr) {
	error_match_args(expr, 2, "or");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "or");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_ATOM, "Function 'or' expected argument 0 to be an atom.");
	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], "or");
	error_prop(expr, expr->call.args[1]);
	error_match_pred(expr, expr->call.args[1]->kind == EXPR_ATOM, "Function 'or' expected argument 1 to be an atom.");

	bool is_true = expr->call.args[0]->atom == builtin_true || expr->call.args[1]->atom == builtin_true;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_true ? builtin_true : builtin_false, line);
}

Expr *eval_null_(Env *env, Expr *expr) {
	error_match_args(expr, 1, "null?");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "null?");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_LIST, "Function 'null?' is defined only for lists.");

	bool is_null = expr->call.args[0]->list.num_exprs == 0;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_null ? builtin_true : builtin_false, line);
}

Expr *eval_atom_(Env *env, Expr *expr) {
	error_match_args(expr, 1, "atom?");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "atom?");
	error_prop(expr, expr->call.args[0]);

	bool is_atom = expr->call.args[0]->kind == EXPR_ATOM || expr->call.args[0]->kind == EXPR_NUMBER;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_atom ? builtin_true : builtin_false, line);
}

Expr *eval_number_(Env *env, Expr *expr) {
	error_match_args(expr, 1, "number?");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "number?");
	error_prop(expr, expr->call.args[0]);

	bool is_number = expr->call.args[0]->kind == EXPR_NUMBER;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_number ? builtin_true : builtin_false, line);
}
Expr *eval_zero_(Env *env, Expr *expr) {
	error_match_args(expr, 1, "zero?");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "zero?");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_NUMBER, "Function 'zero?' is defined only for numbers.");

	bool is_zero = expr->call.args[0]->number == 0;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_zero ? builtin_true : builtin_false, line);
}

Expr *eval_eq_(Env *env, Expr *expr) {
	error_match_args(expr, 2, "eq?");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "eq?");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_ATOM, "Function 'eq?' expected argument 0 to be an atom.");

	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], "eq?");
	error_prop(expr, expr->call.args[1]);
	error_match_pred(expr, expr->call.args[1]->kind == EXPR_ATOM, "Function 'eq?' expected argument 1 to be an atom.");

	bool is_eq = expr->call.args[0]->atom == expr->call.args[1]->atom;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return atom_expr(is_eq ? builtin_true : builtin_false, line);
}

Expr *eval_add1(Env *env, Expr *expr) {
	error_match_args(expr, 1, "add1");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "add1");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_NUMBER, "Function 'add1' is defined only for numbers.");

	uint64_t val = expr->call.args[0]->number;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return number_expr(val + 1, line);
}
Expr *eval_sub1(Env *env, Expr *expr) {
	error_match_args(expr, 1, "sub1");

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], "sub1");
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_NUMBER, "Function 'sub1' is defined only for numbers.");

	uint64_t val = expr->call.args[0]->number;
	uint64_t line = expr->line_number;
	delete_expr(expr);
	return number_expr(val - 1, line);
}

Expr *eval_operator(Env *env, Expr *expr) {
	char *name = expr->call.func->atom;
	error_match_args(expr, 2, name);

	expr->call.args[0] = eval_expr(env, expr->call.args[0]);
	error_define(expr, expr->call.args[0], name);
	error_prop(expr, expr->call.args[0]);
	error_match_pred(expr, expr->call.args[0]->kind == EXPR_NUMBER, "Function '%s' expected argument 0 to be a number.", name);
	expr->call.args[1] = eval_expr(env, expr->call.args[1]);
	error_define(expr, expr->call.args[1], name);
	error_prop(expr, expr->call.args[1]);
	error_match_pred(expr, expr->call.args[1]->kind == EXPR_NUMBER, "Function '%s' expected argument 1 to be a number.", name);

	uint64_t left_val = expr->call.args[0]->number;
	uint64_t right_val = expr->call.args[1]->number;
	uint64_t line = expr->line_number;
	delete_expr(expr);

	if (name == builtin_add) {
		return number_expr(left_val + right_val, line);
	} else if (name == builtin_sub) {
		return number_expr(left_val - right_val, line);
	} else if (name == builtin_mul) {
		return number_expr(left_val * right_val, line);
	} else {
		assert(name == builtin_div);
		return number_expr(left_val / right_val, line);
	}
}

Expr *eval_builtin(Env *env, Expr *expr) {
	assert(expr->kind == EXPR_CALL);
	assert(expr->call.func->kind == EXPR_ATOM);
	assert(is_builtin(expr->call.func->atom));

	char *name = expr->call.func->atom;
	if (name == builtin_define) {
		return eval_define(env, expr);
	} else if (name == builtin_lambda) { // lambda
		return eval_lambda(env, expr);
	} else if (name == builtin_car) { // car
		return eval_car(env, expr);
	} else if (name == builtin_cdr) { // cdr
		return eval_cdr(env, expr);
	} else if (name == builtin_cons) { // cons
		return eval_cons(env, expr);
	} else if (name == builtin_cond) { // cond
		return eval_cond(env, expr);
	} else if (name == builtin_and) { // and
		return eval_and(env, expr);
	} else if (name == builtin_or) { // or
		return eval_or(env, expr);
	} else if (name == builtin_null_) { // null?
		return eval_null_(env, expr);
	} else if (name == builtin_atom_) { // atom?
		return eval_atom_(env, expr);
	} else if (name == builtin_number_) { // number?
		return eval_number_(env, expr);
	} else if (name == builtin_zero_) { // zero?
		return eval_zero_(env, expr);
	} else if (name == builtin_eq_) { // eq?
		return eval_eq_(env, expr);
	} else if (name == builtin_add1) { // add1
		return eval_add1(env, expr);
	} else if (name == builtin_sub1) { // sub1
		return eval_sub1(env, expr);
	} else if (name == builtin_add || name == builtin_sub || name == builtin_mul || name == builtin_div) { // +-*/
		return eval_operator(env, expr);
	} else {
		printf("Expected a builtin function, got %s.\n", expr->atom);
		exit(1);
	}
}

Expr *eval_expr(Env *env, Expr *expr) {
	switch (expr->kind) {
	case EXPR_ATOM:
	{
		Expr *var = env_get(env, expr->atom);
		if (var) {
			if (var->kind == EXPR_ATOM && var->atom == expr->atom) {
				// Cyclic expression!
				delete_expr(var);
			} else {
				delete_expr(expr);
				expr = eval_expr(env, var);
			}
		}
		break;
	}
	case EXPR_NUMBER: break;
	case EXPR_LIST:
	{
		for (int i = 0; i < expr->list.num_exprs; i++) {
			expr->list.exprs[i] = eval_expr(env, expr->list.exprs[i]);
			error_define(expr, expr->list.exprs[i], "list");
			error_prop(expr, expr->list.exprs[i]);
		}
		break;
	}
	case EXPR_CALL:
	{
		expr->call.func = eval_expr(env, expr->call.func);
		error_define(expr, expr->call.func, "function");
		error_prop(expr, expr->call.func);
		// NOTE(Fors): A function can either be a lambda or builtin.
		if (expr->call.func->kind == EXPR_ATOM) {
			char *name = expr->call.func->atom;
			error_match_pred(expr, is_builtin(expr->call.func->atom), "Function call expected atom: %s to identify a lambda expression.", name);
			expr = eval_builtin(env, expr);
		} else {
			error_match_pred(expr, expr->call.func->kind == EXPR_LAMBDA, "Only lambda expressions can be called as a function.");
			size_t num_params = expr->call.func->lambda.num_params;
			error_match_args(expr, num_params, "");

			Env *scope = new_env(env);
			for (int i = 0; i < expr->call.func->lambda.num_params; i++) {
				env_add(scope, expr->call.func->lambda.params[i]->atom, eval_expr(env, clone_expr(expr->call.args[i])));
			}
			Expr *result = eval_expr(scope, clone_expr(expr->call.func->lambda.body));
			delete_expr(expr);
			delete_env(scope);
			expr = result;
		}
		break;
	}
	case EXPR_LAMBDA: break;
	case EXPR_ERROR: break;
	}
	return expr;
}

// ------------------------------------------ PRINTING ---------------------------------------------

void print_expr(Expr *expr) {
	if (!expr) {
		return;
	}
	switch (expr->kind) {
	case EXPR_ATOM:
		printf("%s", expr->atom);
		break;
	case EXPR_NUMBER:
		printf("%llu", expr->number);
		break;
	case EXPR_LIST:
		printf("{ ");
		for (int i = 0; i < expr->list.num_exprs; i++) {
			print_expr(expr->list.exprs[i]);
			printf(" ");
		}
		printf("}");
		break;
	case EXPR_ERROR:
		printf("Error on line (%llu): %s", expr->line_number, expr->error);
		break;
	case EXPR_CALL: break;
	case EXPR_LAMBDA: break;
	}
}