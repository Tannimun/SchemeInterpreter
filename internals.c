/*
|-------------------------------------------------------------------------------
| Internals
|-------------------------------------------------------------------------------
| Copyright (C) 2020 by Daniel Fors - All Rights Reserved
|
| These are som internal functions, mostly concerning memory.
|
| TODO(Fors):
| - Add memory arena?
| - Add stretch buffers?
|
*/

// -------------------------------------- MEMORY MANAGEMENT ----------------------------------------

void *xmalloc(size_t size) {
	void *mem = malloc(size);
	if (!mem) {
		printf("xmalloc failed.\n");
		exit(1);
	}
	return mem;
}

void *xcalloc(size_t count, size_t size) {
	void *mem = calloc(count, size);
	if (!mem) {
		printf("xcalloc failed.\n");
		exit(1);
	}
	return mem;
}

void *xrealloc(void *block, size_t size) {
	void *mem = realloc(block, size);
	if (!mem) {
		printf("xrealloc failed.\n");
		exit(1);
	}
	return mem;
}

void xfree(void *block) {
	free(block);
}

// -------------------------------------- STRING INTERNING -----------------------------------------

typedef struct StringIntern {
	char **pool;
	size_t len;
	size_t cap;
} StringIntern;

StringIntern intern = { 0 };

void new_intern(void) {
	intern.cap = 16;
	intern.pool = xmalloc(intern.cap * sizeof(char *));
}

bool str_cmp(char *first, char *second) {
	if (first == second) {
		return true;
	}
	while (*first != 0 && *second != 0) {
		if (*first++ != *second++) {
			return false;
		}
	}
	return *first == 0 && *second == 0;
}

void intern_grow(void) {
	intern.cap *= 2;
	intern.pool = xrealloc(intern.pool, intern.cap * sizeof(char *));
}

char *intern_str(char *str) {
	// See if the string is already interned
	for (int i = 0; i < intern.len; i++) {
		if (str_cmp(str, intern.pool[i])) {
			return intern.pool[i];
		}
	}
	// Grow intern array if needed
	if (intern.len == intern.cap) {
		intern_grow();
	}

	size_t len = strlen(str);
	char *new_str = xmalloc(len + 1);
	strcpy(new_str, str);
	new_str[len] = 0;
	intern.pool[intern.len] = new_str;
	intern.len++;
	return new_str;
}

// -------------------------------------------- MISC -----------------------------------------------

char *string_format(size_t size, char *fmt, ...) {
	va_list args;
	va_start(args, fmt);

	char *str = xmalloc(size);
	vsnprintf(str, size - 1, fmt, args);
	str = xrealloc(str, strlen(str) + 1);

	va_end(args);
	return str;
}