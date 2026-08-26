#include <stdio.h>
#include <stdbool.h>

#ifndef __STDC_IEC_559__
#error "Requires IEEE 754 floating point!"
#endif

typedef long Int;
typedef double Float;
typedef bool Boolean;

typedef struct {
	Int len;
	const char* content;
} String;

void print_int(Int i) {
	printf("%li\n", i);
}

void print_float(Float f) {
	printf("%f\n", f);
}

void print_boolean(Boolean b) {
	if (b) printf("true\n");
	else printf("false\n");
}

void print_string(String* s) {
	printf("%s\n", s->content);
}
