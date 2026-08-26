#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

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

typedef struct {
	Int len;
	Int element_size;
	const Int* content;
} Array;

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

void print_string(String s) {
	printf("%s\n", s.content);
}

Int get_string_len(String s) {
	return s.len;
}

void exit_if_out_of_bounds(Array* a, Int idx) {
	if (idx >= a->len) {
		printf("ILLEGAL OUT OF BOUNDS ARRAY INDEX - LENGTH: %li, INDEX: %li\n", a->len, idx);
		exit(1);
	}
}

Int get_array_element_int(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Int*) ptr);
}

Float get_array_element_float(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Float*) ptr);
}

Boolean get_array_element_boolean(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Boolean*) ptr);
}

String get_array_element_string(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((String*) ptr);
}
