#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

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
	const void* content;
} Array;

Float floor_float(Float f) {
	return floor(f);
}

Float ceil_float(Float f) {
	return ceil(f);
}

Float round_float(Float f) {
	return round(f);
}

void print_int(Int i) {
	printf("%li", i);
}

void print_float(Float f) {
	printf("%f", f);
}

void print_boolean(Boolean b) {
	if (b) printf("true");
	else printf("false");
}

void print_string(String s) {
	printf("%s", s.content);
}

void println_int(Int i) {
	print_int(i);
	printf("\n");
}

void println_float(Float f) {
	print_float(f);
	printf("\n");
}

void println_boolean(Boolean b) {
	print_boolean(b);
	printf("\n");
}

void println_string(String s) {
	print_string(s);
	printf("\n");
}

Int prompt_int(String s) {
	Int i;
	println_string(s);
	scanf("%ld", &i);
	return i;
}

Float prompt_float(String s) {
	Float f;
	println_string(s);
	scanf("%lf", &f);
	return f;
}

Int len_string(String s) {
	return s.len;
}

Int len_array(Array* a) {
	return a->len;
}

void exit_if_out_of_bounds(Array* a, Int idx) {
	if (idx < 0 || idx >= a->len) {
		printf("ILLEGAL OUT OF BOUNDS ARRAY INDEX - LENGTH: %li, INDEX: %li\n", a->len, idx);
		exit(1);
	}
}

Int index_of_int(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Int*) ptr);
}

Float index_of_float(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Float*) ptr);
}

Boolean index_of_boolean(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((Boolean*) ptr);
}

String index_of_string(Array* a, Int idx) {
	exit_if_out_of_bounds(a, idx);
	long ptr = ((long) a->content + a->element_size * idx);
	return *((String*) ptr);
}
