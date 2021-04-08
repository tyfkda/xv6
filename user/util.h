#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define MAX(a, b)  ((a) > (b) ? (a) : (b))
#define ALIGN(x, align)  (((x) + (align) - 1) & -(align))  // align must be 2^n
#define UNUSED(x)  ((void)(x))

char *strdup_(const char *str);
char *strndup_(const char *str, size_t size);
char *alloc_label(void);
void set_local_label_prefix(const char *prefix);
char *cat_path(const char *base_dir, const char *rel_path);
ssize_t getline_(char **lineptr, size_t *n, FILE *stream, size_t start);
char *abspath(const char *root, const char *path);

void error(const char* fmt, ...) /*__attribute((noreturn))*/;

bool is_im8(intptr_t x);
bool is_im32(intptr_t x);

// Container

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void);
void vec_clear(Vector *vec);
void vec_push(Vector *vec, const void *elem);
void *vec_pop(Vector *vec);
void vec_insert(Vector *vec, int pos, const void *elem);
void vec_remove_at(Vector *vec, int index);
bool vec_contains(Vector *vec, void* elem);

typedef struct Map {
  Vector *keys;
  Vector *vals;
} Map;

Map *new_map(void);
int map_count(Map *map);
void map_put(Map *map, const char *key, const void *val);
bool map_remove(Map *map, const char *key);
void *map_get(Map *map, const char *key);
bool map_try_get(Map *map, const char *key, void **output);
