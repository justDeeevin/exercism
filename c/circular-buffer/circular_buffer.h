#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

#include <stdint.h>
#include <stdlib.h>

typedef int32_t buffer_value_t;

typedef struct {
  size_t capacity;
  size_t len;
  size_t read_position;
  size_t write_position;
  buffer_value_t *data;
} circular_buffer_t;

circular_buffer_t *new_circular_buffer(size_t capacity);

int16_t write(circular_buffer_t *buffer, buffer_value_t value);
int16_t overwrite(circular_buffer_t *buffer, buffer_value_t value);

int16_t read(circular_buffer_t *buffer, buffer_value_t *value);

void clear_buffer(circular_buffer_t *buffer);
void delete_buffer(circular_buffer_t *buffer);

#endif
