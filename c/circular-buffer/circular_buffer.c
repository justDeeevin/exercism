#include "circular_buffer.h"
#include <errno.h>

circular_buffer_t *new_circular_buffer(size_t capacity) {
  circular_buffer_t *buffer = malloc(sizeof(circular_buffer_t));

  buffer->capacity = capacity;
  buffer->read_position = 0;
  buffer->write_position = 0;
  buffer->len = 0;
  buffer->data = malloc(capacity * sizeof(buffer_value_t));

  return buffer;
}

void increment_read_position(circular_buffer_t *buffer);
void increment_read_position(circular_buffer_t *buffer) {
  buffer->read_position = (buffer->read_position + 1) % buffer->capacity;
}

int16_t write(circular_buffer_t *buffer, buffer_value_t value) {
  if (buffer->len == buffer->capacity) {
    errno = ENOBUFS;
    return EXIT_FAILURE;
  }

  int16_t status = overwrite(buffer, value);

  return status;
}

int16_t overwrite(circular_buffer_t *buffer, buffer_value_t value) {
  size_t write_position = buffer->write_position;

  buffer->data[write_position] = value;
  buffer->write_position = (buffer->write_position + 1) % buffer->capacity;

  if (buffer->len < buffer->capacity)
    buffer->len++;
  else
    increment_read_position(buffer);

  return EXIT_SUCCESS;
}

int16_t read(circular_buffer_t *buffer, buffer_value_t *value) {
  if (buffer->len == 0) {
    errno = ENODATA;
    return EXIT_FAILURE;
  }

  *value = buffer->data[buffer->read_position];
  increment_read_position(buffer);
  buffer->len--;

  return EXIT_SUCCESS;
}

void clear_buffer(circular_buffer_t *buffer) {
  buffer->read_position = 0;
  buffer->write_position = 0;
  buffer->len = 0;
}

void delete_buffer(circular_buffer_t *buffer) {
  free(buffer->data);
  free(buffer);
}
