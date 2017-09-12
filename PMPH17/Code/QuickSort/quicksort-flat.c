#ifdef _MSC_VER
#define inline __inline
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <getopt.h>
static int detail_memory = 0;
static int debugging = 0;
static int binary_output = 0;
/* Crash and burn. */

#include <stdarg.h>

static const char *fut_progname;

void panic(int eval, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
        fprintf(stderr, "%s: ", fut_progname);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
        exit(eval);
}

//// Text I/O

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  int (*elem_reader)(void*);
};

static int peekc() {
  int c = getchar();
  if (c != EOF) {
    ungetc(c,stdin);
  }
  return c;
}

static int next_is_not_constituent() {
  int c = peekc();
  return c == EOF || !isalnum(c);
}

static void skipspaces() {
  int c = getchar();
  if (isspace(c)) {
    skipspaces();
  } else if (c == '-' && peekc() == '-') {
    // Skip to end of line.
    for (; c != '\n' && c != EOF; c = getchar());
    // Next line may have more spaces.
    skipspaces();
  } else if (c != EOF) {
    ungetc(c, stdin);
  }
}

static int read_str_elem(struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    reader->n_elems_space * reader->elem_size);
  }

  ret = reader->elem_reader(reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(struct array_reader *reader, int dims) {
  int c;
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc(dims,sizeof(char));
  int cur_dim = dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc(dims,sizeof(int64_t));
  while (1) {
    skipspaces();

    c = getchar();
    if (c == ']') {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (c == ',') {
      skipspaces();
      c = getchar();
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (c == EOF) {
      ret = 1;
      break;
    } else if (first) {
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(const char *type_name, int64_t *shape, int64_t dims) {
  char c;
  if (scanf("empty") == EOF) {
    return 1;
  }

  c = getchar();
  if (c != '(') {
    return 1;
  }

  for (int i = 0; i < dims-1; i++) {
    c = getchar();
    if (c != '[') {
      return 1;
    }
    c = getchar();
    if (c != ']') {
      return 1;
    }
  }

  int n = strlen(type_name);
  for (int i = 0; i < n; i++) {
    c = getchar();
    if (c != type_name[i]) {
      return 1;
    }
  }

  if (getchar() != ')') {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    shape[i] = 0;
  }

  return 0;
}

static int read_str_array(int64_t elem_size, int (*elem_reader)(void*),
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  int64_t read_dims = 0;

  while (1) {
    int c;
    skipspaces();
    c = getchar();
    if (c=='[') {
      read_dims++;
    } else {
      if (c != EOF) {
        ungetc(c, stdin);
      }
      break;
    }
  }

  if (read_dims == 0) {
    return read_str_empty_array(type_name, shape, dims);
  }

  if (read_dims != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, elem_size*reader.n_elems_space);
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(&reader, dims);

  *data = reader.elems;

  return ret;
}

/* Makes a copy of numeric literal removing any underscores, and
   length of the literal. */
static int remove_underscores(char* buf) {
  int buf_index = 0;
  char c = getchar();
  while (isxdigit(c) || c == '.' || c == '+' || c == '-' ||
         c == 'x' || c == 'X' ||
         c == 'e' || c == 'E' || c == '_') {
    if (c == '_') {
      c = getchar();
      continue;
    }
    else {
      buf[buf_index++] = c;
      c = getchar();
    }
  }
  buf[buf_index] = 0;
  ungetc(c, stdin);             /* unget 'i' */
  return buf_index;
}

static int read_str_i8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNi8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(int8_t*)dest = x;
    scanf("i8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNu8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(uint8_t*)dest = x;
    scanf("u8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("i16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    printf("fail\n");
    return 1;
  }
}

static int read_str_u16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("u16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("i32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("u32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi64, (int64_t*)dest) == 1) {
    scanf("i64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  if (sscanf(buf, "%"SCNu64, (int64_t*)dest) == 1) {
    scanf("u64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%f", (float*)dest) == 1) {
    scanf("f32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%lf", (double*)dest) == 1) {
    scanf("f64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_bool(void* dest) {
  /* This is a monstrous hack.  Maybe we should get a proper lexer in here. */
  char b[4];
  skipspaces();
  if (scanf("%4c", b) == 1) {
    if (strncmp(b, "true", 4) == 0) {
      *(char*)dest = 1;
      return 0;
    } else if (strncmp(b, "fals", 4) == 0 && getchar() == 'e') {
      *(char*)dest = 0;
      return 0;
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f32(FILE *out, float *src) {
  return fprintf(out, "%.6ff32", *src);
}

static int write_str_f64(FILE *out, double *src) {
  return fprintf(out, "%.6ff64", *src);
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

// Reading little-endian byte sequences.  On big-endian hosts, we flip
// the resulting bytes.

static int read_byte(void* dest) {
  int num_elems_read = fread(dest, 1, 1, stdin);
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
  uint16_t x;
  int num_elems_read = fread(&x, 2, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  *(uint16_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
  uint32_t x;
  int num_elems_read = fread(&x, 4, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  *(uint32_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
  uint64_t x;
  int num_elems_read = fread(&x, 8, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  *(uint64_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int write_byte(void* dest) {
  int num_elems_written = fwrite(dest, 1, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_2byte(void* dest) {
  uint16_t x = *(uint16_t*)dest;
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  int num_elems_written = fwrite(&x, 2, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_4byte(void* dest) {
  uint32_t x = *(uint32_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  int num_elems_written = fwrite(&x, 4, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_8byte(void* dest) {
  uint64_t x = *(uint64_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  int num_elems_written = fwrite(&x, 8, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

//// Types

typedef int (*writer)(FILE*, void*);
typedef int (*reader)(void*);

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int size; // in bytes
  const writer write_str; // Write in text format.
  const reader read_str; // Read in text format.
  const writer write_bin; // Write in binary format.
  const reader read_bin; // Read in binary format.
};

const static struct primtype_info_t i8 =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (reader)read_str_i8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
const static struct primtype_info_t i16 =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (reader)read_str_i16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
const static struct primtype_info_t i32 =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (reader)read_str_i32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t i64 =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (reader)read_str_i64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t u8 =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (reader)read_str_u8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
const static struct primtype_info_t u16 =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (reader)read_str_u16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
const static struct primtype_info_t u32 =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (reader)read_str_u32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t u64 =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (reader)read_str_u64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t f32 =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (reader)read_str_f32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t f64 =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (reader)read_str_f64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t bool =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (reader)read_str_bool,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};

static const struct primtype_info_t* primtypes[] = {
  &i8, &i16, &i32, &i64,
  &u8, &u16, &u32, &u64,
  &f32, &f64,
  &bool,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary() {
  skipspaces();
  int c = getchar();
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(&bin_version);

    if (ret != 0) { panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, stdin);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum() {
  char read_binname[4];

  int num_matched = scanf("%4c", read_binname);
  if (num_matched != 1) { panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum();
  if (bin_type != expected_type) {
    panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum();
  if (expected_type != bin_primtype) {
    panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  uint64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    uint64_t bin_shape;
    ret = read_le_8byte(&bin_shape);
    if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
    elem_count *= bin_shape;
    shape[i] = (int64_t) bin_shape;
  }

  size_t elem_size = expected_type->size;
  void* tmp = realloc(*data, elem_count * elem_size);
  if (tmp == NULL) {
    panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  size_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
  if (num_elems_read != elem_count) {
    panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    char* elems = (char*) *data;
    for (uint64_t i=0; i<elem_count; i++) {
      char* elem = elems+(i*elem_size);
      for (int j=0; j<elem_size/2; j++) {
        char head = elem[j];
        int tail_index = elem_size-1-j;
        elem[j] = elem[tail_index];
        elem[tail_index] = head;
      }
    }
  }

  return 0;
}

static int read_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary()) {
    return read_str_array(expected_type->size, (reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(expected_type, data, shape, dims);
  }
}

static int write_str_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (void*)data);
  } else {
    int64_t len = shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int64_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      printf("empty(");
      for (int64_t i = 1; i < rank; i++) {
        printf("[]");
      }
      printf("%s", elem_type->type_name);
      printf(")");
    } else if (rank==1) {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (void*) (data + i * elem_size));
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    } else {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    }
  }
  return 0;
}

static int write_bin_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fputs(elem_type->binname, out);
  fwrite(shape, sizeof(int64_t), rank, out);

  if (IS_BIG_ENDIAN) {
    for (size_t i = 0; i < num_elems; i++) {
      unsigned char *elem = data+i*elem_type->size;
      for (size_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, elem_type->size, num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type, void *data, int64_t *shape, int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary()) {
    return expected_type->read_str(dest);
  } else {
    read_bin_ensure_scalar(expected_type);
    return expected_type->read_bin(dest);
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

/* Some simple utilities for wall-clock timing.

   The function get_wall_time() returns the wall time in microseconds
   (with an unspecified offset).
*/

#ifdef _WIN32

#include <windows.h>

int64_t get_wall_time() {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
/* Assuming POSIX */

#include <time.h>
#include <sys/time.h>

int64_t get_wall_time() {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#endif

#define FUT_BLOCK_DIM 16
/* The simple OpenCL runtime framework used by Futhark. */

#ifdef __APPLE__
  #include <OpenCL/cl.h>
#else
  #include <CL/cl.h>
#endif

#define OPENCL_SUCCEED(e) opencl_succeed(e, #e, __FILE__, __LINE__)

static cl_context fut_cl_context;
static cl_command_queue fut_cl_queue;
static const char *cl_preferred_platform = "";
static const char *cl_preferred_device = "";
static int cl_preferred_device_num = 0;
static int cl_debug = 0;

static size_t cl_group_size = 256;
static size_t cl_num_groups = 128;
static size_t cl_tile_size = 32;
static size_t cl_lockstep_width = 1;
static const char* cl_dump_program_to = NULL;
static const char* cl_load_program_from = NULL;

struct opencl_device_option {
  cl_platform_id platform;
  cl_device_id device;
  cl_device_type device_type;
  char *platform_name;
  char *device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */
static void post_opencl_setup(struct opencl_device_option*);

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

static const char* opencl_error_string(unsigned int err)
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

static void opencl_succeed(unsigned int ret,
                    const char *call,
                    const char *file,
                    int line) {
  if (ret != CL_SUCCESS) {
    panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opencl_error_string(ret));
  }
}

void set_preferred_platform(const char *s) {
  cl_preferred_platform = s;
}

void set_preferred_device(const char *s) {
  int x = 0;
  if (*s == '#') {
    s++;
    while (isdigit(*s)) {
      x = x * 10 + (*s++)-'0';
    }
    // Skip trailing spaces.
    while (isspace(*s)) {
      s++;
    }
  }
  cl_preferred_device = s;
  cl_preferred_device_num = x;
}

static char* opencl_platform_info(cl_platform_id platform,
                                  cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

static char* opencl_device_info(cl_device_id device,
                                cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

static void opencl_all_device_options(struct opencl_device_option **devices_out,
                                      size_t *num_devices_out) {
  size_t num_devices = 0, num_devices_added = 0;

  cl_platform_id *all_platforms;
  cl_uint *platform_num_devices;

  cl_uint num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(0, NULL, &num_platforms));

  // Make room for them.
  all_platforms = calloc(num_platforms, sizeof(cl_platform_id));
  platform_num_devices = calloc(num_platforms, sizeof(cl_uint));

  // Fetch all the platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(num_platforms, all_platforms, NULL));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    if (clGetDeviceIDs(all_platforms[i], CL_DEVICE_TYPE_ALL,
                       0, NULL, &platform_num_devices[i]) == CL_SUCCESS) {
      num_devices += platform_num_devices[i];
    } else {
      platform_num_devices[i] = 0;
    }
  }

  // Make room for all the device options.
  struct opencl_device_option *devices =
    calloc(num_devices, sizeof(struct opencl_device_option));

  // Loop through the platforms, getting information about their devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    cl_platform_id platform = all_platforms[i];
    cl_uint num_platform_devices = platform_num_devices[i];

    if (num_platform_devices == 0) {
      continue;
    }

    char *platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    cl_device_id *platform_devices =
      calloc(num_platform_devices, sizeof(cl_device_id));

    // Fetch all the devices.
    OPENCL_SUCCEED(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL,
                                  num_platform_devices, platform_devices, NULL));

    // Loop through the devices, adding them to the devices array.
    for (cl_uint i = 0; i < num_platform_devices; i++) {
      char *device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                     sizeof(cl_device_type),
                                     &devices[num_devices_added].device_type,
                                     NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

static struct opencl_device_option get_preferred_device() {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_platform_matches = 0;
  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strstr(device.platform_name, cl_preferred_platform) != NULL &&
        strstr(device.device_name, cl_preferred_device) != NULL &&
        num_device_matches++ == cl_preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  panic(1, "Could not find acceptable OpenCL device.\n");
}

static void describe_device_option(struct opencl_device_option device) {
  fprintf(stderr, "Using platform: %s\n", device.platform_name);
  fprintf(stderr, "Using device: %s\n", device.device_name);
}

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
  cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
    assert(ret_val == 0);
  }

  cl_build_status build_status;
  ret_val = clGetProgramBuildInfo(program,
                                  device,
                                  CL_PROGRAM_BUILD_STATUS,
                                  sizeof(cl_build_status),
                                  &build_status,
                                  NULL);
  assert(ret_val == 0);

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size);
    assert(ret_val == 0);

    build_log = malloc(ret_val_size+1);
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL);
    assert(ret_val == 0);

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
  }

  return build_status;
}

// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static cl_program setup_opencl(const char *srcs[]) {

  cl_int error;
  cl_platform_id platform;
  cl_device_id device;
  cl_uint platforms, devices;
  size_t max_group_size;

  struct opencl_device_option device_option = get_preferred_device();

  if (cl_debug) {
    describe_device_option(device_option);
  }

  device = device_option.device;
  platform = device_option.platform;

  OPENCL_SUCCEED(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                 sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < cl_group_size) {
    fprintf(stderr, "Warning: Device limits group size to %zu (setting was %zu)\n",
            max_group_size, cl_group_size);
    cl_group_size = max_group_size;
  }

  if (max_tile_size < cl_tile_size) {
    fprintf(stderr, "Warning: Device limits tile size to %zu (setting was %zu)\n",
            max_tile_size, cl_tile_size);
    cl_tile_size = max_tile_size;
  }

  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  fut_cl_context = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  assert(error == 0);

  fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);
  assert(error == 0);

  // Make sure this function is defined.
  post_opencl_setup(&device_option);

  char *fut_opencl_src = NULL;
  size_t src_size = 0;

  // Maybe we have to read OpenCL source from somewhere else (used for debugging).
  if (cl_load_program_from) {
    FILE *f = fopen(cl_load_program_from, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    src_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    fut_opencl_src = malloc(src_size);
    fread(fut_opencl_src, 1, src_size, f);
    fclose(f);
  } else {
    // Build the OpenCL program.  First we have to concatenate all the fragments.
    for (const char **src = srcs; *src; src++) {
      src_size += strlen(*src);
    }

    fut_opencl_src = malloc(src_size + 1);

    size_t n, i;
    for (i = 0, n = 0; srcs[i]; i++) {
      strncpy(fut_opencl_src+n, srcs[i], src_size-n);
      n += strlen(srcs[i]);
    }
    fut_opencl_src[src_size] = 0;

  }

  cl_program prog;
  error = 0;
  const char* src_ptr[] = {fut_opencl_src};

  if (cl_dump_program_to) {
    FILE *f = fopen(cl_dump_program_to, "w");
    assert(f != NULL);
    fputs(fut_opencl_src, f);
    fclose(f);
  }

  prog = clCreateProgramWithSource(fut_cl_context, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  char compile_opts[1024];
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d -DLOCKSTEP_WIDTH=%d -DDEFAULT_GROUP_SIZE=%d -DDEFAULT_NUM_GROUPS=%d  -DDEFAULT_TILE_SIZE=%d", FUT_BLOCK_DIM, cl_lockstep_width, cl_group_size, cl_num_groups, cl_tile_size);
  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));
  free(fut_opencl_src);

  return prog;
}

static const char *fut_opencl_program[] =
                  {"__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))\nstatic inline int8_t add8(int8_t x, int8_t y)\n{\n    return x + y;\n}\nstatic inline int16_t add16(int16_t x, int16_t y)\n{\n    return x + y;\n}\nstatic inline int32_t add32(int32_t x, int32_t y)\n{\n    return x + y;\n}\nstatic inline int64_t add64(int64_t x, int64_t y)\n{\n    return x + y;\n}\nstatic inline int8_t sub8(int8_t x, int8_t y)\n{\n    return x - y;\n}\nstatic inline int16_t sub16(int16_t x, int16_t y)\n{\n    return x - y;\n}\nstatic inline int32_t sub32(int32_t x, int32_t y)\n{\n    return x - y;\n}\nstatic inline int64_t sub64(int64_t x, int64_t y)\n{\n    return x - y;\n}\nstatic inline int8_t mul8(int8_t x, int8_t y)\n{\n    return x * y;\n}\nstatic inline int16_t mul16(int16_t x, int16_t y)\n{\n    return x * y;\n}\nstatic inline int32_t mul32(int32_t x, int32_t y)\n{\n    return x * y;\n}\nstatic inline int64_t mul64(int64_t x, int64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y;\n    int8_t r = x % y;\n    \n    return q - ((",
                   "r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t srem32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int64_t smin64(int64_t x, int64_t",
                   " y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint64_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, uint8_t y)\n{\n    return x & y;\n}\nstatic inline u",
                   "int16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline char ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline char ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline char ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline char ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline char ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline char ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline char ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline char ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline char slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline char slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline char slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline char slt64(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline char sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline char sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline char sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline char sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    ",
                   "while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int8_t sext_i8_i8(int8_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i8_i16(int8_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i8_i32(int8_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i8_i64(int8_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i16_i8(int16_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i16_i16(int16_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i16_i32(int16_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i16_i64(int16_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i32_i8(int32_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i32_i16(int32_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i32_i32(int32_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i32_i64(int32_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i64_i8(int64_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i64_i16(int64_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i64_i32(int64_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i64_i64(int64_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i8_i8(uint8_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i8_i16(uint8_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i8_i32(uint8_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i8_i64(uint8_t x)",
                   "\n{\n    return x;\n}\nstatic inline uint8_t zext_i16_i8(uint16_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i16_i16(uint16_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i16_i32(uint16_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i16_i64(uint16_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i32_i8(uint32_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i32_i16(uint32_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i32_i32(uint32_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i32_i64(uint32_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i64_i8(uint64_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i64_i16(uint64_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i64_i32(uint64_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i64_i64(uint64_t x)\n{\n    return x;\n}\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return x < y ? x : y;\n}\nstatic inline float fmax32(float x, float y)\n{\n    return x < y ? y : x;\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline char cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return x;\n}\nstatic inline int8_t fptosi_f32_i8(float x)\n{\n    return x;",
                   "\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return x;\n}\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define y_4201 (DEFAULT_GROUP_SIZE - 1)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define y_4201 (DEFAULT_GROUP_SIZE - 1)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define y_4201 (DEFAULT_GROUP_SIZE - 1)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define y_4201 (DEFAULT_GROUP_SIZE - 1)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define y_4201 (DEFAULT_GROUP_SIZE - 1)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n#define group_sizze_4188 (DEFAULT_GROUP_SIZE)\n__kernel void chunked_reduce_kernel_5015(__local volatile\n                                         int64_t *mem_aligned_0,\n                                         int32_t arg_3879,\n                                         int32_t num_threads_5007,\n                                         int32_t per_thread_elements_5010,\n                                         __global unsigned char *mem_5302,\n                                         __global unsigned char *mem_5306)\n{\n    __local volatile char *restrict mem_5304 = mem_aligned_0;\n    int32_t wave_sizze_5567;\n    int32_t group_sizze_5568;\n    char thread_active_5569;\n    int32_t global_tid_5015;\n    int32_t local_tid_5016;\n    int32_t group_id_5017;\n    \n    global_tid_5015 = get_global_id(0);\n    local_tid_5016 = get_local_id(0);\n    group_sizze_5568 = get_local_size(0);\n    wave_sizze_5567 = LOCKSTEP_WIDTH;\n    group_id_5017 = get_group_id(0);\n    thread_active_5569 = 1;\n    \n    int32_t chunk_sizze_5021;\n    char res_5024;\n    char final_result_5038;\n    \n",
                   "    chunk_sizze_5021 = smin32(per_thread_elements_5010, squot32(arg_3879 -\n                                                                global_tid_5015 +\n                                                                num_threads_5007 -\n                                                                1,\n                                                                num_threads_5007));\n    \n    char acc_5027 = 1;\n    int32_t groupstream_mapaccum_dummy_chunk_sizze_5025 = 1;\n    \n    if (thread_active_5569) {\n        for (int32_t i_5026 = 0; i_5026 < chunk_sizze_5021; i_5026++) {\n            int32_t binop_y_5055;\n            int32_t convop_x_5056;\n            float x_5031;\n            int32_t i_5032;\n            float y_5033;\n            char res_5034;\n            char x_5035;\n            \n            binop_y_5055 = i_5026 * num_threads_5007;\n            convop_x_5056 = global_tid_5015 + binop_y_5055;\n            x_5031 = *(__global float *) &mem_5302[convop_x_5056 * 4];\n            i_5032 = convop_x_5056 + 1;\n            y_5033 = *(__global float *) &mem_5302[i_5032 * 4];\n            res_5034 = x_5031 <= y_5033;\n            x_5035 = acc_5027 && res_5034;\n            acc_5027 = x_5035;\n        }\n    }\n    res_5024 = acc_5027;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_5016, group_sizze_4188) && 1) {\n        *(__local char *) &mem_5304[local_tid_5016] = res_5024;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t skip_waves_5570;\n    int32_t my_index_5039;\n    int32_t other_offset_5040;\n    char binop_param_x_5041;\n    char binop_param_y_5042;\n    \n    my_index_5039 = local_tid_5016;\n    other_offset_5040 = 0;\n    binop_param_x_5041 = *(__local char *) &mem_5304[local_tid_5016 +\n                                                     other_offset_5040];\n    other_offset_5040 = 1;\n    while (slt32(other_offset_5040, wave_sizze_5567)) {\n        if (slt32(local_tid_5016 + other_offset_5040, group_sizze_4188) &&\n            ((local_tid_5016 - squot32(lo",
                   "cal_tid_5016, wave_sizze_5567) *\n              wave_sizze_5567) & (2 * other_offset_5040 - 1)) == 0) {\n            // read array element\n            {\n                binop_param_y_5042 = *(volatile __local\n                                       char *) &mem_5304[local_tid_5016 +\n                                                         other_offset_5040];\n            }\n            \n            char x_5043;\n            \n            if (thread_active_5569) {\n                x_5043 = binop_param_x_5041 && binop_param_y_5042;\n            }\n            binop_param_x_5041 = x_5043;\n            *(volatile __local char *) &mem_5304[local_tid_5016] =\n                binop_param_x_5041;\n        }\n        other_offset_5040 *= 2;\n    }\n    skip_waves_5570 = 1;\n    while (slt32(skip_waves_5570, squot32(group_sizze_5568 + wave_sizze_5567 -\n                                          1, wave_sizze_5567))) {\n        barrier(CLK_LOCAL_MEM_FENCE);\n        other_offset_5040 = skip_waves_5570 * wave_sizze_5567;\n        if ((local_tid_5016 - squot32(local_tid_5016, wave_sizze_5567) *\n             wave_sizze_5567) == 0 && (squot32(local_tid_5016,\n                                               wave_sizze_5567) & (2 *\n                                                                   skip_waves_5570 -\n                                                                   1)) == 0) {\n            // read array element\n            {\n                binop_param_y_5042 = *(__local\n                                       char *) &mem_5304[local_tid_5016 +\n                                                         other_offset_5040];\n            }\n            \n            char x_5043;\n            \n            if (thread_active_5569) {\n                x_5043 = binop_param_x_5041 && binop_param_y_5042;\n            }\n            binop_param_x_5041 = x_5043;\n            *(__local char *) &mem_5304[local_tid_5016] = binop_param_x_5041;\n        }\n        skip_waves_5570 *= 2;\n    }\n    final_result_5038 = binop",
                   "_param_x_5041;\n    if (local_tid_5016 == 0) {\n        *(__global char *) &mem_5306[group_id_5017] = final_result_5038;\n    }\n}\n__kernel void kernel_replicate_5342(int32_t sizze_3871, __global\n                                    unsigned char *mem_5065)\n{\n    const uint replicate_gtid_5342 = get_global_id(0);\n    \n    if (replicate_gtid_5342 >= sizze_3871)\n        return;\n    *(__global int32_t *) &mem_5065[replicate_gtid_5342 * 4] = 0;\n}\n__kernel void map_kernel_4193(int32_t sizze_3871, __global\n                              unsigned char *mem_5065, __global\n                              unsigned char *mem_5068, __global\n                              unsigned char *mem_5071)\n{\n    int32_t wave_sizze_5348;\n    int32_t group_sizze_5349;\n    char thread_active_5350;\n    int32_t write_i_4186;\n    int32_t global_tid_4193;\n    int32_t local_tid_4194;\n    int32_t group_id_4195;\n    \n    global_tid_4193 = get_global_id(0);\n    local_tid_4194 = get_local_id(0);\n    group_sizze_5349 = get_local_size(0);\n    wave_sizze_5348 = LOCKSTEP_WIDTH;\n    group_id_4195 = get_group_id(0);\n    write_i_4186 = global_tid_4193;\n    thread_active_5350 = slt32(write_i_4186, 1);\n    \n    int32_t write_index_3877;\n    int32_t write_value_3878;\n    \n    if (thread_active_5350) {\n        write_index_3877 = *(__global int32_t *) &mem_5068[write_i_4186 * 4];\n        write_value_3878 = *(__global int32_t *) &mem_5071[write_i_4186 * 4];\n    }\n    if (thread_active_5350 && (sle32(0, write_index_3877) &&\n                               slt32(write_index_3877, sizze_3871))) {\n        *(__global int32_t *) &mem_5065[write_index_3877 * 4] =\n            write_value_3878;\n    }\n}\n__kernel void map_kernel_4284(int32_t sizze_3871, int32_t y_4234, __global\n                              unsigned char *mem_5078, __global\n                              unsigned char *mem_5090, __global\n                              unsigned char *mem_5093)\n{\n    int32_t wave_sizze_5380;\n    int32_t group_sizze_5381;\n    char thread_",
                   "active_5382;\n    int32_t j_4269;\n    int32_t global_tid_4284;\n    int32_t local_tid_4285;\n    int32_t group_id_4286;\n    \n    global_tid_4284 = get_global_id(0);\n    local_tid_4285 = get_local_id(0);\n    group_sizze_5381 = get_local_size(0);\n    wave_sizze_5380 = LOCKSTEP_WIDTH;\n    group_id_4286 = get_group_id(0);\n    j_4269 = global_tid_4284;\n    thread_active_5382 = slt32(j_4269, sizze_3871);\n    \n    int32_t binop_param_y_4267;\n    int32_t group_id_4274;\n    char cond_4275;\n    int32_t final_result_4277;\n    \n    if (thread_active_5382) {\n        binop_param_y_4267 = *(__global int32_t *) &mem_5078[j_4269 * 4];\n        group_id_4274 = squot32(j_4269, y_4234);\n        cond_4275 = 0 == group_id_4274;\n        if (cond_4275) {\n            final_result_4277 = binop_param_y_4267;\n        } else {\n            int32_t carry_in_index_4276;\n            int32_t binop_param_x_4266;\n            int32_t res_4268;\n            \n            carry_in_index_4276 = group_id_4274 - 1;\n            binop_param_x_4266 = *(__global\n                                   int32_t *) &mem_5090[carry_in_index_4276 *\n                                                        4];\n            res_4268 = binop_param_x_4266 + binop_param_y_4267;\n            final_result_4277 = res_4268;\n        }\n    }\n    if (thread_active_5382) {\n        *(__global int32_t *) &mem_5093[j_4269 * 4] = final_result_4277;\n    }\n}\n__kernel void map_kernel_4428(int32_t sizze_3871, int32_t y_4234, __global\n                              unsigned char *mem_5096, __global\n                              unsigned char *mem_5099, __global\n                              unsigned char *mem_5120, __global\n                              unsigned char *mem_5123, __global\n                              unsigned char *mem_5126, __global\n                              unsigned char *mem_5129)\n{\n    int32_t wave_sizze_5417;\n    int32_t group_sizze_5418;\n    char thread_active_5419;\n    int32_t j_4411;\n    int32_t global_tid_4428;\n    int32_t l",
                   "ocal_tid_4429;\n    int32_t group_id_4430;\n    \n    global_tid_4428 = get_global_id(0);\n    local_tid_4429 = get_local_id(0);\n    group_sizze_5418 = get_local_size(0);\n    wave_sizze_5417 = LOCKSTEP_WIDTH;\n    group_id_4430 = get_group_id(0);\n    j_4411 = global_tid_4428;\n    thread_active_5419 = slt32(j_4411, sizze_3871);\n    \n    int32_t f2_4405;\n    float x2_4406;\n    int32_t group_id_4416;\n    char cond_4417;\n    int32_t final_result_4420;\n    float final_result_4421;\n    \n    if (thread_active_5419) {\n        f2_4405 = *(__global int32_t *) &mem_5096[j_4411 * 4];\n        x2_4406 = *(__global float *) &mem_5099[j_4411 * 4];\n        group_id_4416 = squot32(j_4411, y_4234);\n        cond_4417 = 0 == group_id_4416;\n        if (cond_4417) {\n            final_result_4420 = f2_4405;\n            final_result_4421 = x2_4406;\n        } else {\n            int32_t carry_in_index_4418;\n            int32_t f1_4403;\n            float x1_4404;\n            int32_t res_4407;\n            char cond_4408;\n            float res_4409;\n            \n            carry_in_index_4418 = group_id_4416 - 1;\n            f1_4403 = *(__global int32_t *) &mem_5120[carry_in_index_4418 * 4];\n            x1_4404 = *(__global float *) &mem_5123[carry_in_index_4418 * 4];\n            res_4407 = f1_4403 | f2_4405;\n            cond_4408 = slt32(0, f2_4405);\n            if (cond_4408) {\n                res_4409 = x2_4406;\n            } else {\n                float res_4410 = x1_4404 + x2_4406;\n                \n                res_4409 = res_4410;\n            }\n            final_result_4420 = res_4407;\n            final_result_4421 = res_4409;\n        }\n    }\n    if (thread_active_5419) {\n        *(__global int32_t *) &mem_5126[j_4411 * 4] = final_result_4420;\n    }\n    if (thread_active_5419) {\n        *(__global float *) &mem_5129[j_4411 * 4] = final_result_4421;\n    }\n}\n__kernel void map_kernel_4674(int32_t sizze_3871, int32_t y_4234, __global\n                              unsigned char *mem_5132, __glob",
                   "al\n                              unsigned char *mem_5135, __global\n                              unsigned char *mem_5138, __global\n                              unsigned char *mem_5141, __global\n                              unsigned char *mem_5144, __global\n                              unsigned char *mem_5194, __global\n                              unsigned char *mem_5197, __global\n                              unsigned char *mem_5200, __global\n                              unsigned char *mem_5203, __global\n                              unsigned char *mem_5206, __global\n                              unsigned char *mem_5209, __global\n                              unsigned char *mem_5212, __global\n                              unsigned char *mem_5215, __global\n                              unsigned char *mem_5218, __global\n                              unsigned char *mem_5221)\n{\n    int32_t wave_sizze_5483;\n    int32_t group_sizze_5484;\n    char thread_active_5485;\n    int32_t j_4651;\n    int32_t global_tid_4674;\n    int32_t local_tid_4675;\n    int32_t group_id_4676;\n    \n    global_tid_4674 = get_global_id(0);\n    local_tid_4675 = get_local_id(0);\n    group_sizze_5484 = get_local_size(0);\n    wave_sizze_5483 = LOCKSTEP_WIDTH;\n    group_id_4676 = get_group_id(0);\n    j_4651 = global_tid_4674;\n    thread_active_5485 = slt32(j_4651, sizze_3871);\n    \n    int32_t binop_param_y_4637;\n    int32_t f2_4638;\n    int32_t x2_4639;\n    int32_t f2_4640;\n    int32_t x2_4641;\n    int32_t group_id_4656;\n    char cond_4657;\n    int32_t final_result_4663;\n    int32_t final_result_4664;\n    int32_t final_result_4665;\n    int32_t final_result_4666;\n    int32_t final_result_4667;\n    \n    if (thread_active_5485) {\n        binop_param_y_4637 = *(__global int32_t *) &mem_5132[j_4651 * 4];\n        f2_4638 = *(__global int32_t *) &mem_5135[j_4651 * 4];\n        x2_4639 = *(__global int32_t *) &mem_5138[j_4651 * 4];\n        f2_4640 = *(__global int32_t *) &mem_5141[j_4651 * 4];\n        x2_46",
                   "41 = *(__global int32_t *) &mem_5144[j_4651 * 4];\n        group_id_4656 = squot32(j_4651, y_4234);\n        cond_4657 = 0 == group_id_4656;\n        if (cond_4657) {\n            final_result_4663 = binop_param_y_4637;\n            final_result_4664 = f2_4638;\n            final_result_4665 = x2_4639;\n            final_result_4666 = f2_4640;\n            final_result_4667 = x2_4641;\n        } else {\n            int32_t carry_in_index_4658;\n            int32_t binop_param_x_4632;\n            int32_t f1_4633;\n            int32_t x1_4634;\n            int32_t f1_4635;\n            int32_t x1_4636;\n            int32_t res_4642;\n            int32_t res_4643;\n            char cond_4644;\n            int32_t res_4645;\n            int32_t res_4647;\n            char cond_4648;\n            int32_t res_4649;\n            \n            carry_in_index_4658 = group_id_4656 - 1;\n            binop_param_x_4632 = *(__global\n                                   int32_t *) &mem_5194[carry_in_index_4658 *\n                                                        4];\n            f1_4633 = *(__global int32_t *) &mem_5197[carry_in_index_4658 * 4];\n            x1_4634 = *(__global int32_t *) &mem_5200[carry_in_index_4658 * 4];\n            f1_4635 = *(__global int32_t *) &mem_5203[carry_in_index_4658 * 4];\n            x1_4636 = *(__global int32_t *) &mem_5206[carry_in_index_4658 * 4];\n            res_4642 = binop_param_x_4632 + binop_param_y_4637;\n            res_4643 = f1_4633 | f2_4638;\n            cond_4644 = slt32(0, f2_4638);\n            if (cond_4644) {\n                res_4645 = x2_4639;\n            } else {\n                int32_t res_4646 = x1_4634 + x2_4639;\n                \n                res_4645 = res_4646;\n            }\n            res_4647 = f1_4635 | f2_4640;\n            cond_4648 = slt32(0, f2_4640);\n            if (cond_4648) {\n                res_4649 = x2_4641;\n            } else {\n                int32_t res_4650 = x1_4636 + x2_4641;\n                \n                res_4649 = res_46",
                   "50;\n            }\n            final_result_4663 = res_4642;\n            final_result_4664 = res_4643;\n            final_result_4665 = res_4645;\n            final_result_4666 = res_4647;\n            final_result_4667 = res_4649;\n        }\n    }\n    if (thread_active_5485) {\n        *(__global int32_t *) &mem_5209[j_4651 * 4] = final_result_4663;\n    }\n    if (thread_active_5485) {\n        *(__global int32_t *) &mem_5212[j_4651 * 4] = final_result_4664;\n    }\n    if (thread_active_5485) {\n        *(__global int32_t *) &mem_5215[j_4651 * 4] = final_result_4665;\n    }\n    if (thread_active_5485) {\n        *(__global int32_t *) &mem_5218[j_4651 * 4] = final_result_4666;\n    }\n    if (thread_active_5485) {\n        *(__global int32_t *) &mem_5221[j_4651 * 4] = final_result_4667;\n    }\n}\n__kernel void map_kernel_4815(int32_t sizze_3871, int32_t y_4234, __global\n                              unsigned char *mem_5224, __global\n                              unsigned char *mem_5227, __global\n                              unsigned char *mem_5251, __global\n                              unsigned char *mem_5254, __global\n                              unsigned char *mem_5257, __global\n                              unsigned char *mem_5260)\n{\n    int32_t wave_sizze_5521;\n    int32_t group_sizze_5522;\n    char thread_active_5523;\n    int32_t j_4798;\n    int32_t global_tid_4815;\n    int32_t local_tid_4816;\n    int32_t group_id_4817;\n    \n    global_tid_4815 = get_global_id(0);\n    local_tid_4816 = get_local_id(0);\n    group_sizze_5522 = get_local_size(0);\n    wave_sizze_5521 = LOCKSTEP_WIDTH;\n    group_id_4817 = get_group_id(0);\n    j_4798 = global_tid_4815;\n    thread_active_5523 = slt32(j_4798, sizze_3871);\n    \n    int32_t f2_4792;\n    int32_t x2_4793;\n    int32_t group_id_4803;\n    char cond_4804;\n    int32_t final_result_4807;\n    int32_t final_result_4808;\n    \n    if (thread_active_5523) {\n        f2_4792 = *(__global int32_t *) &mem_5224[j_4798 * 4];\n        x2_4793 = *(__global ",
                   "int32_t *) &mem_5227[j_4798 * 4];\n        group_id_4803 = squot32(j_4798, y_4234);\n        cond_4804 = 0 == group_id_4803;\n        if (cond_4804) {\n            final_result_4807 = f2_4792;\n            final_result_4808 = x2_4793;\n        } else {\n            int32_t carry_in_index_4805;\n            int32_t f1_4790;\n            int32_t x1_4791;\n            int32_t res_4794;\n            char cond_4795;\n            int32_t res_4796;\n            \n            carry_in_index_4805 = group_id_4803 - 1;\n            f1_4790 = *(__global int32_t *) &mem_5251[carry_in_index_4805 * 4];\n            x1_4791 = *(__global int32_t *) &mem_5254[carry_in_index_4805 * 4];\n            res_4794 = f1_4790 | f2_4792;\n            cond_4795 = slt32(0, f2_4792);\n            if (cond_4795) {\n                res_4796 = x2_4793;\n            } else {\n                int32_t res_4797 = x1_4791 + x2_4793;\n                \n                res_4796 = res_4797;\n            }\n            final_result_4807 = res_4794;\n            final_result_4808 = res_4796;\n        }\n    }\n    if (thread_active_5523) {\n        *(__global int32_t *) &mem_5257[j_4798 * 4] = final_result_4807;\n    }\n    if (thread_active_5523) {\n        *(__global int32_t *) &mem_5260[j_4798 * 4] = final_result_4808;\n    }\n}\n__kernel void map_kernel_4954(int32_t sizze_3871, int32_t y_4234, __global\n                              unsigned char *mem_5263, __global\n                              unsigned char *mem_5266, __global\n                              unsigned char *mem_5287, __global\n                              unsigned char *mem_5290, __global\n                              unsigned char *mem_5293, __global\n                              unsigned char *mem_5296)\n{\n    int32_t wave_sizze_5558;\n    int32_t group_sizze_5559;\n    char thread_active_5560;\n    int32_t j_4937;\n    int32_t global_tid_4954;\n    int32_t local_tid_4955;\n    int32_t group_id_4956;\n    \n    global_tid_4954 = get_global_id(0);\n    local_tid_4955 = get_local_id(0);\n",
                   "    group_sizze_5559 = get_local_size(0);\n    wave_sizze_5558 = LOCKSTEP_WIDTH;\n    group_id_4956 = get_group_id(0);\n    j_4937 = global_tid_4954;\n    thread_active_5560 = slt32(j_4937, sizze_3871);\n    \n    int32_t f2_4931;\n    int32_t x2_4932;\n    int32_t group_id_4942;\n    char cond_4943;\n    int32_t final_result_4946;\n    int32_t final_result_4947;\n    \n    if (thread_active_5560) {\n        f2_4931 = *(__global int32_t *) &mem_5263[j_4937 * 4];\n        x2_4932 = *(__global int32_t *) &mem_5266[j_4937 * 4];\n        group_id_4942 = squot32(j_4937, y_4234);\n        cond_4943 = 0 == group_id_4942;\n        if (cond_4943) {\n            final_result_4946 = f2_4931;\n            final_result_4947 = x2_4932;\n        } else {\n            int32_t carry_in_index_4944;\n            int32_t f1_4929;\n            int32_t x1_4930;\n            int32_t res_4933;\n            char cond_4934;\n            int32_t res_4935;\n            \n            carry_in_index_4944 = group_id_4942 - 1;\n            f1_4929 = *(__global int32_t *) &mem_5287[carry_in_index_4944 * 4];\n            x1_4930 = *(__global int32_t *) &mem_5290[carry_in_index_4944 * 4];\n            res_4933 = f1_4929 | f2_4931;\n            cond_4934 = slt32(0, f2_4931);\n            if (cond_4934) {\n                res_4935 = x2_4932;\n            } else {\n                int32_t res_4936 = x1_4930 + x2_4932;\n                \n                res_4935 = res_4936;\n            }\n            final_result_4946 = res_4933;\n            final_result_4947 = res_4935;\n        }\n    }\n    if (thread_active_5560) {\n        *(__global int32_t *) &mem_5293[j_4937 * 4] = final_result_4946;\n    }\n    if (thread_active_5560) {\n        *(__global int32_t *) &mem_5296[j_4937 * 4] = final_result_4947;\n    }\n}\n__kernel void map_kernel_4964(int32_t sizze_3871, __global\n                              unsigned char *sizzes_mem_5073, __global\n                              unsigned char *mem_5215, __global\n                              unsigned char *mem_52",
                   "30, __global\n                              unsigned char *mem_5296, __global\n                              unsigned char *mem_5299)\n{\n    int32_t wave_sizze_5561;\n    int32_t group_sizze_5562;\n    char thread_active_5563;\n    int32_t gtid_4957;\n    int32_t global_tid_4964;\n    int32_t local_tid_4965;\n    int32_t group_id_4966;\n    \n    global_tid_4964 = get_global_id(0);\n    local_tid_4965 = get_local_id(0);\n    group_sizze_5562 = get_local_size(0);\n    wave_sizze_5561 = LOCKSTEP_WIDTH;\n    group_id_4966 = get_group_id(0);\n    gtid_4957 = global_tid_4964;\n    thread_active_5563 = slt32(gtid_4957, sizze_3871);\n    \n    int32_t x2_4968;\n    int32_t f_4969;\n    int32_t s_4970;\n    int32_t li_4971;\n    char cond_4975;\n    int32_t res_4976;\n    \n    if (thread_active_5563) {\n        x2_4968 = *(__global int32_t *) &mem_5296[gtid_4957 * 4];\n        f_4969 = *(__global int32_t *) &sizzes_mem_5073[gtid_4957 * 4];\n        s_4970 = *(__global int32_t *) &mem_5215[gtid_4957 * 4];\n        li_4971 = *(__global int32_t *) &mem_5230[gtid_4957 * 4];\n        cond_4975 = slt32(0, f_4969);\n        if (cond_4975) {\n            char cond_4977;\n            int32_t res_4978;\n            \n            cond_4977 = slt32(0, li_4971);\n            if (cond_4977) {\n                res_4978 = li_4971;\n            } else {\n                res_4978 = f_4969;\n            }\n            res_4976 = res_4978;\n        } else {\n            int32_t x_4979;\n            char cond_4980;\n            int32_t res_4981;\n            \n            x_4979 = x2_4968 - 1;\n            cond_4980 = x_4979 == li_4971;\n            if (cond_4980) {\n                int32_t res_4982 = s_4970 - li_4971;\n                \n                res_4981 = res_4982;\n            } else {\n                res_4981 = 0;\n            }\n            res_4976 = res_4981;\n        }\n    }\n    if (thread_active_5563) {\n        *(__global int32_t *) &mem_5299[gtid_4957 * 4] = res_4976;\n    }\n}\n__kernel void map_kernel_4990(int32_t sizze_3871, __globa",
                   "l\n                              unsigned char *arr_mem_5075, __global\n                              unsigned char *mem_5146, __global\n                              unsigned char *mem_5209, __global\n                              unsigned char *mem_5215, __global\n                              unsigned char *mem_5221, __global\n                              unsigned char *mem_5230, __global\n                              unsigned char *mem_5260, __global\n                              unsigned char *mem_5302)\n{\n    int32_t wave_sizze_5564;\n    int32_t group_sizze_5565;\n    char thread_active_5566;\n    int32_t write_i_4983;\n    int32_t global_tid_4990;\n    int32_t local_tid_4991;\n    int32_t group_id_4992;\n    \n    global_tid_4990 = get_global_id(0);\n    local_tid_4991 = get_local_id(0);\n    group_sizze_5565 = get_local_size(0);\n    wave_sizze_5564 = LOCKSTEP_WIDTH;\n    group_id_4992 = get_group_id(0);\n    write_i_4983 = global_tid_4990;\n    thread_active_5566 = slt32(write_i_4983, sizze_3871);\n    \n    int32_t li_4137;\n    int32_t ff_4138;\n    int32_t binop_param_x_4139;\n    int32_t binop_param_y_4140;\n    char c_4141;\n    int32_t iT_4142;\n    float write_value_4143;\n    int32_t res_4144;\n    int32_t res_4145;\n    int32_t res_4146;\n    \n    if (thread_active_5566) {\n        li_4137 = *(__global int32_t *) &mem_5230[write_i_4983 * 4];\n        ff_4138 = *(__global int32_t *) &mem_5260[write_i_4983 * 4];\n        binop_param_x_4139 = *(__global int32_t *) &mem_5209[write_i_4983 * 4];\n        binop_param_y_4140 = *(__global int32_t *) &mem_5215[write_i_4983 * 4];\n        c_4141 = *(__global char *) &mem_5146[write_i_4983];\n        iT_4142 = *(__global int32_t *) &mem_5221[write_i_4983 * 4];\n        write_value_4143 = *(__global float *) &arr_mem_5075[write_i_4983 * 4];\n        res_4144 = li_4137 + ff_4138;\n        res_4145 = binop_param_x_4139 - binop_param_y_4140;\n        if (c_4141) {\n            int32_t x_4147;\n            int32_t res_4148;\n            \n            x_4147 =",
                   " iT_4142 - 1;\n            res_4148 = x_4147 + res_4145;\n            res_4146 = res_4148;\n        } else {\n            int32_t x_4149;\n            int32_t res_4150;\n            \n            x_4149 = res_4144 - 1;\n            res_4150 = x_4149 + res_4145;\n            res_4146 = res_4150;\n        }\n    }\n    if (thread_active_5566 && (sle32(0, res_4146) && slt32(res_4146,\n                                                           sizze_3871))) {\n        *(__global float *) &mem_5302[res_4146 * 4] = write_value_4143;\n    }\n}\n__kernel void reduce_kernel_5046(__local volatile int64_t *mem_aligned_0,\n                                 int32_t num_groups_5006, __global\n                                 unsigned char *mem_5306, __global\n                                 unsigned char *mem_5310)\n{\n    __local volatile char *restrict mem_5308 = mem_aligned_0;\n    int32_t wave_sizze_5571;\n    int32_t group_sizze_5572;\n    char thread_active_5573;\n    int32_t global_tid_5046;\n    int32_t local_tid_5047;\n    int32_t group_id_5048;\n    \n    global_tid_5046 = get_global_id(0);\n    local_tid_5047 = get_local_id(0);\n    group_sizze_5572 = get_local_size(0);\n    wave_sizze_5571 = LOCKSTEP_WIDTH;\n    group_id_5048 = get_group_id(0);\n    thread_active_5573 = 1;\n    \n    char in_bounds_5049;\n    \n    if (thread_active_5573) {\n        in_bounds_5049 = slt32(local_tid_5047, num_groups_5006);\n    }\n    \n    char final_result_5053;\n    \n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_5047, group_sizze_4188) && 1) {\n        char elem_5051;\n        \n        if (in_bounds_5049) {\n            char x_5050 = *(__global char *) &mem_5306[global_tid_5046];\n            \n            elem_5051 = x_5050;\n        } else {\n            elem_5051 = 1;\n        }\n        *(__local char *) &mem_5308[local_tid_5047] = elem_5051;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t skip_waves_5574;\n    char binop_param_x_4163;\n    char binop_param_y_4164;\n    int32_t my_index_5013;\n    int32_t other_of",
                   "fset_5014;\n    \n    my_index_5013 = local_tid_5047;\n    other_offset_5014 = 0;\n    binop_param_x_4163 = *(__local char *) &mem_5308[local_tid_5047 +\n                                                     other_offset_5014];\n    other_offset_5014 = 1;\n    while (slt32(other_offset_5014, wave_sizze_5571)) {\n        if (slt32(local_tid_5047 + other_offset_5014, group_sizze_4188) &&\n            ((local_tid_5047 - squot32(local_tid_5047, wave_sizze_5571) *\n              wave_sizze_5571) & (2 * other_offset_5014 - 1)) == 0) {\n            // read array element\n            {\n                binop_param_y_4164 = *(volatile __local\n                                       char *) &mem_5308[local_tid_5047 +\n                                                         other_offset_5014];\n            }\n            \n            char x_4165;\n            \n            if (thread_active_5573) {\n                x_4165 = binop_param_x_4163 && binop_param_y_4164;\n            }\n            binop_param_x_4163 = x_4165;\n            *(volatile __local char *) &mem_5308[local_tid_5047] =\n                binop_param_x_4163;\n        }\n        other_offset_5014 *= 2;\n    }\n    skip_waves_5574 = 1;\n    while (slt32(skip_waves_5574, squot32(group_sizze_5572 + wave_sizze_5571 -\n                                          1, wave_sizze_5571))) {\n        barrier(CLK_LOCAL_MEM_FENCE);\n        other_offset_5014 = skip_waves_5574 * wave_sizze_5571;\n        if ((local_tid_5047 - squot32(local_tid_5047, wave_sizze_5571) *\n             wave_sizze_5571) == 0 && (squot32(local_tid_5047,\n                                               wave_sizze_5571) & (2 *\n                                                                   skip_waves_5574 -\n                                                                   1)) == 0) {\n            // read array element\n            {\n                binop_param_y_4164 = *(__local\n                                       char *) &mem_5308[local_tid_5047 +\n                                  ",
                   "                       other_offset_5014];\n            }\n            \n            char x_4165;\n            \n            if (thread_active_5573) {\n                x_4165 = binop_param_x_4163 && binop_param_y_4164;\n            }\n            binop_param_x_4163 = x_4165;\n            *(__local char *) &mem_5308[local_tid_5047] = binop_param_x_4163;\n        }\n        skip_waves_5574 *= 2;\n    }\n    final_result_5053 = binop_param_x_4163;\n    if (local_tid_5047 == 0) {\n        *(__global char *) &mem_5310[group_id_5048] = final_result_5053;\n    }\n}\n__kernel void scan1_kernel_4226(__local volatile int64_t *mem_aligned_0,\n                                int32_t sizze_3871, int32_t num_iterations_4231,\n                                int32_t y_4234, __global\n                                unsigned char *sizzes_mem_5073, __global\n                                unsigned char *mem_5078, __global\n                                unsigned char *mem_5084)\n{\n    __local volatile char *restrict mem_5081 = mem_aligned_0;\n    int32_t wave_sizze_5358;\n    int32_t group_sizze_5359;\n    char thread_active_5360;\n    int32_t global_tid_4226;\n    int32_t local_tid_4227;\n    int32_t group_id_4228;\n    \n    global_tid_4226 = get_global_id(0);\n    local_tid_4227 = get_local_id(0);\n    group_sizze_5359 = get_local_size(0);\n    wave_sizze_5358 = LOCKSTEP_WIDTH;\n    group_id_4228 = get_group_id(0);\n    thread_active_5360 = 1;\n    \n    int32_t x_4235;\n    char is_first_thread_4248;\n    int32_t result_4252;\n    \n    if (thread_active_5360) {\n        x_4235 = group_id_4228 * y_4234;\n        is_first_thread_4248 = local_tid_4227 == 0;\n        \n        int32_t binop_param_x_merge_4232 = 0;\n        \n        for (int32_t i_4233 = 0; i_4233 < num_iterations_4231; i_4233++) {\n            int32_t y_4236 = i_4233 * group_sizze_4188;\n            int32_t offset_4237 = x_4235 + y_4236;\n            int32_t j_4238 = offset_4237 + local_tid_4227;\n            char cond_4239 = slt32(j_4238, sizze_3871);\n           ",
                   " int32_t foldres_4241;\n            \n            if (cond_4239) {\n                int32_t sizzes_elem_4240;\n                int32_t res_4215;\n                \n                sizzes_elem_4240 = *(__global\n                                     int32_t *) &sizzes_mem_5073[j_4238 * 4];\n                res_4215 = binop_param_x_merge_4232 + sizzes_elem_4240;\n                foldres_4241 = res_4215;\n            } else {\n                foldres_4241 = binop_param_x_merge_4232;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_4227, group_sizze_4188) && 1) {\n                *(__local int32_t *) &mem_5081[local_tid_4227 * 4] =\n                    foldres_4241;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_4216;\n            int32_t other_index_4217;\n            int32_t binop_param_x_4218;\n            int32_t binop_param_y_4219;\n            int32_t my_index_5363;\n            int32_t other_index_5364;\n            int32_t binop_param_x_5365;\n            int32_t binop_param_y_5366;\n            \n            my_index_4216 = local_tid_4227;\n            if (slt32(local_tid_4227, group_sizze_4188)) {\n                binop_param_y_4219 = *(volatile __local\n                                       int32_t *) &mem_5081[local_tid_4227 *\n                                                            sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_5368 = 1;\n                \n                while (slt32(skip_threads_5368, 32)) {\n                    if (slt32(local_tid_4227, group_sizze_4188) &&\n                        sle32(skip_threads_5368, local_tid_4227 -\n                              squot32(local_tid_4227, 32) * 32)) {\n                        // read operands\n                        {\n                            binop_param_x_4218 = *(volatile __local\n                                                   int32_t *",
                   ") &mem_5081[(local_tid_4227 -\n                                                                         skip_threads_5368) *\n                                                                        sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_4220 = binop_param_x_4218 +\n                                    binop_param_y_4219;\n                            \n                            binop_param_y_4219 = res_4220;\n                        }\n                    }\n                    if (sle32(wave_sizze_5358, skip_threads_5368)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_4227, group_sizze_4188) &&\n                        sle32(skip_threads_5368, local_tid_4227 -\n                              squot32(local_tid_4227, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_5081[local_tid_4227 *\n                                                   sizeof(int32_t)] =\n                                binop_param_y_4219;\n                        }\n                    }\n                    if (sle32(wave_sizze_5358, skip_threads_5368)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5368 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_4227 - squot32(local_tid_4227, 32) * 32) == 31 &&\n                    slt32(local_tid_4227, group_sizze_4188)) {\n                    *(volatile __local\n                      int32_t *) &mem_5081[squot32(local_tid_4227, 32) *\n                                           sizeof(int32_t)] =\n                        binop_param_y_4219;\n                }\n    ",
                   "        }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_4227, 32) == 0 && slt32(local_tid_4227,\n                                                              group_sizze_4188)) {\n                    binop_param_y_5366 = *(volatile __local\n                                           int32_t *) &mem_5081[local_tid_4227 *\n                                                                sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_5369 = 1;\n                    \n                    while (slt32(skip_threads_5369, 32)) {\n                        if ((squot32(local_tid_4227, 32) == 0 &&\n                             slt32(local_tid_4227, group_sizze_4188)) &&\n                            sle32(skip_threads_5369, local_tid_4227 -\n                                  squot32(local_tid_4227, 32) * 32)) {\n                            // read operands\n                            {\n                                binop_param_x_5365 = *(volatile __local\n                                                       int32_t *) &mem_5081[(local_tid_4227 -\n                                                                             skip_threads_5369) *\n                                                                            sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                int32_t res_5367 = binop_param_x_5365 +\n                                        binop_param_y_5366;\n                                \n                                binop_param_y_5366 = res_5367;\n                            }\n                        }\n                        if (sle32(wave_sizze_5358, skip_threads_5369)) {\n                            barrier(CLK_LOCAL_ME",
                   "M_FENCE);\n                        }\n                        if ((squot32(local_tid_4227, 32) == 0 &&\n                             slt32(local_tid_4227, group_sizze_4188)) &&\n                            sle32(skip_threads_5369, local_tid_4227 -\n                                  squot32(local_tid_4227, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_5081[local_tid_4227 *\n                                                       sizeof(int32_t)] =\n                                    binop_param_y_5366;\n                            }\n                        }\n                        if (sle32(wave_sizze_5358, skip_threads_5369)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_5369 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_4227, 32) == 0 || !slt32(local_tid_4227,\n                                                                 group_sizze_4188))) {\n                    // read operands\n                    {\n                        binop_param_x_4218 = *(volatile __local\n                                               int32_t *) &mem_5081[(squot32(local_tid_4227,\n                                                                             32) -\n                                                                     1) *\n                                                                    sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_4220 = binop_param_x_4218 +\n                                binop_param_y_4219;\n                        \n                        binop_param_y_4219 = res_4220;\n                    }\n",
                   "                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_5081[local_tid_4227 *\n                                                                sizeof(int32_t)] =\n                            binop_param_y_4219;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_4227, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_5081[local_tid_4227 *\n                                                            sizeof(int32_t)] =\n                        binop_param_y_4219;\n                }\n            }\n            if (cond_4239) {\n                int32_t scanned_elem_4245 = *(__local\n                                              int32_t *) &mem_5081[local_tid_4227 *\n                                                                   4];\n                \n                *(__global int32_t *) &mem_5078[j_4238 * 4] = scanned_elem_4245;\n            }\n            \n            int32_t new_carry_4250;\n            \n            if (is_first_thread_4248) {\n                int32_t carry_4249 = *(__local int32_t *) &mem_5081[y_4201 * 4];\n                \n                new_carry_4250 = carry_4249;\n            } else {\n                new_carry_4250 = 0;\n            }\n            \n            int32_t binop_param_x_merge_tmp_5362 = new_carry_4250;\n            \n            binop_param_x_merge_4232 = binop_param_x_merge_tmp_5362;\n        }\n        result_4252 = binop_param_x_merge_4232;\n    }\n    if (local_tid_4227 == 0) {\n        *(__global int32_t *) &mem_5084[group_id_4228 * 4] = result_4252;\n    }\n}\n__kernel void scan1_kernel_4341(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t sizze_3871, int32_t count_3890,\n                                int32_t num_itera",
                   "tions_4231, int32_t y_4234,\n                                __global unsigned char *sizzes_mem_5073,\n                                __global unsigned char *arr_mem_5075, __global\n                                unsigned char *mem_5093, __global\n                                unsigned char *mem_5096, __global\n                                unsigned char *mem_5099, __global\n                                unsigned char *mem_5108, __global\n                                unsigned char *mem_5111)\n{\n    __local volatile char *restrict mem_5102 = mem_aligned_0;\n    __local volatile char *restrict mem_5105 = mem_aligned_1;\n    int32_t wave_sizze_5383;\n    int32_t group_sizze_5384;\n    char thread_active_5385;\n    int32_t global_tid_4341;\n    int32_t local_tid_4342;\n    int32_t group_id_4343;\n    \n    global_tid_4341 = get_global_id(0);\n    local_tid_4342 = get_local_id(0);\n    group_sizze_5384 = get_local_size(0);\n    wave_sizze_5383 = LOCKSTEP_WIDTH;\n    group_id_4343 = get_group_id(0);\n    thread_active_5385 = 1;\n    \n    int32_t x_4351;\n    char is_first_thread_4371;\n    int32_t result_4378;\n    float result_4379;\n    \n    if (thread_active_5385) {\n        x_4351 = group_id_4343 * y_4234;\n        is_first_thread_4371 = local_tid_4342 == 0;\n        \n        int32_t f1_merge_4347;\n        float x1_merge_4348;\n        \n        f1_merge_4347 = 0;\n        x1_merge_4348 = 0.0F;\n        for (int32_t i_4349 = 0; i_4349 < num_iterations_4231; i_4349++) {\n            int32_t y_4352 = i_4349 * group_sizze_4188;\n            int32_t offset_4353 = x_4351 + y_4352;\n            int32_t j_4354 = offset_4353 + local_tid_4342;\n            char cond_4355 = slt32(j_4354, sizze_3871);\n            int32_t foldres_4358;\n            float foldres_4359;\n            \n            if (cond_4355) {\n                int32_t sizzes_elem_4357;\n                char cond_4305;\n                float res_4306;\n                int32_t res_4318;\n                char cond_4319;\n                float res_432",
                   "0;\n                \n                sizzes_elem_4357 = *(__global\n                                     int32_t *) &sizzes_mem_5073[j_4354 * 4];\n                cond_4305 = slt32(sizzes_elem_4357, 1);\n                if (cond_4305) {\n                    res_4306 = 0.0F;\n                } else {\n                    int32_t res_4307;\n                    char cond_4308;\n                    int32_t res_4309;\n                    int32_t arg_4312;\n                    int32_t x_4313;\n                    int32_t y_4314;\n                    int32_t x_4315;\n                    int32_t res_4316;\n                    float res_4317;\n                    \n                    res_4307 = *(__global int32_t *) &mem_5093[j_4354 * 4];\n                    cond_4308 = slt32(0, j_4354);\n                    if (cond_4308) {\n                        int32_t i_4310;\n                        int32_t res_4311;\n                        \n                        i_4310 = j_4354 - 1;\n                        res_4311 = *(__global int32_t *) &mem_5093[i_4310 * 4];\n                        res_4309 = res_4311;\n                    } else {\n                        res_4309 = 0;\n                    }\n                    arg_4312 = res_4307 - 1;\n                    x_4313 = arg_4312 - res_4309;\n                    y_4314 = x_4313 + 1;\n                    x_4315 = smod32(count_3890, y_4314);\n                    res_4316 = x_4315 + res_4309;\n                    res_4317 = *(__global float *) &arr_mem_5075[res_4316 * 4];\n                    res_4306 = res_4317;\n                }\n                res_4318 = f1_merge_4347 | sizzes_elem_4357;\n                cond_4319 = slt32(0, sizzes_elem_4357);\n                if (cond_4319) {\n                    res_4320 = res_4306;\n                } else {\n                    float res_4321 = x1_merge_4348 + res_4306;\n                    \n                    res_4320 = res_4321;\n                }\n                foldres_4358 = res_4318;\n                foldres_4359 = res_4320;\n",
                   "            } else {\n                foldres_4358 = f1_merge_4347;\n                foldres_4359 = x1_merge_4348;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_4342, group_sizze_4188) && 1) {\n                *(__local int32_t *) &mem_5102[local_tid_4342 * 4] =\n                    foldres_4358;\n                *(__local float *) &mem_5105[local_tid_4342 * 4] = foldres_4359;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_4322;\n            int32_t other_index_4323;\n            int32_t f1_4324;\n            float x1_4325;\n            int32_t f2_4326;\n            float x2_4327;\n            int32_t my_index_5390;\n            int32_t other_index_5391;\n            int32_t f1_5392;\n            float x1_5393;\n            int32_t f2_5394;\n            float x2_5395;\n            \n            my_index_4322 = local_tid_4342;\n            if (slt32(local_tid_4342, group_sizze_4188)) {\n                f2_4326 = *(volatile __local\n                            int32_t *) &mem_5102[local_tid_4342 *\n                                                 sizeof(int32_t)];\n                x2_4327 = *(volatile __local float *) &mem_5105[local_tid_4342 *\n                                                                sizeof(float)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_5400 = 1;\n                \n                while (slt32(skip_threads_5400, 32)) {\n                    if (slt32(local_tid_4342, group_sizze_4188) &&\n                        sle32(skip_threads_5400, local_tid_4342 -\n                              squot32(local_tid_4342, 32) * 32)) {\n                        // read operands\n                        {\n                            f1_4324 = *(volatile __local\n                                        int32_t *) &mem_5102[(local_tid_4342 -\n                                                              skip_threads_5400",
                   ") *\n                                                             sizeof(int32_t)];\n                            x1_4325 = *(volatile __local\n                                        float *) &mem_5105[(local_tid_4342 -\n                                                            skip_threads_5400) *\n                                                           sizeof(float)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_4328;\n                            char cond_4329;\n                            float res_4330;\n                            \n                            res_4328 = f1_4324 | f2_4326;\n                            cond_4329 = slt32(0, f2_4326);\n                            if (cond_4329) {\n                                res_4330 = x2_4327;\n                            } else {\n                                float res_4331 = x1_4325 + x2_4327;\n                                \n                                res_4330 = res_4331;\n                            }\n                            f2_4326 = res_4328;\n                            x2_4327 = res_4330;\n                        }\n                    }\n                    if (sle32(wave_sizze_5383, skip_threads_5400)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_4342, group_sizze_4188) &&\n                        sle32(skip_threads_5400, local_tid_4342 -\n                              squot32(local_tid_4342, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_5102[local_tid_4342 *\n                                                   sizeof(int32_t)] = f2_4326;\n                            *(volatile __local\n                              float *) &mem_5105[local_tid_4342 *\n                                                 sizeof(float)] = x2_4",
                   "327;\n                        }\n                    }\n                    if (sle32(wave_sizze_5383, skip_threads_5400)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5400 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_4342 - squot32(local_tid_4342, 32) * 32) == 31 &&\n                    slt32(local_tid_4342, group_sizze_4188)) {\n                    *(volatile __local\n                      int32_t *) &mem_5102[squot32(local_tid_4342, 32) *\n                                           sizeof(int32_t)] = f2_4326;\n                    *(volatile __local\n                      float *) &mem_5105[squot32(local_tid_4342, 32) *\n                                         sizeof(float)] = x2_4327;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_4342, 32) == 0 && slt32(local_tid_4342,\n                                                              group_sizze_4188)) {\n                    f2_5394 = *(volatile __local\n                                int32_t *) &mem_5102[local_tid_4342 *\n                                                     sizeof(int32_t)];\n                    x2_5395 = *(volatile __local\n                                float *) &mem_5105[local_tid_4342 *\n                                                   sizeof(float)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_5401 = 1;\n                    \n                    while (slt32(skip_threads_5401, 32)) {\n                        if ((squot32(local_tid_4342, 32) == 0 &&\n                             slt32(local_tid_4342, group_sizze_4188)) &&\n         ",
                   "                   sle32(skip_threads_5401, local_tid_4342 -\n                                  squot32(local_tid_4342, 32) * 32)) {\n                            // read operands\n                            {\n                                f1_5392 = *(volatile __local\n                                            int32_t *) &mem_5102[(local_tid_4342 -\n                                                                  skip_threads_5401) *\n                                                                 sizeof(int32_t)];\n                                x1_5393 = *(volatile __local\n                                            float *) &mem_5105[(local_tid_4342 -\n                                                                skip_threads_5401) *\n                                                               sizeof(float)];\n                            }\n                            // perform operation\n                            {\n                                int32_t res_5396;\n                                char cond_5397;\n                                float res_5398;\n                                \n                                res_5396 = f1_5392 | f2_5394;\n                                cond_5397 = slt32(0, f2_5394);\n                                if (cond_5397) {\n                                    res_5398 = x2_5395;\n                                } else {\n                                    float res_5399 = x1_5393 + x2_5395;\n                                    \n                                    res_5398 = res_5399;\n                                }\n                                f2_5394 = res_5396;\n                                x2_5395 = res_5398;\n                            }\n                        }\n                        if (sle32(wave_sizze_5383, skip_threads_5401)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_4342, 32) == 0 &&\n                             slt",
                   "32(local_tid_4342, group_sizze_4188)) &&\n                            sle32(skip_threads_5401, local_tid_4342 -\n                                  squot32(local_tid_4342, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_5102[local_tid_4342 *\n                                                       sizeof(int32_t)] =\n                                    f2_5394;\n                                *(volatile __local\n                                  float *) &mem_5105[local_tid_4342 *\n                                                     sizeof(float)] = x2_5395;\n                            }\n                        }\n                        if (sle32(wave_sizze_5383, skip_threads_5401)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_5401 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_4342, 32) == 0 || !slt32(local_tid_4342,\n                                                                 group_sizze_4188))) {\n                    // read operands\n                    {\n                        f1_4324 = *(volatile __local\n                                    int32_t *) &mem_5102[(squot32(local_tid_4342,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        x1_4325 = *(volatile __local\n                                    float *) &mem_5105[(squot32(local_tid_4342,\n                                                                32) - 1) *\n                                                       sizeof(float)];\n                    }\n                    // perform operation\n                  ",
                   "  {\n                        int32_t res_4328;\n                        char cond_4329;\n                        float res_4330;\n                        \n                        res_4328 = f1_4324 | f2_4326;\n                        cond_4329 = slt32(0, f2_4326);\n                        if (cond_4329) {\n                            res_4330 = x2_4327;\n                        } else {\n                            float res_4331 = x1_4325 + x2_4327;\n                            \n                            res_4330 = res_4331;\n                        }\n                        f2_4326 = res_4328;\n                        x2_4327 = res_4330;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_5102[local_tid_4342 *\n                                                                sizeof(int32_t)] =\n                            f2_4326;\n                        *(volatile __local float *) &mem_5105[local_tid_4342 *\n                                                              sizeof(float)] =\n                            x2_4327;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_4342, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_5102[local_tid_4342 *\n                                                            sizeof(int32_t)] =\n                        f2_4326;\n                    *(volatile __local float *) &mem_5105[local_tid_4342 *\n                                                          sizeof(float)] =\n                        x2_4327;\n                }\n            }\n            if (cond_4355) {\n                int32_t scanned_elem_4365;\n                float scanned_elem_4366;\n                \n                scanned_elem_4365 = *(__local\n                                      int32_t *) &mem_5102[local_tid_4342 * 4];\n",
                   "                scanned_elem_4366 = *(__local\n                                      float *) &mem_5105[local_tid_4342 * 4];\n                *(__global int32_t *) &mem_5096[j_4354 * 4] = scanned_elem_4365;\n                *(__global float *) &mem_5099[j_4354 * 4] = scanned_elem_4366;\n            }\n            \n            int32_t new_carry_4374;\n            float new_carry_4375;\n            \n            if (is_first_thread_4371) {\n                int32_t carry_4372;\n                float carry_4373;\n                \n                carry_4372 = *(__local int32_t *) &mem_5102[y_4201 * 4];\n                carry_4373 = *(__local float *) &mem_5105[y_4201 * 4];\n                new_carry_4374 = carry_4372;\n                new_carry_4375 = carry_4373;\n            } else {\n                new_carry_4374 = 0;\n                new_carry_4375 = 0.0F;\n            }\n            \n            int32_t f1_merge_tmp_5388 = new_carry_4374;\n            float x1_merge_tmp_5389;\n            \n            x1_merge_tmp_5389 = new_carry_4375;\n            f1_merge_4347 = f1_merge_tmp_5388;\n            x1_merge_4348 = x1_merge_tmp_5389;\n        }\n        result_4378 = f1_merge_4347;\n        result_4379 = x1_merge_4348;\n    }\n    if (local_tid_4342 == 0) {\n        *(__global int32_t *) &mem_5108[group_id_4343 * 4] = result_4378;\n    }\n    if (local_tid_4342 == 0) {\n        *(__global float *) &mem_5111[group_id_4343 * 4] = result_4379;\n    }\n}\n__kernel void scan1_kernel_4507(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                __local volatile int64_t *mem_aligned_2,\n                                __local volatile int64_t *mem_aligned_3,\n                                __local volatile int64_t *mem_aligned_4,\n                                int32_t sizze_3871, int32_t num_iterations_4231,\n                                int32_t y_4234, __global\n                                unsigned char *sizzes_mem_5",
                   "073, __global\n                                unsigned char *arr_mem_5075, __global\n                                unsigned char *mem_5129, __global\n                                unsigned char *mem_5132, __global\n                                unsigned char *mem_5135, __global\n                                unsigned char *mem_5138, __global\n                                unsigned char *mem_5141, __global\n                                unsigned char *mem_5144, __global\n                                unsigned char *mem_5146, __global\n                                unsigned char *mem_5164, __global\n                                unsigned char *mem_5167, __global\n                                unsigned char *mem_5170, __global\n                                unsigned char *mem_5173, __global\n                                unsigned char *mem_5176)\n{\n    __local volatile char *restrict mem_5149 = mem_aligned_0;\n    __local volatile char *restrict mem_5152 = mem_aligned_1;\n    __local volatile char *restrict mem_5155 = mem_aligned_2;\n    __local volatile char *restrict mem_5158 = mem_aligned_3;\n    __local volatile char *restrict mem_5161 = mem_aligned_4;\n    int32_t wave_sizze_5420;\n    int32_t group_sizze_5421;\n    char thread_active_5422;\n    int32_t global_tid_4507;\n    int32_t local_tid_4508;\n    int32_t group_id_4509;\n    \n    global_tid_4507 = get_global_id(0);\n    local_tid_4508 = get_local_id(0);\n    group_sizze_5421 = get_local_size(0);\n    wave_sizze_5420 = LOCKSTEP_WIDTH;\n    group_id_4509 = get_group_id(0);\n    thread_active_5422 = 1;\n    \n    int32_t x_4520;\n    char is_first_thread_4561;\n    int32_t result_4578;\n    int32_t result_4579;\n    int32_t result_4580;\n    int32_t result_4581;\n    int32_t result_4582;\n    \n    if (thread_active_5422) {\n        x_4520 = group_id_4509 * y_4234;\n        is_first_thread_4561 = local_tid_4508 == 0;\n        \n        int32_t binop_param_x_merge_4513;\n        int32_t f1_merge_4514;\n        int32_t x1_merge_4515;",
                   "\n        int32_t f1_merge_4516;\n        int32_t x1_merge_4517;\n        \n        binop_param_x_merge_4513 = 0;\n        f1_merge_4514 = 0;\n        x1_merge_4515 = 0;\n        f1_merge_4516 = 0;\n        x1_merge_4517 = 0;\n        for (int32_t i_4518 = 0; i_4518 < num_iterations_4231; i_4518++) {\n            int32_t y_4521 = i_4518 * group_sizze_4188;\n            int32_t offset_4522 = x_4520 + y_4521;\n            int32_t j_4523 = offset_4522 + local_tid_4508;\n            char cond_4524 = slt32(j_4523, sizze_3871);\n            int32_t foldres_4529;\n            int32_t foldres_4530;\n            int32_t foldres_4531;\n            int32_t foldres_4532;\n            int32_t foldres_4533;\n            \n            if (cond_4524) {\n                float res_elem_4525;\n                float arr_elem_4526;\n                int32_t sizzes_elem_4527;\n                char res_4453;\n                int32_t res_4454;\n                int32_t res_4455;\n                char cond_4456;\n                int32_t res_4457;\n                int32_t res_4459;\n                int32_t res_4460;\n                int32_t res_4461;\n                \n                res_elem_4525 = *(__global float *) &mem_5129[j_4523 * 4];\n                arr_elem_4526 = *(__global float *) &arr_mem_5075[j_4523 * 4];\n                sizzes_elem_4527 = *(__global\n                                     int32_t *) &sizzes_mem_5073[j_4523 * 4];\n                res_4453 = arr_elem_4526 < res_elem_4525;\n                res_4454 = binop_param_x_merge_4513 + sizzes_elem_4527;\n                res_4455 = f1_merge_4514 | sizzes_elem_4527;\n                cond_4456 = slt32(0, sizzes_elem_4527);\n                if (cond_4456) {\n                    res_4457 = sizzes_elem_4527;\n                } else {\n                    int32_t res_4458 = x1_merge_4515 + sizzes_elem_4527;\n                    \n                    res_4457 = res_4458;\n                }\n                if (res_4453) {\n                    res_4459 = 1;\n                } else",
                   " {\n                    res_4459 = 0;\n                }\n                res_4460 = f1_merge_4516 | sizzes_elem_4527;\n                if (cond_4456) {\n                    res_4461 = res_4459;\n                } else {\n                    int32_t res_4462 = x1_merge_4517 + res_4459;\n                    \n                    res_4461 = res_4462;\n                }\n                *(__global char *) &mem_5146[j_4523] = res_4453;\n                foldres_4529 = res_4454;\n                foldres_4530 = res_4455;\n                foldres_4531 = res_4457;\n                foldres_4532 = res_4460;\n                foldres_4533 = res_4461;\n            } else {\n                foldres_4529 = binop_param_x_merge_4513;\n                foldres_4530 = f1_merge_4514;\n                foldres_4531 = x1_merge_4515;\n                foldres_4532 = f1_merge_4516;\n                foldres_4533 = x1_merge_4517;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_4508, group_sizze_4188) && 1) {\n                *(__local int32_t *) &mem_5149[local_tid_4508 * 4] =\n                    foldres_4529;\n                *(__local int32_t *) &mem_5152[local_tid_4508 * 4] =\n                    foldres_4530;\n                *(__local int32_t *) &mem_5155[local_tid_4508 * 4] =\n                    foldres_4531;\n                *(__local int32_t *) &mem_5158[local_tid_4508 * 4] =\n                    foldres_4532;\n                *(__local int32_t *) &mem_5161[local_tid_4508 * 4] =\n                    foldres_4533;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_4463;\n            int32_t other_index_4464;\n            int32_t binop_param_x_4465;\n            int32_t f1_4466;\n            int32_t x1_4467;\n            int32_t f1_4468;\n            int32_t x1_4469;\n            int32_t binop_param_y_4470;\n            int32_t f2_4471;\n            int32_t x2_4472;\n            int32_t f2_4473;\n            int32_t x2_4474;\n            int32_t",
                   " my_index_5434;\n            int32_t other_index_5435;\n            int32_t binop_param_x_5436;\n            int32_t f1_5437;\n            int32_t x1_5438;\n            int32_t f1_5439;\n            int32_t x1_5440;\n            int32_t binop_param_y_5441;\n            int32_t f2_5442;\n            int32_t x2_5443;\n            int32_t f2_5444;\n            int32_t x2_5445;\n            \n            my_index_4463 = local_tid_4508;\n            if (slt32(local_tid_4508, group_sizze_4188)) {\n                binop_param_y_4470 = *(volatile __local\n                                       int32_t *) &mem_5149[local_tid_4508 *\n                                                            sizeof(int32_t)];\n                f2_4471 = *(volatile __local\n                            int32_t *) &mem_5152[local_tid_4508 *\n                                                 sizeof(int32_t)];\n                x2_4472 = *(volatile __local\n                            int32_t *) &mem_5155[local_tid_4508 *\n                                                 sizeof(int32_t)];\n                f2_4473 = *(volatile __local\n                            int32_t *) &mem_5158[local_tid_4508 *\n                                                 sizeof(int32_t)];\n                x2_4474 = *(volatile __local\n                            int32_t *) &mem_5161[local_tid_4508 *\n                                                 sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_5455 = 1;\n                \n                while (slt32(skip_threads_5455, 32)) {\n                    if (slt32(local_tid_4508, group_sizze_4188) &&\n                        sle32(skip_threads_5455, local_tid_4508 -\n                              squot32(local_tid_4508, 32) * 32)) {\n                        // read operands\n                        {\n                            binop_param_x_4465 = *(volatile __local\n                                                   in",
                   "t32_t *) &mem_5149[(local_tid_4508 -\n                                                                         skip_threads_5455) *\n                                                                        sizeof(int32_t)];\n                            f1_4466 = *(volatile __local\n                                        int32_t *) &mem_5152[(local_tid_4508 -\n                                                              skip_threads_5455) *\n                                                             sizeof(int32_t)];\n                            x1_4467 = *(volatile __local\n                                        int32_t *) &mem_5155[(local_tid_4508 -\n                                                              skip_threads_5455) *\n                                                             sizeof(int32_t)];\n                            f1_4468 = *(volatile __local\n                                        int32_t *) &mem_5158[(local_tid_4508 -\n                                                              skip_threads_5455) *\n                                                             sizeof(int32_t)];\n                            x1_4469 = *(volatile __local\n                                        int32_t *) &mem_5161[(local_tid_4508 -\n                                                              skip_threads_5455) *\n                                                             sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_4475;\n                            int32_t res_4476;\n                            char cond_4477;\n                            int32_t res_4478;\n                            int32_t res_4480;\n                            char cond_4481;\n                            int32_t res_4482;\n                            \n                            res_4475 = binop_param_x_4465 + binop_param_y_4470;\n                            res_4476 = f1_4466 | f2_4471;\n     ",
                   "                       cond_4477 = slt32(0, f2_4471);\n                            if (cond_4477) {\n                                res_4478 = x2_4472;\n                            } else {\n                                int32_t res_4479 = x1_4467 + x2_4472;\n                                \n                                res_4478 = res_4479;\n                            }\n                            res_4480 = f1_4468 | f2_4473;\n                            cond_4481 = slt32(0, f2_4473);\n                            if (cond_4481) {\n                                res_4482 = x2_4474;\n                            } else {\n                                int32_t res_4483 = x1_4469 + x2_4474;\n                                \n                                res_4482 = res_4483;\n                            }\n                            binop_param_y_4470 = res_4475;\n                            f2_4471 = res_4476;\n                            x2_4472 = res_4478;\n                            f2_4473 = res_4480;\n                            x2_4474 = res_4482;\n                        }\n                    }\n                    if (sle32(wave_sizze_5420, skip_threads_5455)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_4508, group_sizze_4188) &&\n                        sle32(skip_threads_5455, local_tid_4508 -\n                              squot32(local_tid_4508, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_5149[local_tid_4508 *\n                                                   sizeof(int32_t)] =\n                                binop_param_y_4470;\n                            *(volatile __local\n                              int32_t *) &mem_5152[local_tid_4508 *\n                                                   sizeof(int32_t)] = f2_4471;\n                            *(volatile __local\n ",
                   "                             int32_t *) &mem_5155[local_tid_4508 *\n                                                   sizeof(int32_t)] = x2_4472;\n                            *(volatile __local\n                              int32_t *) &mem_5158[local_tid_4508 *\n                                                   sizeof(int32_t)] = f2_4473;\n                            *(volatile __local\n                              int32_t *) &mem_5161[local_tid_4508 *\n                                                   sizeof(int32_t)] = x2_4474;\n                        }\n                    }\n                    if (sle32(wave_sizze_5420, skip_threads_5455)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5455 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_4508 - squot32(local_tid_4508, 32) * 32) == 31 &&\n                    slt32(local_tid_4508, group_sizze_4188)) {\n                    *(volatile __local\n                      int32_t *) &mem_5149[squot32(local_tid_4508, 32) *\n                                           sizeof(int32_t)] =\n                        binop_param_y_4470;\n                    *(volatile __local\n                      int32_t *) &mem_5152[squot32(local_tid_4508, 32) *\n                                           sizeof(int32_t)] = f2_4471;\n                    *(volatile __local\n                      int32_t *) &mem_5155[squot32(local_tid_4508, 32) *\n                                           sizeof(int32_t)] = x2_4472;\n                    *(volatile __local\n                      int32_t *) &mem_5158[squot32(local_tid_4508, 32) *\n                                           sizeof(int32_t)] = f2_4473;\n                    *(volatile __local\n                      int32_t *) &mem_5161[squot32(local_tid_4508, 32) *\n                                           siz",
                   "eof(int32_t)] = x2_4474;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_4508, 32) == 0 && slt32(local_tid_4508,\n                                                              group_sizze_4188)) {\n                    binop_param_y_5441 = *(volatile __local\n                                           int32_t *) &mem_5149[local_tid_4508 *\n                                                                sizeof(int32_t)];\n                    f2_5442 = *(volatile __local\n                                int32_t *) &mem_5152[local_tid_4508 *\n                                                     sizeof(int32_t)];\n                    x2_5443 = *(volatile __local\n                                int32_t *) &mem_5155[local_tid_4508 *\n                                                     sizeof(int32_t)];\n                    f2_5444 = *(volatile __local\n                                int32_t *) &mem_5158[local_tid_4508 *\n                                                     sizeof(int32_t)];\n                    x2_5445 = *(volatile __local\n                                int32_t *) &mem_5161[local_tid_4508 *\n                                                     sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_5456 = 1;\n                    \n                    while (slt32(skip_threads_5456, 32)) {\n                        if ((squot32(local_tid_4508, 32) == 0 &&\n                             slt32(local_tid_4508, group_sizze_4188)) &&\n                            sle32(skip_threads_5456, local_tid_4508 -\n                                  squot32(local_tid_4508, 32) * 32)) {\n                            // read operands\n                            {\n                                binop_param_x_5436 = *(volatile _",
                   "_local\n                                                       int32_t *) &mem_5149[(local_tid_4508 -\n                                                                             skip_threads_5456) *\n                                                                            sizeof(int32_t)];\n                                f1_5437 = *(volatile __local\n                                            int32_t *) &mem_5152[(local_tid_4508 -\n                                                                  skip_threads_5456) *\n                                                                 sizeof(int32_t)];\n                                x1_5438 = *(volatile __local\n                                            int32_t *) &mem_5155[(local_tid_4508 -\n                                                                  skip_threads_5456) *\n                                                                 sizeof(int32_t)];\n                                f1_5439 = *(volatile __local\n                                            int32_t *) &mem_5158[(local_tid_4508 -\n                                                                  skip_threads_5456) *\n                                                                 sizeof(int32_t)];\n                                x1_5440 = *(volatile __local\n                                            int32_t *) &mem_5161[(local_tid_4508 -\n                                                                  skip_threads_5456) *\n                                                                 sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                int32_t res_5446;\n                                int32_t res_5447;\n                                char cond_5448;\n                                int32_t res_5449;\n                                int32_t res_5451;\n                                char cond_5452;\n                                int32_t res_54",
                   "53;\n                                \n                                res_5446 = binop_param_x_5436 +\n                                    binop_param_y_5441;\n                                res_5447 = f1_5437 | f2_5442;\n                                cond_5448 = slt32(0, f2_5442);\n                                if (cond_5448) {\n                                    res_5449 = x2_5443;\n                                } else {\n                                    int32_t res_5450 = x1_5438 + x2_5443;\n                                    \n                                    res_5449 = res_5450;\n                                }\n                                res_5451 = f1_5439 | f2_5444;\n                                cond_5452 = slt32(0, f2_5444);\n                                if (cond_5452) {\n                                    res_5453 = x2_5445;\n                                } else {\n                                    int32_t res_5454 = x1_5440 + x2_5445;\n                                    \n                                    res_5453 = res_5454;\n                                }\n                                binop_param_y_5441 = res_5446;\n                                f2_5442 = res_5447;\n                                x2_5443 = res_5449;\n                                f2_5444 = res_5451;\n                                x2_5445 = res_5453;\n                            }\n                        }\n                        if (sle32(wave_sizze_5420, skip_threads_5456)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_4508, 32) == 0 &&\n                             slt32(local_tid_4508, group_sizze_4188)) &&\n                            sle32(skip_threads_5456, local_tid_4508 -\n                                  squot32(local_tid_4508, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n         ",
                   "                         int32_t *) &mem_5149[local_tid_4508 *\n                                                       sizeof(int32_t)] =\n                                    binop_param_y_5441;\n                                *(volatile __local\n                                  int32_t *) &mem_5152[local_tid_4508 *\n                                                       sizeof(int32_t)] =\n                                    f2_5442;\n                                *(volatile __local\n                                  int32_t *) &mem_5155[local_tid_4508 *\n                                                       sizeof(int32_t)] =\n                                    x2_5443;\n                                *(volatile __local\n                                  int32_t *) &mem_5158[local_tid_4508 *\n                                                       sizeof(int32_t)] =\n                                    f2_5444;\n                                *(volatile __local\n                                  int32_t *) &mem_5161[local_tid_4508 *\n                                                       sizeof(int32_t)] =\n                                    x2_5445;\n                            }\n                        }\n                        if (sle32(wave_sizze_5420, skip_threads_5456)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_5456 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_4508, 32) == 0 || !slt32(local_tid_4508,\n                                                                 group_sizze_4188))) {\n                    // read operands\n                    {\n                        binop_param_x_4465 = *(volatile __local\n                                               int32_t *) &mem_5149[(squot32(local_tid_4508,\n                           ",
                   "                                                  32) -\n                                                                     1) *\n                                                                    sizeof(int32_t)];\n                        f1_4466 = *(volatile __local\n                                    int32_t *) &mem_5152[(squot32(local_tid_4508,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        x1_4467 = *(volatile __local\n                                    int32_t *) &mem_5155[(squot32(local_tid_4508,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        f1_4468 = *(volatile __local\n                                    int32_t *) &mem_5158[(squot32(local_tid_4508,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        x1_4469 = *(volatile __local\n                                    int32_t *) &mem_5161[(squot32(local_tid_4508,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_4475;\n                        int32_t res_4476;\n                        char cond_4477;\n                        int32_t res_4478;\n                        int32_t res_4480;\n                        char cond_4481;\n                        int32_t res_4482;\n                        \n                        res_4475 = binop_param_x_4465 + binop_param_y_4470;\n                        res_4476 = f1_4466 | f2_4471;\n                        cond_4477 = slt32(0, f2_4471);\n                        if (cond_4477) {\n          ",
                   "                  res_4478 = x2_4472;\n                        } else {\n                            int32_t res_4479 = x1_4467 + x2_4472;\n                            \n                            res_4478 = res_4479;\n                        }\n                        res_4480 = f1_4468 | f2_4473;\n                        cond_4481 = slt32(0, f2_4473);\n                        if (cond_4481) {\n                            res_4482 = x2_4474;\n                        } else {\n                            int32_t res_4483 = x1_4469 + x2_4474;\n                            \n                            res_4482 = res_4483;\n                        }\n                        binop_param_y_4470 = res_4475;\n                        f2_4471 = res_4476;\n                        x2_4472 = res_4478;\n                        f2_4473 = res_4480;\n                        x2_4474 = res_4482;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_5149[local_tid_4508 *\n                                                                sizeof(int32_t)] =\n                            binop_param_y_4470;\n                        *(volatile __local int32_t *) &mem_5152[local_tid_4508 *\n                                                                sizeof(int32_t)] =\n                            f2_4471;\n                        *(volatile __local int32_t *) &mem_5155[local_tid_4508 *\n                                                                sizeof(int32_t)] =\n                            x2_4472;\n                        *(volatile __local int32_t *) &mem_5158[local_tid_4508 *\n                                                                sizeof(int32_t)] =\n                            f2_4473;\n                        *(volatile __local int32_t *) &mem_5161[local_tid_4508 *\n                                                                sizeof(int32_t)] =\n                            x2_4474;\n                    }\n   ",
                   "             }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_4508, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_5149[local_tid_4508 *\n                                                            sizeof(int32_t)] =\n                        binop_param_y_4470;\n                    *(volatile __local int32_t *) &mem_5152[local_tid_4508 *\n                                                            sizeof(int32_t)] =\n                        f2_4471;\n                    *(volatile __local int32_t *) &mem_5155[local_tid_4508 *\n                                                            sizeof(int32_t)] =\n                        x2_4472;\n                    *(volatile __local int32_t *) &mem_5158[local_tid_4508 *\n                                                            sizeof(int32_t)] =\n                        f2_4473;\n                    *(volatile __local int32_t *) &mem_5161[local_tid_4508 *\n                                                            sizeof(int32_t)] =\n                        x2_4474;\n                }\n            }\n            if (cond_4524) {\n                int32_t scanned_elem_4546;\n                int32_t scanned_elem_4547;\n                int32_t scanned_elem_4548;\n                int32_t scanned_elem_4549;\n                int32_t scanned_elem_4550;\n                \n                scanned_elem_4546 = *(__local\n                                      int32_t *) &mem_5149[local_tid_4508 * 4];\n                scanned_elem_4547 = *(__local\n                                      int32_t *) &mem_5152[local_tid_4508 * 4];\n                scanned_elem_4548 = *(__local\n                                      int32_t *) &mem_5155[local_tid_4508 * 4];\n                scanned_elem_4549 = *(__local\n                                      int32_t *) &mem_5158[local_tid_4508 * 4];\n                scanned_elem_4550 = *(__local\n      ",
                   "                                int32_t *) &mem_5161[local_tid_4508 * 4];\n                *(__global int32_t *) &mem_5132[j_4523 * 4] = scanned_elem_4546;\n                *(__global int32_t *) &mem_5135[j_4523 * 4] = scanned_elem_4547;\n                *(__global int32_t *) &mem_5138[j_4523 * 4] = scanned_elem_4548;\n                *(__global int32_t *) &mem_5141[j_4523 * 4] = scanned_elem_4549;\n                *(__global int32_t *) &mem_5144[j_4523 * 4] = scanned_elem_4550;\n            }\n            \n            int32_t new_carry_4567;\n            int32_t new_carry_4568;\n            int32_t new_carry_4569;\n            int32_t new_carry_4570;\n            int32_t new_carry_4571;\n            \n            if (is_first_thread_4561) {\n                int32_t carry_4562;\n                int32_t carry_4563;\n                int32_t carry_4564;\n                int32_t carry_4565;\n                int32_t carry_4566;\n                \n                carry_4562 = *(__local int32_t *) &mem_5149[y_4201 * 4];\n                carry_4563 = *(__local int32_t *) &mem_5152[y_4201 * 4];\n                carry_4564 = *(__local int32_t *) &mem_5155[y_4201 * 4];\n                carry_4565 = *(__local int32_t *) &mem_5158[y_4201 * 4];\n                carry_4566 = *(__local int32_t *) &mem_5161[y_4201 * 4];\n                new_carry_4567 = carry_4562;\n                new_carry_4568 = carry_4563;\n                new_carry_4569 = carry_4564;\n                new_carry_4570 = carry_4565;\n                new_carry_4571 = carry_4566;\n            } else {\n                new_carry_4567 = 0;\n                new_carry_4568 = 0;\n                new_carry_4569 = 0;\n                new_carry_4570 = 0;\n                new_carry_4571 = 0;\n            }\n            \n            int32_t binop_param_x_merge_tmp_5429 = new_carry_4567;\n            int32_t f1_merge_tmp_5430 = new_carry_4568;\n            int32_t x1_merge_tmp_5431 = new_carry_4569;\n            int32_t f1_merge_tmp_5432 = new_carry_4570;\n           ",
                   " int32_t x1_merge_tmp_5433;\n            \n            x1_merge_tmp_5433 = new_carry_4571;\n            binop_param_x_merge_4513 = binop_param_x_merge_tmp_5429;\n            f1_merge_4514 = f1_merge_tmp_5430;\n            x1_merge_4515 = x1_merge_tmp_5431;\n            f1_merge_4516 = f1_merge_tmp_5432;\n            x1_merge_4517 = x1_merge_tmp_5433;\n        }\n        result_4578 = binop_param_x_merge_4513;\n        result_4579 = f1_merge_4514;\n        result_4580 = x1_merge_4515;\n        result_4581 = f1_merge_4516;\n        result_4582 = x1_merge_4517;\n    }\n    if (local_tid_4508 == 0) {\n        *(__global int32_t *) &mem_5164[group_id_4509 * 4] = result_4578;\n    }\n    if (local_tid_4508 == 0) {\n        *(__global int32_t *) &mem_5167[group_id_4509 * 4] = result_4579;\n    }\n    if (local_tid_4508 == 0) {\n        *(__global int32_t *) &mem_5170[group_id_4509 * 4] = result_4580;\n    }\n    if (local_tid_4508 == 0) {\n        *(__global int32_t *) &mem_5173[group_id_4509 * 4] = result_4581;\n    }\n    if (local_tid_4508 == 0) {\n        *(__global int32_t *) &mem_5176[group_id_4509 * 4] = result_4582;\n    }\n}\n__kernel void scan1_kernel_4724(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t sizze_3871, int32_t num_iterations_4231,\n                                int32_t y_4234, __global\n                                unsigned char *sizzes_mem_5073, __global\n                                unsigned char *mem_5146, __global\n                                unsigned char *mem_5209, __global\n                                unsigned char *mem_5221, __global\n                                unsigned char *mem_5224, __global\n                                unsigned char *mem_5227, __global\n                                unsigned char *mem_5230, __global\n                                unsigned char *mem_5239, __global\n                                unsigned char *mem_5242)\n{\n    __l",
                   "ocal volatile char *restrict mem_5233 = mem_aligned_0;\n    __local volatile char *restrict mem_5236 = mem_aligned_1;\n    int32_t wave_sizze_5486;\n    int32_t group_sizze_5487;\n    char thread_active_5488;\n    int32_t global_tid_4724;\n    int32_t local_tid_4725;\n    int32_t group_id_4726;\n    \n    global_tid_4724 = get_global_id(0);\n    local_tid_4725 = get_local_id(0);\n    group_sizze_5487 = get_local_size(0);\n    wave_sizze_5486 = LOCKSTEP_WIDTH;\n    group_id_4726 = get_group_id(0);\n    thread_active_5488 = 1;\n    \n    int32_t x_4734;\n    char is_first_thread_4757;\n    int32_t result_4765;\n    int32_t result_4766;\n    \n    if (thread_active_5488) {\n        x_4734 = group_id_4726 * y_4234;\n        is_first_thread_4757 = local_tid_4725 == 0;\n        \n        int32_t f1_merge_4730;\n        int32_t x1_merge_4731;\n        \n        f1_merge_4730 = 0;\n        x1_merge_4731 = 0;\n        for (int32_t i_4732 = 0; i_4732 < num_iterations_4231; i_4732++) {\n            int32_t y_4735 = i_4732 * group_sizze_4188;\n            int32_t offset_4736 = x_4734 + y_4735;\n            int32_t j_4737 = offset_4736 + local_tid_4725;\n            char cond_4738 = slt32(j_4737, sizze_3871);\n            int32_t foldres_4743;\n            int32_t foldres_4744;\n            \n            if (cond_4738) {\n                int32_t res_elem_4739;\n                char res_elem_4740;\n                int32_t sizzes_elem_4741;\n                int32_t i_4696;\n                int32_t res_4697;\n                int32_t res_4698;\n                int32_t res_4699;\n                char cond_4700;\n                int32_t res_4701;\n                \n                res_elem_4739 = *(__global int32_t *) &mem_5209[j_4737 * 4];\n                res_elem_4740 = *(__global char *) &mem_5146[j_4737];\n                sizzes_elem_4741 = *(__global\n                                     int32_t *) &sizzes_mem_5073[j_4737 * 4];\n                i_4696 = res_elem_4739 - 1;\n                res_4697 = *(__global int32_t *) &mem_5221[",
                   "i_4696 * 4];\n                if (res_elem_4740) {\n                    res_4698 = 0;\n                } else {\n                    res_4698 = 1;\n                }\n                res_4699 = f1_merge_4730 | sizzes_elem_4741;\n                cond_4700 = slt32(0, sizzes_elem_4741);\n                if (cond_4700) {\n                    res_4701 = res_4698;\n                } else {\n                    int32_t res_4702 = x1_merge_4731 + res_4698;\n                    \n                    res_4701 = res_4702;\n                }\n                *(__global int32_t *) &mem_5230[j_4737 * 4] = res_4697;\n                foldres_4743 = res_4699;\n                foldres_4744 = res_4701;\n            } else {\n                foldres_4743 = f1_merge_4730;\n                foldres_4744 = x1_merge_4731;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_4725, group_sizze_4188) && 1) {\n                *(__local int32_t *) &mem_5233[local_tid_4725 * 4] =\n                    foldres_4743;\n                *(__local int32_t *) &mem_5236[local_tid_4725 * 4] =\n                    foldres_4744;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_4703;\n            int32_t other_index_4704;\n            int32_t f1_4705;\n            int32_t x1_4706;\n            int32_t f2_4707;\n            int32_t x2_4708;\n            int32_t my_index_5494;\n            int32_t other_index_5495;\n            int32_t f1_5496;\n            int32_t x1_5497;\n            int32_t f2_5498;\n            int32_t x2_5499;\n            \n            my_index_4703 = local_tid_4725;\n            if (slt32(local_tid_4725, group_sizze_4188)) {\n                f2_4707 = *(volatile __local\n                            int32_t *) &mem_5233[local_tid_4725 *\n                                                 sizeof(int32_t)];\n                x2_4708 = *(volatile __local\n                            int32_t *) &mem_5236[local_tid_4725 *\n                                 ",
                   "                sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_5504 = 1;\n                \n                while (slt32(skip_threads_5504, 32)) {\n                    if (slt32(local_tid_4725, group_sizze_4188) &&\n                        sle32(skip_threads_5504, local_tid_4725 -\n                              squot32(local_tid_4725, 32) * 32)) {\n                        // read operands\n                        {\n                            f1_4705 = *(volatile __local\n                                        int32_t *) &mem_5233[(local_tid_4725 -\n                                                              skip_threads_5504) *\n                                                             sizeof(int32_t)];\n                            x1_4706 = *(volatile __local\n                                        int32_t *) &mem_5236[(local_tid_4725 -\n                                                              skip_threads_5504) *\n                                                             sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_4709;\n                            char cond_4710;\n                            int32_t res_4711;\n                            \n                            res_4709 = f1_4705 | f2_4707;\n                            cond_4710 = slt32(0, f2_4707);\n                            if (cond_4710) {\n                                res_4711 = x2_4708;\n                            } else {\n                                int32_t res_4712 = x1_4706 + x2_4708;\n                                \n                                res_4711 = res_4712;\n                            }\n                            f2_4707 = res_4709;\n                            x2_4708 = res_4711;\n                        }\n                    }\n                    if (sle32(wave_sizze_5486, skip_t",
                   "hreads_5504)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_4725, group_sizze_4188) &&\n                        sle32(skip_threads_5504, local_tid_4725 -\n                              squot32(local_tid_4725, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_5233[local_tid_4725 *\n                                                   sizeof(int32_t)] = f2_4707;\n                            *(volatile __local\n                              int32_t *) &mem_5236[local_tid_4725 *\n                                                   sizeof(int32_t)] = x2_4708;\n                        }\n                    }\n                    if (sle32(wave_sizze_5486, skip_threads_5504)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5504 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_4725 - squot32(local_tid_4725, 32) * 32) == 31 &&\n                    slt32(local_tid_4725, group_sizze_4188)) {\n                    *(volatile __local\n                      int32_t *) &mem_5233[squot32(local_tid_4725, 32) *\n                                           sizeof(int32_t)] = f2_4707;\n                    *(volatile __local\n                      int32_t *) &mem_5236[squot32(local_tid_4725, 32) *\n                                           sizeof(int32_t)] = x2_4708;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_4725, 32) == 0 && slt32(local_tid_4725,\n                                                              group_sizze_4188)) {\n     ",
                   "               f2_5498 = *(volatile __local\n                                int32_t *) &mem_5233[local_tid_4725 *\n                                                     sizeof(int32_t)];\n                    x2_5499 = *(volatile __local\n                                int32_t *) &mem_5236[local_tid_4725 *\n                                                     sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_5505 = 1;\n                    \n                    while (slt32(skip_threads_5505, 32)) {\n                        if ((squot32(local_tid_4725, 32) == 0 &&\n                             slt32(local_tid_4725, group_sizze_4188)) &&\n                            sle32(skip_threads_5505, local_tid_4725 -\n                                  squot32(local_tid_4725, 32) * 32)) {\n                            // read operands\n                            {\n                                f1_5496 = *(volatile __local\n                                            int32_t *) &mem_5233[(local_tid_4725 -\n                                                                  skip_threads_5505) *\n                                                                 sizeof(int32_t)];\n                                x1_5497 = *(volatile __local\n                                            int32_t *) &mem_5236[(local_tid_4725 -\n                                                                  skip_threads_5505) *\n                                                                 sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                int32_t res_5500;\n                                char cond_5501;\n                                int32_t res_5502;\n                                \n                                res_5500 = f1_5496 | f2_5498;\n                                cond_5501 = slt32(0, f2_5498",
                   ");\n                                if (cond_5501) {\n                                    res_5502 = x2_5499;\n                                } else {\n                                    int32_t res_5503 = x1_5497 + x2_5499;\n                                    \n                                    res_5502 = res_5503;\n                                }\n                                f2_5498 = res_5500;\n                                x2_5499 = res_5502;\n                            }\n                        }\n                        if (sle32(wave_sizze_5486, skip_threads_5505)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_4725, 32) == 0 &&\n                             slt32(local_tid_4725, group_sizze_4188)) &&\n                            sle32(skip_threads_5505, local_tid_4725 -\n                                  squot32(local_tid_4725, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_5233[local_tid_4725 *\n                                                       sizeof(int32_t)] =\n                                    f2_5498;\n                                *(volatile __local\n                                  int32_t *) &mem_5236[local_tid_4725 *\n                                                       sizeof(int32_t)] =\n                                    x2_5499;\n                            }\n                        }\n                        if (sle32(wave_sizze_5486, skip_threads_5505)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_5505 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_4725, 32) == 0 || !slt32(lo",
                   "cal_tid_4725,\n                                                                 group_sizze_4188))) {\n                    // read operands\n                    {\n                        f1_4705 = *(volatile __local\n                                    int32_t *) &mem_5233[(squot32(local_tid_4725,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        x1_4706 = *(volatile __local\n                                    int32_t *) &mem_5236[(squot32(local_tid_4725,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_4709;\n                        char cond_4710;\n                        int32_t res_4711;\n                        \n                        res_4709 = f1_4705 | f2_4707;\n                        cond_4710 = slt32(0, f2_4707);\n                        if (cond_4710) {\n                            res_4711 = x2_4708;\n                        } else {\n                            int32_t res_4712 = x1_4706 + x2_4708;\n                            \n                            res_4711 = res_4712;\n                        }\n                        f2_4707 = res_4709;\n                        x2_4708 = res_4711;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_5233[local_tid_4725 *\n                                                                sizeof(int32_t)] =\n                            f2_4707;\n                        *(volatile __local int32_t *) &mem_5236[local_tid_4725 *\n                                                                sizeof(int32_t)] =\n                            x2_4708;\n                    }\n                }\n            }\n ",
                   "           barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_4725, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_5233[local_tid_4725 *\n                                                            sizeof(int32_t)] =\n                        f2_4707;\n                    *(volatile __local int32_t *) &mem_5236[local_tid_4725 *\n                                                            sizeof(int32_t)] =\n                        x2_4708;\n                }\n            }\n            if (cond_4738) {\n                int32_t scanned_elem_4751;\n                int32_t scanned_elem_4752;\n                \n                scanned_elem_4751 = *(__local\n                                      int32_t *) &mem_5233[local_tid_4725 * 4];\n                scanned_elem_4752 = *(__local\n                                      int32_t *) &mem_5236[local_tid_4725 * 4];\n                *(__global int32_t *) &mem_5224[j_4737 * 4] = scanned_elem_4751;\n                *(__global int32_t *) &mem_5227[j_4737 * 4] = scanned_elem_4752;\n            }\n            \n            int32_t new_carry_4760;\n            int32_t new_carry_4761;\n            \n            if (is_first_thread_4757) {\n                int32_t carry_4758;\n                int32_t carry_4759;\n                \n                carry_4758 = *(__local int32_t *) &mem_5233[y_4201 * 4];\n                carry_4759 = *(__local int32_t *) &mem_5236[y_4201 * 4];\n                new_carry_4760 = carry_4758;\n                new_carry_4761 = carry_4759;\n            } else {\n                new_carry_4760 = 0;\n                new_carry_4761 = 0;\n            }\n            \n            int32_t f1_merge_tmp_5492 = new_carry_4760;\n            int32_t x1_merge_tmp_5493;\n            \n            x1_merge_tmp_5493 = new_carry_4761;\n            f1_merge_4730 = f1_merge_tmp_5492;\n            x1_merge_4731 = x1_merge_tmp_5493;\n        }\n        result_4765 = ",
                   "f1_merge_4730;\n        result_4766 = x1_merge_4731;\n    }\n    if (local_tid_4725 == 0) {\n        *(__global int32_t *) &mem_5239[group_id_4726 * 4] = result_4765;\n    }\n    if (local_tid_4725 == 0) {\n        *(__global int32_t *) &mem_5242[group_id_4726 * 4] = result_4766;\n    }\n}\n__kernel void scan1_kernel_4867(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t sizze_3871, int32_t num_iterations_4231,\n                                int32_t y_4234, __global\n                                unsigned char *sizzes_mem_5073, __global\n                                unsigned char *mem_5263, __global\n                                unsigned char *mem_5266, __global\n                                unsigned char *mem_5275, __global\n                                unsigned char *mem_5278)\n{\n    __local volatile char *restrict mem_5269 = mem_aligned_0;\n    __local volatile char *restrict mem_5272 = mem_aligned_1;\n    int32_t wave_sizze_5524;\n    int32_t group_sizze_5525;\n    char thread_active_5526;\n    int32_t global_tid_4867;\n    int32_t local_tid_4868;\n    int32_t group_id_4869;\n    \n    global_tid_4867 = get_global_id(0);\n    local_tid_4868 = get_local_id(0);\n    group_sizze_5525 = get_local_size(0);\n    wave_sizze_5524 = LOCKSTEP_WIDTH;\n    group_id_4869 = get_group_id(0);\n    thread_active_5526 = 1;\n    \n    int32_t x_4877;\n    char is_first_thread_4897;\n    int32_t result_4904;\n    int32_t result_4905;\n    \n    if (thread_active_5526) {\n        x_4877 = group_id_4869 * y_4234;\n        is_first_thread_4897 = local_tid_4868 == 0;\n        \n        int32_t f1_merge_4873;\n        int32_t x1_merge_4874;\n        \n        f1_merge_4873 = 0;\n        x1_merge_4874 = 0;\n        for (int32_t i_4875 = 0; i_4875 < num_iterations_4231; i_4875++) {\n            int32_t y_4878 = i_4875 * group_sizze_4188;\n            int32_t offset_4879 = x_4877 + y_4878;\n            int32_t j_4880 = o",
                   "ffset_4879 + local_tid_4868;\n            char cond_4881 = slt32(j_4880, sizze_3871);\n            int32_t foldres_4884;\n            int32_t foldres_4885;\n            \n            if (cond_4881) {\n                int32_t inp_elem_4882;\n                int32_t res_4844;\n                char cond_4845;\n                int32_t res_4846;\n                \n                inp_elem_4882 = *(__global int32_t *) &sizzes_mem_5073[j_4880 *\n                                                                       4];\n                res_4844 = f1_merge_4873 | inp_elem_4882;\n                cond_4845 = slt32(0, inp_elem_4882);\n                if (cond_4845) {\n                    res_4846 = 1;\n                } else {\n                    int32_t res_4847 = x1_merge_4874 + 1;\n                    \n                    res_4846 = res_4847;\n                }\n                foldres_4884 = res_4844;\n                foldres_4885 = res_4846;\n            } else {\n                foldres_4884 = f1_merge_4873;\n                foldres_4885 = x1_merge_4874;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_4868, group_sizze_4188) && 1) {\n                *(__local int32_t *) &mem_5269[local_tid_4868 * 4] =\n                    foldres_4884;\n                *(__local int32_t *) &mem_5272[local_tid_4868 * 4] =\n                    foldres_4885;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_4848;\n            int32_t other_index_4849;\n            int32_t f1_4850;\n            int32_t x1_4851;\n            int32_t f2_4852;\n            int32_t x2_4853;\n            int32_t my_index_5531;\n            int32_t other_index_5532;\n            int32_t f1_5533;\n            int32_t x1_5534;\n            int32_t f2_5535;\n            int32_t x2_5536;\n            \n            my_index_4848 = local_tid_4868;\n            if (slt32(local_tid_4868, group_sizze_4188)) {\n                f2_4852 = *(volatile __local\n                        ",
                   "    int32_t *) &mem_5269[local_tid_4868 *\n                                                 sizeof(int32_t)];\n                x2_4853 = *(volatile __local\n                            int32_t *) &mem_5272[local_tid_4868 *\n                                                 sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_5541 = 1;\n                \n                while (slt32(skip_threads_5541, 32)) {\n                    if (slt32(local_tid_4868, group_sizze_4188) &&\n                        sle32(skip_threads_5541, local_tid_4868 -\n                              squot32(local_tid_4868, 32) * 32)) {\n                        // read operands\n                        {\n                            f1_4850 = *(volatile __local\n                                        int32_t *) &mem_5269[(local_tid_4868 -\n                                                              skip_threads_5541) *\n                                                             sizeof(int32_t)];\n                            x1_4851 = *(volatile __local\n                                        int32_t *) &mem_5272[(local_tid_4868 -\n                                                              skip_threads_5541) *\n                                                             sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_4854;\n                            char cond_4855;\n                            int32_t res_4856;\n                            \n                            res_4854 = f1_4850 | f2_4852;\n                            cond_4855 = slt32(0, f2_4852);\n                            if (cond_4855) {\n                                res_4856 = x2_4853;\n                            } else {\n                                int32_t res_4857 = x1_4851 + x2_4853;\n                                \n                           ",
                   "     res_4856 = res_4857;\n                            }\n                            f2_4852 = res_4854;\n                            x2_4853 = res_4856;\n                        }\n                    }\n                    if (sle32(wave_sizze_5524, skip_threads_5541)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_4868, group_sizze_4188) &&\n                        sle32(skip_threads_5541, local_tid_4868 -\n                              squot32(local_tid_4868, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_5269[local_tid_4868 *\n                                                   sizeof(int32_t)] = f2_4852;\n                            *(volatile __local\n                              int32_t *) &mem_5272[local_tid_4868 *\n                                                   sizeof(int32_t)] = x2_4853;\n                        }\n                    }\n                    if (sle32(wave_sizze_5524, skip_threads_5541)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5541 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_4868 - squot32(local_tid_4868, 32) * 32) == 31 &&\n                    slt32(local_tid_4868, group_sizze_4188)) {\n                    *(volatile __local\n                      int32_t *) &mem_5269[squot32(local_tid_4868, 32) *\n                                           sizeof(int32_t)] = f2_4852;\n                    *(volatile __local\n                      int32_t *) &mem_5272[squot32(local_tid_4868, 32) *\n                                           sizeof(int32_t)] = x2_4853;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan ",
                   "the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_4868, 32) == 0 && slt32(local_tid_4868,\n                                                              group_sizze_4188)) {\n                    f2_5535 = *(volatile __local\n                                int32_t *) &mem_5269[local_tid_4868 *\n                                                     sizeof(int32_t)];\n                    x2_5536 = *(volatile __local\n                                int32_t *) &mem_5272[local_tid_4868 *\n                                                     sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_5542 = 1;\n                    \n                    while (slt32(skip_threads_5542, 32)) {\n                        if ((squot32(local_tid_4868, 32) == 0 &&\n                             slt32(local_tid_4868, group_sizze_4188)) &&\n                            sle32(skip_threads_5542, local_tid_4868 -\n                                  squot32(local_tid_4868, 32) * 32)) {\n                            // read operands\n                            {\n                                f1_5533 = *(volatile __local\n                                            int32_t *) &mem_5269[(local_tid_4868 -\n                                                                  skip_threads_5542) *\n                                                                 sizeof(int32_t)];\n                                x1_5534 = *(volatile __local\n                                            int32_t *) &mem_5272[(local_tid_4868 -\n                                                                  skip_threads_5542) *\n                                                                 sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                int32_t res_5537;\n",
                   "                                char cond_5538;\n                                int32_t res_5539;\n                                \n                                res_5537 = f1_5533 | f2_5535;\n                                cond_5538 = slt32(0, f2_5535);\n                                if (cond_5538) {\n                                    res_5539 = x2_5536;\n                                } else {\n                                    int32_t res_5540 = x1_5534 + x2_5536;\n                                    \n                                    res_5539 = res_5540;\n                                }\n                                f2_5535 = res_5537;\n                                x2_5536 = res_5539;\n                            }\n                        }\n                        if (sle32(wave_sizze_5524, skip_threads_5542)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_4868, 32) == 0 &&\n                             slt32(local_tid_4868, group_sizze_4188)) &&\n                            sle32(skip_threads_5542, local_tid_4868 -\n                                  squot32(local_tid_4868, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_5269[local_tid_4868 *\n                                                       sizeof(int32_t)] =\n                                    f2_5535;\n                                *(volatile __local\n                                  int32_t *) &mem_5272[local_tid_4868 *\n                                                       sizeof(int32_t)] =\n                                    x2_5536;\n                            }\n                        }\n                        if (sle32(wave_sizze_5524, skip_threads_5542)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_",
                   "threads_5542 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_4868, 32) == 0 || !slt32(local_tid_4868,\n                                                                 group_sizze_4188))) {\n                    // read operands\n                    {\n                        f1_4850 = *(volatile __local\n                                    int32_t *) &mem_5269[(squot32(local_tid_4868,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                        x1_4851 = *(volatile __local\n                                    int32_t *) &mem_5272[(squot32(local_tid_4868,\n                                                                  32) - 1) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_4854;\n                        char cond_4855;\n                        int32_t res_4856;\n                        \n                        res_4854 = f1_4850 | f2_4852;\n                        cond_4855 = slt32(0, f2_4852);\n                        if (cond_4855) {\n                            res_4856 = x2_4853;\n                        } else {\n                            int32_t res_4857 = x1_4851 + x2_4853;\n                            \n                            res_4856 = res_4857;\n                        }\n                        f2_4852 = res_4854;\n                        x2_4853 = res_4856;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_5269[local_tid_4868 *\n                                                                sizeof(int32_t)] =\n                            f2_4852;\n   ",
                   "                     *(volatile __local int32_t *) &mem_5272[local_tid_4868 *\n                                                                sizeof(int32_t)] =\n                            x2_4853;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_4868, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_5269[local_tid_4868 *\n                                                            sizeof(int32_t)] =\n                        f2_4852;\n                    *(volatile __local int32_t *) &mem_5272[local_tid_4868 *\n                                                            sizeof(int32_t)] =\n                        x2_4853;\n                }\n            }\n            if (cond_4881) {\n                int32_t scanned_elem_4891;\n                int32_t scanned_elem_4892;\n                \n                scanned_elem_4891 = *(__local\n                                      int32_t *) &mem_5269[local_tid_4868 * 4];\n                scanned_elem_4892 = *(__local\n                                      int32_t *) &mem_5272[local_tid_4868 * 4];\n                *(__global int32_t *) &mem_5263[j_4880 * 4] = scanned_elem_4891;\n                *(__global int32_t *) &mem_5266[j_4880 * 4] = scanned_elem_4892;\n            }\n            \n            int32_t new_carry_4900;\n            int32_t new_carry_4901;\n            \n            if (is_first_thread_4897) {\n                int32_t carry_4898;\n                int32_t carry_4899;\n                \n                carry_4898 = *(__local int32_t *) &mem_5269[y_4201 * 4];\n                carry_4899 = *(__local int32_t *) &mem_5272[y_4201 * 4];\n                new_carry_4900 = carry_4898;\n                new_carry_4901 = carry_4899;\n            } else {\n                new_carry_4900 = 0;\n                new_carry_4901 = 0;\n            }\n            \n            int32_t f1_merge_",
                   "tmp_5529 = new_carry_4900;\n            int32_t x1_merge_tmp_5530;\n            \n            x1_merge_tmp_5530 = new_carry_4901;\n            f1_merge_4873 = f1_merge_tmp_5529;\n            x1_merge_4874 = x1_merge_tmp_5530;\n        }\n        result_4904 = f1_merge_4873;\n        result_4905 = x1_merge_4874;\n    }\n    if (local_tid_4868 == 0) {\n        *(__global int32_t *) &mem_5275[group_id_4869 * 4] = result_4904;\n    }\n    if (local_tid_4868 == 0) {\n        *(__global int32_t *) &mem_5278[group_id_4869 * 4] = result_4905;\n    }\n}\n__kernel void scan2_kernel_4258(__local volatile int64_t *mem_aligned_0,\n                                int32_t num_groups_4205, __global\n                                unsigned char *mem_5084, __global\n                                unsigned char *mem_5090)\n{\n    __local volatile char *restrict mem_5087 = mem_aligned_0;\n    int32_t wave_sizze_5370;\n    int32_t group_sizze_5371;\n    char thread_active_5372;\n    int32_t global_tid_4258;\n    int32_t local_tid_4259;\n    int32_t group_id_4260;\n    \n    global_tid_4258 = get_global_id(0);\n    local_tid_4259 = get_local_id(0);\n    group_sizze_5371 = get_local_size(0);\n    wave_sizze_5370 = LOCKSTEP_WIDTH;\n    group_id_4260 = get_group_id(0);\n    thread_active_5372 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_4259, num_groups_4205) && 1) {\n        int32_t res_group_sums_elem_4261 = *(__global\n                                             int32_t *) &mem_5084[local_tid_4259 *\n                                                                  4];\n        \n        *(__local int32_t *) &mem_5087[local_tid_4259 * 4] =\n            res_group_sums_elem_4261;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_4253;\n    int32_t other_index_4254;\n    int32_t binop_param_x_4255;\n    int32_t binop_param_y_4256;\n    int32_t my_index_5373;\n    int32_t other_index_5374;\n    int32_t binop_param_x_5375;\n    int32_t binop_param_y_5376;\n    \n    my_index_4253 = local_tid_4259;\n    if ",
                   "(slt32(local_tid_4259, num_groups_4205)) {\n        binop_param_y_4256 = *(volatile __local\n                               int32_t *) &mem_5087[local_tid_4259 *\n                                                    sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_5378 = 1;\n        \n        while (slt32(skip_threads_5378, 32)) {\n            if (slt32(local_tid_4259, num_groups_4205) &&\n                sle32(skip_threads_5378, local_tid_4259 -\n                      squot32(local_tid_4259, 32) * 32)) {\n                // read operands\n                {\n                    binop_param_x_4255 = *(volatile __local\n                                           int32_t *) &mem_5087[(local_tid_4259 -\n                                                                 skip_threads_5378) *\n                                                                sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    int32_t res_4257;\n                    \n                    if (thread_active_5372) {\n                        res_4257 = binop_param_x_4255 + binop_param_y_4256;\n                    }\n                    binop_param_y_4256 = res_4257;\n                }\n            }\n            if (sle32(wave_sizze_5370, skip_threads_5378)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (slt32(local_tid_4259, num_groups_4205) &&\n                sle32(skip_threads_5378, local_tid_4259 -\n                      squot32(local_tid_4259, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_5087[local_tid_4259 *\n                                                            sizeof(int32_t)] =\n                        binop_param_y_4256;\n                }\n            }\n            if (sle32(wave_sizze_5370, skip_threads_5378)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_",
                   "threads_5378 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_4259 - squot32(local_tid_4259, 32) * 32) == 31 &&\n            slt32(local_tid_4259, num_groups_4205)) {\n            *(volatile __local int32_t *) &mem_5087[squot32(local_tid_4259,\n                                                            32) *\n                                                    sizeof(int32_t)] =\n                binop_param_y_4256;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_4259, 32) == 0 && slt32(local_tid_4259,\n                                                      num_groups_4205)) {\n            binop_param_y_5376 = *(volatile __local\n                                   int32_t *) &mem_5087[local_tid_4259 *\n                                                        sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_5379 = 1;\n            \n            while (slt32(skip_threads_5379, 32)) {\n                if ((squot32(local_tid_4259, 32) == 0 && slt32(local_tid_4259,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5379, local_tid_4259 -\n                          squot32(local_tid_4259, 32) * 32)) {\n                    // read operands\n                    {\n                        binop_param_x_5375 = *(volatile __local\n                                               int32_t *) &mem_5087[(local_tid_4259 -\n                                                                     skip_threads_5379) *\n                                                                    sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5377;\n                  ",
                   "      \n                        if (thread_active_5372) {\n                            res_5377 = binop_param_x_5375 + binop_param_y_5376;\n                        }\n                        binop_param_y_5376 = res_5377;\n                    }\n                }\n                if (sle32(wave_sizze_5370, skip_threads_5379)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_4259, 32) == 0 && slt32(local_tid_4259,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5379, local_tid_4259 -\n                          squot32(local_tid_4259, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_5087[local_tid_4259 *\n                                                                sizeof(int32_t)] =\n                            binop_param_y_5376;\n                    }\n                }\n                if (sle32(wave_sizze_5370, skip_threads_5379)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5379 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_4259, 32) == 0 || !slt32(local_tid_4259,\n                                                         num_groups_4205))) {\n            // read operands\n            {\n                binop_param_x_4255 = *(volatile __local\n                                       int32_t *) &mem_5087[(squot32(local_tid_4259,\n                                                                     32) - 1) *\n                                                            sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                int32_t res_4257;\n                \n                if (thread_active_5372) {\n                    res_4257 = binop_param_x_4255 + binop_param_y_4256;\n",
                   "                }\n                binop_param_y_4256 = res_4257;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_5087[local_tid_4259 *\n                                                        sizeof(int32_t)] =\n                    binop_param_y_4256;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_4259, 32) == 0) {\n            *(volatile __local int32_t *) &mem_5087[local_tid_4259 *\n                                                    sizeof(int32_t)] =\n                binop_param_y_4256;\n        }\n    }\n    \n    int32_t scanned_elem_4264;\n    \n    if (thread_active_5372) {\n        scanned_elem_4264 = *(__local int32_t *) &mem_5087[local_tid_4259 * 4];\n    }\n    *(__global int32_t *) &mem_5090[global_tid_4258 * 4] = scanned_elem_4264;\n}\n__kernel void scan2_kernel_4390(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t num_groups_4205, __global\n                                unsigned char *mem_5108, __global\n                                unsigned char *mem_5111, __global\n                                unsigned char *mem_5120, __global\n                                unsigned char *mem_5123)\n{\n    __local volatile char *restrict mem_5114 = mem_aligned_0;\n    __local volatile char *restrict mem_5117 = mem_aligned_1;\n    int32_t wave_sizze_5402;\n    int32_t group_sizze_5403;\n    char thread_active_5404;\n    int32_t global_tid_4390;\n    int32_t local_tid_4391;\n    int32_t group_id_4392;\n    \n    global_tid_4390 = get_global_id(0);\n    local_tid_4391 = get_local_id(0);\n    group_sizze_5403 = get_local_size(0);\n    wave_sizze_5402 = LOCKSTEP_WIDTH;\n    group_id_4392 = get_group_id(0);\n    thread_active_5404 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_4391, num_groups_4205) && 1) {\n        in",
                   "t32_t res_group_sums_elem_4393;\n        float res_group_sums_elem_4394;\n        \n        res_group_sums_elem_4393 = *(__global\n                                     int32_t *) &mem_5108[local_tid_4391 * 4];\n        res_group_sums_elem_4394 = *(__global\n                                     float *) &mem_5111[local_tid_4391 * 4];\n        *(__local int32_t *) &mem_5114[local_tid_4391 * 4] =\n            res_group_sums_elem_4393;\n        *(__local float *) &mem_5117[local_tid_4391 * 4] =\n            res_group_sums_elem_4394;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_4380;\n    int32_t other_index_4381;\n    int32_t f1_4382;\n    float x1_4383;\n    int32_t f2_4384;\n    float x2_4385;\n    int32_t my_index_5405;\n    int32_t other_index_5406;\n    int32_t f1_5407;\n    float x1_5408;\n    int32_t f2_5409;\n    float x2_5410;\n    \n    my_index_4380 = local_tid_4391;\n    if (slt32(local_tid_4391, num_groups_4205)) {\n        f2_4384 = *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                          sizeof(int32_t)];\n        x2_4385 = *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                        sizeof(float)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_5415 = 1;\n        \n        while (slt32(skip_threads_5415, 32)) {\n            if (slt32(local_tid_4391, num_groups_4205) &&\n                sle32(skip_threads_5415, local_tid_4391 -\n                      squot32(local_tid_4391, 32) * 32)) {\n                // read operands\n                {\n                    f1_4382 = *(volatile __local\n                                int32_t *) &mem_5114[(local_tid_4391 -\n                                                      skip_threads_5415) *\n                                                     sizeof(int32_t)];\n                    x1_4383 = *(volatile __local\n                                float *) &mem_5117[(local_tid_4391 -\n",
                   "                                                    skip_threads_5415) *\n                                                   sizeof(float)];\n                }\n                // perform operation\n                {\n                    int32_t res_4386;\n                    char cond_4387;\n                    float res_4388;\n                    \n                    if (thread_active_5404) {\n                        res_4386 = f1_4382 | f2_4384;\n                        cond_4387 = slt32(0, f2_4384);\n                        if (cond_4387) {\n                            res_4388 = x2_4385;\n                        } else {\n                            float res_4389 = x1_4383 + x2_4385;\n                            \n                            res_4388 = res_4389;\n                        }\n                    }\n                    f2_4384 = res_4386;\n                    x2_4385 = res_4388;\n                }\n            }\n            if (sle32(wave_sizze_5402, skip_threads_5415)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (slt32(local_tid_4391, num_groups_4205) &&\n                sle32(skip_threads_5415, local_tid_4391 -\n                      squot32(local_tid_4391, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                            sizeof(int32_t)] =\n                        f2_4384;\n                    *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                          sizeof(float)] =\n                        x2_4385;\n                }\n            }\n            if (sle32(wave_sizze_5402, skip_threads_5415)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5415 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_4391 - squot32(local_tid_43",
                   "91, 32) * 32) == 31 &&\n            slt32(local_tid_4391, num_groups_4205)) {\n            *(volatile __local int32_t *) &mem_5114[squot32(local_tid_4391,\n                                                            32) *\n                                                    sizeof(int32_t)] = f2_4384;\n            *(volatile __local float *) &mem_5117[squot32(local_tid_4391, 32) *\n                                                  sizeof(float)] = x2_4385;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_4391, 32) == 0 && slt32(local_tid_4391,\n                                                      num_groups_4205)) {\n            f2_5409 = *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                              sizeof(int32_t)];\n            x2_5410 = *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                            sizeof(float)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_5416 = 1;\n            \n            while (slt32(skip_threads_5416, 32)) {\n                if ((squot32(local_tid_4391, 32) == 0 && slt32(local_tid_4391,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5416, local_tid_4391 -\n                          squot32(local_tid_4391, 32) * 32)) {\n                    // read operands\n                    {\n                        f1_5407 = *(volatile __local\n                                    int32_t *) &mem_5114[(local_tid_4391 -\n                                                          skip_threads_5416) *\n                                                         sizeof(int32_t)];\n                        x1_5408 = *(volatile __local\n                                    float *) &mem_5117[(local_tid_4391 -\n          ",
                   "                                              skip_threads_5416) *\n                                                       sizeof(float)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5411;\n                        char cond_5412;\n                        float res_5413;\n                        \n                        if (thread_active_5404) {\n                            res_5411 = f1_5407 | f2_5409;\n                            cond_5412 = slt32(0, f2_5409);\n                            if (cond_5412) {\n                                res_5413 = x2_5410;\n                            } else {\n                                float res_5414 = x1_5408 + x2_5410;\n                                \n                                res_5413 = res_5414;\n                            }\n                        }\n                        f2_5409 = res_5411;\n                        x2_5410 = res_5413;\n                    }\n                }\n                if (sle32(wave_sizze_5402, skip_threads_5416)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_4391, 32) == 0 && slt32(local_tid_4391,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5416, local_tid_4391 -\n                          squot32(local_tid_4391, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                                sizeof(int32_t)] =\n                            f2_5409;\n                        *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                              sizeof(float)] =\n                            x2_5410;\n                    }\n                }\n                if (sle32(wave_sizze_5402, skip_threads_5416)) {\n           ",
                   "         barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5416 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_4391, 32) == 0 || !slt32(local_tid_4391,\n                                                         num_groups_4205))) {\n            // read operands\n            {\n                f1_4382 = *(volatile __local\n                            int32_t *) &mem_5114[(squot32(local_tid_4391, 32) -\n                                                  1) * sizeof(int32_t)];\n                x1_4383 = *(volatile __local\n                            float *) &mem_5117[(squot32(local_tid_4391, 32) -\n                                                1) * sizeof(float)];\n            }\n            // perform operation\n            {\n                int32_t res_4386;\n                char cond_4387;\n                float res_4388;\n                \n                if (thread_active_5404) {\n                    res_4386 = f1_4382 | f2_4384;\n                    cond_4387 = slt32(0, f2_4384);\n                    if (cond_4387) {\n                        res_4388 = x2_4385;\n                    } else {\n                        float res_4389 = x1_4383 + x2_4385;\n                        \n                        res_4388 = res_4389;\n                    }\n                }\n                f2_4384 = res_4386;\n                x2_4385 = res_4388;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                        sizeof(int32_t)] =\n                    f2_4384;\n                *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                      sizeof(float)] = x2_4385;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(loc",
                   "al_tid_4391, 32) == 0) {\n            *(volatile __local int32_t *) &mem_5114[local_tid_4391 *\n                                                    sizeof(int32_t)] = f2_4384;\n            *(volatile __local float *) &mem_5117[local_tid_4391 *\n                                                  sizeof(float)] = x2_4385;\n        }\n    }\n    \n    int32_t scanned_elem_4399;\n    float scanned_elem_4400;\n    \n    if (thread_active_5404) {\n        scanned_elem_4399 = *(__local int32_t *) &mem_5114[local_tid_4391 * 4];\n        scanned_elem_4400 = *(__local float *) &mem_5117[local_tid_4391 * 4];\n    }\n    *(__global int32_t *) &mem_5120[global_tid_4390 * 4] = scanned_elem_4399;\n    *(__global float *) &mem_5123[global_tid_4390 * 4] = scanned_elem_4400;\n}\n__kernel void scan2_kernel_4604(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                __local volatile int64_t *mem_aligned_2,\n                                __local volatile int64_t *mem_aligned_3,\n                                __local volatile int64_t *mem_aligned_4,\n                                int32_t num_groups_4205, __global\n                                unsigned char *mem_5164, __global\n                                unsigned char *mem_5167, __global\n                                unsigned char *mem_5170, __global\n                                unsigned char *mem_5173, __global\n                                unsigned char *mem_5176, __global\n                                unsigned char *mem_5194, __global\n                                unsigned char *mem_5197, __global\n                                unsigned char *mem_5200, __global\n                                unsigned char *mem_5203, __global\n                                unsigned char *mem_5206)\n{\n    __local volatile char *restrict mem_5179 = mem_aligned_0;\n    __local volatile char *restrict mem_5182 = mem_aligned_1;\n    __local volatile char *restrict mem",
                   "_5185 = mem_aligned_2;\n    __local volatile char *restrict mem_5188 = mem_aligned_3;\n    __local volatile char *restrict mem_5191 = mem_aligned_4;\n    int32_t wave_sizze_5457;\n    int32_t group_sizze_5458;\n    char thread_active_5459;\n    int32_t global_tid_4604;\n    int32_t local_tid_4605;\n    int32_t group_id_4606;\n    \n    global_tid_4604 = get_global_id(0);\n    local_tid_4605 = get_local_id(0);\n    group_sizze_5458 = get_local_size(0);\n    wave_sizze_5457 = LOCKSTEP_WIDTH;\n    group_id_4606 = get_group_id(0);\n    thread_active_5459 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_4605, num_groups_4205) && 1) {\n        int32_t res_group_sums_elem_4607;\n        int32_t res_group_sums_elem_4608;\n        int32_t res_group_sums_elem_4609;\n        int32_t res_group_sums_elem_4610;\n        int32_t res_group_sums_elem_4611;\n        \n        res_group_sums_elem_4607 = *(__global\n                                     int32_t *) &mem_5164[local_tid_4605 * 4];\n        res_group_sums_elem_4608 = *(__global\n                                     int32_t *) &mem_5167[local_tid_4605 * 4];\n        res_group_sums_elem_4609 = *(__global\n                                     int32_t *) &mem_5170[local_tid_4605 * 4];\n        res_group_sums_elem_4610 = *(__global\n                                     int32_t *) &mem_5173[local_tid_4605 * 4];\n        res_group_sums_elem_4611 = *(__global\n                                     int32_t *) &mem_5176[local_tid_4605 * 4];\n        *(__local int32_t *) &mem_5179[local_tid_4605 * 4] =\n            res_group_sums_elem_4607;\n        *(__local int32_t *) &mem_5182[local_tid_4605 * 4] =\n            res_group_sums_elem_4608;\n        *(__local int32_t *) &mem_5185[local_tid_4605 * 4] =\n            res_group_sums_elem_4609;\n        *(__local int32_t *) &mem_5188[local_tid_4605 * 4] =\n            res_group_sums_elem_4610;\n        *(__local int32_t *) &mem_5191[local_tid_4605 * 4] =\n            res_group_sums_elem_4611;\n    }\n    barrier(CLK_LOC",
                   "AL_MEM_FENCE);\n    \n    int32_t my_index_4583;\n    int32_t other_index_4584;\n    int32_t binop_param_x_4585;\n    int32_t f1_4586;\n    int32_t x1_4587;\n    int32_t f1_4588;\n    int32_t x1_4589;\n    int32_t binop_param_y_4590;\n    int32_t f2_4591;\n    int32_t x2_4592;\n    int32_t f2_4593;\n    int32_t x2_4594;\n    int32_t my_index_5460;\n    int32_t other_index_5461;\n    int32_t binop_param_x_5462;\n    int32_t f1_5463;\n    int32_t x1_5464;\n    int32_t f1_5465;\n    int32_t x1_5466;\n    int32_t binop_param_y_5467;\n    int32_t f2_5468;\n    int32_t x2_5469;\n    int32_t f2_5470;\n    int32_t x2_5471;\n    \n    my_index_4583 = local_tid_4605;\n    if (slt32(local_tid_4605, num_groups_4205)) {\n        binop_param_y_4590 = *(volatile __local\n                               int32_t *) &mem_5179[local_tid_4605 *\n                                                    sizeof(int32_t)];\n        f2_4591 = *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                          sizeof(int32_t)];\n        x2_4592 = *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                          sizeof(int32_t)];\n        f2_4593 = *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                          sizeof(int32_t)];\n        x2_4594 = *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                                                          sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_5481 = 1;\n        \n        while (slt32(skip_threads_5481, 32)) {\n            if (slt32(local_tid_4605, num_groups_4205) &&\n                sle32(skip_threads_5481, local_tid_4605 -\n                      squot32(local_tid_4605, 32) * 32)) {\n                // read operands\n                {\n                    binop_param_x_4585 = *(volatile __local\n                                           int32_t *) &mem_5179[(local_tid_",
                   "4605 -\n                                                                 skip_threads_5481) *\n                                                                sizeof(int32_t)];\n                    f1_4586 = *(volatile __local\n                                int32_t *) &mem_5182[(local_tid_4605 -\n                                                      skip_threads_5481) *\n                                                     sizeof(int32_t)];\n                    x1_4587 = *(volatile __local\n                                int32_t *) &mem_5185[(local_tid_4605 -\n                                                      skip_threads_5481) *\n                                                     sizeof(int32_t)];\n                    f1_4588 = *(volatile __local\n                                int32_t *) &mem_5188[(local_tid_4605 -\n                                                      skip_threads_5481) *\n                                                     sizeof(int32_t)];\n                    x1_4589 = *(volatile __local\n                                int32_t *) &mem_5191[(local_tid_4605 -\n                                                      skip_threads_5481) *\n                                                     sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    int32_t res_4595;\n                    int32_t res_4596;\n                    char cond_4597;\n                    int32_t res_4598;\n                    int32_t res_4600;\n                    char cond_4601;\n                    int32_t res_4602;\n                    \n                    if (thread_active_5459) {\n                        res_4595 = binop_param_x_4585 + binop_param_y_4590;\n                        res_4596 = f1_4586 | f2_4591;\n                        cond_4597 = slt32(0, f2_4591);\n                        if (cond_4597) {\n                            res_4598 = x2_4592;\n                        } else {\n                            int32_t res_4599 = x1_45",
                   "87 + x2_4592;\n                            \n                            res_4598 = res_4599;\n                        }\n                        res_4600 = f1_4588 | f2_4593;\n                        cond_4601 = slt32(0, f2_4593);\n                        if (cond_4601) {\n                            res_4602 = x2_4594;\n                        } else {\n                            int32_t res_4603 = x1_4589 + x2_4594;\n                            \n                            res_4602 = res_4603;\n                        }\n                    }\n                    binop_param_y_4590 = res_4595;\n                    f2_4591 = res_4596;\n                    x2_4592 = res_4598;\n                    f2_4593 = res_4600;\n                    x2_4594 = res_4602;\n                }\n            }\n            if (sle32(wave_sizze_5457, skip_threads_5481)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (slt32(local_tid_4605, num_groups_4205) &&\n                sle32(skip_threads_5481, local_tid_4605 -\n                      squot32(local_tid_4605, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_5179[local_tid_4605 *\n                                                            sizeof(int32_t)] =\n                        binop_param_y_4590;\n                    *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                            sizeof(int32_t)] =\n                        f2_4591;\n                    *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                            sizeof(int32_t)] =\n                        x2_4592;\n                    *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                            sizeof(int32_t)] =\n                        f2_4593;\n                    *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                          ",
                   "                                  sizeof(int32_t)] =\n                        x2_4594;\n                }\n            }\n            if (sle32(wave_sizze_5457, skip_threads_5481)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5481 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_4605 - squot32(local_tid_4605, 32) * 32) == 31 &&\n            slt32(local_tid_4605, num_groups_4205)) {\n            *(volatile __local int32_t *) &mem_5179[squot32(local_tid_4605,\n                                                            32) *\n                                                    sizeof(int32_t)] =\n                binop_param_y_4590;\n            *(volatile __local int32_t *) &mem_5182[squot32(local_tid_4605,\n                                                            32) *\n                                                    sizeof(int32_t)] = f2_4591;\n            *(volatile __local int32_t *) &mem_5185[squot32(local_tid_4605,\n                                                            32) *\n                                                    sizeof(int32_t)] = x2_4592;\n            *(volatile __local int32_t *) &mem_5188[squot32(local_tid_4605,\n                                                            32) *\n                                                    sizeof(int32_t)] = f2_4593;\n            *(volatile __local int32_t *) &mem_5191[squot32(local_tid_4605,\n                                                            32) *\n                                                    sizeof(int32_t)] = x2_4594;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_4605, 32) == 0 && slt32(local_tid_4605,\n                                                      num_groups_4205)) {\n            binop_param_y_5467 = *(volatile __local\n    ",
                   "                               int32_t *) &mem_5179[local_tid_4605 *\n                                                        sizeof(int32_t)];\n            f2_5468 = *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                              sizeof(int32_t)];\n            x2_5469 = *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                              sizeof(int32_t)];\n            f2_5470 = *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                              sizeof(int32_t)];\n            x2_5471 = *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                                                              sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_5482 = 1;\n            \n            while (slt32(skip_threads_5482, 32)) {\n                if ((squot32(local_tid_4605, 32) == 0 && slt32(local_tid_4605,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5482, local_tid_4605 -\n                          squot32(local_tid_4605, 32) * 32)) {\n                    // read operands\n                    {\n                        binop_param_x_5462 = *(volatile __local\n                                               int32_t *) &mem_5179[(local_tid_4605 -\n                                                                     skip_threads_5482) *\n                                                                    sizeof(int32_t)];\n                        f1_5463 = *(volatile __local\n                                    int32_t *) &mem_5182[(local_tid_4605 -\n                                                          skip_threads_5482) *\n                                                         sizeof(int32_t)];\n                        x1_5464 = *(volatile __local\n                              ",
                   "      int32_t *) &mem_5185[(local_tid_4605 -\n                                                          skip_threads_5482) *\n                                                         sizeof(int32_t)];\n                        f1_5465 = *(volatile __local\n                                    int32_t *) &mem_5188[(local_tid_4605 -\n                                                          skip_threads_5482) *\n                                                         sizeof(int32_t)];\n                        x1_5466 = *(volatile __local\n                                    int32_t *) &mem_5191[(local_tid_4605 -\n                                                          skip_threads_5482) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5472;\n                        int32_t res_5473;\n                        char cond_5474;\n                        int32_t res_5475;\n                        int32_t res_5477;\n                        char cond_5478;\n                        int32_t res_5479;\n                        \n                        if (thread_active_5459) {\n                            res_5472 = binop_param_x_5462 + binop_param_y_5467;\n                            res_5473 = f1_5463 | f2_5468;\n                            cond_5474 = slt32(0, f2_5468);\n                            if (cond_5474) {\n                                res_5475 = x2_5469;\n                            } else {\n                                int32_t res_5476 = x1_5464 + x2_5469;\n                                \n                                res_5475 = res_5476;\n                            }\n                            res_5477 = f1_5465 | f2_5470;\n                            cond_5478 = slt32(0, f2_5470);\n                            if (cond_5478) {\n                                res_5479 = x2_5471;\n                            } else {\n                   ",
                   "             int32_t res_5480 = x1_5466 + x2_5471;\n                                \n                                res_5479 = res_5480;\n                            }\n                        }\n                        binop_param_y_5467 = res_5472;\n                        f2_5468 = res_5473;\n                        x2_5469 = res_5475;\n                        f2_5470 = res_5477;\n                        x2_5471 = res_5479;\n                    }\n                }\n                if (sle32(wave_sizze_5457, skip_threads_5482)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_4605, 32) == 0 && slt32(local_tid_4605,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5482, local_tid_4605 -\n                          squot32(local_tid_4605, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_5179[local_tid_4605 *\n                                                                sizeof(int32_t)] =\n                            binop_param_y_5467;\n                        *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                                sizeof(int32_t)] =\n                            f2_5468;\n                        *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                                sizeof(int32_t)] =\n                            x2_5469;\n                        *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                                sizeof(int32_t)] =\n                            f2_5470;\n                        *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                                                                sizeof(int32_t)] =\n                            x2_5471;\n                    }\n                }\n ",
                   "               if (sle32(wave_sizze_5457, skip_threads_5482)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5482 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_4605, 32) == 0 || !slt32(local_tid_4605,\n                                                         num_groups_4205))) {\n            // read operands\n            {\n                binop_param_x_4585 = *(volatile __local\n                                       int32_t *) &mem_5179[(squot32(local_tid_4605,\n                                                                     32) - 1) *\n                                                            sizeof(int32_t)];\n                f1_4586 = *(volatile __local\n                            int32_t *) &mem_5182[(squot32(local_tid_4605, 32) -\n                                                  1) * sizeof(int32_t)];\n                x1_4587 = *(volatile __local\n                            int32_t *) &mem_5185[(squot32(local_tid_4605, 32) -\n                                                  1) * sizeof(int32_t)];\n                f1_4588 = *(volatile __local\n                            int32_t *) &mem_5188[(squot32(local_tid_4605, 32) -\n                                                  1) * sizeof(int32_t)];\n                x1_4589 = *(volatile __local\n                            int32_t *) &mem_5191[(squot32(local_tid_4605, 32) -\n                                                  1) * sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                int32_t res_4595;\n                int32_t res_4596;\n                char cond_4597;\n                int32_t res_4598;\n                int32_t res_4600;\n                char cond_4601;\n                int32_t res_4602;\n                \n                if (thread_active_5459) {\n                    res_4595 = binop_param_x_4585 + binop_param_y_4590",
                   ";\n                    res_4596 = f1_4586 | f2_4591;\n                    cond_4597 = slt32(0, f2_4591);\n                    if (cond_4597) {\n                        res_4598 = x2_4592;\n                    } else {\n                        int32_t res_4599 = x1_4587 + x2_4592;\n                        \n                        res_4598 = res_4599;\n                    }\n                    res_4600 = f1_4588 | f2_4593;\n                    cond_4601 = slt32(0, f2_4593);\n                    if (cond_4601) {\n                        res_4602 = x2_4594;\n                    } else {\n                        int32_t res_4603 = x1_4589 + x2_4594;\n                        \n                        res_4602 = res_4603;\n                    }\n                }\n                binop_param_y_4590 = res_4595;\n                f2_4591 = res_4596;\n                x2_4592 = res_4598;\n                f2_4593 = res_4600;\n                x2_4594 = res_4602;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_5179[local_tid_4605 *\n                                                        sizeof(int32_t)] =\n                    binop_param_y_4590;\n                *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                        sizeof(int32_t)] =\n                    f2_4591;\n                *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                        sizeof(int32_t)] =\n                    x2_4592;\n                *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                        sizeof(int32_t)] =\n                    f2_4593;\n                *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                                                        sizeof(int32_t)] =\n                    x2_4594;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for fir",
                   "st block\n    {\n        if (squot32(local_tid_4605, 32) == 0) {\n            *(volatile __local int32_t *) &mem_5179[local_tid_4605 *\n                                                    sizeof(int32_t)] =\n                binop_param_y_4590;\n            *(volatile __local int32_t *) &mem_5182[local_tid_4605 *\n                                                    sizeof(int32_t)] = f2_4591;\n            *(volatile __local int32_t *) &mem_5185[local_tid_4605 *\n                                                    sizeof(int32_t)] = x2_4592;\n            *(volatile __local int32_t *) &mem_5188[local_tid_4605 *\n                                                    sizeof(int32_t)] = f2_4593;\n            *(volatile __local int32_t *) &mem_5191[local_tid_4605 *\n                                                    sizeof(int32_t)] = x2_4594;\n        }\n    }\n    \n    int32_t scanned_elem_4622;\n    int32_t scanned_elem_4623;\n    int32_t scanned_elem_4624;\n    int32_t scanned_elem_4625;\n    int32_t scanned_elem_4626;\n    \n    if (thread_active_5459) {\n        scanned_elem_4622 = *(__local int32_t *) &mem_5179[local_tid_4605 * 4];\n        scanned_elem_4623 = *(__local int32_t *) &mem_5182[local_tid_4605 * 4];\n        scanned_elem_4624 = *(__local int32_t *) &mem_5185[local_tid_4605 * 4];\n        scanned_elem_4625 = *(__local int32_t *) &mem_5188[local_tid_4605 * 4];\n        scanned_elem_4626 = *(__local int32_t *) &mem_5191[local_tid_4605 * 4];\n    }\n    *(__global int32_t *) &mem_5194[global_tid_4604 * 4] = scanned_elem_4622;\n    *(__global int32_t *) &mem_5197[global_tid_4604 * 4] = scanned_elem_4623;\n    *(__global int32_t *) &mem_5200[global_tid_4604 * 4] = scanned_elem_4624;\n    *(__global int32_t *) &mem_5203[global_tid_4604 * 4] = scanned_elem_4625;\n    *(__global int32_t *) &mem_5206[global_tid_4604 * 4] = scanned_elem_4626;\n}\n__kernel void scan2_kernel_4777(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n          ",
                   "                      int32_t num_groups_4205, __global\n                                unsigned char *mem_5239, __global\n                                unsigned char *mem_5242, __global\n                                unsigned char *mem_5251, __global\n                                unsigned char *mem_5254)\n{\n    __local volatile char *restrict mem_5245 = mem_aligned_0;\n    __local volatile char *restrict mem_5248 = mem_aligned_1;\n    int32_t wave_sizze_5506;\n    int32_t group_sizze_5507;\n    char thread_active_5508;\n    int32_t global_tid_4777;\n    int32_t local_tid_4778;\n    int32_t group_id_4779;\n    \n    global_tid_4777 = get_global_id(0);\n    local_tid_4778 = get_local_id(0);\n    group_sizze_5507 = get_local_size(0);\n    wave_sizze_5506 = LOCKSTEP_WIDTH;\n    group_id_4779 = get_group_id(0);\n    thread_active_5508 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_4778, num_groups_4205) && 1) {\n        int32_t res_group_sums_elem_4780;\n        int32_t res_group_sums_elem_4781;\n        \n        res_group_sums_elem_4780 = *(__global\n                                     int32_t *) &mem_5239[local_tid_4778 * 4];\n        res_group_sums_elem_4781 = *(__global\n                                     int32_t *) &mem_5242[local_tid_4778 * 4];\n        *(__local int32_t *) &mem_5245[local_tid_4778 * 4] =\n            res_group_sums_elem_4780;\n        *(__local int32_t *) &mem_5248[local_tid_4778 * 4] =\n            res_group_sums_elem_4781;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_4767;\n    int32_t other_index_4768;\n    int32_t f1_4769;\n    int32_t x1_4770;\n    int32_t f2_4771;\n    int32_t x2_4772;\n    int32_t my_index_5509;\n    int32_t other_index_5510;\n    int32_t f1_5511;\n    int32_t x1_5512;\n    int32_t f2_5513;\n    int32_t x2_5514;\n    \n    my_index_4767 = local_tid_4778;\n    if (slt32(local_tid_4778, num_groups_4205)) {\n        f2_4771 = *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                        ",
                   "                  sizeof(int32_t)];\n        x2_4772 = *(volatile __local int32_t *) &mem_5248[local_tid_4778 *\n                                                          sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_5519 = 1;\n        \n        while (slt32(skip_threads_5519, 32)) {\n            if (slt32(local_tid_4778, num_groups_4205) &&\n                sle32(skip_threads_5519, local_tid_4778 -\n                      squot32(local_tid_4778, 32) * 32)) {\n                // read operands\n                {\n                    f1_4769 = *(volatile __local\n                                int32_t *) &mem_5245[(local_tid_4778 -\n                                                      skip_threads_5519) *\n                                                     sizeof(int32_t)];\n                    x1_4770 = *(volatile __local\n                                int32_t *) &mem_5248[(local_tid_4778 -\n                                                      skip_threads_5519) *\n                                                     sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    int32_t res_4773;\n                    char cond_4774;\n                    int32_t res_4775;\n                    \n                    if (thread_active_5508) {\n                        res_4773 = f1_4769 | f2_4771;\n                        cond_4774 = slt32(0, f2_4771);\n                        if (cond_4774) {\n                            res_4775 = x2_4772;\n                        } else {\n                            int32_t res_4776 = x1_4770 + x2_4772;\n                            \n                            res_4775 = res_4776;\n                        }\n                    }\n                    f2_4771 = res_4773;\n                    x2_4772 = res_4775;\n                }\n            }\n            if (sle32(wave_sizze_5506, skip_threads_5519)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n  ",
                   "          }\n            if (slt32(local_tid_4778, num_groups_4205) &&\n                sle32(skip_threads_5519, local_tid_4778 -\n                      squot32(local_tid_4778, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                                            sizeof(int32_t)] =\n                        f2_4771;\n                    *(volatile __local int32_t *) &mem_5248[local_tid_4778 *\n                                                            sizeof(int32_t)] =\n                        x2_4772;\n                }\n            }\n            if (sle32(wave_sizze_5506, skip_threads_5519)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5519 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_4778 - squot32(local_tid_4778, 32) * 32) == 31 &&\n            slt32(local_tid_4778, num_groups_4205)) {\n            *(volatile __local int32_t *) &mem_5245[squot32(local_tid_4778,\n                                                            32) *\n                                                    sizeof(int32_t)] = f2_4771;\n            *(volatile __local int32_t *) &mem_5248[squot32(local_tid_4778,\n                                                            32) *\n                                                    sizeof(int32_t)] = x2_4772;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_4778, 32) == 0 && slt32(local_tid_4778,\n                                                      num_groups_4205)) {\n            f2_5513 = *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                                              sizeof(int32_t)];\n            x2_5514 = *(volatile __local int32_t *) &mem_5248",
                   "[local_tid_4778 *\n                                                              sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_5520 = 1;\n            \n            while (slt32(skip_threads_5520, 32)) {\n                if ((squot32(local_tid_4778, 32) == 0 && slt32(local_tid_4778,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5520, local_tid_4778 -\n                          squot32(local_tid_4778, 32) * 32)) {\n                    // read operands\n                    {\n                        f1_5511 = *(volatile __local\n                                    int32_t *) &mem_5245[(local_tid_4778 -\n                                                          skip_threads_5520) *\n                                                         sizeof(int32_t)];\n                        x1_5512 = *(volatile __local\n                                    int32_t *) &mem_5248[(local_tid_4778 -\n                                                          skip_threads_5520) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5515;\n                        char cond_5516;\n                        int32_t res_5517;\n                        \n                        if (thread_active_5508) {\n                            res_5515 = f1_5511 | f2_5513;\n                            cond_5516 = slt32(0, f2_5513);\n                            if (cond_5516) {\n                                res_5517 = x2_5514;\n                            } else {\n                                int32_t res_5518 = x1_5512 + x2_5514;\n                                \n                                res_5517 = res_5518;\n                            }\n                        }\n                        f2_5513 = res_5515;\n                 ",
                   "       x2_5514 = res_5517;\n                    }\n                }\n                if (sle32(wave_sizze_5506, skip_threads_5520)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_4778, 32) == 0 && slt32(local_tid_4778,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5520, local_tid_4778 -\n                          squot32(local_tid_4778, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                                                sizeof(int32_t)] =\n                            f2_5513;\n                        *(volatile __local int32_t *) &mem_5248[local_tid_4778 *\n                                                                sizeof(int32_t)] =\n                            x2_5514;\n                    }\n                }\n                if (sle32(wave_sizze_5506, skip_threads_5520)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5520 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_4778, 32) == 0 || !slt32(local_tid_4778,\n                                                         num_groups_4205))) {\n            // read operands\n            {\n                f1_4769 = *(volatile __local\n                            int32_t *) &mem_5245[(squot32(local_tid_4778, 32) -\n                                                  1) * sizeof(int32_t)];\n                x1_4770 = *(volatile __local\n                            int32_t *) &mem_5248[(squot32(local_tid_4778, 32) -\n                                                  1) * sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                int32_t res_4773;\n                char cond_4774;\n   ",
                   "             int32_t res_4775;\n                \n                if (thread_active_5508) {\n                    res_4773 = f1_4769 | f2_4771;\n                    cond_4774 = slt32(0, f2_4771);\n                    if (cond_4774) {\n                        res_4775 = x2_4772;\n                    } else {\n                        int32_t res_4776 = x1_4770 + x2_4772;\n                        \n                        res_4775 = res_4776;\n                    }\n                }\n                f2_4771 = res_4773;\n                x2_4772 = res_4775;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                                        sizeof(int32_t)] =\n                    f2_4771;\n                *(volatile __local int32_t *) &mem_5248[local_tid_4778 *\n                                                        sizeof(int32_t)] =\n                    x2_4772;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_4778, 32) == 0) {\n            *(volatile __local int32_t *) &mem_5245[local_tid_4778 *\n                                                    sizeof(int32_t)] = f2_4771;\n            *(volatile __local int32_t *) &mem_5248[local_tid_4778 *\n                                                    sizeof(int32_t)] = x2_4772;\n        }\n    }\n    \n    int32_t scanned_elem_4786;\n    int32_t scanned_elem_4787;\n    \n    if (thread_active_5508) {\n        scanned_elem_4786 = *(__local int32_t *) &mem_5245[local_tid_4778 * 4];\n        scanned_elem_4787 = *(__local int32_t *) &mem_5248[local_tid_4778 * 4];\n    }\n    *(__global int32_t *) &mem_5251[global_tid_4777 * 4] = scanned_elem_4786;\n    *(__global int32_t *) &mem_5254[global_tid_4777 * 4] = scanned_elem_4787;\n}\n__kernel void scan2_kernel_4916(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_",
                   "aligned_1,\n                                int32_t num_groups_4205, __global\n                                unsigned char *mem_5275, __global\n                                unsigned char *mem_5278, __global\n                                unsigned char *mem_5287, __global\n                                unsigned char *mem_5290)\n{\n    __local volatile char *restrict mem_5281 = mem_aligned_0;\n    __local volatile char *restrict mem_5284 = mem_aligned_1;\n    int32_t wave_sizze_5543;\n    int32_t group_sizze_5544;\n    char thread_active_5545;\n    int32_t global_tid_4916;\n    int32_t local_tid_4917;\n    int32_t group_id_4918;\n    \n    global_tid_4916 = get_global_id(0);\n    local_tid_4917 = get_local_id(0);\n    group_sizze_5544 = get_local_size(0);\n    wave_sizze_5543 = LOCKSTEP_WIDTH;\n    group_id_4918 = get_group_id(0);\n    thread_active_5545 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_4917, num_groups_4205) && 1) {\n        int32_t resarr0_group_sums_elem_4919;\n        int32_t resarr0_group_sums_elem_4920;\n        \n        resarr0_group_sums_elem_4919 = *(__global\n                                         int32_t *) &mem_5275[local_tid_4917 *\n                                                              4];\n        resarr0_group_sums_elem_4920 = *(__global\n                                         int32_t *) &mem_5278[local_tid_4917 *\n                                                              4];\n        *(__local int32_t *) &mem_5281[local_tid_4917 * 4] =\n            resarr0_group_sums_elem_4919;\n        *(__local int32_t *) &mem_5284[local_tid_4917 * 4] =\n            resarr0_group_sums_elem_4920;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_4906;\n    int32_t other_index_4907;\n    int32_t f1_4908;\n    int32_t x1_4909;\n    int32_t f2_4910;\n    int32_t x2_4911;\n    int32_t my_index_5546;\n    int32_t other_index_5547;\n    int32_t f1_5548;\n    int32_t x1_5549;\n    int32_t f2_5550;\n    int32_t x2_5551;\n    \n    my_index_4906 = loca",
                   "l_tid_4917;\n    if (slt32(local_tid_4917, num_groups_4205)) {\n        f2_4910 = *(volatile __local int32_t *) &mem_5281[local_tid_4917 *\n                                                          sizeof(int32_t)];\n        x2_4911 = *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                          sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_5556 = 1;\n        \n        while (slt32(skip_threads_5556, 32)) {\n            if (slt32(local_tid_4917, num_groups_4205) &&\n                sle32(skip_threads_5556, local_tid_4917 -\n                      squot32(local_tid_4917, 32) * 32)) {\n                // read operands\n                {\n                    f1_4908 = *(volatile __local\n                                int32_t *) &mem_5281[(local_tid_4917 -\n                                                      skip_threads_5556) *\n                                                     sizeof(int32_t)];\n                    x1_4909 = *(volatile __local\n                                int32_t *) &mem_5284[(local_tid_4917 -\n                                                      skip_threads_5556) *\n                                                     sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    int32_t res_4912;\n                    char cond_4913;\n                    int32_t res_4914;\n                    \n                    if (thread_active_5545) {\n                        res_4912 = f1_4908 | f2_4910;\n                        cond_4913 = slt32(0, f2_4910);\n                        if (cond_4913) {\n                            res_4914 = x2_4911;\n                        } else {\n                            int32_t res_4915 = x1_4909 + x2_4911;\n                            \n                            res_4914 = res_4915;\n                        }\n                    }\n                    f2_4910 = res_4912;\n    ",
                   "                x2_4911 = res_4914;\n                }\n            }\n            if (sle32(wave_sizze_5543, skip_threads_5556)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (slt32(local_tid_4917, num_groups_4205) &&\n                sle32(skip_threads_5556, local_tid_4917 -\n                      squot32(local_tid_4917, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_5281[local_tid_4917 *\n                                                            sizeof(int32_t)] =\n                        f2_4910;\n                    *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                            sizeof(int32_t)] =\n                        x2_4911;\n                }\n            }\n            if (sle32(wave_sizze_5543, skip_threads_5556)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5556 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_4917 - squot32(local_tid_4917, 32) * 32) == 31 &&\n            slt32(local_tid_4917, num_groups_4205)) {\n            *(volatile __local int32_t *) &mem_5281[squot32(local_tid_4917,\n                                                            32) *\n                                                    sizeof(int32_t)] = f2_4910;\n            *(volatile __local int32_t *) &mem_5284[squot32(local_tid_4917,\n                                                            32) *\n                                                    sizeof(int32_t)] = x2_4911;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_4917, 32) == 0 && slt32(local_tid_4917,\n                                                      num_groups_4205)) {\n            f2_5550 = *(volatile __local in",
                   "t32_t *) &mem_5281[local_tid_4917 *\n                                                              sizeof(int32_t)];\n            x2_5551 = *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                              sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_5557 = 1;\n            \n            while (slt32(skip_threads_5557, 32)) {\n                if ((squot32(local_tid_4917, 32) == 0 && slt32(local_tid_4917,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5557, local_tid_4917 -\n                          squot32(local_tid_4917, 32) * 32)) {\n                    // read operands\n                    {\n                        f1_5548 = *(volatile __local\n                                    int32_t *) &mem_5281[(local_tid_4917 -\n                                                          skip_threads_5557) *\n                                                         sizeof(int32_t)];\n                        x1_5549 = *(volatile __local\n                                    int32_t *) &mem_5284[(local_tid_4917 -\n                                                          skip_threads_5557) *\n                                                         sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5552;\n                        char cond_5553;\n                        int32_t res_5554;\n                        \n                        if (thread_active_5545) {\n                            res_5552 = f1_5548 | f2_5550;\n                            cond_5553 = slt32(0, f2_5550);\n                            if (cond_5553) {\n                                res_5554 = x2_5551;\n                            } else {\n                                int32_t res_5555 = x1_5549 + x2_5551;\n                          ",
                   "      \n                                res_5554 = res_5555;\n                            }\n                        }\n                        f2_5550 = res_5552;\n                        x2_5551 = res_5554;\n                    }\n                }\n                if (sle32(wave_sizze_5543, skip_threads_5557)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_4917, 32) == 0 && slt32(local_tid_4917,\n                                                               num_groups_4205)) &&\n                    sle32(skip_threads_5557, local_tid_4917 -\n                          squot32(local_tid_4917, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_5281[local_tid_4917 *\n                                                                sizeof(int32_t)] =\n                            f2_5550;\n                        *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                                sizeof(int32_t)] =\n                            x2_5551;\n                    }\n                }\n                if (sle32(wave_sizze_5543, skip_threads_5557)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5557 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_4917, 32) == 0 || !slt32(local_tid_4917,\n                                                         num_groups_4205))) {\n            // read operands\n            {\n                f1_4908 = *(volatile __local\n                            int32_t *) &mem_5281[(squot32(local_tid_4917, 32) -\n                                                  1) * sizeof(int32_t)];\n                x1_4909 = *(volatile __local\n                            int32_t *) &mem_5284[(squot32(local_tid_4917, 32) -\n                          ",
                   "                        1) * sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                int32_t res_4912;\n                char cond_4913;\n                int32_t res_4914;\n                \n                if (thread_active_5545) {\n                    res_4912 = f1_4908 | f2_4910;\n                    cond_4913 = slt32(0, f2_4910);\n                    if (cond_4913) {\n                        res_4914 = x2_4911;\n                    } else {\n                        int32_t res_4915 = x1_4909 + x2_4911;\n                        \n                        res_4914 = res_4915;\n                    }\n                }\n                f2_4910 = res_4912;\n                x2_4911 = res_4914;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_5281[local_tid_4917 *\n                                                        sizeof(int32_t)] =\n                    f2_4910;\n                *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                        sizeof(int32_t)] =\n                    x2_4911;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_4917, 32) == 0) {\n            *(volatile __local int32_t *) &mem_5281[local_tid_4917 *\n                                                    sizeof(int32_t)] = f2_4910;\n            *(volatile __local int32_t *) &mem_5284[local_tid_4917 *\n                                                    sizeof(int32_t)] = x2_4911;\n        }\n    }\n    \n    int32_t scanned_elem_4925;\n    int32_t scanned_elem_4926;\n    \n    if (thread_active_5545) {\n        scanned_elem_4925 = *(__local int32_t *) &mem_5281[local_tid_4917 * 4];\n        scanned_elem_4926 = *(__local int32_t *) &mem_5284[local_tid_4917 * 4];\n    }\n    *(__global int32_t *) &mem_5287[global_tid_4916 * 4] = scanned_elem_4925;\n    *(__global int32_t *) &mem_5290[gl",
                   "obal_tid_4916 * 4] = scanned_elem_4926;\n}\n", NULL};
static cl_kernel chunked_reduce_kernel_5015;
static int chunked_reduce_kernel_5015total_runtime = 0;
static int chunked_reduce_kernel_5015runs = 0;
static cl_kernel kernel_replicate_5342;
static int kernel_replicate_5342total_runtime = 0;
static int kernel_replicate_5342runs = 0;
static cl_kernel map_kernel_4193;
static int map_kernel_4193total_runtime = 0;
static int map_kernel_4193runs = 0;
static cl_kernel map_kernel_4284;
static int map_kernel_4284total_runtime = 0;
static int map_kernel_4284runs = 0;
static cl_kernel map_kernel_4428;
static int map_kernel_4428total_runtime = 0;
static int map_kernel_4428runs = 0;
static cl_kernel map_kernel_4674;
static int map_kernel_4674total_runtime = 0;
static int map_kernel_4674runs = 0;
static cl_kernel map_kernel_4815;
static int map_kernel_4815total_runtime = 0;
static int map_kernel_4815runs = 0;
static cl_kernel map_kernel_4954;
static int map_kernel_4954total_runtime = 0;
static int map_kernel_4954runs = 0;
static cl_kernel map_kernel_4964;
static int map_kernel_4964total_runtime = 0;
static int map_kernel_4964runs = 0;
static cl_kernel map_kernel_4990;
static int map_kernel_4990total_runtime = 0;
static int map_kernel_4990runs = 0;
static cl_kernel reduce_kernel_5046;
static int reduce_kernel_5046total_runtime = 0;
static int reduce_kernel_5046runs = 0;
static cl_kernel scan1_kernel_4226;
static int scan1_kernel_4226total_runtime = 0;
static int scan1_kernel_4226runs = 0;
static cl_kernel scan1_kernel_4341;
static int scan1_kernel_4341total_runtime = 0;
static int scan1_kernel_4341runs = 0;
static cl_kernel scan1_kernel_4507;
static int scan1_kernel_4507total_runtime = 0;
static int scan1_kernel_4507runs = 0;
static cl_kernel scan1_kernel_4724;
static int scan1_kernel_4724total_runtime = 0;
static int scan1_kernel_4724runs = 0;
static cl_kernel scan1_kernel_4867;
static int scan1_kernel_4867total_runtime = 0;
static int scan1_kernel_4867runs = 0;
static cl_kernel scan2_kernel_4258;
static int scan2_kernel_4258total_runtime = 0;
static int scan2_kernel_4258runs = 0;
static cl_kernel scan2_kernel_4390;
static int scan2_kernel_4390total_runtime = 0;
static int scan2_kernel_4390runs = 0;
static cl_kernel scan2_kernel_4604;
static int scan2_kernel_4604total_runtime = 0;
static int scan2_kernel_4604runs = 0;
static cl_kernel scan2_kernel_4777;
static int scan2_kernel_4777total_runtime = 0;
static int scan2_kernel_4777runs = 0;
static cl_kernel scan2_kernel_4916;
static int scan2_kernel_4916total_runtime = 0;
static int scan2_kernel_4916runs = 0;
void setup_opencl_and_load_kernels()
{
    cl_int error;
    cl_program prog = setup_opencl(fut_opencl_program);
    
    {
        chunked_reduce_kernel_5015 = clCreateKernel(prog,
                                                    "chunked_reduce_kernel_5015",
                                                    &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n",
                    "chunked_reduce_kernel_5015");
    }
    {
        kernel_replicate_5342 = clCreateKernel(prog, "kernel_replicate_5342",
                                               &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "kernel_replicate_5342");
    }
    {
        map_kernel_4193 = clCreateKernel(prog, "map_kernel_4193", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4193");
    }
    {
        map_kernel_4284 = clCreateKernel(prog, "map_kernel_4284", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4284");
    }
    {
        map_kernel_4428 = clCreateKernel(prog, "map_kernel_4428", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4428");
    }
    {
        map_kernel_4674 = clCreateKernel(prog, "map_kernel_4674", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4674");
    }
    {
        map_kernel_4815 = clCreateKernel(prog, "map_kernel_4815", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4815");
    }
    {
        map_kernel_4954 = clCreateKernel(prog, "map_kernel_4954", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4954");
    }
    {
        map_kernel_4964 = clCreateKernel(prog, "map_kernel_4964", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4964");
    }
    {
        map_kernel_4990 = clCreateKernel(prog, "map_kernel_4990", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_4990");
    }
    {
        reduce_kernel_5046 = clCreateKernel(prog, "reduce_kernel_5046", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "reduce_kernel_5046");
    }
    {
        scan1_kernel_4226 = clCreateKernel(prog, "scan1_kernel_4226", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_4226");
    }
    {
        scan1_kernel_4341 = clCreateKernel(prog, "scan1_kernel_4341", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_4341");
    }
    {
        scan1_kernel_4507 = clCreateKernel(prog, "scan1_kernel_4507", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_4507");
    }
    {
        scan1_kernel_4724 = clCreateKernel(prog, "scan1_kernel_4724", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_4724");
    }
    {
        scan1_kernel_4867 = clCreateKernel(prog, "scan1_kernel_4867", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_4867");
    }
    {
        scan2_kernel_4258 = clCreateKernel(prog, "scan2_kernel_4258", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_4258");
    }
    {
        scan2_kernel_4390 = clCreateKernel(prog, "scan2_kernel_4390", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_4390");
    }
    {
        scan2_kernel_4604 = clCreateKernel(prog, "scan2_kernel_4604", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_4604");
    }
    {
        scan2_kernel_4777 = clCreateKernel(prog, "scan2_kernel_4777", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_4777");
    }
    {
        scan2_kernel_4916 = clCreateKernel(prog, "scan2_kernel_4916", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_4916");
    }
}
void post_opencl_setup(struct opencl_device_option *option)
{
    if (strcmp(option->platform_name, "NVIDIA CUDA") == 0 &&
        option->device_type == CL_DEVICE_TYPE_GPU) {
        cl_lockstep_width = 32;
        if (debugging)
            fprintf(stderr, "Setting lockstep width to: %d\n",
                    cl_lockstep_width);
    }
    if (strcmp(option->platform_name, "AMD Accelerated Parallel Processing") ==
        0 && option->device_type == CL_DEVICE_TYPE_GPU) {
        cl_lockstep_width = 64;
        if (debugging)
            fprintf(stderr, "Setting lockstep width to: %d\n",
                    cl_lockstep_width);
    }
}
int64_t peak_mem_usage_device = 0;
int64_t cur_mem_usage_device = 0;
struct memblock_device {
    int *references;
    cl_mem mem;
    int64_t size;
} ;
static void memblock_unref_device(struct memblock_device *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'device': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_device -= block->size;
            OPENCL_SUCCEED(clReleaseMemObject(block->mem));
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_device);
        }
    }
}
static void memblock_alloc_device(struct memblock_device *block, int32_t size)
{
    memblock_unref_device(block);
    
    cl_int clCreateBuffer_succeeded_5687;
    
    block->mem = clCreateBuffer(fut_cl_context, CL_MEM_READ_WRITE, size >
                                0 ? size : 1, NULL,
                                &clCreateBuffer_succeeded_5687);
    OPENCL_SUCCEED(clCreateBuffer_succeeded_5687);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_device += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'device' (now allocated: %ld bytes)",
                size, cur_mem_usage_device);
    if (cur_mem_usage_device > peak_mem_usage_device) {
        peak_mem_usage_device = cur_mem_usage_device;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_device);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_device(struct memblock_device *lhs,
                                struct memblock_device *rhs)
{
    memblock_unref_device(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
int64_t peak_mem_usage_local = 0;
int64_t cur_mem_usage_local = 0;
struct memblock_local {
    int *references;
    unsigned char mem;
    int64_t size;
} ;
static void memblock_unref_local(struct memblock_local *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'local': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_local -= block->size;
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_local);
        }
    }
}
static void memblock_alloc_local(struct memblock_local *block, int32_t size)
{
    memblock_unref_local(block);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_local += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'local' (now allocated: %ld bytes)",
                size, cur_mem_usage_local);
    if (cur_mem_usage_local > peak_mem_usage_local) {
        peak_mem_usage_local = cur_mem_usage_local;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_local);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_local(struct memblock_local *lhs,
                               struct memblock_local *rhs)
{
    memblock_unref_local(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
int64_t peak_mem_usage_default = 0;
int64_t cur_mem_usage_default = 0;
struct memblock {
    int *references;
    char *mem;
    int64_t size;
} ;
static void memblock_unref(struct memblock *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in default space: %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_default);
        }
    }
}
static void memblock_alloc(struct memblock *block, int32_t size)
{
    memblock_unref(block);
    block->mem = (char *) malloc(size);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_default += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in default space (now allocated: %ld bytes)",
                size, cur_mem_usage_default);
    if (cur_mem_usage_default > peak_mem_usage_default) {
        peak_mem_usage_default = cur_mem_usage_default;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_default);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set(struct memblock *lhs, struct memblock *rhs)
{
    memblock_unref(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
struct tuple_int32_t_device_mem_int32_t {
    int32_t elem_0;
    struct memblock_device elem_1;
    int32_t elem_2;
} ;
static int32_t static_array_realtype_5581[1] = {0};
static struct memblock_device static_array_5347;
static struct tuple_int32_t_device_mem_int32_t
futhark_main(int64_t arr_mem_sizze_5061, struct memblock_device arr_mem_5062,
             int32_t sizze_3871);
static inline float futhark_log32(float x)
{
    return log(x);
}
static inline float futhark_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futhark_exp32(float x)
{
    return exp(x);
}
static inline float futhark_cos32(float x)
{
    return cos(x);
}
static inline float futhark_sin32(float x)
{
    return sin(x);
}
static inline float futhark_acos32(float x)
{
    return acos(x);
}
static inline float futhark_asin32(float x)
{
    return asin(x);
}
static inline double futhark_atan32(float x)
{
    return atan(x);
}
static inline float futhark_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline char futhark_isnan32(float x)
{
    return isnan(x);
}
static inline char futhark_isinf32(float x)
{
    return isinf(x);
}
static inline double futhark_log64(double x)
{
    return log(x);
}
static inline double futhark_sqrt64(double x)
{
    return sqrt(x);
}
static inline double futhark_exp64(double x)
{
    return exp(x);
}
static inline double futhark_cos64(double x)
{
    return cos(x);
}
static inline double futhark_sin64(double x)
{
    return sin(x);
}
static inline double futhark_acos64(double x)
{
    return acos(x);
}
static inline double futhark_asin64(double x)
{
    return asin(x);
}
static inline double futhark_atan64(double x)
{
    return atan(x);
}
static inline double futhark_atan2_64(double x, double y)
{
    return atan2(x, y);
}
static inline char futhark_isnan64(double x)
{
    return isnan(x);
}
static inline char futhark_isinf64(double x)
{
    return isinf(x);
}
static inline int8_t add8(int8_t x, int8_t y)
{
    return x + y;
}
static inline int16_t add16(int16_t x, int16_t y)
{
    return x + y;
}
static inline int32_t add32(int32_t x, int32_t y)
{
    return x + y;
}
static inline int64_t add64(int64_t x, int64_t y)
{
    return x + y;
}
static inline int8_t sub8(int8_t x, int8_t y)
{
    return x - y;
}
static inline int16_t sub16(int16_t x, int16_t y)
{
    return x - y;
}
static inline int32_t sub32(int32_t x, int32_t y)
{
    return x - y;
}
static inline int64_t sub64(int64_t x, int64_t y)
{
    return x - y;
}
static inline int8_t mul8(int8_t x, int8_t y)
{
    return x * y;
}
static inline int16_t mul16(int16_t x, int16_t y)
{
    return x * y;
}
static inline int32_t mul32(int32_t x, int32_t y)
{
    return x * y;
}
static inline int64_t mul64(int64_t x, int64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline char ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline char ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline char ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline char ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline char ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline char ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline char ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline char ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline char slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline char slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline char slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline char slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline char sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline char sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline char sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline char sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int8_t sext_i8_i8(int8_t x)
{
    return x;
}
static inline int16_t sext_i8_i16(int8_t x)
{
    return x;
}
static inline int32_t sext_i8_i32(int8_t x)
{
    return x;
}
static inline int64_t sext_i8_i64(int8_t x)
{
    return x;
}
static inline int8_t sext_i16_i8(int16_t x)
{
    return x;
}
static inline int16_t sext_i16_i16(int16_t x)
{
    return x;
}
static inline int32_t sext_i16_i32(int16_t x)
{
    return x;
}
static inline int64_t sext_i16_i64(int16_t x)
{
    return x;
}
static inline int8_t sext_i32_i8(int32_t x)
{
    return x;
}
static inline int16_t sext_i32_i16(int32_t x)
{
    return x;
}
static inline int32_t sext_i32_i32(int32_t x)
{
    return x;
}
static inline int64_t sext_i32_i64(int32_t x)
{
    return x;
}
static inline int8_t sext_i64_i8(int64_t x)
{
    return x;
}
static inline int16_t sext_i64_i16(int64_t x)
{
    return x;
}
static inline int32_t sext_i64_i32(int64_t x)
{
    return x;
}
static inline int64_t sext_i64_i64(int64_t x)
{
    return x;
}
static inline uint8_t zext_i8_i8(uint8_t x)
{
    return x;
}
static inline uint16_t zext_i8_i16(uint8_t x)
{
    return x;
}
static inline uint32_t zext_i8_i32(uint8_t x)
{
    return x;
}
static inline uint64_t zext_i8_i64(uint8_t x)
{
    return x;
}
static inline uint8_t zext_i16_i8(uint16_t x)
{
    return x;
}
static inline uint16_t zext_i16_i16(uint16_t x)
{
    return x;
}
static inline uint32_t zext_i16_i32(uint16_t x)
{
    return x;
}
static inline uint64_t zext_i16_i64(uint16_t x)
{
    return x;
}
static inline uint8_t zext_i32_i8(uint32_t x)
{
    return x;
}
static inline uint16_t zext_i32_i16(uint32_t x)
{
    return x;
}
static inline uint32_t zext_i32_i32(uint32_t x)
{
    return x;
}
static inline uint64_t zext_i32_i64(uint32_t x)
{
    return x;
}
static inline uint8_t zext_i64_i8(uint64_t x)
{
    return x;
}
static inline uint16_t zext_i64_i16(uint64_t x)
{
    return x;
}
static inline uint32_t zext_i64_i32(uint64_t x)
{
    return x;
}
static inline uint64_t zext_i64_i64(uint64_t x)
{
    return x;
}
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return x < y ? x : y;
}
static inline float fmax32(float x, float y)
{
    return x < y ? y : x;
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline char cmplt32(float x, float y)
{
    return x < y;
}
static inline char cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return x;
}
static inline double fdiv64(double x, double y)
{
    return x / y;
}
static inline double fadd64(double x, double y)
{
    return x + y;
}
static inline double fsub64(double x, double y)
{
    return x - y;
}
static inline double fmul64(double x, double y)
{
    return x * y;
}
static inline double fmin64(double x, double y)
{
    return x < y ? x : y;
}
static inline double fmax64(double x, double y)
{
    return x < y ? y : x;
}
static inline double fpow64(double x, double y)
{
    return pow(x, y);
}
static inline char cmplt64(double x, double y)
{
    return x < y;
}
static inline char cmple64(double x, double y)
{
    return x <= y;
}
static inline double sitofp_i8_f64(int8_t x)
{
    return x;
}
static inline double sitofp_i16_f64(int16_t x)
{
    return x;
}
static inline double sitofp_i32_f64(int32_t x)
{
    return x;
}
static inline double sitofp_i64_f64(int64_t x)
{
    return x;
}
static inline double uitofp_i8_f64(uint8_t x)
{
    return x;
}
static inline double uitofp_i16_f64(uint16_t x)
{
    return x;
}
static inline double uitofp_i32_f64(uint32_t x)
{
    return x;
}
static inline double uitofp_i64_f64(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f64_i8(double x)
{
    return x;
}
static inline int16_t fptosi_f64_i16(double x)
{
    return x;
}
static inline int32_t fptosi_f64_i32(double x)
{
    return x;
}
static inline int64_t fptosi_f64_i64(double x)
{
    return x;
}
static inline uint8_t fptoui_f64_i8(double x)
{
    return x;
}
static inline uint16_t fptoui_f64_i16(double x)
{
    return x;
}
static inline uint32_t fptoui_f64_i32(double x)
{
    return x;
}
static inline uint64_t fptoui_f64_i64(double x)
{
    return x;
}
static inline float fpconv_f32_f32(float x)
{
    return x;
}
static inline double fpconv_f32_f64(float x)
{
    return x;
}
static inline float fpconv_f64_f32(double x)
{
    return x;
}
static inline double fpconv_f64_f64(double x)
{
    return x;
}
static int detail_timing = 0;
static
struct tuple_int32_t_device_mem_int32_t futhark_main(int64_t arr_mem_sizze_5061,
                                                     struct memblock_device arr_mem_5062,
                                                     int32_t sizze_3871)
{
    int32_t out_memsizze_5340;
    struct memblock_device out_mem_5339;
    
    out_mem_5339.references = NULL;
    
    int32_t out_arrsizze_5341;
    int64_t binop_x_5064 = sext_i32_i64(sizze_3871);
    int64_t bytes_5063 = binop_x_5064 * 4;
    struct memblock_device mem_5065;
    
    mem_5065.references = NULL;
    memblock_alloc_device(&mem_5065, bytes_5063);
    
    int32_t group_sizze_5345;
    int32_t num_groups_5346;
    
    group_sizze_5345 = cl_group_size;
    num_groups_5346 = squot32(sizze_3871 + sext_i32_i32(group_sizze_5345) - 1,
                              sext_i32_i32(group_sizze_5345));
    OPENCL_SUCCEED(clSetKernelArg(kernel_replicate_5342, 0, sizeof(sizze_3871),
                                  &sizze_3871));
    OPENCL_SUCCEED(clSetKernelArg(kernel_replicate_5342, 1,
                                  sizeof(mem_5065.mem), &mem_5065.mem));
    if (1 * (num_groups_5346 * group_sizze_5345) != 0) {
        const size_t global_work_sizze_5576[1] = {num_groups_5346 *
                     group_sizze_5345};
        const size_t local_work_sizze_5580[1] = {group_sizze_5345};
        int64_t time_start_5577, time_end_5578;
        
        if (debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "kernel_replicate_5342");
            fprintf(stderr, "%zu", global_work_sizze_5576[0]);
            fprintf(stderr, "].\n");
            time_start_5577 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                              kernel_replicate_5342, 1, NULL,
                                              global_work_sizze_5576,
                                              local_work_sizze_5580, 0, NULL,
                                              NULL));
        if (debugging) {
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
            time_end_5578 = get_wall_time();
            
            long time_diff_5579 = time_end_5578 - time_start_5577;
            
            if (detail_timing) {
                kernel_replicate_5342total_runtime += time_diff_5579;
                kernel_replicate_5342runs++;
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "kernel_replicate_5342", (int) time_diff_5579);
            }
        }
    }
    
    struct memblock_device mem_5068;
    
    mem_5068.references = NULL;
    memblock_alloc_device(&mem_5068, 4);
    if (sizeof(int32_t) > 0) {
        OPENCL_SUCCEED(clEnqueueCopyBuffer(fut_cl_queue, static_array_5347.mem,
                                           mem_5068.mem, 0, 0, sizeof(int32_t),
                                           0, NULL, NULL));
        if (debugging)
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
    }
    
    struct memblock_device mem_5071;
    
    mem_5071.references = NULL;
    memblock_alloc_device(&mem_5071, 4);
    
    int32_t write_tmp_5582 = sizze_3871;
    
    OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue, mem_5071.mem, CL_TRUE, 0,
                                        sizeof(int32_t), &write_tmp_5582, 0,
                                        NULL, NULL));
    
    int32_t group_sizze_4188;
    
    group_sizze_4188 = cl_group_size;
    
    int32_t num_groups_4191 = squot32(group_sizze_4188, group_sizze_4188);
    int32_t num_threads_4192 = num_groups_4191 * group_sizze_4188;
    
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_4193, 0, sizeof(sizze_3871),
                                  &sizze_3871));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_4193, 1, sizeof(mem_5065.mem),
                                  &mem_5065.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_4193, 2, sizeof(mem_5068.mem),
                                  &mem_5068.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_4193, 3, sizeof(mem_5071.mem),
                                  &mem_5071.mem));
    if (1 * (num_groups_4191 * group_sizze_4188) != 0) {
        const size_t global_work_sizze_5583[1] = {num_groups_4191 *
                     group_sizze_4188};
        const size_t local_work_sizze_5587[1] = {group_sizze_4188};
        int64_t time_start_5584, time_end_5585;
        
        if (debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "map_kernel_4193");
            fprintf(stderr, "%zu", global_work_sizze_5583[0]);
            fprintf(stderr, "].\n");
            time_start_5584 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4193, 1,
                                              NULL, global_work_sizze_5583,
                                              local_work_sizze_5587, 0, NULL,
                                              NULL));
        if (debugging) {
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
            time_end_5585 = get_wall_time();
            
            long time_diff_5586 = time_end_5585 - time_start_5584;
            
            if (detail_timing) {
                map_kernel_4193total_runtime += time_diff_5586;
                map_kernel_4193runs++;
                fprintf(stderr, "kernel %s runtime: %ldus\n", "map_kernel_4193",
                        (int) time_diff_5586);
            }
        }
    }
    
    int32_t arg_3879 = sizze_3871 - 1;
    int32_t max_num_groups_4200;
    
    max_num_groups_4200 = cl_num_groups;
    
    int32_t y_4201 = group_sizze_4188 - 1;
    int32_t x_4202 = sizze_3871 + y_4201;
    int32_t w_div_group_sizze_4203 = squot32(x_4202, group_sizze_4188);
    int32_t num_groups_maybe_zzero_4204 = smin32(w_div_group_sizze_4203,
                                                 max_num_groups_4200);
    int32_t num_groups_4205 = smax32(1, num_groups_maybe_zzero_4204);
    int32_t num_threads_4206 = num_groups_4205 * group_sizze_4188;
    int32_t y_4229 = num_threads_4206 - 1;
    int32_t x_4230 = sizze_3871 + y_4229;
    int32_t num_iterations_4231 = squot32(x_4230, num_threads_4206);
    int32_t y_4234 = num_iterations_4231 * group_sizze_4188;
    int32_t num_threads_4283 = w_div_group_sizze_4203 * group_sizze_4188;
    int32_t x_5003 = arg_3879 + y_4201;
    int32_t w_div_group_sizze_5004 = squot32(x_5003, group_sizze_4188);
    int32_t num_groups_maybe_zzero_5005 = smin32(w_div_group_sizze_5004,
                                                 max_num_groups_4200);
    int32_t num_groups_5006 = smax32(1, num_groups_maybe_zzero_5005);
    int32_t num_threads_5007 = num_groups_5006 * group_sizze_4188;
    int32_t y_5008 = num_threads_5007 - 1;
    int32_t x_5009 = arg_3879 + y_5008;
    int32_t per_thread_elements_5010 = squot32(x_5009, num_threads_5007);
    struct memblock_device mem_5078;
    
    mem_5078.references = NULL;
    memblock_alloc_device(&mem_5078, bytes_5063);
    
    int64_t binop_x_5083 = sext_i32_i64(num_groups_4205);
    int64_t bytes_5082 = binop_x_5083 * 4;
    struct memblock_device mem_5084;
    
    mem_5084.references = NULL;
    memblock_alloc_device(&mem_5084, bytes_5082);
    
    int64_t binop_y_5080 = sext_i32_i64(group_sizze_4188);
    int64_t bytes_5079 = 4 * binop_y_5080;
    struct memblock_device mem_5090;
    
    mem_5090.references = NULL;
    memblock_alloc_device(&mem_5090, bytes_5082);
    
    int64_t bytes_5085 = 4 * binop_x_5083;
    struct memblock_device mem_5093;
    
    mem_5093.references = NULL;
    memblock_alloc_device(&mem_5093, bytes_5063);
    
    struct memblock_device mem_5096;
    
    mem_5096.references = NULL;
    memblock_alloc_device(&mem_5096, bytes_5063);
    
    struct memblock_device mem_5099;
    
    mem_5099.references = NULL;
    memblock_alloc_device(&mem_5099, bytes_5063);
    
    struct memblock_device mem_5108;
    
    mem_5108.references = NULL;
    memblock_alloc_device(&mem_5108, bytes_5082);
    
    struct memblock_device mem_5111;
    
    mem_5111.references = NULL;
    memblock_alloc_device(&mem_5111, bytes_5082);
    
    struct memblock_device mem_5120;
    
    mem_5120.references = NULL;
    memblock_alloc_device(&mem_5120, bytes_5082);
    
    struct memblock_device mem_5123;
    
    mem_5123.references = NULL;
    memblock_alloc_device(&mem_5123, bytes_5082);
    
    struct memblock_device mem_5126;
    
    mem_5126.references = NULL;
    memblock_alloc_device(&mem_5126, bytes_5063);
    
    struct memblock_device mem_5129;
    
    mem_5129.references = NULL;
    memblock_alloc_device(&mem_5129, bytes_5063);
    
    struct memblock_device mem_5132;
    
    mem_5132.references = NULL;
    memblock_alloc_device(&mem_5132, bytes_5063);
    
    struct memblock_device mem_5135;
    
    mem_5135.references = NULL;
    memblock_alloc_device(&mem_5135, bytes_5063);
    
    struct memblock_device mem_5138;
    
    mem_5138.references = NULL;
    memblock_alloc_device(&mem_5138, bytes_5063);
    
    struct memblock_device mem_5141;
    
    mem_5141.references = NULL;
    memblock_alloc_device(&mem_5141, bytes_5063);
    
    struct memblock_device mem_5144;
    
    mem_5144.references = NULL;
    memblock_alloc_device(&mem_5144, bytes_5063);
    
    struct memblock_device mem_5146;
    
    mem_5146.references = NULL;
    memblock_alloc_device(&mem_5146, binop_x_5064);
    
    struct memblock_device mem_5164;
    
    mem_5164.references = NULL;
    memblock_alloc_device(&mem_5164, bytes_5082);
    
    struct memblock_device mem_5167;
    
    mem_5167.references = NULL;
    memblock_alloc_device(&mem_5167, bytes_5082);
    
    struct memblock_device mem_5170;
    
    mem_5170.references = NULL;
    memblock_alloc_device(&mem_5170, bytes_5082);
    
    struct memblock_device mem_5173;
    
    mem_5173.references = NULL;
    memblock_alloc_device(&mem_5173, bytes_5082);
    
    struct memblock_device mem_5176;
    
    mem_5176.references = NULL;
    memblock_alloc_device(&mem_5176, bytes_5082);
    
    struct memblock_device mem_5194;
    
    mem_5194.references = NULL;
    memblock_alloc_device(&mem_5194, bytes_5082);
    
    struct memblock_device mem_5197;
    
    mem_5197.references = NULL;
    memblock_alloc_device(&mem_5197, bytes_5082);
    
    struct memblock_device mem_5200;
    
    mem_5200.references = NULL;
    memblock_alloc_device(&mem_5200, bytes_5082);
    
    struct memblock_device mem_5203;
    
    mem_5203.references = NULL;
    memblock_alloc_device(&mem_5203, bytes_5082);
    
    struct memblock_device mem_5206;
    
    mem_5206.references = NULL;
    memblock_alloc_device(&mem_5206, bytes_5082);
    
    struct memblock_device mem_5209;
    
    mem_5209.references = NULL;
    memblock_alloc_device(&mem_5209, bytes_5063);
    
    struct memblock_device mem_5212;
    
    mem_5212.references = NULL;
    memblock_alloc_device(&mem_5212, bytes_5063);
    
    struct memblock_device mem_5215;
    
    mem_5215.references = NULL;
    memblock_alloc_device(&mem_5215, bytes_5063);
    
    struct memblock_device mem_5218;
    
    mem_5218.references = NULL;
    memblock_alloc_device(&mem_5218, bytes_5063);
    
    struct memblock_device mem_5221;
    
    mem_5221.references = NULL;
    memblock_alloc_device(&mem_5221, bytes_5063);
    
    struct memblock_device mem_5224;
    
    mem_5224.references = NULL;
    memblock_alloc_device(&mem_5224, bytes_5063);
    
    struct memblock_device mem_5227;
    
    mem_5227.references = NULL;
    memblock_alloc_device(&mem_5227, bytes_5063);
    
    struct memblock_device mem_5230;
    
    mem_5230.references = NULL;
    memblock_alloc_device(&mem_5230, bytes_5063);
    
    struct memblock_device mem_5239;
    
    mem_5239.references = NULL;
    memblock_alloc_device(&mem_5239, bytes_5082);
    
    struct memblock_device mem_5242;
    
    mem_5242.references = NULL;
    memblock_alloc_device(&mem_5242, bytes_5082);
    
    struct memblock_device mem_5251;
    
    mem_5251.references = NULL;
    memblock_alloc_device(&mem_5251, bytes_5082);
    
    struct memblock_device mem_5254;
    
    mem_5254.references = NULL;
    memblock_alloc_device(&mem_5254, bytes_5082);
    
    struct memblock_device mem_5257;
    
    mem_5257.references = NULL;
    memblock_alloc_device(&mem_5257, bytes_5063);
    
    struct memblock_device mem_5260;
    
    mem_5260.references = NULL;
    memblock_alloc_device(&mem_5260, bytes_5063);
    
    struct memblock_device mem_5263;
    
    mem_5263.references = NULL;
    memblock_alloc_device(&mem_5263, bytes_5063);
    
    struct memblock_device mem_5266;
    
    mem_5266.references = NULL;
    memblock_alloc_device(&mem_5266, bytes_5063);
    
    struct memblock_device mem_5275;
    
    mem_5275.references = NULL;
    memblock_alloc_device(&mem_5275, bytes_5082);
    
    struct memblock_device mem_5278;
    
    mem_5278.references = NULL;
    memblock_alloc_device(&mem_5278, bytes_5082);
    
    struct memblock_device mem_5287;
    
    mem_5287.references = NULL;
    memblock_alloc_device(&mem_5287, bytes_5082);
    
    struct memblock_device mem_5290;
    
    mem_5290.references = NULL;
    memblock_alloc_device(&mem_5290, bytes_5082);
    
    struct memblock_device mem_5293;
    
    mem_5293.references = NULL;
    memblock_alloc_device(&mem_5293, bytes_5063);
    
    struct memblock_device mem_5296;
    
    mem_5296.references = NULL;
    memblock_alloc_device(&mem_5296, bytes_5063);
    
    int64_t bytes_5305 = sext_i32_i64(num_groups_5006);
    struct memblock_device mem_5306;
    
    mem_5306.references = NULL;
    memblock_alloc_device(&mem_5306, bytes_5305);
    
    struct memblock_device mem_5310;
    
    mem_5310.references = NULL;
    memblock_alloc_device(&mem_5310, 1);
    
    struct memblock_device double_buffer_mem_5315;
    
    double_buffer_mem_5315.references = NULL;
    memblock_alloc_device(&double_buffer_mem_5315, bytes_5063);
    
    struct memblock_device double_buffer_mem_5316;
    
    double_buffer_mem_5316.references = NULL;
    memblock_alloc_device(&double_buffer_mem_5316, bytes_5063);
    
    struct memblock_device mem_5299;
    
    mem_5299.references = NULL;
    memblock_alloc_device(&mem_5299, bytes_5063);
    
    struct memblock_device mem_5302;
    
    mem_5302.references = NULL;
    memblock_alloc_device(&mem_5302, bytes_5063);
    
    struct memblock_local mem_5081;
    
    mem_5081.references = NULL;
    
    struct memblock_local mem_5087;
    
    mem_5087.references = NULL;
    
    struct memblock_local mem_5102;
    
    mem_5102.references = NULL;
    
    struct memblock_local mem_5105;
    
    mem_5105.references = NULL;
    
    struct memblock_local mem_5114;
    
    mem_5114.references = NULL;
    
    struct memblock_local mem_5117;
    
    mem_5117.references = NULL;
    
    struct memblock_local mem_5149;
    
    mem_5149.references = NULL;
    
    struct memblock_local mem_5152;
    
    mem_5152.references = NULL;
    
    struct memblock_local mem_5155;
    
    mem_5155.references = NULL;
    
    struct memblock_local mem_5158;
    
    mem_5158.references = NULL;
    
    struct memblock_local mem_5161;
    
    mem_5161.references = NULL;
    
    struct memblock_local mem_5179;
    
    mem_5179.references = NULL;
    
    struct memblock_local mem_5182;
    
    mem_5182.references = NULL;
    
    struct memblock_local mem_5185;
    
    mem_5185.references = NULL;
    
    struct memblock_local mem_5188;
    
    mem_5188.references = NULL;
    
    struct memblock_local mem_5191;
    
    mem_5191.references = NULL;
    
    struct memblock_local mem_5233;
    
    mem_5233.references = NULL;
    
    struct memblock_local mem_5236;
    
    mem_5236.references = NULL;
    
    struct memblock_local mem_5245;
    
    mem_5245.references = NULL;
    
    struct memblock_local mem_5248;
    
    mem_5248.references = NULL;
    
    struct memblock_local mem_5269;
    
    mem_5269.references = NULL;
    
    struct memblock_local mem_5272;
    
    mem_5272.references = NULL;
    
    struct memblock_local mem_5281;
    
    mem_5281.references = NULL;
    
    struct memblock_local mem_5284;
    
    mem_5284.references = NULL;
    
    struct memblock_local mem_5304;
    
    mem_5304.references = NULL;
    
    struct memblock_local mem_5308;
    
    mem_5308.references = NULL;
    
    int64_t res_mem_sizze_5313;
    struct memblock_device res_mem_5312;
    
    res_mem_5312.references = NULL;
    
    struct memblock_device res_mem_5314;
    
    res_mem_5314.references = NULL;
    
    char res_3883;
    int32_t res_3886;
    char loop_while_3887;
    int32_t count_3890;
    struct memblock_device sizzes_mem_5073;
    
    sizzes_mem_5073.references = NULL;
    
    int64_t arr_mem_sizze_5074;
    struct memblock_device arr_mem_5075;
    
    arr_mem_5075.references = NULL;
    arr_mem_sizze_5074 = arr_mem_sizze_5061;
    memblock_set_device(&sizzes_mem_5073, &mem_5065);
    memblock_set_device(&arr_mem_5075, &arr_mem_5062);
    loop_while_3887 = 1;
    count_3890 = 0;
    while (loop_while_3887) {
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 0, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 1, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 2,
                                      sizeof(num_iterations_4231),
                                      &num_iterations_4231));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 3, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 4,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 5,
                                      sizeof(mem_5078.mem), &mem_5078.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4226, 6,
                                      sizeof(mem_5084.mem), &mem_5084.mem));
        if (1 * (num_groups_4205 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5588[1] = {num_groups_4205 *
                         group_sizze_4188};
            const size_t local_work_sizze_5592[1] = {group_sizze_4188};
            int64_t time_start_5589, time_end_5590;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan1_kernel_4226");
                fprintf(stderr, "%zu", global_work_sizze_5588[0]);
                fprintf(stderr, "].\n");
                time_start_5589 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan1_kernel_4226, 1, NULL,
                                                  global_work_sizze_5588,
                                                  local_work_sizze_5592, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5590 = get_wall_time();
                
                long time_diff_5591 = time_end_5590 - time_start_5589;
                
                if (detail_timing) {
                    scan1_kernel_4226total_runtime += time_diff_5591;
                    scan1_kernel_4226runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan1_kernel_4226", (int) time_diff_5591);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4258, 0, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4258, 1,
                                      sizeof(num_groups_4205),
                                      &num_groups_4205));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4258, 2,
                                      sizeof(mem_5084.mem), &mem_5084.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4258, 3,
                                      sizeof(mem_5090.mem), &mem_5090.mem));
        if (1 * num_groups_4205 != 0) {
            const size_t global_work_sizze_5593[1] = {num_groups_4205};
            const size_t local_work_sizze_5597[1] = {num_groups_4205};
            int64_t time_start_5594, time_end_5595;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan2_kernel_4258");
                fprintf(stderr, "%zu", global_work_sizze_5593[0]);
                fprintf(stderr, "].\n");
                time_start_5594 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan2_kernel_4258, 1, NULL,
                                                  global_work_sizze_5593,
                                                  local_work_sizze_5597, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5595 = get_wall_time();
                
                long time_diff_5596 = time_end_5595 - time_start_5594;
                
                if (detail_timing) {
                    scan2_kernel_4258total_runtime += time_diff_5596;
                    scan2_kernel_4258runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan2_kernel_4258", (int) time_diff_5596);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4284, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4284, 1, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4284, 2, sizeof(mem_5078.mem),
                                      &mem_5078.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4284, 3, sizeof(mem_5090.mem),
                                      &mem_5090.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4284, 4, sizeof(mem_5093.mem),
                                      &mem_5093.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5598[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5602[1] = {group_sizze_4188};
            int64_t time_start_5599, time_end_5600;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4284");
                fprintf(stderr, "%zu", global_work_sizze_5598[0]);
                fprintf(stderr, "].\n");
                time_start_5599 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4284,
                                                  1, NULL,
                                                  global_work_sizze_5598,
                                                  local_work_sizze_5602, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5600 = get_wall_time();
                
                long time_diff_5601 = time_end_5600 - time_start_5599;
                
                if (detail_timing) {
                    map_kernel_4284total_runtime += time_diff_5601;
                    map_kernel_4284runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4284", (int) time_diff_5601);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 0, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 1, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 2, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 3, sizeof(count_3890),
                                      &count_3890));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 4,
                                      sizeof(num_iterations_4231),
                                      &num_iterations_4231));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 5, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 6,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 7,
                                      sizeof(arr_mem_5075.mem),
                                      &arr_mem_5075.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 8,
                                      sizeof(mem_5093.mem), &mem_5093.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 9,
                                      sizeof(mem_5096.mem), &mem_5096.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 10,
                                      sizeof(mem_5099.mem), &mem_5099.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 11,
                                      sizeof(mem_5108.mem), &mem_5108.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4341, 12,
                                      sizeof(mem_5111.mem), &mem_5111.mem));
        if (1 * (num_groups_4205 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5603[1] = {num_groups_4205 *
                         group_sizze_4188};
            const size_t local_work_sizze_5607[1] = {group_sizze_4188};
            int64_t time_start_5604, time_end_5605;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan1_kernel_4341");
                fprintf(stderr, "%zu", global_work_sizze_5603[0]);
                fprintf(stderr, "].\n");
                time_start_5604 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan1_kernel_4341, 1, NULL,
                                                  global_work_sizze_5603,
                                                  local_work_sizze_5607, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5605 = get_wall_time();
                
                long time_diff_5606 = time_end_5605 - time_start_5604;
                
                if (detail_timing) {
                    scan1_kernel_4341total_runtime += time_diff_5606;
                    scan1_kernel_4341runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan1_kernel_4341", (int) time_diff_5606);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 0, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 1, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 2,
                                      sizeof(num_groups_4205),
                                      &num_groups_4205));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 3,
                                      sizeof(mem_5108.mem), &mem_5108.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 4,
                                      sizeof(mem_5111.mem), &mem_5111.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 5,
                                      sizeof(mem_5120.mem), &mem_5120.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4390, 6,
                                      sizeof(mem_5123.mem), &mem_5123.mem));
        if (1 * num_groups_4205 != 0) {
            const size_t global_work_sizze_5608[1] = {num_groups_4205};
            const size_t local_work_sizze_5612[1] = {num_groups_4205};
            int64_t time_start_5609, time_end_5610;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan2_kernel_4390");
                fprintf(stderr, "%zu", global_work_sizze_5608[0]);
                fprintf(stderr, "].\n");
                time_start_5609 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan2_kernel_4390, 1, NULL,
                                                  global_work_sizze_5608,
                                                  local_work_sizze_5612, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5610 = get_wall_time();
                
                long time_diff_5611 = time_end_5610 - time_start_5609;
                
                if (detail_timing) {
                    scan2_kernel_4390total_runtime += time_diff_5611;
                    scan2_kernel_4390runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan2_kernel_4390", (int) time_diff_5611);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 1, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 2, sizeof(mem_5096.mem),
                                      &mem_5096.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 3, sizeof(mem_5099.mem),
                                      &mem_5099.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 4, sizeof(mem_5120.mem),
                                      &mem_5120.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 5, sizeof(mem_5123.mem),
                                      &mem_5123.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 6, sizeof(mem_5126.mem),
                                      &mem_5126.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4428, 7, sizeof(mem_5129.mem),
                                      &mem_5129.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5613[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5617[1] = {group_sizze_4188};
            int64_t time_start_5614, time_end_5615;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4428");
                fprintf(stderr, "%zu", global_work_sizze_5613[0]);
                fprintf(stderr, "].\n");
                time_start_5614 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4428,
                                                  1, NULL,
                                                  global_work_sizze_5613,
                                                  local_work_sizze_5617, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5615 = get_wall_time();
                
                long time_diff_5616 = time_end_5615 - time_start_5614;
                
                if (detail_timing) {
                    map_kernel_4428total_runtime += time_diff_5616;
                    map_kernel_4428runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4428", (int) time_diff_5616);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 0, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 1, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 2, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 3, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 4, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 5, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 6,
                                      sizeof(num_iterations_4231),
                                      &num_iterations_4231));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 7, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 8,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 9,
                                      sizeof(arr_mem_5075.mem),
                                      &arr_mem_5075.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 10,
                                      sizeof(mem_5129.mem), &mem_5129.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 11,
                                      sizeof(mem_5132.mem), &mem_5132.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 12,
                                      sizeof(mem_5135.mem), &mem_5135.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 13,
                                      sizeof(mem_5138.mem), &mem_5138.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 14,
                                      sizeof(mem_5141.mem), &mem_5141.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 15,
                                      sizeof(mem_5144.mem), &mem_5144.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 16,
                                      sizeof(mem_5146.mem), &mem_5146.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 17,
                                      sizeof(mem_5164.mem), &mem_5164.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 18,
                                      sizeof(mem_5167.mem), &mem_5167.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 19,
                                      sizeof(mem_5170.mem), &mem_5170.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 20,
                                      sizeof(mem_5173.mem), &mem_5173.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4507, 21,
                                      sizeof(mem_5176.mem), &mem_5176.mem));
        if (1 * (num_groups_4205 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5618[1] = {num_groups_4205 *
                         group_sizze_4188};
            const size_t local_work_sizze_5622[1] = {group_sizze_4188};
            int64_t time_start_5619, time_end_5620;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan1_kernel_4507");
                fprintf(stderr, "%zu", global_work_sizze_5618[0]);
                fprintf(stderr, "].\n");
                time_start_5619 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan1_kernel_4507, 1, NULL,
                                                  global_work_sizze_5618,
                                                  local_work_sizze_5622, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5620 = get_wall_time();
                
                long time_diff_5621 = time_end_5620 - time_start_5619;
                
                if (detail_timing) {
                    scan1_kernel_4507total_runtime += time_diff_5621;
                    scan1_kernel_4507runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan1_kernel_4507", (int) time_diff_5621);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 0, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 1, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 2, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 3, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 4, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 5,
                                      sizeof(num_groups_4205),
                                      &num_groups_4205));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 6,
                                      sizeof(mem_5164.mem), &mem_5164.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 7,
                                      sizeof(mem_5167.mem), &mem_5167.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 8,
                                      sizeof(mem_5170.mem), &mem_5170.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 9,
                                      sizeof(mem_5173.mem), &mem_5173.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 10,
                                      sizeof(mem_5176.mem), &mem_5176.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 11,
                                      sizeof(mem_5194.mem), &mem_5194.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 12,
                                      sizeof(mem_5197.mem), &mem_5197.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 13,
                                      sizeof(mem_5200.mem), &mem_5200.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 14,
                                      sizeof(mem_5203.mem), &mem_5203.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4604, 15,
                                      sizeof(mem_5206.mem), &mem_5206.mem));
        if (1 * num_groups_4205 != 0) {
            const size_t global_work_sizze_5623[1] = {num_groups_4205};
            const size_t local_work_sizze_5627[1] = {num_groups_4205};
            int64_t time_start_5624, time_end_5625;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan2_kernel_4604");
                fprintf(stderr, "%zu", global_work_sizze_5623[0]);
                fprintf(stderr, "].\n");
                time_start_5624 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan2_kernel_4604, 1, NULL,
                                                  global_work_sizze_5623,
                                                  local_work_sizze_5627, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5625 = get_wall_time();
                
                long time_diff_5626 = time_end_5625 - time_start_5624;
                
                if (detail_timing) {
                    scan2_kernel_4604total_runtime += time_diff_5626;
                    scan2_kernel_4604runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan2_kernel_4604", (int) time_diff_5626);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 1, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 2, sizeof(mem_5132.mem),
                                      &mem_5132.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 3, sizeof(mem_5135.mem),
                                      &mem_5135.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 4, sizeof(mem_5138.mem),
                                      &mem_5138.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 5, sizeof(mem_5141.mem),
                                      &mem_5141.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 6, sizeof(mem_5144.mem),
                                      &mem_5144.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 7, sizeof(mem_5194.mem),
                                      &mem_5194.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 8, sizeof(mem_5197.mem),
                                      &mem_5197.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 9, sizeof(mem_5200.mem),
                                      &mem_5200.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 10, sizeof(mem_5203.mem),
                                      &mem_5203.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 11, sizeof(mem_5206.mem),
                                      &mem_5206.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 12, sizeof(mem_5209.mem),
                                      &mem_5209.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 13, sizeof(mem_5212.mem),
                                      &mem_5212.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 14, sizeof(mem_5215.mem),
                                      &mem_5215.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 15, sizeof(mem_5218.mem),
                                      &mem_5218.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4674, 16, sizeof(mem_5221.mem),
                                      &mem_5221.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5628[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5632[1] = {group_sizze_4188};
            int64_t time_start_5629, time_end_5630;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4674");
                fprintf(stderr, "%zu", global_work_sizze_5628[0]);
                fprintf(stderr, "].\n");
                time_start_5629 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4674,
                                                  1, NULL,
                                                  global_work_sizze_5628,
                                                  local_work_sizze_5632, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5630 = get_wall_time();
                
                long time_diff_5631 = time_end_5630 - time_start_5629;
                
                if (detail_timing) {
                    map_kernel_4674total_runtime += time_diff_5631;
                    map_kernel_4674runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4674", (int) time_diff_5631);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 0, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 1, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 2, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 3,
                                      sizeof(num_iterations_4231),
                                      &num_iterations_4231));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 4, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 5,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 6,
                                      sizeof(mem_5146.mem), &mem_5146.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 7,
                                      sizeof(mem_5209.mem), &mem_5209.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 8,
                                      sizeof(mem_5221.mem), &mem_5221.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 9,
                                      sizeof(mem_5224.mem), &mem_5224.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 10,
                                      sizeof(mem_5227.mem), &mem_5227.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 11,
                                      sizeof(mem_5230.mem), &mem_5230.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 12,
                                      sizeof(mem_5239.mem), &mem_5239.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4724, 13,
                                      sizeof(mem_5242.mem), &mem_5242.mem));
        if (1 * (num_groups_4205 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5633[1] = {num_groups_4205 *
                         group_sizze_4188};
            const size_t local_work_sizze_5637[1] = {group_sizze_4188};
            int64_t time_start_5634, time_end_5635;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan1_kernel_4724");
                fprintf(stderr, "%zu", global_work_sizze_5633[0]);
                fprintf(stderr, "].\n");
                time_start_5634 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan1_kernel_4724, 1, NULL,
                                                  global_work_sizze_5633,
                                                  local_work_sizze_5637, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5635 = get_wall_time();
                
                long time_diff_5636 = time_end_5635 - time_start_5634;
                
                if (detail_timing) {
                    scan1_kernel_4724total_runtime += time_diff_5636;
                    scan1_kernel_4724runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan1_kernel_4724", (int) time_diff_5636);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 0, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 1, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 2,
                                      sizeof(num_groups_4205),
                                      &num_groups_4205));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 3,
                                      sizeof(mem_5239.mem), &mem_5239.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 4,
                                      sizeof(mem_5242.mem), &mem_5242.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 5,
                                      sizeof(mem_5251.mem), &mem_5251.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4777, 6,
                                      sizeof(mem_5254.mem), &mem_5254.mem));
        if (1 * num_groups_4205 != 0) {
            const size_t global_work_sizze_5638[1] = {num_groups_4205};
            const size_t local_work_sizze_5642[1] = {num_groups_4205};
            int64_t time_start_5639, time_end_5640;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan2_kernel_4777");
                fprintf(stderr, "%zu", global_work_sizze_5638[0]);
                fprintf(stderr, "].\n");
                time_start_5639 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan2_kernel_4777, 1, NULL,
                                                  global_work_sizze_5638,
                                                  local_work_sizze_5642, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5640 = get_wall_time();
                
                long time_diff_5641 = time_end_5640 - time_start_5639;
                
                if (detail_timing) {
                    scan2_kernel_4777total_runtime += time_diff_5641;
                    scan2_kernel_4777runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan2_kernel_4777", (int) time_diff_5641);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 1, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 2, sizeof(mem_5224.mem),
                                      &mem_5224.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 3, sizeof(mem_5227.mem),
                                      &mem_5227.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 4, sizeof(mem_5251.mem),
                                      &mem_5251.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 5, sizeof(mem_5254.mem),
                                      &mem_5254.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 6, sizeof(mem_5257.mem),
                                      &mem_5257.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4815, 7, sizeof(mem_5260.mem),
                                      &mem_5260.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5643[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5647[1] = {group_sizze_4188};
            int64_t time_start_5644, time_end_5645;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4815");
                fprintf(stderr, "%zu", global_work_sizze_5643[0]);
                fprintf(stderr, "].\n");
                time_start_5644 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4815,
                                                  1, NULL,
                                                  global_work_sizze_5643,
                                                  local_work_sizze_5647, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5645 = get_wall_time();
                
                long time_diff_5646 = time_end_5645 - time_start_5644;
                
                if (detail_timing) {
                    map_kernel_4815total_runtime += time_diff_5646;
                    map_kernel_4815runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4815", (int) time_diff_5646);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 0, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 1, bytes_5079, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 2, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 3,
                                      sizeof(num_iterations_4231),
                                      &num_iterations_4231));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 4, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 5,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 6,
                                      sizeof(mem_5263.mem), &mem_5263.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 7,
                                      sizeof(mem_5266.mem), &mem_5266.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 8,
                                      sizeof(mem_5275.mem), &mem_5275.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_4867, 9,
                                      sizeof(mem_5278.mem), &mem_5278.mem));
        if (1 * (num_groups_4205 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5648[1] = {num_groups_4205 *
                         group_sizze_4188};
            const size_t local_work_sizze_5652[1] = {group_sizze_4188};
            int64_t time_start_5649, time_end_5650;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan1_kernel_4867");
                fprintf(stderr, "%zu", global_work_sizze_5648[0]);
                fprintf(stderr, "].\n");
                time_start_5649 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan1_kernel_4867, 1, NULL,
                                                  global_work_sizze_5648,
                                                  local_work_sizze_5652, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5650 = get_wall_time();
                
                long time_diff_5651 = time_end_5650 - time_start_5649;
                
                if (detail_timing) {
                    scan1_kernel_4867total_runtime += time_diff_5651;
                    scan1_kernel_4867runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan1_kernel_4867", (int) time_diff_5651);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 0, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 1, bytes_5085, NULL));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 2,
                                      sizeof(num_groups_4205),
                                      &num_groups_4205));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 3,
                                      sizeof(mem_5275.mem), &mem_5275.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 4,
                                      sizeof(mem_5278.mem), &mem_5278.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 5,
                                      sizeof(mem_5287.mem), &mem_5287.mem));
        OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_4916, 6,
                                      sizeof(mem_5290.mem), &mem_5290.mem));
        if (1 * num_groups_4205 != 0) {
            const size_t global_work_sizze_5653[1] = {num_groups_4205};
            const size_t local_work_sizze_5657[1] = {num_groups_4205};
            int64_t time_start_5654, time_end_5655;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan2_kernel_4916");
                fprintf(stderr, "%zu", global_work_sizze_5653[0]);
                fprintf(stderr, "].\n");
                time_start_5654 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  scan2_kernel_4916, 1, NULL,
                                                  global_work_sizze_5653,
                                                  local_work_sizze_5657, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5655 = get_wall_time();
                
                long time_diff_5656 = time_end_5655 - time_start_5654;
                
                if (detail_timing) {
                    scan2_kernel_4916total_runtime += time_diff_5656;
                    scan2_kernel_4916runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "scan2_kernel_4916", (int) time_diff_5656);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 1, sizeof(y_4234),
                                      &y_4234));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 2, sizeof(mem_5263.mem),
                                      &mem_5263.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 3, sizeof(mem_5266.mem),
                                      &mem_5266.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 4, sizeof(mem_5287.mem),
                                      &mem_5287.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 5, sizeof(mem_5290.mem),
                                      &mem_5290.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 6, sizeof(mem_5293.mem),
                                      &mem_5293.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4954, 7, sizeof(mem_5296.mem),
                                      &mem_5296.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5658[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5662[1] = {group_sizze_4188};
            int64_t time_start_5659, time_end_5660;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4954");
                fprintf(stderr, "%zu", global_work_sizze_5658[0]);
                fprintf(stderr, "].\n");
                time_start_5659 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4954,
                                                  1, NULL,
                                                  global_work_sizze_5658,
                                                  local_work_sizze_5662, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5660 = get_wall_time();
                
                long time_diff_5661 = time_end_5660 - time_start_5659;
                
                if (detail_timing) {
                    map_kernel_4954total_runtime += time_diff_5661;
                    map_kernel_4954runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4954", (int) time_diff_5661);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 1,
                                      sizeof(sizzes_mem_5073.mem),
                                      &sizzes_mem_5073.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 2, sizeof(mem_5215.mem),
                                      &mem_5215.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 3, sizeof(mem_5230.mem),
                                      &mem_5230.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 4, sizeof(mem_5296.mem),
                                      &mem_5296.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4964, 5, sizeof(mem_5299.mem),
                                      &mem_5299.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5663[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5667[1] = {group_sizze_4188};
            int64_t time_start_5664, time_end_5665;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4964");
                fprintf(stderr, "%zu", global_work_sizze_5663[0]);
                fprintf(stderr, "].\n");
                time_start_5664 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4964,
                                                  1, NULL,
                                                  global_work_sizze_5663,
                                                  local_work_sizze_5667, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5665 = get_wall_time();
                
                long time_diff_5666 = time_end_5665 - time_start_5664;
                
                if (detail_timing) {
                    map_kernel_4964total_runtime += time_diff_5666;
                    map_kernel_4964runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4964", (int) time_diff_5666);
                }
            }
        }
        if (sizze_3871 * sizeof(float) > 0) {
            OPENCL_SUCCEED(clEnqueueCopyBuffer(fut_cl_queue, arr_mem_5075.mem,
                                               mem_5302.mem, 0, 0, sizze_3871 *
                                               sizeof(float), 0, NULL, NULL));
            if (debugging)
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
        }
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 0, sizeof(sizze_3871),
                                      &sizze_3871));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 1,
                                      sizeof(arr_mem_5075.mem),
                                      &arr_mem_5075.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 2, sizeof(mem_5146.mem),
                                      &mem_5146.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 3, sizeof(mem_5209.mem),
                                      &mem_5209.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 4, sizeof(mem_5215.mem),
                                      &mem_5215.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 5, sizeof(mem_5221.mem),
                                      &mem_5221.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 6, sizeof(mem_5230.mem),
                                      &mem_5230.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 7, sizeof(mem_5260.mem),
                                      &mem_5260.mem));
        OPENCL_SUCCEED(clSetKernelArg(map_kernel_4990, 8, sizeof(mem_5302.mem),
                                      &mem_5302.mem));
        if (1 * (w_div_group_sizze_4203 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5668[1] = {w_div_group_sizze_4203 *
                         group_sizze_4188};
            const size_t local_work_sizze_5672[1] = {group_sizze_4188};
            int64_t time_start_5669, time_end_5670;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "map_kernel_4990");
                fprintf(stderr, "%zu", global_work_sizze_5668[0]);
                fprintf(stderr, "].\n");
                time_start_5669 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_4990,
                                                  1, NULL,
                                                  global_work_sizze_5668,
                                                  local_work_sizze_5672, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5670 = get_wall_time();
                
                long time_diff_5671 = time_end_5670 - time_start_5669;
                
                if (detail_timing) {
                    map_kernel_4990total_runtime += time_diff_5671;
                    map_kernel_4990runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "map_kernel_4990", (int) time_diff_5671);
                }
            }
        }
        if (debugging) {
            int binary_output = 0;
            int32_t x_5673 = arg_3879;
            
            fprintf(stderr, "%s: ", "input size");
            write_scalar(stderr, binary_output, &i32, &x_5673);
            fprintf(stderr, "\n");
        }
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 0,
                                      binop_y_5080, NULL));
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 1,
                                      sizeof(arg_3879), &arg_3879));
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 2,
                                      sizeof(num_threads_5007),
                                      &num_threads_5007));
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 3,
                                      sizeof(per_thread_elements_5010),
                                      &per_thread_elements_5010));
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 4,
                                      sizeof(mem_5302.mem), &mem_5302.mem));
        OPENCL_SUCCEED(clSetKernelArg(chunked_reduce_kernel_5015, 5,
                                      sizeof(mem_5306.mem), &mem_5306.mem));
        if (1 * (num_groups_5006 * group_sizze_4188) != 0) {
            const size_t global_work_sizze_5674[1] = {num_groups_5006 *
                         group_sizze_4188};
            const size_t local_work_sizze_5678[1] = {group_sizze_4188};
            int64_t time_start_5675, time_end_5676;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "chunked_reduce_kernel_5015");
                fprintf(stderr, "%zu", global_work_sizze_5674[0]);
                fprintf(stderr, "].\n");
                time_start_5675 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  chunked_reduce_kernel_5015, 1,
                                                  NULL, global_work_sizze_5674,
                                                  local_work_sizze_5678, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5676 = get_wall_time();
                
                long time_diff_5677 = time_end_5676 - time_start_5675;
                
                if (detail_timing) {
                    chunked_reduce_kernel_5015total_runtime += time_diff_5677;
                    chunked_reduce_kernel_5015runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "chunked_reduce_kernel_5015", (int) time_diff_5677);
                }
            }
        }
        OPENCL_SUCCEED(clSetKernelArg(reduce_kernel_5046, 0, binop_y_5080,
                                      NULL));
        OPENCL_SUCCEED(clSetKernelArg(reduce_kernel_5046, 1,
                                      sizeof(num_groups_5006),
                                      &num_groups_5006));
        OPENCL_SUCCEED(clSetKernelArg(reduce_kernel_5046, 2,
                                      sizeof(mem_5306.mem), &mem_5306.mem));
        OPENCL_SUCCEED(clSetKernelArg(reduce_kernel_5046, 3,
                                      sizeof(mem_5310.mem), &mem_5310.mem));
        if (1 * group_sizze_4188 != 0) {
            const size_t global_work_sizze_5679[1] = {group_sizze_4188};
            const size_t local_work_sizze_5683[1] = {group_sizze_4188};
            int64_t time_start_5680, time_end_5681;
            
            if (debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "reduce_kernel_5046");
                fprintf(stderr, "%zu", global_work_sizze_5679[0]);
                fprintf(stderr, "].\n");
                time_start_5680 = get_wall_time();
            }
            OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue,
                                                  reduce_kernel_5046, 1, NULL,
                                                  global_work_sizze_5679,
                                                  local_work_sizze_5683, 0,
                                                  NULL, NULL));
            if (debugging) {
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
                time_end_5681 = get_wall_time();
                
                long time_diff_5682 = time_end_5681 - time_start_5680;
                
                if (detail_timing) {
                    reduce_kernel_5046total_runtime += time_diff_5682;
                    reduce_kernel_5046runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "reduce_kernel_5046", (int) time_diff_5682);
                }
            }
        }
        
        char read_res_5684;
        
        OPENCL_SUCCEED(clEnqueueReadBuffer(fut_cl_queue, mem_5310.mem, CL_TRUE,
                                           0, sizeof(char), &read_res_5684, 0,
                                           NULL, NULL));
        
        char res_4162 = read_res_5684;
        int32_t res_4174 = count_3890 + 1;
        char loop_cond_4175 = !res_4162;
        
        if (sizze_3871 * sizeof(int32_t) > 0) {
            OPENCL_SUCCEED(clEnqueueCopyBuffer(fut_cl_queue, mem_5299.mem,
                                               double_buffer_mem_5315.mem, 0, 0,
                                               sizze_3871 * sizeof(int32_t), 0,
                                               NULL, NULL));
            if (debugging)
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
        }
        if (sizze_3871 * sizeof(float) > 0) {
            OPENCL_SUCCEED(clEnqueueCopyBuffer(fut_cl_queue, mem_5302.mem,
                                               double_buffer_mem_5316.mem, 0, 0,
                                               sizze_3871 * sizeof(float), 0,
                                               NULL, NULL));
            if (debugging)
                OPENCL_SUCCEED(clFinish(fut_cl_queue));
        }
        
        int64_t arr_mem_sizze_tmp_5351 = bytes_5063;
        struct memblock_device sizzes_mem_tmp_5352;
        
        sizzes_mem_tmp_5352.references = NULL;
        memblock_set_device(&sizzes_mem_tmp_5352, &double_buffer_mem_5315);
        
        struct memblock_device arr_mem_tmp_5353;
        
        arr_mem_tmp_5353.references = NULL;
        memblock_set_device(&arr_mem_tmp_5353, &double_buffer_mem_5316);
        
        char loop_while_tmp_5354 = loop_cond_4175;
        int32_t count_tmp_5357;
        
        count_tmp_5357 = res_4174;
        arr_mem_sizze_5074 = arr_mem_sizze_tmp_5351;
        memblock_set_device(&sizzes_mem_5073, &sizzes_mem_tmp_5352);
        memblock_set_device(&arr_mem_5075, &arr_mem_tmp_5353);
        loop_while_3887 = loop_while_tmp_5354;
        count_3890 = count_tmp_5357;
        memblock_unref_device(&sizzes_mem_tmp_5352);
        memblock_unref_device(&arr_mem_tmp_5353);
    }
    res_3883 = loop_while_3887;
    memblock_set_device(&res_mem_5312, &sizzes_mem_5073);
    memblock_set_device(&res_mem_5314, &arr_mem_5075);
    res_mem_sizze_5313 = arr_mem_sizze_5074;
    res_3886 = count_3890;
    memblock_set_device(&out_mem_5339, &res_mem_5314);
    out_arrsizze_5341 = sizze_3871;
    out_memsizze_5340 = res_mem_sizze_5313;
    
    struct tuple_int32_t_device_mem_int32_t retval_5575;
    
    retval_5575.elem_0 = out_memsizze_5340;
    retval_5575.elem_1.references = NULL;
    memblock_set_device(&retval_5575.elem_1, &out_mem_5339);
    retval_5575.elem_2 = out_arrsizze_5341;
    memblock_unref_device(&out_mem_5339);
    memblock_unref_device(&mem_5065);
    memblock_unref_device(&mem_5068);
    memblock_unref_device(&mem_5071);
    memblock_unref_device(&mem_5078);
    memblock_unref_device(&mem_5084);
    memblock_unref_device(&mem_5090);
    memblock_unref_device(&mem_5093);
    memblock_unref_device(&mem_5096);
    memblock_unref_device(&mem_5099);
    memblock_unref_device(&mem_5108);
    memblock_unref_device(&mem_5111);
    memblock_unref_device(&mem_5120);
    memblock_unref_device(&mem_5123);
    memblock_unref_device(&mem_5126);
    memblock_unref_device(&mem_5129);
    memblock_unref_device(&mem_5132);
    memblock_unref_device(&mem_5135);
    memblock_unref_device(&mem_5138);
    memblock_unref_device(&mem_5141);
    memblock_unref_device(&mem_5144);
    memblock_unref_device(&mem_5146);
    memblock_unref_device(&mem_5164);
    memblock_unref_device(&mem_5167);
    memblock_unref_device(&mem_5170);
    memblock_unref_device(&mem_5173);
    memblock_unref_device(&mem_5176);
    memblock_unref_device(&mem_5194);
    memblock_unref_device(&mem_5197);
    memblock_unref_device(&mem_5200);
    memblock_unref_device(&mem_5203);
    memblock_unref_device(&mem_5206);
    memblock_unref_device(&mem_5209);
    memblock_unref_device(&mem_5212);
    memblock_unref_device(&mem_5215);
    memblock_unref_device(&mem_5218);
    memblock_unref_device(&mem_5221);
    memblock_unref_device(&mem_5224);
    memblock_unref_device(&mem_5227);
    memblock_unref_device(&mem_5230);
    memblock_unref_device(&mem_5239);
    memblock_unref_device(&mem_5242);
    memblock_unref_device(&mem_5251);
    memblock_unref_device(&mem_5254);
    memblock_unref_device(&mem_5257);
    memblock_unref_device(&mem_5260);
    memblock_unref_device(&mem_5263);
    memblock_unref_device(&mem_5266);
    memblock_unref_device(&mem_5275);
    memblock_unref_device(&mem_5278);
    memblock_unref_device(&mem_5287);
    memblock_unref_device(&mem_5290);
    memblock_unref_device(&mem_5293);
    memblock_unref_device(&mem_5296);
    memblock_unref_device(&mem_5306);
    memblock_unref_device(&mem_5310);
    memblock_unref_device(&double_buffer_mem_5315);
    memblock_unref_device(&double_buffer_mem_5316);
    memblock_unref_device(&mem_5299);
    memblock_unref_device(&mem_5302);
    memblock_unref_local(&mem_5081);
    memblock_unref_local(&mem_5087);
    memblock_unref_local(&mem_5102);
    memblock_unref_local(&mem_5105);
    memblock_unref_local(&mem_5114);
    memblock_unref_local(&mem_5117);
    memblock_unref_local(&mem_5149);
    memblock_unref_local(&mem_5152);
    memblock_unref_local(&mem_5155);
    memblock_unref_local(&mem_5158);
    memblock_unref_local(&mem_5161);
    memblock_unref_local(&mem_5179);
    memblock_unref_local(&mem_5182);
    memblock_unref_local(&mem_5185);
    memblock_unref_local(&mem_5188);
    memblock_unref_local(&mem_5191);
    memblock_unref_local(&mem_5233);
    memblock_unref_local(&mem_5236);
    memblock_unref_local(&mem_5245);
    memblock_unref_local(&mem_5248);
    memblock_unref_local(&mem_5269);
    memblock_unref_local(&mem_5272);
    memblock_unref_local(&mem_5281);
    memblock_unref_local(&mem_5284);
    memblock_unref_local(&mem_5304);
    memblock_unref_local(&mem_5308);
    memblock_unref_device(&res_mem_5312);
    memblock_unref_device(&res_mem_5314);
    memblock_unref_device(&sizzes_mem_5073);
    memblock_unref_device(&arr_mem_5075);
    return retval_5575;
}
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
int parse_options(int argc, char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"memory-reporting", no_argument,
                                            NULL, 3}, {"entry-point",
                                                       required_argument, NULL,
                                                       4}, {"binary-output",
                                                            no_argument, NULL,
                                                            5}, {"platform",
                                                                 required_argument,
                                                                 NULL, 6},
                                           {"device", required_argument, NULL,
                                            7}, {"synchronous", no_argument,
                                                 NULL, 8}, {"group-size",
                                                            required_argument,
                                                            NULL, 9},
                                           {"num-groups", required_argument,
                                            NULL, 10}, {"dump-opencl",
                                                        required_argument, NULL,
                                                        11}, {"load-opencl",
                                                              required_argument,
                                                              NULL, 12}, {0, 0,
                                                                          0,
                                                                          0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:me:bp:d:s", long_options,
                             NULL)) != -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                panic(1, "Need a positive number of runs, not %s\n", optarg);
        }
        if (ch == 3 || ch == 'm')
            detail_memory = 1;
        if (ch == 4 || ch == 'e')
            entry_point = optarg;
        if (ch == 5 || ch == 'b')
            binary_output = 1;
        if (ch == 6 || ch == 'p')
            set_preferred_platform(optarg);
        if (ch == 7 || ch == 'd')
            set_preferred_device(optarg);
        if (ch == 8 || ch == 's')
            cl_debug = debugging = 1;
        if (ch == 9)
            cl_group_size = atoi(optarg);
        if (ch == 10)
            cl_num_groups = atoi(optarg);
        if (ch == 11)
            cl_dump_program_to = optarg;
        if (ch == 12)
            cl_load_program_from = optarg;
        if (ch == ':')
            panic(-1, "Missing argument for option %s\n", argv[optind - 1]);
        if (ch == '?')
            panic(-1, "Unknown option %s\n", argv[optind - 1]);
    }
    return optind;
}
void entry_main()
{
    int64_t t_start, t_end;
    int time_runs;
    int64_t arr_mem_sizze_5061;
    struct memblock arr_mem_5062;
    
    arr_mem_5062.references = NULL;
    memblock_alloc(&arr_mem_5062, 0);
    
    int32_t sizze_3871;
    struct tuple_int32_t_device_mem_int32_t main_ret_5685;
    
    {
        int64_t shape[1];
        
        errno = 0;
        if (read_array(&f32, (void **) &arr_mem_5062.mem, shape, 1) != 0)
            panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
                  f32.type_name, strerror(errno));
        sizze_3871 = shape[0];
        arr_mem_sizze_5061 = sizeof(float) * shape[0];
        arr_mem_5062.size = arr_mem_sizze_5061;
    }
    
    int32_t out_memsizze_5340;
    struct memblock out_mem_5339;
    
    out_mem_5339.references = NULL;
    
    int32_t out_arrsizze_5341;
    
    if (perform_warmup) {
        time_runs = 0;
        
        struct memblock_device arr_mem_copy_5686;
        
        arr_mem_copy_5686.references = NULL;
        memblock_alloc_device(&arr_mem_copy_5686, arr_mem_5062.size);
        if (arr_mem_5062.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                arr_mem_copy_5686.mem, CL_TRUE,
                                                0, arr_mem_5062.size,
                                                arr_mem_5062.mem + 0, 0, NULL,
                                                NULL));
        t_start = get_wall_time();
        main_ret_5685 = futhark_main(arr_mem_sizze_5061, arr_mem_copy_5686,
                                     sizze_3871);
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        memblock_unref_device(&main_ret_5685.elem_1);
        memblock_unref_device(&arr_mem_copy_5686);
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        if (run == num_runs - 1)
            detail_timing = 1;
        
        struct memblock_device arr_mem_copy_5686;
        
        arr_mem_copy_5686.references = NULL;
        memblock_alloc_device(&arr_mem_copy_5686, arr_mem_5062.size);
        if (arr_mem_5062.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                arr_mem_copy_5686.mem, CL_TRUE,
                                                0, arr_mem_5062.size,
                                                arr_mem_5062.mem + 0, 0, NULL,
                                                NULL));
        t_start = get_wall_time();
        main_ret_5685 = futhark_main(arr_mem_sizze_5061, arr_mem_copy_5686,
                                     sizze_3871);
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        if (run < num_runs - 1) {
            memblock_unref_device(&main_ret_5685.elem_1);
            memblock_unref_device(&arr_mem_copy_5686);
        }
    }
    memblock_unref(&arr_mem_5062);
    out_memsizze_5340 = main_ret_5685.elem_0;
    memblock_alloc(&out_mem_5339, main_ret_5685.elem_1.size);
    if (main_ret_5685.elem_1.size > 0)
        OPENCL_SUCCEED(clEnqueueReadBuffer(fut_cl_queue,
                                           main_ret_5685.elem_1.mem, CL_TRUE, 0,
                                           main_ret_5685.elem_1.size,
                                           out_mem_5339.mem + 0, 0, NULL,
                                           NULL));
    out_arrsizze_5341 = main_ret_5685.elem_2;
    {
        int64_t shape[] = {out_arrsizze_5341};
        
        write_array(stdout, binary_output, &f32, out_mem_5339.mem, shape, 1);
    }
    printf("\n");
    memblock_unref_device(&main_ret_5685.elem_1);
}
typedef void entry_point_fun();
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
} ;
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                entry_main}};
    int parsed_options = parse_options(argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    setup_opencl_and_load_kernels();
    {
        cl_int success;
        
        static_array_5347.references = NULL;
        static_array_5347.size = 0;
        static_array_5347.mem = clCreateBuffer(fut_cl_context,
                                               CL_MEM_READ_WRITE, (1 >
                                                                   0 ? 1 : 1) *
                                               sizeof(int32_t), NULL, &success);
        OPENCL_SUCCEED(success);
        if (1 > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                static_array_5347.mem, CL_TRUE,
                                                0, 1 * sizeof(int32_t),
                                                static_array_realtype_5581, 0,
                                                NULL, NULL));
    }
    
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    
    for (int i = 0; i < num_entry_points; i++) {
        if (strcmp(entry_points[i].name, entry_point) == 0) {
            entry_point_fun = entry_points[i].fun;
            break;
        }
    }
    if (entry_point_fun == NULL) {
        fprintf(stderr,
                "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                entry_point);
        for (int i = 0; i < num_entry_points; i++)
            fprintf(stderr, "%s\n", entry_points[i].name);
        return 1;
    }
    entry_point_fun();
    if (runtime_file != NULL)
        fclose(runtime_file);
    
    int total_runtime = 0;
    int total_runs = 0;
    
    if (debugging) {
        fprintf(stderr,
                "Kernel chunked_reduce_kernel_5015 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                chunked_reduce_kernel_5015runs,
                (long) chunked_reduce_kernel_5015total_runtime /
                (chunked_reduce_kernel_5015runs !=
                 0 ? chunked_reduce_kernel_5015runs : 1),
                (long) chunked_reduce_kernel_5015total_runtime);
        total_runtime += chunked_reduce_kernel_5015total_runtime;
        total_runs += chunked_reduce_kernel_5015runs;
        fprintf(stderr,
                "Kernel kernel_replicate_5342      executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                kernel_replicate_5342runs,
                (long) kernel_replicate_5342total_runtime /
                (kernel_replicate_5342runs !=
                 0 ? kernel_replicate_5342runs : 1),
                (long) kernel_replicate_5342total_runtime);
        total_runtime += kernel_replicate_5342total_runtime;
        total_runs += kernel_replicate_5342runs;
        fprintf(stderr,
                "Kernel map_kernel_4193            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4193runs, (long) map_kernel_4193total_runtime /
                (map_kernel_4193runs != 0 ? map_kernel_4193runs : 1),
                (long) map_kernel_4193total_runtime);
        total_runtime += map_kernel_4193total_runtime;
        total_runs += map_kernel_4193runs;
        fprintf(stderr,
                "Kernel map_kernel_4284            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4284runs, (long) map_kernel_4284total_runtime /
                (map_kernel_4284runs != 0 ? map_kernel_4284runs : 1),
                (long) map_kernel_4284total_runtime);
        total_runtime += map_kernel_4284total_runtime;
        total_runs += map_kernel_4284runs;
        fprintf(stderr,
                "Kernel map_kernel_4428            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4428runs, (long) map_kernel_4428total_runtime /
                (map_kernel_4428runs != 0 ? map_kernel_4428runs : 1),
                (long) map_kernel_4428total_runtime);
        total_runtime += map_kernel_4428total_runtime;
        total_runs += map_kernel_4428runs;
        fprintf(stderr,
                "Kernel map_kernel_4674            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4674runs, (long) map_kernel_4674total_runtime /
                (map_kernel_4674runs != 0 ? map_kernel_4674runs : 1),
                (long) map_kernel_4674total_runtime);
        total_runtime += map_kernel_4674total_runtime;
        total_runs += map_kernel_4674runs;
        fprintf(stderr,
                "Kernel map_kernel_4815            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4815runs, (long) map_kernel_4815total_runtime /
                (map_kernel_4815runs != 0 ? map_kernel_4815runs : 1),
                (long) map_kernel_4815total_runtime);
        total_runtime += map_kernel_4815total_runtime;
        total_runs += map_kernel_4815runs;
        fprintf(stderr,
                "Kernel map_kernel_4954            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4954runs, (long) map_kernel_4954total_runtime /
                (map_kernel_4954runs != 0 ? map_kernel_4954runs : 1),
                (long) map_kernel_4954total_runtime);
        total_runtime += map_kernel_4954total_runtime;
        total_runs += map_kernel_4954runs;
        fprintf(stderr,
                "Kernel map_kernel_4964            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4964runs, (long) map_kernel_4964total_runtime /
                (map_kernel_4964runs != 0 ? map_kernel_4964runs : 1),
                (long) map_kernel_4964total_runtime);
        total_runtime += map_kernel_4964total_runtime;
        total_runs += map_kernel_4964runs;
        fprintf(stderr,
                "Kernel map_kernel_4990            executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_4990runs, (long) map_kernel_4990total_runtime /
                (map_kernel_4990runs != 0 ? map_kernel_4990runs : 1),
                (long) map_kernel_4990total_runtime);
        total_runtime += map_kernel_4990total_runtime;
        total_runs += map_kernel_4990runs;
        fprintf(stderr,
                "Kernel reduce_kernel_5046         executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                reduce_kernel_5046runs, (long) reduce_kernel_5046total_runtime /
                (reduce_kernel_5046runs != 0 ? reduce_kernel_5046runs : 1),
                (long) reduce_kernel_5046total_runtime);
        total_runtime += reduce_kernel_5046total_runtime;
        total_runs += reduce_kernel_5046runs;
        fprintf(stderr,
                "Kernel scan1_kernel_4226          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_4226runs, (long) scan1_kernel_4226total_runtime /
                (scan1_kernel_4226runs != 0 ? scan1_kernel_4226runs : 1),
                (long) scan1_kernel_4226total_runtime);
        total_runtime += scan1_kernel_4226total_runtime;
        total_runs += scan1_kernel_4226runs;
        fprintf(stderr,
                "Kernel scan1_kernel_4341          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_4341runs, (long) scan1_kernel_4341total_runtime /
                (scan1_kernel_4341runs != 0 ? scan1_kernel_4341runs : 1),
                (long) scan1_kernel_4341total_runtime);
        total_runtime += scan1_kernel_4341total_runtime;
        total_runs += scan1_kernel_4341runs;
        fprintf(stderr,
                "Kernel scan1_kernel_4507          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_4507runs, (long) scan1_kernel_4507total_runtime /
                (scan1_kernel_4507runs != 0 ? scan1_kernel_4507runs : 1),
                (long) scan1_kernel_4507total_runtime);
        total_runtime += scan1_kernel_4507total_runtime;
        total_runs += scan1_kernel_4507runs;
        fprintf(stderr,
                "Kernel scan1_kernel_4724          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_4724runs, (long) scan1_kernel_4724total_runtime /
                (scan1_kernel_4724runs != 0 ? scan1_kernel_4724runs : 1),
                (long) scan1_kernel_4724total_runtime);
        total_runtime += scan1_kernel_4724total_runtime;
        total_runs += scan1_kernel_4724runs;
        fprintf(stderr,
                "Kernel scan1_kernel_4867          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_4867runs, (long) scan1_kernel_4867total_runtime /
                (scan1_kernel_4867runs != 0 ? scan1_kernel_4867runs : 1),
                (long) scan1_kernel_4867total_runtime);
        total_runtime += scan1_kernel_4867total_runtime;
        total_runs += scan1_kernel_4867runs;
        fprintf(stderr,
                "Kernel scan2_kernel_4258          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_4258runs, (long) scan2_kernel_4258total_runtime /
                (scan2_kernel_4258runs != 0 ? scan2_kernel_4258runs : 1),
                (long) scan2_kernel_4258total_runtime);
        total_runtime += scan2_kernel_4258total_runtime;
        total_runs += scan2_kernel_4258runs;
        fprintf(stderr,
                "Kernel scan2_kernel_4390          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_4390runs, (long) scan2_kernel_4390total_runtime /
                (scan2_kernel_4390runs != 0 ? scan2_kernel_4390runs : 1),
                (long) scan2_kernel_4390total_runtime);
        total_runtime += scan2_kernel_4390total_runtime;
        total_runs += scan2_kernel_4390runs;
        fprintf(stderr,
                "Kernel scan2_kernel_4604          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_4604runs, (long) scan2_kernel_4604total_runtime /
                (scan2_kernel_4604runs != 0 ? scan2_kernel_4604runs : 1),
                (long) scan2_kernel_4604total_runtime);
        total_runtime += scan2_kernel_4604total_runtime;
        total_runs += scan2_kernel_4604runs;
        fprintf(stderr,
                "Kernel scan2_kernel_4777          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_4777runs, (long) scan2_kernel_4777total_runtime /
                (scan2_kernel_4777runs != 0 ? scan2_kernel_4777runs : 1),
                (long) scan2_kernel_4777total_runtime);
        total_runtime += scan2_kernel_4777total_runtime;
        total_runs += scan2_kernel_4777runs;
        fprintf(stderr,
                "Kernel scan2_kernel_4916          executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_4916runs, (long) scan2_kernel_4916total_runtime /
                (scan2_kernel_4916runs != 0 ? scan2_kernel_4916runs : 1),
                (long) scan2_kernel_4916total_runtime);
        total_runtime += scan2_kernel_4916total_runtime;
        total_runs += scan2_kernel_4916runs;
    }
    if (debugging)
        fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                total_runs, total_runtime);
    if (detail_memory) {
        fprintf(stderr, "Peak memory usage for space 'device': %ld bytes.\n",
                peak_mem_usage_device);
        fprintf(stderr, "Peak memory usage for space 'local': %ld bytes.\n",
                peak_mem_usage_local);
        fprintf(stderr, "Peak memory usage for default space: %ld bytes.\n",
                peak_mem_usage_default);
    }
    return 0;
}
