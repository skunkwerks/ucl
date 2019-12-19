#include <stdint.h>
#include <stdlib.h>
#include <strings.h>

#if defined(__linux__)
#include <malloc.h>
#elif defined(BSD)
#include <malloc_np.h>
#endif

#include "erl_nif.h"
#include "ucl.h"

    static ERL_NIF_TERM
to_json(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // buffer for returning parsed JSON
    unsigned char* buf;
    ERL_NIF_TERM json;

    // ucl bits and pieces
    struct ucl_parser *parser = NULL;

    ucl_object_t *ucl = NULL;
    ucl_emitter_t emitter = UCL_EMIT_JSON;

    if (!enif_is_binary(env, argv[0])) {
        ERL_NIF_TERM error = enif_make_atom(env, "error");
        ERL_NIF_TERM reason = enif_make_atom(env, "badarg");
        return enif_make_tuple2(env, error, reason);
    }

    ErlNifBinary binary;
    enif_inspect_binary(env, argv[0], &binary);

    // TODO ensure binary is NULL terminated

    // parse UCL
    // https://github.com/vstakhov/libucl/blob/master/doc/api.md#parser-functions-1
    parser = ucl_parser_new(UCL_PARSER_KEY_LOWERCASE);
    ucl_parser_add_chunk (parser, binary.data, binary.size);

    if (ucl_parser_get_error (parser)) {
        ERL_NIF_TERM error = enif_make_atom(env, "error");
        ERL_NIF_TERM reason = enif_make_atom(env, "ucl_invalid");
        return enif_make_tuple2(env, error, reason);
    }
    else if ((ucl = ucl_parser_get_object(parser)) == NULL) {
        ERL_NIF_TERM error = enif_make_atom(env, "error");
        ERL_NIF_TERM reason = enif_make_atom(env, "ucl_no_root");
        return enif_make_tuple2(env, error, reason);
    }
    else {
        ucl = ucl_parser_get_object (parser);
    }

    if (parser != NULL) {
        ucl_parser_free (parser);
    }
    if (ucl != NULL) {
        ucl_object_unref (ucl);
    }

    // turn it into JSON
    size_t len;
    unsigned char *blob;
    blob = ucl_object_emit_len(ucl, emitter, &len);

    // return JSON as binary
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    buf = enif_make_new_binary(env, len, &json);
    memcpy(buf, blob, len);

    return enif_make_tuple2(env, ok, json);

// Public Domain code from https://github.com/davisp/nif-examples
// There are three functions that may be called during the lifetime
// of a NIF. load, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// reload is no longer supported since OTP20
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
  return 0;
}

// Called when changing versions of the C code for a module's NIF
// implementation if I read the docs correctly.
//
// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#upgrade

static int upgrade(ErlNifEnv *env, void **priv, void **old_priv,
                   ERL_NIF_TERM load_info) {
  return 0;
}

// Called when the library is unloaded. Not called after a reload
// executes.
//
// No return value
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static void unload(ErlNifEnv *env, void *priv) { return; }

// initialise NIF library
// support load, reload, upgrade, unload

ERL_NIF_INIT(ucl, nif_funcs, &load, NULL, &upgrade, &unload);
