#include <stdint.h>
#include <strings.h>
#include <stdlib.h>
#include <malloc_np.h>
#include "erl_nif.h"
#include "ucl.h"

    static ERL_NIF_TERM
to_json(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned char* buf;
    ERL_NIF_TERM ret;

    if (!enif_is_binary(env, argv[0])) {
        ERL_NIF_TERM error = enif_make_atom(env, "error");
        ERL_NIF_TERM reason = enif_make_atom(env, "badarg");
        return enif_make_tuple2(env, error, reason);
    }

    ErlNifBinary binary;
    enif_inspect_binary(env, argv[0], &binary);

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    buf = enif_make_new_binary(env, binary.size, &ret);
    memcpy(buf, binary.data, binary.size);

    return enif_make_tuple2(env, ok, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"to_json", 1, to_json}
};

ERL_NIF_INIT(ucl, nif_funcs, NULL, NULL, NULL, NULL)
