#include <erl_nif.h>
#include <cstring>
#include <iostream>

#include "../libs/chiapos/src/encoding.hpp"

#define BUFFER_SIZE 6047

extern "C" {
    ERL_NIF_TERM nif_ANSEncodeDeltas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM nif_ANSDecodeDeltas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

ERL_NIF_TERM make_response(ErlNifEnv* env, const char * ErrorType, const char * Reason) {
    return enif_make_tuple2(env,
        enif_make_atom(env, ErrorType),
        enif_make_atom(env, Reason)
    );
}

ERL_NIF_TERM nif_Encoding_ANSEncodeDeltas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2 ) {
	    return enif_make_badarg(env);
    }

    ErlNifBinary deltas_bin;
    if (!enif_inspect_binary(env, argv[0], &deltas_bin)) {
	    return make_response(env, "error", "deltas_is_not_binary");
    }

    double R;
    if (!enif_get_double(env, argv[1], &R)) {
        return make_response(env, "error","R_is_not_float");
    }

    std::vector<unsigned char> deltas;
    deltas.assign((char *) deltas_bin.data, ((char *) deltas_bin.data) + deltas_bin.size);

    uint8_t out[BUFFER_SIZE];

    size_t size;
    try {
        size = Encoding::ANSEncodeDeltas(deltas, R, out);
    } catch( std::exception *e) {
        return make_response(env, "error", e->what());
    }

    if (size==0) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "ok"),
            enif_make_tuple2(env,
                enif_make_atom(env, "uncompressed"),
                enif_make_binary(env, &deltas_bin)
            ));
    }

    ErlNifBinary binary;
    if (!enif_alloc_binary(size, &binary)) {
        return make_response(env, "error", "memalloc");
    }

    memcpy((char *) binary.data, out, size);
    ERL_NIF_TERM response = enif_make_binary(env, &binary);
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_tuple2(env,
            enif_make_atom(env, "compressed"),
            response
        ));
}

ERL_NIF_TERM nif_Encoding_ANSDecodeDeltas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    if (argc != 3 ) {
	    return enif_make_badarg(env);
    }

    ErlNifBinary deltas_bin;
    if (!enif_inspect_binary(env, argv[0], &deltas_bin)) {
	    return make_response(env, "error", "deltas_is_not_binary");
    }

    int numDeltas;
    if (!enif_get_int(env, argv[1], &numDeltas)) {
        return make_response(env, "error","NumDeltas_is_not_integer");
    }

    double R;
    if (!enif_get_double(env, argv[2], &R)) {
        return make_response(env, "error","R_is_not_float");
    }

    static std::vector<uint8_t> result;
    try {
        result = Encoding::ANSDecodeDeltas((uint8_t *) deltas_bin.data, deltas_bin.size, numDeltas, R);
    } catch( std::exception *e) {
        return make_response(env, "error", e->what());
    }

    char * buf = reinterpret_cast <char*> (&result[0]);
    size_t size = result.size();

    ErlNifBinary binary;
    if (!enif_alloc_binary(size, &binary)) {
        return make_response(env, "error", "memalloc");
    }

    memcpy((char *) binary.data, buf, size);

    ERL_NIF_TERM response = enif_make_binary(env, &binary);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), response);
}


extern "C" {

    int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
        return 0;
    }

	int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

    static ErlNifFunc nif_funcs[] = {
            {"encoding_ans_encode_deltas", 2, nif_Encoding_ANSEncodeDeltas},
            {"encoding_ans_decode_deltas", 3, nif_Encoding_ANSDecodeDeltas},
    };

    ERL_NIF_INIT(echiapos_nif, nif_funcs, &on_load, NULL, NULL, NULL);
};