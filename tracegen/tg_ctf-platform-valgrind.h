#ifndef _TG_CTF_PLATFORM_VALGRIND_H
#define _TG_CTF_PLATFORM_VALGRIND_H

/*
 * barectf valgrind platform
 *
 * Copyright (c) 2016 Philipp Wagner <philipp.wagner@tum.de>
 *
 * Based on the barectf linux fs platform:
 * Copyright (c) 2015 EfficiOS Inc. and Linux Foundation
 * Copyright (c) 2015 Philippe Proulx <pproulx@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdint.h>
#include <tg_ctf.h>

struct tg_ctf_platform_valgrind_ctx;

/**
 * Initializes the platform.
 *
 * @param buf_size   Packet size (bytes)
 * @param trace_dir  Trace directory
 * @returns          Platform context
 */
struct tg_ctf_platform_valgrind_ctx *tg_ctf_platform_valgrind_init(
    unsigned int buf_size, const char *trace_dir);

/**
 * Finalizes the platform.
 *
 * @param ctx    Platform context
 */
void tg_ctf_platform_valgrind_fini(struct tg_ctf_platform_valgrind_ctx *ctx);

/**
 * Returns the barectf stream-specific context of a given platform context.
 *
 * This context is what barectf tracing functions need.
 *
 * @param ctx  Platform context
 * @returns    barectf stream-specific context
 */
struct tg_ctf_default_ctx *tg_ctf_platform_valgrind_get_tg_ctf_ctx(
    struct tg_ctf_platform_valgrind_ctx *ctx);

#endif /* _TG_CTF_PLATFORM_VALGRIND_H */
