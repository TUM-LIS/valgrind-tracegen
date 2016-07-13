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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <tg_ctf.h>
#include <time.h>

#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_mallocfree.h"

#include "tg_ctf-platform-valgrind.h"

/* the "metadata" file with the TSDL description of the stream, converted to a
 * C string.
 */
#include "tg_ctf_metadata.h"

struct tg_ctf_platform_valgrind_ctx {
    struct tg_ctf_default_ctx ctx;
    int fd;
};

static uint64_t get_clock(void* data)
{
    /* XXX: do we need/should we get ns clock? */
    /*struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;*/
    return VG_(read_millisecond_timer)();
}

static void write_packet(struct tg_ctf_platform_valgrind_ctx *ctx)
{
    ssize_t size_written = VG_(write)(ctx->fd, tg_ctf_packet_buf(&ctx->ctx),
                                      tg_ctf_packet_buf_size(&ctx->ctx));
    tl_assert(size_written == tg_ctf_packet_buf_size(&ctx->ctx));
}

static int is_backend_full(void *data)
{
    return 0;
}

static void open_packet(void *data)
{
    struct tg_ctf_platform_valgrind_ctx *ctx = data;

    tg_ctf_default_open_packet(&ctx->ctx);
}

static void close_packet(void *data)
{
    struct tg_ctf_platform_valgrind_ctx *ctx = data;

    /* close packet now */
    tg_ctf_default_close_packet(&ctx->ctx);

    /* write packet to file */
    write_packet(ctx);
}

struct tg_ctf_platform_valgrind_ctx *tg_ctf_platform_valgrind_init(
    unsigned int buf_size, const char *trace_dir)
{
    char stream_path[1024];
    char metadata_path[1024];
    int metadata_fd;
    uint8_t *buf;
    struct tg_ctf_platform_valgrind_ctx *ctx;
    struct tg_ctf_platform_callbacks cbs = {
        .default_clock_get_value = get_clock,
        .is_backend_full = is_backend_full,
        .open_packet = open_packet,
        .close_packet = close_packet,
    };

    ctx = VG_(malloc)("ctx", sizeof(*ctx));
    tl_assert(ctx); /* cannot happen says valgrind */

    buf = VG_(calloc)("buf", buf_size, 1);
    tl_assert(buf);

    VG_(sprintf)(stream_path, "%s/stream", trace_dir);

    ctx->fd = VG_(fd_open)(stream_path,
                           VKI_O_WRONLY | VKI_O_TRUNC | VKI_O_CREAT,
                           00644);
    tl_assert(ctx->fd != -1);

    if (!ctx->fd) {
        VG_(free)(ctx);
        VG_(free)(buf);
        return NULL;
    }

    /* write metadata file into output directory */
    VG_(sprintf)(metadata_path, "%s/metadata", trace_dir);
    metadata_fd = VG_(fd_open)(metadata_path,
                               VKI_O_WRONLY | VKI_O_TRUNC | VKI_O_CREAT,
                               00644);
    tl_assert(metadata_fd != -1);
    ssize_t size_written = VG_(write)(metadata_fd, metadata, metadata_len);
    tl_assert(size_written == metadata_len);
    VG_(close)(metadata_fd);


    tg_ctf_init(&ctx->ctx, buf, buf_size, cbs, ctx);
    open_packet(ctx);

    return ctx;
}

void tg_ctf_platform_valgrind_fini(struct tg_ctf_platform_valgrind_ctx *ctx)
{
    if (tg_ctf_packet_is_open(&ctx->ctx) &&
        !tg_ctf_packet_is_empty(&ctx->ctx)) {
        close_packet(ctx);
    }

    VG_(close)(ctx->fd);
    VG_(free)(tg_ctf_packet_buf(&ctx->ctx));
    VG_(free)(ctx);
}

struct tg_ctf_default_ctx *tg_ctf_platform_valgrind_get_tg_ctf_ctx(
    struct tg_ctf_platform_valgrind_ctx *ctx)
{
    return &ctx->ctx;
}
