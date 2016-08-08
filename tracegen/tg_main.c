
/*--------------------------------------------------------------------*/
/*--- An example Valgrind tool.                          tg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Lackey, an example Valgrind tool that does
   some simple program measurement and tracing.

   Copyright (C) 2002-2015 Nicholas Nethercote
      njn@valgrind.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

// This tool shows how to do some basic instrumentation.
//
// There are four kinds of instrumentation it can do.  They can be turned
// on/off independently with command line options:
//
// * --basic-counts   : do basic counts, eg. number of instructions
//                      executed, jumps executed, etc.
// * --detailed-counts: do more detailed counts:  number of loads, stores
//                      and ALU operations of different sizes.
// * --trace-mem=yes:   trace all (data) memory accesses.
// * --trace-superblocks=yes:
//                      trace all superblock entries.  Mostly of interest
//                      to the Valgrind developers.
//
// The code for each kind of instrumentation is guarded by a clo_* variable:
// clo_basic_counts, clo_detailed_counts, clo_trace_mem and clo_trace_sbs.
//
// If you want to modify any of the instrumentation code, look for the code
// that is guarded by the relevant clo_* variable (eg. clo_trace_mem)
// If you're not interested in the other kinds of instrumentation you can
// remove them.  If you want to do more complex modifications, please read
// VEX/pub/libvex_ir.h to understand the intermediate representation.
//
//
// Specific Details about --trace-mem=yes
// --------------------------------------
// Lackey's --trace-mem code is a good starting point for building Valgrind
// tools that act on memory loads and stores.  It also could be used as is,
// with its output used as input to a post-mortem processing step.  However,
// because memory traces can be very large, online analysis is generally
// better.
//
// It prints memory data access traces that look like this:
//
//   I  0023C790,2  # instruction read at 0x0023C790 of size 2
//   I  0023C792,5
//    S BE80199C,4  # data store at 0xBE80199C of size 4
//   I  0025242B,3
//    L BE801950,4  # data load at 0xBE801950 of size 4
//   I  0023D476,7
//    M 0025747C,1  # data modify at 0x0025747C of size 1
//   I  0023DC20,2
//    L 00254962,1
//    L BE801FB3,1
//   I  00252305,1
//    L 00254AEB,1
//    S 00257998,1
//
// Every instruction executed has an "instr" event representing it.
// Instructions that do memory accesses are followed by one or more "load",
// "store" or "modify" events.  Some instructions do more than one load or
// store, as in the last two examples in the above trace.
//
// Here are some examples of x86 instructions that do different combinations
// of loads, stores, and modifies.
//
//    Instruction          Memory accesses                  Event sequence
//    -----------          ---------------                  --------------
//    add %eax, %ebx       No loads or stores               instr
//
//    movl (%eax), %ebx    loads (%eax)                     instr, load
//
//    movl %eax, (%ebx)    stores (%ebx)                    instr, store
//
//    incl (%ecx)          modifies (%ecx)                  instr, modify
//
//    cmpsb                loads (%esi), loads(%edi)        instr, load, load
//
//    call*l (%edx)        loads (%edx), stores -4(%esp)    instr, load, store
//    pushl (%edx)         loads (%edx), stores -4(%esp)    instr, load, store
//    movsw                loads (%esi), stores (%edi)      instr, load, store
//
// Instructions using x86 "rep" prefixes are traced as if they are repeated
// N times.
//
// Lackey with --trace-mem gives good traces, but they are not perfect, for
// the following reasons:
//
// - It does not trace into the OS kernel, so system calls and other kernel
//   operations (eg. some scheduling and signal handling code) are ignored.
//
// - It could model loads and stores done at the system call boundary using
//   the pre_mem_read/post_mem_write events.  For example, if you call
//   fstat() you know that the passed in buffer has been written.  But it
//   currently does not do this.
//
// - Valgrind replaces some code (not much) with its own, notably parts of
//   code for scheduling operations and signal handling.  This code is not
//   traced.
//
// - There is no consideration of virtual-to-physical address mapping.
//   This may not matter for many purposes.
//
// - Valgrind modifies the instruction stream in some very minor ways.  For
//   example, on x86 the bts, btc, btr instructions are incorrectly
//   considered to always touch memory (this is a consequence of these
//   instructions being very difficult to simulate).
//
// - Valgrind tools layout memory differently to normal programs, so the
//   addresses you get will not be typical.  Thus Lackey (and all Valgrind
//   tools) is suitable for getting relative memory traces -- eg. if you
//   want to analyse locality of memory accesses -- but is not good if
//   absolute addresses are important.
//
// Despite all these warnings, Lackey's results should be good enough for a
// wide range of purposes.  For example, Cachegrind shares all the above
// shortcomings and it is still useful.
//
// For further inspiration, you should look at cachegrind/cg_main.c which
// uses the same basic technique for tracing memory accesses, but also groups
// events together for processing into twos and threes so that fewer C calls
// are made and things run faster.
//
// Specific Details about --trace-superblocks=yes
// ----------------------------------------------
// Valgrind splits code up into single entry, multiple exit blocks
// known as superblocks.  By itself, --trace-superblocks=yes just
// prints a message as each superblock is run:
//
//  SB 04013170
//  SB 04013177
//  SB 04013173
//  SB 04013177
//
// The hex number is the address of the first instruction in the
// superblock.  You can see the relationship more obviously if you use
// --trace-superblocks=yes and --trace-mem=yes together.  Then a "SB"
// message at address X is immediately followed by an "instr:" message
// for that address, as the first instruction in the block is
// executed, for example:
//
//  SB 04014073
//  I  04014073,3
//   L 7FEFFF7F8,8
//  I  04014076,4
//  I  0401407A,3
//  I  0401407D,3
//  I  04014080,3
//  I  04014083,6


#include "pub_tool_basics.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_options.h"
#include "pub_tool_machine.h"     // VG_(fnptr_to_fnentry)
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()

#include "tg_ctf.h"
#include "tg_ctf-platform-valgrind.h"

#include <sys/stat.h>
#include <sys/types.h>


/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

/* Command line options controlling instrumentation kinds, as described at
 * the top of this file. */
static Bool clo_trace_mem       = True;
static Bool clo_trace_sbs       = False;

/* output directory for the CTF trace files */
static const HChar* clo_output_dir = ".";

/* The name of the function of which the number of calls (under
 * --basic-counts=yes) is to be counted, with default. Override with command
 * line option --fnname. */
static const HChar* clo_fnname = "main";

static Bool tg_process_cmd_line_option(const HChar* arg)
{
   if VG_STR_CLO(arg, "--fnname", clo_fnname) {}
   else if VG_STR_CLO(arg, "--output-dir", clo_output_dir) {}
   else if VG_BOOL_CLO(arg, "--trace-mem",         clo_trace_mem) {}
   else if VG_BOOL_CLO(arg, "--trace-superblocks", clo_trace_sbs) {}
   else
      return False;

   tl_assert(clo_output_dir);
   tl_assert(clo_output_dir[0]);

   tl_assert(clo_fnname);
   tl_assert(clo_fnname[0]);

   if (!VG_(is_dir)(clo_output_dir)) {
       VG_(umsg)("Output directory does not exist. Create it before running "
                 "this tool.\n");
       return False;
    }
   return True;
}

static void tg_print_usage(void)
{
   VG_(printf)(
"    --output-dir=<dir>        write CTS trace files to <dir>"
"                              (must already exist) [current directory (.)]\n"
"    --trace-mem=no|yes        trace all loads and stores [yes]\n"
"    --trace-superblocks=no|yes  trace all superblock entries [no]\n"
"    --fnname=<name>           count calls to <name> (only used if\n"
"                              --basic-count=yes)  [main]\n"
   );
}

static void tg_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}

/*------------------------------------------------------------*/
/*--- Tracing                                              ---*/
/*------------------------------------------------------------*/
static struct tg_ctf_platform_valgrind_ctx *ctf_platform_ctx;
static struct tg_ctf_default_ctx *ctf_ctx;


/*------------------------------------------------------------*/
/*--- Stuff for --trace-mem                                ---*/
/*------------------------------------------------------------*/
typedef
   IRExpr
   IRAtom;


#define MAX_DSIZE    512

typedef
   enum { Event_Ir, Event_Dr, Event_Dw, Event_Dm }
   EventKind;

typedef
   struct {
      EventKind  ekind;
      IRAtom*    addr;
      Int        size;
      IRAtom*    guard; /* :: Ity_I1, or NULL=="always True" */
      ThreadId   tid;
   }
   Event;

/* Up to this many unnotified events are allowed.  Must be at least two,
   so that reads and writes to the same address can be merged into a modify.
   Beyond that, larger numbers just potentially induce more spilling due to
   extending live ranges of address temporaries. */
#define N_EVENTS 4

/* Maintain an ordered list of memory events which are outstanding, in
   the sense that no IR has yet been generated to do the relevant
   helper calls.  The SB is scanned top to bottom and memory events
   are added to the end of the list, merging with the most recent
   notified event where possible (Dw immediately following Dr and
   having the same size and EA can be merged).

   This merging is done so that for architectures which have
   load-op-store instructions (x86, amd64), the instr is treated as if
   it makes just one memory reference (a modify), rather than two (a
   read followed by a write at the same address).

   At various points the list will need to be flushed, that is, IR
   generated from it.  That must happen before any possible exit from
   the block (the end, or an IRStmt_Exit).  Flushing also takes place
   when there is no space to add a new event, and before entering a
   RMW (read-modify-write) section on processors supporting LL/SC.

   If we require the simulation statistics to be up to date with
   respect to possible memory exceptions, then the list would have to
   be flushed before each memory reference.  That's a pain so we don't
   bother.

   Flushing the list consists of walking it start to end and emitting
   instrumentation IR for each event, in the order in which they
   appear. */

static Event events[N_EVENTS];
static Int   events_used = 0;


static VG_REGPARM(3) void trace_instr(Addr addr, SizeT size, ThreadId tid)
{
   tg_ctf_default_trace_instr(ctf_ctx, tid, addr);
}

static VG_REGPARM(3) void trace_load(Addr addr, SizeT size, ThreadId tid)
{
    tg_ctf_default_trace_mem(ctf_ctx, tid, 0 /* read */, addr, size);
}

static VG_REGPARM(3) void trace_store(Addr addr, SizeT size, ThreadId tid)
{
    tg_ctf_default_trace_mem(ctf_ctx, tid, 1 /* write */, addr, size);
}

static VG_REGPARM(3) void trace_modify(Addr addr, SizeT size, ThreadId tid)
{
    tg_ctf_default_trace_mem(ctf_ctx, tid, 2 /* modify */, addr, size);
}


static void flushEvents(IRSB* sb)
{
   Int        i;
   const HChar* helperName;
   void*      helperAddr;
   IRExpr**   argv;
   IRDirty*   di;
   Event*     ev;

   for (i = 0; i < events_used; i++) {

      ev = &events[i];

      // Decide on helper fn to call and args to pass it.
      switch (ev->ekind) {
         case Event_Ir: helperName = "trace_instr";
                        helperAddr =  trace_instr;  break;

         case Event_Dr: helperName = "trace_load";
                        helperAddr =  trace_load;   break;

         case Event_Dw: helperName = "trace_store";
                        helperAddr =  trace_store;  break;

         case Event_Dm: helperName = "trace_modify";
                        helperAddr =  trace_modify; break;
         default:
            tl_assert(0);
      }

      // Add the helper.
      argv = mkIRExprVec_3( ev->addr, mkIRExpr_HWord( ev->size ), mkIRExpr_HWord(ev->tid) );
      di   = unsafeIRDirty_0_N( /*regparms*/3,
                                helperName, VG_(fnptr_to_fnentry)( helperAddr ),
                                argv );
      if (ev->guard) {
         di->guard = ev->guard;
      }
      addStmtToIRSB( sb, IRStmt_Dirty(di) );
   }

   events_used = 0;
}

// WARNING:  If you aren't interested in instruction reads, you can omit the
// code that adds calls to trace_instr() in flushEvents().  However, you
// must still call this function, addEvent_Ir() -- it is necessary to add
// the Ir events to the events list so that merging of paired load/store
// events into modify events works correctly.
static void addEvent_Ir ( IRSB* sb, IRAtom* iaddr, UInt isize )
{
   Event* evt;
   tl_assert(clo_trace_mem);
   tl_assert( (VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB)
            || VG_CLREQ_SZB == isize );
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt = &events[events_used];
   evt->ekind = Event_Ir;
   evt->addr  = iaddr;
   evt->size  = isize;
   evt->guard = NULL;
   evt->tid   = VG_(get_running_tid)();
   events_used++;
}

/* Add a guarded read event. */
static
void addEvent_Dr_guarded ( IRSB* sb, IRAtom* daddr, Int dsize, IRAtom* guard )
{
   Event* evt;
   tl_assert(clo_trace_mem);
   tl_assert(isIRAtom(daddr));
   tl_assert(dsize >= 1 && dsize <= MAX_DSIZE);
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt = &events[events_used];
   evt->ekind = Event_Dr;
   evt->addr  = daddr;
   evt->size  = dsize;
   evt->guard = guard;
   evt->tid   = VG_(get_running_tid)();
   events_used++;
}

/* Add an ordinary read event, by adding a guarded read event with an
   always-true guard. */
static
void addEvent_Dr ( IRSB* sb, IRAtom* daddr, Int dsize )
{
   addEvent_Dr_guarded(sb, daddr, dsize, NULL);
}

/* Add a guarded write event. */
static
void addEvent_Dw_guarded ( IRSB* sb, IRAtom* daddr, Int dsize, IRAtom* guard )
{
   Event* evt;
   tl_assert(clo_trace_mem);
   tl_assert(isIRAtom(daddr));
   tl_assert(dsize >= 1 && dsize <= MAX_DSIZE);
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt = &events[events_used];
   evt->ekind = Event_Dw;
   evt->addr  = daddr;
   evt->size  = dsize;
   evt->guard = guard;
   evt->tid   = VG_(get_running_tid)();
   events_used++;
}

/* Add an ordinary write event.  Try to merge it with an immediately
   preceding ordinary read event of the same size to the same
   address. */
static
void addEvent_Dw ( IRSB* sb, IRAtom* daddr, Int dsize )
{
   Event* lastEvt;
   Event* evt;
   tl_assert(clo_trace_mem);
   tl_assert(isIRAtom(daddr));
   tl_assert(dsize >= 1 && dsize <= MAX_DSIZE);

   // Is it possible to merge this write with the preceding read?
   lastEvt = &events[events_used-1];
   if (events_used > 0
       && lastEvt->ekind == Event_Dr
       && lastEvt->size  == dsize
       && lastEvt->guard == NULL
       && eqIRAtom(lastEvt->addr, daddr))
   {
      lastEvt->ekind = Event_Dm;
      return;
   }

   // No.  Add as normal.
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt = &events[events_used];
   evt->ekind = Event_Dw;
   evt->size  = dsize;
   evt->addr  = daddr;
   evt->guard = NULL;
   evt->tid   = VG_(get_running_tid)();
   events_used++;
}


/*------------------------------------------------------------*/
/*--- Stuff for --trace-superblocks                        ---*/
/*------------------------------------------------------------*/

static void trace_superblock(Addr addr)
{
   VG_(printf)("SB %08lx\n", addr);
}


/*------------------------------------------------------------*/
/*--- Basic tool functions                                 ---*/
/*------------------------------------------------------------*/

static void tg_post_clo_init(void)
{
    /* initialize CTF tracing (based on barectf) */
    ctf_platform_ctx = tg_ctf_platform_valgrind_init(512, clo_output_dir);
    ctf_ctx = tg_ctf_platform_valgrind_get_tg_ctf_ctx(ctf_platform_ctx);
}

static
IRSB* tg_instrument ( VgCallbackClosure* closure,
                      IRSB* sbIn,
                      const VexGuestLayout* layout,
                      const VexGuestExtents* vge,
                      const VexArchInfo* archinfo_host,
                      IRType gWordTy, IRType hWordTy )
{
   IRDirty*   di;
   Int        i;
   IRSB*      sbOut;
   IRTypeEnv* tyenv = sbIn->tyenv;
   Addr       iaddr = 0, dst;
   UInt       ilen = 0;
   Bool       condition_inverted = False;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up SB */
   sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( sbOut, sbIn->stmts[i] );
      i++;
   }

   if (clo_trace_sbs) {
      /* Print this superblock's address. */
      di = unsafeIRDirty_0_N(
              0, "trace_superblock",
              VG_(fnptr_to_fnentry)( &trace_superblock ),
              mkIRExprVec_1( mkIRExpr_HWord( vge->base[0] ) )
           );
      addStmtToIRSB( sbOut, IRStmt_Dirty(di) );
   }

   if (clo_trace_mem) {
      events_used = 0;
   }

   for (/*use current i*/; i < sbIn->stmts_used; i++) {
      IRStmt* st = sbIn->stmts[i];
      if (!st || st->tag == Ist_NoOp) continue;

      switch (st->tag) {
         case Ist_NoOp:
         case Ist_AbiHint:
         case Ist_Put:
         case Ist_PutI:
         case Ist_MBE:
            addStmtToIRSB( sbOut, st );
            break;

         case Ist_IMark:
            if (clo_trace_mem) {
               // WARNING: do not remove this function call, even if you
               // aren't interested in instruction reads.  See the comment
               // above the function itself for more detail.
               addEvent_Ir( sbOut, mkIRExpr_HWord( (HWord)st->Ist.IMark.addr ),
                            st->Ist.IMark.len );
            }
            addStmtToIRSB( sbOut, st );
            break;

         case Ist_WrTmp:
            // Add a call to trace_load() if --trace-mem=yes.
            if (clo_trace_mem) {
               IRExpr* data = st->Ist.WrTmp.data;
               if (data->tag == Iex_Load) {
                  addEvent_Dr( sbOut, data->Iex.Load.addr,
                               sizeofIRType(data->Iex.Load.ty) );
               }
            }
            addStmtToIRSB( sbOut, st );
            break;

         case Ist_Store: {
            IRExpr* data = st->Ist.Store.data;
            IRType  type = typeOfIRExpr(tyenv, data);
            tl_assert(type != Ity_INVALID);
            if (clo_trace_mem) {
               addEvent_Dw( sbOut, st->Ist.Store.addr,
                            sizeofIRType(type) );
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_StoreG: {
            IRStoreG* sg   = st->Ist.StoreG.details;
            IRExpr*   data = sg->data;
            IRType    type = typeOfIRExpr(tyenv, data);
            tl_assert(type != Ity_INVALID);
            if (clo_trace_mem) {
               addEvent_Dw_guarded( sbOut, sg->addr,
                                    sizeofIRType(type), sg->guard );
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_LoadG: {
            IRLoadG* lg       = st->Ist.LoadG.details;
            IRType   type     = Ity_INVALID; /* loaded type */
            IRType   typeWide = Ity_INVALID; /* after implicit widening */
            typeOfIRLoadGOp(lg->cvt, &typeWide, &type);
            tl_assert(type != Ity_INVALID);
            if (clo_trace_mem) {
               addEvent_Dr_guarded( sbOut, lg->addr,
                                    sizeofIRType(type), lg->guard );
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_Dirty: {
            if (clo_trace_mem) {
               Int      dsize;
               IRDirty* d = st->Ist.Dirty.details;
               if (d->mFx != Ifx_None) {
                  // This dirty helper accesses memory.  Collect the details.
                  tl_assert(d->mAddr != NULL);
                  tl_assert(d->mSize != 0);
                  dsize = d->mSize;
                  if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
                     addEvent_Dr( sbOut, d->mAddr, dsize );
                  if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
                     addEvent_Dw( sbOut, d->mAddr, dsize );
               } else {
                  tl_assert(d->mAddr == NULL);
                  tl_assert(d->mSize == 0);
               }
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_CAS: {
            /* We treat it as a read and a write of the location.  I
               think that is the same behaviour as it was before IRCAS
               was introduced, since prior to that point, the Vex
               front ends would translate a lock-prefixed instruction
               into a (normal) read followed by a (normal) write. */
            Int    dataSize;
            IRType dataTy;
            IRCAS* cas = st->Ist.CAS.details;
            tl_assert(cas->addr != NULL);
            tl_assert(cas->dataLo != NULL);
            dataTy   = typeOfIRExpr(tyenv, cas->dataLo);
            dataSize = sizeofIRType(dataTy);
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since it's a doubleword-CAS */
            if (clo_trace_mem) {
               addEvent_Dr( sbOut, cas->addr, dataSize );
               addEvent_Dw( sbOut, cas->addr, dataSize );
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_LLSC: {
            IRType dataTy;
            if (st->Ist.LLSC.storedata == NULL) {
               /* LL */
               dataTy = typeOfIRTemp(tyenv, st->Ist.LLSC.result);
               if (clo_trace_mem) {
                  addEvent_Dr( sbOut, st->Ist.LLSC.addr,
                                      sizeofIRType(dataTy) );
                  /* flush events before LL, helps SC to succeed */
                  flushEvents(sbOut);
	       }
            } else {
               /* SC */
               dataTy = typeOfIRExpr(tyenv, st->Ist.LLSC.storedata);
               if (clo_trace_mem)
                  addEvent_Dw( sbOut, st->Ist.LLSC.addr,
                                      sizeofIRType(dataTy) );
            }
            addStmtToIRSB( sbOut, st );
            break;
         }

         case Ist_Exit:
            if (clo_trace_mem) {
               flushEvents(sbOut);
            }

            addStmtToIRSB( sbOut, st );      // Original statement
            break;

         default:
            ppIRStmt(st);
            tl_assert(0);
      }
   }

   if (clo_trace_mem) {
      /* At the end of the sbIn.  Flush outstandings. */
      flushEvents(sbOut);
   }

   return sbOut;
}

static void tg_fini(Int exitcode)
{
   tl_assert(clo_fnname);
   tl_assert(clo_fnname[0]);

   /* end tracing */
   tg_ctf_platform_valgrind_fini(ctf_platform_ctx);
}

static void tg_pre_clo_init(void)
{
   VG_(details_name)            ("tracegen");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("generate program and data traces");
   VG_(details_copyright_author)(
      "Copyright (C) 2016, and GNU GPL'd, by Philipp Wagner.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 200 );

   VG_(basic_tool_funcs)          (tg_post_clo_init,
                                   tg_instrument,
                                   tg_fini);
   VG_(needs_command_line_options)(tg_process_cmd_line_option,
                                   tg_print_usage,
                                   tg_print_debug_usage);
}

VG_DETERMINE_INTERFACE_VERSION(tg_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                tg_main.c ---*/
/*--------------------------------------------------------------------*/
