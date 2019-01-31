#!/usr/bin/env python
import sys
import gi
import operator as op
from functools import partial as bind

gi.require_version('Gst', '1.0')
from gi.repository import GObject, Gst
Gst.init(None)

# --- pytools digest, for self containment ---
delay = bind(bind, bind)
def def_consult(default, d):    return lambda k: d.get(k, default)
def compose2(f, g): return lambda x: f(g(x))
def compose(*args): return reduce(compose2, args)
def const(x):       return lambda _: x
def app(f, x):      return f(x)
def tuncurry(f):    return lambda args: reduce(app, args, f)
def fork(f, g):     return lambda x: (f(x), g(x))
def fst((x, _)):    return x
def snd((_, x)):    return x
def decons(xs):     return xs[0], xs[1:]
def mul(y):         return lambda x: x * y
def divby(y):       return lambda x: float(x) / y
def maybe(f):       return lambda x: None if x is None else f(x)
def guard(p):       return lambda x: x if p(x) else None
eq, neq = tuple(map(delay, [op.eq, op.ne]))
pos = bind(op.lt, 0)

class Context(object):
    def __init__(self, setup): self.setup = setup
    def __exit__(self, _1, _2, _3): self.cleanup()
    def __enter__(self):
        self.cleanup, x = self.setup()
        return x
# ----------- end of digest ------------

MT = Gst.MessageType
M = Gst.Message
_FORMAT_TIME = Gst.Format(Gst.Format.TIME)
ns_to_s, s_to_ns = fork(divby, mul)(10 ** 9)

ignore = const(None)
def to_maybe((valid, x)): return x if valid else None
def do(a,f): return compose(a, f, fst)
def rpt(l): return lambda x: print_('%s %s' % (l, str(x)))
def tl_bitrate(tl): return snd(tl.get_uint('bitrate'))
def io_watch(s, c, f): return lambda: (bind(GObject.source_remove, GObject.io_add_watch(s, c, f)), None)
def signal_watch(bus): return lambda: (bus.remove_signal_watch, bus.add_signal_watch())
def read_command(s): return decons(s.readline().rstrip().split(' ', 1))
def fmt_cap(c): return '%s:%s:%s' % (to_maybe(c.get_int('rate')), c.get_string('format'), to_maybe(c.get_int('channels')))
def get_caps(p): return p.emit('get-audio-pad', 0).get_current_caps()
def fmt_fst_cap(c): return fmt_cap(c.get_structure(0))
def print_(s): sys.stdout.write(s); sys.stdout.write('\n'); sys.stdout.flush()
ctrue = const(True)

def changes(state):
    def set_and_return(x): state[0] = x; return x
    return lambda x: maybe(set_and_return)(guard(neq(state[0]))(x))
def main():
    p = Gst.ElementFactory.make("playbin", None)
    stop, pause, play = tuple(map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING]))
    loop = GObject.MainLoop()

    events = def_consult(ignore, #do(print_, lambda m:str(m.type)),
                   { MT.EOS:          compose(print_, const('eos'))
                   , MT.ERROR:        do(rpt('error'), M.parse_error)
                   , MT.TAG:          do(maybe(rpt('bitrate')), compose(guard(pos), tl_bitrate, M.parse_tag))
                   # , MT.STREAM_START: do(rpt('format'), compose(maybe(fmt_fst_cap), get_caps, const(p)))
                   , MT.DURATION_CHANGED: compose(maybe(compose(rpt('duration'), ns_to_s)), maybe(changes([None])),
                                                  guard(pos), lambda _: p.query_duration(_FORMAT_TIME)[1])
                   # , MT.ASYNC_DONE:       do(rpt('async_done'), M.parse_async_done)
                   # , MT.BUFFERING:        do(maybe(rpt('buffering')), compose(guard(eq(100)), M.parse_buffering))
                   # , MT.LATENCY:       ignore
                   # , MT.STATE_CHANGED: ignore
                   # , MT.STREAM_STATUS: ignore
                   # , MT.ELEMENT:       ignore
                   # , MT.NEED_CONTEXT:  ignore
                   # , MT.HAVE_CONTEXT:  ignore
                   # , MT.NEW_CLOCK:     ignore
                   # , MT.RESET_TIME:    ignore
                   })
    def sync():     p.get_state(100000000000000)
    def position(): return ns_to_s(p.query_position(_FORMAT_TIME)[1])
    def seek(t):    return p.seek_simple(_FORMAT_TIME, Gst.SeekFlags.FLUSH, s_to_ns(t)), sync()
    def handler(d): return tuncurry(def_consult(lambda _: ctrue(print_('unrecognised')), d))
    player = handler({ '':     lambda _: loop.quit()
                     , 'stop':     lambda _: stop()
                     , 'pause':    lambda _: pause()
                     , 'play':     lambda _: play()
                     , 'volume':   lambda a: p.set_property('volume', float(a[0]))
                     , 'position': lambda _: rpt('position')(position())
                     , 'format':   lambda _: (sync(), rpt('format')(maybe(fmt_fst_cap)(get_caps(p))))
                     , 'seekrel':  lambda a: seek(float(a[0]) + position())
                     , 'seek':     lambda a: seek(float(a[0]))
                     , 'sync':     lambda _: sync()
                     , 'uri':      lambda a: (stop(), p.set_property('uri', a[0]), pause())
                     })

    def on_message(_, message, loop): return ctrue(events(message.type)((message, loop)))
    def on_input(s, _):
        try: player(read_command(s))
        except Exception as ex: print "error", ex
        return True

    bus = p.get_bus()
    bus.connect("message", on_message, loop)
    with Context(io_watch(sys.stdin, GObject.IO_IN, on_input)):
        with Context(signal_watch(bus)):
            with Context(lambda: (stop, None)):
                loop.run()

if __name__ == '__main__': main()
