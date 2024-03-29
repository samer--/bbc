#!/usr/bin/env python3
import sys
import gi
import threading
import operator as op
from functools import partial as bind, reduce

gi.require_version('Gst', '1.0')
from gi.repository import Gst
Gst.init(None)

# --- pytools digest, for self containment ---
delay = bind(bind, bind)
def def_consult(default, d):    return lambda k: d.get(k, default)
def identity(x):    return x
def compose2(f, g): return lambda x: f(g(x))
def compose(*args): return reduce(compose2, args)
def const(x):       return lambda _: x
def tuncurry(f):    return lambda args: reduce(lambda f, x: f(x), args, f)
def fork(f, g):     return lambda x: (f(x), g(x))
def snd(xy): _, y = xy; return y
def decons(xs):     return xs[0], xs[1:]
def mul(y):         return lambda x: x * y
def divby(y):       return lambda x: float(x) / y
def maybe(f):       return lambda x: None if x is None else f(x)
def guard(p):       return lambda x: x if p(x) else None
_normal = '\033[0m'
def print_stderr(m): sys.stderr.write(''.join([col(0,'yellow'), m, _normal, '\n'])); sys.stderr.flush()
def swap(xy): x, y = xy; return y, x
def col(brightness, name): return '\033[%dm' % ([30, 90][brightness] + _colours[name])
_colours = dict(map(swap, enumerate(['black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white'])))
@delay
def tracef(info, f, arg): print_stderr("==> %s: %r" % (info, arg)); return f(arg)

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
mutex = threading.Lock()
lock = Context(lambda: (mutex.release, mutex.acquire()))

def to_maybe(t): valid, x = t; return x if valid else None
def rpt(l): return lambda x: print_('%s %s' % (l, str(x)))
def tl_bitrate(tl): return snd(tl.get_uint('bitrate'))
def tr(x): sys.stderr.write('%s\n' % repr(x)); return x
def print_(s):
    with lock: print(s); sys.stdout.flush()
def fmt_cap(c): return '%s:%s:%s' % (to_maybe(c.get_int('rate')), c.get_string('format') or 'F', to_maybe(c.get_int('channels')))
rpt_cap = compose(rpt('format'), lambda c: fmt_cap(c.get_structure(0)))
def maybe_rpt_cap(p): return maybe(lambda a: rpt_cap(a.get_current_caps())(p.emit('get-audio-pad', 0)))

def changes(state, x):
    if x == state[0]: return None
    state[0] = x; return x
def main():
    p = Gst.ElementFactory.make("playbin3", None)
    stop, pause, play = tuple(map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING]))
    durations = bind(changes, [0.0])
    wrapper = [identity] # MUTABLE cell . alternatively: [bind(tracef, 'player')]

    events = def_consult(const(None),
                   { MT.EOS:          compose(print_, const('eos'))
                   , MT.ERROR:        compose(rpt('error'), M.parse_error)
                   , MT.TAG:          compose(maybe(rpt('bitrate')), guard(pos), tl_bitrate, M.parse_tag)
                   , MT.DURATION_CHANGED: compose(maybe(compose(rpt('duration'), ns_to_s)), maybe(durations),
                                                  guard(pos), lambda _: p.query_duration(_FORMAT_TIME)[1])
                   , MT.STREAM_COLLECTION: compose(rpt_cap, lambda m: m.parse_stream_collection().get_stream(0).get_caps())
                   })
    def handle_msg(m): events(m.type)(m)
    def sync():     p.get_state(Gst.CLOCK_TIME_NONE)
    def position(): return ns_to_s(p.query_position(_FORMAT_TIME)[1])
    def seek(t):    return p.seek_simple(_FORMAT_TIME, Gst.SeekFlags.FLUSH, s_to_ns(t)), sync()
    def case(d):    return tuncurry(def_consult(lambda _: print_('unrecognised'), d))
    player = case({ 'stop':     lambda _: (stop(), p.set_property('uri', ''), durations(0.0))
                  , 'pause':    lambda _: pause()
                  , 'play':     lambda _: play()
                  , 'volume':   lambda a: p.set_property('volume', float(a[0]))
                  , 'position': lambda _: rpt('position')(position())
                  , 'id_pos':   lambda a: rpt('id_pos')('%s:%s' % (a[0], position()))
                  , 'seekrel':  lambda a: seek(float(a[0]) + position())
                  , 'seek':     lambda a: seek(float(a[0]))
                  , 'uri':      lambda a: (stop(), p.set_property('uri', a[0]), pause(), sync()) # maybe_rpt_cap(p))
                  , 'trace':    lambda a: wrapper.__setitem__(0, {'on': bind(tracef, 'player'), 'off': identity}[a[0]])
                  , '':         lambda _: (print_stderr('quitting'), exit())
                  })
    def handle_messages(bus):
        while True: handle_msg(bus.timed_pop(Gst.CLOCK_TIME_NONE))

    t = threading.Thread(target=handle_messages, args=[p.get_bus()])
    t.daemon = True; t.start()
    with Context(lambda: (stop, None)):
        while True: wrapper[0](player)(decons(sys.stdin.readline().rstrip().split(' ', 1)))

if __name__ == '__main__': main()
