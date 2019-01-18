#!/usr/bin/env python
import sys
import gi
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
def divby(y):       return lambda x: x / y

class Context(object):
    def __init__(self, setup): self.setup = setup
    def __exit__(self, _1, _2, _3): self.cleanup()
    def __enter__(self):
        x, self.cleanup = self.setup()
        return x
# ----------- end of digest ------------

MT = Gst.MessageType
M = Gst.Message
_FORMAT_TIME = Gst.Format(Gst.Format.TIME)
ns_to_s, s_to_ns = fork(divby, mul)(10 ** 9)

ignore = const(None)
def to_maybe((valid, x)): return x if valid else None
def quit((_, loop)): loop.quit()
def do(a,f): return compose(a, f, fst)
def rpt(l): return lambda x: print_('%s %s' % (l, str(x)))
def tl_bitrate(tl): return snd(tl.get_uint('bitrate'))
def io_watch(s, c, f): return lambda: (None, bind(GObject.source_remove, GObject.io_add_watch(s, c, f)))
def signal_watch(bus): return lambda: (bus.add_signal_watch(), bus.remove_signal_watch)
def read_command(s): return decons(s.readline().rstrip().split(' ', 1))
def fmt_cap(c): return '%s:%s:%s' % (to_maybe(c.get_int('rate')), c.get_string('format'), to_maybe(c.get_int('channels')))
def find_cap(c): return c
def trace(l, x): print_('%s %s' % (l, x)); return x
def print_(s): sys.stdout.write(s); sys.stdout.write('\n'); sys.stdout.flush()
ctrue = const(True)

def main():
    state = {'state': 'idle', 'duration': 0.0, 'bitrate': 0, 'error': None}
    sset = delay(state.__setitem__)
    events = def_consult(ignore,
                   { MT.EOS:          fork(compose(print_, const('eos')), compose(sset('state'), const('stopped')))
                   , MT.ERROR:        fork(do(fork(sset('error'), rpt('gst_error')), M.parse_error), quit)
                   , MT.TAG:          do(sset('bitrate'), compose(tl_bitrate, M.parse_tag))
                   , MT.STREAM_START: do(print_, const('started'))
                   })

    loop = GObject.MainLoop()
    p = Gst.ElementFactory.make("playbin", None)
    stop, pause, play = tuple(map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING]))
    common = { 'volume': lambda a: ctrue(p.set_property('volume', float(a[0])))
             , 'qvolume': lambda a: ctrue(rpt('volume')(p.get_property('volume')))
             , 'qdevice':  lambda a: ctrue(rpt('device')(p.get_property('audio-sink').get_property('device')))
             , 'device':   lambda a: ctrue(p.get_property('audio-sink').set_property('device', a[0]))
             , 'print':    lambda a: ctrue(print_(a[0]))
             }
    def sync():     return print_('sync'), p.get_state(1000000000)
    def position(): return ns_to_s(p.query_position(_FORMAT_TIME)[1])
    def seek(t):    return p.seek_simple(_FORMAT_TIME, Gst.SeekFlags.FLUSH, s_to_ns(t)), sync()
    def handler(d): return tuncurry(def_consult(lambda _: ctrue(print_('unrecognised')), dict(common, **d)))
    player = handler({ 'close':    lambda _: loop.quit()
                     , 'stop':     lambda _: (stop(), sset('state')('stopped'))
                     , 'pause':    lambda _: (pause(), sset('state')('paused'))
                     , 'resume':   lambda _: (play(), sset('state')('playing'))
                     , 'volume':   lambda a: p.set_property('volume', float(a[0]))
                     , 'qvolume':  lambda a: rpt('volume')(p.get_property('volume'))
                     , 'quri':     lambda a: rpt('uri')(p.get_property('uri'))
                     , 'duration': lambda _: rpt('duration')(ns_to_s(p.query_duration(_FORMAT_TIME)[1]))
                     , 'position': lambda _: rpt('position')(position())
                     , 'bitrate':  lambda _: rpt('bitrate')(state['bitrate'])
                     , 'state':    lambda _: rpt('state')(state['state'])
                     , 'seekrel':  lambda a: seek(float(a[0]) + position())
                     , 'seek':     lambda a: seek(float(a[0]))
                     , 'format':   lambda _: rpt('format')(fmt_cap(p.emit('get-audio-pad', 0).get_current_caps().get_structure(0)))
                     })

    def on_message(_, message, loop): return ctrue(events(message.type)((message, loop)))
    def on_input(s, _):
        try: player(read_command(s))
        except Exception as ex: print "error", ex
        return True

    def go():
        with Context(io_watch(sys.stdin, GObject.IO_IN, on_input)):
            with Context(lambda: (None, stop)):
                pause(); sync(); sset('state')('paused'); loop.run()

    top = handler({'quit': const(False), 'uri': lambda a: ctrue((p.set_property('uri', a[0]), go()))})
    bus = p.get_bus()
    bus.connect("message", on_message, loop)
    with Context(signal_watch(bus)):
        while top(read_command(sys.stdin)): pass

if __name__ == '__main__': main()
