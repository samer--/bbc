#!/usr/bin/env python
import sys
import gi
from pytools.basetools import bind, delay, print_, if_none_else, fork, curry, compose, fst, Context, const, tuncurry, decons, mul, divby
from pytools.dicttools import def_consult

gi.require_version('Gst', '1.0')
from gi.repository import GObject, Gst, GLib
# GObject.threads_init()
Gst.init(None)

MT = Gst.MessageType
M = Gst.Message
_FORMAT_TIME = Gst.Format(Gst.Format.TIME)
ns_to_s, s_to_ns = fork(divby, mul)(10 ** 9)

ignore = const(None)
def to_maybe((valid, x)): return x if valid else None
def quit((_, loop)): loop.quit()
def do(a,f): return compose(a, f, fst)
def rpt(l): return lambda x: print_('%s: %s' % (l, str(x)))
def tl_string(tl): return tl.to_string()
def tl_bitrate(tl): return to_maybe(tl.get_uint('bitrate'))
def io_watch(s, c, f): return lambda: (None, bind(GObject.source_remove, GObject.io_add_watch(s, c, f)))
def signal_watch(bus): return lambda: (bus.add_signal_watch(), bus.remove_signal_watch)
def read_command(s): return decons(s.readline().rstrip().split(' ', 1))
def fmt_cap(c): return '%s:%s:%s' % (to_maybe(c.get_int('rate')), c.get_string('format'), to_maybe(c.get_int('channels')))
ctrue = const(True)

@curry(1)
def bus_call(actions, bus, message, loop):
    actions(message.type)((message, loop))
    return True

def main():
    state = {'state': 'idle', 'duration': 0.0, 'bitrate': 0, 'error': None}
    sset = curry(1)(state.__setitem__)
    events = def_consult(ignore,
                   { MT.EOS:          fork(do(rpt('eos'), const('idle')), quit)
                   , MT.ERROR:        fork(do(fork(sset('error'), rpt('gst_error')), M.parse_error), quit)
                   , MT.TAG:          do(sset('bitrate'), compose(tl_bitrate, M.parse_tag))
                   , MT.STREAM_START: do(fork(sset('state'), print_), const('playing'))
                   })

    loop = GObject.MainLoop()
    p = Gst.ElementFactory.make("playbin", None)
    stop, pause, play = tuple(map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING]))
    vol_control = { 'volume': lambda a: ctrue(p.set_property('volume', float(a[0])))
                  , 'qvolume': lambda a: ctrue(rpt('volume')(p.get_property('volume'))) }
    def handler(d): return tuncurry(def_consult(lambda _: ctrue(print_('unrecognised')), dict(vol_control, **d)))
    player = handler({ 'stop':     lambda _: loop.quit()
                     , 'pause':    lambda _: (pause(), sset('state')('paused'))
                     , 'resume':   lambda _: (play(), sset('state')('playing'))
                     , 'volume':   lambda a: p.set_property('volume', float(a[0]))
                     , 'qvolume':  lambda a: rpt('volume')(p.get_property('volume'))
                     , 'duration': lambda _: rpt('duration')(ns_to_s(p.query_duration(_FORMAT_TIME)[1]))
                     , 'position': lambda _: rpt('position')(ns_to_s(p.query_position(_FORMAT_TIME)[1]))
                     , 'bitrate':  lambda _: rpt('bitrate')(state['bitrate'])
                     , 'state':    lambda _: rpt('state')(state['state'])
                     , 'seek':     lambda a: p.seek_simple(_FORMAT_TIME, Gst.SeekFlags.FLUSH, s_to_ns(float(a[0])))
                     , 'format':   lambda _: rpt('format')(fmt_cap(p.emit('get-audio-pad', 0).get_current_caps()[0]))
                     })

    def on_input(s, _):
        try: player(read_command(s))
        except Exception as ex: print "error", ex
        return True

    def go():
        with Context(io_watch(sys.stdin, GObject.IO_IN, on_input)):
            with Context(lambda: (None, stop)): 
                play(); sset('state')('preroll'); loop.run()

    top = handler({'quit': const(False), 'uri': lambda a: ctrue((p.set_property('uri', a[0]), go()))})
    bus = p.get_bus()
    bus.connect("message", bus_call(events), loop)
    with Context(signal_watch(bus)): 
        while top(read_command(sys.stdin)): pass

if __name__ == '__main__': main()