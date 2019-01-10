#!/usr/bin/env python
import sys
import gi
from pytools.optionparsers import pos, a, run, with_parsed_args, val, default, opt
from pytools.basetools import bind, delay, print_, fork, curry, compose, fst, Context, const, tuncurry, decons, mul, divby
from pytools.dicttools import def_consult

gi.require_version('Gst', '1.0')
from gi.repository import GObject, Gst, GLib
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
def playbin(): return Gst.ElementFactory.make("playbin", None)
def pipeline(p): return lambda: (None, bind(p.set_state, Gst.State.NULL))
def io_watch(s, c, f): return lambda: (None, bind(GObject.source_remove, GObject.io_add_watch(s, c, f)))
def signal_watch(bus): return lambda: (bus.add_signal_watch(), bus.remove_signal_watch)
def to_uri(x): return x if Gst.uri_is_valid(x) else Gst.filename_to_uri(x)

@curry(1)
def bus_call(actions, bus, message, loop):
    actions(message.type)((message, loop))
    return True

@with_parsed_args('GStreamer player', [pos('spec', a(str), 'Content file name or URL'),
                                       opt('-v', '--volume', val(a(float), default(0.1)), 'Initial volume in [0, 1]')])
def main(opts):
    p = playbin()
    p.set_property('uri', to_uri(opts.spec))
    pad = p.emit('get-audio-pad', 0)
    bus = p.get_bus()
    loop = GObject.MainLoop()
    state = {'state': 'idle', 'duration': 0.0, 'bitrate': 0, 'error': None}
    sset = curry(1)(state.__setitem__)

    events = def_consult(ignore,
                   { MT.EOS:              do(fork(rpt('EOS'), sset('state')), const('idle'))
                   , MT.ERROR:            fork(do(fork(sset('error'), rpt('ERROR')), M.parse_error), quit)
                   , MT.TAG:              do(sset('bitrate'), compose(tl_bitrate, M.parse_tag))
                   , MT.STREAM_START:     do(fork(sset('state'), rpt('STREAM_START')), const('playing'))
                   })

    stop, pause, play = tuple(map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING]))
    caps = lambda: p.emit('get-audio-pad', 0).get_current_caps()[0]
    commands = tuncurry(def_consult(lambda _: print_('Unrecognised'), 
                   { 'stop':   lambda _: loop.quit()
                   , 'pause':  lambda _: (pause(), sset('state')('paused'))
                   , 'resume': lambda _: (play(), sset('state')('playing'))
                   , 'volume': lambda a: p.set_property('volume', float(a[0]))
                   , 'duration': lambda _: rpt('duration')(ns_to_s(p.query_duration(_FORMAT_TIME)[1]))
                   , 'position': lambda _: rpt('position')(ns_to_s(p.query_position(_FORMAT_TIME)[1]))
                   , 'bitrate': lambda _:  rpt('bitrate')(state['bitrate'])
                   , 'state': lambda _:  rpt('state')(state['state'])
                   , 'seek': lambda a: p.seek_simple(_FORMAT_TIME, Gst.SeekFlags.FLUSH, s_to_ns(float(a[0])))
                   , 'caps': lambda _: rpt('caps')(p.emit('get-audio-pad', 0).get_current_caps()[0])
                   , 'eval': lambda a: print_(eval(' '.join(a), {'caps': caps()}))
                   }))
    def handle_input(s, _):
        try: commands(decons(s.readline().rstrip().split(' ')))
        except Exception as ex: print "Error handling command", ex
        return True

    bus.connect("message", bus_call(events), loop)
    with Context(io_watch(sys.stdin, GObject.IO_IN, handle_input)):
        with Context(signal_watch(bus)): 
            with Context(pipeline(p)): 
                state['state'] = 'preroll'
                p.set_property('volume', opts.volume); play()
                try: loop.run()
                except KeyboardInterrupt: pass

if __name__ == '__main__': run(main)
