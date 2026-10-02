#!/usr/bin/env python3
import sys
import gi
import os
import json
import threading
import operator as op
import importlib.util
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
def not_empty(x):   return len(x) > 0
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
def apply_to(x,f): return f(x)
def with_as(r,f):
    with r as x: return f(x)

def json_read(f): return with_as(open(f, 'r'), json.load)
def unsingleton(x): (x,) = x; return x
def find_unique(p, xs): return unsingleton(filter(p, xs))

_Null = object()
def memoise(f):
    memo = {}
    def g(*x):
        y = memo.get(x, _Null)
        if y is _Null:
            y = f(*x)
            memo[x] = y
        return y
    return g

pos = bind(op.lt, 0)

class Context(object):
    def __init__(self, setup): self.setup = setup
    def __exit__(self, _1, _2, _3): self.cleanup()
    def __enter__(self):
        self.cleanup, x = self.setup()
        return x
# ----------- end of digest ------------

no_overrides = importlib.util.find_spec('gi.overrides.Gst') is None
with_structure = apply_to if no_overrides else with_as

MT = Gst.MessageType
M = Gst.Message
_FORMAT_TIME = Gst.Format(Gst.Format.TIME)
ns_to_s, s_to_ns = fork(divby, mul)(10 ** 9)
mutex = threading.Lock()
lock = Context(lambda: (mutex.release, mutex.acquire()))

def to_maybe(t): valid, x = t; return x if valid else None
def maybe_int(c, k): return to_maybe(c.get_int(k))
def rpt(l): return lambda x: print_('%s %s' % (l, str(x)))
def tl_bitrate(tl): return snd(tl.get_uint('bitrate'))
def tr(x): sys.stderr.write('%s\n' % str(x)); return x
def print_(s):
    with lock: print(s); sys.stdout.flush()
def fmt_cap(c): return '%s:%s:%s' % (maybe_int(c,'rate'), c.get_string('format') or 'F', maybe_int(c, 'channels'))
def rpt_cap(c): return rpt('format')(with_structure(c.get_structure(0), fmt_cap))
def stream_caps(m): return m.parse_stream_collection().get_stream(0).get_caps()

def changes(state, x):
    if x == state[0]: return None
    state[0] = x; return x

yt_fmt = os.getenv('GST12_YTDLP_FORMAT', '251')
yt_dlp_use_cmd = bool(int(os.getenv('GST12_YTDLP_USE_CMD','0')))

@memoise
def yt_dlp():
    import yt_dlp
    params = maybe(json_read)(guard(not_empty)(os.getenv('GST12_YTDLP_CONFIG')))
    tr('gst12: Initialising yt-dlp with %s...' % params)
    yt=yt_dlp.YoutubeDL(params=dict(params or {}, logtostderr=True), auto_init=False)
    yt.add_info_extractor(yt.get_info_extractor('Youtube'))
    tr('gst12: Initialised yt-dlp object.')
    return yt

def youtube_url_api(url):
    i = yt_dlp().extract_info(url, download=False)
    def pred(i): return i['format_id'] == yt_fmt
    tr('formats: %s' % ' '.join([f['format_id'] for f in i['formats']]))
    return find_unique(pred, i['formats'])['url']

def youtube_url_cmd(url):
    import subprocess
    args = ['yt-dlp', '-v', '--format', 'bestaudio', '--print', 'urls', url]
    return subprocess.check_output(args, text=True)

def main():
    tr('gst12: gst overrides? %s, yt-dlp command? %s' % (not no_overrides, yt_dlp_use_cmd))
    youtube_url = youtube_url_cmd if yt_dlp_use_cmd  else youtube_url_api
    def url(x): return youtube_url(x) if 'www.youtube.com' in x else x

    p = Gst.ElementFactory.make("playbin3", None)
    stop, pause, play = map(delay(p.set_state), [Gst.State.NULL, Gst.State.PAUSED, Gst.State.PLAYING])
    durations = bind(changes, [0.0])
    wrapper = [identity] # MUTABLE cell . alternatively: [bind(tracef, 'player')]

    events = def_consult(const(None),
                   { MT.EOS:          compose(print_, const('eos'))
                   , MT.ERROR:        compose(rpt('error'), M.parse_error)
                   , MT.TAG:          compose(maybe(rpt('bitrate')), guard(pos), tl_bitrate, M.parse_tag)
                   , MT.DURATION_CHANGED: compose(maybe(compose(rpt('duration'), ns_to_s)), maybe(durations),
                                                  guard(pos), lambda _: p.query_duration(_FORMAT_TIME)[1])
                   , MT.STREAM_COLLECTION: compose(rpt_cap, stream_caps)
                   })
    def handle_msg(m): events(m.type)(m)
    def sync():     p.get_state(Gst.CLOCK_TIME_NONE)
    def position(): return ns_to_s(max(0, p.query_position(_FORMAT_TIME)[1]))
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
                  , 'uri':      lambda a: (stop(), p.set_property('uri', url(a[0])), pause(), sync())
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
