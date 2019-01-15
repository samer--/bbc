#!/usr/bin/env python
import json
from urllib import urlopen
from pytools.basetools import with_resource, extend, conj, fst, snd, delay, for_each, print_
from pytools.dicttools import has_key_with_val, key_pair
from pytools.optionparsers import pos, a, opt, val, default, one_of, run, flag, with_parsed_args

has = delay(has_key_with_val)
def lguard(pred, x): return [x] if pred(x) else []

transfer_format, protocol, bitrate = tuple(map(has, ['transferFormat', 'protocol', 'bitrate']))
mediasets = ['pc', 'audio-syndication', 'audio-syndication-dash', 'apple-ipad-hls', 'iptv-all',
             'apple_icy-mp3a', 'http-icy-aac-lc-a']
def playlist(pid): return json.load, ('http://www.bbc.co.uk/programmes/%s/playlist.json' % pid)
def mediaset(ver, mediaset, vpid):
    return json.load, ('http://open.live.bbc.co.uk/mediaselector/%s/select/version/2.0/mediaset/%s/format/json/vpid/%s' % (ver, mediaset, vpid))

def pair_with_closer(s): return s, s.close
def uget((parse, url)): return with_resource(lambda: pair_with_closer(urlopen(url)), parse)
def vpid(info): return info['defaultAvailableVersion']['pid']

def dash_for_ms(pred, ms):
    def dash_for_conn(conn):   return map(key_pair('priority', 'href'), lguard(pred, conn))
    def dash_for_media(media): return extend(dash_for_conn, media['connection'])
    return extend(dash_for_media, ms['media'])

@with_parsed_args('BBC stream lookup',
                  [ pos('pid', a(str), 'PID of desired programme')
                  , opt('-v', '--is_vpid',  flag, 'If true, then PID is already a VPID, no need to lookup')
                  , opt('-a', '--all',      flag, 'If true, then print all matching stream URLs')
                  , opt('-i', '--index',    val(a(int), default(0)), 'Index of URL in sorted list if not printing all')
                  , opt('-r', '--ver',      val(one_of(['5', '6']), default('6')), 'mediaselector version number')
                  , opt('-f', '--format',   val(one_of(['dash', 'hls']), default('dash')), 'Transfer format')
                  , opt('-p', '--protocol', val(one_of(['http', 'https']), default('http')), 'Transfer protocol')
                  , opt('-m', '--mediaset', val(one_of(mediasets), default('pc')), 'Mediaset identifier')
                  ])
def main(opts):
    ms = uget(mediaset(opts.ver, opts.mediaset, opts.pid if opts.is_vpid else vpid(uget(playlist(opts.pid)))))
    urls = map(snd, sorted(dash_for_ms(conj(transfer_format(opts.format), protocol(opts.protocol)), ms), key=fst))
    for_each(print_, urls if opts.all else urls[opts.index:opts.index+1])

if __name__ == '__main__': run(main)
