# bbc
Tools for working with BBC content and metadata.

This is currently a bit of a mish-mash of half-baked ideas for managing BBC content
and playing it without having to go through the BBC website (iPlayer or Sounds) in
a web-browser. This means they can be used on a headless device such as you might 
have connected to a hi-fi. Still, there are some usable bits:

## bbc.py - Python script to get media stream URL from programme id

Run `./bbc.py -h` for help. Writes stream URLs for a given programme id (PID)
or version programme id (VID) on standard output, which you can then send to
any player program, eg GStreamer's gst123. Suppose `m0001pti` is a PID, you can
play the defaul MPEG-DASH stream like this:

    $ gst123 `./bbc.py m0001pti`

Command line flags allow you to choose a different stream format (eg HLS),
to select by VPID rather than PID, and to write all available stream URLs
(or a particular one) instead of just the default. Incidentally, the BBC
radio channel names are valid VPIDs, so you can play a high-quality (320 kb/s)
BBC Radio 3 stream using

    $ gst123 -f hls -v bbc_radio_three

## bbc.pl - Prolog tools for working with the current radio schedules

This Prolog script allows you to get the current schedule for any BBC
radio station, search for programmes, play them using your preferred
progam (eg `gst213`, `gst-play-1.0`, `mplayer` etc), or write-out an
MPD (music player daemon) compatible playlist for browsing the schedule
via your MPD client. 

The basic idea seems sound, and works up to a point, but the URLs provided
by the BBC are time-limited and need refreshing quite often. Hence, it needs
work on the database management side to keep the set of available programmes
and their stream URLs up to date.

## swiplayer - Prolog-based web server for exploring BBC content

This not even quarter-baked, just the beginnings of an idea -- come back later!
