#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: ashex.st,v 1.4 2018-11-07 12:01:39-08 - - $"

Character extend [
   asHexString [
      |int hexchars|
      hexchars := '0123456789ABCDEF'.
      int := self asInteger.
      ^ (hexchars at: (int quo: 16) + 1) asString,
        (hexchars at: (int rem: 16) + 1) asString.
   ]
].

[stdin atEnd] whileFalse: [
   |char|
   char := stdin next.
   stdout << char << ': ' << char asHexString << Character nl.
]

