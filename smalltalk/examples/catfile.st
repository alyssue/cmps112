#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: catfile.st,v 1.11 2012-03-02 15:10:13-08 - - $"
execname := '$RCSfile: catfile.st,v $' =~ '.*: (.*),v.*' at: 1.

FileStream extend [
   nl := Character nl.
   catFile [
      |thisChar frequencyTable frequency|
"      frequencyTable := Dictionary new.
      "      [self atEnd not] whileTrue: [
         thisChar := self next.
"         frequency := frequencyTable keyAtValue: thisChar ifAbsent: [frequency := 0.]."
"         frequencyTable add: thisChar->[frequency+1.].
"         stdout << thisChar.
      ]
   ]
]

nl := Character nl.
Smalltalk arguments size = 0
   ifTrue: [stdin catFile]
   ifFalse: [
      Smalltalk arguments do: [:filename |
         |separator|
         separator := '::::::::::::::::::::::::::::::::'.
         stdout << separator << nl << filename << nl << separator << nl.
         filename = '-'
            ifTrue: [stdin catFile]
            ifFalse: [
               [
                  |infile|
                  infile := FileStream open: filename
                                       mode: FileStream read.
                  infile catFile.
                  infile close.
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]
      ]
   ]

"TEST: echo 'This is a test file.' >test.data"
"TEST: echo 'Hello, world!' >>test.data"
"TEST: echo 'foo bar baz qux' >>test.data"
"TEST: catfile.st test.data mks /dev/null"
"TEST: rm test.data"

