

hzip.st -dtcu inputfile [outputfile]


" ------------------------------------------------------------------------------ "
" The compression algorithm reads the input file to construct the decoding tree. "
" ------------------------------------------------------------------------------ "


" Define BitStream Object"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]


" Define Leaf Object"

Object subclass: Leaf [
   |char count|
   <comment: 'A leaf or node in our Tree'>

   char [ ^char ]
   count [ ^count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new. "creates Leaf OBJECT"
      result setChar: aChar andCount: aCount.
      ^result
   ]


   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
]



" ------------------------------------------------------------------------ "
" -d
  Debug information is printed for the benefit of the application author.
  Exact details are not specified. "
" ------------------------------------------------------------------------ "




" ------------------------------------------------------------------------ "
" -t
  The compression algorithm is activated, and the decoding tree is printed
  to the standard output. The output filename may not be specified. "
" ------------------------------------------------------------------------ "

If the -t option is specified, write out the encoding table sorted by character.
The first column is a single character, if printable, or an integer if not. 
The second column is the frequency for that character. 
The third column is a sequence of 0 and 1 characters indicating the encoding. 
Format should appear as if done by one of the following format items. 
In the table, the symbol ‘‘_’’ represents a space character.










" ------------------------------------------------------------------------ "
" -c
  The input file is compressed and written to the output file, if specified, or
  to the standard output, if not. "
" ------------------------------------------------------------------------ "
if -c rather than the -t, the compression algorithm reads the input file to perform
the compression and proceeds as follows :

" Read in the input file "
FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self next << nl.
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


and create a frequency table, 
counting the number of times each character appears on input. 
The frequency table is indexed from 0 to 255 with characters. 
Add entry 256 with a count of 1 to indicate EOF.

" Declaration of frequencyTable"
frequencyTable := array new: 256
frequencyTable at: 256 put: 1 "indicate EOF."


Iterate over the frequency table, 
and for each non-zero element, 
create a leaf node and insert that leaf node into a priority queue, 
with the character and the count. 

In Smalltalk, use a SortedCollection. 

The counts take precedence, but if two entries have the same count, 
the one with the smaller character (lexicographically) is considered smaller.



Repeatedly remove the two smallest elements from the priority queue, 
creating a new tree which is then entered into the priority queue. 
The smaller tree or leaf removed becomes the left child, 
and the larger the right child. 

The charcter in the new tree is the left child’s character. 
This process stops when there is only one tree left and the priority queue is empty.



For each character that has appeared as non-zero in the frequency table, 
construct an encoding string, using a depth-first traversal. 
The encoding string is a sequence of bits indicating the path from the root to a leaf.


If the -t option is not specified, write out the encoding table as follows : 
Perform a post-order traversal of the decoding tree, 
writing out one bit at a time in big-endian format. 
for each leaf, write out a 0 bit, followed by the 8 bits of the corresponding byte. 
Write out the bits in the order bit 7, bit 6, . . ., bit 0, that is high bit first. 
As a special case, if the byte is 0, write out bit 8, which will be a 0 for a byte value of 0, 
and 1 for a byte value of 256 (the EOF marker).


For each interior node, write out a 1 bit. When the tree is completely written
out, write another 1 bit to indicate the end of the tree.


Reopen the input file and write out the encoded version of each byte. At end of
the buffer, write out the encoding string for EOF. Then pad with 0 bits out to a
byte boundary, if needed.



" ------------------------------------------------------------------------ "
" -u
  The input file is assumed to be compressed, and is uncompressed, written
  to the output file, if specified, or to the standard output, if not. "
" ------------------------------------------------------------------------ "

To uncompress a file, consider that the compressed file has a decoding tree followed
by data. It is possible that the compressed version is large than the uncompressed
version. Proceed as follows :


(a) Reconstruct the Huffman decoding tree. Read one bit.


(b) If it is a 0, read the next 8 bits and reconstruct the byte, giving a value from 0
to 255. If the value is 0, read one more bit, and if it is a 1, add 256 to the byte,
giving the encoding for EOF. Then push this byte onto a stack.


(c) If it is a 1, pop the 1-subtree from the stack, then pop the 0-subtree from the
stack, and create a new tree with both of those children and push the new tree
back on the stack.


(d) The last extra 1 bit will be read when there is only one tree on the stack. Popping
this tree will cause the stack to be empty, so this is the decoding tree.


(e) Now loop over the rest of the input file to reconstruct the original file : Initialize
a pointer to the root of the decoding tree.


(f) Read a single bit and use it to move down the 0-link or the 1-link to the child
of the current node.


(g) If thise node is a leaf, write out the corresponding byte and reset the pointer
back to the root of the tree.


(h) If not, continue reading bits until you find a leaf.






