#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"---------------------------------------------------------------------------------------"
"  Alyssa Melton
   CMPS 112: Fall 2018
   Asg 3: A Compressor in SmallTalk
   I worked independantly on this assignment, no partner specified."
"---------------------------------------------------------------------------------------"

"---------------------------------------------------------------------------------------"
"  CLASSES, SUBCLASSES, AND OBJECT EXTENSIONS  "
"---------------------------------------------------------------------------------------"

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


Object subclass: OutBits [
  |bitIndex currentByte myStream|

  OutBits class >> new [
    self shouldNotImplement.
  ]
  OutBits class >> new: fileStream [
    |result|
    result := super new.
    result init: fileStream.
    ^result
  ]
  clearByte [
    bitIndex := 8.
    currentByte := 0.
  ]
  init: fileStream [
    myStream := fileStream.
    self clearByte.
  ]
  flushByte [
    bitIndex = 8 ifFalse: [
       myStream nextPutByte: currentByte.
       self clearByte.
    ]
  ]
  writeBit: bit [
    currentByte := currentByte bitAt: bitIndex put: bit.
    bitIndex := bitIndex - 1.
    bitIndex = 0 ifTrue: [self flushByte].
  ]

  writeString: bitstring [
    bitstring do: [ :bit |
      self writeBit: bit asInteger.
    ].
  ]

]


"---------------------------------------------------------------------------------------"

Object subclass: Leaf [
  |char count|

  char [ ^char ]
  count [ ^count ]
  isLeaf [ ^ true ]

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

  postorder: visitor [
      visitor value: char value: true.
  ]

]

"---------------------------------------------------------------------------------------"

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

  postorder: visitor [
    left postorder: visitor.
    right postorder: visitor.
    visitor value: (self char) value: false.
  ]

  isLeaf [
    ^ false
  ]

  leftChild [ ^ left ]
  rightChild [ ^ right ]
]

"---------------------------------------------------------------------------------------"

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

"---------------------------------------------------------------------------------------"

FileStream extend [
   nl := Character nl.

   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]

   getFrequencyTable [
      |frequencyTable |
      frequencyTable := ZeroArray new: 257.
      frequencyTable incr: 256. "indicate EOF."
      [self atEnd not] whileTrue: [
        | thisChar charAsInt |
        thisChar := self next.
        charAsInt := thisChar asInteger.
        frequencyTable incr: charAsInt. 
      ].
      ^frequencyTable
   ]
]

"---------------------------------------------------------------------------------------"

Number extend [
  asHexString: suffix [
    | remainder hexRep hexValue result |
    remainder := self \\ 16.
    result := self // 16.
    remainder < 10 ifTrue: [ hexRep := result asString.].
    remainder = 10 ifTrue: [ hexRep := 'A'.].
    remainder = 11 ifTrue: [ hexRep := 'B'.].
    remainder = 12 ifTrue: [ hexRep := 'C'.].
    remainder = 13 ifTrue: [ hexRep := 'D'.].
    remainder = 14 ifTrue: [ hexRep := 'E'.].
    remainder = 15 ifTrue: [ hexRep := 'F'.].

    result = 0 
      ifTrue: [ hexValue := suffix, hexRep. ] 
      ifFalse: [ hexValue := suffix, (result asHexString: hexRep). ].
    ^hexValue 
  ]
]

"---------------------------------------------------------------------------------------"

Character extend [
   asHexString [
      |int hexchars|
      hexchars := '0123456789ABCDEF'.
      int := self asInteger.
      ^ (hexchars at: (int quo: 16) + 1) asString,
        (hexchars at: (int rem: 16) + 1) asString.
   ]
].

"---------------------------------------------------------------------------------------"
"  INITITIALIZATION  "
"---------------------------------------------------------------------------------------"

  nl := Character nl.
  dctuExists := false.
  flagD := false.
  flagC := false.
  flagT := false.
  flagU := false.
  filename1 := ''.
  filename2 := stdout.
  numArgs := Smalltalk arguments size.

  Smalltalk arguments do: [:arg |
      (arg includes: $-) "is this arg -d -c -t or -u?"
        ifTrue: [
           dctuExists := true.
           (arg includes: $d)
              ifTrue: [flagD := true.].
           (arg includes: $c) 
              ifTrue: [flagC := true.].
           (arg includes: $t)
              ifTrue: [flagT := true.].
           (arg includes: $u)
              ifTrue: [flagU := true.].
      ] ifFalse: [ " if not -dtcu, then is a filename"
           dctuExists ifTrue: [
              filename1 isEmpty "we will assign the next arguement that is not '-dctu' to infile"
                 ifTrue: [ filename1 := arg. ] 
                 ifFalse: [ filename2 :=  arg.].
           ] ifFalse: [ dctuExists error: 'requires -dctu arg'. ].
     ].
  ].

  " which flags "
  flagD ifTrue: [
      '-d = TRUE: Displaying debugging information.' display.
      nl display.

      flagT ifTrue: [
          '-t = TRUE: Should print the decoding tree.' display.
          nl display.
      ].

      flagC ifTrue: [
        '-c = TRUE: Compress File.' display.
        nl display.
        
        'infile: ' display.
        infile display.
        nl display.

        'outfile: ' display.
        outfile display.
        nl display.
      ].

      flagU ifTrue: [
        '-u = TRUE: Decompress File.' display.
        nl display.
        
        'infile: ' display.
        infile display.
        nl display.

        'outfile: ' display.
        outfile display.
        nl display.
      ].   
  ].

  " filenames "
  flagD ifTrue: [ 
     'The value of filename1 is : ' display.
     filename1 inspect. 
     nl display.

     'The value of filename2 is : ' display.
     filename2 inspect.
     nl display.
  ].

  " open files for reading and writing "
  infile := FileStream open: filename1
                       mode: FileStream read.

  (filename2 isString)
    ifTrue: [outfile := FileStream open: filename2 
                                   mode: FileStream write.
    ] ifFalse: [outfile := filename2.].

"---------------------------------------------------------------------------------------"
" -C = TRUE or -T = TRUE"
"---------------------------------------------------------------------------------------"

(flagC | flagT) ifTrue: [

  "(A) Read in the input file and create a frequency table counting 
  the number of times each character appears on input. 
  The frequency table is indexed from 0 to 255 with characters. 
  Add entry 256 with a count of 1 to indicate EOF."
  frequencyTable := infile getFrequencyTable.

  flagD ifTrue: [ 
     'The frequencyTable after creation: ' display.
     frequencyTable inspect. 
  ].

  "---------------------------------------------------------------------------------------"
  "(B) Iterate over the frequency table, 
  and for each non-zero element, 
  create a leaf node and insert that leaf node into a priority queue, 
  with the character and the count. 
  In Smalltalk, use a SortedCollection. 

  The counts take precedence, but if two entries have the same count, 
  the one with the smaller character (lexicographically) is considered smaller."

  priorityQueue := SortedCollection new.
  0 to: 256 do: [ :asciiVal |
     | aChar aCount newLeaf |
     aCount := frequencyTable at: asciiVal. 
     aCount isNil ifFalse: [ "if character exists in file"
           aChar := asciiVal asCharacter.
           newLeaf := Leaf new: aChar count: aCount.
           priorityQueue add: newLeaf.
        ].
  ].

  flagD ifTrue: [ 
     'The priorityQueue after creation with leaves: ' display.
     priorityQueue inspect. 
  ].


  "---------------------------------------------------------------------------------------"
  "(C) Repeatedly remove the two smallest elements from the priority queue, 
  creating a new tree which is then entered into the priority queue. 
  The smaller tree or leaf removed becomes the left child, 
  and the larger the right child.
  The charcter in the new tree is the left child’s character.
  This process stops when there is only one tree left and the priority queue is empty."

  [priorityQueue size > 1] whileTrue: [ 
     | first second sum leftChar newTree |
     first := priorityQueue removeFirst.
     second := priorityQueue removeFirst.
     sum := first count + second count.
     leftChar := first char.

     newTree := Tree new: leftChar count: sum left: first right: second.
     priorityQueue add: newTree.
  ]. 

  flagD ifTrue: [ 
     'The priorityQueue after creating the tree out of the Leaves: ' display.
     priorityQueue inspect.
  ].

  "---------------------------------------------------------------------------------------"
  "(D) For each character that has appeared as non-zero in the frequency table, 
  construct an encoding string, using a depth-first traversal. 
  The encoding string is a sequence of bits indicating the path from the root to a leaf."

  encodeTable := ZeroArray new: 257.
  treeOfChars := priorityQueue removeFirst. 
  treeOfChars inorder: [:char :string |
     | asciiVal |
     asciiVal := char asciiValue.
     encodeTable at: asciiVal put: string.
  ] prefix: ''.

  flagD ifTrue: [ 
     'The encodeTable after creation: ' display.
     encodeTable inspect. 
  ].
].


"---------------------------------------------------------------------------------------"
" -T = TRUE "
"---------------------------------------------------------------------------------------"
" -t
  The decoding tree is printed to the standard output. 
  The output filename may not be specified. "
"---------------------------------------------------------------------------------------"


flagT ifTrue: [

  "(E) If the -t option is specified, write out the encoding table sorted *by character*.
  The first column is a single character, if printable, or an integer if not. 
  The second column is the frequency for that character. 
  The third column is a sequence of 0 and 1 characters indicating the encoding. 
  Format should appear as if done by one of the following format items. 
  In the table, the symbol ‘‘_’’ represents a space character."

  0 to: 256 do: [ :asciiVal |
    | thisChar printThis bitCode frequency |
    "printable characters include those with ascii values 33 - 126"
    printable := (32 < asciiVal) & (asciiVal < 127).   
    frequency := frequencyTable at: asciiVal.
    frequency isNil ifFalse: [
      bitCode := encodeTable at: asciiVal.
      "print cases: "
      printable 
        ifTrue: [ 
            printThis := asciiVal asCharacter.
            outfile << ' ' << printThis << ' '.
        ] ifFalse: [
          (asciiVal = 256) 
          ifTrue: [ 
            printThis := 'EOF'.
            outfile << printThis.
          ] ifFalse: [  
            printThis := asciiVal asHexString: ''.
          (printThis size = 1)
            ifTrue: [ printThis := 'x0', printThis. ]
            ifFalse: [ printThis := 'x', printThis. ].
            outfile << printThis. ].
        ].

        "print all that occur"
        outfile << '    '. 

        (frequency < 1000)
        ifTrue: [ outfile << ' ' ].
        (frequency < 100)
        ifTrue: [ outfile << ' '  ].
        (frequency < 10)
        ifTrue: [ outfile << ' '  ].

        outfile << frequency << '  ' << bitCode << nl.
    ].
  ].

  infile close.
  outfile close.
]. 


"---------------------------------------------------------------------------------------"
" -C = TRUE "
"---------------------------------------------------------------------------------------"
" -c
  The input file is compressed and written to the output file, if specified, or
  to the standard output, if not. "
"---------------------------------------------------------------------------------------"

flagC ifTrue: [
  " If the -t option is not specified, write out the encoding table as follows: "
  "(F) Perform a post-order traversal of the decoding tree, 
  writing out one bit at a time in big-endian format. 
  for each leaf, write out a 0 bit, followed by the 8 bits of the corresponding byte.
  Write out the bits in the order bit 7, bit 6, . . ., bit 0, that is high bit first. 
  As a special case, if the byte is 0, write out bit 8, which will be a 0 for a byte value of 0, 
  and 1 for a byte value of 256 (the EOF marker)."
  
  flagD ifTrue: [ 
     'inside flagC marker: nothing excecuted.' display.
     nl display.
  ].

  bitsOut := OutBits new: outfile.

  flagD ifTrue: [ 
     'inside flagC marker 2: bitsOut created, before post-order' display.
     nl display.
  ].


  "Perform a post-order traversal of the decoding tree"
  treeOfChars postorder: [ :char :isALeaf |
    isALeaf ifTrue: [
      | zeroVal |
      "write out a 0 bit followed by the 8 bits of the corresponding byte"
      bitsOut writeBit: 0.

      "Write out the bits in the order bit 7, bit 6, . . ., bit 0, that is high bit first."
      zeroVal := true.
      (8 to: 1 by: -1) do: [ :i|
            bitsOut writeBit: ((char asciiValue) bitAt: i).
            ((char asciiValue) bitAt: i) = 0 ifFalse: [zeroVal := false.].
      ].
      zeroVal ifTrue: [ bitsOut writeBit: 1. ].
    ] ifFalse: [
      bitsOut writeBit: 1.
    ].
  ].
  bitsOut writeBit: 1.


  flagD ifTrue: [ 
     'inside flagC marker 3: after postorder.' display.
     nl display.
  ].

  


  "(H) Reopen the input file and write out the encoded version of each byte. At end of
  the buffer, write out the encoding string for EOF. Then pad with 0 bits out to a
  byte boundary, if needed."

  infile reset.
  [infile atEnd not] whileTrue: [
  | thisChar asciiVal encodedString |
    thisChar := infile next.
    asciiVal := thisChar asciiValue.
    encodedString := encodeTable at: asciiVal.
    bitsOut writeString: encodedString.
  ].

  "At end of the buffer, write out the encoding string for EOF"
  encodedString := encodeTable at: 256.
  bitsOut writeString: encodedString.

  bitsOut flushByte.
  infile close.
  outfile close.
].



"---------------------------------------------------------------------------------------"
" U = TRUE"
"---------------------------------------------------------------------------------------"
" -u
  The input file is assumed to be compressed, and is uncompressed, written
  to the output file, if specified, or to the standard output, if not. "
"---------------------------------------------------------------------------------------"
"To uncompress a file, consider that the compressed file has a decoding tree followed
by data. It is possible that the compressed version is large than the uncompressed
version. Proceed as follows :"

flagU ifTrue: [
  "(a) Reconstruct the Huffman decoding tree. Read one bit."
  readBitStream := BitStream new: infile.
  stack := OrderedCollection new.
  decodingTreeComplete := false.

  [decodingTreeComplete = true] whileFalse: [
    thisBit := readBitStream nextBit.

    "(b) If it is a 0, read the next 8 bits and reconstruct the byte, giving a value from 0
    to 255. If the value is 0, read one more bit, and if it is a 1, add 256 to the byte,
    giving the encoding for EOF. Then push this byte onto a stack."
    (thisBit = 0) 
    ifTrue: [ 
      | baseTwoValue asciiVal |
      baseTwoValue := 128.
      asciiVal := 0.
      (1 to: 8) do: [ :skip |
        thisBit := readBitStream nextBit. 
        (thisBit = 1) ifTrue: [
          asciiVal := asciiVal + baseTwoValue.
        ].
        baseTwoValue := baseTwoValue / 2.
      ].

      (asciiVal = 0) ifTrue: [ 
        thisBit := readBitStream nextBit. 
        (thisBit = 1) ifTrue: [ asciiVal := 256. ]. 
      ].

      flagD ifTrue: [ 
        'asciiVal: ' display.
        asciiVal display.
        nl display.

        'asciiVal asCharacter: ' display.
        asciiVal asCharacter display.
        nl display.
        nl display.
      ].

      (asciiVal = 256) 
      ifTrue: [ stack addFirst: (Leaf new: 'EOF' count: 0). ] 
      ifFalse:[ stack addFirst: (Leaf new: (asciiVal asCharacter) count: 0).].
      
    ]

    "(c) If it is a 1, pop the 1-subtree from the stack, then pop the 0-subtree from the
    stack, and create a new tree with both of those children and push the new tree
    back on the stack." 
    ifFalse: [ 
      (stack size > 1) 
      ifTrue: [
        | leftChild rightChild |
        rightChild := stack removeFirst.
        leftChild := stack removeFirst.
        stack addFirst: (Tree new: (leftChild char) count: 0 left: leftChild right: rightChild).
      ]
      "(d) The last extra 1 bit will be read when there is only one tree on the stack. Popping
      this tree will cause the stack to be empty, so this is the decoding tree."
      ifFalse: [
        decodingTree := stack removeFirst.
        decodingTreeComplete := true.
      ].
    ].
  ].

  flagD ifTrue: [ 
      'heres the decoding tree made out of the leaves: ' display.
      nl display.
      decodingTree display.
      nl display.
  ].

  "(e) Now loop over the rest of the input file to reconstruct the original file : Initialize
  a pointer to the root of the decoding tree."
  "(f) Read a single bit and use it to move down the 0-link or the 1-link to the child
  of the current node."
  foundEOF := false.
  [foundEOF] whileFalse: [
    | thisBit |
    treePointer := decodingTree.

    "(h) If not (a leaf), continue reading bits until you find a leaf."
    [treePointer isLeaf] whileFalse: [
      thisBit := readBitStream nextBit.
      (thisBit = 0)
      ifTrue: [
        treePointer := treePointer leftChild.
        flagD ifTrue: [ 
          'went left: ' display.
          nl display.
          treePointer display.
          nl display.
        ].
      ]
      ifFalse: [
        treePointer := treePointer rightChild.
        flagD ifTrue: [ 
          'went right: ' display.
          nl display.
          treePointer display.
          nl display.
        ].
      ].
    ].

    "(g) If this node is a leaf, write out the corresponding byte and reset the pointer
    back to the root of the tree."
    flagD ifTrue: [ 
      'heres the value of treePointer after finding a leaf: ' display.
      nl display.
      treePointer display.
      nl display.
    ].

    (treePointer char = 'EOF') 
    ifTrue: [foundEOF := true.] 
    ifFalse: [ outfile << (treePointer char) ].
  ].

  infile close.
  outfile close.
].



