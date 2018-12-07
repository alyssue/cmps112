some loops:

1 to: 20 do: [:x | x printNl]

1 to: 20 by: 2 do: [:x | x printNl]

20 to: 1 by: -1 do: [:x | x printNl]

variable x is local to the block. 


i := Interval from: 5 to: 10

" i is an Interval. an Interval is an Object."

i do [:x | x printNl]

i := (Interval from: 5 to: 10 by: 2)
i do: [:x | x printNl]



Streams can also operate on files. 
If you wanted to dump the file /etc/passwd to your terminal, 
you could create a stream on the file, 
and then stream over its contents:

   f := FileStream open: '/etc/passwd' mode: FileStream read
   f linesDo: [ :c | Transcript nextPutAll: c; nl ]
   f position: 30
   25 timesRepeat: [ Transcript nextPut: (f next) ]
   f close