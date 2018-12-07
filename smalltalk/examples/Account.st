Object subclass: Account [
	| balance |
	<comment: 'A place to deposit and withdraw money'>

	Account class >> new [
		<category: 'instance creation'>
		|r| "variable"
		r := super new. "creates Account *Object*"
		r init.
		^r "returns the newly created and initialized Account Object"
	]

	init [
		<category: 'initialization'>
		balance := 0
	]


	"printOn is central printing function, once defined all of the other 
	printing methods end up calling it. it takes an argument of where to print
	and is usually Trancript, which is stdout (terminal)"
	printOn: stream [ 
		<category: 'printing'>
		super printOn: stream. 
		stream nextPutAll: 'with balance: '.
		balance printOn: steam

	]

	spend: amount [
		<category: 'moving money'>
		balance := balance - amount
	]


	deposit: amount [
		<category: 'moving money'>
		balance := balance + amount
	]
]


Account subclass: Savings [
	| interest |
	" Parent took care of the 'new' message and created a new object
	of the appropriate size."

	" the subclass Savings will receive the 'init' message first; 
	it sets up its own instance variable, and then passes the init
	message up the chain to let the parent take care of the rest 
	of the initializing for an Account"
	init [
		<category: 'initialization'>
		interest := 0.
		^super init
	]


	" we are adding the amount to our running total of interest.
	The line 'self deposit: amount' tells Smalltalk to send outselves a message,
	in this case 'deposit: amount'. Then, smalltalk looks up the method for
	deposit and finds it in the parent, account. this changes overall
	balance in the savings account. "
	interest: amount [
		interest := interest + amount.
		self deposit: amount
	]

	clearInterest [
		| oldinterest |
		oldinterest := interest.
		interest := 0.
		^oldinterest
	]
]

"will keeps track of what checknumber we're on, and how many
checks we have left."
Account subclass: Checking [
	| checknum checksleft history|

	init [
		<category: 'initialization'>
		checksleft := 0.
		history := Dictionary new.
		^super init
	]

	"this function has two arguements, number and checkcount."
	newChecks: number count: checkcount [
		<category: 'spending'>
		checknum := number.
		checksleft := checkcount
	]

	writeCheck: amount [
		<category: 'spending'>
		| num |

		"Sanity check that we have checks left in our checkbook"
		(checksleft < 1) "returns boolean"
			ifTrue: [^self error: 'Out of checks'].

		"Make sure we've never used this check number before"
		num := checknum.
		(history includesKey: num)
			ifTrue: [ ^self error: 'Duplicate check number'].

		"Record the check number and amount"
		history at: num put: amount. "just how Dictionary works, key and value"

		"Update our next checknumber, checks left, and balance"
		checknum := checknum + 1.
		checkleft := checkleft -1.
		self spend: amount
		^ num
	]
	
	printOn: stream [
		super printOn: stream.
		', checks left: ' printOn: stream.
		checksleft printOn: stream.
		',checks written: ' printOn: stream.
		(history size) printOn: stream.
	]

	check: num [
		| c |
		c := history
			at: num
			ifAbsent: [ ^self error :'No such check #' ].
		^c "return the num to print" 
	]

	printChecks [
		history keysAndValuesDo: [ :key :value |
			key print.
			' - ' print.
			value printNl.

		]
	]

	" mycheck checksOver: 200 do: [:x | x printNl]"
	checksOver: amount do: aBlock
		history keysAndValuesDo: [:key :value |
			(value > amount)
				ifTrue: [aBlock value: key]
		]

	"alternatively."
	checksOver: amount do: aBlock [
		| chosen |
		chosen := history select: [:amt | amt > amount].
		chosen keysDo: aBlock
	]
]



