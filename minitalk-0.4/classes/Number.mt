Magnitude subclass: #Number
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Number methodsFor: 'arithmetic'!

+ aNumber
    "Answer the sum of the receiver and aNumber."
    ^self subclassResponsibility!

- aNumber
    "Answer the difference between the receiver and aNumber."
    ^self subclassResponsibility!

* aNumber
    "Answer the result of multiplying the receiver by aNumber."
    ^self subclassResponsibility!

/ aNumber
    "Answer the result of dividing the receiver by aNumber."
    ^self subclassResponsibility!

// aNumber
    "Answer the integer quotient defined by division with
     truncation toward negative infinity."
    ^(self / aNumber) floor!

\\ aNumber
    "Answer the integer remainder defined by division with
     truncation toward negative infinity."
    ^self - ((self // aNumber) * aNumber)!

quo: aNumber
    "Answer the integer quotient defined by division with
     truncation toward zero."
    ^(self / aNumber) truncated!

rem: aNumber
    "Answer the integer remainder defined by division with
     truncation toward zero."
    ^self - ((self quo: aNumber) * aNumber)!

abs
    "Answer the absolute value of the receiver."
    ^self < 0 ifTrue: [self negated]
              ifFalse: [self]!

negated
    "Answer the receiver negated."
    ^0 - self!

reciprocal
    ^1 / self!

pi
    "Answer the receiver multiplied by pi."
    ^self * 3.14159265359! !

!Number methodsFor: 'mathematical functions'!

sin
    ^self asFloat sin!

cos
    ^self asFloat cos!

tan
    ^self asFloat tan!

arcSin
    ^self asFloat arcSin!

arcCos
    ^self asFloat arcCos!

arcTan
    ^self asFloat arcTan!

exp
    ^self asFloat exp!

ln
    ^self asFloat ln!

log: aNumber
    ^self ln / aNumber ln!

floorLog: radix
    ^(self log: radix) floor!

raisedTo: aNumber
    ^(self ln * aNumber) exp!

raisedToInteger: anInteger
    | answer |
    (anInteger isKindOf: Integer)
        ifFalse: [^self error: 'raisedToInteger: needs integer as power'].
    answer <- 1.
    anInteger abs timesRepeat: [answer <- answer * self].
    ^anInteger negative
        ifFalse: [answer]
        ifTrue: [answer reciprocal]!

sqrt
    "Answer the square root of the receiver."
    ^self asFloat sqrt!

squared
    "Answer the receiver squared."
    ^self * self! !

!Number methodsFor: 'testing'!

even
    ^(self \\ 2) = 0!

odd
    ^(self \\ 2) = 1!

negative
    ^self < 0!

positive
    ^self >= 0!

strictlyPositive
    ^self > 0!

sign
    self > 0 ifTrue: [^1].
    self < 0 ifTrue: [^-1].
    ^0! !

!Number methodsFor: 'truncation and round off'!

ceiling
    ^self asFloat ceiling!

floor
    ^self asFloat floor!

truncated
    ^self < 0
        ifTrue: [self ceiling]
        ifFalse: [self floor]!

truncateTo: aNumber
    ^(self quo: aNumber) * aNumber!

rounded
    ^(self + (self sign / 2)) truncated!

roundTo: aNumber
    ^(self / aNumber) rounded * aNumber! !

!Number methodsFor: 'converting'!

degreesToRadians
    ^(self / 360) * 2 pi!

radiansToDegrees
    ^(self / 2 pi) * 360! !

!Number methodsFor: 'interval creation'!

to: stop
    ""
    ^Interval from: self to: stop!

to: stop by: step
    ""
    ^Interval from: self to: stop by: step! !

!Number methodsFor: 'interval iteration'!

to: stop do: aBlock
    ""
    ^(self to: stop) do: aBlock!

to: stop by: step do: aBlock
    ""
    ^(self to: stop by: step) do: aBlock! !

!Number methodsFor: 'coercing'!

retry: aSymbol coercing: aNumber
    (aSymbol == #= and: [(aNumber isKindOf: Number) == false])
        ifTrue: [^false].
    self generality < aNumber generality
        ifTrue: [^(aNumber coerce: self) perform: aSymbol with: aNumber].
    self generality > aNumber generality
        ifTrue: [^self perform: aSymbol with: (self coerce: aNumber)].
    self error: 'coercion attempt failed'! !

!Number methodsFor: 'point creation'!

@ aNumber
    "Answer a new Point with the receiver as x coordinate
     and aNumber as y coordinate."
    ^Point x: self y: aNumber! !
