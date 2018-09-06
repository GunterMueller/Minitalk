class
    "Answer the receiver's class."
    <1>
    ^self primitiveFailed
isKindOf: aClass
    "Answer true if the receiver's class is aClass or inherits
     from aClass, false otherwise."
    ^self class == aClass
        ifTrue: [true]
        ifFalse: [self class inheritsFrom: aClass]
isMemberOf: aClass
    "Answer true if the receiver's class is aClass, false otherwise."
    ^self class == aClass
respondsTo: aSymbol
    "Answer true if aSymbol is a message selector for a method defined in
     the receiver's class or a class it inherits from, false otherwise."
    ^self class canUnderstand: aSymbol
species
    "Answer the class which is the class of the receiver or is similar to
     the class of the receiver. Used for obtaining copies of the receiver."
    ^self class
isNil
    "Answer true if the receiver is nil, false otherwise."
    ^false
notNil
    "Answer true if the receiver is not nil, false otherwise."
    ^true
yourself
    "Answer the receiver. Useful for cascading."
    ^self
halt: aString
    "Trap to the debugger."
    <0>
    ^self primitiveFailed
halt
    "Trap to the debugger. Uses default message."
    ^self halt: 'halt encountered'
basicInspect
    "Show the receiver. Is never redefined in subclasses."
    <22>
    ^self primitiveFailed
inspect
    "Show the receiver. Can be redefined in subclasses."
    ^self basicInspect
confirm: aString
    <23>
    ^self primitiveFailed
stop
    "Stop the MiniTalk system."
    (self confirm: 'Are you sure?')
        ifTrue: [^Driver stop]
        ifFalse: [^'ok']
error: aString
    "Report an error to the user. Can be redefined in subclasses."
    ^self halt: aString
doesNotUnderstand: aMessage
    "The standard handler for messages not understood by the receiver."
    ^self halt: self printString,
                'does not understand',
                aMessage selector printString
primitiveFailed
    "Primitive has failed."
    ^self error: 'a primitive has failed'
shouldNotImplement
    "Message is not appropriate for object."
    ^self error: 'this message is not appropriate for this object'
subclassResponsibility
    "Subclass should have overridden one of the messages."
    ^self error: 'my subclass should have overridden one of my messages'
errorSubscriptBounds: index
    ""
    ^self error: 'subscript is out of bounds: ', index printString
totalSize
    "Answer the total size of the receiver, i.e. the number of named
     instance variables plus the number of indexed instance variable."
    <24>
    ^self primitiveFailed
basicSize
    "Answer the number of indexed instance variables.
     Is never redefined in subclasses."
    ^self totalSize - self class instSize
size
    "Answer the number of indexed instance variables.
     Can be redefined in subclasses."
    ^self basicSize
become: otherObject
    ""
    <48>
    ^self primitiveFailed
instVarAt: anInteger
    "Answer the value of the named instance variable at position
     anInteger. Legal positions range from 1 to self class instSize."
    <30>
    ^self primitiveFailed
instVarAt: anInteger put: anObject
    "Change the value of the named instance variable at position
     anInteger. Legal positions range from 1 to self class instSize.
     Answer anObject."
    <31>
    ^self primitiveFailed
basicAt: anInteger
    "Answer the value of the indexed instance variable at position
     anInteger. Legal positions range from 1 to self basicSize.
     Is not redefined in any subclass."
    <32>
    ^self primitiveFailed
basicAt: anInteger put: anObject
    "Change the value of the indexed instance variable at position
     anInteger. Legal positions range from 1 to self basicSize.
     Answer anObject. Is not redefined in any subclass."
    <33>
    ^self primitiveFailed
at: anInteger
    "Same as basicAt:, but may be redefined in subclasses."
    ^self basicAt: anInteger
at: anInteger put: anObject
    "Same as basicAt:put:, but may be redefined in subclasses."
    ^self basicAt: anInteger put: anObject
copy
    "Answer a shallow copy of the receiver. Subclasses typically
     override this method when a shallow copy is not sufficient."
     ^self shallowCopy
deepCopy
    "Answer a deep copy of the receiver."
    | class index newObject |
    class <- self class.
    "First handle the indexed instance variables."
    class isVariable
        ifTrue: [index <- self basicSize.
                 newObject <- class basicNew: index.
                 [index > 0] whileTrue: [
                     newObject basicAt: index
                               put: (self basicAt: index) deepCopy.
                     index <- index - 1]]
        ifFalse: [newObject <- class basicNew].
    "Then handle the named instance variables."
    index <- class instSize.
    [index > 0] whileTrue: [
        newObject instVarAt: index
                  put: (self instVarAt: index) deepCopy.
        index <- index - 1].
    ^newObject
shallowCopy
    "Answer a shallow copy of the receiver."
    | class index newObject |
    class <- self class.
    "First handle the indexed instance variables."
    class isVariable
        ifTrue: [index <- self basicSize.
                 newObject <- class basicNew: index.
                 [index > 0] whileTrue: [
                     newObject basicAt: index
                               put: (self basicAt: index).
                     index <- index - 1]]
        ifFalse: [newObject <- class basicNew].
    "Then handle the named instance variables."
    index <- class instSize.
    [index > 0] whileTrue: [
        newObject instVarAt: index
                  put: (self instVarAt: index).
        index <- index - 1].
    ^newObject
== anObject
    "Answer true if the receiver and anObject are the same,
     otherwise false. This not redefined in any subclass."
    <9>
    ^self primitiveFailed
~~ anObject
    "Answer true if the receiver and anObject are not the same,
     otherwise false."
    ^(self == anObject) not
= anObject
    "Default implementation of equality is equivalence."
    ^self == anObject
~= anObject
    "Inequality."
    ^(self = anObject) not
hash
    ^self class name hash
perform: aSymbol
    <47>
    ^self primitiveFailed
perform: aSymbol with: anObject
    <47>
    ^self primitiveFailed
perform: aSymbol with: firstObject with: secondObject
    <47>
    ^self primitiveFailed
perform: aSymbol with: firstObject with: secondObject with: thirdObject
    <47>
    ^self primitiveFailed
printString
    "Answer a string whose characters are a description of the receiver."
    | className article |
    className <- self class name.
    (className at: 1) isVowel
        ifTrue: [article <- 'an ']
        ifFalse: [article <- 'a '].
    ^article, className
storeOn: aStream
    ^aStream nextPutAll: 'not implemented yet'
storeString
    "Answer a string representation that can be used
     to reconstruct the receiver."
    | aStream |
    aStream <- WriteStream on: (String new: 16).
    self storeOn: aStream.
    ^aStream contents
new
    "New instances of UndefinedObject are not allowed."
    ^self shouldNotImplement
deepCopy
    "Answer nil, the receiver."
    ^nil
shallowCopy
    "Answer nil, the receiver."
    ^nil
printString
    ""
    ^'nil'
isNil
    "Answer true if the receiver is nil, false otherwise."
    ^true
notNil
    "Answer true if the receiver is not nil, false otherwise."
    ^false
new
    "New instances of Boolean are not allowed."
    ^self shouldNotImplement
eqv: aBoolean
    "Answer true if the receiver and aBoolean have the same truth values,
     otherwise answer false."
    ^self == aBoolean
xor: aBoolean
    "Answer true if the receiver and aBoolean have different truth values,
     otherwise answer false."
    ^self ~~ aBoolean
printString
    ""
    ^'false'
not
    "Answer true if the receiver is false and false otherwise."
    ^true
& aBoolean
    "Answer true if the receiver and aBoolean are both true,
     false otherwise."
    ^false
| aBoolean
    "Answer true if either the receiver, aBoolean, or both are true,
     false otherwise."
    ^aBoolean
and: aBlock
    "If the receiver is true, compute and answer the block result,
     otherwise answer false without evaluating the block."
    ^false
or: aBlock
    "If the receiver is false, compute and answer the block result,
     otherwise answer true without avaluating the block."
    ^aBlock value
ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, answer nil."
    ^nil
ifFalse: falseBlock
    "If the receiver is false, evaluate the falseBlock and answer the result.
     If the receiver is true, answer nil."
    ^falseBlock value
ifTrue: trueBlock ifFalse: falseBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^falseBlock value
ifFalse: falseBlock ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^falseBlock value
printString
    ""
    ^'true'
not
    "Answer true if the receiver is false and false otherwise."
    ^false
& aBoolean
    "Answer true if the receiver and aBoolean are both true,
     false otherwise."
    ^aBoolean
| aBoolean
    "Answer true if either the receiver, aBoolean, or both are true,
     false otherwise."
    ^true
and: aBlock
    "If the receiver is true, compute and answer the block result,
     otherwise answer false without evaluating the block."
    ^aBlock value
or: aBlock
    "If the receiver is false, compute and answer the block result,
     otherwise answer true without avaluating the block."
    ^true
ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, answer nil."
    ^trueBlock value
ifFalse: falseBlock
    "If the receiver is false, evaluate the falseBlock and answer the result.
     If the receiver is true, answer nil."
    ^nil
ifTrue: trueBlock ifFalse: falseBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^trueBlock value
ifFalse: falseBlock ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^trueBlock value
hash
    ^self subclassResponsibility
= aMagnitude
    ^self subclassResponsibility
< aMagnitude
    ^self subclassResponsibility
<= aMagnitude
    ^(self < aMagnitude) or: [self = aMagnitude]
> aMagnitude
    ^aMagnitude < self
>= aMagnitude
    ^(self > aMagnitude) or: [self = aMagnitude]
between: aMagnitude and: anotherMagnitude
    ^(aMagnitude <= self) and: [self <= anotherMagnitude]
min: aMagnitude
    ^(self < aMagnitude) ifTrue: [self] ifFalse: [aMagnitude]
max: aMagnitude
    ^(self > aMagnitude) ifTrue: [self] ifFalse: [aMagnitude]
key: aKey value: aValue
    "Answer a new association with key aKey and value aValue."
    ^self new key: aKey value: aValue
= anAssociation
    ""
    ^key = anAssociation key
< anAssociation
    ""
    ^key < anAssociation key
key
    "Answer the key of the receiver."
    ^key
value
    "Answer the value of the receiver."
    ^value
key: anObject
    "Modify the receiver key to be anObject."
    key <- anObject
value: anObject
    "Modify the receiver value to be anObject."
    value <- anObject
key: aKey value: aValue
    "Modify the receiver key to be aKey and the
     receiver value to be aValue."
    key <- aKey.
    value <- aValue
printOn: aStream
    "Print a representation of the receiver on aStream."
    self key printOn: aStream.
    aStream nextPutAll: '==>'.
    self value printOn: aStream
printString
    ""
    ^self key printString,
     ' ==> ',
     self value printString
storeOn: aStream
    "Print an executable representation of the receiver on aStream."
    aStream nextPutAll: '(Association key: '.
    self key storeOn: aStream.
    aStream nextPutAll: ' value: '.
    self value storeOn: aStream.
    aStream nextPut: $)
cr
    ""
    ^$

isVowel
    ^(self == $a) |
     (self == $e) |
     (self == $i) |
     (self == $o) |
     (self == $u) |
     (self == $A) |
     (self == $E) |
     (self == $I) |
     (self == $O) |
     (self == $U)
output
    ""
    <11>
    ^self primitiveFailed
+ aNumber
    "Answer the sum of the receiver and aNumber."
    ^self subclassResponsibility
- aNumber
    "Answer the difference between the receiver and aNumber."
    ^self subclassResponsibility
* aNumber
    "Answer the result of multiplying the receiver by aNumber."
    ^self subclassResponsibility
/ aNumber
    "Answer the result of dividing the receiver by aNumber."
    ^self subclassResponsibility
// aNumber
    "Answer the integer quotient defined by division with
     truncation toward negative infinity."
    ^(self / aNumber) floor
\\ aNumber
    "Answer the integer remainder defined by division with
     truncation toward negative infinity."
    ^self - ((self // aNumber) * aNumber)
quo: aNumber
    "Answer the integer quotient defined by division with
     truncation toward zero."
    ^(self / aNumber) truncated
rem: aNumber
    "Answer the integer remainder defined by division with
     truncation toward zero."
    ^self - ((self quo: aNumber) * aNumber)
abs
    "Answer the absolute value of the receiver."
    ^self < 0 ifTrue: [self negated]
              ifFalse: [self]
negated
    "Answer the receiver negated."
    ^0 - self
reciprocal
    ^1 / self
pi
    "Answer the receiver multiplied by pi."
    ^self * 3.14159265359
sin
    ^self asFloat sin
cos
    ^self asFloat cos
tan
    ^self asFloat tan
arcSin
    ^self asFloat arcSin
arcCos
    ^self asFloat arcCos
arcTan
    ^self asFloat arcTan
exp
    ^self asFloat exp
ln
    ^self asFloat ln
log: aNumber
    ^self ln / aNumber ln
floorLog: radix
    ^(self log: radix) floor
raisedTo: aNumber
    ^(self ln * aNumber) exp
raisedToInteger: anInteger
    | answer |
    (anInteger isKindOf: Integer)
        ifFalse: [^self error: 'raisedToInteger: needs integer as power'].
    answer <- 1.
    anInteger abs timesRepeat: [answer <- answer * self].
    ^anInteger negative
        ifFalse: [answer]
        ifTrue: [answer reciprocal]
sqrt
    "Answer the square root of the receiver."
    ^self asFloat sqrt
squared
    "Answer the receiver squared."
    ^self * self
even
    ^(self \\ 2) = 0
odd
    ^(self \\ 2) = 1
negative
    ^self < 0
positive
    ^self >= 0
strictlyPositive
    ^self > 0
sign
    self > 0 ifTrue: [^1].
    self < 0 ifTrue: [^-1].
    ^0
ceiling
    ^self asFloat ceiling
floor
    ^self asFloat floor
truncated
    ^self < 0
        ifTrue: [self ceiling]
        ifFalse: [self floor]
truncateTo: aNumber
    ^(self quo: aNumber) * aNumber
rounded
    ^(self + (self sign / 2)) truncated
roundTo: aNumber
    ^(self / aNumber) rounded * aNumber
degreesToRadians
    ^(self / 360) * 2 pi
radiansToDegrees
    ^(self / 2 pi) * 360
to: stop
    ""
    ^Interval from: self to: stop
to: stop by: step
    ""
    ^Interval from: self to: stop by: step
to: stop do: aBlock
    ""
    ^(self to: stop) do: aBlock
to: stop by: step do: aBlock
    ""
    ^(self to: stop by: step) do: aBlock
retry: aSymbol coercing: aNumber
    (aSymbol == #= and: [(aNumber isKindOf: Number) == false])
        ifTrue: [^false].
    self generality < aNumber generality
        ifTrue: [^(aNumber coerce: self) perform: aSymbol with: aNumber].
    self generality > aNumber generality
        ifTrue: [^self perform: aSymbol with: (self coerce: aNumber)].
    self error: 'coercion attempt failed'
@ aNumber
    "Answer a new Point with the receiver as x coordinate
     and aNumber as y coordinate."
    ^Point x: self y: aNumber
generality
    ^80
coerce: aNumber
    ^aNumber asFloat
asFloat
    ^self
asFraction
    ^self error: 'cannot coerce a Float to a Fraction'
asSmallInteger
    ^self error: 'cannot coerce a Float to a SmallInteger'
floor
    <50>
    ^self primitiveFailed
ceiling
    <51>
    ^self primitiveFailed
sin
    <52>
    ^self primitiveFailed
cos
    <53>
    ^self primitiveFailed
tan
    <54>
    ^self primitiveFailed
arcSin
    <55>
    ^self primitiveFailed
arcCos
    <56>
    ^self primitiveFailed
arcTan
    <57>
    ^self primitiveFailed
exp
    <58>
    ^self primitiveFailed
ln
    <59>
    ^self primitiveFailed
sqrt
    <60>
    ^self primitiveFailed
+ aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self addFloat: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]
- aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self subFloat: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]
* aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self mulFloat: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]
/ aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self divFloat: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]
< aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self lessFloat: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]
= aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self equalFloat: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]
addFloat: aFloat
    <42>
    ^self primitiveFailed
subFloat: aFloat
    <43>
    ^self primitiveFailed
mulFloat: aFloat
    <44>
    ^self primitiveFailed
divFloat: aFloat
    <41>
    aFloat = 0.0
        ifTrue: [^self error: 'floating division by zero'].
    ^self primitiveFailed
lessFloat: aFloat
    <40>
    ^self primitiveFailed
equalFloat: aFloat
    <39>
    ^self primitiveFailed
printString
    ""
    <45>
    ^self primitiveFailed
numerator: n denominator: d
    "Answer a new Fraction with numerator n and denominator d.
     Use the standard representation for fractions."
    | numer denom factor |
    (n < 0 xor: d < 0)
        ifTrue: [numer <- n abs negated]
        ifFalse: [numer <- n abs].
    denom <- d abs.
    factor <- n gcd: d.
    ^self new numerator: (numer / factor) denominator: (denom / factor)
generality
    ^60
coerce: aNumber
    ^aNumber asFraction
asFloat
    ^numerator asFloat / denominator asFloat
asFraction
    ^self
asSmallInteger
    ^self error: 'cannot coerce a Fraction to a SmallInteger'
+ aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self addFraction: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]
- aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self subFraction: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]
* aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self mulFraction: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]
/ aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self divFraction: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]
< aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self lessFraction: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]
= aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self equalFraction: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]
addFraction: aFraction
    ^Fraction numerator: (numerator * aFraction denominator) +
                         (denominator * aFraction numerator)
              denominator: denominator * aFraction denominator
subFraction: aFraction
    ^Fraction numerator: (numerator * aFraction denominator) -
                         (denominator * aFraction numerator)
              denominator: denominator * aFraction denominator
mulFraction: aFraction
    ^Fraction numerator: numerator * aFraction numerator
              denominator: denominator * aFraction denominator
divFraction: aFraction
    ^Fraction numerator: numerator * aFraction denominator
              denominator: denominator * aFraction numerator
lessFraction: aFraction
    ^(numerator * aFraction denominator) <
     (denominator * aFraction numerator)
equalFraction: aFraction
    ^(numerator * aFraction denominator) =
     (denominator * aFraction numerator)
printString
    ""
    ^numerator printString, ' / ', denominator printString
numerator
    ^numerator
denominator
    ^denominator
numerator: n denominator: d
    "Set the numerator to n and the denominator to d."
    numerator <- n.
    denominator <- d
test
   ^self subclass: #Test
         instanceVariableNames: ''
         classVariableNames: ''
         poolDictionaries: ''
         category: ''
   
factorial
    | answer |
    answer <- 1.
    1 to: self do: [:i | answer <- answer * i].
    ^answer
gcd: anInteger
    | a b t |
    a <- self abs.
    b <- anInteger abs.
    [b = 0] whileFalse:
        [t <- a \\ b.
         a <- b.
         b <- t].
    ^a
lcm: anInteger
    | gcd |
    gcd <- self gcd: anInteger.
    ^self / gcd * anInteger
timesRepeat: aBlock
    | n |
    n <- 1.
    [n <= self] whileTrue:
        [aBlock value.
         n <- n + 1]
generality
    ^20
coerce: aNumber
    ^aNumber asSmallInteger
asFloat
    <46>
    ^self primitiveFailed
asFraction
    ^Fraction numerator: self denominator: 1
asSmallInteger
    ^self
+ aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self addSmallInteger: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]
- aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self subSmallInteger: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]
* aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self mulSmallInteger: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]
/ aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self divSmallInteger: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]
< aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self lessSmallInteger: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]
= aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self equalSmallInteger: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]
hash
    ^self
addSmallInteger: aSmallInteger
    <27>
    ^self primitiveFailed
subSmallInteger: aSmallInteger
    <28>
    ^self primitiveFailed
mulSmallInteger: aSmallInteger
    <29>
    ^self primitiveFailed
divSmallInteger: aSmallInteger
    <26>
    aSmallInteger = 0
        ifTrue: [^self error: 'integer division by zero'].
    ^Fraction numerator: self denominator: aSmallInteger
lessSmallInteger: aSmallInteger
    <38>
    ^self primitiveFailed
equalSmallInteger: aSmallInteger
    <37>
    ^self primitiveFailed
bitAnd: aSmallInteger
    <25>
    ^self primitiveFailed
printString
    ""
    <10>
    ^self primitiveFailed
nextLink: aLink
    "Create an instance of Link that references the argument, aLink."
    ^self new nextLink: aLink
nextLink
    "Answer the receiver's reference."
    ^nextLink
nextLink: aLink
    "Set the receiver's reference to be the argument, aLink."
    nextLink <- aLink
link: anObject to: aLink
    ""
    ^(super nextLink: aLink) object: anObject
printString
    ""
    ^self object printString,
     '->',
     self nextLink printString
object
    ""
    ^object
object: anObject
    ""
    object <- anObject
on: aBlock
    ^self new 
priority
    ^priority
priority: aPriority
    priority <- aPriority
resume
    
suspend
    
terminate
    
startMinitalk
    ^self
activePriority
    ^activePriority
activeProcess
    ^activeProcess
terminateActive
    
yield
    
timingPriority
    ^7
highIOPriority
    ^6
lowIOPriority
    ^5
userInterruptPriority
    ^4
userSchedulingPriority
    ^3
userBackgroundPriority
    ^2
systemBackgroundPriority
    ^1
on: aCollection
    ^self new on: aCollection
next
    ^self subclassResponsibility
next: anInteger
    | aStream |
    aStream <- WriteStream on: (collection species new: anInteger).
    anInteger timesRepeat: [aStream nextPut: self next].
    ^aStream contents
nextMatchFor: anObject
    ^self next = anObject
contents
    | aStream |
    "aStream <- WriteStream on: ()."
    ^self
nextPut: anObject
    ^self subclassResponsibility
nextPutAll: aCollection
    aCollection do: [:element | self nextPut: element].
    ^aCollection
next: anInteger put: anObject
    anInteger timesRepeat: [self nextPut: anObject].
    ^anObject
atEnd
    ^self subclassResponsibility
do: aBlock
    [self atEnd] whileFalse:
        [aBlock value: self next]
printString
    ^self
storeOn
    ^self
on: aCollection
    collection <- aCollection
new
    ^self basicNew setSeed
atEnd
    ^false
next
    | temp |
    [seed <- 13849 + (27181 * seed) bitAnd: 16rFFFF.
     temp <- seed / 65536.0.
     temp = 0] whileTrue.
    ^temp
setSeed
    seed <- 16rAFFE
on: aCollection from: firstIndex to: lastIndex
    ^(super on: aCollection) from: firstIndex to: lastIndex
on: aCollection
    ^self on: aCollection from: 1 to: aCollection size
next
    | anObject |
    self atEnd ifTrue: [^self error: 'stream has no more elements'].
    anObject <- collection at: currentPosition.
    currentPosition <- currentPosition + 1.
    ^anObject
nextPut: anObject
    self atEnd ifTrue: [^self error: 'stream has no more elements'].
    collection at: currentPosition put: anObject.
    currentPosition <- currentPosition + 1.
    ^anObject
atEnd
    ^currentPosition > lastPosition
from: first to: last
    currentPosition <- first.
    lastPosition <- last
dummy
    ""
    ^self
dummy
    ""
    ^self
dummy
    ""
    ^self
dummy
    ""
    ^self
dummy
    ""
    ^self
dummy
    ""
    ^self
with: anObject
    | newCollection |
    newCollection <- self new.
    newCollection add: anObject.
    ^newCollection
with: firstObject with: secondObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    ^newCollection
with: firstObject with: secondObject with: thirdObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    ^newCollection
with: firstObject with: secondObject with: thirdObject with: fourthObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    newCollection add: fourthObject.
    ^newCollection
add: anObject
    self subclassResponsibility
addAll: aCollection
    aCollection do: [:each | self add: each].
    ^aCollection
remove: anObject ifAbsent: exceptionBlock
    self subclassResponsibility
remove: anObject
    ^self remove: anObject ifAbsent: [self errorNotFound]
removeAll: aCollection
    aCollection do: [:each | self remove: each].
    ^aCollection
errorNotFound
    self error: 'object is not in the collection'
errorNotKeyed
    self error: self class name,
                's do not respond to keyed accessing messages'
isEmpty
    ^self size = 0
includes: anObject
    self do: [:each | anObject = each ifTrue: [^true]].
    ^false
occurrencesOf: anObject
    | tally |
    tally <- 0.
    self do: [:each | anObject = each ifTrue: [tally <- tally + 1]].
    ^tally
size
    | tally |
    tally <- 0.
    self do: [:each | tally <- tally + 1].
    ^tally
do: aBlock
    self subclassResponsibility
collect: aBlock
    | newCollection |
    newCollection <- self species new.
    self do: [:each | newCollection add: (aBlock value: each)].
    ^newCollection
detect: aBlock
    ^self detect: aBlock ifNone: [self errorNotFound]
detect: aBlock ifNone: exceptionBlock
    self do: [:each | (aBlock value: each) ifTrue: [^each]].
    ^exceptionBlock value
inject: thisValue into: binaryBlock
    | nextValue |
    nextValue <- thisValue.
    self do: [:each | nextValue <- binaryBlock value: nextValue value: each].
    ^nextValue
reject: aBlock
    ^self select: [:element | (aBlock value: element) == false]
select: aBlock
    | newCollection |
    newCollection <- self species new.
    self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
    ^newCollection
asSet
    | aSet |
    aSet <- Set new: self size.
    self do: [:each | aSet add: each].
    ^aSet
printOn: aStream
    | tooMany |
    tooMany <- aStream position + self maxPrint.
    aStream nextPutAll: self class name, '('.
    self do:
        [:element |
          aStream position > tooMany
              ifTrue: [aStream nextPutAll: '...etc...)'. ^self].
          element printOn: aStream.
          aStream space].
    aStream nextPut: $)
storeOn: aStream
    | noneYet |
    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: 'new)'.
    noneYet <- true.
    self do:
        [:each |
          noneYet ifTrue: [noneYet <- false]
                  ifFalse: [aStream nextPut: $;].
          aStream nextPutAll: 'add: '.
          aStream store: each].
    noneYet ifFalse: [aStream nextPutAll: '; yourself'].
    aStream nextPut: $)
printString
    | elementString |
    elementString <- ''.
    self do: [:element | elementString <-
                         elementString, element printString, ' '].
    ^self class name, '(', elementString, ')'
maxPrint
    ^5000
size
    self subclassResponsibility
remove: oldObject ifAbsent: anExceptionBlock
    self shouldNotImplement
do: aBlock
    | index length |
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue: [aBlock value: (self at: index)]
collect: aBlock
    | aStream index length |
    aStream <- WriteStream on: (self species new: self size).
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue: [aStream nextPut: (aBlock value: (self at: index))].
    ^aStream contents
select: aBlock
    | aStream index length |
    aStream <- WriteStream on: (self species new: self size).
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue:
            [(aBlock value: (self at: index))
                ifTrue: [aStream nextPut: (self at: index)]].
    ^aStream contents
growSize
    ""
    ^10
grow
    ""
    | newCollection |
    newCollection <- self species new: self size + self growSize.
    newCollection replaceFrom: 1 to: self size with: self.
    ^self become: newCollection
isEmpty
    "Answer true if the receiver has no elements, otherwise answer false."
    ^firstLink isNil
at: index
    "Answer the element at position index."
    | count element |
    count <- 1.
    element <- firstLink.
    [element isNil] whileFalse:
        [count = index
            ifTrue: [^element]
            ifFalse: [count <- count + 1.
                      element <- element nextLink]].
    ^self errorSubscriptBounds: index
at: index put: element
    "Report an error since at:put: is not appropriate for a LinkedList."
    ^self error: 'do not store into a LinkedList using at:put:'
addFirst: aLink
    "Add aLink to the beginning of the receiver's list. Answer aLink."
    aLink nextLink: firstLink.
    firstLink isNil ifTrue: [lastLink <- aLink].
    firstLink <- aLink.
    ^aLink
addLast: aLink
    "Add aLink to the end of the receiver's list. Answer aLink."
    aLink nextLink: nil.
    firstLink isNil
        ifTrue: [firstLink <- aLink]
        ifFalse: [lastLink nextLink: aLink].
    lastLink <- aLink.
    ^aLink
add: aLink
    "Add aLink to the receiver's list. Answer aLink."
    ^self addLast: aLink
remove: aLink ifAbsent: aBlock
    "Remove aLink from the receiver's list and answer it.
     Answer the value of aBlock if aLink is not an element of the list."
    | tempLink |
    aLink == firstLink
        ifTrue: [firstLink <- aLink nextLink.
                 aLink == lastLink ifTrue: [lastLink <- nil]]
        ifFalse: [tempLink <- firstLink.
                  [tempLink isNil ifTrue: [^aBlock value].
                   tempLink nextLink == aLink] whileFalse:
                      [tempLink <- tempLink nextLink].
                  tempLink nextLink: aLink nextLink.
                  aLink == lastLink ifTrue: [lastLink <- tempLink]].
    aLink nextLink: nil.
    ^aLink
remove: aLink
    "Remove aLink from the receiver's list and answer it.
     Report an error if aLink is not an element of the list."
    ^self remove: aLink
          ifAbsent: [^self error: 'link not found in linked list']
removeFirst
    "Remove the receiver's first element and answer it.
     If the receiver is empty, report an error."
    ^self remove: firstLink
removeLast
    "Remove the receiver's last element and answer it.
     If the receiver is empty, report an error."
    ^self remove: lastLink
do: aBlock
    "Evaluate aBlock for each element of the receiver's list."
    | element |
    element <- firstLink.
    [element isNil] whileFalse:
        [aBlock value: element.
         element <- element nextLink]
new
    ""
    ^super new initSignals
forMutualExclusion
    ""
    ^self new signal
critical: aBlock
    ""
    | value |
    self wait.
    value <- aBlock value.
    self signal.
    ^value
initSignals
    ""
    excessSignals <- 0
signal
    ""
    ^self
wait
    ""
    ^self
dummy
    ""
    ^self
printString
    ""
    | elementString |
    elementString <- ''.
    self do: [:element | elementString <-
                         elementString, element printString, ' '].
    ^'(', elementString, ')'
dummy
    ""
    ^self
dummy
    ""
    ^self
input
    "Input a string from the keyboard."
    <2>
    ^self primitiveFailed
at: anInteger
    <35>
    ^self primitiveFailed
at: anInteger put: aCharacter
    <36>
    ^self primitiveFailed
size
    ""
    <19>
    ^self primitiveFailed
, aString
    ""
    <8>
    ^self primitiveFailed
= aString
    <21>
    ^self primitiveFailed
hash
    <49>
    ^self primitiveFailed
printString
    ""
    ^'''', self, ''''
output
    "Output the receiver to the screen."
    <3>
    ^self primitiveFailed
dummy
    ""
    ^self
species
    ^String
printString
    ""
    ^self "asString"
asString
    ""
    | size string |
    size <- self size.
    string <- String new: size.
    1 to: size do: [:i | string at: i put: (self at: i)].
    ^string
from: start to: stop by: step
    ""
    ^self new start: start stop: stop step: step
from: start to: stop
    ""
    ^self from: start to: stop by: 1
species
    ^Array
at: aNumber
    ""
    | answer |
    (aNumber isKindOf: Integer)
        ifFalse: [^self errorNonIntegerIndex].
    aNumber > 0
        ifTrue: [
            answer <- start + (step * (aNumber - 1)).
            (step > 0 and: [answer between: start and: stop])
                ifTrue: [^answer].
            (step < 0 and: [answer between: stop and: start])
                ifTrue: [^answer].
        ].
    ^self errorSubscriptBounds: aNumber
at: anInteger put: aNumber
    ""
    ^self shouldNotImplement
do: aBlock
    | aValue |
    aValue <- start.
    step < 0
        ifTrue: [[stop <= aValue]
                     whileTrue: [aBlock value: aValue.
                                 aValue <- aValue + step]]
        ifFalse: [[stop >= aValue]
                     whileTrue: [aBlock value: aValue.
                                 aValue <- aValue + step]]
start: num1 stop: num2 step: num3
    ""
    start <- num1.
    stop <- num2.
    step <- num3
printString
    ^step = 1 ifTrue: [start printString,
                       ' to: ',
                       stop printString]
              ifFalse: [start printString,
                        ' to: ',
                        stop printString,
                        ' by: ',
                        step printString]
at: index
    self errorNotKeyed
at: index put: anObject
    self errorNotKeyed
add: anObject
    | linkedObject |
    linkedObject <- elements.
    [linkedObject notNil] whileTrue:
        [linkedObject object = anObject ifTrue: [^anObject].
         linkedObject <- linkedObject nextLink].
    elements <- LinkedObject link: anObject to: elements.
    ^anObject
remove: anObject ifAbsent: aBlock
    | linkedObject |
    linkedObject <- elements.
    linkedObject isNil ifTrue: [^aBlock value].
    linkedObject object = anObject ifTrue:
        [elements <- linkedObject nextLink.
         ^anObject].
    [linkedObject nextLink notNil] whileTrue:
        [linkedObject nextLink object = anObject ifTrue:
            [linkedObject nextLink: linkedObject nextLink nextLink.
             ^anObject].
         linkedObject <- linkedObject nextLink].
    ^aBlock value
remove: anObject
    ^self remove: anObject
          ifAbsent: [^self error: 'object is not in set']
do: aBlock
    | linkedObject |
    linkedObject <- elements.
    [linkedObject notNil] whileTrue:
        [aBlock value: linkedObject object.
         linkedObject <- linkedObject nextLink]
at: key ifAbsent: aBlock
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [^aBlock value]
        ifFalse: [^association value]
at: key
    ^self at: key
          ifAbsent: [self errorKeyNotFound]
associationAt: key ifAbsent: aBlock
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [^aBlock value]
        ifFalse: [^association]
associationAt: key
    ^self associationAt: key
          ifAbsent: [self errorKeyNotFound]
at: key put: anObject
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [association <- Association key: key value: anObject.
                 elements <- LinkedObject link: association to: elements]
        ifFalse: [association value: anObject].
    ^anObject
add: anAssociation
    | association |
    association <- self getAssociationFor: anAssociation key.
    association isNil
        ifTrue: [elements <- LinkedObject link: anAssociation to: elements]
        ifFalse: [association value: anAssociation value].
    ^anAssociation
remove: anObject ifAbsent: aBlock
    self shouldNotImplement
removeKey: aKey ifAbsent: aBlock
    | linkedAssociation |
    linkedAssociation <- elements.
    linkedAssociation isNil ifTrue: [^aBlock value].
    linkedAssociation object key == aKey
        ifTrue: [elements <- elements nextLink.
                 ^self]
        ifFalse: [
            [linkedAssociation nextLink isNil] whileFalse:
                [linkedAssociation nextLink object key == aKey
                    ifTrue: [linkedAssociation nextLink:
                                 linkedAssociation nextLink nextLink.
                             ^self]
                    ifFalse: [linkedAssociation <-
                                  linkedAssociation nextLink]].
            ^aBlock value
        ]
removeKey: aKey
    ^self removeKey: aKey
          ifAbsent: [self errorKeyNotFound]
do: aBlock
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         aBlock value: association value.
         linkedAssociation <- linkedAssociation nextLink]
associationsDo: aBlock
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         aBlock value: association.
         linkedAssociation <- linkedAssociation nextLink]
includesKey: key
    ^(self getAssociationFor: key) notNil
errorKeyNotFound
    self error: 'key not found'
getAssociationFor: aKey
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         (association key = aKey) ifTrue: [^association].
         linkedAssociation <- linkedAssociation nextLink].
    ^nil
dummy
    ""
    ^self
disassembleCode
    <61>
    ^self primitiveFailed
disassemble
    ('<primitive: ', primitive printString, '>') output.
    Character cr output.
    self disassembleCode
selector
    ^selector
primitive
    ^primitive
numberargs
    ^numberargs
tempsize
    ^tempsize
stacksize
    ^stacksize
bytecodes
    ^bytecodes
literals
    ^literals
sourceOffset
    ^sourceOffset
sourceLength
    ^sourceLength
sourceCode
    ""
    | file sourceCode |
    sourceOffset isNil
        ifTrue: [sourceCode <- 'source code is not available']
        ifFalse: [
            file <- File openRead: 'minitalk.mt'.
            sourceCode <- file readAt: sourceOffset length: sourceLength.
            file close.
        ].
    ^sourceCode
sourceCode: aString
    ""
    | file |
    file <- File openAppend: 'minitalk.mt'.
    sourceOffset <- file size.
    file write: aString.
    file close.
    sourceLength <- aString size
dummy
    ""
    ^self
dummy
    ""
    ^self
whileFalse
    "Repeatedly evaluate the receiver as long as it answers false."
    ^[self value] whileFalse: []
whileFalse: aBlock
    "Repeatedly evaluate aBlock as long as the receiver evaluates to false."
    ^self value ifFalse: [aBlock value. self whileFalse: aBlock]
whileTrue
    "Repeatedly evaluate the receiver as long as it answers true."
    ^[self value] whileTrue: []
whileTrue: aBlock
    "Repeatedly evaluate aBlock as long as the receiver evaluates to true."
    ^self value ifTrue: [aBlock value. self whileTrue: aBlock]
value
    ""
    <6>
    ^self primitiveFailed
value: parameter
    ""
    <6>
    ^self primitiveFailed
value: parameter1 value: parameter2
    ""
    <6>
    ^self primitiveFailed
value: parameter1 value: parameter2 value: parameter3
    ""
    <6>
    ^self primitiveFailed
fork
    "Create and schedule a new Process for the execution of
     the expressions the receiver contains."
    self newProcess resume
forkAt: aPriority
    
newProcess
    "Answer a new suspended Process for the execution of the expressions
     the receiver contains. The new Process is not scheduled."
    ^Process on: self
newProcessWith: anArray
    
dummy
    ""
    ^self
dummy
    ""
    ^self
hasPointersBit
    ^16r8000
isIndexableBit
    ^16r4000
instSizeMask
    ^16r00FF
compiledMethodAt: selector
    ^methodDictionary at: selector
sourceCodeAt: selector
    ^(methodDictionary at: selector) sourceCode
sourceMethodAt: selector
    ^(methodDictionary at: selector) sourceCode
disassembleAt: selector
    ^(methodDictionary at: selector) disassemble
includesSelector: selector
    ^methodDictionary includesKey: selector
canUnderstand: selector
    ""
    | class |
    class <- self.
    [class == nil] whileFalse:
        [(class includesSelector: selector) ifTrue: [^true].
         class <- class superclass].
    ^false
inheritsFrom: aClass
    ""
    | class |
    class <- superclass.
    [class == nil] whileFalse:
        [class == aClass ifTrue: [^true].
         class <- class superclass].
    ^false
template
    ""
    ^
'
    "What does it do?"
    | temporaries |
    statements
'
addSelector: aSymbol withMethod: aMethod
    ""
    methodDictionary at: aSymbol put: aMethod
removeSelector: aSymbol
    ""
    methodDictionary removeKey: aSymbol ifAbsent: []
editMethod: aSymbol
    ""
    | method oldSourceCode newSourceCode association |
    method <- methodDictionary at: aSymbol ifAbsent: [].
    method isNil
        ifTrue: [oldSourceCode <- aSymbol printString, self template]
        ifFalse: [oldSourceCode <- method sourceCode].
    newSourceCode <- Editor editString: oldSourceCode.
    newSourceCode ~= oldSourceCode
        ifTrue: [
            association <- Compiler compile: newSourceCode in: self.
            association value sourceCode: newSourceCode.
            self addSelector: association key
                 withMethod: association value
        ].
    
basicNew
    "Create an instance of the receiver."
    <4>
    ^self primitiveFailed
new
    "Create an instance of the receiver."
    ^self basicNew
basicNew: anInteger
    "Create an instance of the receiver."
    <5>
    ^self primitiveFailed
new: anInteger
    "Create an instance of the receiver."
    ^self basicNew: anInteger
printString
    ^self name
superclass
    "Answer the superclass of the receiver."
    ^superclass
methodDictionary
    "Answer the method dictionary of the receiver."
    ^methodDictionary
characteristic
    "Answer the class characteristic of the receiver."
    ^characteristic
isPointers
    ^(characteristic bitAnd: Behavior hasPointersBit) ~= 0
isBinary
    ^(characteristic bitAnd: Behavior hasPointersBit) = 0
isVariable
    ^(characteristic bitAnd: Behavior isIndexableBit) ~= 0
isFixed
    ^(characteristic bitAnd: Behavior isIndexableBit) = 0
instSize
    ^characteristic bitAnd: Behavior instSizeMask
name
    "Answer the name of the receiver."
    ^name
instvars
    "Answer the instance variables of the receiver."
    ^instvars
dummy
    ""
    ^self
subclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass name: classSymbol
               inEnvironment: MiniTalk
               subclassOf: self
               instanceVariableNames: stringOfInstVarNames
               variable: false
               pointers: true
               classVariableNames: stringOfClassVarNames
               poolDictionaries: stringOfPoolDictNames
               category: categoryNameString
               comment: ''
variableSubclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass new
variableBinarySubclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass new
subclassOf: superMeta
    "Answer an instance of Metaclass that is a subclass of superMeta."
    ^self new subclassOf: superMeta
subclassOf: superMeta
    "Initialize the receiver to be a subclass of superMeta."
    superclass <- superMeta.
    methodDictionary <- Dictionary new.
    characteristic <- superMeta characteristic
name: newName
  inEnvironment: aSystemDictionary
  subclassOf: superClass
  instanceVariableNames: stringOfInstVarNames
  variable: variableBoolean
  pointers: pointerBoolean
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryName
  comment: commentString
    ""
    | class |
    name <- newName.
    class <- self new.
    ^class
name
    ""
    ^super name, ' class'
start
    "Start the MiniTalk system."
    ^self new loop
stop
    "Stop the MiniTalk system."
    <20>
loop
    "The read-eval-print loop, a most simple user interface."
    | string result |
    [true] whileTrue:
        ['==> ' output.
         string <- String input.
         result <- Compiler evaluate: string.
         result printString output.
         Character cr output.
        ]
evaluate: aString
    ""
    | association result |
    association <- self compile: 'doIt ', aString
                        in: UndefinedObject
                        lastValueNeeded: true.
    UndefinedObject addSelector: #doIt
                    withMethod: association value.
    result <- nil doIt.
    UndefinedObject removeSelector: #doIt.
    ^result
compile: aString in: aClass
    ""
    ^self compile: aString
          in: aClass
          lastValueNeeded: false
compile: aString in: aClass lastValueNeeded: aBoolean
    ""
    <7>
    ^self primitiveFailed
editString: aString
    "Edit aString using an external file editor. Answer edited string."
    | junk file string |
    junk <- 'junk.mt'.
    file <- File openWrite: junk.
    file write: aString.
    file close.
    self editFile: junk.
    file <- File openRead: junk.
    string <- file readAll.
    file close.
    File delete: 'junk.*'.
    ^string
editFile: aFilename
    "Edit a file using an external file editor."
    <12>
    ^self primitiveFailed
openFile: fileName withMode: fileMode
    ""
    <14>
    ^ self error: 'cannot open file'
delete: fileName
    ""
    <13>
    ^self error: 'cannot delete file'
openRead: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'rt')
openWrite: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'wt')
openAppend: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'at')
handle: aHandle
  handle <- aHandle
readAt: anOffset length: aLength
    ""
    <16>
    ^self error: 'cannot read file'
size
    <18>
    ^self error: 'cannot determine size of file'
readAll
    <34>
    ^self error: 'cannot read file'
write: aString
    <17>
    ^self error: 'cannot write file'
close
    ""
    <15>
    ^self error: 'cannot close file'
selector: aSymbol
    ""
    ^self selector: aSymbol arguments: (Array new: 0)
selector: aSymbol argument: anObject
    ""
    ^self selector: aSymbol arguments: (Array with: anObject)
selector: aSymbol arguments: anArray
    ""
    ^self new selector: aSymbol arguments: anArray
selector
    "Answer the selector of the receiver."
    ^selector
arguments
    "Answer the arguments of the receiver."
    ^arguments
selector: aSymbol arguments: anArray
    selector <- aSymbol.
    arguments <- anArray
x: xCoord y: yCoord
    "Answer a new Point with coordinates xCoord and yCoord."
    ^self new x: xCoord y: yCoord
x
    "Answer the x coordinate of the receiver."
    ^x
x: aNumber
    "Set the x coordinate of the receiver to be aNumber."
    x <- aNumber
y
    "Answer the y coordinate of the receiver."
    ^y
y: aNumber
    "Set the y coordinate of the receiver to be aNumber."
    y <- aNumber
< aPoint
    "Answer whether the receiver is above and to the left of aPoint."
    ^(x < aPoint x) and: [y < aPoint y]
<= aPoint
    "Answer whether the receiver is neither below nor to the right of aPoint."
    ^(x <= aPoint x) and: [y <= aPoint y]
> aPoint
    "Answer whether the receiver is below and to the right of aPoint."
    ^(x > aPoint x) and: [y > aPoint y]
>= aPoint
    "Answer whether the receiver is neither above nor to the left of aPoint."
    ^(x >= aPoint x) and: [y >= aPoint y]
between: aPoint and: anotherPoint
    "Answer whether the receiver lies right and below aPoint as well
     as left and above anotherPoint, including the point coordinates."
    ^(aPoint <= self) and: [self <= anotherPoint]
max: aPoint
    "Answer the lower right corner of the rectangle defined
     by the receiver and aPoint."
    ^(x max: aPoint x) @ (y max: aPoint y)
min: aPoint
    "Answer the upper left corner of the rectangle defined
     by the receiver and aPoint."
    ^(x min: aPoint x) @ (y min: aPoint y)
+ delta
    ^(delta isKindOf: Number)
        ifTrue: [(x + delta) @ (y + delta)]
        ifFalse: [(x + delta x) @ (y + delta y)]
- delta
    ^(delta isKindOf: Number)
        ifTrue: [(x - delta) @ (y - delta)]
        ifFalse: [(x - delta x) @ (y - delta y)]
* scale
    ^(scale isKindOf: Number)
        ifTrue: [(x * scale) @ (y * scale)]
        ifFalse: [(x * scale x) @ (y * scale y)]
/ scale
    ^(scale isKindOf: Number)
        ifTrue: [(x / scale) @ (y / scale)]
        ifFalse: [(x / scale x) @ (y / scale y)]
negated
    ^x negated @ y negated
truncated
    ^x truncated @ y truncated
printString
    ^x printString,
     ' @ ',
     y printString
corner: aPoint
    "Answer a Rectangle whose origin is the receiver
     and whose corner is aPoint."
    ^Rectangle origin: self corner: aPoint
extent: aPoint
    "Answer a Rectangle whose origin is the receiver
     and whose extent is aPoint."
    ^Rectangle origin: self extent: aPoint
x: xCoord y: yCoord
    x <- xCoord.
    y <- yCoord
origin: originPoint corner: cornerPoint
    "Answer a new Rectangle with top left point originPoint
     and bottom right point cornerPoint."
    ^self new origin: originPoint corner: cornerPoint
origin: originPoint extent: extentPoint
    "Answer a new Rectangle with top left point originPoint
     and bottom right point cornerPoint."
    ^self new origin: originPoint corner: originPoint + extentPoint
topLeft
    "Answer the Point at the top left corner of the receiver."
    ^origin
bottomRight
    "Answer the Point at the bottom right corner of the receiver."
    ^origin
left
    ^origin x
top
    ^origin y
right
    ^corner x
bottom
    ^corner y
printString
    ^origin printString,
     ' corner: ',
     corner printString
origin: originPoint corner: cornerPoint
    origin <- originPoint.
    corner <- cornerPoint
spiral
    | aPen |
    Graphics on.
    aPen <- self new. 
    aPen down.
    (1 to: 100) do:
        [:i | aPen go: i; turn: 0.2 pi].
    String input.
    Graphics off
hilbert: n
    | aPen start h |
    Graphics on.
    aPen <- self new.
    start <- 320 @ 240.
    h <- 384.
    (1 to: n) do:
        [:i | h <- h / 2.
              start <- (start x + (h / 2)) @ (start y + (h / 2)).
              aPen up; goTo: start; down.
              self hilbertAwithPen: aPen order: i width: h].
    String input.
    Graphics off
hilbertAwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0.
         self hilbertBwithPen: aPen order: i - 1 width: h.
        ]
hilbertBwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0 negated.
         self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertAwithPen: aPen order: i - 1 width: h.
        ]
hilbertCwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0.
         self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertDwithPen: aPen order: i - 1 width: h.
        ]
hilbertDwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertCwithPen: aPen order: i - 1 width: h.
        ]
sierpinski: n
    | aPen start h |
    Graphics on.
    aPen <- self new.
    start <- 320 @ 240.
    h <- 96.
    start <- (start x) @ (start y + h).
    (1 to: n) do:
        [:i | h <- h / 2.
              start <- (start x - (h * 2)) @ (start y + h).
              aPen up; goTo: start; down.
              self sierpinskiAwithPen: aPen order: i width: h.
              aPen goDelta: h @ h negated.
              self sierpinskiBwithPen: aPen order: i width: h.
              aPen goDelta: h negated @ h negated.
              self sierpinskiCwithPen: aPen order: i width: h.
              aPen goDelta: h negated @ h.
              self sierpinskiDwithPen: aPen order: i width: h.
              aPen goDelta: h @ h].
    String input.
    Graphics off
sierpinskiAwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h negated.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: (2 * h) @ 0.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
        ]
sierpinskiBwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h negated.
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ (2 * h) negated.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h negated.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
        ]
sierpinskiCwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: (2 * h) negated @ 0.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h negated.
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
        ]
sierpinskiDwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ (2 * h).
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
        ]
north
    ^0.0 pi
west
    ^1.5 pi
south
    ^1.0 pi
east
    ^0.5 pi
new
    ""
    ^super new up; goTo: 320 @ 240; direction: Pen north
down
    drawing <- true
up
    drawing <- false
isDown
    ^drawing
isUp
    ^drawing not
location
    ^location
direction
    ^direction
direction: aDirection
    direction <- aDirection
turn: amount
    direction <- direction + amount
go: amount
    | xDelta yDelta |
    xDelta <- amount * direction sin.
    yDelta <- amount negated * direction cos.
    self goTo: (location x + xDelta) @ (location y + yDelta)
goDelta: aPoint
    self goTo: (location x + aPoint x) @ (location y + aPoint y)
goTo: aPoint
    drawing ifTrue:
        [Graphics drawLineFrom: location truncated
                  to: aPoint truncated
                  color: 15].
    location <- aPoint
on
    <248>
    ^self primitiveFailed
off
    <249>
    ^self primitiveFailed
xStart: x1 yStart: y1 xEnd: x2 yEnd: y2 color: color
    <250>
    ^self primitiveFailed
drawLineFrom: startPoint to: endPoint color: color
    self xStart: startPoint x
         yStart: startPoint y
         xEnd: endPoint x
         yEnd: endPoint y
         color: color
xLeft: x1 yTop: y1 xRight: x2 yBottom: y2 color: color
    <251>
    ^self primitiveFailed
drawRectangle: aRectangle color: color
    self xLeft: aRectangle left
         yTop: aRectangle top
         xRight: aRectangle right
         yBottom: aRectangle bottom
         color: color
