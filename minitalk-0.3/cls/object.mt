nil subclass: #Object
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Object methodsFor: 'general queries'!

class
    "Answer the receiver's class."
    <1>
    ^self primitiveFailed!

isKindOf: aClass
    "Answer true if the receiver's class is aClass or inherits
     from aClass, false otherwise."
    ^self class == aClass
        ifTrue: [true]
        ifFalse: [self class inheritsFrom: aClass]!

isMemberOf: aClass
    "Answer true if the receiver's class is aClass, false otherwise."
    ^self class == aClass!

respondsTo: aSymbol
    "Answer true if aSymbol is a message selector for a method defined in
     the receiver's class or a class it inherits from, false otherwise."
    ^self class canUnderstand: aSymbol! !

!Object methodsFor: 'private'!

species
    "Answer the class which is the class of the receiver or is similar to
     the class of the receiver. Used for obtaining copies of the receiver."
    ^self class! !

!Object methodsFor: 'specific queries'!

isNil
    "Answer true if the receiver is nil, false otherwise."
    ^false!

notNil
    "Answer true if the receiver is not nil, false otherwise."
    ^true! !

!Object methodsFor: 'unusual queries'!

yourself
    "Answer the receiver. Useful for cascading."
    ^self! !

!Object methodsFor: 'interfacing with the debugger'!

halt: aString
    "Trap to the debugger."
    <0>
    ^self primitiveFailed!

halt
    "Trap to the debugger. Uses default message."
    ^self halt: 'halt encountered'!

basicInspect
    "Show the receiver. Is never redefined in subclasses."
    <22>
    ^self primitiveFailed!

inspect
    "Show the receiver. Can be redefined in subclasses."
    ^self basicInspect! !

!Object methodsFor: 'interfacing with the user'!

confirm: aString
    <23>
    ^self primitiveFailed!

stop
    "Stop the MiniTalk system."
    (self confirm: 'Are you sure?')
        ifTrue: [^Driver stop]
        ifFalse: [^'ok']! !

!Object methodsFor: 'interfacing with the error handler'!

error: aString
    "Report an error to the user. Can be redefined in subclasses."
    ^self halt: aString!

doesNotUnderstand: aMessage
    "The standard handler for messages not understood by the receiver."
    ^self halt: self printString,
                'does not understand',
                aMessage selector printString! !

!Object methodsFor: 'often-used error messages'!

primitiveFailed
    "Primitive has failed."
    ^self error: 'a primitive has failed'!

shouldNotImplement
    "Message is not appropriate for object."
    ^self error: 'this message is not appropriate for this object'!

subclassResponsibility
    "Subclass should have overridden one of the messages."
    ^self error: 'my subclass should have overridden one of my messages'!

errorSubscriptBounds: index
    ""
    ^self error: 'subscript is out of bounds: ', index printString! !

!Object methodsFor: 'size queries'!

totalSize
    "Answer the total size of the receiver, i.e. the number of named
     instance variables plus the number of indexed instance variable."
    <24>
    ^self primitiveFailed!

basicSize
    "Answer the number of indexed instance variables.
     Is never redefined in subclasses."
    ^self totalSize - self class instSize!

size
    "Answer the number of indexed instance variables.
     Can be redefined in subclasses."
    ^self basicSize! !

!Object methodsFor: 'system primitive'!

become: otherObject
    ""
    <48>
    ^self primitiveFailed! !

!Object methodsFor: 'accessing and modifying named instance variables'!

instVarAt: anInteger
    "Answer the value of the named instance variable at position
     anInteger. Legal positions range from 1 to self class instSize."
    <30>
    ^self primitiveFailed!

instVarAt: anInteger put: anObject
    "Change the value of the named instance variable at position
     anInteger. Legal positions range from 1 to self class instSize.
     Answer anObject."
    <31>
    ^self primitiveFailed! !

!Object methodsFor: 'accessing and modifying indexed instance variables'!

basicAt: anInteger
    "Answer the value of the indexed instance variable at position
     anInteger. Legal positions range from 1 to self basicSize.
     Is not redefined in any subclass."
    <32>
    ^self primitiveFailed!

basicAt: anInteger put: anObject
    "Change the value of the indexed instance variable at position
     anInteger. Legal positions range from 1 to self basicSize.
     Answer anObject. Is not redefined in any subclass."
    <33>
    ^self primitiveFailed!

at: anInteger
    "Same as basicAt:, but may be redefined in subclasses."
    ^self basicAt: anInteger!

at: anInteger put: anObject
    "Same as basicAt:put:, but may be redefined in subclasses."
    ^self basicAt: anInteger put: anObject! !

!Object methodsFor: 'copying'!

copy
    "Answer a shallow copy of the receiver. Subclasses typically
     override this method when a shallow copy is not sufficient."
     ^self shallowCopy!

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
    ^newObject!

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
    ^newObject! !

!Object methodsFor: 'identity comparisons'!

== anObject
    "Answer true if the receiver and anObject are the same,
     otherwise false. This not redefined in any subclass."
    <9>
    ^self primitiveFailed!

~~ anObject
    "Answer true if the receiver and anObject are not the same,
     otherwise false."
    ^(self == anObject) not! !

!Object methodsFor: 'equality comparisons'!

= anObject
    "Default implementation of equality is equivalence."
    ^self == anObject!

~= anObject
    "Inequality."
    ^(self = anObject) not!

hash
    ^self class name hash! !

!Object methodsFor: 'message handling'!

perform: aSymbol
    <47>
    ^self primitiveFailed!

perform: aSymbol with: anObject
    <47>
    ^self primitiveFailed!

perform: aSymbol with: firstObject with: secondObject
    <47>
    ^self primitiveFailed!

perform: aSymbol with: firstObject with: secondObject with: thirdObject
    <47>
    ^self primitiveFailed! !

!Object methodsFor: 'printing'!

printString
    "Answer a string whose characters are a description of the receiver."
    | className article |
    className <- self class name.
    (className at: 1) isVowel
        ifTrue: [article <- 'an ']
        ifFalse: [article <- 'a '].
    ^article, className! !

!Object methodsFor: 'storing'!

storeOn: aStream
    ^aStream nextPutAll: 'not implemented yet'!

storeString
    "Answer a string representation that can be used
     to reconstruct the receiver."
    | aStream |
    aStream <- WriteStream on: (String new: 16).
    self storeOn: aStream.
    ^aStream contents! !
