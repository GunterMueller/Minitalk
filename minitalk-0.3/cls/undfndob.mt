Object subclass: #UndefinedObject
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!UndefinedObject class methodsFor: 'instance creation'!

new
    "New instances of UndefinedObject are not allowed."
    ^self shouldNotImplement! !

!UndefinedObject methodsFor: 'no-ops for copying'!

deepCopy
    "Answer nil, the receiver."
    ^nil!

shallowCopy
    "Answer nil, the receiver."
    ^nil! !

!UndefinedObject methodsFor: 'printing'!

printString
    ""
    ^'nil'! !

!UndefinedObject methodsFor: 'testing'!

isNil
    "Answer true if the receiver is nil, false otherwise."
    ^true!

notNil
    "Answer true if the receiver is not nil, false otherwise."
    ^false! !
