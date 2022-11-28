Boolean subclass: #False
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!False methodsFor: 'logical operations'!

printString
    ""
    ^'false'!

not
    "Answer true if the receiver is false and false otherwise."
    ^true!

& aBoolean
    "Answer true if the receiver and aBoolean are both true,
     false otherwise."
    ^false!

| aBoolean
    "Answer true if either the receiver, aBoolean, or both are true,
     false otherwise."
    ^aBoolean!

and: aBlock
    "If the receiver is true, compute and answer the block result,
     otherwise answer false without evaluating the block."
    ^false!

or: aBlock
    "If the receiver is false, compute and answer the block result,
     otherwise answer true without avaluating the block."
    ^aBlock value! !

!False methodsFor: 'control structures'!

ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, answer nil."
    ^nil!

ifFalse: falseBlock
    "If the receiver is false, evaluate the falseBlock and answer the result.
     If the receiver is true, answer nil."
    ^falseBlock value!

ifTrue: trueBlock ifFalse: falseBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^falseBlock value!

ifFalse: falseBlock ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^falseBlock value!
