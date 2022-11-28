Boolean subclass: #True
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!True methodsFor: 'logical operations'!

printString
    ""
    ^'true'!

not
    "Answer true if the receiver is false and false otherwise."
    ^false!

& aBoolean
    "Answer true if the receiver and aBoolean are both true,
     false otherwise."
    ^aBoolean!

| aBoolean
    "Answer true if either the receiver, aBoolean, or both are true,
     false otherwise."
    ^true!

and: aBlock
    "If the receiver is true, compute and answer the block result,
     otherwise answer false without evaluating the block."
    ^aBlock value!

or: aBlock
    "If the receiver is false, compute and answer the block result,
     otherwise answer true without avaluating the block."
    ^true! !

!True methodsFor: 'control structures'!

ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, answer nil."
    ^trueBlock value!

ifFalse: falseBlock
    "If the receiver is false, evaluate the falseBlock and answer the result.
     If the receiver is true, answer nil."
    ^nil!

ifTrue: trueBlock ifFalse: falseBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^trueBlock value!

ifFalse: falseBlock ifTrue: trueBlock
    "If the receiver is true, evaluate the trueBlock and answer the result.
     If the receiver is false, evaluate the falseBlock and answer the result."
    ^trueBlock value!
