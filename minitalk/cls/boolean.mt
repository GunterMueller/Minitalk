Object subclass: #Boolean
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Boolean class methodsFor: 'instance creation'!

new
    "New instances of Boolean are not allowed."
    ^self shouldNotImplement! !

!Boolean methodsFor: 'logical operations'!

eqv: aBoolean
    "Answer true if the receiver and aBoolean have the same truth values,
     otherwise answer false."
    ^self == aBoolean!

xor: aBoolean
    "Answer true if the receiver and aBoolean have different truth values,
     otherwise answer false."
    ^self ~~ aBoolean! !
