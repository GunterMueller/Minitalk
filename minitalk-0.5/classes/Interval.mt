SequenceableCollection subclass: #Interval
    instanceVariableNames: 'start stop step'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Interval class methodsFor: 'instance creation'!

from: start to: stop by: step
    ""
    ^self new start: start stop: stop step: step!

from: start to: stop
    ""
    ^self from: start to: stop by: 1! !

!Interval methodsFor: 'private'!

species
    ^Array! !

!Interval methodsFor: 'access'!

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
    ^self errorSubscriptBounds: aNumber!

at: anInteger put: aNumber
    ""
    ^self shouldNotImplement! !

!Interval methodsFor: 'instance initialization'!

do: aBlock
    | aValue |
    aValue <- start.
    step < 0
        ifTrue: [[stop <= aValue]
                     whileTrue: [aBlock value: aValue.
                                 aValue <- aValue + step]]
        ifFalse: [[stop >= aValue]
                     whileTrue: [aBlock value: aValue.
                                 aValue <- aValue + step]]! !

!Interval methodsFor: 'instance initialization'!

start: num1 stop: num2 step: num3
    ""
    start <- num1.
    stop <- num2.
    step <- num3! !

!Interval methodsFor: 'printing'!

printString
    ^step = 1 ifTrue: [start printString,
                       ' to: ',
                       stop printString]
              ifFalse: [start printString,
                        ' to: ',
                        stop printString,
                        ' by: ',
                        step printString]! !
