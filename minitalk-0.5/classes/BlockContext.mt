Context variableSubclass: #BlockContext
    instanceVariableNames: 'caller instptr stackptr stack numberargs initialip home'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!BlockContext class methodsFor: 'dummy'!

dummy
    ""
    ^self! !

!BlockContext methodsFor: 'control structures'!

whileFalse
    "Repeatedly evaluate the receiver as long as it answers false."
    ^[self value] whileFalse: []!

whileFalse: aBlock
    "Repeatedly evaluate aBlock as long as the receiver evaluates to false."
    ^self value ifFalse: [aBlock value. self whileFalse: aBlock]!

whileTrue
    "Repeatedly evaluate the receiver as long as it answers true."
    ^[self value] whileTrue: []!

whileTrue: aBlock
    "Repeatedly evaluate aBlock as long as the receiver evaluates to true."
    ^self value ifTrue: [aBlock value. self whileTrue: aBlock]! !

!BlockContext methodsFor: 'evaluating'!

value
    ""
    <6>
    ^self primitiveFailed!

value: parameter
    ""
    <6>
    ^self primitiveFailed!

value: parameter1 value: parameter2
    ""
    <6>
    ^self primitiveFailed!

value: parameter1 value: parameter2 value: parameter3
    ""
    <6>
    ^self primitiveFailed! !

!BlockContext methodsFor: 'scheduling'!

fork
    "Create and schedule a new Process for the execution of
     the expressions the receiver contains."
    self newProcess resume!

forkAt: aPriority
    !

newProcess
    "Answer a new suspended Process for the execution of the expressions
     the receiver contains. The new Process is not scheduled."
    ^Process on: self!

newProcessWith: anArray
    ! !
