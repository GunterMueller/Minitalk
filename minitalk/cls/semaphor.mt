LinkedList subclass: #Semaphore
    instanceVariableNames: 'excessSignals'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Semaphore class methodsFor: 'instance creation'!

new
    ""
    ^super new initSignals!

forMutualExclusion
    ""
    ^self new signal! !

!Semaphore methodsFor: 'communication'!

critical: aBlock
    ""
    | value |
    self wait.
    value <- aBlock value.
    self signal.
    ^value!

initSignals
    ""
    excessSignals <- 0!

signal
    ""
    ^self!

wait
    ""
    ^self! !
