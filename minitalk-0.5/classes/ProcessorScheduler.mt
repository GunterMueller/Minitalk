Object subclass: #ProcessorScheduler
    instanceVariableNames: 'activePriority activeProcess'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!ProcessorScheduler class methodsFor: 'instance creation'!

startMinitalk
    ^self! !

!ProcessorScheduler methodsFor: 'accessing'!

activePriority
    ^activePriority!

activeProcess
    ^activeProcess! !

!ProcessorScheduler methodsFor: 'process state change'!

terminateActive
    !

yield
    ! !

!ProcessorScheduler methodsFor: 'priority names'!

timingPriority
    ^7!

highIOPriority
    ^6!

lowIOPriority
    ^5!

userInterruptPriority
    ^4!

userSchedulingPriority
    ^3!

userBackgroundPriority
    ^2!

systemBackgroundPriority
    ^1! !
