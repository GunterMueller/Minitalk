Link subclass: #Process
    instanceVariableNames: 'block state priority'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Process class methodsFor: 'instance creation'!

on: aBlock
    ^self new ! !

!Process methodsFor: 'scheduling'!

priority
    ^priority!

priority: aPriority
    priority <- aPriority! !

!Process methodsFor: 'changing state'!

resume
    !

suspend
    !

terminate
    ! !
