Link subclass: #LinkedObject
    instanceVariableNames: 'object'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!LinkedObject class methodsFor: 'instance creation'!

link: anObject to: aLink
    ""
    ^(super nextLink: aLink) object: anObject! !

!LinkedObject methodsFor: 'access'!

printString
    ""
    ^self object printString,
     '->',
     self nextLink printString!

object
    ""
    ^object!

object: anObject
    ""
    object <- anObject! !
