Object subclass: #Link
    instanceVariableNames: 'nextLink'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Link class methodsFor: 'instance creation'!

nextLink: aLink
    "Create an instance of Link that references the argument, aLink."
    ^self new nextLink: aLink! !

!Link methodsFor: 'accessing'!

nextLink
    "Answer the receiver's reference."
    ^nextLink!

nextLink: aLink
    "Set the receiver's reference to be the argument, aLink."
    nextLink <- aLink! !
