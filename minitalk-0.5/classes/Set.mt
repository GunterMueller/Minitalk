Collection variableSubclass: #Set
    instanceVariableNames: 'elements'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Set methodsFor: 'accessing'!

at: index
    self errorNotKeyed!

at: index put: anObject
    self errorNotKeyed! !

!Set methodsFor: 'adding'!

add: anObject
    | linkedObject |
    linkedObject <- elements.
    [linkedObject notNil] whileTrue:
        [linkedObject object = anObject ifTrue: [^anObject].
         linkedObject <- linkedObject nextLink].
    elements <- LinkedObject link: anObject to: elements.
    ^anObject! !

!Set methodsFor: 'removing'!

remove: anObject ifAbsent: aBlock
    | linkedObject |
    linkedObject <- elements.
    linkedObject isNil ifTrue: [^aBlock value].
    linkedObject object = anObject ifTrue:
        [elements <- linkedObject nextLink.
         ^anObject].
    [linkedObject nextLink notNil] whileTrue:
        [linkedObject nextLink object = anObject ifTrue:
            [linkedObject nextLink: linkedObject nextLink nextLink.
             ^anObject].
         linkedObject <- linkedObject nextLink].
    ^aBlock value!

remove: anObject
    ^self remove: anObject
          ifAbsent: [^self error: 'object is not in set']! !

!Set methodsFor: 'enumerating'!

do: aBlock
    | linkedObject |
    linkedObject <- elements.
    [linkedObject notNil] whileTrue:
        [aBlock value: linkedObject object.
         linkedObject <- linkedObject nextLink]! !
