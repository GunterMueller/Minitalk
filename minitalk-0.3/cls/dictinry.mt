Set variableSubclass: #Dictionary
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Dictionary methodsFor: 'accessing'!

at: key ifAbsent: aBlock
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [^aBlock value]
        ifFalse: [^association value]!

at: key
    ^self at: key
          ifAbsent: [self errorKeyNotFound]!

associationAt: key ifAbsent: aBlock
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [^aBlock value]
        ifFalse: [^association]!

associationAt: key
    ^self associationAt: key
          ifAbsent: [self errorKeyNotFound]!

at: key put: anObject
    | association |
    association <- self getAssociationFor: key.
    association isNil
        ifTrue: [association <- Association key: key value: anObject.
                 elements <- LinkedObject link: association to: elements]
        ifFalse: [association value: anObject].
    ^anObject! !

!Dictionary methodsFor: 'adding'!

add: anAssociation
    | association |
    association <- self getAssociationFor: anAssociation key.
    association isNil
        ifTrue: [elements <- LinkedObject link: anAssociation to: elements]
        ifFalse: [association value: anAssociation value].
    ^anAssociation! !

!Dictionary methodsFor: 'removing'!

remove: anObject ifAbsent: aBlock
    self shouldNotImplement!

removeKey: aKey ifAbsent: aBlock
    | linkedAssociation |
    linkedAssociation <- elements.
    linkedAssociation isNil ifTrue: [^aBlock value].
    linkedAssociation object key == aKey
        ifTrue: [elements <- elements nextLink.
                 ^self]
        ifFalse: [
            [linkedAssociation nextLink isNil] whileFalse:
                [linkedAssociation nextLink object key == aKey
                    ifTrue: [linkedAssociation nextLink:
                                 linkedAssociation nextLink nextLink.
                             ^self]
                    ifFalse: [linkedAssociation <-
                                  linkedAssociation nextLink]].
            ^aBlock value
        ]!

removeKey: aKey
    ^self removeKey: aKey
          ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'enumerating'!

do: aBlock
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         aBlock value: association value.
         linkedAssociation <- linkedAssociation nextLink]!

associationsDo: aBlock
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         aBlock value: association.
         linkedAssociation <- linkedAssociation nextLink]! !

!Dictionary methodsFor: 'testing'!

includesKey: key
    ^(self getAssociationFor: key) notNil! !

!Dictionary methodsFor: 'private'!

errorKeyNotFound
    self error: 'key not found'!

getAssociationFor: aKey
    | linkedAssociation association |
    linkedAssociation <- elements.
    [linkedAssociation notNil] whileTrue:
        [association <- linkedAssociation object.
         (association key = aKey) ifTrue: [^association].
         linkedAssociation <- linkedAssociation nextLink].
    ^nil! !
