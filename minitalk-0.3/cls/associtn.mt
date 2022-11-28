Magnitude subclass: #Association
    instanceVariableNames: 'key value'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Association class methodsFor: 'instance creation'!

key: aKey value: aValue
    "Answer a new association with key aKey and value aValue."
    ^self new key: aKey value: aValue! !

!Association methodsFor: 'comparison'!

= anAssociation
    ""
    ^key = anAssociation key!

< anAssociation
    ""
    ^key < anAssociation key! !

!Association methodsFor: 'accessing'!

key
    "Answer the key of the receiver."
    ^key!

value
    "Answer the value of the receiver."
    ^value!

key: anObject
    "Modify the receiver key to be anObject."
    key <- anObject!

value: anObject
    "Modify the receiver value to be anObject."
    value <- anObject!

key: aKey value: aValue
    "Modify the receiver key to be aKey and the
     receiver value to be aValue."
    key <- aKey.
    value <- aValue! !

!Association methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream."
    self key printOn: aStream.
    aStream nextPutAll: '==>'.
    self value printOn: aStream!

printString
    ""
    ^self key printString,
     ' ==> ',
     self value printString! !

!Association methodsFor: 'storing'!

storeOn: aStream
    "Print an executable representation of the receiver on aStream."
    aStream nextPutAll: '(Association key: '.
    self key storeOn: aStream.
    aStream nextPutAll: ' value: '.
    self value storeOn: aStream.
    aStream nextPut: $)! !
