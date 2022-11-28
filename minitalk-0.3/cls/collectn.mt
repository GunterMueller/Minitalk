Object subclass: #Collection
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Collection class methodsFor: 'instance creation'!

with: anObject
    | newCollection |
    newCollection <- self new.
    newCollection add: anObject.
    ^newCollection!

with: firstObject with: secondObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    ^newCollection!

with: firstObject with: secondObject with: thirdObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    ^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject
    | newCollection |
    newCollection <- self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    newCollection add: fourthObject.
    ^newCollection! !

!Collection methodsFor: 'adding'!

add: anObject
    self subclassResponsibility!

addAll: aCollection
    aCollection do: [:each | self add: each].
    ^aCollection! !

!Collection methodsFor: 'removing'!

remove: anObject ifAbsent: exceptionBlock
    self subclassResponsibility!

remove: anObject
    ^self remove: anObject ifAbsent: [self errorNotFound]!

removeAll: aCollection
    aCollection do: [:each | self remove: each].
    ^aCollection! !

!Collection methodsFor: 'private'!

errorNotFound
    self error: 'object is not in the collection'!

errorNotKeyed
    self error: self class name,
                's do not respond to keyed accessing messages'! !

!Collection methodsFor: 'testing'!

isEmpty
    ^self size = 0!

includes: anObject
    self do: [:each | anObject = each ifTrue: [^true]].
    ^false!

occurrencesOf: anObject
    | tally |
    tally <- 0.
    self do: [:each | anObject = each ifTrue: [tally <- tally + 1]].
    ^tally! !

!Collection methodsFor: 'accessing'!

size
    | tally |
    tally <- 0.
    self do: [:each | tally <- tally + 1].
    ^tally! !

!Collection methodsFor: 'enumerating'!

do: aBlock
    self subclassResponsibility!

collect: aBlock
    | newCollection |
    newCollection <- self species new.
    self do: [:each | newCollection add: (aBlock value: each)].
    ^newCollection!

detect: aBlock
    ^self detect: aBlock ifNone: [self errorNotFound]!

detect: aBlock ifNone: exceptionBlock
    self do: [:each | (aBlock value: each) ifTrue: [^each]].
    ^exceptionBlock value!

inject: thisValue into: binaryBlock
    | nextValue |
    nextValue <- thisValue.
    self do: [:each | nextValue <- binaryBlock value: nextValue value: each].
    ^nextValue!

reject: aBlock
    ^self select: [:element | (aBlock value: element) == false]!

select: aBlock
    | newCollection |
    newCollection <- self species new.
    self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
    ^newCollection! !

!Collection methodsFor: 'converting'!

asSet
    | aSet |
    aSet <- Set new: self size.
    self do: [:each | aSet add: each].
    ^aSet! !

!Collection methodsFor: 'printing'!

printOn: aStream
    | tooMany |
    tooMany <- aStream position + self maxPrint.
    aStream nextPutAll: self class name, '('.
    self do:
        [:element |
          aStream position > tooMany
              ifTrue: [aStream nextPutAll: '...etc...)'. ^self].
          element printOn: aStream.
          aStream space].
    aStream nextPut: $)!

storeOn: aStream
    | noneYet |
    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: 'new)'.
    noneYet <- true.
    self do:
        [:each |
          noneYet ifTrue: [noneYet <- false]
                  ifFalse: [aStream nextPut: $;].
          aStream nextPutAll: 'add: '.
          aStream store: each].
    noneYet ifFalse: [aStream nextPutAll: '; yourself'].
    aStream nextPut: $)!

printString
    | elementString |
    elementString <- ''.
    self do: [:element | elementString <-
                         elementString, element printString, ' '].
    ^self class name, '(', elementString, ')'! !

!Collection methodsFor: 'private'!

maxPrint
    ^5000! !
