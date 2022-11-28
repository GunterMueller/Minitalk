Collection subclass: #SequenceableCollection
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!SequenceableCollection methodsFor: 'accessing'!

size
    self subclassResponsibility! !

!SequenceableCollection methodsFor: 'removing'!

remove: oldObject ifAbsent: anExceptionBlock
    self shouldNotImplement! !

!SequenceableCollection methodsFor: 'enumerating'!

do: aBlock
    | index length |
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue: [aBlock value: (self at: index)]!

collect: aBlock
    | aStream index length |
    aStream <- WriteStream on: (self species new: self size).
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue: [aStream nextPut: (aBlock value: (self at: index))].
    ^aStream contents!

select: aBlock
    | aStream index length |
    aStream <- WriteStream on: (self species new: self size).
    index <- 0.
    length <- self size.
    [(index <- index + 1) <= length]
        whileTrue:
            [(aBlock value: (self at: index))
                ifTrue: [aStream nextPut: (self at: index)]].
    ^aStream contents! !

!SequenceableCollection methodsFor: 'growing'!

growSize
    ""
    ^10!

grow
    ""
    | newCollection |
    newCollection <- self species new: self size + self growSize.
    newCollection replaceFrom: 1 to: self size with: self.
    ^self become: newCollection! !
