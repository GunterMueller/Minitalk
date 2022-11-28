SequenceableCollection subclass: #LinkedList
    instanceVariableNames: 'firstLink lastLink'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!LinkedList methodsFor: 'testing'!

isEmpty
    "Answer true if the receiver has no elements, otherwise answer false."
    ^firstLink isNil! !

!LinkedList methodsFor: 'accessing'!

at: index
    "Answer the element at position index."
    | count element |
    count <- 1.
    element <- firstLink.
    [element isNil] whileFalse:
        [count = index
            ifTrue: [^element]
            ifFalse: [count <- count + 1.
                      element <- element nextLink]].
    ^self errorSubscriptBounds: index!

at: index put: element
    "Report an error since at:put: is not appropriate for a LinkedList."
    ^self error: 'do not store into a LinkedList using at:put:'! !

!LinkedList methodsFor: 'adding'!

addFirst: aLink
    "Add aLink to the beginning of the receiver's list. Answer aLink."
    aLink nextLink: firstLink.
    firstLink isNil ifTrue: [lastLink <- aLink].
    firstLink <- aLink.
    ^aLink!

addLast: aLink
    "Add aLink to the end of the receiver's list. Answer aLink."
    aLink nextLink: nil.
    firstLink isNil
        ifTrue: [firstLink <- aLink]
        ifFalse: [lastLink nextLink: aLink].
    lastLink <- aLink.
    ^aLink!

add: aLink
    "Add aLink to the receiver's list. Answer aLink."
    ^self addLast: aLink! !

!LinkedList methodsFor: 'removing'!

remove: aLink ifAbsent: aBlock
    "Remove aLink from the receiver's list and answer it.
     Answer the value of aBlock if aLink is not an element of the list."
    | tempLink |
    aLink == firstLink
        ifTrue: [firstLink <- aLink nextLink.
                 aLink == lastLink ifTrue: [lastLink <- nil]]
        ifFalse: [tempLink <- firstLink.
                  [tempLink isNil ifTrue: [^aBlock value].
                   tempLink nextLink == aLink] whileFalse:
                      [tempLink <- tempLink nextLink].
                  tempLink nextLink: aLink nextLink.
                  aLink == lastLink ifTrue: [lastLink <- tempLink]].
    aLink nextLink: nil.
    ^aLink!

remove: aLink
    "Remove aLink from the receiver's list and answer it.
     Report an error if aLink is not an element of the list."
    ^self remove: aLink
          ifAbsent: [^self error: 'link not found in linked list']!

removeFirst
    "Remove the receiver's first element and answer it.
     If the receiver is empty, report an error."
    ^self remove: firstLink!

removeLast
    "Remove the receiver's last element and answer it.
     If the receiver is empty, report an error."
    ^self remove: lastLink! !

!LinkedList methodsFor: 'enumerating'!

do: aBlock
    "Evaluate aBlock for each element of the receiver's list."
    | element |
    element <- firstLink.
    [element isNil] whileFalse:
        [aBlock value: element.
         element <- element nextLink]! !
