# postoffice

Mirror of the Official Repository at http://git.kpe.io/postoffice.git/

# Allegro CL imap and pop interface

copyright (c) 1999 Franz Inc.

The contents of this document are:

*   [The **IMAP** Interface](#the-imap-interface)
    
*   [The **POP** Interface](#the-pop-interface)
    
*   [The **conditions** signaled by the **IMAP** and **POP** interfaces.](#conditions)
    
*   [The **SMTP** Interface](#the-smtp-interface) (used for sending mail)
    

## The IMAP Interface

**imap** is a client-server protocol for processing electronic mail boxes.  **imap** is the successor to the **pop** protocol.   It is **not** an upward compatible successor.      The main focus of this document is the **imap** protocol.    Only one small section describes the functions in the **pop** interface.

The imap interface is based on the Imap4rev1 protocol described in rfc2060.   Where this document is describing the actions of the imap commands it should be considered a secondary source of information about those commands and rfc2060 should be considered the primary source.

The advantages of **imap** over **pop** are:

1.  **imap** can work with multiple mailboxes (**pop** works with a single mailbox)
    
2.  With **imap** you're encouraged to leave mail in mailboxes on the server machine, thus it can be read from any machine on the network.    With **pop** you're encouraged to download the mail to the client machine's disk, and it thus becomes inaccessible to all other client machines.
    
3.  **imap** parses the headers of messages thus allowing easier analysis of mail messages by the client program.
    
4.  **imap** supports searching messages for data and sorting by date.
    
5.  **imap** supports annotating messages with flags, thus making subsequent searching easier.
    

### Package

The functions in this interface are defined in the **net.post-office** package.   The previous version of this module gave this package the **po** nickname.  We've removed that nickname to reduce the possibility of clashing with user-defined packages.  You are free to add that nickname back if you so desire.

### Mailboxes

Mailboxes are repositories for messages.   Mailboxes are named by Lisp strings.  The mailbox "inbox" always exists and it is the mailbox in which new messages are stored.   New mailboxes can be created.     They can have simple names, like "foo" or they can have hierarchical names (like "clients/california/widgetco").   After connecting to an imap server you can determine what string of characters you must use between simple names to create a hierarchical name (in this example "/" was the separator character).

Each mailbox has an associated unique number called its **uidvalidity**.     This number won't change as long as **imap** is the only program used to manipulate the mailbox.   In fact if you see that the number has changed then that means that some other program has done something to the mailbox that destroyed the information that **imap** had been keeping about the mailbox.    In particular you can't now retrieve messages by their unique ids that you had used before.

### Messages

Messages in a mailbox can be denoted in one of two ways:  message sequence number or  unique id.  

The _message sequence number_ is the normal way.  The messages in a mailbox are numbered from 1 to N where N is the number of messages in the mailbox.    There are never any gaps in the sequence numbers.  If you tell **imap** to delete messages 3,4 and 5 then it will return a value telling you the it has deleted messages 3,3 and 3.  This is because when you deleted message 3, message 4 became the new message 3 just before it was deleted and then message 5 became message 3 just before it was deleted.

A _unique id_ of a message is a number associated with a message that is unique only within a mailbox.   As long as the uidvalidity value of a mailbox doesn't change, the unique ids used in deleted messages will never be reused for new messages. 

### Flags

A flag is a symbol denoting that a message or mailbox has a certain property.   We use keywords in Lisp to denote flags.   There are two kinds of flags - System and User flags.  System flags begin with the backslash character, which is an unfortunate design decision  since that means that in Lisp we have to remember to use two backslashes (e.g.  **:\\\\deleted**).    A subset of the flags can be stored permanently in the mailbox with the messages.  When a connection is made to an **imap** server it will return the list of flags and permanent flags (and these are stored in the mailbox object returned for access by the program).   If the list of permanent flags includes **:\\\\\*** then the program can create its own flag names (not beginning with a backslash) and can store them permanently in messages.

Some of the important system flags are:

*   **:\\\\seen** - this means that the message has been read   (a **fetch-letter** has been done that includes the content of the message, not just its headers)
    
*   **:\\\\deleted** \- the message will be deleted the next time an **expunge-mailbox** or **close-mailbox** is done.
    
*   **:\\\\recent** \- this is the first session to have been notified about this message being present in the mailbox.
    

### Connecting to the server

```cl
(make-imap-connection host &key user password port timeout)
```

This creates a connection to the **imap** server on machine **host** and logs in as **user** with password **password.**   The **port** argument defaults to143, which is the port on which the **imap** server normally listens.    The **timeout** argument defaults to 30 (seconds) and this value is used to limit the amount of time this imap interface code will wait for a response from the server before giving up.    In certain circumstances the server may get so busy that you see timeout errors signaled in this code.  In that case you should specify a larger timeout when connecting.

The **make-imap-connection** function returns a **mailbox** object which is then passed to other functions in this interface.   From this one connection you can access all of the mailboxes owned by **user**.

After  the connection is  established a mailbox is **not** selected.   In this state attempting to execute message access functions may result in cryptic error messages from the **imap** server that won't tell you what you need to know -- that a mailbox is not selected.   Therefore be sure to select a mailbox using **select-mailbox** shortly after connecting.

```cl
(close-connection mailbox)
```

This sends a **logout** command to the **imap** server and then closes the socket that's communicating with the **imap** server.    **mailbox** is the object returned by **make-imap-connection.**    This does _not_ close the currently select mailbox before logging out, thus messages marked to be deleted in the currently selected mailbox will _not_ be removed from the  mailbox.  Use **close-mailbox** or **expunge-mailbox** before calling this **close-connection** to ensure that messages to be deleted are deleted.

### Mailbox manipulation

These functions work on mailboxes as a whole.    The **mailbox** argument to the functions is is the object returned by **make-imap-connection.**   If a return value isn't specified for a function then the return value isn't important - if something goes wrong an error will be signaled.

```cl
(select-mailbox mailbox name)
```

makes the mailbox named by the string **name** be the current mailbox and store statistics about that mailbox in the **mailbox** object where they can be retrieved by the accessors described below.     The selected mailbox is the source for all message manipulation functions.

```cl
(create-mailbox mailbox name)
```

creates a new mailbox with the given **name**.   It is an error if the mailbox already exists.  If you want to create a mailbox in a hierarchy then you should be sure that it uses the correct hierarchy separator character string (see **mailbox-separator)**.   You do **not**   have to create intermediate levels of the hierarchy yourself -- just provide the complete name and the **imap** server will create all necessary levels.

```cl
(delete-mailbox mailbox name)
```

deletes the mailbox with the given name.

```cl
(rename-mailbox mailbox  old-name new-name)
```

changes the name of mailbox **old-name** to **new-name**.   It's an error if **new-name** already exists.  There's a special behavior if **old-name** is "inbox".  In this case all of the messages in "inbox" are moved to **new-name** mailbox, but the "inbox" mailbox continues to exist.   Note: The **imap** server supplied with Linux does **not** support this special behavior of renaming "inbox".

```cl
(mailbox-list mailbox &key reference pattern)
```

returns a list of items describing the mailboxes that match the arguments.      The **reference** is the root of the hierarchy to scan.  By default is is the empty string (from which all mailboxes are reachable).     The **pattern** is a string matched against all mailbox names reachable from **reference.** There are two special characters allowed in the **pattern:**  Asterisk (\*) matches all characters including hierarchy delimiters.   Percent (%) matches all characters but not the hierarchy delimiter.  Thus

```cl
(mailbox-list mailbox :pattern "\*")
```

returns a list of all mailboxes at all depths in the hierarchy.   

The value returned is a list of lists, but we've created the **mailbox-list** struct definition in order to make accessing the parts of the inner lists   easier.   The accessors for that structure are:

```cl
(mailbox-list-flags mailbox-list)
```

returns the flags describing this entry.   The most important flag to check is **:\\\\noselect** as this specifies that this is not a mailbox but instead just a directory in the hierarchy of mailboxes.   The flag **:\\\\noinferiors** specifies that you can't create a hierarchical mailbox name with this as a prefix.    This flag is often associated with the special mailbox "inbox".

```cl
(mailbox-list-separator mailbox-list)
```

returns a string containing the characters used to separate names in a hierarchical name.

```cl
(mailbox-list-name mailbox-list)
```

returns the name of the mailbox or directory (see mailbox-list-flags to determine which it is).

### Message manipulation

These functions work with the messages in the currently selected mailbox.     The **mailbox** argument is the object returned by **make-imap-connection.**   The **messages** argument is either a number (denoting a single message), or is the list **(:seq N M)** denoting messages **N** through **M,** or is a list of numbers and **:seq** forms denoting the messages specified in the list.

```cl
(alter-flags mailbox messages &key flags add-flags remove-flags silent uid)
```

changes the flags of the messages in the specified way.  Exactly one of  **flags, add-flags**, and **remove-flags** must  be specified.  **flags** specifies the complete set of flags to be stores in the **messages** and the other two add or remove flags.   If **uid** is true then **messages** will be interpreted as unique ids rather than message sequence numbers.      Normally **alter-flags** returns a data structure that describes the state of the flags after the alternation has been done.  This data structure can be examined  with the **fetch-field** function.    If **silent** is true then this data structure won't be created thus saving some time and space.

Removing a message from a mailbox is done by adding the **:\\\\deleted** flag to the message and then either calling **close-mailbox** or **expunge-mailbox.**

```cl
(close-mailbox mailbox)
```

permanently removes all messages flagged as **:\\\\deleted** from the currently selected mailbox and then un-selects the currently selected mailbox.  After this command has finished there is no currently selected mailbox.

```cl
(copy-to-mailbox mailbox messages destination &key uid)
```

copies the specified **messages** from the currently selected mailbox to the mailbox named **destination** (given as a string).   The flags are copied as well. The destination mailbox must already exist.  The messages are **not** removed from the selected mailbox after the copy   .If **uid** is true then the **messages** are considered to be unique ids rather than message sequence numbers.

```cl
(delete-letter mailbox messages &key expunge uid)
```

Mark the **messages** for deletion and then remove them permanently (using **expunge-mailbox**) if **expunge** is true.    **expunge** defaults to true.    If **uid** is true then the message numbers are unique ids instead of messages sequence numbers.

```cl
(expunge-mailbox mailbox)
```

permanently removes all messages flagged as **:\\\\deleted** from the currently selected mailbox.   The currently selected mailbox stays selected.

```cl
(fetch-field message part info &key uid)
```

is used to extract the desired information from the value returned by **fetch-letter**.     With **fetch-letter** you can retrieve a variety of information about one or more messages and **fetch-field** can search though that information and return a  particular piece of information about a particular letter.   **message** is the message number (it's assumed to be a message sequence number unless **uid** is true, in which case it's a unique id).   **part** is the type of information desired.  It is a string just as used in the call to **fetch-letter**.

```cl
(fetch-letter mailbox message &key uid)
```

Return the complete message, headers and body, as one big string.   This is a combination of **fetch-field** and **fetch-parts** where the part specification is "body\[\]".

```cl
(fetch-parts mailbox messages parts &key uid)
```

retrieves the specified **parts** of the specified **messages.**    If **uid** is true then the **messages** are considered to be unique ids rather than message sequence numbers.      The description of what can be specified for **parts** is quite complex and is described in the section below "Fetching a Letter".

The return value from this function is a structure that can be examined with **fetch-field**.

When the result returned includes an envelope value the following functions can be used to extract  the components of the envelope:

*   **envelope-date**
    
*   **envelope-subject**
    
*   **envelope-from**
    
*   **envelope-sender**
    
*   **envelope-reply-to**
    
*   **envelope-to**
    
*   **envelope-cc**
    
*   **envelope-bcc**
    
*   **envelope-in-reply-to**
    
*   **envelope-message-id**
    

`(noop mailbox)` does nothing but  remind the **imap** server that this client is still active, thus resetting the timers used in the server that will automatically shut down this connection after a period of inactivity.   Like all other commands if messages have been added to the currently selected mailbox, the server will return the new message count as a response to the **noop** command, and this can be check using **mailbox-message-count**.   

`(search-mailbox mailbox search-expression &key uid)` returns a list of messages in the mailbox that satisfy the **search-expression.**   If **uid** is true then unique ids will be returned instead of message sequence numbers.  See the section "Searching for messages" for details on the **search-expression**.

### Mailbox Accessors

The mailbox object contains information about the **imap** server it's connected to as well as the currently selected mailbox.   This information can potentially be updated each time a request is made to the **imap** server.    The following functions access values from the mailbox object.

```cl
(mailbox-flags mailbox)
```

returns a complete list of flags used in all the messages in this mailbox.

```cl
(mailbox-permanent-flags mailbox)
```

returns a list of flags that can be stored permanently in a message.   If the flag **:\\\\\*** is present then it means that the client can create its own flags.

```cl
(mailbox-message-count mailbox)
```

returns the number of messages in the currently selected mailbox

```cl
(mailbox-recent-messages mailbox)
```

returns the number of messages have just arrived in the mailbox.

```cl
(mailbox-separator mailbox)
```

returns the hierarchy separator string for this **imap** server.

```cl
(mailbox-uidnext mailbox)
```

returns the value predicated to be the  unique id assigned to the next message.

```cl
(mailbox-uidvalidty mailbox)
```

returns the uidvalidity value for the currently selected mailbox.

### Fetching a Letter

When using **fetch-parts** to access letters, you must specify the parts of the messages in which you're interested.   There are a wide variety of specifiers, some redundant and overlapping, described in the imap specification in rfe2060.  We'll describe the most common ones here.   The specification is always a string but it may be specified more than one thing by the use of parentheses in the string, e.g. "(flags envelope)".  

The most common specifiers are:

*   **body\[\]** - this returns the full message: headers and body.   You can use **fetch-letter** if you only want this part and you want to avoid having to call **fetch-field**.
    
*   **body\[text\]** - this returns just the the text of the body of the message, not the header.
    
*   **body** - this returns a list describing the structure of the message.
    
*   **envelope** - this parses the header and returns a list of information in it.   We've defined a set of accessors **(**like **envelope-xxx**) that allow you to retrieve the envelope information easily.
    
*   **flags** - return a list of the flags in the message
    
*   **uid** - the unique identifier of the message
    

The result of a **fetch-parts** is a data structure containing all of the requested information.   The **fetch-field** function is then used to extract the particular information for the particular message.

### Searching for Messages

.The **imap** server is able to search for messages matching a search expression.     A search-expression is a predicate or one of these forms:

*   (**and** search-expression ...)
    
*   (**or**  search-expression ...)
    
*   (**not** search-expression)
    

A predicate is

*   a number in which case the predicate is true if and only if we're are considering this message
    
*   a **(:seq N M)** expression that is true if we're considering messages N through M.
    
*   **:all** - this predicate is always true
    
*   **:answered** - true if the message has the **:\\\\answered** flag
    
*   **(:bcc "string")** \- true if the envelope structure's bcc field contains this "string".
    
*   **(:before date)** - true if the messages internal date is before this date.  The date can either be a string in the rfc822 form (e.g. "7-Mar-1999") or a lisp universal time.
    
*   **(:body "string")** \- true if the body of the message contains "string"
    
*   **(:cc "string")** -  true if the envelope structure's cc field contains this "string".
    
*   **:deleted** - true if the **:\\\\deleted** flag is set for this message
    
*   **:draft** - true if the **:\\\\draft** flag is set for this message
    
*   **:flagged** \- true if the **:\\\\flagged** flag is set for this message
    
*   **(:from "string")** -  true if the envelope structure's from  field contains this "string".
    
*   **(:header "field" "string")** - true if the message contains a header named "field" and its value contains "string".
    
*   **(:keyword flag)** - true if the specified flag is set for this message
    
*   **(:larger N)** - true if the rfc822 size of the message is larger than N.
    
*   **:new** \- true if the message has the **:\\\\recent** flag set but not the **:\\\\seen** flag.
    
*   **:seen** \- true if the message has the **:\\\\seen** flag set.
    
*   **(:sentbefore date)** - true if the message's Date header is earlier than the given date.  See the description of :before for the format of dates.
    
*   **(:senton date)** - true if the message's Date header is within the specified date.
    
*   **(:sentsince date)** \- true if the message's Date header is within or since the given date.
    
*   **(:smaller N)** - true if the rfc822 size of the message is smaller than N
    
*   **(:subject "string")** \- true if the Subject header line of the message contains "string"
    
*   **(:text "string")** \- true if the message's header or body contains the specified "string"
    
*   **(:to "string")** - true if the envelope structure's to field contains this "string".
    
*   **(:uid message-set)** - true if the message is one of the message denoted by the message set, where the message set describes messages by unique id.
    
*   **:unanswered** - true if the message does not have the **:\\\\answered** flag set
    
*   **:undeleted** - true if the message does not have the **:\\\\deleted** flag set
    
*   **:undraft** \- true if the message does not have the **:\\\\draft** flag set.
    
*   **:unflagged** \- true if the message does not have the **:\\\\flagged** flag set.
    
*   **(:unkeyword flag)** - true if the message does not have the specified flag set.
    
*   **:unseen** \- true if the message does not have the **:\\\\seen** flag set.
    

### Examples

We show an example of using this interface

**Connect to the imap server on the machine holding the email:**

```
user(2): (setq mb (make-imap-connection "mailmachine.franz.com" 

                            :user "myacct" 

                            :password "mypasswd"))

#<mailbox::imap-mailbox @ #x2064ca4a>
```

**Select the inbox, that's where the incoming mail arrives:**

```
user(3): (select-mailbox mb "inbox")

t
```

**Check how many messages are in the mailbox:**

```
user(4): (mailbox-message-count mb)

7
```

**There are seven messages at the moment.   Fetch the whole 4th message.  We could call (fetch-letter mb 4) here instead and then not have to call fetch-field later.**

```
user(5): (setq body (fetch-parts mb 4 "body\[\]"))

((4

("BODY\[\]" "Return-Path: <jkfmail@tiger.franz.com>

Received: from tiger.franz.com (jkf@tiger \[192.132.95.103\])

    by tiger.franz.com (8.8.7/8.8.7) with SMTP id LAA20261

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 11:36:26 -0700

Date: Mon, 13 Sep 1999 11:36:26 -0700

From: jkf mail tester <jkfmail@tiger.franz.com>

Message-Id: <199909131836.LAA20261@tiger.franz.com>



message number 5

")))
```

**The value was returned inside a data structure designed to hold information about one or more messages.   In order to extract the particular information we want we use fetch-field:**

```
user(6): (fetch-field 4 "body\[\]" body)

"Return-Path: <jkfmail@tiger.franz.com>

Received: from tiger.franz.com (jkf@tiger \[192.132.95.103\])

    by tiger.franz.com (8.8.7/8.8.7) with SMTP id LAA20261

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 11:36:26 -0700

Date: Mon, 13 Sep 1999 11:36:26 -0700

From: jkf mail tester <jkfmail@tiger.franz.com>

Message-Id: <199909131836.LAA20261@tiger.franz.com>



message number 5

"
```

**We use the search function to find all the messages containing the word blitzfig.  It turns out there is only one.  We then extract the contents of that message.**

```
user(7): (search-mailbox mb '(:text "blitzfig"))

(7)

user(8): (fetch-field 7 "body\[\]" (fetch-letter mb 7 "body\[\]"))

"Return-Path: <jkf@verada.com>

Received: from main.verada.com (main.verada.com \[208.164.216.3\])

    by tiger.franz.com (8.8.7/8.8.7) with ESMTP id NAA20541

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 13:37:24 -0700

Received: from main.verada.com (IDENT:jkf@localhost \[127.0.0.1\])

    by main.verada.com (8.9.3/8.9.3) with ESMTP id NAA06121

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 13:36:54 -0700

Message-Id: <199909132036.NAA06121@main.verada.com>

To: jkfmail@tiger.franz.com

Subject: s test

Date: Mon, 13 Sep 1999 13:36:54 -0700

From: jkf <jkf@verada.com>



secret word: blitzfig

ok?

"

**We've been using message sequence numbers up to now.    The are the simplest to use but if you're concerned with keeping track of messages when deletions are being done then using unique id's is useful.   Here we do the above search example using uids:**

user(9): (search-mailbox mb '(:text "blitzfig") :uid t)

(68)

user(10): (fetch-field 68 "body\[\]" (fetch-letter mb 68 "body\[\]" :uid t) :uid t)

"Return-Path: <jkf@verada.com>

Received: from main.verada.com (main.verada.com \[208.164.216.3\])

    by tiger.franz.com (8.8.7/8.8.7) with ESMTP id NAA20541

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 13:37:24 -0700

Received: from main.verada.com (IDENT:jkf@localhost \[127.0.0.1\])

    by main.verada.com (8.9.3/8.9.3) with ESMTP id NAA06121

    for <jkfmail@tiger.franz.com>; Mon, 13 Sep 1999 13:36:54 -0700

Message-Id: <199909132036.NAA06121@main.verada.com>

To: jkfmail@tiger.franz.com

Subject: s test

Date: Mon, 13 Sep 1999 13:36:54 -0700

From: jkf <jkf@verada.com>



secret word: blitzfig

ok?

"
```

**We'll delete that letter with the secret word and then note that we have only six messages in the mailbox.**

```
user(11): (delete-letter mb 68 :uid t)

(7)

user(12): (mailbox-message-count mb)

6
```

**Now we assume that a bit of time has passed and we want to see if any new messages have been delivered into the mailbox.   In order to find out we have to send a command to the imap server since it will only notify us of new messages when it responds to a command.   Since we have nothing to ask the imap server to do we issue the noop command, which does nothing on the server.**

```
user(13): (noop mb)

nil

user(14): (mailbox-message-count mb)

7
```

**The server told us that there are now 7 messages in the inbox, one more than before.  Next we create a new mailbox, copy the messages from the inbox to the new mailbox and then delete them from the inbox.  Note how we use the :seq form to specify a sequence of messages.**

```
user(15): (create-mailbox mb "tempbox")

t

user(18): (let ((count (mailbox-message-count mb)))

(copy-to-mailbox mb \`(:seq 1 ,count) "tempbox")

(delete-letter mb \`(:seq 1 ,count)))

(1 1 1 1 1 1 1)

user(19): (mailbox-message-count mb)

0
```

**When we're done there are 0 messages in the currently selected mailbox, which is inbox.  We now select the maibox we just created and see that the messages are there.**

```
user(22): (select-mailbox mb "tempbox")

t

user(23): (mailbox-message-count mb)

7
```

**Finally we shut down the connection.   Note that imap servers will automatically shut down a connection that's been idle for too long (usually around 10 minutes).  When that happens, the next time the client tries to use an imap function to access the mailbox an error will occur.   There is nothing that can be done to revive the connection however it is important to call close-imap-connection on the lisp side in order to free up the resources still in use for the now dead connection.**

```
user(24): (close-connection mb)

t
```

## The Pop interface

The **pop** protocol is a very simple means for retrieving messages from a single mailbox.     The functions in the interface are:

```cl
(make-pop-connection host &key user password port timeout)
```

This creates a connection to the **pop** server on machine **host** and logs in as **user** with password **password.**   The **port** argument defaults to 110, which is the port on which the **pop** server normally listens.    The **timeout** argument defaults to 30 (seconds) and this value is used to limit the amount of time this pop interface code will wait for a response from the server before giving up.    In certain circumstances the server may get so busy that you see timeout errors signaled in this code.  In that case you should specify a larger timeout when connecting.

The value returned by this function is a **mailbox** object.  You can call **mailbox-message-count** on the **mailbox** object to determine how many letters are currently stored in the mailbox.

```cl
(close-connection mb)
```

Disconnect from the pop server.  All messages marked for deletion will be deleted.

```cl
(delete-letter mb messages)
```

Mark the specified **messages** for deletion.  **mb** is the mailbox object returned by **make-pop-connection**.  The messages are only  marked for deletion.  They are not removed until a **close-connection** is done.  If the connection to the **pop** server is broken before a **close-connection** is done, the messages will **not** be deleted and they will no longer be marked for deletion either.

**messages** can either be a message number, a list of the form **(:seq N M)** meaning messages **N** through **M** or it can be a list of message numbers and/or **:seq** specifiers.   The messages in a mailbox are numbered starting with one.  Marking a message for deletion does not affect the numbering of other messages in the mailbox.

```cl
(fetch-letter mb message)
```

Fetch from the pop server connection **mb** the letter numbered **message**.    The letters in a mailbox are numbered starting with one.  The entire message, including the headers,  is returned as a string.    It is an error to attempt to fetch a letter marked for deletion.

```cl
(make-envelope-from-text text)
```

**text** is a string that is the first part of a mail message, including at least all of the headers lines and the blank line following the headers.  This function parses the header lines and return an **envelope** structure containing information from the header.   

```cl
(noop mb)
```

This is the no-operation command.  It is useful for letting the **pop** server know that this connection should be kept alive (**pop** servers tend to disconnect after a few minutes of inactivity).   In order to make **noop** have behavior similar to that of the **imap** version of **noop**, we don't send a 'noop' command to the pop server, instead we send a 'stat' command.    This means that after this command is completed the **mailbox-message-count** will contain the current count of messages in the mailbox.

```cl
(parse-mail-header text)
```


**text** is a string that is the first part of a mail message, including at least all of the headers lines and the blank line following the headers.  This function parses the header lines and returns an assoc list where each item has the form **(header . value)**.   Both the **header** and **value** are strings.  Note that header names will most likely be mixed case (but this is not a requirment) so you'll want to use **:test #'equalp** when searching for a particular header with **assoc**.   **parse-mail-header** returns as a second value a string that is everything after the headers (which is often referred to as the body of the message).

```cl
(top-lines mb message line-count)
```

Return a string that contains all the header lines and the first **line-count** lines of the body of **message**.   To just retrieve the headers a **line-count** of zero can be given.  See the function **make-envelope-from-text** for a means of reading the information in the header.

```cl
(unique-id mb &optional message)
```

Return the unique indentifier for the given message, or for all non-deleted messages if **message** is nil.   The unique identifier is is a string that is different for every message.   If the **message** argument  is not given then this command returns a list of lists where each list contains two items: the message number and the unique id.


## Conditions

When an unexpected event occurs a condition is signaled.   This applies to both the **imap** and **pop** interfaces.  There are two classes of conditions signaled by this package:

*   **po-condition** - this class denotes conditions that need not and in fact should not interrupt program flow.   When the mailbox server is responding to a command it sometimes sends informational warning messages and we turn them into conditions.    It's important for all messages from the server to be read and processed otherwise the next command issued will see messages in response to the previous command.   Therefore the user code should never do a non-local-transfer in response to a **po-condition.**
*   **po-error -** this class denotes conditions that will prevent execution from continuing.  If one of these errors is not caught, the interactive debugger will be entered.

Instances of both of these condition classes have these slots in addition to the standard condition slots: 

| Name | Accessor | Value |
|------|----------|-------|
|identifier|po-condition-identifier|keyword describing the kind of condition being signaled.  See the table below for the possible values.|
|server-string|po-condition-server-string|If the condition was created because of a messages sent from the mailbox server then this is that message.|

The meaning of the identifier value is as follows

|**Identifier**|Kind|Meaning|
|--------------|----|-------|
|**:problem**|po-condition|The server has responded with a warning message.   The most likely warning is that the mailbox can only be opened in read-only mode due to another processing using it.|
|**:unknown-ok**|po-condition|The server has sent an informative message that we don't understand.   It's probably safe to ignore this.|
|**:unknown-untagged**|po-condition|The server has sent an informative message that we don't understand.   It's probably safe to ignore this.|
|**:error-response**|po-error|The server cannot execute the requested command.|
|**:syntax-error**|po-error|The arguments to a function in this package are malformed.|
|**:unexpected**|po-error|The server has responded a way we don't understand and which prevents us from continuing|
|**:server-shutdown-connection**|po-error|The connection to the server has been broken.  This usually occurs when the connection has been idle for too long and the server intentionally disconnects.    Just before this condition is signaled we close down the socket connection to free up the socket resource on our side.  When this condition is signaled the user program should not use the mailbox object  again (even to call **close-connection** on it).|
|**:timeout**|po-error|The server did not respond quickly enough.   The timeout value is set in the call to **make-imap-connection.**|

## The SMTP Interface

With the smtp interface, a Lisp program can contact a mail server and send electronic mail.   The contents of the message must be a simple text string.  There is no provision for encoding binary data and sending it as a Mime attachment.

```cl
(send-letter mail-server from to message &key subject reply-to)
```

**mail-server** can be a string naming a machine or an integer IP address.   The **mail-server** is contacted and asked to send a **message** (a string) **from** a given email address **to** a given email address or list of addresses.   The email addresses must be of the form "foo" or ["foo@bar.com"](mailto:foo@bar.com).  You can **not** use addresses like ["Joe <foo@bar.com>"](mailto:Joe%20%3cfoo@bar.com%3e) or ["(Joe) foo@bar.com"](mailto:(Joe)%20foo@bar.com).  

A mail header is built and prepended to the **message** before it is sent.   The mail header includes a **From** and **To** line and will optionally include a  **Subject** and **Reply-To** line if those are given in the call to **send-letter.**.

The text of the **message** should be lines separated by #\\newline's.    The **smtp** interface will automatically insert the necessary #\\returns's when it transmits the message to the mail server.

```cl
(send-smtp mail-server from to &rest messages)
```

**mail-server** can be a string naming a machine or an integer IP address.   The **mail-server** is contacted and asked to send a  message **from** a given email address **to** a given email address or list of addresses.    The email addresses must be of the form "foo" or ["foo@bar.com"](mailto:foo@bar.com).  You can **not** use addresses like ["Joe <foo@bar.com>"](mailto:Joe%20%3cfoo@bar.com%3e) or ["(Joe) foo@bar.com"](mailto:(Joe)%20foo@bar.com).  

The message sent is a concatenation of all of the **messages** (which should be strings).   A header is **not** prepended to the message.   This means that the application program can build its own header if it wants to include in that header more than **send-letter** supports (e.g. a Mime encoded attachment).  If no header is provided then some mail servers (e.g. **sendmail**) will notice this fact and will automatically create a header.

The text of the **messages** should be lines separated by #\\newline's.    The **smtp** interface will automatically insert the necessary #\\returns's when it transmits the message to the mail server.
