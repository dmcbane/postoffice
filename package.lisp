(defpackage :net.post-office
  (:use #:cl
	#-allegro #:acl-compat.excl
	#+allegro #:excl
	#-allegro :acl-socket
	#+allegro :socket
	#-allegro :acl-compat-mp
	#+allegro :mp)
  (:export
   ;; From smtp.lisp
   #:send-letter
   #:send-smtp
   #:test-email-address

   ;; From imap.lisp
   
   #:address-name
   #:address-additional
   #:address-mailbox
   #:address-host
   
   #:alter-flags
   #:close-connection
   #:close-mailbox
   #:copy-to-mailbox
   #:create-mailbox
   #:delete-letter
   #:delete-mailbox
   
   #:envelope-date
   #:envelope-subject
   #:envelope-from
   #:envelope-sender
   #:envelope-reply-to
   #:envelope-to
   #:envelope-cc
   #:envelope-bcc
   #:envelope-in-reply-to
   #:envelope-message-id
   
   #:expunge-mailbox
   #:fetch-field
   #:fetch-letter
   #:fetch-parts
   #:*imap-version-number*
   #:make-envelope-from-text
   #:mailbox-flags      ; accessor
   #:mailbox-permanent-flags ; acc
   #:mailbox-list
   #:mailbox-list-flags
   #:mailbox-list-separator
   #:mailbox-list-name
   #:mailbox-message-count ; accessor
   #:mailbox-recent-messages ; ac
   #:mailbox-separator  ; accessor
   #:mailbox-uidvalidity
   #:make-imap-connection
   #:make-pop-connection
   #:noop
   #:parse-mail-header
   #:top-lines	; pop only
   #:unique-id  ; pop only
   
   #:po-condition
   #:po-condition-identifier
   #:po-condition-server-string
   #:po-error
   
   #:rename-mailbox
   #:search-mailbox
   #:select-mailbox
   
   ))
