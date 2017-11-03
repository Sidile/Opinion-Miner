(load "topics.lisp")
(defstruct document posscore negscore word-count topic-count document-words)

(defun split-line (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp (x) (position x "\""))

(defun split-review (string &key (delimiterr #'delimiterr))
  (loop :for beg = (position-if-not delimiterr string)
    :then (position-if-not delimiterr string :start (1+ end))
    :for end = (and beg (position-if delimiterr string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterr (y) (position y ".?!"))

(defun split-lexi (string &key (delimiterl #'delimiterl))
  (loop :for beg = (position-if-not delimiterl string)
    :then (position-if-not delimiterl string :start (1+ end))
    :for end = (and beg (position-if delimiterl string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
	
(defun delimiterl (z) (position z "#"))

(defun split-sentence (string &key (delimitero #'delimitero))
  (loop :for beg = (position-if-not delimitero string)
    :then (position-if-not delimitero string :start (1+ end))
    :for end = (and beg (position-if delimitero string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
	
(defun delimitero (o) (position o " ,"))
	
	
(defun lexi-search (word)		
	(with-open-file (input "SentiWordNet lexicon re.txt")
		(loop for line = (read-line input nil)
			while line do 
			(defparameter *parsed-lexi-line* (split-lexi line))
			;;Creates the word with pos and possitivity 
			(when (equalp (string-trim 
				'(#\Space #\Tab #\Newline) (nth 4 *parsed-lexi-line*)) word )
				(defparameter *lexicon-entry* nil)
				(push (nth 4 *parsed-lexi-line*) *lexicon-entry*)
				(push (nth 3 *parsed-lexi-line*) *lexicon-entry*)
				(push (nth 2 *parsed-lexi-line*) *lexicon-entry*)
				(push (nth 1 *parsed-lexi-line*) *lexicon-entry*)
				(push (nth 0 *parsed-lexi-line*) *lexicon-entry*)
				(return-from lexi-search *lexicon-entry*)))
		(return-from lexi-search 0)))

(defun create-document (words lexi-sentence)
	;;create a document with posscore negscore word-count topic-count document-words
	(setq new-document (make-document
		:posscore 0
		:negscore 0
		:word-count 0
		:topic-count (0 0 0 0)
		:document-words nil ))
	(loop for word in lexi-sentence
		do
			(push word (document-words))
			(setf document-posscore new-document (+ document-posscore (topic-word-posscore word)))
			(setf document-negscore new-document (+ document-negscore (topic-word-negscore word)))
			(incf document-word-count new-document)
	)
)

(with-open-file (input "reviews2.txt")
	(defparameter words (make-hash-table))
	;;(setf *random-state* (make-random-state t))
	(setf (gethash 1 words) 0) ;Hash key 1 contains the total number of word occurances in the hash table
	(setf (gethash 2 words) nil) ;Hash key 2 contains a list of all keys in the hash table
	;;(defparameter *testword* (lexi-search "able"))
	;;(collective-topic words *testword*)
	;;(write-topic words 0)
	;;(defparameter *testword2* (lexi-search "good"))
	;;(collective-topic words *testword2*)
	;;(write-topic words 0)
	;;(defparameter *testword3* (lexi-search "good"))
	;(collective-topic words *testword3*)
	;;(write-topic words 0)
	;;(defparameter topics (random-topics words))
	;;(write-topic (nth 1 topics) 1)
	;;(write-topic (nth 2 topics) 2)
	;;(write-topic (nth 3 topics) 3)
	;;(write-topic (nth 4 topics) 4)
	(loop for line = (read-line input nil) ;Iterates through the file
		while line do
		(defparameter *parsed-line* (split-line line))
		(defparameter *parsed-review* (split-review (nth 17 *parsed-line*)))
		(defparameter lexi-review nil)
		(loop for sentence in *parsed-review* ;Iterates through the review
			while sentence do
			(defparameter *parsed-sentence* (split-sentence sentence))
			(defparameter lexi-sentence nil)
			(loop for word in *parsed-sentence* ;Iterates through the sentence
				while word do 
				(defparameter *lexi-word* (lexi-search 
					(string-trim '(#\Space #\Tab #\Newline #\, #\.) word)))
				(if (not (equalp *lexi-word* 0))
					progn(
					;;(format t "~a ~%" *lexi-word* ))))))
						(collective-topic words *lexi-word*) ;Sends the word to the collective-topic
						(push (gethash (nth 1 *lexi-word*) words) lexi-sentence)
						)))
			(push (create-document words lexi-sentence) lexi-review))) 
	(write-topic words "tryatopic.txt") ;Writes the collective-topic to a file
	(defparameter topics (random-topics words))
	(write-topic (nth 0 topics) 1)
	(write-topic (nth 1 topics) 2)
	(write-topic (nth 2 topics) 3)
	(write-topic (nth 3 topics) 4)
	(write-topic (nth 4 topics) 0)
)
	
	
		
		



