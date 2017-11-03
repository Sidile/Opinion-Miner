(defstruct topic-word pos id posscore negscore count word)

;;(defvar topics nil)
(defun print-collective-topic (key word)
	(format t "Test topic: ~S ~%" word)
	(with-open-file (stream "tryatopic.txt"
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
		(format stream "~S ~%" word))) ;Write hashed value to file
		
(defun print-topic1 (key word)
	(format t "Topic 1: ~S ~%" word)
	(with-open-file (stream "topic1.txt"
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
		(format stream "~S ~%" word))) ;Write hashed value to file

(defun print-topic2 (key word)
	(format t "Topic 2: ~S ~%" word)
	(with-open-file (stream "topic2.txt"
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
		(format stream "~S ~%" word))) ;Write hashed value to file
		
(defun print-topic3 (key word)
	(format t "Topic 3: ~S ~%" word)
	(with-open-file (stream "topic3.txt"
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
		(format stream "~S ~%" word))) ;Write hashed value to file

(defun print-topic4 (key word)
	(format t "Topic 4: ~S ~%" word)
	(with-open-file (stream "topic4.txt"
						:direction :output
						:if-exists :append
						:if-does-not-exist :create)
		(format stream "~S ~%" word))) ;Write hashed value to file


(defun random-topics (words)
	(defvar topics nil)
	(setf *random-state* (make-random-state t))
	(defparameter topic1 (make-hash-table))
	(defparameter topic2 (make-hash-table))
	(defparameter topic3 (make-hash-table))
	(defparameter topic4 (make-hash-table))
	(defparameter topic5 (make-hash-table))
	(defparameter topic6 (make-hash-table))
	(setf (gethash 1 topic1) 0)
	(setf (gethash 2 topic1) nil)
	(setf (gethash 1 topic2) 0)
	(setf (gethash 2 topic2) nil)
	(setf (gethash 1 topic3) 0)
	(setf (gethash 2 topic3) nil)
	(setf (gethash 1 topic4) 0)
	(setf (gethash 2 topic4) nil)
	(setf (gethash 1 topic5) 0)
	(setf (gethash 2 topic5) nil)
	(setf (gethash 1 topic6) 0)
	(setf (gethash 2 topic6) nil)
	(push topic1 topics)
	(push topic2 topics)
	(push topic3 topics)
	(push topic4 topics)
	(push words topics)
	;;(maphash #'topic-assign-random words)
	(loop for id in (gethash 2 words)
		do
		(loop for x from 0 to (-(topic-word-count (gethash id words))1)
			do
			(defparameter chosen-topic (+(random 4)1))
			(format t "~a ~%" chosen-topic)
			(setq word (gethash id words))
			(if (not (gethash id (nth chosen-topic topics))) ;If the word does not exist, create it
				(progn	
					(setq new-word (make-topic-word
						:pos (topic-word-pos word)
						:id (topic-word-id word)
						:posscore (topic-word-posscore word)
						:negscore (topic-word-negscore word)
						:word (topic-word-word word)
						:count 1))
					(setf (gethash id (nth chosen-topic topics)) new-word)
					(push id (gethash 2 (nth chosen-topic topics))) ;Add id to list of ids
				)
				(incf (topic-word-count (gethash id (nth chosen-topic topics)))) ;If the word exists, increase count
			)
			(incf (gethash 1 (nth chosen-topic topics))) ;Increase total word count for the topic
		)
	)
	(return-from random-topics topics)
)

(defun first-topics (words topics)
	(defvar new-topics nil)
	(setf *random-state* (make-random-state t))
	(defparameter topic1 (make-hash-table))
	(defparameter topic2 (make-hash-table))
	(defparameter topic3 (make-hash-table))
	(defparameter topic4 (make-hash-table))
	(defparameter topic5 (make-hash-table))
	(defparameter topic6 (make-hash-table))
	(setf (gethash 1 topic1) 0)
	(setf (gethash 2 topic1) nil)
	(setf (gethash 1 topic2) 0)
	(setf (gethash 2 topic2) nil)
	(setf (gethash 1 topic3) 0)
	(setf (gethash 2 topic3) nil)
	(setf (gethash 1 topic4) 0)
	(setf (gethash 2 topic4) nil)
	(setf (gethash 1 topic5) 0)
	(setf (gethash 2 topic5) nil)
	(setf (gethash 1 topic6) 0)
	(setf (gethash 2 topic6) nil)
	(push topic1 new-topics)
	(push topic2 new-topics)
	(push topic3 new-topics)
	(push topic4 new-topics)
	(push words new-topics)
	;;(maphash #'topic-assign-random words)
	(loop for id in (gethash 2 words)
		do
		(loop for x from 0 to (-(topic-word-count (gethash id words))1)
			do
			(defparameter chosen-topic (+(random 4)1))
			(format t "~a ~%" chosen-topic)
			(setq word (gethash id words))
			(if (not (gethash id (nth chosen-topic topics))) ;If the word does not exist, create it
				(progn	
					(setq new-word (make-topic-word
						:pos (topic-word-pos word)
						:id (topic-word-id word)
						:posscore (topic-word-posscore word)
						:negscore (topic-word-negscore word)
						:word (topic-word-word word)
						:count 1))
					(setf (gethash id (nth chosen-topic topics)) new-word)
					(push id (gethash 2 (nth chosen-topic topics))) ;Add id to list of ids
				)
				(incf (topic-word-count (gethash id (nth chosen-topic topics)))) ;If the word exists, increase count
			)
			(incf (gethash 1 (nth chosen-topic topics))) ;Increase total word count for the topic
		)
	)
	(return-from random-topics topics)
)

(defun collective-topic (words lexi-word)
	(defparameter *lexi-id* (parse-integer (nth 1 lexi-word)))
	;;(format t "~a ~%" *lexi-id*)
	;;(format t "~a ~%" (gethash *lexi-id* words))
	(if (not (gethash *lexi-id* words)) ;If the word does not exist, create it
		(progn		
			(setq word (make-topic-word
				:pos (nth 0 lexi-word)
				:id *lexi-id*
				:posscore (nth 2 lexi-word)
				:negscore (nth 3 lexi-word)
				:word (nth 4 lexi-word)
				:count 1))
			(setf (gethash *lexi-id* words) word)
			(push (topic-word-id word) (gethash 2 words)) ;Add id to list of ids
		)
		(incf (topic-word-count(gethash *lexi-id* words))) ;Else increase count
	)
	(incf (gethash 1 words)) ;Increase total word count for collective-topic
)
		
(defun write-topic (topic topic-num)
	;;(defparameter *lexi-id* (parse-integer (nth 1 word)))
	(case topic-num
		(0 (maphash #'print-collective-topic topic)) ;Write collective-topic to file
		(1 (maphash #'print-topic1 topic))
		(2 (maphash #'print-topic2 topic))
		(3 (maphash #'print-topic3 topic))
		(4 (maphash #'print-topic4 topic))
	)
	;;(format t "~a ~%" (gethash *lexi-id* words))
	;;(format t "~a ~%" (gethash 1740 words))
	;;(if (gethash 1740 words)
		;;(format t " ~% word ~%")
	;;)
)