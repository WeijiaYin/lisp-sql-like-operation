(defun make-student (stu_id stu_name stu_faculty)
  (list :stu_id stu_id :stu_name stu_name :stu_faculty stu_faculty))

(defun make-course (course_id course_name course_prof course_faculty)
  (list :course_id course_id :course_name course_name :course_prof course_prof :course_faculty course_faculty))

(defun make-enroll (stu_id course_id grade)
  (list :stu_id stu_id :course_id course_id :grade grade))

(defvar *dbstudent* nil)

(defvar *dbcourse* nil)

(defvar *dbenroll* nil)

(defun insert-student (student) (push student *dbstudent*))
(defun insert-course (course) (push course *dbcourse*))
(defun insert-enroll (enroll) (push enroll *dbenroll*))

(defun insert-table (val val1 val2 val3 &optional val4)
  (cond ((and (equal val "student") (null (select-student (where :stu_id val1)))) (insert-student (make-student val1 val2 val3)))
        ((and (equal val "course") (null (select-course (where :course_id val1)))) (insert-course (make-course val1 val2 val3 val4)))
        ((and (equal val "enroll") (null (select-ecnroll (where :stu_id val1 :course_id val2)))) (insert-enroll (make-enroll val1 val2 val3)))))


(defun initdb ()
  (insert-student (make-student "10001" "Mary" "MiNI"))
  (insert-student (make-student "10002" "John" "MiNI"))
  (insert-student (make-student "10003" "Harry" "MiNI"))
  (insert-student (make-student "10004" "Anna" "MiNI"))
  (insert-student (make-student "10005" "Anna" "EiTI"))

  (insert-course (make-course "MSA-001" "Calculus" "profA" "MiNI"))
  (insert-course (make-course "MSA-002" "Algorithms" "profB" "MiNI"))
  (insert-course (make-course "MSA-003" "C++" "profC" "MiNI"))
  (insert-course (make-course "MSA-004" "C" "profD" "MiNI"))

  (insert-enroll (make-enroll "10001" "MSA-001" 90))
  (insert-enroll (make-enroll "10001" "MSA-002" 60))
  (insert-enroll (make-enroll "10001" "MSA-003" 40))
  (insert-enroll (make-enroll "10002" "MSA-003" 70))
  (insert-enroll (make-enroll "10003" "MSA-003" 91))
  (insert-enroll (make-enroll "10004" "MSA-003" 92))
  (insert-enroll (make-enroll "10005" "MSA-003" 0)))

(defun len (lst)
  (if (null lst)
      0
    (+ (len (cdr lst)) 1)))

(defun cleardb ()
  (loop repeat (len *dbstudent*) do (pop *dbstudent*))
  (loop repeat (len *dbcourse*) do (pop *dbcourse*))
  (loop repeat (len *dbenroll*) do (pop *dbenroll*)))

(defun select-student (selector-fn)
  (remove-if-not selector-fn *dbstudent*))

(defun select-course (selector-fn)
  (remove-if-not selector-fn *dbcourse*))

(defun select-enroll (selector-fn)
  (remove-if-not selector-fn *dbenroll*))

(defun select-table (val selector-fn)
  (cond ((equal val "student") (select-student selector-fn))
        ((equal val "course") (select-course selector-fn))
        ((equal val "enroll") (select-enroll selector-fn)))
  )

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun delete-student (selector-fn)
  (setf *dbstudent* (remove-if selector-fn *dbstudent*)))

(defun delete-course (selector-fn)
  (setf *dbcourse* (remove-if selector-fn *dbcourse*)))

(defun delete-enroll (selector-fn)
  (setf *dbenroll* (remove-if selector-fn *dbenroll*)))

(defun delete-table (val selector-fn)
  (cond ((equal val "student") (delete-student selector-fn))
        ((equal val "course") (delete-course selector-fn))
        ((equal val "enroll") (delete-enroll selector-fn))))

(defun make-if-expr (field value row-sym)
  `(if ,value (setf (getf ,row-sym ,field) ,value)))

(defun make-if-list (fields row-sym)
  (loop while fields
        collecting (make-if-expr (pop fields) (pop fields) row-sym)))

(defmacro update(table selector-fn &rest clauses)
  (let ((selector-sym (gensym "selector-fn"))
        (row-sym (gensym "row")))
    `(let ((,selector-sym ,selector-fn))
       (setf ,table
             (mapcar
              #'(lambda (,row-sym) 
                  (when (funcall ,selector-sym ,row-sym)
                    ,@(make-if-list clauses row-sym))
                  ,row-sym)
              ,table)))))
