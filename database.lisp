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
