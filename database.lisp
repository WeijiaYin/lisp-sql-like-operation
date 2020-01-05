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


(defun where (&key stu_id stu_name stu_faculty course_id course_name course_prof course_faculty grade)
  #' (lambda (value)
       (and
        (if stu_id (equal (getf value :stu_id) stu_id) t)
        (if stu_name (equal (getf value :stu_name) stu_name) t)
        (if stu_faculty (equal (getf value :stu_faculty) stu_faculty) t)
        (if course_id (equal (getf value :course_id) course_id) t)
        (if course_name (equal (getf value :course_name) course_name) t)
        (if course_prof (equal (getf value :course_prof) course_prof) t)
        (if course_faculty (equal (getf value :course_faculty) course_faculty) t)
        (if grade (equal (getf value :grade) grade) t))))

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

(defun update-table (val selector-fn &key stu_id stu_name stu_faculty course_id course_name course_prof course_faculty grade)
  (cond ((equal val "student")  (setf *dbstudent*
                                      (mapcar
                                       #'(lambda (row)
                                           (when (funcall selector-fn row)
                                             (if stu_id    (setf (getf row :stu_id) stu_id))
                                             (if stu_name   (setf (getf row :stu_name) stu_name))
                                             (if stu_faculty   (setf (getf row :stu_faculty) stu_faculty)))
                                           row) *dbstudent*)))
        ((equal val "course") (setf *dbcourse*
                                    (mapcar
                                     #'(lambda (row)
                                         (when (funcall selector-fn row)
                                           (if course_id      (setf (getf row :course_id) course_id))
                                           (if course_name    (setf (getf row :course_name) course_name))
                                           (if course_prof    (setf (getf row :course_prof) course_prof))
                                           (if course_faculty (setf (getf row :course_faculty) course_faculty)))
                                         row) *dbcourse*)))
        ((equal val "enroll") (setf *dbenroll*
                                    (mapcar
                                     #'(lambda (row)
                                         (when (funcall selector-fn row)
                                           (if stu_id    (setf (getf row :stu_id) stu_id))
                                           (if course_id   (setf (getf row :course_id) course_id))
                                           (if grade   (setf (getf row :grade) grade)))
                                         row) *dbenroll*)))))
