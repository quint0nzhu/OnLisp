;;author: quinton
;;date: 2019-03-02
;;this program is the source code of book on lisp chapter 4

(proclaim '(inline last1 single append1 conc1 mklist));;声明一些简单的函数为内联函数

(defun last1 (lst);;获得列表的最后一个元素
  (car (last lst)))

(defun single (lst);;判断一个列表中是否只包含一个元素
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj);;在列表后面追加一个元素
  (append lst (list obj)))

(defun conc1 (lst obj);;在列表后面追加一个元素，破坏性的
  (nconc lst (list obj)))

(defun mklist (obj);;用参数构告一个列表
  (if (listp obj) obj (list obj)))

(defun longer (x y);;判断x的长度是否大于y
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst);;参数列表中的每一个元素作为fn的参数被调用，收集调用返回不是nil的结果形成列表并返回
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n);;将source列表分解成长度为n的子列表，并将子列表作为元素，收集成一个列表返回
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x);;返回列表x中的所有原子
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree);;用test函数测试tree中的元素，如果为真，就删除
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))


