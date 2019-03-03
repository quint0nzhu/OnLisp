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

(defun find2 (fn lst);;从lst中找一个元素，使得fn用它作为参数调用后返回结果不为nil，返回两个对象：该元素和fn以该元素为参数调用后的结果
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql));;如果在列表中x在y的前面则返回x开头的子列表，否则返回nil
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql));;如果在列表中x在y的后面则返回x开头的子列表，否则返回nil
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql));;判断obj在lst列表中是否有重复
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst);;lst中某一个元素，作为fn的参数调用后，如果返回t，则从该元素开始把lst列表断开，变成两个子列表
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst);;找到lst中一个元素使以它为参数调用fn得到的值最大，返回该元素及那个最大值，相同时，返回先出现的元素
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst);;返回lst中的某一个元素，该元素在fn调用下值最大，fn为二元谓词，作用于列表中的两个元素
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
