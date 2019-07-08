;;author: quinton
;;date: 2019-03-02
;;this program is the source code of book on lisp

(defun make-dbms (db);;构造一个数据库，自带三个函数闭包
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)
   ))

(defun bad-reverse (lst);;逆序一个列表，不好的实现
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(defun good-reverse (lst);;逆序一个列表，好的实现
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

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

(defun mostn (fn lst);;获得列表中元素被fn调用后，返回值最大的所有元素组成的子列表和这个最大值
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun mapa-b (fn a b &optional (step 1));;从a到b步长step得到的数作为参数调用fn，收集结果变成列表返回
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n);;从0到n步长1得到的数作为参数调用fn，收集结果变成列表返回
  (mapa-b fn 0 n))

(defun map1-n (fn n);;从1到n步长1得到的数作为参数调用fn，收集结果变成列表返回
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn);;初始值为start，下一个值为以循环变量为参数调用succ-fn后得到的结果，每次以循环变量为参数调用test-fn所得到的结果作为判断循环是否结束
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts);;以lsts中的元素为参数调用fn，得到的结果作为元素，形成列表返回
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts);;同时对多个列表应用fn，并将结果收集成一个列表返回
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args);;遍历树结构的每一个元素，并在其上调用fn
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

(defun readlist (&rest args);;读入一行并以列表形式返回
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args);;打印一行提示符
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args);;模拟Lisp的toplevel环境
  (format *query-io* "Entering break-loop.'~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args);;构造一个字符串
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args);;构造一个符号
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args);;接受一系列对象，并打印重读它们
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym);;返回一个由该符号名称里的字符所组成的列表
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))

(defvar *!equivs* (make-hash-table));;建立一个哈希表，保存函数和它对应的破坏性版本，以非破坏性版本为键

(defun ! (fn);;返回函数的破坏性版本
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!);;更新或设置一个函数的破坏性版本
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn);;函数第一次运行后，会把结果存入哈希表中，下次运行时，直接从哈希表中查找
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun compose (&rest fns);;复合函数调用，最后个函数可以有多个参数，其他的函数只能有一个参数
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else);;函数if调用后如果返回真则再调用函数then，如果不为真，可以再调用函数else
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns);;几个函数调用结果的交集
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns);;几个函数调用结果的合集
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base);;用来定义线性列表的递归函数的函数
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(defun ttrav (rec &optional (base #'identity));;在树结构上进行递归操作
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(defun trec (rec &optional (base #'identity));;在树形结构上遍历
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                      #'(lambda ()
                          (self (car tree)))
                      #'(lambda ()
                          (if (cdr tree)
                              (self (cdr tree))))))))
    #'self))

(defstruct node contents yes no);;定义一个node结点结构体

(defvar *nodes* (make-hash-table));;定义一个存放node的哈希表

(defun defnode (name conts &optional yes no);;生成一个node并放入哈希表中
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defun run-node (name);;用来遍历树的函数
  (let ((n (gethash name *nodes*)))
        (cond ((node-yes n)
               (format t "~A~%>> " (node-contents n))
               (case (read)
                 (yes (run-node (node-yes n)))
                 (t (run-node (node-no n)))))
              (t (node-contents n)))))

(defun defnode (name conts &optional yes no);;这个版本不用再定义node数据结构了，也不用单独定义函数来遍历这些结点，节点成为闭包，只要调用闭包就可以了
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>> " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t (funcall (gethash no *nodes*)))))
            #'(lambda() conts))))

(defvar *nodes-list* nil);;定义一个列表存放节点

(defun defnode (&rest args);;定义一个节点，参数为上一个问题答案，本问题内容，回答yes的答案，回答no的答案
  (push args *nodes-list*)
  args)

(defun compile-net (root);;使用静态引用的编译过程，递归地进行处理，直到树的叶子节点，在递归过程层层返回时，每一步都返回了两个目标函数对应的节点，而不仅仅是给出它们的名字。
  (let ((node (assoc root *nodes-list*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (yes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))

(defmacro nif (expr pos zero neg);;expr如果大于0执行pos，如果小于0执行neg，如果等于0执行zero
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro memq (obj lst);;自定义的member，以eq作为相等判断标准
  `(member ,obj ,lst :test #'eq))

(defmacro while (test &body body);;实现的while循环
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr);;用于测试宏展开的宏，只展开一级
  `(pprint (macroexpand-1 ',expr)))

(defmacro our-expander (name) `(get ,name 'expander));;普通宏的工作模式

(defmacro our-defmacro (name parms &body body);;自己实现defmacro的实现
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,parms (cdr ,g)
                     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr);;自己实现macroexpand-1的实现
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))

(defun make-initforms (bindforms);;由初始化列表生成赋初值的代码
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                  (list b nil)))
          bindforms))

(defun make-stepforms (bindforms);;生成更新循环变量的代码
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))

(defmacro our-do (bindforms (test &rest result) &body body);;实现do宏的代码
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
        ,label
        (if ,test
            (return (progn ,@result)))
        ,@body
        (psetq ,@(make-stepforms bindforms))
        (go ,label))))

(defmacro our-and (&rest args);;自己实现and，效率比较慢
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args);;自已实现and，效率比较高
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))

;;(defun move-objs-fn (objs dx dy);;移动各对象并将变化后的局部重画，函数版本
;;  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
;;    (dolist (o objs)
;;      (incf (obj-x o) dx)
;;      (incf (obj-y o) dy))
;;    (multiple-value-bind (xa ya xb yb) (bounds objs)
;;      (redraw (min x0 xa) (min y0 ya)
;;              (max x1 xb) (max y1 yb)))))

;;(defun scale-objs-fn (objs factor);;放缩各对象并将变化后的局部重画，函数版本
;;  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
;;    (dolist (o objs)
;;      (setf (obj-dx o) (* (obj-dx o) factor)
;;            (obj-dy o) (* (obj-dy o) factor)))
;;    (multiple-value-bind (xa ya xb yb) (bounds objs)
;;      (redraw (min x0 xa) (min y0 ya)
;;              (max x1 xb) (max y1 yb)))))

;;(defmacro with-redraw ((var objs) &body body);;把局部重画的代码抽象成宏
;;  (let ((gob (gensym))
;;        (x0 (gensym)) (y0 (gensym))
;;        (x1 (gensym)) (y1 (gensym)))
;;    `(let ((,gob ,objs))
;;       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
;;         (dolist (,var ,gob) ,@body)
;;         (multiple-value-bind (xa ya xb yb) (bounds ,gob)
;;           (redraw (min ,x0 ,xa) (min ,y0 ,ya)
;;                   (max ,x1 ,xb) (max ,y1 ,yb)))))))

;;(defun move-objs-mac (objs dx dy);;移动各对象并将变化后的局部重画，宏版本
;;  (with-redraw (o objs)
;;    (incf (obj-x o) dx)
;;    (incf (obj-y o) dy)))

;;(defun scale-objs-mac (objs factor);;放缩各对象并将变化后的局部重画，宏版本
;;  (with-redraw (o objs)
;;    (setf (obj-dx o) (* (obj-dx o) factor)
;;          (obj-dy o) (* (obj-dy o) factor))))

(defmacro before-mac (x y seq);;判断一个列表中x是否在y前面，该宏利用预先求值避免捕捉
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
        (position yval seq))))

(defmacro for ((var start stop) &body body);;利用闭包避免捕捉

  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

(defmacro for-gensym ((var start stop) &body body);;通过gensym避免捕捉
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro for-right ((var start stop) &body body);;一个for的正确版本的宏
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro for-error1 ((var start stop) &body body);;多重求值的for宏
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

(defmacro for-error2 ((var start stop) &body body);;错识误的求值顺序的for宏
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop)
          (,var ,start (1+ ,var)))
         ((> ,var ,gstop))
       ,@body)))

(defun ntha (n lst);;返回列表第n个元素，正确版本
  (if (= n 0)
      (car lst)
      (ntha (- n 1) (cdr lst))))

(defmacro nthb (n lst);;返回列表第n个元素，错误宏版本，不能编译，但在SBCL上可以正常编译
  `(if (= ,n 0)
       (car ,lst)
       (nthb (- ,n 1) (cdr ,lst))))

(defmacro nthc (n lst);;返回列表第n个元素，非递归版宏
  `(do ((n2 ,n (1- n2))
        (lst2 ,lst (cdr lst2)))
       ((= n2 0) (car lst2))))

(defmacro nthd (n lst);;解决递归宏展开的两个办法之一，把递归部分变成函数
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst);;把递归部分变成全局函数
  (if (= n 0)
      (car lst)
      (nth-fn (- n 1) (cdr lst))))

(defmacro nthe (n lst);;把递归部分变成局部函数
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

(defun or-expand (args);;递归函数，用来递归生成展开式
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))

(defmacro ora (&rest args);;自定义or函数，调用递归函数来生成展开式
  (or-expand args))

(defmacro orb (&rest args);;自定义or函数的另一个版本，在宏的参数个数上做递归
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))

