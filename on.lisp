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

(defmacro our-let (binds &body body);;let的宏实现，先把参数名提取出来，再加上body，再把参数值提取出来
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x)
                             (car x)
                             x))
              binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x)
                      (cadr x)
                      nil))
              binds)))

(defmacro when-bind ((var expr) &body body);;三种绑定变量的宏之一，这里先计算expr，根据其值来决定是否执行body
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body);;三种绑定变量的宏之一，接收一个由成对(sym expr)所组成的列表，如果任何一个expr返回nil，则整个when-bind*表达式就返回nil，且每个sym的值可以传递，和let*一样
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body);;三种绑定变量的宏之一，把syms中的符号变成由gensym生成的符号
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

(defun condlet-binds (vars cl);;返回各变量对应的值
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (car bindform) vars))
                        (cdr bindform))))
          (cdr cl)))


(defun condlet-clause (vars cl bodfn);;绑定变量的值，并调用代码主体
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defmacro condlet (clauses &body body);;接受一个绑定语句的列表和一个代码主体，每个绑定语句是否生效都要视其对应的测试表达式而定，第一个测试表达式为真的绑定语句所构造的绑定环境将会胜出，代码主体将在这个绑定环境中被求值
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car

                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

;;(defmacro with-db-mac (db &body body);;完全用宏实现的with宏
;;  (let ((temp (gensym)))
;;    `(let ((,temp *db*))
;;       (unwind-protect
;;            (progn
;;              (setq *db* ,db)
;;              (lock *db*)
;;              ,@body)
;;         (progn
;;           (release *db*)
;;           (setq *db* ,temp))))))

;;(defun with-db-fn (old-db new-db body);;实现with宏的辅助函数
;;  (unwind-protect
;;       (progn
;;         (setq *db* new-db)
;;         (lock *db*)
;;         (funcall body))
;;    (progn
;;      (release *db*)
;;      (setq *db* old-db))))

;;(defmacro with-db-mac-fn (db &body body);;用函数和宏结合起来实现的with宏
;;  (let ((gbod (gensym)))
;;    `(let ((,gbod #'(lambda () ,@body)))
;;       (declare (dynamic-extent ,gbod))
;;       (with-db-fn *db* ,db ,gbod))))

(defmacro if3 (test t-case nil-case ?-case);;三值逻辑的条件选择宏
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg);;接受数值表达式作为第一个参数，并根据这个表达式的符号来求值接下来三个参数中的一个
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choices);;使用高效地测试集合的成员关系，测试一个对象是否属于某些备选对象的集合
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args);;上面in宏的引用版变形
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices);;in宏的自定义测试函数版，此处使用一元测试函数
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defun >casex (g cl);;生成case每个子句展开式的函数
  (let ((key (car cl))
        (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

(defmacro >case (expr &rest clauses);;和case类似，但会对每个子句里的键进行求值
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defmacro while (test &body body);;简单的迭代宏，如果test为真就执行body
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body);;简单的迭代宏，如果test为真就停止执行body
  `(do ()
       (,test)
     ,@body))

(defmacro for11-7 ((var start stop) &body body);;简单的迭代宏，在start和stop之间迭代
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro do-tuples/o (parms source &body body);;求值主体时绑定一组变量到一个列表中相继的子序列上，不折回到表头
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc #'(lambda ,parms ,@body)
                  ,@(map0-n #'(lambda (n)
                                `(nthcdr ,n ,src))
                            (- (length source)
                               (length parms))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))

(defmacro do-tuples/c (parms source &body body);;求值主体时绑定一组变量到一个列表中相继的子序列上，然后折回到表头
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar #'(lambda (args)
                                    `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n #'(lambda (n)
                                         `(nth ,(1- n)
                                               ,rest))
                                     len))))))))))

(defun mvdo-rebind-gen (rebinds);;实现具体的初值和迭代变的绑定的函数
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                         'setq
                         'multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

(defun mvdo-gen (binds rebinds test body);;mvdo*宏的函数实现部分
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defmacro mvdo* (parm-cl test-cl &body body);;支持多值的do*版本宏
  (mvdo-gen parm-cl parm-cl test-cl body))

;;(mvdo* (((px py) (pos player) (move player mx my));;一个利用mvdo*实现的碰撞游戏
;;        ((x1 y1) (pos obj1) (move obj1 (- px x1)
;;                                  (- py y1)))
;;        ((x2 y2) (pos obj2) (move obj2 (- px x2)
;;                                  (- py y2)))
;;        ((mx my) (mouse-vector) (mouse-vector))
;;        (win nil (touch obj1 obj2))
;;        (lose nil (and (touch obj1 player)
;;                       (touch obj2 player))))
;;       ((or win lose) (if win 'win 'lose))
;;       (clear)
;;       (draw obj1)
;;       (draw obj2)
;;       (draw player))

(defun shuffle (x y);;按顺序合并两个列表，多余元素补在新生成列表后面
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(defmacro mvpsetq (&rest args);;多值并行设置变量
  (let* ((pairs (group args 2))
         (syms (mapcar #'(lambda (p)
                           (mapcar #'(lambda (x) (gensym))
                                   (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                     ,@(mapcan #'(lambda (p s)
                                   (shuffle (mklist (car p))
                                            s))
                               pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defmacro mvdo (binds (test &rest result) &body body);;do的多值绑定版本，注意和do*的多值绑定版本的区别
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x)
                                           (gensym))
                                       (car b))
                               (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                             (list var (cadr b)))
                         binds
                         temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
               (mappend #'mklist (mapcar #'car binds))
               (mappend #'mklist temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan #'(lambda (b)
                                 (if (third b)
                                     (list (car b)
                                           (third b))))
                             binds))
          (go ,label)))))

(define-modify-macro toggle2 () not);;利用CL提供的宏自动定义基于setf的宏

(defmacro toggle (&rest args);;可以将参数列表中每个元素都toggle
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))

(defmacro allf (val &rest args);;将同一值赋给多个广义变量
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args));;将参数设置为nil

(defmacro tf (&rest args) `(allf t ,@args));;将参数设置为t

(define-modify-macro concf (obj) nconc);;破坏性修改列表结尾的宏

(defun conc1f/function (place obj);;在列表结尾追加一个元素的函数
  (nconc place (list obj)))

(define-modify-macro conc1f (obj) conc1f/function);;将追加元素的函数封装成一个宏

(defun concnew/function (place obj &rest args);;只有在列表中没有该元素时才会追加到结尾
  (unless (apply #'member obj place args)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args);;将上面那个函数封装成宏
  concnew/function)

(defmacro _f (op place &rest args);;破坏性把函数应用于一个广义变量
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args);;将列表中某个位置的元素移除
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args);;按照给定判定条件，将列表中某个位置的元素移除
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place);;弹出列表中前n个元素
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

(defmacro sortf (op &rest places);;将参数进行排序
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
                             (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m)
                                        (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m)
                                        (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar
                      #'(lambda (arg)
                          `(unless (,op ,(car rest) ,arg)
                             (rotatef ,(car rest) ,arg)))
                      (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

(defvar *world* '((a . 2) (b . 16) (c . 50) (d . 20) (f . 12)));;实际数据库

(defvar *cache* (make-hash-table));;数据库缓存

(defun retrieve (key);;查询函数
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
        (values x y)
        (cdr (assoc key *world*)))))

(defsetf retrieve (key) (val);;对上面的查询函数求逆
  `(setf (gethash ,key *cache*) ,val))

(defun avg (&rest args);;函数求平均值
  (/ (apply #'+ args) (length args)))

(defmacro avg-mac (&rest args);;用宏在编译期计算平均值，对length的调用会在编译期完成
  `(/ (+ ,@args) ,(length args)))

(defun most-of (&rest args);;参数中一半以上是真，就返回真，否则返回nil
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of-mac (&rest args);;在编译期间计算比较，参数中一半以上是真就返回真，理想情况下，只需对刚过半的参数求值
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need)))
                     args)))))

(defun nthmost (n lst);;返回数列中第n大的值，要先复制，再排序
  (nth n (sort (copy-list lst) #'>)))

(defun nthmost-gen (var vars &optional long?);;生成比较每一个数值的代码
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list
                                 (reverse vars)
                                 (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))

(defun gen-start (glst syms);;开始生成比较代码
  (reverse
   (maplist #'(lambda (syms)
                (let ((var (gensym)))
                  `(let ((,var (pop ,glst)))
                     ,(nthmost-gen var (reverse syms)))))
            (reverse syms))))

(defmacro nthmost-mac (n lst);;定义一个宏版本的nthmost，n是个比较小的数话，可以每次找前n个最大的数，然后返回之中最小的，就是结果
  (if (and (integerp n) (< n 20))
      (with-gensyms (glst gi)
        (let ((syms (map0-n #'(lambda (x) (gensym)) n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))

(defconstant *segs* 20)
(defconstant *du* (/ 1.0 *segs*))
(defconstant *pts* (make-array (list (1+ *segs*) 2)))

(defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3);;生成贝塞尔曲线的宏
  (with-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
    `(let ((,gx0 ,x0) (,gy0 ,y0)
           (,gx1 ,x1) (,gy1 ,y1)
           (,gx3 ,x3) (,gy3 ,y3))
       (let ((cx (* (- ,gx1 ,gx0) 3))
             (cy (* (- ,gy1 ,gy0) 3))
             (px (* (- ,x2 ,gx1) 3))
             (py (* (- ,y2 ,gy1) 3)))
         (let ((bx (- px cx))
               (by (- py cy))
               (ax (- ,gx3 px ,gx0))
               (ay (- ,gy3 py ,gy0)))
           ,@(map1-n #'(lambda (n)
                         (let* ((u (* n *du*))
                                (u2 (* u u))
                                (u3 (expt u 3)))
                           `(setf (aref *pts* ,n 0)
                                  (+ (* ax ,u3)
                                     (* bx ,u2)
                                     (* cx ,u)
                                     ,gx0)
                                  (aref *pts* ,n 1)
                                  (+ (* ay ,u3)
                                     (* by ,u2)
                                     (* cy ,u)
                                     ,gy0))))
                     (1- *segs*))
           (setf (aref *pts* *segs* 0) ,gx3
                 (aref *pts* *segs* 1) ,gy3))))))

(defmacro aif (test-form then-form &optional else-form);;指代变形if宏
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body);;指代变形when宏
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body);;指代变形while宏
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args);;指代变形and宏
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses);;指代变形cond宏
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body);;指代变形lambda宏
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args);;指代变形block宏
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args))))))
               args)))

(defmacro aif2 (test &optional then else);;绑定第一个值，并测试第二个值，if的多值指代宏
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body);;when的多值指代宏
  `(aif2 ,test (progn ,@body)))

(defmacro awhile2 (test &body body);;while的多值指代宏
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
              (aif2 ,test
                    (progn ,@body)
                    (setq ,flag nil))))))

(defmacro acond2 (&rest clauses);;cond的多值指代宏
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(let ((g (gensym)));;另一个版本的read宏，用第二返回值指示失败
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body);;遍历一个文件里的所有表达式
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

(defun rbuild (expr);;根据参数列表决定如何生成函数调用关系
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns);;不包含compose的函数调用
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns);;包含compose的函数调用
  (let ((g (gensym)))
    `(lambda (,g)

       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

(defmacro fn (expr) `#',(rbuild expr));;通用的用于构造函数的宏

(defmacro alrec (rec &optional base);;用it指代当前列表的car，用rec指代递归调用，递归调用列表中的每一个元素
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts);;把上面的宏改写成on-cdrs形式，使得可以方便定义有名函数
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defun our-copy-list (lst);;利用on-cdrs定义的copy-list函数
  (on-cdrs (cons it rec) nil lst))

(defun our-remove-duplicates (lst);;利用on-cdrs定义的remove-duplicates函数
  (on-cdrs (adjoin it rec) nil lst))

(defun our-find-if (fn lst);;利用on-cdrs定义的find-if函数
  (on-cdrs (if (funcall fn it) it rec) nil lst))

(defun our-some (fn lst);;利用on-cdrs定义的some函数
  (on-cdrs (or (funcall fn it) rec) nil lst))

(defun unions (&rest sets);;利用on-cdrs定义的union函数
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets);;利用on-cdrs定义的intersection函数
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs);;利用on-cdrs定义的set-difference函数
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args);;利用on-cdrs定义的maxmin函数，返回列表中的最大值和最小值
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

(defmacro atrec (rec &optional (base 'it));;在树形结构上遍历，利用指代
  (let ((lfn (gensym))
        (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees);;将上面的宏包装成on-trees形式
  `(funcall (atrec ,rec ,base) ,@trees))

(defun our-copy-tree (tree);;利用on-trees定义our-copy-tree函数
  (on-trees (cons left right) it tree))

(defun our-count-leaves (tree);;利用on-trees定义our-count-leaves函数
  (on-trees (+ left (or right 1)) 1 tree))

(defun our-flatten (tree);;利用on-trees定义our-flatten函数
  (on-trees (nconc left right) (mklist it) tree))

(defun our-rfind-if (fn tree);;利用on-trees定义our-rfind-if
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))

(defconstant unforced (gensym));;定义一个常量符号

(defstruct delay forced closure);;定义闭包

(defmacro delay (expr);;delay的实现，延时求值
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda()
                 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x);;force的实现，不会延时求值
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
          (delay-forced x))
      x))

(defmacro abbrev (short long);;自动定义缩略语
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names);;一次性定义多个缩略语
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

(defmacro propmacro (propname);;自动定义访问宏
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props);;一次性定义多个访问宏
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p)
                   props))))

(defun a+expand
    (args syms);;生成其展开式，并聚集出一个这些生成符号的列表
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(a+expand (cdr args)
                      (append syms (list sym)))))
      `(+ ,@syms)))

(defmacro a+ (&rest args);;使it总是绑定到上个参数返回的值上
  (a+expand args nil))

(defun alist-expand (args syms);;与a+expand几乎一样
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it, sym))
           ,(alist-expand (cdr args)
                          (append syms (list sym)))))
      `(list ,@syms)))

(defmacro alist (&rest args);;与a+几乎一样
  (alist-expand args nil))

(defun pop-symbol (sym);;弹出第一个符号
  (intern (subseq (symbol-name sym) 1)))

(defun anaphex (args expr);;递归展开器
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex (cdr args)
                     (append expr (list sym)))))
      expr))

(defmacro defanaph (name &optional calls);;自动产生a+和alist宏
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex1 (args call);;宏调用中所有参数都将被求值，it总是被绑定在前一个参数的值上
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args);;只有第一个参数是必须求值的，并且it将被绑定在这个值上
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defun anaphex3 (op args);;第一个参数被按照广义变量来对待，而it将被绑定在它的初始值上
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro defanaph1 (name &key calls (rule :all));;将上面三个展开器拼接在一起
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

;;(set-macro-character #\';;'的可能定义
;;                     #'(lambda (stream char)
;;                         (declare (ignore char))
;;                         (list 'quote (read stream t nil t))))

(set-dispatch-macro-character #\# #\?;;一个用于常委函数的读取宏
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))

(set-macro-character #\] (get-macro-character #\)));;把]识别成定界符

(set-dispatch-macro-character #\# #\[;;一个定义定界符的读取宏
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                        ((> i (floor (cadr pair)))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

(let ((rpar (get-macro-character #\))));;辅助函数，将函数应用到它读到的东西
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  #'(lambda (stream char1 char2)
                                      (declare (ignore char1 char2))
                                      (apply fn
                                             (read-delimited-list right stream t))))))

(defmacro defdelim (left right parms &body body);;一个用于定义定界符读取宏的宏
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(defdelim #\{ #\} (&rest args) `(fn (compose ,@args)));;一个用于函数型复合的读取宏

(defun destruc (pat seq &optional (atom? #'atom) (n 0));;遍历匹配模式，将每个变量和运行期对应对象的位置关联在一起
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
            (let ((p (car pat))
                  (rec (destruc (cdr pat) seq atom? (1+ n))))
              (if (funcall atom? p)
                  (cons `(,p (elt ,seq ,n))
                        rec)
                  (let ((var (gensym)))
                    (cons (cons `(,var (elt ,seq ,n))
                                (destruc p var atom?))
                          rec))))))))

(defun dbind-ex (binds body);;生成宏展开代码
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
              binds)
         ,(dbind-ex (mapcan #'(lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                            binds)
                    body))))

(defmacro dbind (pat seq &body body);;通用序列解构操作符
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(defmacro with-matrix (pats ar &body body);;数组上的解构
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan
                #'(lambda (pat)
                    (incf row)
                    (setq col -1)
                    (mapcar #'(lambda (p)
                                `(,p (aref ,gar
                                           ,row
                                           ,(incf col))))
                            pat))
                pats))
         ,@body))))

(defmacro with-array (pat ar &body body);;稀疏表的解构
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
              pat)
         ,@body))))

(defmacro with-struct ((name . fields) struct &body body);;结构体上的解构
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f), gs)))
              fields)
         ,@body))))

(defun wplac-ex (binds body);;为解构宏创建出与之对应的按名调用版本
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                          binds)
         ,(wplac-ex (mapcan #'(lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                            binds)
                    body))))

(defmacro with-places (pat seq &body body);;序列上的引用解构
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun binding (x binds);;形成绑定列表
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun varsym? (x);;判断是否为模式变量
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun match (x y &optional binds);;逐个元素地比较它的参数，建立起来了一系列值和变量之间的赋值关系
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun var? (x);;测试是否某个东西是一个变量
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun vars-in (expr &optional (atom? #'atom));;返回一个表达式中的所有匹配变量
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defmacro if-match (pat seq then &optional else);;慢的匹配操作符，通过比较模式跟序列来建立绑定
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun length-test (pat rest);;测试是否具有正确的长度
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))

(defun gensym? (s);;是否为gensym生成的符号
  (and (symbolp s) (not (symbol-package s))))

(defun match1 (refs then else);;生成模式树上每个叶子的匹配代码
  (dbind ((pat expr) . rest) refs
         (cond ((gensym? pat)
                `(let ((,pat ,expr))
                   (if (and (typep ,pat 'sequence)
                            ,(length-test pat rest))
                       ,then
                       ,else)))
               ((eq pat '_) then)
               ((var? pat)
                (let ((ge (gensym)))
                  `(let ((,ge ,expr))
                     (if (or (gensym? ,pat) (equal ,pat ,ge))
                         (let ((,pat ,ge)) ,then)
                         ,else))))
               (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)));;定义模式内容和模式结构之间的差别

(defun gen-match (refs then else);;为嵌套的模式递归生成匹配代码传给match1
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defmacro pat-match (pat seq then else);;完成展开工作，它不为模式变量建立任何新绑定
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)

        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defmacro if-match (pat seq then &optional else);;快速匹配操作符，不再创建变量绑定的列表，而是将变量的值保存进这些变量本身
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defun make-db (&optional (size 100));;创建一个数据库
  (make-hash-table :size size))

(defvar *default-db* (make-db));;创建一个默认数据库

(defun clear-db (&optional (db *default-db*));;清除一个数据库
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*));;数据库查询
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*));;插入新事实到数据库项中
  (push val (db-query key db)))

(defmacro fact (pred &rest args);;用来给数据库加入新事实
  `(progn (db-push ',pred ',args)
          ',args))

(defun lookup (pred args &optional binds);;返回一个能够使模式匹配到数据库中某个事实的所有绑定的列表
  (mapcan #'(lambda (x)
              (aif2 (match x args binds) (list it)))
          (db-query pred)))

(defun interpret-not (clause binds);;逻辑非过滤
  (if (interpret-query clause binds)
      nil
      (list binds)))

(defun interpret-or (clauses binds);;逻辑或过滤
  (mapcan #'(lambda (c)
              (interpret-query c binds))
          clauses))

(defun interpret-and (clauses binds);;逻辑与过滤
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (interpret-query (car clauses) b))
              (interpret-and (cdr clauses) binds))))

(defun interpret-query (expr &optional binds);;递归生成绑定
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lookup (car expr) (cdr expr) binds))))

(defmacro with-answer (query &body body);;一个查询解释器
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query #'atom))
         ,@body))))

(defun compile-not (q body);;逻辑非过滤
  (let ((tag (gensym)))
    `(if (block ,tag
           ,(compile-query q `(return-from ,tag nil))
           t)
         ,body)))

(defun compile-or (clauses body);;逻辑或过滤
  (if (null clauses)
      nil
      (let ((gbod (gensym))
            (vars (vars-in body #'simple?)))
        `(labels ((,gbod ,vars ,body))
           ,@(mapcar #'(lambda (cl)
                         (compile-query cl `(,gbod ,@vars)))
                     clauses)))))

(defun compile-and (clauses body);;逻辑与过滤
  (if (null clauses)
      body
      (compile-query (car clauses)
                     (compile-and (cdr clauses) body))))

(defun compile-simple (q body);;没有逻辑过滤和表达式的情况
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-query (q body);;递归生成绑定，用变量来保存它们的值
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defmacro with-answer (query &body body);;查询编译器
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query `(progn ,@body))))

(defvar *actual-cont* #'values)

(define-symbol-macro *cont*
  *actual-cont*)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,' ,f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))

(defvar *saved* nil)

(=defun re-start ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))

(=defun dft-node (tree)
  (cond ((null tree) (re-start))
        ((atom tree) (=values tree))
        (t (push #'(lambda () (dft-node (cdr tree)))
                 *saved*)
           (dft-node (car tree)))))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
          (t (princ node)
             (re-start)))))

(defstruct proc pri state wait)

(proclaim '(special *procs* *proc*))

(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
                        (format t "~%>> ")
                        (princ (eval (read)))
                        (pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc
            :state #'(lambda (,(gensym))
                       ,expr
                       (pick-process))
            :pri ,pri)
           *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
           (setq *procs* nil)
           ,@body
           (catch *halt* (loop (pick-process)))))

(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
            (let ((val (or (not (proc-wait p))
                           (funcall (proc-wait p)))))
              (when val
                (setq proc1 p
                      max pri
                      val1 val))))))
    (values proc1 val1)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
               #'(lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))

(defvar *open-doors* nil)

(=defun pedestrian ()
        (wait d (car *open-doors*)
              (format t "Entering ~A~%" d)))

(program ped ()
         (fork (pedestrian) 1))

(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
        (format t "Approach ~A. " door)
        (claim 'knock door)
        (wait d (check 'open door)
              (format t "Enter ~A. " door)
              (unclaim 'knock door)
              (claim 'inside door)))

(=defun host (door)
        (wait k (check 'knock door)
              (format t "Open ~A. " door)
              (claim 'open door)
              (wait g (check 'inside door)
                    (format t "Close ~A.~%" door)
                    (unclaim 'open door))))

(program ballet ()
         (fork (visitor 'door1) 1)
         (fork (host 'door1) 1)
         (fork (visitor 'door2) 1)
         (fork (host 'door2) 1))

(=defun capture (city)
        (take city)
        (setpri 1)
        (yield
         (fortify city)))

(=defun plunder (city)
        (loot city)
        (ransom city))

(defun take (c) (format t "Liberating ~A.~%" c))

(defun fortify (c) (format t "Rebuilding ~A.~%" c))

(defun loot (c) (format t "Nationalizing ~A.~%" c))

(defun ransom (c) (format t "Refinancing ~A.~%" c))

(program barbarians ()
         (fork (capture 'rome) 100)
         (fork (plunder 'rome) 98))
