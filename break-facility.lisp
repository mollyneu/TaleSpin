;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Miscellaneous; Base: 10 -*-


;;; 
;;;		      BREAK FACILITY PACKAGE FOR COMMON LISP
;;;          Copyright (C) 1993   Michael T. Cox   (cox@cc.gatech.edu)
;;;
;;; 
;;; 	      *******************************************************
;;;
;;; This  program is  free  software; you can redistribute it and/or  modify it
;;; under the terms  of the GNU General Public License as published by the Free
;;; Software  Foundation;  either  version  1,  or  (at  your option) any later
;;; version.
;;; 
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without  even the  implied  warranty  of  MERCHANTABILITY  or
;;; FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General Public  License for
;;; more details.
;;; 
;;; You should  have received a copy of  the  GNU  General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.  In emacs type C-h C-w to view license.
;;; 
;;; 
;;; 	      *******************************************************
;;;
;;; Do you ever get tired of adding break statement to your LISP code  and then
;;; having to comment  them out or  remove them later? Do you ever forget which
;;; functions have these  break statements inserted  into  them?  The following
;;; macro package automates some of these services for you in the spirit of the
;;; trace facility of Common LISP.
;;; 
;;; To  use  the package,  simply  insert  a do-break  call  into  each of  the
;;; functions  that are still  under  development  and not completely debugged.
;;; (Note that  this  effort is  partially  automated for  you as  well  via an
;;; included emacs keyboard  macro provided below.)  For instance there may  be
;;; two functions foo  and bar as follows. Note that the do-break call may take
;;; an optional format string and format parameters.
;;; 
;;; 
;;; 
;;; (defun foo (x &optional (y 12) &rest body)
;;;   "Example function with formatted do-break call"
;;;   (do-break foo "~%X: ~s~%Y: ~s~%Body: ~s" x y body)
;;;   (list x y . body))
;;; 
;;; (defun bar (x)
;;;   "Example function with do-break call in body"
;;;   (my_computation x)
;;;   (do-break bar)
;;;   (more_computation x))
;;; 
;;; 
;;; When  testing the code,  one may  activate  the  break inside  one of these
;;; functions  by calling the function add-break.  For instance (add-break foo)
;;; causes the interpretation or execution of  foo to stop any time it is later
;;; invoked, the parameters passed to foo are printed, and the LISP debugger is
;;; entered.  Like any  normal break statement, the control continues after the
;;; do-break call  when the debugger is existed. If break is also set to on for
;;; the bar function, then some computation will first  be performed, the break
;;; will be executed, and then more computation is performed before exiting the
;;; bar function.
;;; 
;;; Like trace, one need not quote the function names when adding a function to
;;; the break list. Also like trace, if you pass no parameters to the add-break
;;; statement,  then the facility will  list  all of  those functions currently
;;; with break enabled. The un-break function is like untrace. One may un-break
;;; a particular  set of functions that are currently active,  or by passing no
;;; parameters to un-break all  functions currently active are removed from the
;;; break list.
;;; 
;;; An emacs keyboard macro is also provided with this package  to help add the
;;; do-break statements to existing code. Simply load  the macro or place it in
;;; your  .emacs file. Then go to the top of a file containing code you wish to
;;; modify. Typing "ESC-21 ESC-x add-do-break"  will  start searching your file
;;; for all function  definitions,  asking you if  you want to add the do-break
;;; statement to the first line  after  the parameter list.  By  including  the
;;; prefix "21" above  it allows you to skip insertion  up to 21  times. Before
;;; inserting each do-break statement, the macro pauses and prompts the user in
;;; the minibuffer for verification.  A SPACE allows the insertion,  DEL causes
;;; it to skip, CTRL-c  suspends the process and allows you to edit in the file
;;; (perhaps you want to add a format string  to a previous do-break before you
;;; forget) until  you type ESC-CTRL-c, and CTRL-d causes the entire process to
;;; be aborted.




(defvar *Break-List* nil
  "The list of functions to enter the debugger upon execution.")



;;;
;;; Macro add-break adds any number of functions to the break list. If no
;;; parameter is passed, then it prints the break list instead.
;;;
;;; ||||| Should actually check to see that the function names are indeed
;;; functions and not typos.
;;; 
(defmacro add-break (&rest function-names)
  `(cond ((null (quote ,function-names))
	  *Break-List*)
	 (t
	  (setf *Break-List*
		(union *Break-List* (quote ,function-names)))
	  (quote ,function-names)))
  )



;;;
;;; Macro un-break removes any number of functions from the break list. If
;;; no parameter is passed, it removes all functions.
;;;
(defmacro un-break (&rest function-names)
  `(cond ((null (quote ,function-names))
	  (let ((temp *Break-List*))
	    (setf *Break-List* nil)
	    temp))
	 (t
	  (setf *Break-List*
		(set-difference *Break-List* (quote ,function-names)))
	  (quote ,function-names)))
  )


;;;
;;; Calls to macro do-break are inserted into user fucntions at points where
;;; breaks are desired. The first argument is mandatory and must be the
;;; name of the function within which the do-break call is inserted. Optional 
;;; format strings and arguments are allowed.
;;;
(defmacro do-break (function-name &optional break-string &rest arguments)
  `(if (member (quote ,function-name) *Break-List*)
      (apply
	#'break
	(if ,break-string
	    (list ,break-string ,@arguments)
	    (list "inside function ~s~%" (quote ,function-name)))))
  )


