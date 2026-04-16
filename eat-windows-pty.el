;;; eat-windows-pty.el --- ConPTY support for emacs-eat on native Windows -*- lexical-binding: t; -*-

;; Author: Fujisawa Electric Management Office
;; URL: https://github.com/zawatton/eat-windows-pty
;; Version: 0.1.0
;; Keywords: terminals, processes
;; Package-Requires: ((emacs "29.1") (eat "0.9"))

;;; Commentary:

;; Native Windows Emacs has no PTY: `make-process' cannot allocate one
;; and any package that wraps its target with `/usr/bin/env sh -c ...'
;; (as upstream eat does) fails immediately with "Spawning child
;; process: Invalid argument", because /usr/bin/env does not exist on
;; Windows.
;;
;; This file works around that by spawning `conhost.exe' itself as the
;; child process with the (undocumented but stable) flag set
;;
;;     conhost.exe --headless --height H --width W --feature pty PROG ARGS...
;;
;; conhost owns the ConPTY pseudo-console, runs PROG inside it, and
;; relays VT100 byte streams over its stdin/stdout pipes.  From Emacs's
;; perspective the child is just a pipe-mode subprocess emitting ANSI
;; escapes, which the eat terminal emulator already knows how to render.
;;
;; The approach is taken from PR #126 against akib/emacs-eat by Artem
;; Khramov (`thearcticcat'), commit 9e0931c (2023-12-25), which was
;; never merged upstream because the maintainer does not run Windows.
;; Instead of forking eat, this file overrides the two affected
;; functions (`eat-exec' and `eat--1') after eat is loaded, so we can
;; keep tracking upstream eat verbatim.
;;
;; Tested against eat commit 3a6f418 (2024-03-15).  If upstream eat
;; refactors `eat-exec' or `eat--1' substantially, the override bodies
;; below need to be re-synced from the new upstream.

;;; Usage:

;;     (with-eval-after-load 'eat
;;       (when (eq system-type 'windows-nt)
;;         (require 'eat-windows-pty)))

;;; Code:

(require 'eat)

(defun eat--build-command (command switches width height)
  "Build the argv vector that `make-process' will spawn for eat.

On Windows, wrap COMMAND with `conhost.exe --headless --feature pty'
so the child program runs inside a Windows pseudo-console while Emacs
talks to it through ordinary pipes.  WIDTH and HEIGHT are the desired
terminal dimensions in cells.

On other systems, fall back to upstream eat's `/usr/bin/env sh -c
stty ...' wrapper so this file is harmless if loaded by mistake."
  (cond
   ((eq system-type 'windows-nt)
    `("conhost.exe" "--headless"
      "--height" ,(number-to-string height)
      "--width"  ,(number-to-string width)
      "--feature" "pty"
      ,command ,@switches))
   (t
    `("/usr/bin/env" "sh" "-c"
      ,(format "stty -nl echo rows %d columns %d sane 2>%s ; \
if [ $1 = .. ]; then shift; fi; exec \"$@\""
               height width null-device)
      ".."
      ,command ,@switches))))

(defun eat-exec (buffer name command startfile switches)
  "Start up a process in BUFFER for Eat mode.

Run COMMAND with SWITCHES.  Set NAME as the name of the process.
Blast any old process running in the buffer.  Don't set the buffer
mode.  You can use this to cheaply run a series of processes in the
same Eat buffer.  The hook `eat-exec-hook' is run after each exec.

This is a local override of upstream `eat-exec'.  The body mirrors
upstream eat 3a6f418 verbatim except that the `:command' argument to
`make-process' is constructed via `eat--build-command', which routes
through conhost.exe on Windows."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when-let* ((eat-terminal)
                  (proc (eat-term-parameter
                         eat-terminal 'eat--process)))
        (remove-hook 'eat-exit-hook #'eat--kill-buffer t)
        (delete-process proc))
      ;; Ensure final newline.
      (goto-char (point-max))
      (unless (or (= (point-min) (point-max))
                  (= (char-before (point-max)) ?\n))
        (insert ?\n))
      (unless (= (point-min) (point-max))
        (insert "\n\n"))
      (setq eat-terminal (eat-term-make buffer (point)))
      (eat-semi-char-mode)
      (when-let* ((window (get-buffer-window nil t)))
        (with-selected-window window
          (eat-term-resize eat-terminal (window-max-chars-per-line)
                           (floor (window-screen-lines)))))
      (setf (eat-term-parameter eat-terminal 'input-function)
            #'eat--send-input)
      (setf (eat-term-parameter eat-terminal 'set-cursor-function)
            #'eat--set-cursor)
      (setf (eat-term-parameter eat-terminal 'grab-mouse-function)
            #'eat--grab-mouse)
      (setf (eat-term-parameter
             eat-terminal 'manipulate-selection-function)
            #'eat--manipulate-kill-ring)
      (setf (eat-term-parameter eat-terminal 'ring-bell-function)
            #'eat--bell)
      (setf (eat-term-parameter eat-terminal 'set-cwd-function)
            #'eat--set-cwd)
      (setf (eat-term-parameter eat-terminal 'ui-command-function)
            #'eat--handle-uic)
      (eat--set-term-sixel-params)
      ;; Crank up a new process.
      (let* ((size (eat-term-size eat-terminal))
             (process-environment
              (nconc
               (list
                (concat "TERM=" (eat-term-name))
                (concat "TERMINFO=" eat-term-terminfo-directory)
                (concat "INSIDE_EMACS=" eat-term-inside-emacs)
                (concat "EAT_SHELL_INTEGRATION_DIR="
                        eat-term-shell-integration-directory))
               process-environment))
             (process-connection-type t)
             ;; We should suppress conversion of end-of-line format.
             (inhibit-eol-conversion t)
             (process
              (make-process
               :name name
               :buffer buffer
               :command (eat--build-command command switches
                                            (car size) (cdr size))
               :filter #'eat--filter
               :sentinel #'eat--sentinel
               :file-handler t)))
        (process-put process 'adjust-window-size-function
                     #'eat--adjust-process-window-size)
        (set-process-query-on-exit-flag
         process eat-query-before-killing-running-terminal)
        ;; Jump to the end, and set the process mark.
        (goto-char (point-max))
        (set-marker (process-mark process) (point))
        (setf (eat-term-parameter eat-terminal 'eat--process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--input-process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--output-process)
              process)
        (when eat-kill-buffer-on-exit
          (add-hook 'eat-exit-hook #'eat--kill-buffer 90 t))
        ;; Feed it the startfile.
        (when startfile
          (sleep-for 1)
          (goto-char (point-max))
          (insert-file-contents startfile)
          (process-send-string
           process (delete-and-extract-region (point) (point-max)))))
      (eat-term-redisplay eat-terminal))
    (run-hook-with-args 'eat-exec-hook (eat-term-parameter
                                        eat-terminal 'eat--process))
    buffer))

(defun eat--1 (program arg display-buffer-fn)
  "Start a new Eat terminal emulator in a buffer.

PROGRAM and ARG is same as in `eat' and `eat-other-window'.
DISPLAY-BUFFER-FN is the function to display the buffer.

Local override of upstream `eat--1' that, on Windows, hands PROGRAM
to `eat-exec' directly instead of routing it through `/usr/bin/env
sh -c PROGRAM' (which fails because /usr/bin/env doesn't exist on
Windows).  conhost.exe spawned by `eat--build-command' takes the
place of the missing shell wrapper."
  (let ((program (or program (funcall eat-default-shell-function)))
        (buffer
         (cond
          ((numberp arg)
           (get-buffer-create (format "%s<%d>" eat-buffer-name arg)))
          (arg
           (generate-new-buffer eat-buffer-name))
          (t
           (get-buffer-create eat-buffer-name)))))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (funcall display-buffer-fn buffer)
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (if (eq system-type 'windows-nt)
            ;; Split PROGRAM into argv so conhost can exec it directly.
            ;; `split-string-shell-command' is Emacs 28+.
            (let ((argv (split-string-shell-command program)))
              (eat-exec buffer (buffer-name)
                        (car argv) nil (cdr argv)))
          (eat-exec buffer (buffer-name) "/usr/bin/env" nil
                    (list "sh" "-c" program))))
      buffer)))

(provide 'eat-windows-pty)

;;; eat-windows-pty.el ends here
