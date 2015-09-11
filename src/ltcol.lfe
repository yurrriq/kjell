(defmodule ltcol
  (export all)
  (export (t 1) (test 0))
  (export (text-attr 2) (fg-color 2) (bg-color 2))
  (export (zipfoldl 4)))

(defun ANSI-CTRL-CHARS () "\e[")

(defun ANSI-CLEAR () "\e[0m")

(defun TABLE ()
  #m(ansi #m(text-attr #m(reset       "0"
                          bright      "1"
                          dim         "2"
                          underscore  "4"
                          blink       "5"
                          reverse     "7"
                          hidden      "8"
                          alt_font_1 "11"
                          alt_font_2 "12"
                          alt_font_3 "13"
                          alt_font_4 "14"
                          alt_font_5 "15"
                          alt_font_6 "16"
                          alt_font_7 "17"
                          alt_font_8 "18"
                          alt_font_9 "19"
                          framed     "51"
                          encircled  "52"
                          overlined  "52")
             fg-color  #m(black      "30"
                          red        "31"
                          green      "32"
                          yellow     "33"
                          blue       "34"
                          magenta    "35"
                          cyan       "36"
                          white      "37"
                          default_fg "39")
             bg-color  #m(black      "40"
                          red        "41"
                          green      "42"
                          yellow     "43"
                          blue       "44"
                          magenta    "45"
                          cyan       "46"
                          white      "47"
                          default_bg "49"))))

(defun t (lines) (t-a lines)) ;; only ANSI, for now.

(defun text-attr (class fmt)      (get-val class 'text-attr fmt))

(defun fg-color  (class color)    (get-val class 'fg-color  color))

(defun bg-color  (class color)    (get-val class 'bg-color  color))

(defun get-val   (class attr fmt) (mref (mref (mref (TABLE) class) attr) fmt))

(defun ctrl-sequence (['ansi attrs] (ctrl-sequence 'ansi attrs '())))

(defun ctrl-sequence
  (['ansi `(#(,f ,attr) . ,t) acc]
   (ctrl-sequence 'ansi  t `(,(apply (MODULE) f `(ansi ,attr)) . ,acc)))
  (['ansi '() acc]
   (lists:concat
    `(,(ANSI-CTRL-CHARS) ,(string:join (lists:reverse acc) ";") "m"))))

(defun t-a (lines) (t-a lines '()))

(defun t-a
  ([`(,line . ,t) acc]
   (let ((`#(,attrs ,str) line))
     (t-a t `(,(string:concat (ctrl-sequence 'ansi attrs) str) . ,acc))))
  (['() acc] (lists:flatten (lists:reverse acc))))

(defun test ()
  (let* ((colors          '(black red green yellow blue magenta cyan white))
         (attrs           '(bright dim underscore blink reverse hidden))
         (all-combos      (lc ((<- attr attrs)
                               (<- fg   colors)
                               (<- bg   (lists:reverse colors)))
                            `#(,attr ,fg ,bg)))
         (all-combos-attr (lists:map
                           (lambda (attr-tuple)
                             (let ((`#(,attr ,fg ,bg) attr-tuple))
                               `#((#(text-attr ,attr)
                                   #(fg-color  ,fg)
                                   #(bg-color  ,bg))
                                  ,(lists:concat `(,(atom_to_list fg)
                                                   " on "
                                                   ,(atom_to_list bg)
                                                   " ("
                                                   ,(atom_to_list attr)
                                                   ")" ,(ANSI-CLEAR) "\n")))))
                           all-combos))
         (all-combos-str   (t-a all-combos-attr)))
    (io:format "~ts" `(,all-combos-str))))
