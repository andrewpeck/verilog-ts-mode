;;; test-hog.el -*- lexical-binding: t; -*-

(require 'verilog-port-copy)
(require 'ert)

(add-to-list 'treesit-extra-load-path "~/.emacs.d/.local/etc/tree-sitter")
(add-to-list 'treesit-extra-load-path "~/.emacs.d/.local/cache/tree-sitter")

(defmacro completion-test (file port-list)
  `(ert-deftest ,(intern file) nil
     (should (equal
              ,port-list
              (progn (find-file (concat (locate-dominating-file "." ".git") ,file))
                     (verilog-ts-mode)
                     (goto-char (point-min))
                     (verilog-ts--compute-buffer-local-candidates))))))

(completion-test "test/test1.sv"
                 '(#("clk" 0 3 (verilog-ts-type "port"))
                   #("din" 0 3 (verilog-ts-type "port"))
                   #("sum" 0 3 (verilog-ts-type "port"))
                   #("sum_a" 0 5 (verilog-ts-type "logic"))
                   #("sum_b" 0 5 (verilog-ts-type "logic"))
                   #("din_a" 0 5 (verilog-ts-type "logic"))
                   #("din_b" 0 5 (verilog-ts-type "logic"))
                   #("din_padded" 0 10 (verilog-ts-type "logic"))
                   #("N" 0 1 (verilog-ts-type "parameter"))
                   #("W" 0 1 (verilog-ts-type "parameter"))
                   #("NPOW2" 0 5 (verilog-ts-type "localparam"))
                   #("NB" 0 2 (verilog-ts-type "localparam"))))
